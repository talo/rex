use std::{collections::VecDeque, vec};

use rex_ast::{
    expr::{Expr, Scope, Var},
    id::IdDispenser,
};
use rex_lexer::{
    span::{Position, Span, Spanned},
    Token, Tokens,
};

use crate::{
    error::{Error, ParserErr},
    op::Operator,
};

pub mod error;
pub mod op;

pub struct Parser {
    pub token_cursor: usize,
    pub tokens: Tokens,
    pub errors: Vec<ParserErr>,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Parser {
        let mut parser = Parser {
            token_cursor: 0,
            tokens: tokens
                .into_iter()
                .filter_map(|token| match token {
                    Token::Whitespace(..) | Token::WhitespaceNewline(..) => None,
                    token => Some(token),
                })
                .collect(),
            errors: Vec::new(),
        };
        parser.strip_comments();
        parser
    }

    pub fn current_token(&self) -> Option<Token> {
        if self.token_cursor < self.tokens.len() {
            Some(self.tokens[self.token_cursor].clone())
        } else {
            None
        }
    }

    pub fn peek_token(&self, n: usize) -> Option<Token> {
        if self.token_cursor + n < self.tokens.len() {
            Some(self.tokens[self.token_cursor + n].clone())
        } else {
            None
        }
    }

    pub fn next_token(&mut self) {
        self.token_cursor += 1;
    }

    pub fn strip_comments(&mut self) {
        let mut cursor = 0;

        while cursor < self.tokens.len() {
            match self.tokens[cursor] {
                Token::CommentL(..) => {
                    self.tokens.remove(cursor);
                    while cursor < self.tokens.len() {
                        if let Token::CommentR(..) = self.tokens[cursor] {
                            self.tokens.remove(cursor);
                            break;
                        }
                        self.tokens.remove(cursor);
                    }
                }
                _ => {
                    cursor += 1;
                    continue;
                }
            }
        }
    }

    pub fn parse_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        let lhs_expr = self.parse_unary_expr(id_dispenser)?;
        let expr = self.parse_binary_expr(id_dispenser, lhs_expr);
        if let Err(err) = &expr {
            self.errors.push(format!("{}", err).into());
        }
        if self.errors.is_empty() {
            expr
        } else {
            Err(Error::Parser(self.errors.clone()))
        }
    }

    pub fn parse_binary_expr(
        &mut self,
        id_dispenser: &mut IdDispenser,
        lhs_expr: Expr,
    ) -> Result<Expr, Error> {
        let lhs_expr_span = lhs_expr.span();

        // Get the next token.
        let token = match self.current_token() {
            Some(token) => token,
            // Having no next token should finish the parsing of the binary
            // expression.
            None => return Ok(lhs_expr),
        };
        let prec = token.precedence();

        // Parse the binary operator.
        let operator = match token {
            Token::Add(..) => Operator::Add,
            Token::Concat(..) => Operator::Concat,
            Token::Div(..) => Operator::Div,
            Token::Dot(..) => Operator::Dot,
            Token::Eq(..) => Operator::Eq,
            Token::Ge(..) => Operator::Ge,
            Token::Gt(..) => Operator::Gt,
            Token::Le(..) => Operator::Le,
            Token::Lt(..) => Operator::Lt,
            Token::Mul(..) => Operator::Mul,
            Token::Sub(..) => Operator::Sub,
            _ => {
                return Ok(lhs_expr);
            }
        };
        let operator_span = token.span();

        // We have now decided that this token can be parsed so we consume it.
        self.next_token();

        // Parse the next part of this binary expression.
        let rhs_expr = self.parse_unary_expr(id_dispenser)?;
        let rhs_expr_span = *rhs_expr.span();

        let next_binary_expr_takes_precedence = match self.current_token() {
            // Next token has lower precedence
            Some(token) if prec > token.precedence() => false,
            // Next token has the same precedence
            Some(token) if prec == token.precedence() => match token {
                // But it is left-associative
                Token::Add(..)
                | Token::And(..)
                | Token::Concat(..)
                | Token::Div(..)
                | Token::Mul(..)
                | Token::Or(..)
                | Token::Sub(..) => false,
                // But it is right-associative
                _ => true,
            },
            // Next token has higher precedence
            Some(_) => true,
            // No more tokens
            None => false,
        };

        let rhs_expr = if next_binary_expr_takes_precedence {
            self.parse_binary_expr(id_dispenser, rhs_expr)?
        } else {
            rhs_expr
        };

        let id_op = id_dispenser.next();
        let id_op_lhs = id_dispenser.next();
        let id_op_lhs_rhs = id_dispenser.next();

        let inner_span = Span::from_begin_end(lhs_expr_span.begin, operator_span.end);
        let outer_span = Span::from_begin_end(lhs_expr_span.begin, rhs_expr_span.end);

        self.parse_binary_expr(
            id_dispenser,
            Expr::App(
                id_op_lhs_rhs,
                outer_span,
                Box::new(Expr::App(
                    id_op_lhs,
                    inner_span,
                    Box::new(Expr::Var(Var::new(
                        id_op,
                        *operator_span,
                        operator.to_string(),
                    ))),
                    Box::new(lhs_expr),
                )),
                Box::new(rhs_expr),
            ),
        )
    }

    pub fn parse_unary_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        let mut call_base_expr = match self.current_token() {
            Some(Token::ParenL(..)) => self.parse_paren_expr(id_dispenser),
            Some(Token::BracketL(..)) => self.parse_bracket_expr(id_dispenser),
            Some(Token::BraceL(..)) => self.parse_brace_expr(id_dispenser),
            Some(Token::Bool(..)) => self.parse_literal_bool_expr(id_dispenser),
            Some(Token::Float(..)) => self.parse_literal_float_expr(id_dispenser),
            Some(Token::Int(..)) => self.parse_literal_int_expr(id_dispenser),
            Some(Token::String(..)) => self.parse_literal_str_expr(id_dispenser),
            Some(Token::Ident(..)) => self.parse_ident_expr(id_dispenser),
            Some(Token::BackSlash(..)) => self.parse_lambda_expr(id_dispenser),
            Some(Token::Let(..)) => self.parse_let_expr(id_dispenser),
            Some(Token::If(..)) => self.parse_if_expr(id_dispenser),
            Some(Token::Sub(..)) => self.parse_neg_expr(id_dispenser),
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("unexpected `{}`", token),
                ));
                return Err(Error::Parser(self.errors.clone()));
            }
            token => {
                self.errors.push(ParserErr::new(
                    Span::new(0, 0, 0, 0),
                    format!("unexpected {:?}", token),
                ));
                return Err(Error::Parser(self.errors.clone()));
            }
        }?;
        let call_base_expr_span = *call_base_expr.span();

        let mut call_arg_exprs = VecDeque::new();
        loop {
            let token = self.current_token();
            let call_arg_expr = match token {
                Some(Token::ParenL(..)) => self.parse_paren_expr(id_dispenser),
                Some(Token::BracketL(..)) => self.parse_bracket_expr(id_dispenser),
                Some(Token::BraceL(..)) => self.parse_brace_expr(id_dispenser),
                Some(Token::Bool(..)) => self.parse_literal_bool_expr(id_dispenser),
                Some(Token::Float(..)) => self.parse_literal_float_expr(id_dispenser),
                Some(Token::Int(..)) => self.parse_literal_int_expr(id_dispenser),
                Some(Token::String(..)) => self.parse_literal_str_expr(id_dispenser),
                Some(Token::Ident(..)) => self.parse_ident_expr(id_dispenser),
                Some(Token::BackSlash(..)) => self.parse_lambda_expr(id_dispenser),
                Some(Token::Let(..)) => self.parse_let_expr(id_dispenser),
                Some(Token::If(..)) => self.parse_if_expr(id_dispenser),
                _ => break,
            }?;
            call_arg_exprs.push_back(call_arg_expr);
        }

        while let Some(call_arg_expr) = call_arg_exprs.pop_front() {
            let call_arg_expr_span_end = call_arg_expr.span().end;
            call_base_expr = Expr::App(
                id_dispenser.next(),
                Span::from_begin_end(call_base_expr_span.begin, call_arg_expr_span_end),
                Box::new(call_base_expr),
                Box::new(call_arg_expr),
            );
        }
        Ok(call_base_expr)
    }

    pub fn parse_paren_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        // Eat the left parenthesis.
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::ParenL(span, ..)) => {
                self.next_token();
                span
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected '(' got {}", token),
                ));
                return Err(Error::Parser(self.errors.clone()));
            }
            _ => {
                return Err(vec!["expected `(`".to_string().into()].into());
            }
        };

        // Parse the inner expression.
        let mut expr = match self.current_token() {
            Some(Token::Add(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "+"))
            }
            Some(Token::And(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "&&"))
            }
            Some(Token::Concat(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "++"))
            }
            Some(Token::Div(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "/"))
            }
            Some(Token::Dot(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "."))
            }
            Some(Token::Eq(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "=="))
            }
            Some(Token::Ge(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, ">="))
            }
            Some(Token::Gt(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, ">"))
            }
            Some(Token::Le(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "<="))
            }
            Some(Token::Lt(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "<"))
            }
            Some(Token::Mul(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "*"))
            }
            Some(Token::Or(span, ..)) => {
                self.next_token();
                Expr::Var(Var::next_with_span(id_dispenser, span, "||"))
            }
            Some(Token::Sub(span, ..)) => {
                if let Some(Token::ParenR(..)) = self.peek_token(1) {
                    // In the case of the `-` operator we need to explicitly
                    // check for the closing right parenthesis, because it is
                    // valid to have an expressions like `(- 69)`. This is
                    // different from other operators, because it is not valid
                    // to have an expression like `(+ 69)`` or `(>= 3)``.
                    //
                    // It would not be a crazy idea to explicitly check for the
                    // closing right parenthesis in other operators. Although we
                    // do not want to allow expressions like `(+ 420)` the
                    // explicit check will allow for better error messages.
                    self.next_token();
                    Expr::Var(Var::next_with_span(id_dispenser, span, "-"))
                } else {
                    self.parse_expr(id_dispenser)?
                }
            }
            _ => self.parse_expr(id_dispenser)?,
        };

        // Eat the right parenthesis.
        let token = self.current_token();
        let span_end = match token {
            Some(Token::ParenR(span, ..)) => {
                self.next_token();
                span
            }
            Some(Token::Comma(..)) => {
                // parse inner expressions
                return self.parse_tuple(id_dispenser, span_begin, expr);
            }
            _ => {
                self.errors.push("expected `)`".into());
                return Ok(expr);
            }
        };

        expr.set_span_begin_end(span_begin.begin, span_end.end);

        Ok(expr)
    }

    pub fn parse_tuple(
        &mut self,
        id_dispenser: &mut IdDispenser,
        span_begin: Span,
        first_item: Expr,
    ) -> Result<Expr, Error> {
        let mut items = vec![first_item];
        loop {
            // eat the comma
            let token = self.current_token();
            match token {
                Some(Token::Comma(..)) => self.next_token(),
                Some(Token::ParenR(end_span)) => {
                    self.next_token();
                    return Ok(Expr::Tuple(
                        id_dispenser.next(),
                        Span::from_begin_end(span_begin.begin, end_span.end),
                        items,
                    ));
                }
                _ => items.push(self.parse_expr(id_dispenser)?),
            }
        }
    }

    pub fn parse_bracket_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        // Eat the left bracket.
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::BracketL(span, ..)) => {
                self.next_token();
                span.begin
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `[` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `[`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Catch the case where the list is empty.
        let token = self.current_token();
        if let Some(Token::BracketR(span, ..)) = token {
            self.next_token();
            return Ok(Expr::List(
                id_dispenser.next(),
                Span::from_begin_end(span_begin, span.end),
                vec![],
            ));
        }

        let mut exprs = Vec::new();
        loop {
            // Parse the next expression.
            let expr = self.parse_expr(id_dispenser)?;
            let span_expr = *expr.span();
            exprs.push(expr);
            // Eat the comma.
            let token = self.current_token();
            match token {
                Some(Token::Comma(..)) => self.next_token(),
                None => {
                    self.errors.push(ParserErr::new(
                        Span::from_begin_end(span_begin, span_expr.end),
                        "expected `,` or `]`".to_string(),
                    ));
                    break;
                }
                _ => {
                    break;
                }
            };
        }

        // Eat the right bracket.
        let token = self.current_token();
        let span_end = match token {
            Some(Token::BracketR(span, ..)) => {
                self.next_token();
                span.end
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `]` got {}", token),
                ));

                return Ok(Expr::List(
                    id_dispenser.next(),
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    exprs,
                ));
            }
            _ => {
                self.errors.push("expected `]`".into());
                return Ok(Expr::List(
                    id_dispenser.next(),
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    exprs,
                ));
            }
        };

        Ok(Expr::List(
            id_dispenser.next(),
            Span::from_begin_end(span_begin, span_end),
            exprs,
        ))
    }

    pub fn parse_brace_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        // Eat the left brace.
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::BraceL(span, ..)) => {
                self.next_token();
                span.begin
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `[` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `[`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Catch the case where the dict is empty.
        let token = self.current_token();
        if let Some(Token::BraceR(span, ..)) = token {
            self.next_token();
            return Ok(Expr::Dict(
                id_dispenser.next(),
                Span::from_begin_end(span_begin, span.end),
                Default::default(),
            ));
        }

        let mut kvs = Vec::new();
        loop {
            // Parse the ident.
            let var = match self.parse_ident_expr(id_dispenser)? {
                Expr::Var(var) => var,
                _ => unreachable!(),
            };
            // Eat the =.
            let token = self.current_token();
            match token {
                Some(Token::Assign(..)) => self.next_token(),
                _ => {
                    self.errors.push(ParserErr::new(
                        Span::from_begin_end(span_begin, var.span.end),
                        "expected `=`".to_string(),
                    ));
                    break;
                }
            };
            // Parse the expression.
            let expr = self.parse_expr(id_dispenser)?;
            let span_expr = *expr.span();
            kvs.push((var.name, expr));
            // Eat the comma.
            let token = self.current_token();
            match token {
                Some(Token::Comma(..)) => self.next_token(),
                None => {
                    self.errors.push(ParserErr::new(
                        Span::from_begin_end(span_begin, span_expr.end),
                        "expected `,` or `}}`".to_string(),
                    ));
                    break;
                }
                _ => {
                    break;
                }
            };
        }

        // Eat the right brace.
        let token = self.current_token();
        let span_end = match token {
            Some(Token::BraceR(span, ..)) => {
                self.next_token();
                span.end
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `}}` got {}", token),
                ));

                return Ok(Expr::Dict(
                    id_dispenser.next(),
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    kvs.into_iter().collect(),
                ));
            }
            _ => {
                self.errors.push("expected `}}`".into());
                return Ok(Expr::Dict(
                    id_dispenser.next(),
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    kvs.into_iter().collect(),
                ));
            }
        };

        Ok(Expr::Dict(
            id_dispenser.next(),
            Span::from_begin_end(span_begin, span_end),
            kvs.into_iter().collect(),
        ))
    }

    pub fn parse_neg_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        // Eat the minus.
        let token = self.current_token();
        let span_token = match token {
            Some(Token::Sub(span, ..)) => {
                self.next_token();
                span
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `-` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `-`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Parse the inner expression.
        let expr = self.parse_expr(id_dispenser)?;
        let expr_span_end = expr.span().end;

        // Return the negative expression.
        let id_neg = id_dispenser.next();
        Ok(Expr::App(
            id_dispenser.next(),
            Span::from_begin_end(span_token.begin, expr_span_end),
            Box::new(Expr::Var(Var::new(id_neg, span_token, "negate"))),
            Box::new(expr),
        ))
    }

    //
    pub fn parse_lambda_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        // Eat the backslash.
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::BackSlash(span, ..)) => {
                self.next_token();
                span.begin
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `\\` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `\\`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Parse the params.
        let mut params = VecDeque::new();
        loop {
            let token = self.current_token();
            match token {
                Some(Token::Ident(param, span, ..)) => {
                    self.next_token();
                    params.push_back((id_dispenser.next(), span, param));
                }
                _ => break,
            }
        }

        // Parse the arrow.
        let token = self.current_token();
        let _span_arrow = match token {
            Some(Token::ArrowR(span, ..)) => {
                self.next_token();
                span
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `->` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `->`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Parse the body
        let mut body = self.parse_expr(id_dispenser)?;
        let mut body_span_end = body.span().end;
        while let Some((param_id, param_span, param)) = params.pop_back() {
            body = Expr::Lam(
                id_dispenser.next(),
                Span::from_begin_end(param_span.begin, body_span_end),
                Scope::new_sync(),
                Var::new(param_id, param_span, param),
                Box::new(body),
            );
            body_span_end = body.span().end;
        }
        // Adjust the outer most lambda to include the initial backslash
        body.set_span_begin(span_begin);

        Ok(body)
    }

    //
    pub fn parse_let_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        // Eat the `let` token
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::Let(span, ..)) => {
                self.next_token();
                span.begin
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `let` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `let`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Parse the variable declarations.
        let mut decls = VecDeque::new();
        loop {
            // Variable name
            let token = self.current_token();
            let var = match token {
                Some(Token::Ident(val, span, ..)) => {
                    self.next_token();
                    (id_dispenser.next(), span, val)
                }
                _ => break,
            };
            // =
            let token = self.current_token();
            match token {
                Some(Token::Assign(_span, ..)) => {
                    self.next_token();
                }
                Some(token) => {
                    self.errors.push(ParserErr::new(
                        *token.span(),
                        format!("expected `=` got {}", token),
                    ));
                    return Err(self.errors.clone().into());
                }
                _ => {
                    self.errors.push("expected `=`".into());
                    return Err(self.errors.clone().into());
                }
            }
            // Parse the variable definition
            decls.push_back((var, self.parse_expr(id_dispenser)?));
            // Parse `,` or `in`
            let token = self.current_token();
            match token {
                Some(Token::Comma(_span, ..)) => {
                    self.next_token();
                    continue;
                }
                Some(Token::In(..)) => break,
                Some(token) => {
                    self.errors.push(ParserErr::new(
                        *token.span(),
                        format!("expected `,` or `in` got {}", token),
                    ));
                    return Err(self.errors.clone().into());
                }
                _ => {
                    self.errors.push("expected `,` or `in`".into());
                    return Err(self.errors.clone().into());
                }
            }
        }

        // Parse the `in` token
        let token = self.current_token();
        let _span_arrow = match token {
            Some(Token::In(span, ..)) => {
                self.next_token();
                span
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `in` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `in`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Parse the body
        let mut body = self.parse_expr(id_dispenser)?;
        let mut body_span_end = body.span().end;
        while let Some(((var_id, var_span, var), def)) = decls.pop_back() {
            body = Expr::Let(
                id_dispenser.next(),
                Span::from_begin_end(var_span.begin, body_span_end),
                Var::new(var_id, var_span, var),
                Box::new(def),
                Box::new(body),
            )
            .into();
            body_span_end = body.span().end;
        }
        // Adjust the outer most let-in expression to include the initial let
        // token
        body.set_span_begin(span_begin);

        Ok(body)
    }

    //
    pub fn parse_if_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        // Eat the `if` token
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::If(span, ..)) => {
                self.next_token();
                span.begin
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `if` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `if`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Parse the cond expression
        let cond = self.parse_expr(id_dispenser)?;

        // Parse the `then` token
        let token = self.current_token();
        let _span_arrow = match token {
            Some(Token::Then(span, ..)) => {
                self.next_token();
                span
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `then` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `then`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Parse the then expression
        let then = self.parse_expr(id_dispenser)?;

        // Parse the `else` token
        let token = self.current_token();
        let _span_arrow = match token {
            Some(Token::Else(span, ..)) => {
                self.next_token();
                span
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `else` got {}", token),
                ));
                return Err(self.errors.clone().into());
            }
            _ => {
                self.errors.push("expected `else`".into());
                return Err(self.errors.clone().into());
            }
        };

        // Parse the else expression
        let r#else = self.parse_expr(id_dispenser)?;
        let else_span_end = r#else.span().end;

        Ok(Expr::Ite(
            id_dispenser.next(),
            Span::from_begin_end(span_begin, else_span_end),
            Box::new(cond),
            Box::new(then),
            Box::new(r#else),
        ))
    }

    //
    pub fn parse_literal_bool_expr(
        &mut self,
        id_dispenser: &mut IdDispenser,
    ) -> Result<Expr, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Bool(val, span, ..)) => Ok(Expr::Bool(id_dispenser.next(), span, val)),
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `bool` got {}", token),
                ));
                Err(self.errors.clone().into())
            }
            _ => {
                self.errors.push("expected `bool`".into());
                Err(self.errors.clone().into())
            }
        }
    }

    //
    pub fn parse_literal_float_expr(
        &mut self,
        id_dispenser: &mut IdDispenser,
    ) -> Result<Expr, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Float(val, span, ..)) => Ok(Expr::Float(id_dispenser.next(), span, val)),
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `float` got {}", token),
                ));
                Err(self.errors.clone().into())
            }
            _ => {
                self.errors.push("expected `float`".into());
                Err(self.errors.clone().into())
            }
        }
    }

    //
    pub fn parse_literal_int_expr(
        &mut self,
        id_dispenser: &mut IdDispenser,
    ) -> Result<Expr, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Int(val, span, ..)) => Ok(Expr::Uint(id_dispenser.next(), span, val)),
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `int` got {}", token),
                ));
                Err(self.errors.clone().into())
            }
            _ => {
                self.errors.push("expected `int`".into());
                Err(self.errors.clone().into())
            }
        }
    }

    //
    pub fn parse_literal_str_expr(
        &mut self,
        id_dispenser: &mut IdDispenser,
    ) -> Result<Expr, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::String(val, span, ..)) => Ok(Expr::String(id_dispenser.next(), span, val)),
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `str` got {}", token),
                ));
                Err(self.errors.clone().into())
            }
            _ => {
                self.errors.push("expected `str`".into());
                Err(self.errors.clone().into())
            }
        }
    }

    //
    pub fn parse_ident_expr(&mut self, id_dispenser: &mut IdDispenser) -> Result<Expr, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Ident(name, span, ..)) => {
                Ok(Expr::Var(Var::next_with_span(id_dispenser, span, name)))
            }
            Some(token) => {
                self.errors.push(ParserErr::new(
                    *token.span(),
                    format!("expected `ident` got {}", token),
                ));
                Err(self.errors.clone().into())
            }
            _ => {
                self.errors.push("expected `ident`".into());
                Err(self.errors.clone().into())
            }
        }
    }

    pub fn print_errors(&self) {
        for err in self.errors.iter() {
            println!("{}", err);
        }
    }
}

#[cfg(test)]
mod tests {
    use rex_ast::id::Id;
    use rex_lexer::Token;

    use super::*;

    #[test]
    fn test_parse_comment() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("true {- this is a boolean -}").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(expr, Expr::Bool(Id(0), Span::new(1, 1, 1, 4), true));

        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("{- this is a boolean -} false").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(expr, Expr::Bool(Id(0), Span::new(1, 25, 1, 29), false));

        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(3.54 {- this is a float -}, {- this is an int -} 42, false {- this is a boolean -})").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::Tuple(
                Id(3),
                Span::new(1, 1, 1, 84),
                vec![
                    Expr::Float(Id(0), Span::new(1, 2, 1, 5), 3.54),
                    Expr::Uint(Id(1), Span::new(1, 51, 1, 52), 42),
                    Expr::Bool(Id(2), Span::new(1, 55, 1, 59), false),
                ],
            )
        );
    }

    #[test]
    fn test_add() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("1 + 2").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(4),
                Span::new(1, 1, 1, 5),
                Box::new(Expr::App(
                    Id(3),
                    Span::new(1, 1, 1, 3),
                    Box::new(Expr::Var(Var {
                        id: Id(2),
                        span: Span::new(1, 3, 1, 3),
                        name: "+".to_string()
                    })),
                    Box::new(Expr::Uint(Id(0), Span::new(1, 1, 1, 1), 1))
                )),
                Box::new(Expr::Uint(Id(1), Span::new(1, 5, 1, 5), 2))
            )
        );

        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(6.9 + 3.14)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(4),
                Span::new(1, 1, 1, 12),
                Box::new(Expr::App(
                    Id(3),
                    Span::new(1, 2, 1, 6),
                    Box::new(Expr::Var(Var {
                        id: Id(2),
                        span: Span::new(1, 6, 1, 6),
                        name: "+".to_string()
                    })),
                    Box::new(Expr::Float(Id(0), Span::new(1, 2, 1, 4), 6.9))
                )),
                Box::new(Expr::Float(Id(1), Span::new(1, 8, 1, 11), 3.14))
            )
        );

        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(+) 420").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(2),
                Span::new(1, 1, 1, 7),
                Box::new(Expr::Var(Var {
                    id: Id(0),
                    span: Span::new(1, 1, 1, 3),
                    name: "+".to_string()
                })),
                Box::new(Expr::Uint(Id(1), Span::new(1, 5, 1, 7), 420))
            )
        );
    }

    #[test]
    fn test_sub() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("1 - 2").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(4),
                Span::new(1, 1, 1, 5),
                Box::new(Expr::App(
                    Id(3),
                    Span::new(1, 1, 1, 3),
                    Box::new(Expr::Var(Var {
                        id: Id(2),
                        span: Span::new(1, 3, 1, 3),
                        name: "-".to_string()
                    })),
                    Box::new(Expr::Uint(Id(0), Span::new(1, 1, 1, 1), 1))
                )),
                Box::new(Expr::Uint(Id(1), Span::new(1, 5, 1, 5), 2))
            )
        );

        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(6.9 - 3.14)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(4),
                Span::new(1, 1, 1, 12),
                Box::new(Expr::App(
                    Id(3),
                    Span::new(1, 2, 1, 6),
                    Box::new(Expr::Var(Var {
                        id: Id(2),
                        span: Span::new(1, 6, 1, 6),
                        name: "-".to_string()
                    })),
                    Box::new(Expr::Float(Id(0), Span::new(1, 2, 1, 4), 6.9))
                )),
                Box::new(Expr::Float(Id(1), Span::new(1, 8, 1, 11), 3.14))
            )
        );

        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(-) 4.20").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(2),
                Span::new(1, 1, 1, 8),
                Box::new(Expr::Var(Var {
                    id: Id(0),
                    span: Span::new(1, 1, 1, 3),
                    name: "-".to_string()
                })),
                Box::new(Expr::Float(Id(1), Span::new(1, 5, 1, 8), 4.2))
            ),
        );
    }

    #[test]
    fn test_negate() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("-1").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(2),
                Span::new(1, 1, 1, 2),
                Box::new(Expr::Var(Var {
                    id: Id(1),
                    span: Span::new(1, 1, 1, 1),
                    name: "negate".to_string()
                })),
                Box::new(Expr::Uint(Id(0), Span::new(1, 2, 1, 2), 1))
            ),
        );

        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(-1)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(2),
                Span::new(1, 1, 1, 4),
                Box::new(Expr::Var(Var {
                    id: Id(1),
                    span: Span::new(1, 2, 1, 2),
                    name: "negate".to_string()
                })),
                Box::new(Expr::Uint(Id(0), Span::new(1, 3, 1, 3), 1))
            ),
        );

        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(- 6.9)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        assert_eq!(
            expr,
            Expr::App(
                Id(2),
                Span::new(1, 1, 1, 7),
                Box::new(Expr::Var(Var {
                    id: Id(1),
                    span: Span::new(1, 2, 1, 2),
                    name: "negate".to_string()
                })),
                Box::new(Expr::Float(Id(0), Span::new(1, 4, 1, 6), 6.9))
            ),
        );
    }
}
