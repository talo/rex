use std::collections::VecDeque;

use rex_ast::{
    ast::{Call, IfThenElse, Lambda, LetIn, Var, AST},
    id::IdDispenser,
    span::{Position, Span, Spanned},
};

use crate::{
    error::{Error, ParserErr},
    lexer::{Token, Tokens},
    op::Operator,
};

pub mod error;
pub mod lexer;
pub mod op;

pub struct Parser {
    pub id_dispenser: IdDispenser,
    pub token_cursor: usize,
    pub tokens: Tokens,
    pub errors: Vec<ParserErr>,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Parser {
        Parser {
            id_dispenser: IdDispenser::new(),
            token_cursor: 0,
            tokens: tokens
                .into_iter()
                .filter_map(|token| match token {
                    Token::Whitespace(..) | Token::WhitespaceNewline(..) => None,
                    token => Some(token),
                })
                .collect(),
            errors: Vec::new(),
        }
    }

    pub fn with_dispenser(id_dispenser: IdDispenser, tokens: Tokens) -> Parser {
        Parser {
            id_dispenser,
            token_cursor: 0,
            tokens: tokens
                .into_iter()
                .filter_map(|token| match token {
                    Token::Whitespace(..) | Token::WhitespaceNewline(..) => None,
                    token => Some(token),
                })
                .collect(),
            errors: Vec::new(),
        }
    }

    pub fn current_token(&self) -> Option<Token> {
        if self.token_cursor < self.tokens.len() {
            Some(self.tokens[self.token_cursor].clone())
        } else {
            None
        }
    }

    pub fn next_token(&mut self) {
        self.token_cursor += 1;
    }

    pub fn parse_expr(&mut self) -> Result<AST, Error> {
        let lhs_expr = self.parse_unary_expr()?;
        let expr = self.parse_binary_expr(lhs_expr);
        if let Err(err) = &expr {
            self.errors.push(format!("{}", err).into());
        }
        if self.errors.is_empty() {
            expr
        } else {
            Err(Error::Parser(self.errors.clone()))
        }
    }

    pub fn parse_binary_expr(&mut self, lhs_expr: AST) -> Result<AST, Error> {
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
        let rhs_expr = self.parse_unary_expr()?;
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
            self.parse_binary_expr(rhs_expr)?
        } else {
            rhs_expr
        };

        let id_op = self.id_dispenser.next();
        let id_op_lhs = self.id_dispenser.next();
        let id_op_lhs_rhs = self.id_dispenser.next();
        self.parse_binary_expr(AST::Call(Call::new(
            Span::from_begin_end(lhs_expr_span.begin, rhs_expr_span.end),
            id_op_lhs_rhs,
            Call::new(
                Span::from_begin_end(lhs_expr_span.begin, operator_span.end),
                id_op_lhs,
                Var::new(*operator_span, id_op, operator),
                lhs_expr,
            ),
            rhs_expr,
        )))
    }

    pub fn parse_unary_expr(&mut self) -> Result<AST, Error> {
        let mut call_base_expr = match self.current_token() {
            Some(Token::ParenL(..)) => self.parse_paren_expr(),
            Some(Token::BracketL(..)) => self.parse_bracket_expr(),
            Some(Token::BraceL(..)) => self.parse_brace_expr(),
            Some(Token::Bool(..)) => self.parse_literal_bool_expr(),
            Some(Token::Float(..)) => self.parse_literal_float_expr(),
            Some(Token::Int(..)) => self.parse_literal_int_expr(),
            Some(Token::Null(..)) => self.parse_literal_null_expr(),
            Some(Token::String(..)) => self.parse_literal_str_expr(),
            Some(Token::Ident(..)) => self.parse_ident_expr(),
            Some(Token::BackSlash(..)) => self.parse_lambda_expr(),
            Some(Token::Let(..)) => self.parse_let_expr(),
            Some(Token::If(..)) => self.parse_if_expr(),
            Some(Token::Sub(..)) => self.parse_neg_expr(),
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
                Some(Token::ParenL(..)) => self.parse_paren_expr(),
                Some(Token::BracketL(..)) => self.parse_bracket_expr(),
                Some(Token::BraceL(..)) => self.parse_brace_expr(),
                Some(Token::Bool(..)) => self.parse_literal_bool_expr(),
                Some(Token::Float(..)) => self.parse_literal_float_expr(),
                Some(Token::Int(..)) => self.parse_literal_int_expr(),
                Some(Token::String(..)) => self.parse_literal_str_expr(),
                Some(Token::Ident(..)) => self.parse_ident_expr(),
                Some(Token::BackSlash(..)) => self.parse_lambda_expr(),
                Some(Token::Let(..)) => self.parse_let_expr(),
                Some(Token::If(..)) => self.parse_if_expr(),
                Some(Token::Sub(..)) => self.parse_neg_expr(),
                _ => break,
            }?;
            call_arg_exprs.push_back(call_arg_expr);
        }

        while let Some(call_arg_expr) = call_arg_exprs.pop_front() {
            let call_arg_expr_span = call_arg_expr.span();
            call_base_expr = AST::Call(Call::new(
                Span::from_begin_end(call_base_expr_span.begin, call_arg_expr_span.end),
                self.id_dispenser.next(),
                call_base_expr,
                call_arg_expr,
            ));
        }
        Ok(call_base_expr)
    }

    pub fn parse_paren_expr(&mut self) -> Result<AST, Error> {
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
                AST::Var(Var::new(span, self.id_dispenser.next(), "+"))
            }
            Some(Token::And(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "&&"))
            }
            Some(Token::Concat(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "++"))
            }
            Some(Token::Div(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "/"))
            }
            Some(Token::Dot(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "."))
            }
            Some(Token::Eq(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "=="))
            }
            Some(Token::Ge(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), ">="))
            }
            Some(Token::Gt(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), ">"))
            }
            Some(Token::Le(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "<="))
            }
            Some(Token::Lt(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "<"))
            }
            Some(Token::Mul(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "*"))
            }
            Some(Token::Or(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "||"))
            }
            Some(Token::Sub(span, ..)) => {
                self.next_token();
                AST::Var(Var::new(span, self.id_dispenser.next(), "-"))
            }
            _ => self.parse_expr()?,
        };

        // Eat the right parenthesis.
        let token = self.current_token();
        let span_end = match token {
            Some(Token::ParenR(span, ..)) => {
                self.next_token();
                span
            }
            Some(_) => {
                // parse inner expressions
                return Ok(expr);
            }
            _ => {
                self.errors.push("expected `)`".into());
                return Ok(expr);
            }
        };

        expr.set_span_begin(span_begin.begin);
        expr.set_span_end(span_end.end);
        Ok(expr)
    }

    pub fn parse_bracket_expr(&mut self) -> Result<AST, Error> {
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
            return Ok(AST::List(
                Span::from_begin_end(span_begin, span.end),
                vec![],
            ));
        }

        let mut exprs = Vec::new();
        loop {
            // Parse the next expression.
            let expr = self.parse_expr()?;
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

                return Ok(AST::List(
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    exprs,
                ));
            }
            _ => {
                self.errors.push("expected `]`".into());
                return Ok(AST::List(
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    exprs,
                ));
            }
        };

        Ok(AST::List(Span::from_begin_end(span_begin, span_end), exprs))
    }

    pub fn parse_brace_expr(&mut self) -> Result<AST, Error> {
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

        // Catch the case where the list is empty.
        let token = self.current_token();
        if let Some(Token::BraceR(span, ..)) = token {
            self.next_token();
            return Ok(AST::Dict(
                Span::from_begin_end(span_begin, span.end),
                vec![],
            ));
        }

        let mut kvs = Vec::new();
        loop {
            // Parse the ident.
            let var = match self.parse_ident_expr()? {
                AST::Var(var) => var,
                _ => unreachable!(),
            };
            let span_var = var.span;
            // Eat the =.
            let token = self.current_token();
            match token {
                Some(Token::Assign(..)) => self.next_token(),
                _ => {
                    self.errors.push(ParserErr::new(
                        Span::from_begin_end(span_begin, span_var.end),
                        "expected `=`".to_string(),
                    ));
                    break;
                }
            };
            // Parse the expression.
            let expr = self.parse_expr()?;
            let span_expr = *expr.span();
            kvs.push((var, expr));
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

                return Ok(AST::Dict(
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    kvs,
                ));
            }
            _ => {
                self.errors.push("expected `}}`".into());
                return Ok(AST::Dict(
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    kvs,
                ));
            }
        };

        Ok(AST::Dict(Span::from_begin_end(span_begin, span_end), kvs))
    }

    pub fn parse_neg_expr(&mut self) -> Result<AST, Error> {
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
        let expr = self.parse_expr()?;

        // Return the negative expression.
        let id_neg = self.id_dispenser.next();
        Ok(AST::Call(Call::new(
            Span::from_begin_end(span_token.begin, expr.span().end),
            self.id_dispenser.next(),
            Var::new(span_token, id_neg, "-"),
            expr,
        )))
    }

    //
    pub fn parse_lambda_expr(&mut self) -> Result<AST, Error> {
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
                Some(Token::Ident(val, span, ..)) => {
                    self.next_token();
                    params.push_back(Var::new(span, self.id_dispenser.next(), val));
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
        let mut body = self.parse_expr()?;
        while let Some(param) = params.pop_back() {
            body = AST::Lambda(Lambda::new(
                Span::from_begin_end(param.span.begin, body.span().end),
                self.id_dispenser.next(),
                param,
                body,
            ));
        }
        // Adjust the outer most lambda to include the initial backslash
        body.set_span_begin(span_begin);

        Ok(body)
    }

    //
    pub fn parse_let_expr(&mut self) -> Result<AST, Error> {
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
                    Var::new(span, self.id_dispenser.next(), val)
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
            decls.push_back((var, self.parse_expr()?));
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
        let mut body = self.parse_expr()?;
        while let Some((var, def)) = decls.pop_back() {
            body = LetIn::new(
                Span::from_begin_end(var.span.begin, body.span().end),
                self.id_dispenser.next(),
                var,
                def,
                body,
            )
            .into();
        }
        // Adjust the outer most let-in expression to include the initial let
        // token
        body.set_span_begin(span_begin);

        Ok(body)
    }

    //
    pub fn parse_if_expr(&mut self) -> Result<AST, Error> {
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
        let cond = self.parse_expr()?;

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
        let then = self.parse_expr()?;

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
        let r#else = self.parse_expr()?;

        Ok(AST::IfThenElse(IfThenElse::new(
            Span::from_begin_end(span_begin, r#else.span().end),
            self.id_dispenser.next(),
            cond,
            then,
            r#else,
        )))
    }

    //
    pub fn parse_literal_bool_expr(&mut self) -> Result<AST, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Bool(val, span, ..)) => Ok(AST::Bool(span, val)),
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
    pub fn parse_literal_float_expr(&mut self) -> Result<AST, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Float(val, span, ..)) => Ok(AST::Float(span, val)),
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
    pub fn parse_literal_int_expr(&mut self) -> Result<AST, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Int(val, span, ..)) => Ok(AST::Uint(span, val)),
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
    pub fn parse_literal_null_expr(&mut self) -> Result<AST, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Null(span, ..)) => Ok(AST::Null(span)),
            _ => {
                self.errors.push("expected `null`".into());
                Err(self.errors.clone().into())
            }
        }
    }

    //
    pub fn parse_literal_str_expr(&mut self) -> Result<AST, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::String(val, span, ..)) => Ok(AST::String(span, val)),
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
    pub fn parse_ident_expr(&mut self) -> Result<AST, Error> {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Ident(name, span, ..)) => {
                Ok(AST::Var(Var::new(span, self.id_dispenser.next(), name)))
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

    use crate::lexer::Token;

    use super::*;

    #[test]
    fn test_parse_literals() {
        let mut parser = Parser::new(Token::tokenize("true").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Bool(Span::new(1, 1, 1, 4), true));

        let mut parser = Parser::new(Token::tokenize("false").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Bool(Span::new(1, 1, 1, 5), false));

        let mut parser = Parser::new(Token::tokenize("0").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Uint(Span::new(1, 1, 1, 1), 0));

        let mut parser = Parser::new(Token::tokenize("1").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Uint(Span::new(1, 1, 1, 1), 1));

        let mut parser = Parser::new(Token::tokenize("42").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Uint(Span::new(1, 1, 1, 2), 42));

        let mut parser = Parser::new(Token::tokenize("0.0").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Float(Span::new(1, 1, 1, 3), 0.0));

        let mut parser = Parser::new(Token::tokenize("1.0").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Float(Span::new(1, 1, 1, 3), 1.0));

        let mut parser = Parser::new(Token::tokenize("3.54").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Float(Span::new(1, 1, 1, 4), 3.54));
    }

    #[test]
    fn test_parse_list() {
        let mut parser = Parser::new(Token::tokenize("[]").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::List(Span::new(1, 1, 1, 2), vec![]));

        let mut parser = Parser::new(Token::tokenize("[[]]").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            AST::List(
                Span::new(1, 1, 1, 4),
                vec![AST::List(Span::new(1, 2, 1, 3), vec![])],
            )
        );

        let mut parser = Parser::new(Token::tokenize("[[], []]").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            AST::List(
                Span::new(1, 1, 1, 8),
                vec![
                    AST::List(Span::new(1, 2, 1, 3), vec![]),
                    AST::List(Span::new(1, 6, 1, 7), vec![])
                ],
            )
        );

        let mut parser =
            Parser::new(Token::tokenize("[true, 42, 3.54, \"foo\", [], ident]").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            AST::List(
                Span::new(1, 1, 1, 34),
                vec![
                    AST::Bool(Span::new(1, 2, 1, 5), true),
                    AST::Uint(Span::new(1, 8, 1, 9), 42),
                    AST::Float(Span::new(1, 12, 1, 15), 3.54),
                    AST::String(Span::new(1, 18, 1, 22), "foo".to_string()),
                    AST::List(Span::new(1, 25, 1, 26), vec![]),
                    AST::Var(Var::new(Span::new(1, 29, 1, 33), Id::default(), "ident"))
                ],
            )
        );
    }

    #[test]
    fn test_parse_dict() {
        let mut parser = Parser::new(Token::tokenize("{}").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, AST::Dict(Span::new(1, 1, 1, 2), vec![]));

        let mut parser =
            Parser::new(Token::tokenize("{ x = true, y = 42, z = 3.54, w = \"foo\", p = [], q = { x = false, y = 354, z = 4.2, w = \"bar\" }}").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            AST::Dict(
                Span::new(1, 1, 1, 96),
                vec![
                    (
                        Var::new(Span::new(1, 3, 1, 3), Id(0), "x"),
                        AST::Bool(Span::new(1, 7, 1, 10), true)
                    ),
                    (
                        Var::new(Span::new(1, 13, 1, 13), Id(1), "y"),
                        AST::Uint(Span::new(1, 17, 1, 18), 42)
                    ),
                    (
                        Var::new(Span::new(1, 21, 1, 21), Id(2), "z"),
                        AST::Float(Span::new(1, 25, 1, 28), 3.54)
                    ),
                    (
                        Var::new(Span::new(1, 31, 1, 31), Id(3), "w"),
                        AST::String(Span::new(1, 35, 1, 39), "foo".to_string())
                    ),
                    (
                        Var::new(Span::new(1, 42, 1, 42), Id(4), "p"),
                        AST::List(Span::new(1, 46, 1, 47), vec![])
                    ),
                    (
                        Var::new(Span::new(1, 50, 1, 50), Id(5), "q"),
                        AST::Dict(
                            Span::new(1, 54, 1, 95),
                            vec![
                                (
                                    Var::new(Span::new(1, 56, 1, 56), Id(6), "x"),
                                    AST::Bool(Span::new(1, 60, 1, 64), false)
                                ),
                                (
                                    Var::new(Span::new(1, 67, 1, 67), Id(7), "y"),
                                    AST::Uint(Span::new(1, 71, 1, 73), 354)
                                ),
                                (
                                    Var::new(Span::new(1, 76, 1, 76), Id(8), "z"),
                                    AST::Float(Span::new(1, 80, 1, 82), 4.2)
                                ),
                                (
                                    Var::new(Span::new(1, 85, 1, 85), Id(9), "w"),
                                    AST::String(Span::new(1, 89, 1, 93), "bar".to_string())
                                ),
                            ],
                        )
                    ),
                ],
            )
        );
    }

    #[test]
    fn test_parse_variable() {
        let mut parser = Parser::new(Token::tokenize("foo").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            AST::Var(Var::new(Span::new(1, 1, 1, 3), Id(0), "foo")),
        );

        let mut parser = Parser::new(Token::tokenize("(bar)").unwrap());
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            AST::Var(Var::new(Span::new(1, 1, 1, 5), Id(0), "bar")),
        );
    }

    // #[test]
    // fn test_parse_math_operators() {
    //     let mut parser = Parser::new(Token::tokenize("1 + 2").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         AST::Call(Call::new(
    //             Span::new(1, 1, 1, 5),
    //             AST::Var(Var::new(Span::new(1, 3, 1, 3), Id(1), "+")),
    //             vec![
    //                 AST::Int(1, Span::new(1, 1, 1, 1)),
    //                 AST::Int(2, Span::new(1, 5, 1, 5))
    //             ],
    //         )),
    //     );

    //     let mut parser = Parser::new(Token::tokenize("1 + 2 * 3").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         AST::Call(
    //             AST::Var(UnresolvedVar::new("+", Span::new(1, 3, 1, 3))).into(),
    //             vec![
    //                 AST::Int(1, Span::new(1, 1, 1, 1)),
    //                 AST::Call(
    //                     AST::Var(UnresolvedVar::new("*", Span::new(1, 7, 1, 7))).into(),
    //                     vec![
    //                         AST::Int(2, Span::new(1, 5, 1, 5)),
    //                         AST::Int(3, Span::new(1, 9, 1, 9))
    //                     ],
    //                     Span::new(1, 5, 1, 9)
    //                 )
    //             ],
    //             Span::new(1, 1, 1, 9)
    //         ),
    //     );

    //     let mut parser = Parser::new(Token::tokenize("1 * 2 + 3").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         AST::Call(
    //             AST::Var(UnresolvedVar::new("+", Span::new(1, 7, 1, 7))).into(),
    //             vec![
    //                 AST::Call(
    //                     AST::Var(UnresolvedVar::new("*", Span::new(1, 3, 1, 3))).into(),
    //                     vec![
    //                         AST::Int(1, Span::new(1, 1, 1, 1)),
    //                         AST::Int(2, Span::new(1, 5, 1, 5))
    //                     ],
    //                     Span::new(1, 1, 1, 5)
    //                 ),
    //                 AST::Int(3, Span::new(1, 9, 1, 9))
    //             ],
    //             Span::new(1, 1, 1, 9)
    //         ),
    //     );

    //     let mut parser = Parser::new(Token::tokenize("1 * (2 + 3)").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         AST::Call(
    //             AST::Var(UnresolvedVar::new("*", Span::new(1, 3, 1, 3))).into(),
    //             vec![
    //                 AST::Int(1, Span::new(1, 1, 1, 1)),
    //                 AST::Call(
    //                     AST::Var(UnresolvedVar::new("+", Span::new(1, 8, 1, 8))).into(),
    //                     vec![
    //                         AST::Int(2, Span::new(1, 6, 1, 6)),
    //                         AST::Int(3, Span::new(1, 10, 1, 10))
    //                     ],
    //                     Span::new(1, 5, 1, 11)
    //                 ),
    //             ],
    //             Span::new(1, 1, 1, 11)
    //         ),
    //     );
    // }

    // #[test]
    // fn test_parse_dot_operator() {
    //     let mut parser = Parser::new(Token::tokenize("f . g").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Call(
    //             Expr::Var(UnresolvedVar::new(".", Span::new(1, 3, 1, 3))).into(),
    //             vec![
    //                 Expr::Var(UnresolvedVar::new("f", Span::new(1, 1, 1, 1))),
    //                 Expr::Var(UnresolvedVar::new("g", Span::new(1, 5, 1, 5))),
    //             ],
    //             Span::new(1, 1, 1, 5)
    //         ),
    //     );

    //     let mut parser = Parser::new(Token::tokenize("f . g . h").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Call(
    //             Expr::Var(UnresolvedVar::new(".", Span::new(1, 3, 1, 3))).into(),
    //             vec![
    //                 Expr::Var(UnresolvedVar::new("f", Span::new(1, 1, 1, 1))),
    //                 Expr::Call(
    //                     Expr::Var(UnresolvedVar::new(".", Span::new(1, 7, 1, 7)))
    //                         .into(),
    //                     vec![
    //                         Expr::Var(UnresolvedVar::new("g", Span::new(1, 5, 1, 5))),
    //                         Expr::Var(UnresolvedVar::new("h", Span::new(1, 9, 1, 9))),
    //                     ],
    //                     Span::new(1, 5, 1, 9)
    //                 )
    //             ],
    //             Span::new(1, 1, 1, 9)
    //         ),
    //     );

    //     let mut parser = Parser::new(Token::tokenize("(f . g) . h").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Call(
    //             Expr::Var(UnresolvedVar::new(".", Span::new(1, 9, 1, 9))).into(),
    //             vec![
    //                 Expr::Call(
    //                     Expr::Var(UnresolvedVar::new(".", Span::new(1, 4, 1, 4)))
    //                         .into(),
    //                     vec![
    //                         Expr::Var(UnresolvedVar::new("f", Span::new(1, 2, 1, 2))),
    //                         Expr::Var(UnresolvedVar::new("g", Span::new(1, 6, 1, 6))),
    //                     ],
    //                     Span::new(1, 1, 1, 7)
    //                 ),
    //                 Expr::Var(UnresolvedVar::new("h", Span::new(1, 11, 1, 11))),
    //             ],
    //             Span::new(1, 1, 1, 11)
    //         ),
    //     );
    // }

    // #[test]
    // fn test_call() {
    //     let mut parser = Parser::new(Token::tokenize("f x").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Call(
    //             Expr::Var(UnresolvedVar::new("f", Span::new(1, 1, 1, 1))).into(),
    //             vec![Expr::Var(UnresolvedVar::new(
    //                 "x",
    //                 Span::new(1, 3, 1, 3)
    //             ))],
    //             Span::new(1, 1, 1, 3)
    //         ),
    //     );

    //     let mut parser = Parser::new(Token::tokenize("f x y").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Call(
    //             Expr::Var(UnresolvedVar::new("f", Span::new(1, 1, 1, 1))).into(),
    //             vec![
    //                 Expr::Var(UnresolvedVar::new("x", Span::new(1, 3, 1, 3))),
    //                 Expr::Var(UnresolvedVar::new("y", Span::new(1, 5, 1, 5)))
    //             ],
    //             Span::new(1, 1, 1, 5)
    //         ),
    //     );

    //     let mut parser = Parser::new(Token::tokenize("f x (g y) z").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Call(
    //             Expr::Var(UnresolvedVar::new("f", Span::new(1, 1, 1, 1))).into(),
    //             vec![
    //                 Expr::Var(UnresolvedVar::new("x", Span::new(1, 3, 1, 3))),
    //                 Expr::Call(
    //                     Expr::Var(UnresolvedVar::new("g", Span::new(1, 6, 1, 6)))
    //                         .into(),
    //                     vec![Expr::Var(UnresolvedVar::new(
    //                         "y",
    //                         Span::new(1, 8, 1, 8)
    //                     )),],
    //                     Span::new(1, 5, 1, 9)
    //                 ),
    //                 Expr::Var(UnresolvedVar::new("z", Span::new(1, 11, 1, 11)))
    //             ],
    //             Span::new(1, 1, 1, 11)
    //         ),
    //     );
    // }

    // #[test]
    // fn test_lambda() {
    //     let mut parser = Parser::new(Token::tokenize(r#"\x -> x"#).unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Lambda(
    //             vec![UnresolvedVar::new("x", Span::new(1, 2, 1, 2))],
    //             Expr::Var(UnresolvedVar::new("x", Span::new(1, 7, 1, 7))).into(),
    //             Span::new(1, 1, 1, 7)
    //         ),
    //     );

    //     let mut parser = Parser::new(Token::tokenize(r#"\f x y -> f y x"#).unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Lambda(
    //             vec![
    //                 UnresolvedVar::new("f", Span::new(1, 2, 1, 2)),
    //                 UnresolvedVar::new("x", Span::new(1, 4, 1, 4)),
    //                 UnresolvedVar::new("y", Span::new(1, 6, 1, 6)),
    //             ],
    //             Expr::Call(
    //                 Expr::Var(UnresolvedVar::new("f", Span::new(1, 11, 1, 11))).into(),
    //                 vec![
    //                     Expr::Var(UnresolvedVar::new("y", Span::new(1, 13, 1, 13))),
    //                     Expr::Var(UnresolvedVar::new("x", Span::new(1, 15, 1, 15)))
    //                 ],
    //                 Span::new(1, 11, 1, 15)
    //             )
    //             .into(),
    //             Span::new(1, 1, 1, 15)
    //         ),
    //     );

    //     let mut parser =
    //         Parser::new(Token::tokenize(r#"(\f x y -> x + y) 1.0 3.54"#).unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Call(
    //             Expr::Lambda(
    //                 vec![
    //                     UnresolvedVar::new("f", Span::new(1, 3, 1, 3)),
    //                     UnresolvedVar::new("x", Span::new(1, 5, 1, 5)),
    //                     UnresolvedVar::new("y", Span::new(1, 7, 1, 7)),
    //                 ],
    //                 Expr::Call(
    //                     Expr::Var(UnresolvedVar::new("+", Span::new(1, 14, 1, 14)))
    //                         .into(),
    //                     vec![
    //                         Expr::Var(UnresolvedVar::new("x", Span::new(1, 12, 1, 12))),
    //                         Expr::Var(UnresolvedVar::new("y", Span::new(1, 16, 1, 16)))
    //                     ],
    //                     Span::new(1, 12, 1, 16)
    //                 )
    //                 .into(),
    //                 Span::new(1, 1, 1, 17)
    //             )
    //             .into(),
    //             vec![
    //                 Expr::Float(1.0, Span::new(1, 19, 1, 21)),
    //                 Expr::Float(3.54, Span::new(1, 23, 1, 26))
    //             ],
    //             Span::new(1, 1, 1, 26)
    //         ),
    //     );
    // }

    //     #[test]
    //     fn test_let_in() {
    //         let mut parser =
    //             Parser::new(Token::tokenize(r#"let x = 1.0, y = 3.54 in x + y"#).unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         assert_eq!(
    //             expr,
    //             Expr::Call(
    //                 Expr::Lambda(
    //                     vec!["x".to_string()],
    //                     Expr::Call(
    //                         Expr::Lambda(
    //                             vec!["y".to_string()],
    //                             Expr::Call(
    //                                 Expr::Var("+".to_string(), Span::new(1, 28, 1, 28))
    //                                     .into(),
    //                                 vec![
    //                                     Expr::Var("x".to_string(), Span::new(1, 26, 1, 26)),
    //                                     Expr::Var("y".to_string(), Span::new(1, 30, 1, 30))
    //                                 ],
    //                                 Span::new(1, 26, 1, 30)
    //                             )
    //                             .into(),
    //                             Span::new(1, 1, 1, 30)
    //                         )
    //                         .into(),
    //                         vec![Expr::Float(3.54, Span::new(1, 18, 1, 21))],
    //                         Span::new(1, 1, 1, 30)
    //                     )
    //                     .into(),
    //                     Span::new(1, 1, 1, 30)
    //                 )
    //                 .into(),
    //                 vec![Expr::Float(1.0, Span::new(1, 9, 1, 11)),],
    //                 Span::new(1, 1, 1, 30)
    //             ),
    //         );
    //     }

    //     #[test]
    //     fn test_precedence() {
    //         let mut parser = Parser::new(Token::tokenize("x + y + z").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         assert_eq!(
    //             expr,
    //             Expr::Call(
    //                 Expr::Var("+".to_string(), Span::new(1, 7, 1, 7)).into(),
    //                 vec![
    //                     Expr::Call(
    //                         Expr::Var("+".to_string(), Span::new(1, 3, 1, 3)).into(),
    //                         vec![
    //                             Expr::Var("x".to_string(), Span::new(1, 1, 1, 1)),
    //                             Expr::Var("y".to_string(), Span::new(1, 5, 1, 5)),
    //                         ],
    //                         Span::new(1, 1, 1, 5)
    //                     ),
    //                     Expr::Var("z".to_string(), Span::new(1, 9, 1, 9)),
    //                 ],
    //                 Span::new(1, 1, 1, 9)
    //             ),
    //         );

    //         let mut parser = Parser::new(Token::tokenize("f x + g y").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         assert_eq!(
    //             expr,
    //             Expr::Call(
    //                 Expr::Var("+".to_string(), Span::new(1, 5, 1, 5)).into(),
    //                 vec![
    //                     Expr::Call(
    //                         Expr::Var("f".to_string(), Span::new(1, 1, 1, 1)).into(),
    //                         vec![Expr::Var(
    //                             "x".to_string(),
    //                             Span::new(1, 3, 1, 3)
    //                         ),],
    //                         Span::new(1, 1, 1, 3)
    //                     ),
    //                     Expr::Call(
    //                         Expr::Var("g".to_string(), Span::new(1, 7, 1, 7)).into(),
    //                         vec![Expr::Var(
    //                             "y".to_string(),
    //                             Span::new(1, 9, 1, 9)
    //                         ),],
    //                         Span::new(1, 7, 1, 9)
    //                     ),
    //                 ],
    //                 Span::new(1, 1, 1, 9)
    //             ),
    //         );

    //         let mut parser = Parser::new(Token::tokenize("f . g x").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         assert_eq!(
    //             expr,
    //             Expr::Call(
    //                 Expr::Var(".".to_string(), Span::new(1, 3, 1, 3)).into(),
    //                 vec![
    //                     Expr::Var("f".to_string(), Span::new(1, 1, 1, 1)),
    //                     Expr::Call(
    //                         Expr::Var("g".to_string(), Span::new(1, 5, 1, 5)).into(),
    //                         vec![Expr::Var(
    //                             "x".to_string(),
    //                             Span::new(1, 7, 1, 7)
    //                         ),],
    //                         Span::new(1, 5, 1, 7)
    //                     ),
    //                 ],
    //                 Span::new(1, 1, 1, 7)
    //             ),
    //         );

    //         let mut parser = Parser::new(Token::tokenize("(f . g) x").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         assert_eq!(
    //             expr,
    //             Expr::Call(
    //                 Expr::Call(
    //                     Expr::Var(".".to_string(), Span::new(1, 4, 1, 4)).into(),
    //                     vec![
    //                         Expr::Var("f".to_string(), Span::new(1, 2, 1, 2)),
    //                         Expr::Var("g".to_string(), Span::new(1, 6, 1, 6)),
    //                     ],
    //                     Span::new(1, 1, 1, 7)
    //                 )
    //                 .into(),
    //                 vec![Expr::Var(
    //                     "x".to_string(),
    //                     Span::new(1, 9, 1, 9)
    //                 ),],
    //                 Span::new(1, 1, 1, 9)
    //             ),
    //         );

    //         let mut parser = Parser::new(Token::tokenize("f x + g y * h z").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         assert_eq!(
    //             expr,
    //             Expr::Call(
    //                 Expr::Var("+".to_string(), Span::new(1, 5, 1, 5)).into(),
    //                 vec![
    //                     Expr::Call(
    //                         Expr::Var("f".to_string(), Span::new(1, 1, 1, 1)).into(),
    //                         vec![Expr::Var(
    //                             "x".to_string(),
    //                             Span::new(1, 3, 1, 3)
    //                         ),],
    //                         Span::new(1, 1, 1, 3)
    //                     ),
    //                     Expr::Call(
    //                         Expr::Var("*".to_string(), Span::new(1, 11, 1, 11)).into(),
    //                         vec![
    //                             Expr::Call(
    //                                 Expr::Var("g".to_string(), Span::new(1, 7, 1, 7))
    //                                     .into(),
    //                                 vec![Expr::Var(
    //                                     "y".to_string(),
    //                                     Span::new(1, 9, 1, 9)
    //                                 ),],
    //                                 Span::new(1, 7, 1, 9)
    //                             ),
    //                             Expr::Call(
    //                                 Expr::Var("h".to_string(), Span::new(1, 13, 1, 13))
    //                                     .into(),
    //                                 vec![Expr::Var(
    //                                     "z".to_string(),
    //                                     Span::new(1, 15, 1, 15)
    //                                 ),],
    //                                 Span::new(1, 13, 1, 15)
    //                             ),
    //                         ],
    //                         Span::new(1, 7, 1, 15)
    //                     ),
    //                 ],
    //                 Span::new(1, 1, 1, 15)
    //             ),
    //         );

    //         let mut parser = Parser::new(Token::tokenize("f . g (h x) . i").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         assert_eq!(
    //             expr,
    //             Expr::Call(
    //                 Expr::Var(".".to_string(), Span::new(1, 3, 1, 3)).into(),
    //                 vec![
    //                     Expr::Var("f".to_string(), Span::new(1, 1, 1, 1)),
    //                     Expr::Call(
    //                         Expr::Var(".".to_string(), Span::new(1, 13, 1, 13)).into(),
    //                         vec![
    //                             Expr::Call(
    //                                 Expr::Var("g".to_string(), Span::new(1, 5, 1, 5))
    //                                     .into(),
    //                                 vec![Expr::Call(
    //                                     Expr::Var("h".to_string(), Span::new(1, 8, 1, 8))
    //                                         .into(),
    //                                     vec![Expr::Var(
    //                                         "x".to_string(),
    //                                         Span::new(1, 10, 1, 10)
    //                                     ),],
    //                                     Span::new(1, 7, 1, 11)
    //                                 ),],
    //                                 Span::new(1, 5, 1, 11)
    //                             ),
    //                             Expr::Var("i".to_string(), Span::new(1, 15, 1, 15)),
    //                         ],
    //                         Span::new(1, 5, 1, 15)
    //                     ),
    //                 ],
    //                 Span::new(1, 1, 1, 15)
    //             ),
    //         );
    //     }

    // #[test]
    // fn test_newline() {
    //     let mut parser = Parser::new(
    //         Token::tokenize(
    //             "test.rex",
    //             r#"
    // \x y z ->
    //  map
    //   (f . g (h z) . i)
    //   (j x y (k z))"#,
    //         )
    //         .unwrap(),
    //     );
    //     let expr = parser.parse_expr().unwrap();
    //     assert_eq!(
    //         expr,
    //         Expr::Lambda(
    //             vec!["x".to_string(), "y".to_string(), "z".to_string()],
    //             Expr::Call(
    //                 Expr::Var("map".to_string(), Span::new(3, 2, 3, 4)).into(),
    //                 vec![
    //                     Expr::Call(
    //                         Expr::Var(".".to_string(), Span::new(4, 6, 4, 6)).into(),
    //                         vec![
    //                             Expr::Var("f".to_string(), Span::new(4, 4, 4, 4)),
    //                             Expr::Call(
    //                                 Expr::Var(".".to_string(), Span::new(4, 16, 4, 16))
    //                                     .into(),
    //                                 vec![
    //                                     Expr::Call(
    //                                         Expr::Var(
    //                                             "g".to_string(),
    //                                             Span::new(4, 8, 4, 8)
    //                                         )
    //                                         .into(),
    //                                         vec![Expr::Call(
    //                                             Expr::Var(
    //                                                 "h".to_string(),
    //                                                 Span::new(4, 11, 4, 11)
    //                                             )
    //                                             .into(),
    //                                             vec![Expr::Var(
    //                                                 "z".to_string(),
    //                                                 Span::new(4, 13, 4, 13)
    //                                             ),],
    //                                             Span::new(4, 10, 4, 14)
    //                                         ),],
    //                                         Span::new(4, 8, 4, 14)
    //                                     ),
    //                                     Expr::Var(
    //                                         "i".to_string(),
    //                                         Span::new(4, 18, 4, 18)
    //                                     ),
    //                                 ],
    //                                 Span::new(4, 8, 4, 18)
    //                             ),
    //                         ],
    //                         Span::new(4, 3, 4, 19)
    //                     ),
    //                     Expr::Call(
    //                         Expr::Var("j".to_string(), Span::new(5, 4, 5, 4)).into(),
    //                         vec![
    //                             Expr::Var("x".to_string(), Span::new(5, 6, 5, 6)),
    //                             Expr::Var("y".to_string(), Span::new(5, 8, 5, 8)),
    //                             Expr::Call(
    //                                 Expr::Var("k".to_string(), Span::new(5, 11, 5, 11))
    //                                     .into(),
    //                                 vec![Expr::Var(
    //                                     "z".to_string(),
    //                                     Span::new(5, 13, 5, 13)
    //                                 ),],
    //                                 Span::new(5, 10, 5, 14)
    //                             ),
    //                         ],
    //                         Span::new(5, 3, 5, 15)
    //                     ),
    //                 ],
    //                 Span::new(3, 2, 5, 15)
    //             )
    //             .into(),
    //             Span::new(2, 1, 5, 15)
    //         ),
    //     );
    // }
}
