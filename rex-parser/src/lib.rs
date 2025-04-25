use std::{collections::VecDeque, vec};

use rex_ast::expr::{Expr, Scope, Var};
use rex_lexer::{
    span::{Position, Span, Spanned},
    Token, Tokens,
};

use crate::{error::ParserErr, op::Operator};

pub mod error;
pub mod op;

pub struct Parser {
    token_cursor: usize,
    tokens: Vec<Token>,
    eof: Span,
    errors: Vec<ParserErr>,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Parser {
        let mut parser = Parser {
            token_cursor: 0,
            tokens: tokens
                .items
                .into_iter()
                .filter_map(|token| match token {
                    Token::Whitespace(..) | Token::WhitespaceNewline(..) => None,
                    token => Some(token),
                })
                .collect(),
            eof: tokens.eof,
            errors: Vec::new(),
        };
        parser.strip_comments();
        parser
    }

    fn current_token(&self) -> Token {
        if self.token_cursor < self.tokens.len() {
            self.tokens[self.token_cursor].clone()
        } else {
            Token::Eof(self.eof)
        }
    }

    fn peek_token(&self, n: usize) -> Token {
        if self.token_cursor + n < self.tokens.len() {
            self.tokens[self.token_cursor + n].clone()
        } else {
            Token::Eof(self.eof)
        }
    }

    fn next_token(&mut self) {
        self.token_cursor += 1;
    }

    fn strip_comments(&mut self) {
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

    fn record_error(&mut self, e: ParserErr) {
        self.errors.push(e);
    }

    pub fn parse_program(&mut self) -> Result<Expr, Vec<ParserErr>> {
        match self.parse_expr() {
            Ok(expr) => {
                // Make sure there's no trailing tokens. The whole program
                // should be one expression.
                match self.current_token() {
                    Token::Eof(..) => {}
                    token => self.record_error(ParserErr::new(
                        *token.span(),
                        format!("unexpected {}", token),
                    )),
                }

                if self.errors.len() > 0 {
                    Err(self.errors.clone())
                } else {
                    Ok(expr)
                }
            }
            Err(e) => {
                self.record_error(e);
                Err(self.errors.clone())
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParserErr> {
        let lhs_expr = self.parse_unary_expr()?;
        self.parse_binary_expr(lhs_expr)
    }

    fn parse_binary_expr(&mut self, lhs_expr: Expr) -> Result<Expr, ParserErr> {
        let lhs_expr_span = lhs_expr.span();

        // Get the next token.
        let token = match self.current_token() {
            // Having no next token should finish the parsing of the binary
            // expression.
            Token::Eof(..) => return Ok(lhs_expr),
            token => token,
        };
        let prec = token.precedence();

        // Parse the binary operator.
        let operator = match token {
            Token::Add(..) => Operator::Add,
            Token::And(..) => Operator::And,
            Token::Concat(..) => Operator::Concat,
            Token::Div(..) => Operator::Div,
            Token::Dot(..) => Operator::Dot,
            Token::Eq(..) => Operator::Eq,
            Token::Ge(..) => Operator::Ge,
            Token::Gt(..) => Operator::Gt,
            Token::Le(..) => Operator::Le,
            Token::Lt(..) => Operator::Lt,
            Token::Mul(..) => Operator::Mul,
            Token::Or(..) => Operator::Or,
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
            // No more tokens
            Token::Eof(..) => false,
            // Next token has lower precedence
            token if prec > token.precedence() => false,
            // Next token has the same precedence
            token if prec == token.precedence() => match token {
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
            _ => true,
        };

        let rhs_expr = if next_binary_expr_takes_precedence {
            self.parse_binary_expr(rhs_expr)?
        } else {
            rhs_expr
        };

        let inner_span = Span::from_begin_end(lhs_expr_span.begin, operator_span.end);
        let outer_span = Span::from_begin_end(lhs_expr_span.begin, rhs_expr_span.end);

        self.parse_binary_expr(Expr::App(
            outer_span,
            Box::new(Expr::App(
                inner_span,
                Box::new(Expr::Var(Var::with_span(
                    *operator_span,
                    operator.to_string(),
                ))),
                Box::new(lhs_expr),
            )),
            Box::new(rhs_expr),
        ))
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ParserErr> {
        let mut call_base_expr = match self.current_token() {
            Token::ParenL(..) => self.parse_paren_expr(),
            Token::BracketL(..) => self.parse_bracket_expr(),
            Token::BraceL(..) => self.parse_brace_expr(),
            Token::Bool(..) => self.parse_literal_bool_expr(),
            Token::Float(..) => self.parse_literal_float_expr(),
            Token::Int(..) => self.parse_literal_int_expr(),
            Token::String(..) => self.parse_literal_str_expr(),
            Token::Ident(..) => self.parse_ident_expr(),
            Token::BackSlash(..) => self.parse_lambda_expr(),
            Token::Let(..) => self.parse_let_expr(),
            Token::If(..) => self.parse_if_expr(),
            Token::Sub(..) => self.parse_neg_expr(),
            Token::Eof(span) => {
                return Err(ParserErr::new(span, format!("unexpected EOF")));
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("unexpected {}", token),
                ));
            }
        }?;
        let call_base_expr_span = *call_base_expr.span();

        let mut call_arg_exprs = VecDeque::new();
        loop {
            let call_arg_expr = match self.current_token() {
                Token::ParenL(..) => self.parse_paren_expr(),
                Token::BracketL(..) => self.parse_bracket_expr(),
                Token::BraceL(..) => self.parse_brace_expr(),
                Token::Bool(..) => self.parse_literal_bool_expr(),
                Token::Float(..) => self.parse_literal_float_expr(),
                Token::Int(..) => self.parse_literal_int_expr(),
                Token::String(..) => self.parse_literal_str_expr(),
                Token::Ident(..) => self.parse_ident_expr(),
                Token::BackSlash(..) => self.parse_lambda_expr(),
                Token::Let(..) => self.parse_let_expr(),
                Token::If(..) => self.parse_if_expr(),
                _ => break,
            }?;
            call_arg_exprs.push_back(call_arg_expr);
        }

        while let Some(call_arg_expr) = call_arg_exprs.pop_front() {
            let call_arg_expr_span_end = call_arg_expr.span().end;
            call_base_expr = Expr::App(
                Span::from_begin_end(call_base_expr_span.begin, call_arg_expr_span_end),
                Box::new(call_base_expr),
                Box::new(call_arg_expr),
            );
        }
        Ok(call_base_expr)
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, ParserErr> {
        // Eat the left parenthesis.
        let span_begin = match self.current_token() {
            Token::ParenL(span, ..) => {
                self.next_token();
                span
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `(` got {}", token),
                ));
            }
        };

        // Parse the inner expression.
        let mut expr = match self.current_token() {
            Token::ParenR(span, ..) => {
                self.next_token();
                // Empty tuple
                return Ok(Expr::Tuple(
                    Span::from_begin_end(span_begin.begin, span.end),
                    vec![],
                ));
            }
            Token::Add(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "+"))
            }
            Token::And(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "&&"))
            }
            Token::Concat(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "++"))
            }
            Token::Div(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "/"))
            }
            Token::Dot(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "."))
            }
            Token::Eq(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "=="))
            }
            Token::Ge(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, ">="))
            }
            Token::Gt(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, ">"))
            }
            Token::Le(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "<="))
            }
            Token::Lt(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "<"))
            }
            Token::Mul(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "*"))
            }
            Token::Or(span, ..) => {
                self.next_token();
                Expr::Var(Var::with_span(span, "||"))
            }
            Token::Sub(span, ..) => {
                if let Token::ParenR(..) = self.peek_token(1) {
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
                    Expr::Var(Var::with_span(span, "-"))
                } else {
                    self.parse_expr()?
                }
            }
            _ => self.parse_expr()?,
        };

        // Eat the right parenthesis.
        let span_end = match self.current_token() {
            Token::ParenR(span, ..) => {
                self.next_token();
                span
            }
            Token::Comma(..) => {
                // parse inner expressions
                return self.parse_tuple(span_begin, expr);
            }
            token => {
                self.record_error(ParserErr::new(*token.span(), "expected `)`"));
                return Ok(expr);
            }
        };

        expr.set_span_begin_end(span_begin.begin, span_end.end);

        Ok(expr)
    }

    fn parse_tuple(&mut self, span_begin: Span, first_item: Expr) -> Result<Expr, ParserErr> {
        let mut items = vec![first_item];
        loop {
            // eat the comma
            match self.current_token() {
                Token::Comma(..) => self.next_token(),
                Token::ParenR(end_span) => {
                    self.next_token();
                    return Ok(Expr::Tuple(
                        Span::from_begin_end(span_begin.begin, end_span.end),
                        items,
                    ));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_bracket_expr(&mut self) -> Result<Expr, ParserErr> {
        // Eat the left bracket.
        let span_begin = match self.current_token() {
            Token::BracketL(span, ..) => {
                self.next_token();
                span.begin
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `[` got {}", token),
                ));
            }
        };

        let mut exprs = Vec::new();
        loop {
            if let Token::BracketR(..) = self.current_token() {
                break;
            }

            // Parse the next expression.
            exprs.push(self.parse_expr()?);
            // Eat the comma.
            match self.current_token() {
                Token::Comma(..) => self.next_token(),
                Token::Eof(span) => {
                    self.record_error(ParserErr::new(span, "expected `,` or `]`"));
                    break;
                }
                _ => {
                    break;
                }
            };
        }

        // Eat the right bracket.
        let span_end = match self.current_token() {
            Token::BracketR(span, ..) => {
                self.next_token();
                span.end
            }
            token => {
                self.record_error(ParserErr::new(
                    *token.span(),
                    format!("expected `]` got {}", token),
                ));

                return Ok(Expr::List(
                    Span::from_begin_end(span_begin, token.span().end),
                    exprs,
                ));
            }
        };

        Ok(Expr::List(
            Span::from_begin_end(span_begin, span_end),
            exprs,
        ))
    }

    fn parse_brace_expr(&mut self) -> Result<Expr, ParserErr> {
        // Eat the left brace.
        let span_begin = match self.current_token() {
            Token::BraceL(span, ..) => {
                self.next_token();
                span.begin
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `[` got {}", token),
                ));
            }
        };

        let mut kvs = Vec::new();
        loop {
            if let Token::BraceR(_, ..) = self.current_token() {
                break;
            }

            // Parse the ident.
            let var = match self.parse_ident_expr()? {
                Expr::Var(var) => var,
                _ => unreachable!(),
            };
            // Eat the =.
            match self.current_token() {
                Token::Assign(..) => self.next_token(),
                token => {
                    self.record_error(ParserErr::new(*token.span(), "expected `=`"));
                    break;
                }
            };
            // Parse the expression.
            kvs.push((var.name, self.parse_expr()?));
            // Eat the comma.
            match self.current_token() {
                Token::Comma(..) => self.next_token(),
                Token::Eof(span) => {
                    self.record_error(ParserErr::new(span, "expected `,` or `}}`"));
                    break;
                }
                _ => {
                    break;
                }
            };
        }

        // Eat the right brace.
        let span_end = match self.current_token() {
            Token::BraceR(span, ..) => {
                self.next_token();
                span.end
            }
            token => {
                self.record_error(ParserErr::new(
                    *token.span(),
                    format!("expected `}}` got {}", token),
                ));

                return Ok(Expr::Dict(
                    Span::from_begin_end(span_begin, Position::new(0, 0)),
                    kvs.into_iter().collect(),
                ));
            }
        };

        Ok(Expr::Dict(
            Span::from_begin_end(span_begin, span_end),
            kvs.into_iter().collect(),
        ))
    }

    fn parse_neg_expr(&mut self) -> Result<Expr, ParserErr> {
        // Eat the minus.
        let span_token = match self.current_token() {
            Token::Sub(span, ..) => {
                self.next_token();
                span
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `-` got {}", token),
                ));
            }
        };

        // Parse the inner expression.
        let expr = self.parse_expr()?;
        let expr_span_end = expr.span().end;

        // Return the negative expression.
        Ok(Expr::App(
            Span::from_begin_end(span_token.begin, expr_span_end),
            Box::new(Expr::Var(Var::with_span(span_token, "negate"))),
            Box::new(expr),
        ))
    }

    //
    fn parse_lambda_expr(&mut self) -> Result<Expr, ParserErr> {
        // Eat the backslash.
        let span_begin = match self.current_token() {
            Token::BackSlash(span, ..) => {
                self.next_token();
                span.begin
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `\\` got {}", token),
                ));
            }
        };

        // Parse the params.
        let mut params = VecDeque::new();
        loop {
            match self.current_token() {
                Token::Ident(param, span, ..) => {
                    self.next_token();
                    params.push_back((span, param));
                }
                _ => break,
            }
        }

        // Parse the arrow.
        let _span_arrow = match self.current_token() {
            Token::ArrowR(span, ..) => {
                self.next_token();
                span
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `->` got {}", token),
                ));
            }
        };

        // Parse the body
        let mut body = self.parse_expr()?;
        let mut body_span_end = body.span().end;
        while let Some((param_span, param)) = params.pop_back() {
            body = Expr::Lam(
                Span::from_begin_end(param_span.begin, body_span_end),
                Scope::new_sync(),
                Var::with_span(param_span, param),
                Box::new(body),
            );
            body_span_end = body.span().end;
        }
        // Adjust the outer most lambda to include the initial backslash
        body.set_span_begin(span_begin);

        Ok(body)
    }

    //
    fn parse_let_expr(&mut self) -> Result<Expr, ParserErr> {
        // Eat the `let` token
        let span_begin = match self.current_token() {
            Token::Let(span, ..) => {
                self.next_token();
                span.begin
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `let` got {}", token),
                ));
            }
        };

        // Parse the variable declarations.
        let mut decls = VecDeque::new();
        loop {
            // Variable name
            let var = match self.current_token() {
                Token::Ident(val, span, ..) => {
                    self.next_token();
                    (span, val)
                }
                _ => break,
            };
            // =
            match self.current_token() {
                Token::Assign(_span, ..) => {
                    self.next_token();
                }
                token => {
                    return Err(ParserErr::new(
                        *token.span(),
                        format!("expected `=` got {}", token),
                    ));
                }
            }
            // Parse the variable definition
            decls.push_back((var, self.parse_expr()?));
            // Parse `,` or `in`
            match self.current_token() {
                Token::Comma(_span, ..) => {
                    self.next_token();
                    continue;
                }
                Token::In(..) => break,
                token => {
                    return Err(ParserErr::new(
                        *token.span(),
                        format!("expected `,` or `in` got {}", token),
                    ));
                }
            }
        }

        // Parse the `in` token
        let _span_arrow = match self.current_token() {
            Token::In(span, ..) => {
                self.next_token();
                span
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `in` got {}", token),
                ));
            }
        };

        // Parse the body
        let mut body = self.parse_expr()?;
        let mut body_span_end = body.span().end;
        while let Some(((var_span, var), def)) = decls.pop_back() {
            body = Expr::Let(
                Span::from_begin_end(var_span.begin, body_span_end),
                Var::with_span(var_span, var),
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
    fn parse_if_expr(&mut self) -> Result<Expr, ParserErr> {
        // Eat the `if` token
        let span_begin = match self.current_token() {
            Token::If(span, ..) => {
                self.next_token();
                span.begin
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `if` got {}", token),
                ));
            }
        };

        // Parse the cond expression
        let cond = self.parse_expr()?;

        // Parse the `then` token
        let _span_arrow = match self.current_token() {
            Token::Then(span, ..) => {
                self.next_token();
                span
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `then` got {}", token),
                ));
            }
        };

        // Parse the then expression
        let then = self.parse_expr()?;

        // Parse the `else` token
        let _span_arrow = match self.current_token() {
            Token::Else(span, ..) => {
                self.next_token();
                span
            }
            token => {
                return Err(ParserErr::new(
                    *token.span(),
                    format!("expected `else` got {}", token),
                ));
            }
        };

        // Parse the else expression
        let r#else = self.parse_expr()?;
        let else_span_end = r#else.span().end;

        Ok(Expr::Ite(
            Span::from_begin_end(span_begin, else_span_end),
            Box::new(cond),
            Box::new(then),
            Box::new(r#else),
        ))
    }

    //
    fn parse_literal_bool_expr(&mut self) -> Result<Expr, ParserErr> {
        let token = self.current_token();
        self.next_token();
        match token {
            Token::Bool(val, span, ..) => Ok(Expr::Bool(span, val)),
            token => Err(ParserErr::new(
                *token.span(),
                format!("expected `bool` got {}", token),
            )),
        }
    }

    //
    fn parse_literal_float_expr(&mut self) -> Result<Expr, ParserErr> {
        let token = self.current_token();
        self.next_token();
        match token {
            Token::Float(val, span, ..) => Ok(Expr::Float(span, val)),
            token => Err(ParserErr::new(
                *token.span(),
                format!("expected `float` got {}", token),
            )),
        }
    }

    //
    fn parse_literal_int_expr(&mut self) -> Result<Expr, ParserErr> {
        let token = self.current_token();
        self.next_token();
        match token {
            Token::Int(val, span, ..) => Ok(Expr::Uint(span, val)),
            token => Err(ParserErr::new(
                *token.span(),
                format!("expected `int` got {}", token),
            )),
        }
    }

    //
    fn parse_literal_str_expr(&mut self) -> Result<Expr, ParserErr> {
        let token = self.current_token();
        self.next_token();
        match token {
            Token::String(val, span, ..) => Ok(Expr::String(span, val)),
            token => Err(ParserErr::new(
                *token.span(),
                format!("expected `str` got {}", token),
            )),
        }
    }

    //
    fn parse_ident_expr(&mut self) -> Result<Expr, ParserErr> {
        let token = self.current_token();
        self.next_token();
        match token {
            Token::Ident(name, span, ..) => Ok(Expr::Var(Var::with_span(span, name))),
            token => Err(ParserErr::new(
                *token.span(),
                format!("expected `ident` got {}", token),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::ParserErr;
    use rex_ast::{app, assert_expr_eq, b, f, tup, u, v};
    use rex_lexer::{span, Token};

    use super::*;

    #[test]
    fn test_parse_comment() {
        let mut parser = Parser::new(Token::tokenize("true {- this is a boolean -}").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(expr, b!(span!(1:1 - 1:5); true));

        let mut parser = Parser::new(Token::tokenize("{- this is a boolean -} false").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(expr, b!(span!(1:25 - 1:30); false));

        let mut parser = Parser::new(Token::tokenize("(3.54 {- this is a float -}, {- this is an int -} 42, false {- this is a boolean -})").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            tup!(
                span!(1:1 - 1:85);
                f!(span!(1:2 - 1:6); 3.54),
                u!(span!(1:51 - 1:53); 42),
                b!(span!(1:55 - 1:60); false),
            )
        );
    }

    #[test]
    fn test_add() {
        let mut parser = Parser::new(Token::tokenize("1 + 2").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:6);
                app!(
                    span!(1:1 - 1:4);
                    v!(span!(1:3 - 1:4); "+"),
                    u!(span!(1:1 - 1:2); 1)
                ),
                u!(span!(1:5 - 1:6); 2)
            )
        );

        let mut parser = Parser::new(Token::tokenize("(6.9 + 3.14)").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:13);
                app!(
                    span!(1:2 - 1:7);
                    v!(span!(1:6 - 1:7); "+"),
                    f!(span!(1:2 - 1:5); 6.9)
                ),
                f!(span!(1:8 - 1:12); 3.14)
            )
        );

        let mut parser = Parser::new(Token::tokenize("(+) 420").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:8);
                v!(span!(1:1 - 1:4); "+"),
                u!(span!(1:5 - 1:8); 420)
            )
        );
    }

    #[test]
    fn test_sub() {
        let mut parser = Parser::new(Token::tokenize("1 - 2").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:6);
                app!(
                    span!(1:1 - 1:4);
                    v!(span!(1:3 - 1:4); "-"),
                    u!(span!(1:1 - 1:2); 1)
                ),
                u!(span!(1:5 - 1:6); 2)
            )
        );

        let mut parser = Parser::new(Token::tokenize("(6.9 - 3.14)").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:13);
                app!(
                    span!(1:2 - 1:7);
                    v!(span!(1:6 - 1:7); "-"),
                    f!(span!(1:2 - 1:5); 6.9)
                ),
                f!(span!(1:8 - 1:12); 3.14)
            )
        );

        let mut parser = Parser::new(Token::tokenize("(-) 4.20").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:9);
                v!(span!(1:1 - 1:4); "-"),
                f!(span!(1:5 - 1:9); 4.20)
            )
        );
    }

    #[test]
    fn test_negate() {
        let mut parser = Parser::new(Token::tokenize("-1").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:3);
                v!(span!(1:1 - 1:2); "negate"),
                u!(span!(1:2 - 1:3); 1)
            )
        );

        let mut parser = Parser::new(Token::tokenize("(-1)").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:5);
                v!(span!(1:2 - 1:3); "negate"),
                u!(span!(1:3 - 1:4); 1)
            )
        );

        let mut parser = Parser::new(Token::tokenize("(- 6.9)").unwrap());
        let expr = parser.parse_program().unwrap();
        assert_expr_eq!(
            expr,
            app!(
                span!(1:1 - 1:8);
                v!(span!(1:2 - 1:3); "negate"),
                f!(span!(1:4 - 1:7); 6.9)
            )
        );
    }

    #[test]
    fn test_errors() {
        let mut parser = Parser::new(Token::tokenize("1 + 2 + in + 3").unwrap());
        let res = parser.parse_program();
        assert_eq!(
            res,
            Err(vec![ParserErr::new(
                Span::new(1, 9, 1, 11),
                "unexpected in"
            )])
        );

        let mut parser = Parser::new(Token::tokenize("1 + 2 in + 3").unwrap());
        let res = parser.parse_program();
        assert_eq!(
            res,
            Err(vec![ParserErr::new(Span::new(1, 7, 1, 9), "unexpected in")])
        );

        let mut parser = Parser::new(Token::tokenize("get 0 [    ").unwrap());
        let res = parser.parse_program();
        assert_eq!(
            res,
            Err(vec![ParserErr::new(
                Span::new(1, 12, 1, 12),
                "unexpected EOF"
            )])
        );

        let mut parser = Parser::new(Token::tokenize("elem0 [  ").unwrap());
        let res = parser.parse_program();
        assert_eq!(
            res,
            Err(vec![ParserErr::new(
                Span::new(1, 10, 1, 10),
                "unexpected EOF"
            )])
        );

        let mut parser = Parser::new(
            Token::tokenize(
                "
            { a = 1, b }
            { a = 1, b = 2, c }
            { a = 1, b = 3, c = 3, d }
            ",
            )
            .unwrap(),
        );
        let res = parser.parse_program();
        assert_eq!(
            res,
            Err(vec![
                ParserErr::new(Span::new(2, 24, 2, 25), "expected `=`"),
                ParserErr::new(Span::new(3, 31, 3, 32), "expected `=`"),
                ParserErr::new(Span::new(4, 38, 4, 39), "expected `=`")
            ])
        );
    }
}
