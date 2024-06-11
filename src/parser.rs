use std::{collections::BTreeMap, fmt};

use crate::{
    lexer::{Token, Tokens},
    span::{Span, Spanned},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    And,
    Concat,
    Div,
    Dot,
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
    Mul,
    Or,
    Sub,
}

impl Operator {
    pub fn to_string(&self) -> String {
        match self {
            Operator::Add => "+".to_string(),
            Operator::And => "&&".to_string(),
            Operator::Concat => "++".to_string(),
            Operator::Div => "/".to_string(),
            Operator::Dot => ".".to_string(),
            Operator::Eq => "==".to_string(),
            Operator::Ne => "!=".to_string(),
            Operator::Ge => ">=".to_string(),
            Operator::Gt => ">".to_string(),
            Operator::Le => "<=".to_string(),
            Operator::Lt => "<".to_string(),
            Operator::Mul => "*".to_string(),
            Operator::Or => "||".to_string(),
            Operator::Sub => "-".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum Expr {
    Null(#[cfg_attr(feature = "serde", serde(skip))] Span),
    Bool(bool, #[cfg_attr(feature = "serde", serde(skip))] Span),
    Int(u64, #[cfg_attr(feature = "serde", serde(skip))] Span),
    Float(f64, #[cfg_attr(feature = "serde", serde(skip))] Span),
    String(String, #[cfg_attr(feature = "serde", serde(skip))] Span),
    List(Vec<Expr>, #[cfg_attr(feature = "serde", serde(skip))] Span),
    Record(
        BTreeMap<String, Expr>,
        #[cfg_attr(feature = "serde", serde(skip))] Span,
    ),
    Var(String, #[cfg_attr(feature = "serde", serde(skip))] Span),
    Call(
        Box<Expr>,
        Vec<Expr>,
        #[cfg_attr(feature = "serde", serde(skip))] Span,
    ),
    Lambda(
        Vec<String>,
        Box<Expr>,
        #[cfg_attr(feature = "serde", serde(skip))] Span,
    ),
}

impl Spanned for Expr {
    fn span(&self) -> &Span {
        match self {
            Expr::Null(span, ..) => span,
            Expr::Bool(_, span, ..) => span,
            Expr::Int(_, span, ..) => span,
            Expr::Float(_, span, ..) => span,
            Expr::String(_, span, ..) => span,
            Expr::List(_, span, ..) => span,
            Expr::Record(_, span, ..) => span,
            Expr::Var(_, span, ..) => span,
            Expr::Call(_, _, span, ..) => span,
            Expr::Lambda(_, _, span, ..) => span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Expr::Null(span, ..) => span,
            Expr::Bool(_, span, ..) => span,
            Expr::Int(_, span, ..) => span,
            Expr::Float(_, span, ..) => span,
            Expr::String(_, span, ..) => span,
            Expr::List(_, span, ..) => span,
            Expr::Record(_, span, ..) => span,
            Expr::Var(_, span, ..) => span,
            Expr::Call(_, _, span, ..) => span,
            Expr::Lambda(_, _, span, ..) => span,
        }
    }
}

pub struct ParserErr {
    span: Span,
    message: String,
}

impl ParserErr {
    pub fn new(span: Span, message: String) -> ParserErr {
        ParserErr {
            span: span,
            message: message,
        }
    }
}

impl fmt::Display for ParserErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.span.begin, self.message)
    }
}

pub struct Parser {
    token_cursor: usize,
    tokens: Tokens,
    errors: Vec<ParserErr>,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Parser {
        Parser {
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

    pub fn parse_expr(&mut self) -> Expr {
        let lhs_expr = self.parse_unary_expr();
        self.parse_binary_expr(lhs_expr)
    }

    pub fn parse_binary_expr(&mut self, lhs_expr: Expr) -> Expr {
        let span_begin = lhs_expr.span().begin.clone();

        // Get the next token.
        let token = match self.current_token() {
            Some(token) => token,
            // Having no next token should finish the parsing of the binary
            // expression.
            None => return lhs_expr,
        };
        let prec = token.precedence();

        // Parse the binary operator.
        let operator = match token {
            Token::Add(..) => Operator::Add,
            Token::Concat(..) => Operator::Concat,
            Token::Div(..) => Operator::Div,
            Token::Dot(..) => Operator::Dot,
            Token::Mul(..) => Operator::Mul,
            Token::Sub(..) => Operator::Sub,
            _ => {
                self.errors.push(ParserErr::new(
                    token.span().clone(),
                    format!("unexpect token {}", token),
                ));
                return lhs_expr;
            }
        };
        let operator_span = token.span();

        // We have now decided that this token can be parsed so we consume it.
        self.next_token();

        // Parse the next part of this binary expression.
        let rhs_expr = self.parse_unary_expr();
        let span_end = rhs_expr.span().end.clone();

        match self.current_token() {
            Some(next_token) => {
                if prec > next_token.precedence() {
                    // The next token is of a lower precedence so this binary
                    // expression is should be parsed first, as the left hand side
                    // of the next.
                    self.parse_binary_expr(Expr::Call(
                        Expr::Var(operator.to_string(), operator_span.clone()).into(),
                        vec![lhs_expr, rhs_expr],
                        Span::from_begin_end(span_begin, span_end),
                    ))
                } else if prec == next_token.precedence() {
                    match next_token {
                        // token is left-associative
                        Token::Add(..)
                        | Token::And(..)
                        | Token::Concat(..)
                        | Token::Div(..)
                        | Token::Mul(..)
                        | Token::Or(..)
                        | Token::Sub(..) => self.parse_binary_expr(Expr::Call(
                            Expr::Var(operator.to_string(), operator_span.clone()).into(),
                            vec![lhs_expr, rhs_expr],
                            Span::from_begin_end(span_begin, span_end),
                        )),
                        // token is right-associative
                        _ => {
                            let rhs_expr = self.parse_binary_expr(rhs_expr);
                            let span_end = rhs_expr.span().end.clone();
                            Expr::Call(
                                Expr::Var(operator.to_string(), operator_span.clone()).into(),
                                vec![lhs_expr, rhs_expr],
                                Span::from_begin_end(span_begin, span_end),
                            )
                        }
                    }
                } else {
                    // The next token is of a higher precedence so the right hand
                    // side of this expression is actually the left hand side of
                    // the next binary expression.
                    let rhs_expr = self.parse_binary_expr(rhs_expr);
                    let span_end = rhs_expr.span().end.clone();
                    Expr::Call(
                        Expr::Var(operator.to_string(), operator_span.clone()).into(),
                        vec![lhs_expr, rhs_expr],
                        Span::from_begin_end(span_begin, span_end),
                    )
                }
            }
            // There are no more parts of the expression.
            None => Expr::Call(
                Expr::Var(operator.to_string(), operator_span.clone()).into(),
                vec![lhs_expr, rhs_expr],
                Span::from_begin_end(span_begin, span_end),
            ),
        }
    }

    pub fn parse_unary_expr(&mut self) -> Expr {
        let token = self.current_token();
        let call_target_expr = match token {
            Some(Token::ParenL(..)) => self.parse_paren_expr(),
            Some(Token::BracketL(..)) => self.parse_bracket_expr(),
            Some(Token::Bool(..)) => self.parse_literal_bool_expr(),
            Some(Token::Float(..)) => self.parse_literal_float_expr(),
            Some(Token::Int(..)) => self.parse_literal_int_expr(),
            Some(Token::String(..)) => self.parse_literal_str_expr(),
            Some(Token::Ident(..)) => self.parse_ident_expr(),
            Some(Token::BackSlash(..)) => self.parse_lambda_expr(),
            Some(Token::Sub(..)) => self.parse_neg_expr(),
            _ => unimplemented!(),
        };
        let call_target_expr_span = call_target_expr.span().clone();

        let mut call_arg_exprs = Vec::new();
        let mut call_arg_exprs_span = None;
        loop {
            let token = self.current_token();
            let call_arg_expr = match token {
                Some(Token::ParenL(..)) => self.parse_paren_expr(),
                Some(Token::BracketL(..)) => self.parse_bracket_expr(),
                Some(Token::Bool(..)) => self.parse_literal_bool_expr(),
                Some(Token::Float(..)) => self.parse_literal_float_expr(),
                Some(Token::Int(..)) => self.parse_literal_int_expr(),
                Some(Token::String(..)) => self.parse_literal_str_expr(),
                Some(Token::Ident(..)) => self.parse_ident_expr(),
                Some(Token::BackSlash(..)) => self.parse_lambda_expr(),
                Some(Token::Sub(..)) => self.parse_neg_expr(),
                _ => break,
            };
            call_arg_exprs_span = Some(call_arg_expr.span().clone());
            call_arg_exprs.push(call_arg_expr);
        }

        if call_arg_exprs.is_empty() {
            call_target_expr
        } else {
            Expr::Call(
                call_target_expr.into(),
                call_arg_exprs,
                Span::from_begin_end(
                    call_target_expr_span.begin,
                    call_arg_exprs_span.unwrap().end,
                ),
            )
        }
    }

    pub fn parse_paren_expr(&mut self) -> Expr {
        // Eat the left parenthesis.
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::ParenL(span, ..)) => {
                self.next_token();
                span
            }
            _ => unimplemented!(),
        };

        // Parse the inner expression.
        let mut expr = match self.current_token() {
            Some(Token::Add(span, ..)) => {
                self.next_token();
                Expr::Var("+".to_string(), span)
            }
            Some(Token::And(span, ..)) => {
                self.next_token();
                Expr::Var("&&".to_string(), span)
            }
            Some(Token::Concat(span, ..)) => {
                self.next_token();
                Expr::Var("++".to_string(), span)
            }
            Some(Token::Div(span, ..)) => {
                self.next_token();
                Expr::Var("/".to_string(), span)
            }
            Some(Token::Dot(span, ..)) => {
                self.next_token();
                Expr::Var(".".to_string(), span)
            }
            Some(Token::Mul(span, ..)) => {
                self.next_token();
                Expr::Var("*".to_string(), span)
            }
            Some(Token::Or(span, ..)) => {
                self.next_token();
                Expr::Var("||".to_string(), span)
            }
            Some(Token::Sub(span, ..)) => {
                self.next_token();
                Expr::Var("-".to_string(), span)
            }
            _ => self.parse_expr(),
        };

        // Eat the right parenthesis.
        let token = self.current_token();
        let span_end = match token {
            Some(Token::ParenR(span, ..)) => {
                self.next_token();
                span
            }
            oops => {
                dbg!(oops);
                unimplemented!()
            }
        };

        expr.set_span_begin(span_begin.begin.clone());
        expr.set_span_end(span_end.end.clone());
        expr
    }

    pub fn parse_bracket_expr(&mut self) -> Expr {
        // Eat the left bracket.
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::BracketL(span, ..)) => {
                self.next_token();
                span.begin.clone()
            }
            _ => unimplemented!(),
        };

        // Catch the case where the list is empty.
        let token = self.current_token();
        if let Some(Token::BracketR(span, ..)) = token {
            self.next_token();
            return Expr::List(
                vec![],
                Span::from_begin_end(span_begin.clone(), span.end.clone()),
            );
        }

        let mut exprs = Vec::new();
        loop {
            // Parse the next expression.
            let expr = self.parse_expr();
            let span_expr = expr.span().clone();
            exprs.push(expr);
            // Eat the comma.
            let token = self.current_token();
            match token {
                Some(Token::Comma(..)) => self.next_token(),
                Some(Token::BracketL(..)) => break,
                _ => {
                    self.errors.push(ParserErr::new(
                        Span::from_begin_end(span_begin.clone(), span_expr.end.clone()),
                        format!("expected `,` or `]`"),
                    ));
                    break;
                }
            };
        }

        // Eat the right bracket.
        let token = self.current_token();
        let span_end = match token {
            Some(Token::BracketR(span, ..)) => {
                self.next_token();
                span.end.clone()
            }
            _ => unimplemented!(),
        };

        Expr::List(exprs, Span::from_begin_end(span_begin, span_end))
    }

    pub fn parse_neg_expr(&mut self) -> Expr {
        // Eat the minus.
        let token = self.current_token();
        let span_token = match token {
            Some(Token::Sub(span, ..)) => {
                self.next_token();
                span
            }
            _ => unimplemented!(),
        };

        // Parse the inner expression.
        let expr = self.parse_expr();
        let span_end = expr.span().end.clone();

        Expr::Call(
            Expr::Var("-".to_string(), span_token.clone()).into(),
            vec![expr],
            Span::from_begin_end(span_token.begin.clone(), span_end),
        )
    }

    //
    pub fn parse_lambda_expr(&mut self) -> Expr {
        // Eat the backslash.
        let token = self.current_token();
        let span_begin = match token {
            Some(Token::BackSlash(span, ..)) => {
                self.next_token();
                span.begin.clone()
            }
            _ => unimplemented!(),
        };

        // Parse the params.
        let mut params = Vec::new();
        loop {
            let token = self.current_token();
            match token {
                Some(Token::Ident(val, _span, ..)) => {
                    self.next_token();
                    params.push(val);
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
            _ => unimplemented!(),
        };

        // Parse the body.
        let body = self.parse_expr();
        let span_end = body.span().end.clone();

        Expr::Lambda(
            params,
            body.into(),
            Span::from_begin_end(span_begin, span_end),
        )
    }

    //
    pub fn parse_literal_bool_expr(&mut self) -> Expr {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Bool(val, span, ..)) => Expr::Bool(val, span),
            _ => unimplemented!(),
        }
    }

    //
    pub fn parse_literal_float_expr(&mut self) -> Expr {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Float(val, span, ..)) => Expr::Float(val, span),
            _ => unimplemented!(),
        }
    }

    //
    pub fn parse_literal_int_expr(&mut self) -> Expr {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Int(val, span, ..)) => Expr::Int(val, span),
            _ => unimplemented!(),
        }
    }

    //
    pub fn parse_literal_str_expr(&mut self) -> Expr {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::String(val, span, ..)) => Expr::String(val, span),
            _ => unimplemented!(),
        }
    }

    //
    pub fn parse_ident_expr(&mut self) -> Expr {
        let token = self.current_token();
        self.next_token();
        match token {
            Some(Token::Ident(val, span, ..)) => Expr::Var(val, span),
            _ => unimplemented!(),
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
    use crate::lexer::Token;

    use super::*;

    #[test]
    fn test_parse_literals() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "true"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Bool(true, Span::new("test.rex", 1, 1, 1, 4)),);

        let mut parser = Parser::new(Token::tokenize("test.rex", "false"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Bool(false, Span::new("test.rex", 1, 1, 1, 5)),);

        let mut parser = Parser::new(Token::tokenize("test.rex", "0"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Int(0, Span::new("test.rex", 1, 1, 1, 1)),);

        let mut parser = Parser::new(Token::tokenize("test.rex", "1"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Int(1, Span::new("test.rex", 1, 1, 1, 1)),);

        let mut parser = Parser::new(Token::tokenize("test.rex", "42"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Int(42, Span::new("test.rex", 1, 1, 1, 2)),);

        let mut parser = Parser::new(Token::tokenize("test.rex", "0.0"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Float(0.0, Span::new("test.rex", 1, 1, 1, 3)),);

        let mut parser = Parser::new(Token::tokenize("test.rex", "1.0"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Float(1.0, Span::new("test.rex", 1, 1, 1, 3)),);

        let mut parser = Parser::new(Token::tokenize("test.rex", "3.14"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Float(3.14, Span::new("test.rex", 1, 1, 1, 4)),);
    }

    #[test]
    fn test_parse_list() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "[]"));
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::List(vec![], Span::new("test.rex", 1, 1, 1, 2)));

        let mut parser = Parser::new(Token::tokenize("test.rex", "[[]]"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::List(
                vec![Expr::List(vec![], Span::new("test.rex", 1, 2, 1, 3))],
                Span::new("test.rex", 1, 1, 1, 4)
            )
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "[[], []]"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::List(
                vec![
                    Expr::List(vec![], Span::new("test.rex", 1, 2, 1, 3)),
                    Expr::List(vec![], Span::new("test.rex", 1, 6, 1, 7))
                ],
                Span::new("test.rex", 1, 1, 1, 8)
            )
        );

        let mut parser = Parser::new(Token::tokenize(
            "test.rex",
            "[true, 42, 3.14, \"foo\", [], ident]",
        ));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::List(
                vec![
                    Expr::Bool(true, Span::new("test.rex", 1, 2, 1, 5)),
                    Expr::Int(42, Span::new("test.rex", 1, 8, 1, 9)),
                    Expr::Float(3.14, Span::new("test.rex", 1, 12, 1, 15)),
                    Expr::String("foo".to_string(), Span::new("test.rex", 1, 18, 1, 22)),
                    Expr::List(vec![], Span::new("test.rex", 1, 25, 1, 26)),
                    Expr::Var("ident".to_string(), Span::new("test.rex", 1, 29, 1, 33))
                ],
                Span::new("test.rex", 1, 1, 1, 34)
            )
        );
    }

    #[test]
    fn test_parse_variable() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "foo"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Var("foo".to_string(), Span::new("test.rex", 1, 1, 1, 3)),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "(bar)"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Var("bar".to_string(), Span::new("test.rex", 1, 1, 1, 5)),
        );
    }

    #[test]
    fn test_parse_math_operators() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "1 + 2"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("+".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                vec![
                    Expr::Int(1, Span::new("test.rex", 1, 1, 1, 1)),
                    Expr::Int(2, Span::new("test.rex", 1, 5, 1, 5))
                ],
                Span::new("test.rex", 1, 1, 1, 5)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "1 + 2 * 3"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("+".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                vec![
                    Expr::Int(1, Span::new("test.rex", 1, 1, 1, 1)),
                    Expr::Call(
                        Expr::Var("*".to_string(), Span::new("test.rex", 1, 7, 1, 7)).into(),
                        vec![
                            Expr::Int(2, Span::new("test.rex", 1, 5, 1, 5)),
                            Expr::Int(3, Span::new("test.rex", 1, 9, 1, 9))
                        ],
                        Span::new("test.rex", 1, 5, 1, 9)
                    )
                ],
                Span::new("test.rex", 1, 1, 1, 9)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "1 * 2 + 3"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("+".to_string(), Span::new("test.rex", 1, 7, 1, 7)).into(),
                vec![
                    Expr::Call(
                        Expr::Var("*".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                        vec![
                            Expr::Int(1, Span::new("test.rex", 1, 1, 1, 1)),
                            Expr::Int(2, Span::new("test.rex", 1, 5, 1, 5))
                        ],
                        Span::new("test.rex", 1, 1, 1, 5)
                    ),
                    Expr::Int(3, Span::new("test.rex", 1, 9, 1, 9))
                ],
                Span::new("test.rex", 1, 1, 1, 9)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "1 * (2 + 3)"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("*".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                vec![
                    Expr::Int(1, Span::new("test.rex", 1, 1, 1, 1)),
                    Expr::Call(
                        Expr::Var("+".to_string(), Span::new("test.rex", 1, 8, 1, 8)).into(),
                        vec![
                            Expr::Int(2, Span::new("test.rex", 1, 6, 1, 6)),
                            Expr::Int(3, Span::new("test.rex", 1, 10, 1, 10))
                        ],
                        Span::new("test.rex", 1, 5, 1, 11)
                    ),
                ],
                Span::new("test.rex", 1, 1, 1, 11)
            ),
        );
    }

    #[test]
    fn test_parse_dot_operator() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "f . g"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var(".".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                vec![
                    Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)),
                    Expr::Var("g".to_string(), Span::new("test.rex", 1, 5, 1, 5)),
                ],
                Span::new("test.rex", 1, 1, 1, 5)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "f . g . h"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var(".".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                vec![
                    Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)),
                    Expr::Call(
                        Expr::Var(".".to_string(), Span::new("test.rex", 1, 7, 1, 7)).into(),
                        vec![
                            Expr::Var("g".to_string(), Span::new("test.rex", 1, 5, 1, 5)),
                            Expr::Var("h".to_string(), Span::new("test.rex", 1, 9, 1, 9)),
                        ],
                        Span::new("test.rex", 1, 5, 1, 9)
                    )
                ],
                Span::new("test.rex", 1, 1, 1, 9)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "(f . g) . h"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var(".".to_string(), Span::new("test.rex", 1, 9, 1, 9)).into(),
                vec![
                    Expr::Call(
                        Expr::Var(".".to_string(), Span::new("test.rex", 1, 4, 1, 4)).into(),
                        vec![
                            Expr::Var("f".to_string(), Span::new("test.rex", 1, 2, 1, 2)),
                            Expr::Var("g".to_string(), Span::new("test.rex", 1, 6, 1, 6)),
                        ],
                        Span::new("test.rex", 1, 1, 1, 7)
                    ),
                    Expr::Var("h".to_string(), Span::new("test.rex", 1, 11, 1, 11)),
                ],
                Span::new("test.rex", 1, 1, 1, 11)
            ),
        );
    }

    #[test]
    fn test_call() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "f x"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)).into(),
                vec![Expr::Var(
                    "x".to_string(),
                    Span::new("test.rex", 1, 3, 1, 3)
                )],
                Span::new("test.rex", 1, 1, 1, 3)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "f x y"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)).into(),
                vec![
                    Expr::Var("x".to_string(), Span::new("test.rex", 1, 3, 1, 3)),
                    Expr::Var("y".to_string(), Span::new("test.rex", 1, 5, 1, 5))
                ],
                Span::new("test.rex", 1, 1, 1, 5)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "f x (g y) z"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)).into(),
                vec![
                    Expr::Var("x".to_string(), Span::new("test.rex", 1, 3, 1, 3)),
                    Expr::Call(
                        Expr::Var("g".to_string(), Span::new("test.rex", 1, 6, 1, 6)).into(),
                        vec![Expr::Var(
                            "y".to_string(),
                            Span::new("test.rex", 1, 8, 1, 8)
                        ),],
                        Span::new("test.rex", 1, 5, 1, 9)
                    ),
                    Expr::Var("z".to_string(), Span::new("test.rex", 1, 11, 1, 11))
                ],
                Span::new("test.rex", 1, 1, 1, 11)
            ),
        );
    }

    #[test]
    fn test_lambda() {
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"\x -> x"#));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Lambda(
                vec!["x".to_string()],
                Expr::Var("x".to_string(), Span::new("test.rex", 1, 7, 1, 7)).into(),
                Span::new("test.rex", 1, 1, 1, 7)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", r#"\f x y -> f y x"#));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Lambda(
                vec!["f".to_string(), "x".to_string(), "y".to_string()],
                Expr::Call(
                    Expr::Var("f".to_string(), Span::new("test.rex", 1, 11, 1, 11)).into(),
                    vec![
                        Expr::Var("y".to_string(), Span::new("test.rex", 1, 13, 1, 13)),
                        Expr::Var("x".to_string(), Span::new("test.rex", 1, 15, 1, 15))
                    ],
                    Span::new("test.rex", 1, 11, 1, 15)
                )
                .into(),
                Span::new("test.rex", 1, 1, 1, 15)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", r#"(\f x y -> x + y) 1.0 3.14"#));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Lambda(
                    vec!["f".to_string(), "x".to_string(), "y".to_string()],
                    Expr::Call(
                        Expr::Var("+".to_string(), Span::new("test.rex", 1, 14, 1, 14)).into(),
                        vec![
                            Expr::Var("x".to_string(), Span::new("test.rex", 1, 12, 1, 12)),
                            Expr::Var("y".to_string(), Span::new("test.rex", 1, 16, 1, 16))
                        ],
                        Span::new("test.rex", 1, 12, 1, 16)
                    )
                    .into(),
                    Span::new("test.rex", 1, 1, 1, 17)
                )
                .into(),
                vec![
                    Expr::Float(1.0, Span::new("test.rex", 1, 19, 1, 21)),
                    Expr::Float(3.14, Span::new("test.rex", 1, 23, 1, 26))
                ],
                Span::new("test.rex", 1, 1, 1, 26)
            ),
        );
    }

    #[test]
    fn test_precedence() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "x + y + z"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("+".to_string(), Span::new("test.rex", 1, 7, 1, 7)).into(),
                vec![
                    Expr::Call(
                        Expr::Var("+".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                        vec![
                            Expr::Var("x".to_string(), Span::new("test.rex", 1, 1, 1, 1)),
                            Expr::Var("y".to_string(), Span::new("test.rex", 1, 5, 1, 5)),
                        ],
                        Span::new("test.rex", 1, 1, 1, 5)
                    ),
                    Expr::Var("z".to_string(), Span::new("test.rex", 1, 9, 1, 9)),
                ],
                Span::new("test.rex", 1, 1, 1, 9)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "f x + g y"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("+".to_string(), Span::new("test.rex", 1, 5, 1, 5)).into(),
                vec![
                    Expr::Call(
                        Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)).into(),
                        vec![Expr::Var(
                            "x".to_string(),
                            Span::new("test.rex", 1, 3, 1, 3)
                        ),],
                        Span::new("test.rex", 1, 1, 1, 3)
                    ),
                    Expr::Call(
                        Expr::Var("g".to_string(), Span::new("test.rex", 1, 7, 1, 7)).into(),
                        vec![Expr::Var(
                            "y".to_string(),
                            Span::new("test.rex", 1, 9, 1, 9)
                        ),],
                        Span::new("test.rex", 1, 7, 1, 9)
                    ),
                ],
                Span::new("test.rex", 1, 1, 1, 9)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "f . g x"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var(".".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                vec![
                    Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)).into(),
                    Expr::Call(
                        Expr::Var("g".to_string(), Span::new("test.rex", 1, 5, 1, 5)).into(),
                        vec![Expr::Var(
                            "x".to_string(),
                            Span::new("test.rex", 1, 7, 1, 7)
                        ),],
                        Span::new("test.rex", 1, 5, 1, 7)
                    ),
                ],
                Span::new("test.rex", 1, 1, 1, 7)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "(f . g) x"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Call(
                    Expr::Var(".".to_string(), Span::new("test.rex", 1, 4, 1, 4)).into(),
                    vec![
                        Expr::Var("f".to_string(), Span::new("test.rex", 1, 2, 1, 2)).into(),
                        Expr::Var("g".to_string(), Span::new("test.rex", 1, 6, 1, 6)).into(),
                    ],
                    Span::new("test.rex", 1, 1, 1, 7)
                )
                .into(),
                vec![Expr::Var("x".to_string(), Span::new("test.rex", 1, 9, 1, 9)).into(),],
                Span::new("test.rex", 1, 1, 1, 9)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "f x + g y * h z"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var("+".to_string(), Span::new("test.rex", 1, 5, 1, 5)).into(),
                vec![
                    Expr::Call(
                        Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)).into(),
                        vec![Expr::Var("x".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),],
                        Span::new("test.rex", 1, 1, 1, 3)
                    ),
                    Expr::Call(
                        Expr::Var("*".to_string(), Span::new("test.rex", 1, 11, 1, 11)).into(),
                        vec![
                            Expr::Call(
                                Expr::Var("g".to_string(), Span::new("test.rex", 1, 7, 1, 7))
                                    .into(),
                                vec![
                                    Expr::Var("y".to_string(), Span::new("test.rex", 1, 9, 1, 9))
                                        .into(),
                                ],
                                Span::new("test.rex", 1, 7, 1, 9)
                            ),
                            Expr::Call(
                                Expr::Var("h".to_string(), Span::new("test.rex", 1, 13, 1, 13))
                                    .into(),
                                vec![Expr::Var(
                                    "z".to_string(),
                                    Span::new("test.rex", 1, 15, 1, 15)
                                )
                                .into(),],
                                Span::new("test.rex", 1, 13, 1, 15)
                            ),
                        ],
                        Span::new("test.rex", 1, 7, 1, 15)
                    ),
                ],
                Span::new("test.rex", 1, 1, 1, 15)
            ),
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "f . g (h x) . i"));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                Expr::Var(".".to_string(), Span::new("test.rex", 1, 3, 1, 3)).into(),
                vec![
                    Expr::Var("f".to_string(), Span::new("test.rex", 1, 1, 1, 1)).into(),
                    Expr::Call(
                        Expr::Var(".".to_string(), Span::new("test.rex", 1, 13, 1, 13)).into(),
                        vec![
                            Expr::Call(
                                Expr::Var("g".to_string(), Span::new("test.rex", 1, 5, 1, 5))
                                    .into(),
                                vec![Expr::Call(
                                    Expr::Var("h".to_string(), Span::new("test.rex", 1, 8, 1, 8))
                                        .into(),
                                    vec![Expr::Var(
                                        "x".to_string(),
                                        Span::new("test.rex", 1, 10, 1, 10)
                                    )
                                    .into(),],
                                    Span::new("test.rex", 1, 7, 1, 11)
                                ),],
                                Span::new("test.rex", 1, 5, 1, 11)
                            ),
                            Expr::Var("i".to_string(), Span::new("test.rex", 1, 15, 1, 15)).into(),
                        ],
                        Span::new("test.rex", 1, 5, 1, 15)
                    ),
                ],
                Span::new("test.rex", 1, 1, 1, 15)
            ),
        );
    }

    #[test]
    fn test_newline() {
        let mut parser = Parser::new(Token::tokenize(
            "test.rex",
            r#"
\x y z ->
 map
  (f . g (h z) . i)
  (j x y (k z))"#,
        ));
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Lambda(
                vec!["x".to_string(), "y".to_string(), "z".to_string()],
                Expr::Call(
                    Expr::Var("map".to_string(), Span::new("test.rex", 3, 2, 3, 4)).into(),
                    vec![
                        Expr::Call(
                            Expr::Var(".".to_string(), Span::new("test.rex", 4, 6, 4, 6)).into(),
                            vec![
                                Expr::Var("f".to_string(), Span::new("test.rex", 4, 4, 4, 4))
                                    .into(),
                                Expr::Call(
                                    Expr::Var(".".to_string(), Span::new("test.rex", 4, 16, 4, 16))
                                        .into(),
                                    vec![
                                        Expr::Call(
                                            Expr::Var(
                                                "g".to_string(),
                                                Span::new("test.rex", 4, 8, 4, 8)
                                            )
                                            .into(),
                                            vec![Expr::Call(
                                                Expr::Var(
                                                    "h".to_string(),
                                                    Span::new("test.rex", 4, 11, 4, 11)
                                                )
                                                .into(),
                                                vec![Expr::Var(
                                                    "z".to_string(),
                                                    Span::new("test.rex", 4, 13, 4, 13)
                                                )
                                                .into(),],
                                                Span::new("test.rex", 4, 10, 4, 14)
                                            ),],
                                            Span::new("test.rex", 4, 8, 4, 14)
                                        ),
                                        Expr::Var(
                                            "i".to_string(),
                                            Span::new("test.rex", 4, 18, 4, 18)
                                        )
                                        .into(),
                                    ],
                                    Span::new("test.rex", 4, 8, 4, 18)
                                ),
                            ],
                            Span::new("test.rex", 4, 3, 4, 19)
                        ),
                        Expr::Call(
                            Expr::Var("j".to_string(), Span::new("test.rex", 5, 4, 5, 4)).into(),
                            vec![
                                Expr::Var("x".to_string(), Span::new("test.rex", 5, 6, 5, 6))
                                    .into(),
                                Expr::Var("y".to_string(), Span::new("test.rex", 5, 8, 5, 8))
                                    .into(),
                                Expr::Call(
                                    Expr::Var("k".to_string(), Span::new("test.rex", 5, 11, 5, 11))
                                        .into(),
                                    vec![Expr::Var(
                                        "z".to_string(),
                                        Span::new("test.rex", 5, 13, 5, 13)
                                    )
                                    .into(),],
                                    Span::new("test.rex", 5, 10, 5, 14)
                                ),
                            ],
                            Span::new("test.rex", 5, 3, 5, 15)
                        ),
                    ],
                    Span::new("test.rex", 3, 2, 5, 15)
                )
                .into(),
                Span::new("test.rex", 2, 1, 5, 15)
            ),
        );
    }
}
