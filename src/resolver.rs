use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    result,
};

use ouroboros::Type;

use crate::{
    parser::Expr,
    span::{Span, Spanned},
};

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum Error {
    #[error("type mismatched, expected {expected} got {got}")]
    TypeMismatch { expected: Type, got: Type },
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Id(pub u64);

impl Id {
    pub fn new() -> Id {
        Id(0)
    }

    pub fn inc(&mut self) -> Id {
        let id = self.0;
        self.0 += 1;
        Id(id)
    }
}

impl Default for Id {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum IR {
    Null(Span),
    Bool(bool, Span),
    Uint(u64, Span),
    Int(i64, Span),
    Float(f64, Span),
    String(String, Span),
    List(Vec<IR>, Span),
    Record(BTreeMap<String, serde_json::Value>, Span),
    Call(Call),
    Lambda(Lambda),
    Variable(Variable),
}

impl Spanned for IR {
    fn span(&self) -> &Span {
        match self {
            IR::Null(span) => span,
            IR::Bool(_, span) => span,
            IR::Uint(_, span) => span,
            IR::Int(_, span) => span,
            IR::Float(_, span) => span,
            IR::String(_, span) => span,
            IR::List(_, span) => span,
            IR::Record(_, span) => span,
            IR::Call(call) => &call.span,
            IR::Lambda(lam) => &lam.span,
            IR::Variable(var) => &var.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            IR::Null(span) => span,
            IR::Bool(_, span) => span,
            IR::Uint(_, span) => span,
            IR::Int(_, span) => span,
            IR::Float(_, span) => span,
            IR::String(_, span) => span,
            IR::List(_, span) => span,
            IR::Record(_, span) => span,
            IR::Call(call) => &mut call.span,
            IR::Lambda(lam) => &mut lam.span,
            IR::Variable(var) => &mut var.span,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Variable {
    pub id: Id,
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Call {
    pub id: Id,
    pub base: Box<IR>,
    pub args: VecDeque<IR>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Lambda {
    pub id: Id,
    pub params: VecDeque<Variable>,
    pub body: Box<IR>,
    pub span: Span,
}

pub struct Resolver {
    pub curr_id: Id,
    pub scope: HashMap<String, Id>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            curr_id: Id::new(),
            scope: HashMap::new(),
        }
    }

    pub fn inject_builtin(&mut self, name: &str) -> Id {
        let id = self.curr_id.inc();
        self.scope.insert(name.to_string(), id);
        id
    }

    pub fn resolve(&mut self, expr: Expr) -> Result<IR> {
        match expr {
            Expr::Null(span, ..) => Ok(IR::Null(span)),
            Expr::Bool(x, span, ..) => Ok(IR::Bool(x, span)),
            Expr::Int(x, span, ..) => Ok(IR::Uint(x, span)),
            Expr::Float(x, span, ..) => Ok(IR::Float(x, span)),
            Expr::String(x, span, ..) => Ok(IR::String(x, span)),
            Expr::List(xs, span, ..) => self.resolve_list(xs, span),
            Expr::Record(xs, span, ..) => self.resolve_record(xs, span),
            Expr::Var(name, span, ..) => self.resolve_variable(name, span),
            Expr::Call(base, args, span, ..) => self.resolve_call(*base, args, span),
            Expr::Lambda(param_names, body, span, ..) => {
                self.resolve_lambda(param_names, *body, span)
            }
        }
    }

    fn resolve_list(&mut self, xs: Vec<Expr>, span: Span) -> Result<IR> {
        let mut ys = Vec::with_capacity(xs.len());
        for x in xs {
            ys.push(self.resolve(x)?);
        }
        Ok(IR::List(ys, span))
    }

    fn resolve_record(&mut self, xs: BTreeMap<String, Expr>, span: Span) -> Result<IR> {
        let mut ys = BTreeMap::new();
        for (field_name, x) in xs {
            ys.insert(
                field_name,
                serde_json::to_value(self.resolve(x)?).expect("should serialize to json"),
            );
        }
        Ok(IR::Record(ys, span))
    }

    fn resolve_variable(&mut self, name: String, span: Span) -> Result<IR> {
        match self.scope.get(&name) {
            Some(id) => Ok(IR::Variable(Variable {
                id: *id,
                name,
                span,
            })),
            None => {
                let id = self.curr_id.inc();
                Ok(IR::Variable(Variable { id, name, span }))
            }
        }
    }

    fn resolve_call(&mut self, base: Expr, args: Vec<Expr>, span: Span) -> Result<IR> {
        let base = Box::new(self.resolve(base)?);
        let mut xs = VecDeque::with_capacity(args.len());
        for x in args {
            xs.push_back(self.resolve(x)?);
        }
        let id = self.curr_id.inc();
        let call = Call {
            id,
            base,
            args: xs,
            span,
        };
        Ok(IR::Call(call))
    }

    fn resolve_lambda(&mut self, param_names: Vec<String>, body: Expr, span: Span) -> Result<IR> {
        // Create a new scope for the lambda that extends the current scope
        let mut params = VecDeque::with_capacity(param_names.len());
        let mut scope = self.scope.clone();
        for param_name in param_names {
            let id = self.curr_id.inc();
            params.push_back(Variable {
                id,
                name: param_name.clone(),
                span: span.clone(),
            }); // FIXME: Lambda variables should have their own span
            scope.insert(param_name, id);
        }

        // Create a resolver that uses the new scope and extends the current
        // namespace
        let mut resolver = Resolver {
            curr_id: self.curr_id,
            scope,
        };
        let body = Box::new(resolver.resolve(body)?);

        // Keep the new IDs and new type resolutions of the inner resolver.
        // However, the parameters declared by a lambda are only valid in the
        // body of that lambda, so we do not keep the new scope.
        self.curr_id = resolver.curr_id;

        Ok(IR::Lambda(Lambda {
            id: self.curr_id.inc(),
            params,
            body,
            span,
        }))
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::Token,
        parser::Parser,
        resolver::{Call, Id, Lambda, Resolver, Variable, IR},
        span::Span,
    };

    #[test]
    fn test_values() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "true"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(ir, Ok(IR::Bool(true, Span::new("test.rex", 1, 1, 1, 4))));

        let mut parser = Parser::new(Token::tokenize("test.rex", "42"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(ir, Ok(IR::Uint(42, Span::new("test.rex", 1, 1, 1, 2))));

        let mut parser = Parser::new(Token::tokenize("test.rex", "3.55"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(ir, Ok(IR::Float(3.55, Span::new("test.rex", 1, 1, 1, 4))));

        let mut parser = Parser::new(Token::tokenize("test.rex", r#""hello""#));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(IR::String(
                "hello".to_string(),
                Span::new("test.rex", 1, 1, 1, 7)
            ))
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "[0, 1, 42]"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(IR::List(
                vec![
                    IR::Uint(0, Span::new("test.rex", 1, 2, 1, 2)),
                    IR::Uint(1, Span::new("test.rex", 1, 5, 1, 5)),
                    IR::Uint(42, Span::new("test.rex", 1, 8, 1, 9))
                ],
                Span::new("test.rex", 1, 1, 1, 10)
            ))
        );
    }

    #[test]
    fn test_variables() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "x"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(IR::Variable(Variable {
                id: Id(0),
                name: "x".to_string(),
                span: Span::new("test.rex", 1, 1, 1, 1)
            }))
        );
    }

    #[test]
    fn test_lambda() {
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"\x -> f x"#));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(IR::Lambda(Lambda {
                id: Id(3),
                params: vec![Variable {
                    id: Id(0),
                    name: "x".to_string(),
                    span: Span::new("test.rex", 1, 1, 1, 9) // FIXME: Lambda variables should have their own span
                }]
                .into(),
                body: Box::new(IR::Call(Call {
                    id: Id(2),
                    base: Box::new(IR::Variable(Variable {
                        id: Id(1),
                        name: "f".to_string(),
                        span: Span::new("test.rex", 1, 7, 1, 7) // FIXME: Lambda variables should have their own span
                    })),
                    args: vec![IR::Variable(Variable {
                        id: Id(0),
                        name: "x".to_string(),
                        span: Span::new("test.rex", 1, 9, 1, 9)
                    })]
                    .into(),
                    span: Span::new("test.rex", 1, 7, 1, 9)
                })),
                span: Span::new("test.rex", 1, 1, 1, 9)
            }))
        );
    }

    #[test]
    fn test_lambda_capture() {
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"\x -> \y -> f x y"#));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(IR::Lambda(Lambda {
                id: Id(5),
                params: vec![Variable {
                    id: Id(0),
                    name: "x".to_string(),
                    span: Span::new("test.rex", 1, 1, 1, 17) // FIXME: Lambda variables should have their own span
                }]
                .into(),
                body: Box::new(IR::Lambda(Lambda {
                    id: Id(4),
                    params: vec![Variable {
                        id: Id(1),
                        name: "y".to_string(),
                        span: Span::new("test.rex", 1, 7, 1, 17) // FIXME: Lambda variables should have their own span
                    }]
                    .into(),
                    body: Box::new(IR::Call(Call {
                        id: Id(3),
                        base: Box::new(IR::Variable(Variable {
                            id: Id(2),
                            name: "f".to_string(),
                            span: Span::new("test.rex", 1, 13, 1, 13)
                        })),
                        args: vec![
                            IR::Variable(Variable {
                                id: Id(0),
                                name: "x".to_string(),
                                span: Span::new("test.rex", 1, 15, 1, 15)
                            }),
                            IR::Variable(Variable {
                                id: Id(1),
                                name: "y".to_string(),
                                span: Span::new("test.rex", 1, 17, 1, 17)
                            })
                        ]
                        .into(),
                        span: Span::new("test.rex", 1, 13, 1, 17)
                    })),
                    span: Span::new("test.rex", 1, 7, 1, 17)
                })),
                span: Span::new("test.rex", 1, 1, 1, 17)
            }))
        );
    }

    #[test]
    fn test_lambda_shadow() {
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"\x -> \x -> f x"#));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(IR::Lambda(Lambda {
                id: Id(5),
                params: vec![Variable {
                    id: Id(0),
                    name: "x".to_string(),
                    span: Span::new("test.rex", 1, 1, 1, 15) // FIXME: Lambda variables should have their own span
                }]
                .into(),
                body: Box::new(IR::Lambda(Lambda {
                    id: Id(4),
                    params: vec![Variable {
                        id: Id(1),
                        name: "x".to_string(),
                        span: Span::new("test.rex", 1, 7, 1, 15) // FIXME: Lambda variables should have their own span
                    }]
                    .into(),
                    body: Box::new(IR::Call(Call {
                        id: Id(3),
                        base: Box::new(IR::Variable(Variable {
                            id: Id(2),
                            name: "f".to_string(),
                            span: Span::new("test.rex", 1, 13, 1, 13)
                        })),
                        args: vec![IR::Variable(Variable {
                            id: Id(1),
                            name: "x".to_string(),
                            span: Span::new("test.rex", 1, 15, 1, 15)
                        })]
                        .into(),
                        span: Span::new("test.rex", 1, 13, 1, 15)
                    })),
                    span: Span::new("test.rex", 1, 7, 1, 15)
                })),
                span: Span::new("test.rex", 1, 1, 1, 15)
            }))
        );
    }
}
