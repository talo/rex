use std::{
    collections::{HashMap, VecDeque},
    result,
};

use ouroboros::Type;

use crate::{
    ast::{Call, Lambda, AST},
    parser::Expr,
    span::{Span, Spanned},
    var::{UnresolvedVar, Var},
    Id,
};

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum Error {
    #[error("type mismatched, expected {expected} got {got}")]
    TypeMismatch { expected: Type, got: Type },
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

    pub fn resolve(&mut self, expr: Expr) -> Result<AST> {
        match expr {
            Expr::Null(span, ..) => Ok(AST::Null(span)),
            Expr::Bool(x, span, ..) => Ok(AST::Bool(x, span)),
            Expr::Int(x, span, ..) => Ok(AST::Uint(x, span)),
            Expr::Float(x, span, ..) => Ok(AST::Float(x, span)),
            Expr::String(x, span, ..) => Ok(AST::String(x, span)),
            Expr::List(xs, span, ..) => self.resolve_list(xs, span),
            Expr::Var(var, ..) => self.resolve_variable(var),
            Expr::Call(base, args, span, ..) => self.resolve_call(*base, args, span),
            Expr::Lambda(params, body, span, ..) => self.resolve_lambda(params, *body, span),
            Expr::LetIn(vars, defs, body, span, ..) => self.resolve_let_in(vars, defs, *body, span),
        }
    }

    fn resolve_list(&mut self, xs: Vec<Expr>, span: Span) -> Result<AST> {
        let mut ys = Vec::with_capacity(xs.len());
        for x in xs {
            ys.push(self.resolve(x)?);
        }
        Ok(AST::List(ys, span))
    }

    fn resolve_variable(&mut self, var: UnresolvedVar) -> Result<AST> {
        match self.scope.get(&var.name) {
            Some(id) => Ok(AST::Var(Var {
                id: *id,
                name: var.name,
                span: var.span,
            })),
            None => {
                let id = self.curr_id.inc();
                Ok(AST::Var(Var {
                    id,
                    name: var.name,
                    span: var.span,
                }))
            }
        }
    }

    fn resolve_call(&mut self, base: Expr, args: Vec<Expr>, span: Span) -> Result<AST> {
        args.into_iter().fold(self.resolve(base), |base, arg| {
            base.and_then(|base| {
                Ok(AST::Call(Call {
                    id: self.curr_id.inc(),
                    base: Box::new(base),
                    arg: Box::new(self.resolve(arg)?),
                    span: base.span().clone().merge(arg.span().clone()),
                }))
            })
        })
    }

    fn resolve_lambda(&mut self, vars: Vec<UnresolvedVar>, body: Expr, span: Span) -> Result<AST> {
        let mut scope = self.scope.clone();

        let vars = vars
            .into_iter()
            .map(|var| {
                let id = self.curr_id.inc();
                scope.insert(var.name.clone(), id);
                Var {
                    id,
                    name: var.name,
                    span: var.span,
                }
            })
            .collect::<Vec<_>>();

        vars.into_iter().rev().fold(
            Resolver {
                curr_id: self.curr_id,
                scope,
            }
            .resolve(body),
            |body, var| {
                body.and_then(|body| {
                    Ok(AST::Lambda(Lambda {
                        id: self.curr_id.inc(),
                        var: var.clone(),
                        body: Box::new(body),
                        span: var.span.merge(body.span().clone()),
                    }))
                })
            },
        )
    }

    fn resolve_let_in(
        &mut self,
        vars: Vec<UnresolvedVar>,
        defs: Vec<Expr>,
        body: Expr,
        span: Span,
    ) -> Result<AST> {
        // Create a new scope for the let-in expression that extends the current
        // scope (and will shadow it)
        let mut params = VecDeque::with_capacity(vars.len());
        let mut param_defs = VecDeque::with_capacity(vars.len());
        let mut scope = self.scope.clone();
        for (UnresolvedVar { name, span }, def) in vars.into_iter().zip(defs.into_iter()) {
            let id = self.curr_id.inc();
            params.push_back(Variable {
                id,
                name: name.clone(),
                span: span.clone(),
            });
            scope.insert(name, id);

            // Create a resolver that uses the new scope and extends the current
            // namespace
            let mut resolver = Resolver {
                curr_id: self.curr_id,
                scope: scope.clone(),
            };
            param_defs.push_back(resolver.resolve(def)?);
        }

        let mut resolver = Resolver {
            curr_id: self.curr_id,
            scope,
        };
        let body = Box::new(resolver.resolve(body)?);

        Ok(AST::LetIn(LetIn {
            id: self.curr_id.inc(),
            vars: params,
            defs: param_defs,
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
        resolver::{Call, Id, Lambda, Resolver, Variable, AST},
        span::Span,
    };

    #[test]
    fn test_values() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "true").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(ir, Ok(AST::Bool(true, Span::new("test.rex", 1, 1, 1, 4))));

        let mut parser = Parser::new(Token::tokenize("test.rex", "42").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(ir, Ok(AST::Uint(42, Span::new("test.rex", 1, 1, 1, 2))));

        let mut parser = Parser::new(Token::tokenize("test.rex", "3.55").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(ir, Ok(AST::Float(3.55, Span::new("test.rex", 1, 1, 1, 4))));

        let mut parser = Parser::new(Token::tokenize("test.rex", r#""hello""#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(AST::String(
                "hello".to_string(),
                Span::new("test.rex", 1, 1, 1, 7)
            ))
        );

        let mut parser = Parser::new(Token::tokenize("test.rex", "[0, 1, 42]").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(AST::List(
                vec![
                    AST::Uint(0, Span::new("test.rex", 1, 2, 1, 2)),
                    AST::Uint(1, Span::new("test.rex", 1, 5, 1, 5)),
                    AST::Uint(42, Span::new("test.rex", 1, 8, 1, 9))
                ],
                Span::new("test.rex", 1, 1, 1, 10)
            ))
        );
    }

    #[test]
    fn test_variables() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "x").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(AST::Variable(Variable {
                id: Id(0),
                name: "x".to_string(),
                span: Span::new("test.rex", 1, 1, 1, 1)
            }))
        );
    }

    #[test]
    fn test_lambda() {
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"\x -> f x"#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(AST::Lambda(Lambda {
                id: Id(3),
                params: vec![Variable {
                    id: Id(0),
                    name: "x".to_string(),
                    span: Span::new("test.rex", 1, 2, 1, 2)
                }]
                .into(),
                body: Box::new(AST::Call(Call {
                    id: Id(2),
                    base: Box::new(AST::Variable(Variable {
                        id: Id(1),
                        name: "f".to_string(),
                        span: Span::new("test.rex", 1, 7, 1, 7)
                    })),
                    args: vec![AST::Variable(Variable {
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
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"\x -> \y -> f x y"#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(AST::Lambda(Lambda {
                id: Id(5),
                params: vec![Variable {
                    id: Id(0),
                    name: "x".to_string(),
                    span: Span::new("test.rex", 1, 2, 1, 2) // FIXME: Lambda variables should have their own span
                }]
                .into(),
                body: Box::new(AST::Lambda(Lambda {
                    id: Id(4),
                    params: vec![Variable {
                        id: Id(1),
                        name: "y".to_string(),
                        span: Span::new("test.rex", 1, 8, 1, 8)
                    }]
                    .into(),
                    body: Box::new(AST::Call(Call {
                        id: Id(3),
                        base: Box::new(AST::Variable(Variable {
                            id: Id(2),
                            name: "f".to_string(),
                            span: Span::new("test.rex", 1, 13, 1, 13)
                        })),
                        args: vec![
                            AST::Variable(Variable {
                                id: Id(0),
                                name: "x".to_string(),
                                span: Span::new("test.rex", 1, 15, 1, 15)
                            }),
                            AST::Variable(Variable {
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
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"\x -> \x -> f x"#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap());
        assert_eq!(
            ir,
            Ok(AST::Lambda(Lambda {
                id: Id(5),
                params: vec![Variable {
                    id: Id(0),
                    name: "x".to_string(),
                    span: Span::new("test.rex", 1, 2, 1, 2)
                }]
                .into(),
                body: Box::new(AST::Lambda(Lambda {
                    id: Id(4),
                    params: vec![Variable {
                        id: Id(1),
                        name: "x".to_string(),
                        span: Span::new("test.rex", 1, 8, 1, 8)
                    }]
                    .into(),
                    body: Box::new(AST::Call(Call {
                        id: Id(3),
                        base: Box::new(AST::Variable(Variable {
                            id: Id(2),
                            name: "f".to_string(),
                            span: Span::new("test.rex", 1, 13, 1, 13)
                        })),
                        args: vec![AST::Variable(Variable {
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
