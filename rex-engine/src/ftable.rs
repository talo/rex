use std::{
    collections::{BTreeMap, HashMap},
    future::Future,
    pin::Pin,
};

use futures::future;
use rex_ast::{
    a, arrow,
    ast::Var,
    b,
    id::{Id, IdDispenser},
    list, tuple,
    types::{ADTVariantFields, Type, ADT},
};
use rex_resolver::Scope;

use crate::{
    apply,
    error::Error,
    value::{Data, DataFields, Function, NamedDataFields, UnnamedDataFields, Value},
    Context,
};

type FtableFn<S> = Box<
    dyn for<'r> Fn(
            &'r Context,
            &'r Ftable<S>,
            &'r S,
            &'r Vec<Value>,
        ) -> Pin<Box<dyn Future<Output = Result<Value, Error>> + Send + 'r>>
        + Sync,
>;

pub struct Ftable<S: Send + Sync + 'static> {
    pub ftable: HashMap<Id, (Function, FtableFn<S>)>,
    pub fallback: Option<FtableFn<S>>,
}

impl<S> Default for Ftable<S>
where
    S: Send + Sync + 'static,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<S: Send + Sync + 'static> Ftable<S> {
    pub fn new() -> Self {
        Self {
            ftable: HashMap::new(),
            fallback: None,
        }
    }

    pub fn set_fallback(&mut self, fallback: FtableFn<S>) {
        self.fallback = Some(fallback);
    }

    pub fn with_intrinsics(id_dispenser: &mut IdDispenser) -> Self {
        let mut this = Self {
            ftable: HashMap::new(),
            fallback: None,
        };

        // arithmetic
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "+".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Uint(x + y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Int(x + y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Float(x + y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "-".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Uint(x - y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Int(x - y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Float(x - y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "*".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Uint(x * y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Int(x * y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Float(x * y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "/".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Uint(x / y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Int(x / y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Float(x / y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // inequalities
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: ">=".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x >= y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x >= y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x >= y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x >= y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedCmp {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: ">".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x > y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x > y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x > y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x > y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedCmp {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "<=".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x <= y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x <= y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x <= y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x <= y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedCmp {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "<".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x < y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x < y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x < y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x < y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedCmp {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "==".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Primitives
                        (Some(Value::Null), Some(Value::Null)) => Ok(Value::Bool(true)),
                        (Some(Value::Bool(x)), Some(Value::Bool(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::List(xs)), Some(Value::List(ys))) => Ok(Value::Bool(xs == ys)),
                        (Some(Value::Tuple(xs)), Some(Value::Tuple(ys))) => {
                            Ok(Value::Bool(xs == ys))
                        }
                        (Some(Value::Dict(xs)), Some(Value::Dict(ys))) => Ok(Value::Bool(xs == ys)),

                        (Some(Value::Id(i)), Some(Value::Id(j))) => Ok(Value::Bool(i == j)),
                        // Functions
                        (Some(Value::Function(f)), Some(Value::Function(g))) => {
                            Ok(Value::Bool(f == g))
                        }
                        (Some(Value::Closure(f)), Some(Value::Closure(g))) => {
                            Ok(Value::Bool(f == g))
                        }
                        // Data
                        (Some(Value::Data(x)), Some(Value::Data(y))) => Ok(Value::Bool(x == y)),
                        // Everything else
                        _ => Ok(Value::Bool(false)),
                    }
                })
            }),
        );

        // '++'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "++".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::List(xs)), Some(Value::List(ys))) => {
                            let mut zs = Vec::with_capacity(xs.len() + ys.len());
                            zs.extend(xs.iter().cloned());
                            zs.extend(ys.iter().cloned());
                            Ok(Value::List(zs))
                        }
                        (Some(Value::String(xs)), Some(Value::String(ys))) => {
                            Ok(Value::String(format!("{}{}", xs, ys)))
                        }
                        // Bad types
                        (Some(Value::List(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: list!(b!()),
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedConcat {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // 'map'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "map".to_string(),
                params: vec![arrow!(a!() => b!()), list!(a!())],
                ret: list!(b!()),
            },
            Box::new(|ctx, runner, state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(f), Some(Value::List(xs))) => {
                            let mut ys = Vec::with_capacity(xs.len());
                            for x in xs {
                                ys.push(apply(ctx, runner, state, f.clone(), x.clone()));
                            }
                            Ok(Value::List(
                                future::join_all(ys)
                                    .await
                                    .into_iter()
                                    .collect::<Result<_, _>>()?,
                            ))
                        }
                        // Everything else
                        (Some(_), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        // 'filter'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "filter".to_string(),
                params: vec![arrow!(a!() => Type::Bool), list!(a!())],
                ret: list!(a!()),
            },
            Box::new(|ctx, runner, state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(f), Some(Value::List(xs))) => {
                            let mut ys = Vec::with_capacity(xs.len());
                            for x in xs {
                                match apply(ctx, runner, state, f.clone(), x.clone()).await? {
                                    Value::Bool(true) => ys.push(x.clone()),
                                    Value::Bool(_) => {}
                                    x => Err(Error::UnexpectedType {
                                        expected: Type::Bool,
                                        got: x.clone(),
                                        trace: Default::default(),
                                    })?,
                                }
                            }
                            Ok(Value::List(ys))
                        }
                        // Everything else
                        (Some(_), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        // 'zip'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "zip".to_string(),
                params: vec![list!(a!()), list!(b!())],
                ret: list!(tuple!(a!(), b!())),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::List(xs)), Some(Value::List(ys))) => {
                            let mut zs = Vec::with_capacity(xs.len().min(ys.len()));
                            for (x, y) in xs.iter().zip(ys.iter()) {
                                zs.push(Value::Tuple(vec![x.clone(), y.clone()]))
                            }
                            Ok(Value::List(zs))
                        }
                        // Everything else
                        (Some(Value::List(_)), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(b!()),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        (Some(x), _) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        // Option
        // `some`
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "some".to_string(),
                params: vec![a!()],
                ret: Type::Option(Box::new(a!())),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        // Wrong number of arguments
                        None => Err(Error::ExpectedArguments {
                            expected: 1,
                            got: 0,
                            trace: Default::default(),
                        }),
                        // Implementation
                        Some(x) => Ok(Value::Option(Some(Box::new(x.clone())))),
                    }
                })
            }),
        );

        // 'none'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "none".to_string(),
                params: vec![],
                ret: Type::Option(Box::new(a!())),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        // Wrong number of arguments
                        None => Ok(Value::Option(None)),
                        // Everything else
                        Some(x) => Err(Error::UnexpectedType {
                            expected: Type::Option(Box::new(a!())),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // Result
        // `ok`
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "ok".to_string(),
                params: vec![a!()],
                ret: Type::Result(Box::new(a!()), Box::new(b!())),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        // Wrong number of arguments
                        None => Err(Error::ExpectedArguments {
                            expected: 1,
                            got: 0,
                            trace: Default::default(),
                        }),
                        // Implementation
                        Some(x) => Ok(Value::Result(Ok(Box::new(x.clone())))),
                    }
                })
            }),
        );

        // 'err'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "err".to_string(),
                params: vec![b!()],
                ret: Type::Result(Box::new(a!()), Box::new(b!())),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        // Wrong number of arguments
                        None => Err(Error::ExpectedArguments {
                            expected: 1,
                            got: 0,
                            trace: Default::default(),
                        }),
                        // Implementation
                        Some(x) => Ok(Value::Result(Err(Box::new(x.clone())))),
                    }
                })
            }),
        );

        // 'get'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "get".to_string(),
                params: vec![Type::Uint, a!()],
                ret: b!(),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        (Some(Value::Uint(i)), Some(Value::List(xs))) => {
                            match xs.get(*i as usize) {
                                Some(x) => Ok(x.clone()),
                                _ => Err(Error::IndexOutOfBounds {
                                    trace: Default::default(),
                                }),
                            }
                        }
                        (Some(Value::Uint(i)), Some(Value::Tuple(xs))) => match xs.get(*i as usize)
                        {
                            Some(x) => Ok(x.clone()),
                            _ => Err(Error::IndexOutOfBounds {
                                trace: Default::default(),
                            }),
                        },
                        (Some(Value::String(k)), Some(Value::Dict(dict))) => {
                            for (k_check, v) in dict {
                                if k == k_check {
                                    return Ok(v.clone());
                                }
                            }
                            Err(Error::KeyNotFound {
                                trace: Default::default(),
                            })
                        }
                        // Bad types
                        (Some(x), Some(Value::List(_))) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        (Some(x), Some(Value::Tuple(_))) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        (Some(x), Some(Value::Dict(_))) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (_, Some(x)) => Err(Error::ExpectedIndexable {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        this
    }

    pub fn register_function(&mut self, function: Function, lam: FtableFn<S>) {
        self.ftable.insert(function.id, (function, lam));
    }

    pub fn register_adt(&mut self, id_dispenser: &mut IdDispenser, adt: ADT) {
        self.register_adt_with_defaults(id_dispenser, adt, Default::default());
    }

    pub fn register_adt_with_defaults(
        &mut self,
        id_dispenser: &mut IdDispenser,
        adt: ADT,
        defaults: BTreeMap<String, DataFields>,
    ) {
        let ret = Type::ADT(adt.clone());
        for variant in adt.variants {
            match &variant.fields {
                Some(ADTVariantFields::Named(named_fields)) => {
                    // REGISTER GETTER FUNCTION FOR EACH FIELD
                    //for (field_name, _field_type) in &named_fields.fields {
                    for field_name in named_fields.fields.keys() {
                        let ret = ret.clone();
                        let field_name = field_name.clone();
                        let function = Function {
                            id: id_dispenser.next(),
                            name: field_name.clone(),
                            params: vec![Type::Dict(
                                vec![(field_name.clone(), a!())].into_iter().collect(),
                            )],
                            ret: a!(),
                        };
                        self.register_function(
                            function,
                            Box::new(move |_ctx, _runner, _state, args| {
                                let ret = ret.clone();
                                let field_name = field_name.clone();
                                Box::pin(async move {
                                    match args.first() {
                                        // Wrong number of arguments
                                        None => Err(Error::ExpectedArguments {
                                            expected: 1,
                                            got: args.len(),
                                            trace: Default::default(),
                                        }),
                                        // Implementation
                                        Some(Value::Data(data)) => {
                                            match &data.fields {
                                                Some(DataFields::Named(NamedDataFields {
                                                    fields,
                                                })) => match fields.get(&field_name) {
                                                    Some(field_value) => Ok(field_value.clone()),
                                                    _ => Err(Error::FieldNotFound {
                                                        field: field_name.clone(),
                                                        trace: Default::default(),
                                                    }),
                                                },
                                                // FIXME: We do not currently
                                                // support field-less ADT
                                                // variants
                                                _ => unimplemented!(),
                                            }
                                        }
                                        // Bad type
                                        Some(x) => Err(Error::UnexpectedType {
                                            expected: ret.clone(),
                                            got: x.clone(),
                                            trace: Default::default(),
                                        }),
                                    }
                                })
                            }),
                        );
                    }

                    // REGISTER TYPE CTOR FUNCTION
                    let ret = ret.clone();
                    let named_fields = named_fields.clone();
                    let defaults = defaults.clone();
                    let function = Function {
                        id: id_dispenser.next(),
                        name: variant.name.clone(),
                        params: vec![Type::Dict(
                            named_fields
                                .fields
                                .iter()
                                .map(|(n, t)| (n.clone(), t.clone()))
                                .collect(),
                        )],
                        ret: ret.clone(),
                    };
                    self.register_function(
                        function,
                        Box::new(move |_ctx, _runner, _state, args| {
                            let ret = ret.clone();
                            let variant = variant.clone();
                            let named_fields = named_fields.clone();
                            let defaults = defaults.clone();
                            Box::pin(async move {
                                match args.first() {
                                    // Wrong number of arguments
                                    None => Err(Error::ExpectedArguments {
                                        expected: 1,
                                        got: args.len(),
                                        trace: Default::default(),
                                    }),
                                    // Implementation
                                    Some(Value::Dict(fields))
                                        if Value::Dict({
                                            let mut d = defaults
                                                .get(&variant.name)
                                                .cloned()
                                                .map(|d| match d {
                                                    DataFields::Named(d_named_fields) => {
                                                        d_named_fields.fields
                                                    }
                                                    _ => unreachable!(),
                                                })
                                                .unwrap_or(Default::default());
                                            d.extend(fields.clone());
                                            d
                                        })
                                        .implements(&Type::Dict(
                                            named_fields
                                                .fields
                                                .iter()
                                                .map(|(n, t)| (n.clone(), t.clone()))
                                                .collect(),
                                        )) =>
                                    {
                                        Ok(Value::Data(Data {
                                            name: variant.name.clone(),
                                            fields: Some(DataFields::Named(NamedDataFields {
                                                fields: {
                                                    let mut d = defaults
                                                        .get(&variant.name)
                                                        .cloned()
                                                        .map(|d| match d {
                                                            DataFields::Named(d_named_fields) => {
                                                                d_named_fields.fields
                                                            }
                                                            _ => unreachable!(),
                                                        })
                                                        .unwrap_or(Default::default());
                                                    d.extend(fields.clone());
                                                    d
                                                },
                                            })),
                                        }))
                                    }
                                    // Bad type
                                    Some(x) => Err(Error::UnexpectedType {
                                        expected: ret.clone(),
                                        got: x.clone(),
                                        trace: Default::default(),
                                    }),
                                }
                            })
                        }),
                    );
                }
                Some(ADTVariantFields::Unnamed(unnamed_fields)) => {
                    let ret = ret.clone();
                    let unnamed_fields = unnamed_fields.clone();
                    let function = Function {
                        id: id_dispenser.next(),
                        name: variant.name.clone(),
                        params: vec![Type::Tuple(unnamed_fields.fields.clone())],
                        ret: ret.clone(),
                    };
                    self.register_function(
                        function,
                        Box::new(move |_ctx, _runner, _state, args| {
                            let ret = ret.clone();
                            let unnamed_fields = unnamed_fields.clone();
                            let variant = variant.clone();
                            Box::pin(async move {
                                match args.first() {
                                    // Wrong number of arguments
                                    None => Err(Error::ExpectedArguments {
                                        expected: 1,
                                        got: args.len(),
                                        trace: Default::default(),
                                    }),
                                    // Implementation
                                    Some(tuple @ Value::Tuple(fields))
                                        if tuple.implements(&Type::Tuple(
                                            unnamed_fields.fields.clone(),
                                        )) =>
                                    {
                                        Ok(Value::Data(Data {
                                            name: variant.name.clone(),
                                            fields: Some(DataFields::Unnamed(UnnamedDataFields {
                                                fields: fields.clone(),
                                            })),
                                        }))
                                    }
                                    // Bad type
                                    Some(x) => Err(Error::UnexpectedType {
                                        expected: ret.clone(),
                                        got: x.clone(),
                                        trace: Default::default(),
                                    }),
                                }
                            })
                        }),
                    );
                }

                // FIXME: We do not currently support ADT variants that do not
                // have fields
                _ => unimplemented!(),
            }
        }
    }

    pub async fn lookup(&self, _ctx: &Context, var: &Var) -> Result<Value, Error> {
        match self.ftable.get(&var.id) {
            Some((f, _)) => Ok(Value::Function(f.clone())),
            None => match &self.fallback {
                Some(_) => {
                    // Create a dummy function for the fallback
                    let dummy_function = Function {
                        id: var.id,
                        name: var.name.clone(),
                        params: vec![], // Assuming no parameters for fallback
                        ret: a!(),      // Assuming any return type
                    };
                    Ok(Value::Function(dummy_function))
                }
                None => Err(Error::VarNotFound {
                    var: var.clone(),
                    trace: Default::default(),
                }),
            },
        }
    }

    pub async fn dispatch(
        &self,
        ctx: &Context,
        ftable: &Ftable<S>,
        state: &S,
        function: &Function,
        args: &Vec<Value>,
    ) -> Result<Value, Error> {
        match self.ftable.get(&function.id) {
            Some((_f, lam)) => lam(ctx, ftable, state, args).await,
            None => match &self.fallback {
                Some(fallback) => fallback(ctx, ftable, state, args).await,
                None => Err(Error::FunctionNotFound {
                    function: function.clone(),
                    trace: Default::default(),
                }),
            },
        }
    }

    pub fn scope(&self) -> Scope {
        let mut scope = Scope::new();
        for (id, (f, _)) in &self.ftable {
            scope.vars.insert(f.name.clone(), *id);
        }
        scope
    }
}
