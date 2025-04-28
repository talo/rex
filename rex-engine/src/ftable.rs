use std::{
    collections::{BTreeMap, HashMap},
    future::Future,
    num::NonZeroUsize,
    pin::Pin,
};

use futures::{stream, StreamExt, TryStreamExt};
use rex_ast::{
    a, arrow, b,
    id::{Id, IdDispenser},
    list, tuple,
    types::{ADTVariantFields, Type, ADT},
};
use rex_resolver::Scope;

use crate::{
    apply::{self, apply0},
    error::Error,
    value::{Data, DataFields, Function, NamedDataFields, UnnamedDataFields, Value},
    Context,
};

pub trait F<'r, S: Send + Sync + 'static>:
    Fn(
        &'r Context,
        &'r Ftable<S>,
        &'r S,
        &'r Vec<Value>,
    ) -> Pin<Box<dyn Future<Output = Result<Value, Error>> + Send + 'r>>
    + Sync
    + Send
{
    fn clone_box(&self) -> FtableFn<S>;
}

impl<'r, G, S> F<'r, S> for G
where
    S: Send + Sync + 'static,
    for<'q> G: Fn(
            &'q Context,
            &'q Ftable<S>,
            &'q S,
            &'q Vec<Value>,
        ) -> Pin<Box<dyn Future<Output = Result<Value, Error>> + Send + 'q>>
        + Sync
        + Send
        + Clone
        + 'r + 'q,
{
    fn clone_box(&self) -> FtableFn<S> {
        Box::new((*self).clone())
    }
}

pub type FtableFn<S> = Box<dyn for<'r> F<'r, S>>;

#[derive(Clone)]
pub struct Ftable<S: Send + Sync + 'static> {
    pub ftable: HashMap<Id, (Function, FtableFn<S>)>,
}

impl<S> Clone for Box<dyn for<'r> F<'r, S>>
where
    S: Send + Sync + 'static,
{
    fn clone(&self) -> Self {
        (**self).clone_box()
    }
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
        }
    }

    pub fn with_intrinsics(id_dispenser: &mut IdDispenser) -> Self {
        let mut this = Self {
            ftable: HashMap::new(),
        };

        // boolean logic
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "&&".to_string(),
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
                        (Some(Value::Bool(x)), Some(Value::Bool(y))) => Ok(Value::Bool(*x && *y)),
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
                name: "||".to_string(),
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
                        (Some(Value::Bool(x)), Some(Value::Bool(y))) => Ok(Value::Bool(*x || *y)),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

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
                name: "negate".to_string(),
                params: vec![a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        // Wrong number of arguments
                        None => Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        Some(Value::Uint(x)) => Ok(Value::Int(-(*x as i64))),
                        Some(Value::Int(x)) => Ok(Value::Int(-x)),
                        Some(Value::Float(x)) => Ok(Value::Float(-x)),
                        // Bad types
                        Some(y) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
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
                        (None, None) | (None, _) | (_, None) => Err(Error::ExpectedArguments {
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
        // 'sqrt'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "sqrt".to_string(),
                params: vec![a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        // Wrong number of arguments
                        None => Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                        // Implementation
                        Some(Value::Uint(x)) => Ok(Value::Float((*x as f64).sqrt())),
                        Some(Value::Int(x)) => Ok(Value::Float((*x as f64).sqrt())),
                        Some(Value::Float(x)) => Ok(Value::Float(x.sqrt())),
                        // Bad types
                        Some(y) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
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
                                ys.push(apply::apply(ctx, runner, state, f.clone(), x.clone()));
                            }
                            let nthreads = std::thread::available_parallelism()
                                .unwrap_or(NonZeroUsize::new(1).unwrap())
                                .get()
                                * 2;
                            let s = stream::iter(ys).buffered(nthreads).try_collect().await?;

                            Ok(Value::List(s))
                        }
                        (Some(f), Some(Value::Result(Ok(x)))) => {
                            let r = apply::apply(ctx, runner, state, f.clone(), *x.clone()).await?;
                            Ok(Value::Result(Ok(Box::new(r))))
                        }
                        (Some(_), Some(Value::Result(Err(e)))) => Ok(Value::Result(Err(e.clone()))),
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
        // 'filter_map'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "filter_map".to_string(),
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
                                ys.push(apply::apply(ctx, runner, state, f.clone(), x.clone()));
                            }
                            let nthreads = std::thread::available_parallelism()
                                .unwrap_or(NonZeroUsize::new(1).unwrap())
                                .get()
                                * 2;
                            let s: Vec<Value> =
                                stream::iter(ys).buffered(nthreads).try_collect().await?;
                            let mut result: Vec<Value> = Vec::new();
                            for value in s {
                                match &value {
                                    Value::Option(Some(x)) => {
                                        result.push(Value::clone(x));
                                    }
                                    Value::Option(None) => {
                                        // Omit from result
                                    }
                                    _ => {
                                        return Err(Error::UnexpectedType {
                                            expected: Type::Option(Box::new(a!())),
                                            got: value.clone(),
                                            trace: Default::default(),
                                        })
                                    }
                                }
                            }

                            Ok(Value::List(result))
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
        // 'unwrap_or_else'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "unwrap_or_else".to_string(),
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
                        (Some(_), Some(Value::Result(Ok(x)))) => Ok(Value::clone(x)),
                        (Some(f), Some(Value::Result(Err(e)))) => {
                            Ok(apply::apply(ctx, runner, state, f.clone(), *e.clone()).await?)
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
        // 'flatmap'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "flatmap".to_string(),
                params: vec![list!(list!(a!()))],
                ret: list!(a!()),
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        // Implementation
                        Some(Value::List(xs)) => {
                            let mut acc = vec![];
                            for x in xs {
                                match x {
                                    Value::List(xs) => {
                                        for x in xs {
                                            acc.push(x.clone());
                                        }
                                    }
                                    x => {
                                        return Err(Error::UnexpectedType {
                                            expected: list!(a!()),
                                            got: x.clone(),
                                            trace: Default::default(),
                                        })
                                    }
                                }
                            }
                            Ok(Value::List(acc))
                        }
                        // Everything else
                        Some(x) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        _ => Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // 'fold'
        // FIXME: doesn't work as the lambda doesnt seem to get applied
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "fold".to_string(),
                params: vec![arrow!(tuple!(b!(), a!()) => b!()), b!(), list!(a!())],
                ret: b!(),
            },
            Box::new(|ctx, runner, state, args| {
                Box::pin(async move {
                    match (args.first(), args.get(1), args.get(2)) {
                        // Implementation
                        (Some(f), Some(z), Some(Value::List(xs))) => {
                            let mut acc = z.clone();
                            for x in xs {
                                acc = apply::apply(
                                    ctx,
                                    runner,
                                    state,
                                    f.clone(),
                                    Value::Tuple(vec![acc.clone(), x.clone()]),
                                )
                                .await?;
                            }
                            Ok(acc)
                        }
                        // Everything else
                        (Some(_), Some(_), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        _ => Err(Error::ExpectedArguments {
                            expected: 3,
                            got: args.len(),
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
                                match apply::apply(ctx, runner, state, f.clone(), x.clone()).await?
                                {
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
        // 'take'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "take".to_string(),
                params: vec![Type::Uint, list!(a!())],
                ret: list!(a!()),
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
                        (Some(Value::Uint(n)), Some(Value::List(xs))) => {
                            Ok(Value::List(xs.iter().take(*n as usize).cloned().collect()))
                        }
                        // Bad types
                        (Some(Value::Uint(_)), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        (Some(n), _) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: n.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );
        // 'skip'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "skip".to_string(),
                params: vec![Type::Uint, list!(a!())],
                ret: list!(a!()),
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
                        (Some(Value::Uint(n)), Some(Value::List(xs))) => {
                            Ok(Value::List(xs.iter().skip(*n as usize).cloned().collect()))
                        }
                        // Bad types
                        (Some(Value::Uint(_)), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        (Some(n), _) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: n.clone(),
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
        // unzip a list of tuples
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "unzip".to_string(),
                params: vec![Type::List(Box::new(Type::Tuple(vec![a!(), a!()])))],
                ret: Type::Tuple(vec![Type::List(Box::new(a!())), Type::List(Box::new(a!()))]),
            },
            Box::new(|_ctx, _ftable, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        // Wrong number of arguments
                        None => Err(Error::ExpectedArguments {
                            expected: 1,
                            got: 0,
                            trace: Default::default(),
                        }),
                        // Implementation
                        Some(Value::List(xs)) => {
                            let mut ys = Vec::with_capacity(xs.len());
                            let mut zs = Vec::with_capacity(xs.len());
                            for x in xs {
                                match x {
                                    Value::Tuple(xs) => match xs.as_slice() {
                                        [x, y] => {
                                            ys.push(x.clone());
                                            zs.push(y.clone());
                                        }
                                        _ => Err(Error::ExpectedTuple {
                                            expected: 2,
                                            got: xs.len(),
                                            trace: Default::default(),
                                        })?,
                                    },
                                    x => Err(Error::UnexpectedType {
                                        expected: Type::Tuple(vec![a!(), a!()]),
                                        got: x.clone(),
                                        trace: Default::default(),
                                    })?,
                                }
                            }
                            Ok(Value::Tuple(vec![Value::List(ys), Value::List(zs)]))
                        }
                        // Everything else
                        Some(x) => Err(Error::UnexpectedType {
                            expected: Type::List(Box::new(Type::Tuple(vec![a!(), a!()]))),
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
        // 'len'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "len".to_string(),
                // FIXME: needs trait with length
                params: vec![a!()], //vec![list!(a!())],
                ret: Type::Uint,
            },
            Box::new(|_ctx, _runner, _state, args| {
                Box::pin(async move {
                    match args.first() {
                        Some(Value::List(xs)) => Ok(Value::Uint(xs.len() as u64)),
                        Some(Value::Tuple(xs)) => Ok(Value::Uint(xs.len() as u64)),
                        Some(Value::String(xs)) => Ok(Value::Uint(xs.len() as u64)),
                        Some(_) => Err(Error::UnexpectedType {
                            expected: Type::List(Box::new(a!())),
                            got: args.first().unwrap().clone(),
                            trace: Default::default(),
                        }),
                        None => Err(Error::ExpectedArguments {
                            expected: 1,
                            got: 0,
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // 'has'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "has".to_string(),
                params: vec![Type::String, a!()],
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
                        (Some(Value::String(key)), Some(Value::Dict(xs))) => {
                            Ok(Value::Bool(xs.contains_key(key.as_str())))
                        }
                        // Bad types
                        (Some(x), Some(Value::Dict(_))) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (_, Some(x)) => Err(Error::ExpectedKeyable {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // 'has'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "has".to_string(),
                params: vec![Type::String, a!()],
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
                        (Some(Value::String(key)), Some(Value::Dict(xs))) => {
                            Ok(Value::Bool(xs.contains_key(key.as_str())))
                        }
                        // Bad types
                        (Some(x), Some(Value::Dict(_))) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                        // Everything else
                        (_, Some(x)) => Err(Error::ExpectedKeyable {
                            got: x.clone(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // casts
        // int cast
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "int".to_string(),
                params: vec![a!()],
                ret: Type::Int,
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { cast_to(ctx, state, ftable, args, Type::Int).await })
            }),
        );

        // uint cast
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "uint".to_string(),
                params: vec![a!()],
                ret: Type::Uint,
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { cast_to(ctx, state, ftable, args, Type::Uint).await })
            }),
        );

        // float cast
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "float".to_string(),
                params: vec![a!()],
                ret: Type::Uint,
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { cast_to(ctx, state, ftable, args, Type::Float).await })
            }),
        );

        // string cast
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "str".to_string(),
                params: vec![a!()],
                ret: Type::Uint,
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { cast_to(ctx, state, ftable, args, Type::String).await })
            }),
        );

        // list cast dict to key value pairss
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "list".to_string(),
                params: vec![a!()],
                ret: Type::List(Box::new(a!())),
            },
            Box::new(|_ctx, _ftable, _state, args| {
                Box::pin(async move {
                    if args.len() != 1 {
                        return Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        });
                    }

                    let dict = match &args[0] {
                        Value::Dict(data) => data,
                        _ => {
                            return Err(Error::UnexpectedType {
                                expected: Type::Dict(BTreeMap::new()),
                                got: args[0].clone(),
                                trace: Default::default(),
                            })
                        }
                    };

                    Ok(Value::List(
                        dict.iter()
                            .map(|(k, v)| {
                                Value::Tuple(vec![Value::String(k.to_string()), v.clone()])
                            })
                            .collect(),
                    ))
                })
            }),
        );

        // downcast adts to dicts
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "dict".to_string(),
                params: vec![a!()],
                ret: Type::List(Box::new(a!())),
            },
            Box::new(|_ctx, _ftable, _state, args| {
                Box::pin(async move {
                    if args.len() != 1 {
                        return Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        });
                    }

                    let adt = match &args[0] {
                        Value::Data(data) => data,
                        Value::Dict(dict) => return Ok(Value::Dict(dict.clone())),
                        _ => {
                            return Err(Error::UnexpectedType {
                                expected: Type::Dict(BTreeMap::new()),
                                got: args[0].clone(),
                                trace: Default::default(),
                            })
                        }
                    };

                    let fields = match &adt.fields {
                        Some(DataFields::Named(fields)) => fields,
                        _ => {
                            return Err(Error::Custom {
                                error: "Expected named fields in adt".to_string(),
                                trace: Default::default(),
                            })
                        }
                    };

                    let dict = fields.fields.clone().into_iter().collect();

                    Ok(Value::Dict(dict))
                })
            }),
        );

        // sum function
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "sum".to_string(),
                params: vec![list!(a!())],
                ret: a!(),
            },
            Box::new(|_ctx, _ftable, _state, args| {
                Box::pin(async move {
                    if args.len() != 1 {
                        return Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        });
                    }

                    let list = match &args[0] {
                        Value::List(l) => l,
                        _ => {
                            return Err(Error::UnexpectedType {
                                expected: list!(a!()),
                                got: args[0].clone(),
                                trace: Default::default(),
                            })
                        }
                    };

                    let mut sum = 0.0;
                    let mut t = Type::Float;

                    for value in list {
                        match value {
                            Value::Int(n) => {
                                t = Type::Int;
                                sum += *n as f64
                            }
                            Value::Uint(n) => {
                                t = Type::Uint;
                                sum += *n as f64
                            }
                            Value::Float(n) => sum += *n,
                            _ => {
                                return Err(Error::UnexpectedType {
                                    expected: Type::Float,
                                    got: value.clone(),
                                    trace: Default::default(),
                                })
                            }
                        }
                    }
                    // cast back
                    match t {
                        Type::Int => Ok(Value::Int(sum as i64)),
                        Type::Uint => Ok(Value::Uint(sum as u64)),
                        Type::Float => Ok(Value::Float(sum)),
                        _ => Err(Error::Custom {
                            error: "Invalid type".to_string(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // avg function
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "avg".to_string(),
                params: vec![list!(a!())],
                ret: a!(),
            },
            Box::new(|_ctx, _ftable, _state, args| {
                Box::pin(async move {
                    if args.len() != 1 {
                        return Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        });
                    }

                    let list = match &args[0] {
                        Value::List(l) => l,
                        _ => {
                            return Err(Error::UnexpectedType {
                                expected: list!(a!()),
                                got: args[0].clone(),
                                trace: Default::default(),
                            })
                        }
                    };

                    let mut sum = 0.0;

                    for value in list {
                        match value {
                            Value::Int(n) => sum += *n as f64,
                            Value::Uint(n) => sum += *n as f64,
                            Value::Float(n) => sum += *n,
                            _ => {
                                return Err(Error::UnexpectedType {
                                    expected: Type::Float,
                                    got: value.clone(),
                                    trace: Default::default(),
                                })
                            }
                        }
                    }

                    Ok(Value::Float(sum / list.len() as f64))
                })
            }),
        );

        // min function
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "list_min".to_string(),
                params: vec![list!(a!())],
                ret: a!(),
            },
            Box::new(|_ctx, _ftable, _state, args| {
                Box::pin(async move {
                    if args.len() != 1 {
                        return Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        });
                    }

                    let list = match &args[0] {
                        Value::List(l) => l,
                        _ => {
                            return Err(Error::UnexpectedType {
                                expected: list!(a!()),
                                got: args[0].clone(),
                                trace: Default::default(),
                            })
                        }
                    };

                    let mut min = f64::INFINITY;
                    let mut t = Type::Float;

                    for value in list {
                        match value {
                            Value::Int(n) => {
                                t = Type::Int;
                                min = min.min(*n as f64)
                            }
                            Value::Uint(n) => {
                                t = Type::Uint;
                                min = min.min(*n as f64);
                            }
                            Value::Float(n) => min = min.min(*n),
                            _ => {
                                return Err(Error::UnexpectedType {
                                    expected: Type::Float,
                                    got: value.clone(),
                                    trace: Default::default(),
                                })
                            }
                        }
                    }
                    // cast back
                    match t {
                        Type::Int => Ok(Value::Int(min as i64)),
                        Type::Uint => Ok(Value::Uint(min as u64)),
                        Type::Float => Ok(Value::Float(min)),
                        _ => Err(Error::Custom {
                            error: "Invalid type".to_string(),
                            trace: Default::default(),
                        }),
                    }
                })
            }),
        );

        // min_by function
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "list_min_by".to_string(),
                params: vec![arrow!(a!() => b!()), Type::List(Box::new(a!()))],
                ret: a!(),
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { list_by(ctx, state, ftable, args, |a, b| a < b).await })
            }),
        );

        // max function
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "list_max".to_string(),
                params: vec![Type::List(Box::new(a!()))],
                ret: a!(),
            },
            Box::new(|_ctx, _ftable, _state, args| {
                Box::pin(async move {
                    if args.len() != 1 {
                        return Err(Error::ExpectedArguments {
                            expected: 1,
                            got: args.len(),
                            trace: Default::default(),
                        });
                    }

                    let list = match &args[0] {
                        Value::List(l) => l,
                        _ => {
                            return Err(Error::UnexpectedType {
                                expected: list!(a!()),
                                got: args[0].clone(),
                                trace: Default::default(),
                            })
                        }
                    };

                    let mut max = f64::NEG_INFINITY;

                    for value in list {
                        match value {
                            Value::Int(n) => max = max.max(*n as f64),
                            Value::Uint(n) => max = max.max(*n as f64),
                            Value::Float(n) => max = max.max(*n),
                            _ => {
                                return Err(Error::Custom {
                                    error: "List must contain only numbers".to_string(),
                                    trace: Default::default(),
                                })
                            }
                        }
                    }

                    Ok(Value::Float(max))
                })
            }),
        );

        // max_by function
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "list_max_by".to_string(),
                params: vec![arrow!(a!() => b!()), list!(a!())],
                ret: a!(),
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { list_by(ctx, state, ftable, args, |a, b| a > b).await })
            }),
        );

        // mae function
        #[cfg(feature = "stats")]
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "mae".to_string(),
                params: vec![
                    Type::List(Box::new(Type::Float)),
                    Type::List(Box::new(Type::Float)),
                ],
                ret: Type::Float,
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { crate::stats::mae(ctx, state, ftable, args).await })
            }),
        );

        // rmse function
        #[cfg(feature = "stats")]
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "rmse".to_string(),
                params: vec![
                    Type::List(Box::new(Type::Float)),
                    Type::List(Box::new(Type::Float)),
                ],
                ret: Type::Float,
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { crate::stats::rmse(ctx, state, ftable, args).await })
            }),
        );

        // pearson_corr function
        #[cfg(feature = "stats")]
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "pearson_corr".to_string(),
                params: vec![
                    Type::List(Box::new(Type::Float)),
                    Type::List(Box::new(Type::Float)),
                ],
                ret: Type::Float,
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { crate::stats::pearson_corr(ctx, state, ftable, args).await })
            }),
        );

        // spearman_corr function
        #[cfg(feature = "stats")]
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "spearman_corr".to_string(),
                params: vec![
                    Type::List(Box::new(Type::Float)),
                    Type::List(Box::new(Type::Float)),
                ],
                ret: Type::Float,
            },
            Box::new(|ctx, ftable, state, args| {
                Box::pin(async move { crate::stats::spearman_corr(ctx, state, ftable, args).await })
            }),
        );

        // range function
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "range".to_string(),
                params: vec![Type::Int, Type::Int, Type::Option(Box::new(Type::Int))],
                ret: Type::List(Box::new(Type::Int)),
            },
            Box::new(|_, _, _, args| {
                Box::pin(async move {
                    if args.len() < 2 || args.len() > 3 {
                        return Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                            trace: Default::default(),
                        });
                    }

                    let start = match &args[0] {
                        Value::Int(n) => *n,
                        _ => {
                            return Err(Error::UnexpectedType {
                                expected: Type::Int,
                                got: args[0].clone(),
                                trace: Default::default(),
                            })
                        }
                    };

                    let end = match &args[1] {
                        Value::Int(n) => *n,
                        _ => {
                            return Err(Error::UnexpectedType {
                                expected: Type::Int,
                                got: args[1].clone(),
                                trace: Default::default(),
                            })
                        }
                    };

                    let step = if args.len() == 3 {
                        match &args[2] {
                            Value::Option(Some(n)) => match **n {
                                Value::Int(n) => n,
                                _ => {
                                    return Err(Error::UnexpectedType {
                                        expected: Type::Int,
                                        got: (**n).clone(),
                                        trace: Default::default(),
                                    })
                                }
                            },
                            Value::Option(None) => 1,
                            _ => {
                                return Err(Error::UnexpectedType {
                                    expected: Type::Option(Box::new(Type::Int)),
                                    got: args[2].clone(),
                                    trace: Default::default(),
                                })
                            }
                        }
                    } else {
                        1
                    };

                    if step == 0 {
                        return Err(Error::Custom {
                            error: "Step cannot be zero".to_string(),
                            trace: Default::default(),
                        });
                    }

                    let range: Vec<Value> = if step > 0 {
                        (start..end)
                            .step_by(step as usize)
                            .map(Value::Int)
                            .collect()
                    } else {
                        (end..start)
                            .rev()
                            .step_by((-step) as usize)
                            .map(Value::Int)
                            .collect()
                    };

                    Ok(Value::List(range))
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
                        Box::new(move |ctx, ftable, state, args| {
                            let ret = ret.clone();
                            let variant = variant.clone();
                            let named_fields = named_fields.clone();
                            let defaults = defaults.clone();

                            Box::pin(async move {
                                // If there are defaults that match this type
                                // ctor then we need to evaluate all of their
                                // sub-values
                                let defaults = match defaults.get(&variant.name) {
                                    Some(data_fields) => match apply0(
                                        ctx,
                                        ftable,
                                        state,
                                        Value::Data(Data {
                                            name: variant.name.clone(),
                                            fields: Some(data_fields.clone()),
                                        }),
                                    )
                                    .await?
                                    {
                                        Value::Data(Data {
                                            fields: Some(DataFields::Named(named_fields)),
                                            ..
                                        }) => named_fields.fields,
                                        _ => Default::default(),
                                    },
                                    _ => Default::default(),
                                };

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
                                            let mut d = defaults.clone();
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
                                                    let mut d = defaults.clone();
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

    pub async fn lookup(&self, _ctx: &Context, id: &Id) -> Result<Value, Error> {
        self.ftable
            .get(id)
            .map(|(f, _)| Value::Function(f.clone()))
            .ok_or(Error::IdNotFound {
                id: *id,
                trace: Default::default(),
            })
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
            None => Err(Error::FunctionNotFound {
                function: function.clone(),
                trace: Default::default(),
            }),
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

// list_by
async fn list_by<S: Send + Sync + 'static>(
    ctx: &Context,
    state: &S,
    ftable: &Ftable<S>,
    args: &[Value],
    f: impl Fn(f64, f64) -> bool,
) -> Result<Value, Error> {
    if args.len() != 2 {
        return Err(Error::Custom {
            error: "list_by function expects 2 arguments: a function and a list of values"
                .to_string(),
            trace: Default::default(),
        });
    }

    let func = &args[0];

    let list = match &args[1] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "Second argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    let mut x = f64::INFINITY;
    let mut x_value = Value::Null;

    for value in list {
        let result = apply::apply(ctx, ftable, state, func.clone(), value.clone()).await?;

        match result {
            Value::Int(n) => {
                if f(n as f64, x) {
                    x = n as f64;
                    x_value = value.clone();
                }
            }
            Value::Uint(n) => {
                if f(n as f64, x) {
                    x = n as f64;
                    x_value = value.clone();
                }
            }
            Value::Float(n) => {
                if f(n, x) {
                    x = n;
                    x_value = value.clone();
                }
            }
            _ => {
                return Err(Error::Custom {
                    error: "List must contain only numbers".to_string(),
                    trace: Default::default(),
                })
            }
        }
    }

    Ok(x_value)
}

async fn cast_to<S: Send + Sync + 'static>(
    _ctx: &Context,
    _state: &S,
    _ftable: &Ftable<S>,
    args: &[Value],
    t: Type,
) -> Result<Value, Error> {
    match (args.first(), t) {
        (Some(Value::String(s)), Type::Int) => match s.parse() {
            Ok(i) => Ok(Value::Int(i)),
            Err(e) => Err(Error::Custom {
                error: format!("Failed to parse int: {}", e),
                trace: Default::default(),
            }),
        },
        (Some(Value::String(s)), Type::Uint) => match s.parse() {
            Ok(i) => Ok(Value::Uint(i)),
            Err(e) => Err(Error::Custom {
                error: format!("Failed to parse uint: {}", e),
                trace: Default::default(),
            }),
        },
        (Some(Value::String(s)), Type::Float) => match s.parse() {
            Ok(i) => Ok(Value::Float(i)),
            Err(e) => Err(Error::Custom {
                error: format!("Failed to parse float: {}", e),
                trace: Default::default(),
            }),
        },
        (Some(Value::Float(s)), Type::Int) => Ok(Value::Int(*s as i64)),
        (Some(Value::Uint(s)), Type::Int) => Ok(Value::Int(*s as i64)),
        (Some(Value::Float(s)), Type::Uint) => Ok(Value::Uint(*s as u64)),
        (Some(Value::Int(s)), Type::Uint) => Ok(Value::Uint(*s as u64)),
        (Some(Value::Int(s)), Type::Float) => Ok(Value::Float(*s as f64)),
        (Some(Value::Uint(s)), Type::Float) => Ok(Value::Float(*s as f64)),
        (Some(v), Type::String) => Ok(Value::String(v.to_string())),
        (Some(v), _) => Err(Error::UnexpectedType {
            expected: Type::String,
            got: v.clone(),
            trace: Default::default(),
        }),
        (None, _) => Err(Error::UnexpectedType {
            expected: Type::String,
            got: Value::Null,
            trace: Default::default(),
        }),
    }
}
