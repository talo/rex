use std::{collections::HashMap, future::Future, pin::Pin};

use rex_ast::id::Id;
use rex_lexer::span::Span;
use rex_type_system::{
    arrow,
    types::{ToType, Type},
};

use crate::{
    codec::{Decode, Encode},
    error::Error,
    eval::Value,
};

pub trait F<'r>:
    Fn(
        &'r Ftable,
        &'r Vec<Value>,
    ) -> Pin<Box<dyn Future<Output = Result<Value, Error>> + Send + 'r>>
    + Sync
    + Send
{
    fn clone_box(&self) -> FtableFn;
}

impl<'r, G> F<'r> for G
where
    for<'q> G: Fn(
            &'q Ftable,
            &'q Vec<Value>,
        ) -> Pin<Box<dyn Future<Output = Result<Value, Error>> + Send + 'q>>
        + Sync
        + Send
        + Clone
        + 'r + 'q,
{
    fn clone_box(&self) -> FtableFn {
        Box::new((*self).clone())
    }
}

pub type FtableFn = Box<dyn for<'r> F<'r>>;

#[derive(Clone)]
pub struct Ftable(pub HashMap<String, Vec<(Type, FtableFn)>>);

impl Ftable {
    pub fn with_prelude() -> Self {
        let mut ftable = Self(Default::default());
        ftable.register_fn("negate", |x: u64| -(x as i64));
        ftable.register_fn("negate", |x: i64| -x);
        ftable.register_fn("negate", |x: f64| -x);
        ftable.register_fn("+", |x: u64, y: u64| x + y);
        ftable.register_fn("+", |x: i64, y: i64| x + y);
        ftable.register_fn("+", |x: f64, y: f64| x + y);
        ftable.register_fn("-", |x: u64, y: u64| x - y);
        ftable.register_fn("-", |x: i64, y: i64| x - y);
        ftable.register_fn("-", |x: f64, y: f64| x - y);
        ftable.register_fn("*", |x: u64, y: u64| x * y);
        ftable.register_fn("*", |x: i64, y: i64| x * y);
        ftable.register_fn("*", |x: f64, y: f64| x * y);
        ftable.register_fn("/", |x: u64, y: u64| x / y);
        ftable.register_fn("/", |x: i64, y: i64| x / y);
        ftable.register_fn("/", |x: f64, y: f64| x / y);
        ftable.register_fn("abs", |x: i64| x.abs());
        ftable.register_fn("abs", |x: f64| x.abs());
        ftable.register_fn("sqrt", |x: f64| x.sqrt());
        ftable.register_fn("pow", |x: f64, y: i32| x.powi(y));
        ftable.register_fn("pow", |x: f64, y: f64| x.powf(y));
        ftable
    }

    pub fn register_fn<F, A, B>(&mut self, n: impl ToString, f: F)
    where
        F: CallFn<A, B> + Clone + Sync + Send + 'static,
    {
        self.0.entry(n.to_string()).or_default().push((
            arrow!(F::a_type() => F::b_type()),
            Box::new(move |_ftable, args| {
                let f = f.clone();
                Box::pin(async move { f.call(Id(u64::MAX), Span::default(), args).await })
            }),
        ));
    }

    pub fn lookup_fns(&self, n: &str, t: Type) -> impl Iterator<Item = &FtableFn> {
        self.0
            .get(n)
            .map(|v| v.iter())
            .into_iter()
            .flatten()
            .filter_map(move |(ftype, f)| match ftype.maybe_compatible(&t) {
                Ok(()) => Some(f),
                Err(e) => {
                    println!("bad lookup: {}", e);
                    None
                }
            })
    }
}

impl Clone for Box<dyn for<'r> F<'r>> {
    fn clone(&self) -> Self {
        (**self).clone_box()
    }
}

pub fn decode_arg<A>(args: &Vec<Value>, i: usize) -> Result<A, Error>
where
    A: Decode,
{
    args.get(i)
        .ok_or(Error::MissingArgument { argument: i })
        .and_then(|a0| A::try_decode(a0))
}

pub trait CallFn<A0, B> {
    fn call(
        &self,
        id: Id,
        span: Span,
        args: &Vec<Value>,
    ) -> impl Future<Output = Result<Value, Error>> + Send + Sync + '_;
    fn a_type() -> Type;
    fn b_type() -> Type;
}

impl<F, A0, B> CallFn<(A0,), B> for F
where
    F: Fn(A0) -> B + Send + Sync + 'static,
    A0: Decode + ToType + Send + Sync + 'static,
    B: Encode + ToType + Send + Sync + 'static,
{
    fn call(
        &self,
        id: Id,
        span: Span,
        args: &Vec<Value>,
    ) -> impl Future<Output = Result<Value, Error>> + Send + Sync + '_ {
        let a0 = decode_arg(args, 0);
        async move {
            let (a0,) = (a0?,);
            self(a0).try_encode(id, span)
        }
    }

    fn a_type() -> Type {
        A0::to_type()
    }

    fn b_type() -> Type {
        B::to_type()
    }
}

impl<F, A0, A1, B> CallFn<(A0, A1), B> for F
where
    F: Fn(A0, A1) -> B + Send + Sync + 'static,
    A0: Decode + ToType + Send + Sync + 'static,
    A1: Decode + ToType + Send + Sync + 'static,
    B: Encode + ToType + Send + Sync + 'static,
{
    fn call(
        &self,
        id: Id,
        span: Span,
        args: &Vec<Value>,
    ) -> impl Future<Output = Result<Value, Error>> + Send + Sync + '_ {
        let a0 = decode_arg(args, 0);
        let a1 = decode_arg(args, 1);
        async move {
            let (a0, a1) = (a0?, a1?);
            self(a0, a1).try_encode(id, span)
        }
    }

    fn a_type() -> Type {
        A0::to_type()
    }

    fn b_type() -> Type {
        arrow!(A1::to_type() => B::to_type())
    }
}

impl<F, A0, A1, A2, B> CallFn<(A0, A1, A2), B> for F
where
    F: Fn(A0, A1, A2) -> B + Send + Sync + 'static,
    A0: Decode + ToType + Send + Sync + 'static,
    A1: Decode + ToType + Send + Sync + 'static,
    A2: Decode + ToType + Send + Sync + 'static,
    B: Encode + ToType + Send + Sync + 'static,
{
    fn call(
        &self,
        id: Id,
        span: Span,
        args: &Vec<Value>,
    ) -> impl Future<Output = Result<Value, Error>> + Send + Sync + '_ {
        let a0 = decode_arg(args, 0);
        let a1 = decode_arg(args, 1);
        let a2 = decode_arg(args, 2);
        async move {
            let (a0, a1, a2) = (a0?, a1?, a2?);
            self(a0, a1, a2).try_encode(id, span)
        }
    }

    fn a_type() -> Type {
        A0::to_type()
    }

    fn b_type() -> Type {
        arrow!(A1::to_type() => B::to_type())
    }
}

impl<F, A0, A1, A2, A3, B> CallFn<(A0, A1, A2, A3), B> for F
where
    F: Fn(A0, A1, A2, A3) -> B + Send + Sync + 'static,
    A0: Decode + ToType + Send + Sync + 'static,
    A1: Decode + ToType + Send + Sync + 'static,
    A2: Decode + ToType + Send + Sync + 'static,
    A3: Decode + ToType + Send + Sync + 'static,
    B: Encode + ToType + Send + Sync + 'static,
{
    fn call(
        &self,
        id: Id,
        span: Span,
        args: &Vec<Value>,
    ) -> impl Future<Output = Result<Value, Error>> + Send + Sync + '_ {
        let a0 = decode_arg(args, 0);
        let a1 = decode_arg(args, 1);
        let a2 = decode_arg(args, 2);
        let a3 = decode_arg(args, 3);
        async move {
            let (a0, a1, a2, a3) = (a0?, a1?, a2?, a3?);
            self(a0, a1, a2, a3).try_encode(id, span)
        }
    }

    fn a_type() -> Type {
        A0::to_type()
    }

    fn b_type() -> Type {
        arrow!(A1::to_type() => B::to_type())
    }
}
