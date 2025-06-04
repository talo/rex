use std::{borrow::Borrow, marker::PhantomData, sync::Arc};

use chrono::{DateTime, Utc};
use rex_ast::expr::Expr;
use rex_lexer::span::Span;
use rex_type_system::types::{ToType, Type};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use uuid::Uuid;

use crate::error::Error;

pub trait Encode
where
    Self: Sized,
{
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error>;
}

impl Encode for bool {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Bool(span, self)))
    }
}

impl Encode for u8 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Uint(span, self as u64)))
    }
}

impl Encode for u16 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Uint(span, self as u64)))
    }
}

impl Encode for u32 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Uint(span, self as u64)))
    }
}

impl Encode for u64 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Uint(span, self)))
    }
}

impl Encode for i8 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Int(span, self as i64)))
    }
}

impl Encode for i16 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Int(span, self as i64)))
    }
}

impl Encode for i32 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Int(span, self as i64)))
    }
}

impl Encode for i64 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Int(span, self)))
    }
}

impl Encode for f32 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Float(span, self as f64)))
    }
}

impl Encode for f64 {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Float(span, self)))
    }
}

impl Encode for &str {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::String(span, self.to_string())))
    }
}

impl Encode for String {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::String(span, self)))
    }
}

impl Encode for Uuid {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Uuid(span, self)))
    }
}

impl Encode for DateTime<Utc> {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::DateTime(span, self)))
    }
}

impl Encode for serde_json::Value {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Named(
            span,
            "serde_json::Value".to_string(),
            Some(Arc::new(Expr::String(
                span,
                serde_json::to_string(&self).map_err(|e| Error::from(e.to_string()))?,
            ))),
        )))
    }
}

impl Encode for () {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Tuple(span, vec![])))
    }
}

impl<T0> Encode for (T0,)
where
    T0: Encode,
{
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Tuple(span, vec![self.0.try_encode(span)?])))
    }
}

impl<T0, T1> Encode for (T0, T1)
where
    T0: Encode,
    T1: Encode,
{
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Tuple(
            span,
            vec![self.0.try_encode(span)?, self.1.try_encode(span)?],
        )))
    }
}

impl<T0, T1, T2> Encode for (T0, T1, T2)
where
    T0: Encode,
    T1: Encode,
    T2: Encode,
{
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Tuple(
            span,
            vec![
                self.0.try_encode(span)?,
                self.1.try_encode(span)?,
                self.2.try_encode(span)?,
            ],
        )))
    }
}

impl<T0, T1, T2, T3> Encode for (T0, T1, T2, T3)
where
    T0: Encode,
    T1: Encode,
    T2: Encode,
    T3: Encode,
{
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Tuple(
            span,
            vec![
                self.0.try_encode(span)?,
                self.1.try_encode(span)?,
                self.2.try_encode(span)?,
                self.3.try_encode(span)?,
            ],
        )))
    }
}

impl<T> Encode for Vec<T>
where
    T: Encode,
{
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        let mut ys = Vec::with_capacity(self.len());
        for x in self {
            ys.push(x.try_encode(span)?);
        }
        Ok(Arc::new(Expr::List(span, ys)))
    }
}

impl<T, E> Encode for Result<T, E>
where
    T: Encode,
    E: Encode,
{
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        match self {
            Ok(x) => Ok(Arc::new(Expr::Named(
                span,
                "Ok".to_string(),
                Some(x.try_encode(span)?),
            ))),
            Err(x) => Ok(Arc::new(Expr::Named(
                span,
                "Err".to_string(),
                Some(x.try_encode(span)?),
            ))),
        }
    }
}

impl<T> Encode for Option<T>
where
    T: Encode,
{
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        match self {
            Some(x) => Ok(Arc::new(Expr::Named(
                span,
                "Some".to_string(),
                Some(x.try_encode(span)?),
            ))),
            None => Ok(Arc::new(Expr::Named(span, "None".to_string(), None))),
        }
    }
}

pub trait Decode
where
    Self: Sized,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error>;
}

impl Decode for bool {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Bool(_, x) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for u8 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Uint(_, x) => Ok(*x as u8),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Uint),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for u16 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Uint(_, x) => Ok(*x as u16),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Uint),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for u32 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Uint(_, x) => Ok(*x as u32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Uint),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}
impl Decode for u64 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Uint(_, x) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Uint),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for u128 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Uint(_, x) => Ok(*x as u128),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Uint),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for i8 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Int(_, x) => Ok(*x as i8),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for i16 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Int(_, x) => Ok(*x as i16),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for i32 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Int(_, x) => Ok(*x as i32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for i64 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Int(_, x) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for i128 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Int(_, x) => Ok(*x as i128),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for f32 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Float(_, x) => Ok(*x as f32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Float),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for f64 {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Float(_, x) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Float),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for String {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::String(_, x) => Ok(x.clone()),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for Uuid {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Uuid(_, u) => Ok(*u),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for DateTime<Utc> {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::DateTime(_, dt) => Ok(*dt),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for serde_json::Value {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Named(_, n, Some(inner)) if n == "serde_json::Value" => match &**inner {
                Expr::String(_, s) => {
                    Ok(serde_json::from_str(s).map_err(|e| Error::from(e.to_string()))?)
                }
                _ => Err(Error::ExpectedTypeGotValue {
                    expected: Arc::new(Self::to_type()),
                    got: v.clone(),
                    trace: Default::default(),
                }),
            },
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for () {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Tuple(_span, xs) if xs.is_empty() => Ok(()),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl<T0> Decode for (T0,)
where
    T0: Decode + ToType,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Tuple(_span, xs) if xs.len() == 1 => Ok((T0::try_decode(&xs[0])?,)),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl<T0, T1> Decode for (T0, T1)
where
    T0: Decode + ToType,
    T1: Decode + ToType,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Tuple(_span, xs) if xs.len() == 2 => {
                Ok((T0::try_decode(&xs[0])?, T1::try_decode(&xs[1])?))
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl<T0, T1, T2> Decode for (T0, T1, T2)
where
    T0: Decode + ToType,
    T1: Decode + ToType,
    T2: Decode + ToType,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Tuple(_span, xs) if xs.len() == 3 => Ok((
                T0::try_decode(&xs[0])?,
                T1::try_decode(&xs[1])?,
                T2::try_decode(&xs[2])?,
            )),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl<T0, T1, T2, T3> Decode for (T0, T1, T2, T3)
where
    T0: Decode + ToType,
    T1: Decode + ToType,
    T2: Decode + ToType,
    T3: Decode + ToType,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Tuple(_span, xs) if xs.len() == 4 => Ok((
                T0::try_decode(&xs[0])?,
                T1::try_decode(&xs[1])?,
                T2::try_decode(&xs[2])?,
                T3::try_decode(&xs[3])?,
            )),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl<T> Decode for Vec<T>
where
    T: Decode,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::List(_span, xs) => {
                let mut ys = Vec::with_capacity(xs.len());
                for x in xs {
                    ys.push(T::try_decode(x)?);
                }
                Ok(ys)
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl<T, E> Decode for Result<T, E>
where
    T: Decode + ToType,
    E: Decode + ToType,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Named(_span, name, Some(x)) if name == "Ok" => Ok(Ok(T::try_decode(x)?)),
            Expr::Named(_span, name, Some(x)) if name == "Err" => Ok(Err(E::try_decode(x)?)),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Result(Arc::new(T::to_type()), Arc::new(E::to_type()))),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl<T> Decode for Option<T>
where
    T: Decode + ToType,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Named(_span, name, Some(x)) if name == "Some" => Ok(Some(T::try_decode(x)?)),
            Expr::Named(_span, name, None) if name == "None" => Ok(None),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Option(Arc::new(T::to_type()))),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

#[derive(Clone)]
pub struct Func<A, B> {
    pub expr: Arc<Expr>,
    _a: PhantomData<A>,
    _b: PhantomData<B>,
}

impl<A, B> Borrow<Arc<Expr>> for Func<A, B> {
    fn borrow(&self) -> &Arc<Expr> {
        &self.expr
    }
}

impl<A, B> Borrow<Arc<Expr>> for &Func<A, B> {
    fn borrow(&self) -> &Arc<Expr> {
        &self.expr
    }
}

impl<A, B> Encode for Func<A, B> {
    fn try_encode(self, _span: Span) -> Result<Arc<Expr>, Error> {
        Ok(self.expr)
    }
}

impl<A, B> Decode for Func<A, B> {
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        Ok(Self {
            expr: v.clone(),
            _a: PhantomData,
            _b: PhantomData,
        })
    }
}

impl<A, B> ToType for Func<A, B>
where
    A: ToType,
    B: ToType,
{
    fn to_type() -> Type {
        Type::Arrow(Arc::new(A::to_type()), Arc::new(B::to_type()))
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Promise<T> {
    pub uuid: Uuid,
    phantom: PhantomData<T>,
}

impl<T> Serialize for Promise<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.uuid.serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for Promise<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(Promise::new(Uuid::deserialize(deserializer)?))
    }
}

impl<T> Promise<T> {
    pub fn new(uuid: Uuid) -> Self {
        Promise {
            uuid,
            phantom: PhantomData,
        }
    }
}

impl<T> Encode for Promise<T> {
    fn try_encode(self, span: Span) -> Result<Arc<Expr>, Error> {
        Ok(Arc::new(Expr::Promise(span, self.uuid)))
    }
}

impl<T> Decode for Promise<T>
where
    T: ToType,
{
    fn try_decode(v: &Arc<Expr>) -> Result<Self, Error> {
        match &**v {
            Expr::Promise(_, uuid) => Ok(Promise::new(*uuid)),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl<T> ToType for Promise<T>
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::Promise(Arc::new(T::to_type()))
    }
}
