use std::{
    borrow::{Borrow, BorrowMut},
    marker::PhantomData,
    sync::Arc,
};

use chrono::{DateTime, Utc};
use rex_ast::expr::Expr;
use rex_lexer::span::Span;
use rex_type_system::types::{ToType, Type};
use uuid::Uuid;

use crate::error::Error;

pub trait Encode
where
    Self: Sized,
{
    fn try_encode(self, span: Span) -> Result<Expr, Error>;
}

impl Encode for bool {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Bool(span, self))
    }
}

impl Encode for u8 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uint(span, self as u64))
    }
}

impl Encode for u16 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uint(span, self as u64))
    }
}

impl Encode for u32 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uint(span, self as u64))
    }
}

impl Encode for u64 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uint(span, self))
    }
}

impl Encode for i8 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Int(span, self as i64))
    }
}

impl Encode for i16 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Int(span, self as i64))
    }
}

impl Encode for i32 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Int(span, self as i64))
    }
}

impl Encode for i64 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Int(span, self))
    }
}

impl Encode for f32 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Float(span, self as f64))
    }
}

impl Encode for f64 {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Float(span, self))
    }
}

impl Encode for &str {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::String(span, self.to_string()))
    }
}

impl Encode for String {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::String(span, self))
    }
}

impl Encode for Uuid {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uuid(span, self))
    }
}

impl Encode for DateTime<Utc> {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::DateTime(span, self))
    }
}

impl Encode for () {
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(span, vec![]))
    }
}

impl<T0> Encode for (T0,)
where
    T0: Encode,
{
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(span, vec![self.0.try_encode(span)?]))
    }
}

impl<T0, T1> Encode for (T0, T1)
where
    T0: Encode,
    T1: Encode,
{
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(
            span,
            vec![self.0.try_encode(span)?, self.1.try_encode(span)?],
        ))
    }
}

impl<T0, T1, T2> Encode for (T0, T1, T2)
where
    T0: Encode,
    T1: Encode,
    T2: Encode,
{
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(
            span,
            vec![
                self.0.try_encode(span)?,
                self.1.try_encode(span)?,
                self.2.try_encode(span)?,
            ],
        ))
    }
}

impl<T0, T1, T2, T3> Encode for (T0, T1, T2, T3)
where
    T0: Encode,
    T1: Encode,
    T2: Encode,
    T3: Encode,
{
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(
            span,
            vec![
                self.0.try_encode(span)?,
                self.1.try_encode(span)?,
                self.2.try_encode(span)?,
                self.3.try_encode(span)?,
            ],
        ))
    }
}

impl<T> Encode for Vec<T>
where
    T: Encode,
{
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        let mut ys = Vec::with_capacity(self.len());
        for x in self {
            ys.push(x.try_encode(span)?);
        }
        Ok(Expr::List(span, ys))
    }
}

impl<T, E> Encode for Result<T, E>
where
    T: Encode,
    E: Encode,
{
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        match self {
            Ok(x) => Ok(Expr::Named(
                span,
                "Ok".to_string(),
                Some(Box::new(x.try_encode(span)?)),
            )),
            Err(x) => Ok(Expr::Named(
                span,
                "Err".to_string(),
                Some(Box::new(x.try_encode(span)?)),
            )),
        }
    }
}

impl<T> Encode for Option<T>
where
    T: Encode,
{
    fn try_encode(self, span: Span) -> Result<Expr, Error> {
        match self {
            Some(x) => Ok(Expr::Named(
                span,
                "Some".to_string(),
                Some(Box::new(x.try_encode(span)?)),
            )),
            None => Ok(Expr::Named(span, "None".to_string(), None)),
        }
    }
}

pub trait Decode
where
    Self: Sized,
{
    fn try_decode(v: &Expr) -> Result<Self, Error>;
}

impl Decode for bool {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Bool(_, x) => Ok(*x as bool),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Type::Int),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for u8 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Uuid(_, u) => Ok(u.clone()),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for DateTime<Utc> {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::DateTime(_, dt) => Ok(dt.clone()),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Arc::new(Self::to_type()),
                got: v.clone(),
                trace: Default::default(),
            }),
        }
    }
}

impl Decode for () {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Tuple(_span, xs) if xs.len() == 0 => Ok(()),
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
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
    pub expr: Expr,
    _a: PhantomData<A>,
    _b: PhantomData<B>,
}

impl<A, B> Borrow<Expr> for Func<A, B> {
    fn borrow(&self) -> &Expr {
        &self.expr
    }
}

impl<A, B> Borrow<Expr> for &Func<A, B> {
    fn borrow(&self) -> &Expr {
        &self.expr
    }
}

impl<A, B> Borrow<Expr> for &mut Func<A, B> {
    fn borrow(&self) -> &Expr {
        &self.expr
    }
}

impl<A, B> BorrowMut<Expr> for Func<A, B> {
    fn borrow_mut(&mut self) -> &mut Expr {
        &mut self.expr
    }
}

impl<A, B> BorrowMut<Expr> for &mut Func<A, B> {
    fn borrow_mut(&mut self) -> &mut Expr {
        &mut self.expr
    }
}

impl<A, B> Encode for Func<A, B> {
    fn try_encode(self, _span: Span) -> Result<Expr, Error> {
        Ok(self.expr)
    }
}

impl<A, B> Decode for Func<A, B> {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
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
