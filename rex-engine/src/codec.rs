use std::{
    borrow::{Borrow, BorrowMut},
    marker::PhantomData,
};

use rex_ast::{expr::Expr, id::Id};
use rex_lexer::span::Span;
use rex_type_system::types::{ToType, Type};

use crate::error::Error;

pub trait Encode
where
    Self: Sized,
{
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error>;
}

impl Encode for bool {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Bool(id, span, self))
    }
}

impl Encode for u8 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uint(id, span, self as u64))
    }
}

impl Encode for u16 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uint(id, span, self as u64))
    }
}

impl Encode for u32 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uint(id, span, self as u64))
    }
}

impl Encode for u64 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Uint(id, span, self))
    }
}

impl Encode for i8 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Int(id, span, self as i64))
    }
}

impl Encode for i16 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Int(id, span, self as i64))
    }
}

impl Encode for i32 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Int(id, span, self as i64))
    }
}

impl Encode for i64 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Int(id, span, self))
    }
}

impl Encode for f32 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Float(id, span, self as f64))
    }
}

impl Encode for f64 {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Float(id, span, self))
    }
}

impl Encode for &str {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::String(id, span, self.to_string()))
    }
}

impl Encode for String {
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::String(id, span, self))
    }
}

impl<T0> Encode for (T0,)
where
    T0: Encode,
{
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(id, span, vec![self.0.try_encode(id, span)?]))
    }
}

impl<T0, T1> Encode for (T0, T1)
where
    T0: Encode,
    T1: Encode,
{
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(
            id,
            span,
            vec![self.0.try_encode(id, span)?, self.1.try_encode(id, span)?],
        ))
    }
}

impl<T0, T1, T2> Encode for (T0, T1, T2)
where
    T0: Encode,
    T1: Encode,
    T2: Encode,
{
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(
            id,
            span,
            vec![
                self.0.try_encode(id, span)?,
                self.1.try_encode(id, span)?,
                self.2.try_encode(id, span)?,
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
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        Ok(Expr::Tuple(
            id,
            span,
            vec![
                self.0.try_encode(id, span)?,
                self.1.try_encode(id, span)?,
                self.2.try_encode(id, span)?,
                self.3.try_encode(id, span)?,
            ],
        ))
    }
}

impl<T> Encode for Vec<T>
where
    T: Encode,
{
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        let mut ys = Vec::with_capacity(self.len());
        for x in self {
            ys.push(x.try_encode(id, span)?);
        }
        Ok(Expr::List(id, span, ys))
    }
}

impl<T, E> Encode for Result<T, E>
where
    T: Encode,
    E: Encode,
{
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        match self {
            Ok(x) => {
                Ok(Expr::Named(id, span, "Ok".to_string(), Box::new(x.try_encode(id, span)?)))
            }
            Err(x) => {
                Ok(Expr::Named(id, span, "Err".to_string(), Box::new(x.try_encode(id, span)?)))
            }
        }
    }
}

impl<T> Encode for Option<T>
where
    T: Encode
{
    fn try_encode(self, id: Id, span: Span) -> Result<Expr, Error> {
        match self {
            Some(x) => {
                Ok(Expr::Named(id, span, "Some".to_string(), Box::new(x.try_encode(id, span)?)))
            }
            None => {
                Ok(Expr::Named(id, span, "None".to_string(), Box::new(Expr::Tuple(id, span, vec![]))))
            }
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
            Expr::Bool(_, _, x) => Ok(*x as bool),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for u8 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Uint(_, _, x) => Ok(*x as u8),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for u16 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Uint(_, _, x) => Ok(*x as u16),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for u32 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Uint(_, _, x) => Ok(*x as u32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}
impl Decode for u64 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Uint(_, _, x) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for u128 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Uint(_, _, x) => Ok(*x as u128),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i8 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Int(_, _, x) => Ok(*x as i8),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i16 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Int(_, _, x) => Ok(*x as i16),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i32 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Int(_, _, x) => Ok(*x as i32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i64 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Int(_, _, x) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i128 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Int(_, _, x) => Ok(*x as i128),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for f32 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Float(_, _, x) => Ok(*x as f32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Float,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for f64 {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Float(_, _, x) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Float,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for String {
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::String(_, _, x) => Ok(x.clone()),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
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
            Expr::Tuple(_id, _span, xs) => {
                if xs.len() != 1 {
                    return Err(Error::ExpectedTypeGotValue {
                        expected: Self::to_type(),
                        got: v.clone(),
                    });
                }
                Ok((T0::try_decode(&xs[0])?,))
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
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
            Expr::Tuple(_id, _span, xs) => {
                if xs.len() != 2 {
                    return Err(Error::ExpectedTypeGotValue {
                        expected: Self::to_type(),
                        got: v.clone(),
                    });
                }
                Ok((T0::try_decode(&xs[0])?, T1::try_decode(&xs[1])?))
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
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
            Expr::Tuple(_id, _span, xs) => {
                if xs.len() != 3 {
                    return Err(Error::ExpectedTypeGotValue {
                        expected: Self::to_type(),
                        got: v.clone(),
                    });
                }
                Ok((
                    T0::try_decode(&xs[0])?,
                    T1::try_decode(&xs[1])?,
                    T2::try_decode(&xs[2])?,
                ))
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
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
            Expr::Tuple(_id, _span, xs) => {
                if xs.len() != 4 {
                    return Err(Error::ExpectedTypeGotValue {
                        expected: Self::to_type(),
                        got: v.clone(),
                    });
                }
                Ok((
                    T0::try_decode(&xs[0])?,
                    T1::try_decode(&xs[1])?,
                    T2::try_decode(&xs[2])?,
                    T3::try_decode(&xs[3])?,
                ))
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
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
            Expr::List(_id, _span, xs) => {
                let mut ys = Vec::with_capacity(xs.len());
                for x in xs {
                    ys.push(T::try_decode(x)?);
                }
                Ok(ys)
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl<T, E> Decode for Result<T, E>
where
    T: Decode,
    E: Decode,
{
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Named(_id, _span, name, x) if name == "Ok" => {
                Ok(Ok(T::try_decode(x)?))
            }
            Expr::Named(_id, _span, name, x) if name == "Err" => {
                Ok(Err(E::try_decode(x)?))
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Result(
                    Box::new(Type::Var(Id::new())),
                    Box::new(Type::Var(Id::new()))),
                got: v.clone(),
            }),
        }
    }
}

impl<T> Decode for Option<T>
where
    T: Decode
{
    fn try_decode(v: &Expr) -> Result<Self, Error> {
        match v {
            Expr::Named(_id, _span, name, x) if name == "Some" => {
                Ok(Some(T::try_decode(x)?))
            }
            Expr::Named(_id, _span, name, _x) if name == "None" => {
                Ok(None)
            }
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Option(Box::new(Type::Var(Id::new()))),
                got: v.clone(),
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
    fn try_encode(self, _id: Id, _span: Span) -> Result<Expr, Error> {
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
        Type::Arrow(Box::new(A::to_type()), Box::new(B::to_type()))
    }
}
