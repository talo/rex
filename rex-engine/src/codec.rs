use rex_ast::{expr::Expr, id::Id};
use rex_lexer::span::Span;
use rex_type_system::types::Type;

use crate::{error::Error, eval::Value};

pub trait Encode
where
    Self: Sized,
{
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error>;
}

impl Encode for bool {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Bool(id, span, self)))
    }
}

impl Encode for u8 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Uint(id, span, self as u64)))
    }
}

impl Encode for u16 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Uint(id, span, self as u64)))
    }
}

impl Encode for u32 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Uint(id, span, self as u64)))
    }
}

impl Encode for u64 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Uint(id, span, self)))
    }
}

impl Encode for i8 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Int(id, span, self as i64)))
    }
}

impl Encode for i16 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Int(id, span, self as i64)))
    }
}

impl Encode for i32 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Int(id, span, self as i64)))
    }
}

impl Encode for i64 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Int(id, span, self)))
    }
}

impl Encode for f32 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Float(id, span, self as f64)))
    }
}

impl Encode for f64 {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::Float(id, span, self)))
    }
}

impl Encode for &str {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::String(id, span, self.to_string())))
    }
}

impl Encode for String {
    fn try_encode(self, id: Id, span: Span) -> Result<Value, Error> {
        Ok(Value::Expr(Expr::String(id, span, self)))
    }
}

pub trait Decode
where
    Self: Sized,
{
    fn try_decode(v: &Value) -> Result<Self, Error>;
}

impl Decode for bool {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Bool(_, _, x)) => Ok(*x as bool),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for u8 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Uint(_, _, x)) => Ok(*x as u8),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for u16 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Uint(_, _, x)) => Ok(*x as u16),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for u32 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Uint(_, _, x)) => Ok(*x as u32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}
impl Decode for u64 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Uint(_, _, x)) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for u128 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Uint(_, _, x)) => Ok(*x as u128),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Uint,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i8 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Int(_, _, x)) => Ok(*x as i8),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i16 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Int(_, _, x)) => Ok(*x as i16),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i32 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Int(_, _, x)) => Ok(*x as i32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i64 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Int(_, _, x)) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for i128 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Int(_, _, x)) => Ok(*x as i128),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for f32 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Float(_, _, x)) => Ok(*x as f32),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Float,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for f64 {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::Float(_, _, x)) => Ok(*x),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Float,
                got: v.clone(),
            }),
        }
    }
}

impl Decode for String {
    fn try_decode(v: &Value) -> Result<Self, Error> {
        match v {
            Value::Expr(Expr::String(_, _, x)) => Ok(x.clone()),
            _ => Err(Error::ExpectedTypeGotValue {
                expected: Type::Int,
                got: v.clone(),
            }),
        }
    }
}
