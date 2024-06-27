use std::{collections::VecDeque, marker::PhantomData};

use ouroboros::Type;

use crate::{
    resolver::{Call, Lambda, Variable, IR},
    span::Span,
};

use super::{value_to_ir, Engine, Error, Function, Runner, Trace, TraceNode, Value};

pub struct IntrinsicRunner<T>(PhantomData<T>);

impl<T> Default for IntrinsicRunner<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IntrinsicRunner<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

#[async_trait::async_trait]
impl<T> Runner for IntrinsicRunner<T>
where
    T: Send,
{
    type Ctx = T;

    async fn lookup(
        &mut self,
        _ctx: &mut Self::Ctx,
        var: &Variable,
    ) -> Result<Option<Value>, Error> {
        // FIXME: All types are unit types until type inference is implemented
        match var.name.as_str() {
            "++" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::String, Type::String],
                ret: Type::String,
            }))),
            "+" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            "-" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            "*" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            "/" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            "." => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            "map" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            "get" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            _ => Ok(None),
        }
    }

    async fn run(
        &mut self,
        engine: &mut Engine,
        ctx: &mut Self::Ctx,
        trace: &mut Trace,
        f: Function,
        mut args: VecDeque<Value>,
    ) -> Result<Value, Error> {
        match f.name.as_str() {
            "++" => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                match (a, b) {
                    (Value::String(a), Value::String(b)) => {
                        Ok(Value::String(format!("{}{}", a, b)))
                    }
                    (Value::List(mut a), Value::List(b)) => {
                        a.extend(b);
                        Ok(Value::List(a))
                    }
                    _ => unreachable!(),
                }
            }
            "+" => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                match (a, b) {
                    (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a.wrapping_add(b))),
                    (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a.wrapping_add(b))),
                    (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a + b)),
                    _ => unreachable!(),
                }
            }
            "-" => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                match (a, b) {
                    (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a.wrapping_sub(b))),
                    (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a.wrapping_sub(b))),
                    (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a - b)),
                    _ => unreachable!(),
                }
            }
            "*" => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                match (a, b) {
                    (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a.wrapping_mul(b))),
                    (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a.wrapping_mul(b))),
                    (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a * b)),
                    _ => unreachable!(),
                }
            }
            "/" => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                match (a, b) {
                    (Value::U64(a), Value::U64(b)) => {
                        if b == 0 {
                            Err(Error::DivByZero)
                        } else {
                            Ok(Value::U64(a.wrapping_div(b)))
                        }
                    }
                    (Value::I64(a), Value::I64(b)) => {
                        if b == 0 {
                            Err(Error::DivByZero)
                        } else {
                            Ok(Value::I64(a.wrapping_div(b)))
                        }
                    }
                    (Value::F64(a), Value::F64(b)) => {
                        if b == 0.0 {
                            Err(Error::DivByZero)
                        } else {
                            Ok(Value::F64(a / b))
                        }
                    }
                    _ => unreachable!(),
                }
            }
            "." => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                let x = Variable {
                    id: engine.curr_id.inc(),
                    name: "x0".to_string(),
                    span: Span::empty(),
                };
                Ok(Value::Lambda(Lambda {
                    id: engine.curr_id.inc(),
                    params: vec![x.clone()].into(),
                    body: Box::new(IR::Call(Call {
                        id: engine.curr_id.inc(),
                        base: Box::new(value_to_ir(a, Span::empty())),
                        args: vec![IR::Call(Call {
                            id: engine.curr_id.inc(),
                            base: Box::new(value_to_ir(b, Span::empty())),
                            args: vec![IR::Variable(x)].into(),
                            span: Span::empty(),
                        })]
                        .into(),
                        span: Span::empty(),
                    })),
                    span: Span::empty(),
                }))
            }
            "map" => {
                let f = args.pop_front().unwrap();
                let xs = args.pop_front().unwrap();
                match xs {
                    Value::List(xs) => {
                        let mut apps = Vec::with_capacity(xs.len());
                        for x in xs {
                            let id = engine.curr_id.inc();
                            apps.push(
                                engine
                                    .eval(
                                        self,
                                        ctx,
                                        trace,
                                        IR::Call(Call {
                                            id,
                                            base: Box::new(value_to_ir(f.clone(), Span::empty())),
                                            args: vec![value_to_ir(x, Span::empty())].into(),
                                            span: Span::empty(),
                                        }),
                                    )
                                    .await?,
                            );
                        }
                        trace.step(TraceNode::ListCtor, Span::empty());
                        Ok(Value::List(apps))
                    }
                    _ => unreachable!(),
                }
            }
            "get" => {
                let n = args.pop_front().unwrap();
                let xs = args.pop_front().unwrap();
                match xs {
                    Value::List(xs) => match n {
                        Value::U64(n) => {
                            if n as usize >= xs.len() {
                                Err(Error::IndexOutOfBounds { index: n })
                            } else {
                                Ok(xs[n as usize].clone())
                            }
                        }
                        Value::I64(n) => {
                            if n as usize >= xs.len() {
                                Err(Error::IndexOutOfBounds { index: n as u64 })
                            } else {
                                Ok(xs[n as usize].clone())
                            }
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            _ => Err(Error::VarNotFound { name: f.name }),
        }
    }
}
