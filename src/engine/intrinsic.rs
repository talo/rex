use std::{collections::VecDeque, marker::PhantomData};

use ouroboros::{Type, TypeInfo, A};

use crate::{
    resolver::{Call, Lambda, Variable, IR},
    span::Span,
};

use super::{value_to_ir, Engine, Error, Function, Runner, Trace, TraceNode, Value};

#[derive(Clone)]
pub struct IntrinsicRunner<R, T>(R, PhantomData<T>)
where
    R: Runner<Ctx = T> + Clone,
    T: Clone;

#[derive(Clone)]
pub struct NullRunner;

#[async_trait::async_trait]
impl Runner for NullRunner {
    type Ctx = ();

    async fn lookup(
        &mut self,
        _ctx: &mut Self::Ctx,
        _var: &Variable,
    ) -> Result<Option<Value>, Error> {
        Ok(None)
    }

    async fn run(
        &mut self,
        _engine: &mut Engine,
        _ctx: &mut Self::Ctx,
        _trace: &mut Trace,
        _f: Function,
        _args: VecDeque<Value>,
    ) -> Result<Value, Error> {
        unreachable!()
    }
}

impl Default for IntrinsicRunner<NullRunner, ()> {
    fn default() -> Self {
        Self::new(NullRunner)
    }
}

impl<R, T> IntrinsicRunner<R, T>
where
    R: Runner<Ctx = T> + Clone,
    T: Clone,
{
    pub fn new(runner: R) -> Self {
        Self(runner, PhantomData)
    }
}

#[async_trait::async_trait]
impl<R, T> Runner for IntrinsicRunner<R, T>
where
    R: Runner<Ctx = T> + Send + Clone,
    T: Send + Clone,
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
            "get" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            "json" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::String],
                ret: A::t(),
            }))),
            "map" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            "toJsonStr" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![A::t()],
                ret: Type::String,
            }))),
            "toStr" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![A::t()],
                ret: Type::String,
            }))),
            "zip" => Ok(Some(Value::Function(Function {
                id: var.id,
                name: var.name.clone(),
                params: vec![Type::Unit, Type::Unit],
                ret: Type::Unit,
            }))),
            _ => self.0.lookup(_ctx, var).await,
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
                    (a, b) => Err(Error::Type {
                        expected: "String or List".to_string(),
                        got: format!("{:?} or {:?}", a, b),
                    }),
                    // FIXME: only unreachable once type checking is implemented
                    // _ => unreachable!(),
                }
            }
            "+" => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                match (a, b) {
                    (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a.wrapping_add(b))),
                    (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a.wrapping_add(b))),
                    (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a + b)),
                    (a, b) => Err(Error::Type {
                        expected: "Number".to_string(),
                        got: format!("{:?} or {:?}", a, b),
                    }), // FIXME: only unreachable once type checking is implemented
                        // _ => unreachable!(),
                }
            }
            "-" => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                match (a, b) {
                    (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a.wrapping_sub(b))),
                    (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a.wrapping_sub(b))),
                    (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a - b)),
                    (a, b) => Err(Error::Type {
                        expected: "Number".to_string(),
                        got: format!("{:?} or {:?}", a, b),
                    }), // FIXME: only unreachable once type checking is implemented
                        // _ => unreachable!(),
                }
            }
            "*" => {
                let a = args.pop_front().unwrap();
                let b = args.pop_front().unwrap();
                match (a, b) {
                    (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a.wrapping_mul(b))),
                    (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a.wrapping_mul(b))),
                    (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a * b)),
                    (a, b) => Err(Error::Type {
                        expected: "Number".to_string(),
                        got: format!("{:?} or {:?}", a, b),
                    }), // FIXME: only unreachable once type checking is implemented
                        // _ => unreachable!(),
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
                    (a, b) => Err(Error::Type {
                        expected: "Number".to_string(),
                        got: format!("{:?} or {:?}", a, b),
                    }), // FIXME: only unreachable once type checking is implemented
                        // _ => unreachable!(),
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
                        v => Err(Error::Type {
                            expected: "Number".to_string(),
                            got: v.to_string(),
                        }),
                    },
                    Value::Record(xs) => match n {
                        Value::String(n) => {
                            if let Some(v) = xs.get(&n) {
                                Ok(v.clone().into())
                            } else {
                                Err(Error::FieldNotFound { name: n })
                            }
                        }
                        v => Err(Error::Type {
                            expected: "String".to_string(),
                            got: v.to_string(),
                        }),
                    },
                    v => Err(Error::Type {
                        expected: "List or Record".to_string(),
                        got: v.to_string(),
                    }),
                }
            }
            "json" => {
                let val: String = serde_json::from_value(
                    args.pop_front().unwrap().try_into().unwrap(),
                )
                .map_err(|_| Error::Type {
                    expected: "String".to_string(),
                    got: "unknown".into(),
                })?;
                let val: serde_json::value::Map<String, serde_json::Value> =
                    serde_json::from_str(&val.replace('\\', "")).map_err(|_| Error::Type {
                        expected: "Record".to_string(),
                        got: val,
                    })?;
                Ok(Value::Record(val.into_iter().collect()))
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
                    xs => Err(Error::Type {
                        expected: "List".to_string(),
                        got: format!("{:?}", xs),
                    }), // FIXME: only unreachable once type checking is implemented
                        // _ => unreachable!(),
                }
            }
            "toStr" => {
                let x = args.pop_front().unwrap();
                Ok(Value::String(x.to_string()))
            }
            "toJsonStr" => {
                let x = args.pop_front().unwrap();
                let v: serde_json::Value = x.try_into().expect("value failed to serialize");
                Ok(Value::String(
                    serde_json::to_string(&v).expect("value failed to serialize"),
                ))
            }
            "zip" => {
                let xs = args.pop_front().unwrap();
                let ys = args.pop_front().unwrap();
                match (xs, ys) {
                    (Value::List(xs), Value::List(ys)) => {
                        let mut zs = Vec::with_capacity(xs.len());
                        for (x, y) in xs.into_iter().zip(ys.into_iter()) {
                            zs.push(Value::List(vec![x, y]));
                        }
                        Ok(Value::List(zs))
                    }
                    (xs, ys) => Err(Error::Type {
                        expected: "List".to_string(),
                        got: format!("{:?} or {:?}", xs, ys),
                    }), // FIXME: only unreachable once type checking is implemented
                        // _ => unreachable!(),
                }
            }
            _ => self.0.run(engine, ctx, trace, f, args).await,
        }
    }
}
