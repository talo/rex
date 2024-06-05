use std::collections::VecDeque;

use ouroboros::Type;

use crate::{
    resolver::{Call, Id, Lambda, Variable, IR},
    span::{Span, Spanned as _},
};

pub mod error;
pub mod trace;
pub mod value;

pub use error::*;
pub use trace::*;
pub use value::*;

#[async_trait::async_trait]
pub trait Runner {
    async fn lookup(&mut self, var: &Variable) -> Result<Option<Value>, Error>;
    async fn run(
        &mut self,
        engine: &mut Engine,
        trace: &mut Trace,
        f: Function,
        args: VecDeque<Value>,
    ) -> Result<Value, Error>;
}

pub struct Engine {
    pub curr_id: Id,
}

impl Engine {
    pub fn new(curr_id: Id) -> Self {
        Self { curr_id }
    }

    pub async fn run<R: Runner + Send>(
        &mut self,
        runner: &mut R,
        ir: IR,
    ) -> (Result<Value, Error>, Trace) {
        let mut trace = Trace::from_span(ir.span().clone());
        let result = self.eval(runner, &mut trace, ir).await;
        (result, trace)
    }

    #[async_recursion::async_recursion]
    pub async fn eval<R: Runner + Send>(
        &mut self,
        runner: &mut R,
        trace: &mut Trace,
        ir: IR,
    ) -> Result<Value, Error> {
        match ir {
            IR::Null(span, ..) => self.eval_null(trace, &span).await,
            IR::Bool(x, span, ..) => self.eval_bool(trace, &span, x).await,
            IR::Uint(x, span, ..) => self.eval_int(trace, &span, x).await,
            IR::Float(x, span, ..) => self.eval_float(trace, &span, x).await,
            IR::String(x, span, ..) => self.eval_string(trace, &span, x).await,
            IR::List(xs, span, ..) => self.eval_list(runner, trace, &span, xs).await,
            IR::Call(call) => self.eval_call(runner, trace, call).await,
            IR::Lambda(lam) => self.eval_lambda(trace, lam).await,
            IR::Variable(var) => self.eval_variable(runner, trace, var).await,
            _ => unimplemented!(),
        }
    }

    async fn eval_null(&mut self, _trace: &mut Trace, _span: &Span) -> Result<Value, Error> {
        Ok(Value::Null)
    }

    async fn eval_bool(
        &mut self,
        _trace: &mut Trace,
        _span: &Span,
        x: bool,
    ) -> Result<Value, Error> {
        Ok(Value::Bool(x))
    }

    async fn eval_int(&mut self, _trace: &mut Trace, _span: &Span, x: u64) -> Result<Value, Error> {
        Ok(Value::U64(x))
    }

    async fn eval_float(
        &mut self,
        _trace: &mut Trace,
        _span: &Span,
        x: f64,
    ) -> Result<Value, Error> {
        Ok(Value::F64(x))
    }

    async fn eval_string(
        &mut self,
        _trace: &mut Trace,
        _span: &Span,
        x: String,
    ) -> Result<Value, Error> {
        Ok(Value::String(x))
    }

    async fn eval_list<R: Runner + Send>(
        &mut self,
        runner: &mut R,
        trace: &mut Trace,
        span: &Span,
        xs: Vec<IR>,
    ) -> Result<Value, Error> {
        let mut ys = Vec::new();
        for x in xs {
            ys.push(self.eval(runner, trace, x).await?);
        }
        trace.step(TraceNode::ListCtor, span.clone());
        Ok(Value::List(ys))
    }

    async fn eval_call<R: Runner + Send>(
        &mut self,
        runner: &mut R,
        trace: &mut Trace,
        mut call: Call,
    ) -> Result<Value, Error> {
        let span = call.span.clone();
        let mut base = self.eval(runner, trace, *call.base).await?;

        while call.args.len() > 0 {
            match base {
                Value::Lambda(mut lam) => {
                    let mut trace_params = Vec::new();
                    let mut trace_args = Vec::new();
                    while lam.params.len() > 0 && call.args.len() > 0 {
                        let var = lam.params.pop_front().unwrap();
                        let arg = call.args.pop_front().unwrap();
                        let arg = self.eval(runner, trace, arg).await?;
                        trace_params.push(var.name.clone());
                        trace_args.push(arg.clone());
                        self.replace_var_in_lambda(&mut lam, var.id, value_to_ir(arg));
                    }
                    let trace =
                        trace.step(TraceNode::Lambda(trace_params, trace_args), span.clone());
                    if lam.params.len() > 0 {
                        return Ok(Value::Lambda(lam));
                    }
                    base = self.eval(runner, trace, *lam.body).await?;
                }

                Value::Function(f) => {
                    if call.args.len() < f.params.len() {
                        let n_vars = f.params.len() - call.args.len();
                        let mut vars = VecDeque::with_capacity(n_vars);
                        for i in 0..n_vars {
                            vars.push_back(Variable {
                                id: self.curr_id.next(),
                                name: format!("x{}", i),
                                span: span.clone(),
                            });
                        }
                        for var in vars.iter() {
                            call.args.push_back(IR::Variable(var.clone()));
                        }
                        return Ok(Value::Lambda(Lambda {
                            id: self.curr_id.next(),
                            params: vars,
                            body: Box::new(IR::Call(Call {
                                id: self.curr_id.next(),
                                base: Box::new(IR::Variable(Variable {
                                    id: f.id,
                                    name: f.name.clone(),
                                    span: span.clone(),
                                })),
                                args: call.args,
                                span: span.clone(),
                            })),
                            span: span.clone(),
                        }));
                    } else {
                        let mut args = Vec::with_capacity(f.params.len());
                        for _ in 0..f.params.len() {
                            args.push(
                                self.eval(runner, trace, call.args.pop_front().unwrap())
                                    .await?,
                            );
                        }
                        let trace = trace.step(
                            TraceNode::Function(f.name.clone(), args.clone()),
                            span.clone(),
                        );
                        base = runner.run(self, trace, f, args.into()).await?;
                    }
                }
                _ => {
                    return Err(Error::Type {
                        expected: "function".to_string(),
                        got: "other".to_string(),
                    })
                }
            }
        }

        Ok(base)
    }

    async fn eval_lambda(&mut self, _trace: &mut Trace, lam: Lambda) -> Result<Value, Error> {
        Ok(Value::Lambda(lam))
    }

    async fn eval_variable<R: Runner + Send>(
        &mut self,
        runner: &mut R,
        _trace: &mut Trace,
        var: Variable,
    ) -> Result<Value, Error> {
        runner
            .lookup(&var)
            .await
            .and_then(|x| x.ok_or(Error::VarNotFound { name: var.name }))
    }

    fn replace_var_in_lambda(&mut self, lam: &mut Lambda, var_id: Id, val: IR) {
        self.replace_var_in_ir(lam.body.as_mut(), var_id, val);
    }

    fn replace_var_in_ir(&mut self, ir: &mut IR, var_id: Id, val: IR) {
        match ir {
            IR::Variable(var) if var.id == var_id => {
                *ir = val;
            }
            IR::Call(call) => {
                self.replace_var_in_ir(call.base.as_mut(), var_id, val.clone());
                for arg in call.args.iter_mut() {
                    self.replace_var_in_ir(arg, var_id, val.clone());
                }
            }
            IR::Lambda(lam) => {
                if lam.params.iter().any(|v| v.id == var_id) {
                    return;
                }
                self.replace_var_in_ir(lam.body.as_mut(), var_id, val);
            }
            IR::List(xs, ..) => {
                for x in xs.iter_mut() {
                    self.replace_var_in_ir(x, var_id, val.clone());
                }
            }
            _ => {}
        }
    }
}

pub fn value_to_ir(val: Value) -> IR {
    match val {
        Value::Null => IR::Null(Span::empty()),
        Value::Bool(x) => IR::Bool(x, Span::empty()),
        Value::U64(x) => IR::Uint(x, Span::empty()),
        Value::I64(x) => IR::Int(x, Span::empty()),
        Value::F64(x) => IR::Float(x, Span::empty()),
        Value::String(x) => IR::String(x, Span::empty()),
        Value::List(xs) => IR::List(xs.into_iter().map(value_to_ir).collect(), Span::empty()),
        Value::Function(f) => IR::Variable(Variable {
            id: f.id,
            name: f.name,
            span: Span::empty(),
        }),
        Value::Lambda(lam) => IR::Lambda(lam),
    }
}

pub struct OpRunner;

#[async_trait::async_trait]
impl Runner for OpRunner {
    async fn lookup(&mut self, var: &Variable) -> Result<Option<Value>, Error> {
        // FIXME: All types are unit types until type inference is implemented
        match var.name.as_str() {
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
            _ => Ok(None),
        }
    }

    async fn run(
        &mut self,
        engine: &mut Engine,
        trace: &mut Trace,
        f: Function,
        mut args: VecDeque<Value>,
    ) -> Result<Value, Error> {
        match f.name.as_str() {
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
                    id: engine.curr_id.next(),
                    name: "x0".to_string(),
                    span: Span::empty(),
                };
                Ok(Value::Lambda(Lambda {
                    id: engine.curr_id.next(),
                    params: vec![x.clone()].into(),
                    body: Box::new(IR::Call(Call {
                        id: engine.curr_id.next(),
                        base: Box::new(value_to_ir(a)),
                        args: vec![IR::Call(Call {
                            id: engine.curr_id.next(),
                            base: Box::new(value_to_ir(b)),
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
                            let id = engine.curr_id.next();
                            apps.push(
                                engine
                                    .eval(
                                        &mut OpRunner,
                                        trace,
                                        IR::Call(Call {
                                            id,
                                            base: Box::new(value_to_ir(f.clone())),
                                            args: vec![value_to_ir(x)].into(),
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
            _ => Err(Error::VarNotFound { name: f.name }),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        engine::{Trace, TraceNode, Value},
        lexer::Token,
        parser::Parser,
        resolver::{Call, Id, Lambda, Resolver, Variable, IR},
        span::Span,
    };

    use super::{Engine, OpRunner};

    #[tokio::test]
    async fn test_ops() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "1 + 2 + 3 + 4"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, _trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(10));

        let mut parser = Parser::new(Token::tokenize("test.rex", "1 * 2 * 3 * 4"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, _trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(24));

        let mut parser = Parser::new(Token::tokenize("test.rex", "(/) 6 3"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, _trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(2));
    }

    #[tokio::test]
    async fn test_lambda() {
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"(\x -> x + 1) 2"#));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(3));
        println!("{}", trace);

        let mut parser = Parser::new(Token::tokenize("test.rex", r#"(\x -> x 1) ((+) 2)"#));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(3));
        println!("{}", trace);

        let mut parser = Parser::new(Token::tokenize("test.rex", r#"(\x -> x 1 2) (+)"#));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(3));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_curry() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "(+) 1"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(
            value,
            Value::Lambda(Lambda {
                id: Id(3),
                params: vec![Variable {
                    id: Id(2),
                    name: "x0".to_string(),
                    span: Span::new("test.rex", 1, 1, 1, 5)
                }]
                .into(),
                body: Box::new(IR::Call(Call {
                    id: Id(4),
                    base: Box::new(IR::Variable(Variable {
                        id: Id(0),
                        name: "+".to_string(),
                        span: Span::new("test.rex", 1, 1, 1, 5),
                    })),
                    args: vec![
                        IR::Uint(1, Span::new("test.rex", 1, 5, 1, 5)),
                        IR::Variable(Variable {
                            id: Id(2),
                            name: "x0".to_string(),
                            span: Span::new("test.rex", 1, 1, 1, 5)
                        })
                    ]
                    .into(),
                    span: Span::new("test.rex", 1, 1, 1, 5),
                })),
                span: Span::new("test.rex", 1, 1, 1, 5),
            })
        );
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_compose() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "((+) 1 . (+) 2 . (*) 3) 4"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(15));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_map() {
        let mut parser = Parser::new(Token::tokenize(
            "test.rex",
            "map ((+) 1 . (+) 2 . (*) 3) [1, 2, 3, 4]",
        ));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(
            value,
            Value::List(vec![
                Value::U64(6),
                Value::U64(9),
                Value::U64(12),
                Value::U64(15)
            ])
        );
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_trace() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "1 * (2 + 3) * 4"));
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut OpRunner, ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(20));
        assert_eq!(
            trace,
            Trace {
                step: 0,
                children: vec![
                    Trace {
                        step: 1,
                        children: vec![],
                        node: TraceNode::Function(
                            "+".to_string(),
                            vec![Value::U64(2), Value::U64(3)],
                        ),
                        span: Span::new("test.rex", 1, 5, 1, 11),
                    },
                    Trace {
                        step: 2,
                        children: vec![],
                        node: TraceNode::Function(
                            "*".to_string(),
                            vec![Value::U64(1), Value::U64(5)]
                        ),
                        span: Span::new("test.rex", 1, 1, 1, 11),
                    },
                    Trace {
                        step: 3,
                        children: vec![],
                        node: TraceNode::Function(
                            "*".to_string(),
                            vec![Value::U64(5), Value::U64(4)]
                        ),
                        span: Span::new("test.rex", 1, 1, 1, 15),
                    },
                ],
                node: TraceNode::Root,
                span: Span::new("test.rex", 1, 1, 1, 15),
            }
        );
    }
}
