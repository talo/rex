use std::collections::{BTreeMap, VecDeque};

use crate::{
    resolver::{Call, Id, Lambda, Variable, IR},
    span::{Span, Spanned as _},
};

pub mod error;
pub mod intrinsic;
pub mod trace;
pub mod value;

pub use error::*;
pub use intrinsic::*;
pub use trace::*;
pub use value::*;

#[async_trait::async_trait]
pub trait Runner {
    type Ctx;

    async fn lookup(&mut self, ctx: &mut Self::Ctx, var: &Variable)
        -> Result<Option<Value>, Error>;
    async fn run(
        &mut self,
        engine: &mut Engine,
        ctx: &mut Self::Ctx,
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
        mut ctx: R::Ctx,
        ir: IR,
    ) -> (Result<Value, Error>, Trace)
    where
        R::Ctx: Send,
    {
        let mut trace = Trace::from_span(ir.span().clone());
        let result = self.eval(runner, &mut ctx, &mut trace, ir).await;
        (result, trace)
    }

    #[async_recursion::async_recursion]
    pub async fn eval<R: Runner + Send>(
        &mut self,
        runner: &mut R,
        ctx: &mut R::Ctx,
        trace: &mut Trace,
        ir: IR,
    ) -> Result<Value, Error>
    where
        R::Ctx: Send,
    {
        match ir {
            IR::Null(span, ..) => self.eval_null(trace, &span).await,
            IR::Bool(x, span, ..) => self.eval_bool(trace, &span, x).await,
            IR::Int(x, span, ..) => self.eval_int(trace, &span, x).await,
            IR::Uint(x, span, ..) => self.eval_uint(trace, &span, x).await,
            IR::Float(x, span, ..) => self.eval_float(trace, &span, x).await,
            IR::String(x, span, ..) => self.eval_string(trace, &span, x).await,
            IR::List(xs, span, ..) => self.eval_list(runner, ctx, trace, &span, xs).await,
            IR::Record(xs, span, ..) => self.eval_record(runner, ctx, trace, &span, xs).await,
            IR::Call(call) => self.eval_call(runner, ctx, trace, call).await,
            IR::Lambda(lam) => self.eval_lambda(trace, lam).await,
            IR::Variable(var) => self.eval_variable(runner, ctx, trace, var).await,
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

    async fn eval_int(&mut self, _trace: &mut Trace, _span: &Span, x: i64) -> Result<Value, Error> {
        Ok(Value::I64(x))
    }

    async fn eval_uint(
        &mut self,
        _trace: &mut Trace,
        _span: &Span,
        x: u64,
    ) -> Result<Value, Error> {
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
        ctx: &mut R::Ctx,
        trace: &mut Trace,
        span: &Span,
        xs: Vec<IR>,
    ) -> Result<Value, Error>
    where
        R::Ctx: Send,
    {
        let mut ys = Vec::new();
        for x in xs {
            ys.push(self.eval(runner, ctx, trace, x).await?);
        }
        trace.step(TraceNode::ListCtor, span.clone());
        Ok(Value::List(ys))
    }

    async fn eval_record<R: Runner + Send>(
        &mut self,
        _runner: &mut R,
        _ctx: &mut R::Ctx,
        _trace: &mut Trace,
        _span: &Span,
        xs: BTreeMap<String, serde_json::Value>,
    ) -> Result<Value, Error>
    where
        R::Ctx: Send,
    {
        Ok(Value::Record(xs))
    }

    async fn eval_call<R: Runner + Send>(
        &mut self,
        runner: &mut R,
        ctx: &mut R::Ctx,
        trace: &mut Trace,
        mut call: Call,
    ) -> Result<Value, Error>
    where
        R::Ctx: Send,
    {
        let span = call.span.clone();
        let mut base = self.eval(runner, ctx, trace, *call.base).await?;

        while !call.args.is_empty() {
            match base {
                Value::Lambda(mut lam) => {
                    let mut trace_params = Vec::new();
                    let mut trace_args = Vec::new();
                    while !lam.params.is_empty() & !call.args.is_empty() {
                        let var = lam.params.pop_front().unwrap();
                        let arg = call.args.pop_front().unwrap();
                        let arg = self.eval(runner, ctx, trace, arg).await?;
                        trace_params.push(var.name.clone());
                        trace_args.push(arg.clone());
                        self.replace_var_in_lambda(
                            &mut lam,
                            var.id,
                            value_to_ir(arg, span.clone()),
                        );
                    }
                    let trace =
                        trace.step(TraceNode::Lambda(trace_params, trace_args), span.clone());
                    if !lam.params.is_empty() {
                        return Ok(Value::Lambda(lam));
                    }
                    base = self.eval(runner, ctx, trace, *lam.body).await?;
                }

                Value::Function(f) => {
                    if call.args.len() < f.params.len() {
                        let n_vars = f.params.len() - call.args.len();
                        let mut vars = VecDeque::with_capacity(n_vars);
                        for i in 0..n_vars {
                            vars.push_back(Variable {
                                id: self.curr_id.inc(),
                                name: format!("x{}", i),
                                span: span.clone(),
                            });
                        }
                        for var in vars.iter() {
                            call.args.push_back(IR::Variable(var.clone()));
                        }
                        return Ok(Value::Lambda(Lambda {
                            id: self.curr_id.inc(),
                            params: vars,
                            body: Box::new(IR::Call(Call {
                                id: self.curr_id.inc(),
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
                                self.eval(runner, ctx, trace, call.args.pop_front().unwrap())
                                    .await?,
                            );
                        }
                        let trace = trace.step(
                            TraceNode::Function(f.name.clone(), args.clone()),
                            span.clone(),
                        );
                        base = runner.run(self, ctx, trace, f, args.into()).await?;
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
        ctx: &mut R::Ctx,
        _trace: &mut Trace,
        var: Variable,
    ) -> Result<Value, Error> {
        runner
            .lookup(ctx, &var)
            .await
            .and_then(|x| x.ok_or(Error::VarNotFound { name: var.name }))
    }

    fn replace_var_in_lambda(&mut self, lam: &mut Lambda, var_id: Id, val: IR) {
        Engine::replace_var_in_ir(lam.body.as_mut(), var_id, val);
    }

    fn replace_var_in_ir(ir: &mut IR, var_id: Id, val: IR) {
        match ir {
            IR::Variable(var) if var.id == var_id => {
                *ir = val;
            }
            IR::Call(call) => {
                Engine::replace_var_in_ir(call.base.as_mut(), var_id, val.clone());
                for arg in call.args.iter_mut() {
                    Engine::replace_var_in_ir(arg, var_id, val.clone());
                }
            }
            IR::Lambda(lam) => {
                if lam.params.iter().any(|v| v.id == var_id) {
                    return;
                }
                Engine::replace_var_in_ir(lam.body.as_mut(), var_id, val);
            }
            IR::List(xs, ..) => {
                for x in xs.iter_mut() {
                    Engine::replace_var_in_ir(x, var_id, val.clone());
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use crate::{
        engine::{Trace, TraceNode, Value},
        lexer::Token,
        parser::Parser,
        resolver::{Call, Id, Lambda, Resolver, Variable, IR},
        span::Span,
    };

    use super::{Engine, IntrinsicRunner};

    #[tokio::test]
    async fn test_ops() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "1 + 2 + 3 + 4").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, _trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(10));

        let mut parser = Parser::new(Token::tokenize("test.rex", "1 * 2 * 3 * 4").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, _trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(24));

        let mut parser = Parser::new(Token::tokenize("test.rex", "(/) 6 3").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, _trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(2));

        let mut parser =
            Parser::new(Token::tokenize("test.rex", r#"(++) "hello, " "world!""#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, _trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::String("hello, world!".to_string()));

        let mut parser =
            Parser::new(Token::tokenize("test.rex", r#""hello" ++ ", " ++ 'world!'"#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, _trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::String("hello, world!".to_string()));
    }

    #[tokio::test]
    async fn test_lambda() {
        let mut parser = Parser::new(Token::tokenize("test.rex", r#"(\x -> x + 1) 2"#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(3));
        println!("{}", trace);

        let mut parser =
            Parser::new(Token::tokenize("test.rex", r#"(\x -> x 1) ((+) 2)"#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(3));
        println!("{}", trace);

        let mut parser = Parser::new(Token::tokenize("test.rex", r#"(\x -> x 1 2) (+)"#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(3));
        println!("{}", trace);

        let mut parser =
            Parser::new(Token::tokenize("test.rex", r#"(\x y -> x y 2) (+) 1"#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(3));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_curry() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "(+) 1").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
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
        let mut parser =
            Parser::new(Token::tokenize("test.rex", "((+) 1 . (+) 2 . (*) 3) 4").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(15));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_json() {
        let mut parser =
            Parser::new(Token::tokenize("test.rex", r#"json '{ "hello": "world" }' "#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::Record(BTreeMap::from([("hello".to_string(), serde_json::to_value("world").unwrap())])));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_to_str() {
        let mut parser =
            Parser::new(Token::tokenize("test.rex", r#"json ( '{ "hello": '++ (toStr ( 1 + 2 )) ++ ', "world":' ++ ( toStr ( json '{"foo": "bar"}')) ++ ' }' ) "#).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::Record(BTreeMap::from([
            ("hello".to_string(), serde_json::json!( {"u64": 3} )),
            ("world".to_string(), serde_json::json!({ "record": { "foo": "bar" } }))
        ])));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_let() {
        let mut parser =
            Parser::new(Token::tokenize("test.rex", "let x = 1, y = 2 in x + y").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(3));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_map() {
        let mut parser = Parser::new(
            Token::tokenize("test.rex", "map ((+) 1 . (+) 2 . (*) 3) [1, 2, 3, 4]").unwrap(),
        );
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
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
    async fn test_zip() {
        let mut parser =
            Parser::new(Token::tokenize("test.rex", "zip [1, 2, 3, 4] [4, 3, 2, 1]").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(
            value,
            Value::List(vec![
                Value::List(vec![Value::U64(1), Value::U64(4)]),
                Value::List(vec![Value::U64(2), Value::U64(3)]),
                Value::List(vec![Value::U64(3), Value::U64(2)]),
                Value::List(vec![Value::U64(4), Value::U64(1)]),
            ])
        );
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_nested_map() {
        let mut parser = Parser::new(
            Token::tokenize(
                "test.rex",
                "map (\\x -> ( (+) 1 . (+) 2 . (*) 3 ) ( get 1 x ) ) [ [1, 2, 3, 4] ]",
            )
            .unwrap(),
        );
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::List(vec![Value::U64(9),]));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_nested_triple_nested_map() {
        let mut parser = Parser::new(
            Token::tokenize(
                "test.rex",
                "map (\\x -> map (\\y -> map (\\z -> z + 1) y) x) [[[1, 2], [3, 4]]]",
            )
            .unwrap(),
        );
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(
            value,
            Value::List(vec![Value::List(vec![
                Value::List(vec![Value::U64(2), Value::U64(3)]),
                Value::List(vec![Value::U64(4), Value::U64(5)]),
            ])])
        );
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_let_in_map() {
        let mut parser = Parser::new(Token::tokenize(
            "test.rex",
            "let ext = (\\l -> get 0 l) in map (\\x -> let y = (ext x) in y + 1) [[1], [2], [3], [4]] ]",
        ).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(
            value,
            Value::List(vec![
                Value::U64(2),
                Value::U64(3),
                Value::U64(4),
                Value::U64(5),
            ])
        );
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_let_in_map_word_boundaries() {
        let mut parser = Parser::new(Token::tokenize(
            "test.rex",
            "let inext = (\\atrue -> get 0 atrue) in map (\\falsey -> let yin = (inext falsey), amapa = 1 in yin + amapa) [[1], [2], [3], [4]] ]",
        ).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(
            value,
            Value::List(vec![
                Value::U64(2),
                Value::U64(3),
                Value::U64(4),
                Value::U64(5),
            ])
        );
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_let_in_nested_map() {
        let mut parser = Parser::new(Token::tokenize(
            "test.rex",
            "let ext = (\\l -> get 0 l) in map (\\x -> map (\\y -> let z = (ext y) in z + 1) x) [[[1], [2]], [[3], [4]]]",
        ).unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(
            value,
            Value::List(vec![
                Value::List(vec![Value::U64(2), Value::U64(3)]),
                Value::List(vec![Value::U64(4), Value::U64(5)]),
            ])
        );
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_get() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "get 1 [1, 2, 3, 4]").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::U64(2));
        println!("{}", trace);

        // TODO: implement once we have record representation
        // let mut parser = Parser::new(Token::tokenize(
        //     "test.rex",
        //     r#"get "a" {"a": 1}"#,
        // ));
        // let mut resolver = Resolver::new();
        // let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        // let mut engine = Engine::new(resolver.curr_id);
        // let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        // let value = result.unwrap();
        // assert_eq!(
        //     value,
        //     Value::U64(2)
        // );
        // println!("{}", trace);
    }

    #[tokio::test]
    async fn test_map_composed_lambda() {
        let mut parser = Parser::new(
            Token::tokenize(
                "test.rex",
                "map ((+) 1 . \\x -> x + 2) (get 1 ([[1, 2]] ++ [[3, 4]]))",
            )
            .unwrap(),
        );
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
        let value = result.unwrap();
        assert_eq!(value, Value::List(vec![Value::U64(6), Value::U64(7)]));
        println!("{}", trace);
    }

    #[tokio::test]
    async fn test_trace() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "1 * (2 + 3) * 4").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut engine = Engine::new(resolver.curr_id);
        let (result, trace) = engine.run(&mut IntrinsicRunner::default(), (), ir).await;
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
                        timestamp: None,
                    },
                    Trace {
                        step: 2,
                        children: vec![],
                        node: TraceNode::Function(
                            "*".to_string(),
                            vec![Value::U64(1), Value::U64(5)]
                        ),
                        span: Span::new("test.rex", 1, 1, 1, 11),
                        timestamp: None,
                    },
                    Trace {
                        step: 3,
                        children: vec![],
                        node: TraceNode::Function(
                            "*".to_string(),
                            vec![Value::U64(5), Value::U64(4)]
                        ),
                        span: Span::new("test.rex", 1, 1, 1, 15),
                        timestamp: None,
                    },
                ],
                node: TraceNode::Root,
                timestamp: None,
                span: Span::new("test.rex", 1, 1, 1, 15),
            }
        );
    }
}
