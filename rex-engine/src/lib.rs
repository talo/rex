use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display, Formatter},
};

use ftable::Ftable;
use futures::future;

use error::Error;
use rex_ast::{
    ast::{Call, Ctor, Fields, IfThenElse, Lambda, LetIn, NamedFields, UnnamedFields, Var, AST},
    id::Id,
    types::Type,
};
use value::{Closure, Data, DataFields, FunctionLike, NamedDataFields, UnnamedDataFields, Value};

mod error;
mod ftable;
mod value;

#[derive(Clone, Debug, PartialEq)]
pub struct Context {
    pub vars: HashMap<Id, Value>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn extend(&mut self, rhs: Context) {
        self.vars.extend(rhs.vars)
    }
}

impl Display for Context {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, (k, v)) in self.vars.iter().enumerate() {
            k.fmt(f)?;
            "←".fmt(f)?;
            v.fmt(f)?;
            if i + 1 < self.vars.len() {
                "; ".fmt(f)?;
            }
        }
        Ok(())
    }
}

pub async fn apply(
    ctx: &Context,
    ftable: &Ftable,
    base: Value,
    arg: Value,
) -> Result<Value, Error> {
    match base {
        Value::Function(function) => {
            if function.params.len() == 1 {
                ftable.dispatch(ctx, ftable, &function, &vec![arg]).await
            } else {
                Ok(Value::Closure(Closure {
                    captured_ctx: Context::new(), // Fresh context, because functions don't close over anything
                    captured_args: vec![arg.clone()],
                    body: FunctionLike::Function(function.clone()),
                }))
            }
        }
        Value::Closure(closure) => match &closure.body {
            FunctionLike::Function(function) => {
                let mut closure = closure.clone();
                closure.captured_args.push(arg.clone());
                if function.params.len() == closure.captured_args.len() {
                    ftable
                        .dispatch(ctx, ftable, function, &closure.captured_args)
                        .await
                } else {
                    Ok(Value::Closure(closure))
                }
            }
            FunctionLike::Lambda(lam) => {
                let mut new_ctx = ctx.clone();
                new_ctx.vars.insert(lam.var.id, arg);
                new_ctx.extend(closure.captured_ctx);
                eval(&new_ctx, ftable, *lam.body.clone()).await
            }
        },
        got => Err(Error::ExpectedCallable { got }),
    }
}

#[async_recursion::async_recursion]
pub async fn eval(ctx: &Context, ftable: &Ftable, ast: AST) -> Result<Value, Error> {
    match ast {
        AST::Null(_span) => Ok(Value::Null),
        AST::Bool(_span, x) => Ok(Value::Bool(x)),
        AST::Uint(_span, x) => Ok(Value::Uint(x)),
        AST::Int(_span, x) => Ok(Value::Int(x)),
        AST::Float(_span, x) => Ok(Value::Float(x)),
        AST::String(_span, x) => Ok(Value::String(x)),

        AST::List(_span, list) => eval_list(ctx, ftable, list).await,
        AST::Tuple(_span, tuple) => eval_tuple(ctx, ftable, tuple).await,
        AST::Dict(_span, dict) => eval_dict(ctx, ftable, dict).await,

        AST::Var(var) => eval_var(ctx, ftable, var).await,
        AST::Call(call) => eval_call(ctx, ftable, call).await,
        AST::Lambda(lam) => eval_lambda(ctx, ftable, lam).await,
        AST::LetIn(let_in) => eval_let_in(ctx, ftable, let_in).await,
        AST::IfThenElse(ite) => eval_ite(ctx, ftable, ite).await,
        AST::Ctor(ctor) => eval_ctor(ctx, ftable, ctor).await,
    }
}

async fn eval_list(ctx: &Context, ftable: &Ftable, list: Vec<AST>) -> Result<Value, Error> {
    let mut result = Vec::with_capacity(list.len());
    for v in list {
        result.push(eval(ctx, ftable, v));
    }
    Ok(Value::List(
        future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<_, _>>()?,
    ))
}

async fn eval_tuple(ctx: &Context, ftable: &Ftable, tuple: Vec<AST>) -> Result<Value, Error> {
    let mut result = Vec::with_capacity(tuple.len());
    for v in tuple {
        result.push(eval(ctx, ftable, v));
    }
    Ok(Value::Tuple(
        future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<_, _>>()?,
    ))
}

async fn eval_dict(ctx: &Context, ftable: &Ftable, dict: Vec<(Var, AST)>) -> Result<Value, Error> {
    let mut result = BTreeMap::new();
    let mut keys = Vec::with_capacity(dict.len());
    let mut vals = Vec::with_capacity(dict.len());
    for (k, v) in dict {
        keys.push(k.name);
        vals.push(eval(ctx, ftable, v));
    }
    for (k, v) in keys.into_iter().zip(future::join_all(vals).await) {
        result.insert(k, v?);
    }
    Ok(Value::Dict(result))
}

async fn eval_var(ctx: &Context, ftable: &Ftable, var: Var) -> Result<Value, Error> {
    match ctx.vars.get(&var.id) {
        Some(value) => Ok(value.clone()),
        _ => ftable.lookup(ctx, &var).await,
    }
}

async fn eval_call(ctx: &Context, ftable: &Ftable, call: Call) -> Result<Value, Error> {
    let (base, arg) =
        future::join(eval(ctx, ftable, *call.base), eval(ctx, ftable, *call.arg)).await;
    apply(ctx, ftable, base?, arg?).await
}

async fn eval_lambda(ctx: &Context, _ftable: &Ftable, lam: Lambda) -> Result<Value, Error> {
    Ok(Value::Closure(Closure {
        captured_ctx: ctx.clone(),
        captured_args: vec![],
        body: FunctionLike::Lambda(lam),
    }))
}

async fn eval_let_in(ctx: &Context, ftable: &Ftable, let_in: LetIn) -> Result<Value, Error> {
    let mut new_ctx = ctx.clone();
    new_ctx
        .vars
        .insert(let_in.var.id, eval(ctx, ftable, *let_in.def).await?);
    eval(&new_ctx, ftable, *let_in.body).await
}

async fn eval_ite(ctx: &Context, ftable: &Ftable, ite: IfThenElse) -> Result<Value, Error> {
    let cond = eval(ctx, ftable, *ite.cond).await?;
    match cond {
        Value::Bool(true) => eval(ctx, ftable, *ite.then).await,
        Value::Bool(false) => eval(ctx, ftable, *ite.r#else).await,
        got => Err(Error::UnexpectedType {
            expected: Type::Bool,
            got,
        }),
    }
}

async fn eval_ctor(ctx: &Context, ftable: &Ftable, ctor: Ctor) -> Result<Value, Error> {
    Ok(Value::Data(Data {
        name: ctor.name,
        fields: Some(eval_fields(ctx, ftable, ctor.fields).await?),
    }))
}

async fn eval_fields(ctx: &Context, ftable: &Ftable, fields: Fields) -> Result<DataFields, Error> {
    match fields {
        Fields::Named(named_fields) => Ok(DataFields::Named(
            eval_named_fields(ctx, ftable, named_fields).await?,
        )),
        Fields::Unnamed(unnamed_fields) => Ok(DataFields::Unnamed(
            eval_unnamed_fields(ctx, ftable, unnamed_fields).await?,
        )),
    }
}

async fn eval_named_fields(
    ctx: &Context,
    ftable: &Ftable,
    named_fields: NamedFields,
) -> Result<NamedDataFields, Error> {
    let mut result = BTreeMap::new();
    let mut keys = Vec::with_capacity(named_fields.fields.len());
    let mut vals = Vec::with_capacity(named_fields.fields.len());
    for (k, v) in named_fields.fields {
        keys.push(k.name);
        vals.push(eval(ctx, ftable, v));
    }
    for (k, v) in keys.into_iter().zip(future::join_all(vals).await) {
        result.insert(k, v?);
    }
    Ok(NamedDataFields { fields: result })
}

async fn eval_unnamed_fields(
    ctx: &Context,
    ftable: &Ftable,
    unnamed_fields: UnnamedFields,
) -> Result<UnnamedDataFields, Error> {
    let mut result = Vec::with_capacity(unnamed_fields.fields.len());
    for v in unnamed_fields.fields {
        result.push(eval(ctx, ftable, v));
    }
    Ok(UnnamedDataFields {
        fields: future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<_, _>>()?,
    })
}

#[cfg(test)]
mod test {

    use rex_ast::{adt, adt_variant_with_named_fields, id::IdDispenser, types::Type};
    use rex_parser::{lexer::Token, Parser};
    use rex_resolver::resolve;

    use crate::{
        eval,
        value::{Data, DataFields, NamedDataFields, Value},
        Context, Ftable,
    };

    fn ftable_with_point_adt(id_dispenser: &mut IdDispenser) -> Ftable {
        let mut ftable = Ftable::with_intrinsics(id_dispenser);
        ftable.register_adt(
            id_dispenser,
            adt!(
                Point = Point2D { x = Type::Uint, y = Type::Uint } | Point3D { x = Type::Uint, y = Type::Uint, z = Type::Uint }
            ),
        );
        ftable
    }

    #[tokio::test]
    async fn math() {
        let mut parser = Parser::new(Token::tokenize("1 + 2").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();
        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(val, Value::Uint(3))
    }

    #[tokio::test]
    async fn math_with_precedence() {
        let mut parser = Parser::new(Token::tokenize("1 + 2 * 3").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();
        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(val, Value::Uint(7));
    }

    #[tokio::test]
    async fn lambda_1() {
        let mut parser = Parser::new(Token::tokenize("(\\x -> x) 1").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(val, Value::Uint(1));
    }

    #[tokio::test]
    async fn lambda_2() {
        let mut parser = Parser::new(Token::tokenize("(\\x y -> x + y) 1 2").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(val, Value::Uint(3));
    }

    #[tokio::test]
    async fn lambda_3() {
        let mut parser = Parser::new(Token::tokenize("(\\x y z -> x + y * z) 1 2 3").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(val, Value::Uint(7));
    }

    #[tokio::test]
    async fn let_in() {
        let mut parser = Parser::new(Token::tokenize("let x = 1 + 2, y = 3 in x * y").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(val, Value::Uint(9));
    }

    #[tokio::test]
    async fn if_then_else_max() {
        let mut parser =
            Parser::new(Token::tokenize("(\\x y -> if x > y then x else y) 4 20").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(val, Value::Uint(20));
    }

    #[tokio::test]
    async fn test_map() {
        let mut parser =
            Parser::new(Token::tokenize("map (\\x -> x * 3 + 2 + 1) [1, 2, 3, 4]").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::Uint(6),
                Value::Uint(9),
                Value::Uint(12),
                Value::Uint(15)
            ])
        );
    }

    #[tokio::test]
    async fn test_zip() {
        let mut parser = Parser::new(Token::tokenize("zip [1, 2, 3, 4] [4, 3, 2, 1]").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::Tuple(vec![Value::Uint(1), Value::Uint(4)]),
                Value::Tuple(vec![Value::Uint(2), Value::Uint(3)]),
                Value::Tuple(vec![Value::Uint(3), Value::Uint(2)]),
                Value::Tuple(vec![Value::Uint(4), Value::Uint(1)]),
            ])
        );
    }

    #[tokio::test]
    async fn test_filter() {
        let mut parser = Parser::new(Token::tokenize("filter ((<) 2) [4, 3, 2, 1]").unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(val, Value::List(vec![Value::Uint(4), Value::Uint(3)]));
    }

    #[tokio::test]
    async fn test_ctor() {
        let tokens = Token::tokenize("filter (λp → x p + y p == 1) [Point2D { x = 0, y = 0 }, Point2D { x = 0, y = 1 }, Point3D { x = 1, y = 0, z = 2 }, Point3D { x = 1, y = 1, z = 2 }]").unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = ftable_with_point_adt(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, ast).await.unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::Data(Data {
                    name: "Point2D".to_string(),
                    fields: Some(DataFields::Named(NamedDataFields {
                        fields: vec![
                            ("x".to_string(), Value::Uint(0)),
                            ("y".to_string(), Value::Uint(1))
                        ]
                        .into_iter()
                        .collect()
                    }))
                }),
                Value::Data(Data {
                    name: "Point3D".to_string(),
                    fields: Some(DataFields::Named(NamedDataFields {
                        fields: vec![
                            ("x".to_string(), Value::Uint(1)),
                            ("y".to_string(), Value::Uint(0)),
                            ("z".to_string(), Value::Uint(2))
                        ]
                        .into_iter()
                        .collect()
                    }))
                }),
            ])
        );
    }
}
