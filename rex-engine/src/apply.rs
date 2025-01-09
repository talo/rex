use std::collections::BTreeMap;

use futures::future;

use crate::{
    error::Error,
    eval,
    ftable::Ftable,
    value::{Closure, Data, DataFields, FunctionLike, NamedDataFields, UnnamedDataFields, Value},
    Context,
};

pub async fn apply<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    base: Value,
    arg: Value,
) -> Result<Value, Error> {
    match base {
        Value::Function(function) => {
            if function.params.len() == 1 {
                ftable
                    .dispatch(ctx, ftable, state, &function, &vec![arg])
                    .await
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
                        .dispatch(ctx, ftable, state, function, &closure.captured_args)
                        .await
                } else {
                    Ok(Value::Closure(closure))
                }
            }
            FunctionLike::Lambda(lam_id, _param, body) => {
                let mut new_ctx = ctx.clone();
                new_ctx.vars.insert(*lam_id, arg);
                new_ctx.extend(closure.captured_ctx);
                eval(&new_ctx, ftable, state, body.clone()).await
            }
        },
        got => Err(Error::ExpectedCallable {
            got,
            trace: Default::default(),
        }),
    }
}

#[async_recursion::async_recursion]
pub async fn apply0<S>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    base: Value,
) -> Result<Value, Error>
where
    S: Send + Sync + 'static,
{
    match base {
        // Basics
        Value::Null => Ok(Value::Null),
        Value::Bool(x) => Ok(Value::Bool(x)),
        Value::Uint(x) => Ok(Value::Uint(x)),
        Value::Int(x) => Ok(Value::Int(x)),
        Value::Float(x) => Ok(Value::Float(x)),
        Value::String(x) => Ok(Value::String(x)),
        Value::List(xs) => {
            let mut futs = Vec::new();
            for x in xs {
                futs.push(apply0(ctx, ftable, state, x));
            }
            Ok(Value::List(
                future::join_all(futs)
                    .await
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        Value::Tuple(xs) => {
            let mut futs = Vec::new();
            for x in xs {
                futs.push(apply0(ctx, ftable, state, x));
            }
            Ok(Value::Tuple(
                future::join_all(futs)
                    .await
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        Value::Dict(kvs) => {
            let mut result = BTreeMap::new();
            let mut keys = Vec::with_capacity(kvs.len());
            let mut vals = Vec::with_capacity(kvs.len());
            for (k, v) in kvs {
                keys.push(k);
                vals.push(apply0(ctx, ftable, state, v));
            }
            for (k, v) in keys.into_iter().zip(future::join_all(vals).await) {
                result.insert(k, v?);
            }
            Ok(Value::Dict(result))
        }
        Value::Option(x) => match x {
            Some(x) => Ok(Value::Option(Some(Box::new(
                apply0(ctx, ftable, state, *x).await?,
            )))),
            None => Ok(Value::Option(None)),
        },
        Value::Result(x) => match x {
            Ok(x) => Ok(Value::Result(Ok(Box::new(
                apply0(ctx, ftable, state, *x).await?,
            )))),
            Err(x) => Ok(Value::Result(Err(Box::new(
                apply0(ctx, ftable, state, *x).await?,
            )))),
        },
        // Id
        Value::Id(id) => {
            match ctx.vars.get(&id) {
                Some(Value::Id(id)) => Ok(Value::Id(*id)), // We do not want to recurse infinitely
                _ => match ftable.lookup(ctx, &id).await {
                    Ok(v) => Ok(apply0(ctx, ftable, state, v).await?),
                    Err(e) => Err(e),
                },
            }
        }
        // Functions
        Value::Function(function) => {
            if function.params.is_empty() {
                ftable
                    .dispatch(ctx, ftable, state, &function, &vec![])
                    .await
            } else {
                Ok(Value::Function(function))
            }
        }
        Value::Closure(closure) => match &closure.body {
            FunctionLike::Function(function)
                if function.params.len() == closure.captured_args.len() =>
            {
                ftable
                    .dispatch(ctx, ftable, state, function, &closure.captured_args)
                    .await
            }
            _ => Ok(Value::Closure(closure)),
        },
        // Data
        Value::Data(data) => {
            let mut result = Data {
                name: data.name,
                fields: None,
            };
            match data.fields {
                Some(DataFields::Named(named_fields)) => {
                    let mut new_fields = BTreeMap::new();
                    for (k, v) in named_fields.fields {
                        new_fields.insert(k, apply0(ctx, ftable, state, v).await?);
                    }
                    result.fields = Some(DataFields::Named(NamedDataFields { fields: new_fields }));
                }
                Some(DataFields::Unnamed(unnamed_fields)) => {
                    let mut new_fields = Vec::new();
                    for v in unnamed_fields.fields {
                        new_fields.push(apply0(ctx, ftable, state, v).await?);
                    }
                    result.fields = Some(DataFields::Unnamed(UnnamedDataFields {
                        fields: new_fields,
                    }));
                }
                None => {}
            }
            Ok(Value::Data(result))
        }
    }
}
