use std::{collections::HashMap, future::Future, pin::Pin};

use rex_ast::{
    a, arrow,
    ast::Var,
    b,
    id::{Id, IdDispenser},
    list, tuple,
    types::{ADTVariantFields, Type, ADT},
};
use rex_resolver::Scope;

use crate::{
    apply,
    error::Error,
    value::{Data, DataFields, Function, NamedDataFields, UnnamedDataFields, Value},
    Context,
};

pub struct Ftable {
    pub ftable: HashMap<
        Id,
        (
            Function,
            Box<
                dyn for<'r> Fn(
                        &'r Context,
                        &'r Ftable,
                        &'r Vec<Value>,
                    )
                        -> Pin<Box<dyn Future<Output = Result<Value, Error>> + Send + 'r>>
                    + Sync,
            >,
        ),
    >,
}

impl Ftable {
    pub fn new() -> Self {
        Self {
            ftable: HashMap::new(),
        }
    }

    pub fn with_intrinsics(id_dispenser: &mut IdDispenser) -> Self {
        let mut this = Self {
            ftable: HashMap::new(),
        };

        // arithmetic
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "+".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Uint(x + y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Int(x + y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Float(x + y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum { got: x.clone() }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "-".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Uint(x - y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Int(x - y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Float(x - y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum { got: x.clone() }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "*".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Uint(x * y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Int(x * y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Float(x * y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum { got: x.clone() }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "/".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Uint(x / y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Int(x / y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Float(x / y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedNum { got: x.clone() }),
                    }
                })
            }),
        );

        // inequalities
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: ">=".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x >= y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x >= y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x >= y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x >= y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedCmp { got: x.clone() }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: ">".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x > y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x > y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x > y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x > y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedCmp { got: x.clone() }),
                    }
                })
            }),
        );

        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "<=".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x <= y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x <= y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x <= y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x <= y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedCmp { got: x.clone() }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "<".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x < y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x < y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x < y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x < y)),
                        // Bad types
                        (Some(Value::Uint(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: y.clone(),
                        }),
                        (Some(Value::Int(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Int,
                            got: y.clone(),
                        }),
                        (Some(Value::Float(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::Float,
                            got: y.clone(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedCmp { got: x.clone() }),
                    }
                })
            }),
        );
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "==".to_string(),
                params: vec![a!(), a!()],
                ret: Type::Bool,
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Primitives
                        (Some(Value::Null), Some(Value::Null)) => Ok(Value::Bool(true)),
                        (Some(Value::Bool(x)), Some(Value::Bool(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::Uint(x)), Some(Value::Uint(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::Int(x)), Some(Value::Int(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::Float(x)), Some(Value::Float(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::String(x)), Some(Value::String(y))) => Ok(Value::Bool(x == y)),
                        (Some(Value::List(xs)), Some(Value::List(ys))) => Ok(Value::Bool(xs == ys)),
                        (Some(Value::Tuple(xs)), Some(Value::Tuple(ys))) => {
                            Ok(Value::Bool(xs == ys))
                        }
                        (Some(Value::Dict(xs)), Some(Value::Dict(ys))) => Ok(Value::Bool(xs == ys)),

                        (Some(Value::Id(i)), Some(Value::Id(j))) => Ok(Value::Bool(i == j)),
                        // Functions
                        (Some(Value::Function(f)), Some(Value::Function(g))) => {
                            Ok(Value::Bool(f == g))
                        }
                        (Some(Value::Closure(f)), Some(Value::Closure(g))) => {
                            Ok(Value::Bool(f == g))
                        }
                        // Data
                        (Some(Value::Data(x)), Some(Value::Data(y))) => Ok(Value::Bool(x == y)),
                        // Everything else
                        _ => Ok(Value::Bool(false)),
                    }
                })
            }),
        );

        // '++'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "++".to_string(),
                params: vec![a!(), a!()],
                ret: a!(),
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::List(xs)), Some(Value::List(ys))) => {
                            let mut zs = Vec::with_capacity(xs.len() + ys.len());
                            zs.extend(xs.iter().cloned());
                            zs.extend(ys.iter().cloned());
                            Ok(Value::List(zs))
                        }
                        (Some(Value::String(xs)), Some(Value::String(ys))) => {
                            Ok(Value::String(format!("{}{}", xs, ys)))
                        }
                        // Bad types
                        (Some(Value::List(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: list!(b!()),
                            got: y.clone(),
                        }),
                        (Some(Value::String(_)), Some(y)) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: y.clone(),
                        }),
                        // Everything else
                        (Some(x), _) => Err(Error::ExpectedConcat { got: x.clone() }),
                    }
                })
            }),
        );

        // 'map'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "map".to_string(),
                params: vec![arrow!(a!() => b!()), list!(a!())],
                ret: list!(b!()),
            },
            Box::new(|ctx, runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(f), Some(Value::List(xs))) => {
                            let mut ys = Vec::with_capacity(xs.len());
                            for x in xs {
                                ys.push(apply(ctx, runner, f.clone(), x.clone()).await?);
                            }
                            Ok(Value::List(ys))
                        }
                        // Everything else
                        (Some(_), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                        }),
                    }
                })
            }),
        );
        // 'filter'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "filter".to_string(),
                params: vec![arrow!(a!() => Type::Bool), list!(a!())],
                ret: list!(a!()),
            },
            Box::new(|ctx, runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(f), Some(Value::List(xs))) => {
                            let mut ys = Vec::with_capacity(xs.len());
                            for x in xs {
                                match apply(ctx, runner, f.clone(), x.clone()).await? {
                                    Value::Bool(true) => ys.push(x.clone()),
                                    Value::Bool(_) => {}
                                    _ => Err(Error::BadType)?,
                                }
                            }
                            Ok(Value::List(ys))
                        }
                        // Everything else
                        (Some(_), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                        }),
                    }
                })
            }),
        );
        // 'zip'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "zip".to_string(),
                params: vec![list!(a!()), list!(b!())],
                ret: list!(tuple!(a!(), b!())),
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::List(xs)), Some(Value::List(ys))) => {
                            let mut zs = Vec::with_capacity(xs.len().min(ys.len()));
                            for (x, y) in xs.iter().zip(ys.iter()) {
                                zs.push(Value::Tuple(vec![x.clone(), y.clone()]))
                            }
                            Ok(Value::List(zs))
                        }
                        // Everything else
                        (Some(Value::List(_)), Some(x)) => Err(Error::UnexpectedType {
                            expected: list!(b!()),
                            got: x.clone(),
                        }),
                        (Some(x), _) => Err(Error::UnexpectedType {
                            expected: list!(a!()),
                            got: x.clone(),
                        }),
                    }
                })
            }),
        );

        // 'get'
        this.register_function(
            Function {
                id: id_dispenser.next(),
                name: "get".to_string(),
                params: vec![Type::Uint, a!()],
                ret: b!(),
            },
            Box::new(|_ctx, _runner, args| {
                Box::pin(async move {
                    match (args.get(0), args.get(1)) {
                        // Wrong number of arguments
                        (_, None) | (None, _) => Err(Error::ExpectedArguments {
                            expected: 2,
                            got: args.len(),
                        }),
                        // Implementation
                        (Some(Value::Uint(i)), Some(Value::List(xs))) => {
                            match xs.get(*i as usize) {
                                Some(x) => Ok(x.clone()),
                                _ => Err(Error::IndexOutOfBounds),
                            }
                        }
                        (Some(Value::Uint(i)), Some(Value::Tuple(xs))) => match xs.get(*i as usize)
                        {
                            Some(x) => Ok(x.clone()),
                            _ => Err(Error::IndexOutOfBounds),
                        },
                        (Some(Value::String(k)), Some(Value::Dict(dict))) => {
                            for (k_check, v) in dict {
                                if k == k_check {
                                    return Ok(v.clone());
                                }
                            }
                            Err(Error::KeyNotFound)
                        }
                        // Bad types
                        (Some(x), Some(Value::List(_))) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: x.clone(),
                        }),
                        (Some(x), Some(Value::Tuple(_))) => Err(Error::UnexpectedType {
                            expected: Type::Uint,
                            got: x.clone(),
                        }),
                        (Some(x), Some(Value::Dict(_))) => Err(Error::UnexpectedType {
                            expected: Type::String,
                            got: x.clone(),
                        }),
                        // Everything else
                        (_, Some(x)) => Err(Error::ExpectedIndexable { got: x.clone() }),
                    }
                })
            }),
        );

        this
    }

    pub fn register_function(
        &mut self,
        function: Function,
        lam: Box<
            dyn for<'r> Fn(
                    &'r Context,
                    &'r Ftable,
                    &'r Vec<Value>,
                )
                    -> Pin<Box<dyn Future<Output = Result<Value, Error>> + Send + 'r>>
                + Sync,
        >,
    ) {
        self.ftable.insert(function.id, (function, lam));
    }

    pub fn register_adt(&mut self, id_dispenser: &mut IdDispenser, adt: ADT) {
        let ret = Type::ADT(adt.clone());
        for variant in adt.variants {
            match &variant.fields {
                Some(ADTVariantFields::Named(named_fields)) => {
                    for (field_name, _field_type) in &named_fields.fields {
                        let ret = ret.clone();
                        let field_name = field_name.clone();
                        let function = Function {
                            id: id_dispenser.next(),
                            name: field_name.clone(),
                            params: vec![Type::Dict(
                                vec![(field_name.clone(), a!())].into_iter().collect(),
                            )],
                            ret: a!(),
                        };
                        self.register_function(
                            function,
                            Box::new(move |_ctx, _runner, args| {
                                let ret = ret.clone();
                                let field_name = field_name.clone();
                                Box::pin(async move {
                                    match args.get(0) {
                                        // Wrong number of arguments
                                        None => Err(Error::ExpectedArguments {
                                            expected: 1,
                                            got: args.len(),
                                        }),
                                        // Implementation
                                        Some(Value::Data(data)) => {
                                            match &data.fields {
                                                Some(DataFields::Named(NamedDataFields {
                                                    fields,
                                                })) => match fields.get(&field_name) {
                                                    Some(field_value) => Ok(field_value.clone()),
                                                    _ => Err(Error::FieldNotFound {
                                                        field: field_name.clone(),
                                                    }),
                                                },
                                                // FIXME: We do not currently
                                                // support field-less ADT
                                                // variants
                                                _ => unimplemented!(),
                                            }
                                        }
                                        // Bad type
                                        Some(x) => Err(Error::UnexpectedType {
                                            expected: ret.clone(),
                                            got: x.clone(),
                                        }),
                                    }
                                })
                            }),
                        );
                    }

                    let ret = ret.clone();
                    let named_fields = named_fields.clone();
                    let function = Function {
                        id: id_dispenser.next(),
                        name: variant.name.clone(),
                        params: vec![Type::Dict(
                            named_fields
                                .fields
                                .iter()
                                .map(|(n, t)| (n.clone(), t.clone()))
                                .collect(),
                        )],
                        ret: ret.clone(),
                    };
                    self.register_function(
                        function,
                        Box::new(move |_ctx, _runner, args| {
                            let ret = ret.clone();
                            let variant = variant.clone();
                            let named_fields = named_fields.clone();
                            Box::pin(async move {
                                match args.get(0) {
                                    // Wrong number of arguments
                                    None => Err(Error::ExpectedArguments {
                                        expected: 1,
                                        got: args.len(),
                                    }),
                                    // Implementation
                                    Some(dict @ Value::Dict(fields))
                                        if dict.implements(&Type::Dict(
                                            named_fields
                                                .fields
                                                .iter()
                                                .map(|(n, t)| (n.clone(), t.clone()))
                                                .collect(),
                                        )) =>
                                    {
                                        Ok(Value::Data(Data {
                                            name: variant.name.clone(),
                                            fields: Some(DataFields::Named(NamedDataFields {
                                                fields: fields.clone(),
                                            })),
                                        }))
                                    }
                                    // Bad type
                                    Some(x) => Err(Error::UnexpectedType {
                                        expected: ret.clone(),
                                        got: x.clone(),
                                    }),
                                }
                            })
                        }),
                    );
                }
                Some(ADTVariantFields::Unnamed(unnamed_fields)) => {
                    let ret = ret.clone();
                    let unnamed_fields = unnamed_fields.clone();
                    let function = Function {
                        id: id_dispenser.next(),
                        name: variant.name.clone(),
                        params: vec![Type::Tuple(unnamed_fields.fields.clone())],
                        ret: ret.clone(),
                    };
                    self.register_function(
                        function,
                        Box::new(move |_ctx, _runner, args| {
                            let ret = ret.clone();
                            let unnamed_fields = unnamed_fields.clone();
                            let variant = variant.clone();
                            Box::pin(async move {
                                match args.get(0) {
                                    // Wrong number of arguments
                                    None => Err(Error::ExpectedArguments {
                                        expected: 1,
                                        got: args.len(),
                                    }),
                                    // Implementation
                                    Some(tuple @ Value::Tuple(fields))
                                        if tuple.implements(&Type::Tuple(
                                            unnamed_fields.fields.clone(),
                                        )) =>
                                    {
                                        Ok(Value::Data(Data {
                                            name: variant.name.clone(),
                                            fields: Some(DataFields::Unnamed(UnnamedDataFields {
                                                fields: fields.clone(),
                                            })),
                                        }))
                                    }
                                    // Bad type
                                    Some(x) => Err(Error::UnexpectedType {
                                        expected: ret.clone(),
                                        got: x.clone(),
                                    }),
                                }
                            })
                        }),
                    );
                }

                // FIXME: We do not currently support ADT variants that do not
                // have fields
                _ => unimplemented!(),
            }
        }
    }

    pub async fn lookup(&self, _ctx: &Context, var: &Var) -> Result<Value, Error> {
        self.ftable
            .get(&var.id)
            .map(|(f, _)| Value::Function(f.clone()))
            .ok_or(Error::VarNotFound { var: var.clone() })
    }

    pub async fn dispatch(
        &self,
        ctx: &Context,
        ftable: &Ftable,
        function: &Function,
        args: &Vec<Value>,
    ) -> Result<Value, Error> {
        match self.ftable.get(&function.id) {
            Some((_f, lam)) => lam(ctx, ftable, args).await,
            None => Err(Error::FunctionNotFound {
                function: function.clone(),
            }),
        }
    }

    pub fn scope(&self) -> Scope {
        let mut scope = Scope::new();
        for (id, (f, _)) in &self.ftable {
            scope.vars.insert(f.name.clone(), *id);
        }
        scope
    }
}
