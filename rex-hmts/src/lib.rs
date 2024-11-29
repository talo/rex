use std::collections::BTreeMap;

use rex_ast::{
    ast::{Lambda, Var, AST},
    id::IdDispenser,
    types::{ADTVariantFields, ADTVariantNamedFields, ADTVariantUnnamedFields, Type},
};
use rex_engine::{
    value::{Closure, Data, DataFields, FunctionLike, NamedDataFields, UnnamedDataFields, Value},
    Context,
};
use rex_lexer::span::Span;

pub fn resolve_type_var(ident: &str) -> Type {
    match ident {
        "null" => Type::Null,
        "bool" => Type::Bool,
        "uint" => Type::Uint,
        "int" => Type::Int,
        "float" => Type::Float,
        "string" => Type::String,
        _ => panic!("unknown type variable: {}", ident),
    }
}

pub fn gen_random_value(id_dispenser: &mut IdDispenser, t: &Type) -> Value {
    match t {
        Type::Null => Value::Null,
        Type::Bool => Value::Bool(rand::random()),
        Type::Uint => Value::Uint(rand::random()),
        Type::Int => Value::Int(rand::random()),
        Type::Float => Value::Float(rand::random()),
        Type::String => {
            let len = rand::random::<usize>() % 10;
            let mut xs = String::with_capacity(len);
            for _ in 0..len {
                xs.push((rand::random::<u8>() % 36 + 48) as char);
            }
            Value::String(xs)
        }
        Type::Option(t) => {
            if rand::random() {
                Value::Null
            } else {
                gen_random_value(id_dispenser, t)
            }
        }
        Type::Result(t, e) => {
            if rand::random() {
                Value::Result(Ok(Box::new(gen_random_value(id_dispenser, t))))
            } else {
                Value::Result(Err(Box::new(gen_random_value(id_dispenser, e))))
            }
        }
        Type::Tuple(ts) => {
            let mut xs = Vec::with_capacity(ts.len());
            for t in ts {
                xs.push(gen_random_value(id_dispenser, t));
            }
            Value::Tuple(xs)
        }
        Type::Dict(ts) => {
            let mut xs = BTreeMap::new();
            for (k, t) in ts {
                xs.insert(k.clone(), gen_random_value(id_dispenser, t));
            }
            Value::Dict(xs)
        }
        Type::Arrow(_a, b) => {
            let lam_id = id_dispenser.next();
            let body_id = id_dispenser.next();

            Value::Closure(Closure {
                captured_ctx: Context {
                    vars: vec![(body_id, gen_random_value(id_dispenser, b))]
                        .into_iter()
                        .collect(),
                },
                captured_args: vec![],
                body: FunctionLike::Lambda(Lambda {
                    span: Span::default(),
                    id: id_dispenser.next(),
                    var: Var::new(Span::default(), lam_id, "_lam"),
                    body: Box::new(AST::Var(Var::new(Span::default(), body_id, "_x"))),
                }),
            })
        }
        Type::Generic(..) => panic!("cannot generate random generic value"),
        Type::List(t) => {
            let len = rand::random::<usize>() % 10;
            let mut xs = Vec::with_capacity(len);
            for _ in 0..len {
                xs.push(gen_random_value(id_dispenser, t));
            }
            Value::List(xs)
        }
        Type::ADT(adt) => {
            let i = rand::random::<usize>() % adt.variants.len();
            let variant = &adt.variants[i];
            match &variant.fields {
                Some(ADTVariantFields::Named(ADTVariantNamedFields { fields })) => {
                    let mut xs = BTreeMap::new();
                    for (k, t) in fields {
                        xs.insert(k.clone(), gen_random_value(id_dispenser, t));
                    }
                    Value::Data(Data {
                        name: variant.name.clone(),
                        fields: Some(DataFields::Named(NamedDataFields { fields: xs })),
                    })
                }
                Some(ADTVariantFields::Unnamed(ADTVariantUnnamedFields { fields })) => {
                    let mut xs = Vec::with_capacity(fields.len());
                    for t in fields {
                        xs.push(gen_random_value(id_dispenser, t));
                    }
                    Value::Data(Data {
                        name: variant.name.clone(),
                        fields: Some(DataFields::Unnamed(UnnamedDataFields { fields: xs })),
                    })
                }
                None => Value::Data(Data {
                    name: variant.name.clone(),
                    fields: None,
                }),
            }
        }
    }
}
