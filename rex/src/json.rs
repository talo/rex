use crate::{
    ast::{expr::Expr, id::Id},
    engine::error::Error as EngineError,
    lexer::span::Span,
    type_system::types::Type,
};
use serde_json::{Map, Number, Value};
use std::collections::BTreeMap;

/// Convert a JSON [`Value`] to an [`Expr`]
///
/// The conversion is done in a manner that is compatible with [`serde_json`]. The resulting
/// [`Expr`] is the same format as would be produced by [`Encode::try_encode`] for a given Rust
/// type.
///
/// This function is intended for cases where you don't have access to a suitable Rust type at
/// compile time. An example is when invoking a module from tengu, where the type is only available
/// at runtime.
///
/// [`Encode::try_encode`]: crate::engine::codec::Encode::try_encode
pub fn json_to_expr(json: &Value, want: &Type) -> Result<Expr, EngineError> {
    match (want, json) {
        // (Type::UnresolvedVar(_), _) => unimplemented!(),
        // (Type::Var(_), _) => unimplemented!(),
        // (Type::ForAll(_, _, _), _) => unimplemented!(),
        (Type::ADT(adt), _) => {
            if adt.variants.len() == 1 {
                if let Some(variant_t) = &adt.variants[0].t {
                    Ok(Expr::Named(
                        Id::new(),
                        Span::default(),
                        adt.variants[0].name.clone(),
                        Some(Box::new(json_to_expr(json, variant_t)?)),
                    ))
                } else {
                    Err(type_error(json, want))
                }
            } else {
                for variant in adt.variants.iter() {
                    match &variant.t {
                        Some(t) => match json {
                            Value::Object(entries) if entries.len() == 1 => {
                                if let Some(inner_json) = entries.get(&variant.name) {
                                    let inner_rex = json_to_expr(inner_json, t)?;
                                    return Ok(Expr::Named(
                                        Id::new(),
                                        Span::default(),
                                        variant.name.clone(),
                                        Some(Box::new(inner_rex)),
                                    ));
                                }
                            }
                            _ => {}
                        },
                        None => match json {
                            Value::String(s) if s == &variant.name => {
                                return Ok(Expr::Named(
                                    Id::new(),
                                    Span::default(),
                                    variant.name.clone(),
                                    None,
                                ));
                            }
                            _ => {}
                        },
                    }
                }
                Err(type_error(json, want))
            }
        }
        // (Type::Arrow(_, _), _) => unimplemented!(),
        (Type::Result(ok_type, err_type), Value::Object(name_object)) if name_object.len() == 1 => {
            if let Some(ok_value) = name_object.get("Ok") {
                let inner = json_to_expr(ok_value, ok_type)?;
                Ok(Expr::Named(
                    Id::new(),
                    Span::default(),
                    "Ok".to_string(),
                    Some(Box::new(inner)),
                ))
            } else if let Some(err_value) = name_object.get("Err") {
                let inner = json_to_expr(err_value, err_type)?;
                Ok(Expr::Named(
                    Id::new(),
                    Span::default(),
                    "Err".to_string(),
                    Some(Box::new(inner)),
                ))
            } else {
                Err(type_error(json, want))
            }
        }
        (Type::Option(inner), _) => match json {
            Value::Null => {
                return Ok(Expr::Named(
                    Id::new(),
                    Span::default(),
                    "None".to_string(),
                    None,
                ));
            }
            _ => {
                return Ok(Expr::Named(
                    Id::new(),
                    Span::default(),
                    "Some".to_string(),
                    Some(Box::new(json_to_expr(json, inner)?)),
                ));
            }
        },
        (Type::List(item_type), Value::Array(json_items)) => {
            let mut exprs: Vec<Expr> = Vec::new();
            for json_item in json_items {
                exprs.push(json_to_expr(json_item, item_type)?);
            }
            Ok(Expr::List(Id::new(), Span::default(), exprs))
        }
        (Type::Dict(type_entries), Value::Object(json_entries))
            if type_entries.len() == json_entries.len() =>
        {
            let mut expr_entries: BTreeMap<String, Expr> = BTreeMap::new();
            for (k, t) in type_entries.iter() {
                let json_entry = json_entries.get(k).ok_or_else(|| type_error(json, want))?;
                expr_entries.insert(k.clone(), json_to_expr(&json_entry, t)?);
            }
            Ok(Expr::Dict(Id::new(), Span::default(), expr_entries))
        }
        (Type::Tuple(item_types), Value::Array(item_values))
            if item_types.len() == item_values.len() =>
        {
            let mut exprs: Vec<Expr> = Vec::new();
            for i in 0..item_types.len() {
                exprs.push(json_to_expr(&item_values[i], &item_types[i])?);
            }
            Ok(Expr::Tuple(Id::new(), Span::default(), exprs))
        }
        (Type::Bool, Value::Bool(b)) => Ok(Expr::Bool(Id::new(), Span::default(), *b)),
        (Type::Uint, Value::Number(n)) => {
            if let Some(v) = n.as_u64() {
                Ok(Expr::Uint(Id::new(), Span::default(), v))
            } else {
                Err(type_error(json, want))
            }
        }
        (Type::Int, Value::Number(n)) => {
            if let Some(v) = n.as_i64() {
                Ok(Expr::Int(Id::new(), Span::default(), v))
            } else {
                Err(type_error(json, want))
            }
        }
        (Type::Float, Value::Number(n)) => {
            if let Some(v) = n.as_f64() {
                Ok(Expr::Float(Id::new(), Span::default(), v))
            } else {
                Err(type_error(json, want))
            }
        }
        (Type::String, Value::String(s)) => Ok(Expr::String(Id::new(), Span::default(), s.clone())),
        (Type::Uuid, _) => Ok(Expr::Uuid(
            Id::new(),
            Span::default(),
            serde_json::from_value(json.clone()).map_err(|_| type_error(json, want))?,
        )),
        (Type::DateTime, _) => Ok(Expr::DateTime(
            Id::new(),
            Span::default(),
            serde_json::from_value(json.clone()).map_err(|_| type_error(json, want))?,
        )),
        (_, _) => Err(type_error(json, want)),
    }
}

/// Convert an [`Expr`] to a JSON [`Value`]
///
/// The conversion is done in a manner that is compatible with [`serde_json`]. The resulting JSON
/// [`Value`] is in the same format as you'd get if you called [`Decode::try_decode`] for a given
/// Rust type, followed by calling [`serde_json::to_value`] on the result.
///
/// This function is intended for cases where you don't have access to a suitable Rust type at
/// compile time.  An example is when serializing the input arguments to a module invoked from
/// tengu, since the argument types are only known at runtime.
///
/// [`Decode::try_decode`]: crate::engine::codec::Decode::try_decode
pub fn expr_to_json(expr: &Expr, want: &Type) -> Result<Value, EngineError> {
    match (want, expr) {
        // (Type::UnresolvedVar(_), _) => unimplemented!(),
        // (Type::Var(_), _) => unimplemented!(),
        // (Type::ForAll(_, _, _), _) => unimplemented!(),
        (Type::ADT(adt), _) => {
            if adt.variants.len() == 1 {
                match expr {
                    Expr::Named(_, _, n, Some(inner_expr)) if n == &adt.variants[0].name => {
                        if let Some(inner) = &adt.variants[0].t {
                            expr_to_json(inner_expr, inner)
                        } else {
                            todo!(
                                "expr_to_json2 for ADT with single variant, no inner type: {:#?}",
                                adt
                            )
                        }
                    }
                    _ => Err(expr_error(expr, want)),
                }
            } else {
                match expr {
                    Expr::Named(_, _, n, opt_inner) => {
                        for variant in adt.variants.iter() {
                            if n == &variant.name {
                                match (&variant.t, opt_inner) {
                                    (Some(variant_t), Some(inner)) => {
                                        let mut map: Map<String, Value> = Map::new();
                                        map.insert(
                                            variant.name.clone(),
                                            expr_to_json(inner, variant_t)?,
                                        );
                                        return Ok(Value::Object(map));
                                    }
                                    (None, None) => return Ok(Value::String(n.clone())),
                                    (_, _) => return Err(expr_error(expr, want)),
                                }
                            }
                        }
                        Err(expr_error(expr, want))
                    }
                    _ => Err(expr_error(expr, want)),
                }
                // todo!("expr_to_json for ADT with multiple variants: {:#?}", adt)
            }
        }
        // (Type::Arrow(_, _), _) => unimplemented!(),
        (Type::Result(ok_type, err_type), Expr::Named(_, _, n, Some(v))) => {
            let mut map: Map<String, Value> = Map::new();
            match n.as_str() {
                "Ok" => {
                    map.insert("Ok".to_string(), expr_to_json(v, ok_type)?);
                    Ok(Value::Object(map))
                }
                "Err" => {
                    map.insert("Err".to_string(), expr_to_json(v, err_type)?);
                    Ok(Value::Object(map))
                }
                _ => Err(expr_error(expr, want)),
            }
        }
        (Type::Option(inner_type), Expr::Named(_, _, n, ov)) => match (n.as_str(), ov) {
            ("Some", Some(v)) => expr_to_json(v, inner_type),
            ("None", None) => Ok(Value::Null),
            _ => Err(expr_error(expr, want)),
        },
        (Type::List(inner_type), Expr::List(_, _, items)) => {
            let mut values: Vec<Value> = Vec::new();
            for item in items.iter() {
                values.push(expr_to_json(item, inner_type)?);
            }
            Ok(Value::Array(values))
        }
        (Type::Dict(type_entries), Expr::Dict(_, _, value_entries))
            if type_entries.len() == value_entries.len() =>
        {
            let mut map: Map<String, Value> = Map::new();
            for (k, t) in type_entries.iter() {
                let v = value_entries.get(k).ok_or_else(|| expr_error(expr, want))?;
                map.insert(k.clone(), expr_to_json(v, t)?);
            }
            Ok(Value::Object(map))
        }
        (Type::Tuple(inner_types), Expr::Tuple(_, _, items))
            if inner_types.len() == items.len() =>
        {
            let mut values: Vec<Value> = Vec::new();
            for i in 0..inner_types.len() {
                values.push(expr_to_json(&items[i], &inner_types[i])?);
            }
            Ok(Value::Array(values))
        }
        (Type::Bool, Expr::Bool(_, _, b)) => Ok(Value::Bool(*b)),
        (Type::Uint, Expr::Uint(_, _, x)) => Ok(Value::Number((*x).into())),
        (Type::Int, Expr::Int(_, _, x)) => Ok(Value::Number((*x).into())),
        (Type::Float, Expr::Float(_, _, x)) => match Number::from_f64(*x) {
            Some(n) => Ok(Value::Number(n)),
            None => Err(expr_error(expr, want)),
        },
        (Type::String, Expr::String(_, _, x)) => Ok(Value::String(x.clone())),
        (Type::Uuid, Expr::Uuid(_, _, u)) => {
            Ok(serde_json::to_value(u).map_err(|_| expr_error(expr, want))?)
        }
        (Type::DateTime, Expr::DateTime(_, _, dt)) => {
            Ok(serde_json::to_value(dt).map_err(|_| expr_error(expr, want))?)
        }
        _ => Err(expr_error(expr, want)),
    }
}

fn type_error(json: &Value, want: &Type) -> EngineError {
    EngineError::ExpectedTypeGotJSON {
        expected: want.clone(),
        got: json.clone(),
    }
}

fn expr_error(expr: &Expr, want: &Type) -> EngineError {
    EngineError::ExpectedTypeGotValue {
        expected: want.clone(),
        got: expr.clone(),
    }
}
