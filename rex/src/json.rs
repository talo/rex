use crate::{
    ast::expr::Expr,
    engine::{
        codec::{Decode, Encode},
        error::Error as EngineError,
    },
    lexer::span::Span,
    type_system::types::{AppliedType, Type, TypeCon},
};
use serde_json::{Map, Number, Value};
use std::{collections::BTreeMap, sync::Arc};

#[derive(Clone, Debug)]
pub struct EnumPatch {
    pub enum_name: String,
    pub discriminant: i64,
}

/// Options for handling enums. Some enums are encoded as integers, while others are encoded as
/// strings, and the types we get from Ouroboros don't provide enough information to know which
/// kind of encoding to use. For this reason, we whitelist a set of ADT names which are to be
/// represented in JSON as ints. This applies to both encoding and decoding.
///
/// Additionally, there are enum types in libqdx which do use int encoding, but the discriminant
/// in the type information are missing or incorrect. This occurs due to a bug in the ouroboros
/// `derive(TypeInfo)` macro which assumes that discriminants are a `u8`, and ignores any that are
/// outside the range 0-255. If an enum specifies a negative value or one greater than 255, it
/// will not have a discriminant. To correctly handle these types in such a way that the JSON
/// serialization of Exprs is consistent with that of the serde JSON serialization methods, we
/// allow callers to supply a list of "patches" which tell us what int discriminants to use for
/// given names in a given in num. At the time of writing, I've only found one such case,
/// `Stereochemistry::Down`, which has the discriminant `-1`.
#[derive(Clone, Default, Debug)]
pub struct JsonOptions {
    pub int_enums: BTreeMap<String, Vec<EnumPatch>>,
}

impl JsonOptions {
    /// Register the name of an ADT enum that should be encoded as integers, rather than strings.
    /// This only has an effect for variants which do not have an associated type, and which have
    /// a discriminator present.
    pub fn add_int_enum(&mut self, name: &str) {
        self.int_enums.insert(name.to_string(), vec![]);
    }

    /// Register the name of an ADT enum that should be encoded as integers, rather than strings.
    /// This only has an effect for variants which do not have an associated type, and which have
    /// a discriminator present. In addition, apply the substitutions in `patches` when deciding
    /// what discriminator a given variant has.
    pub fn add_int_enum_with_patches(&mut self, name: &str, patches: Vec<EnumPatch>) {
        self.int_enums.insert(name.to_string(), patches);
    }
}

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
pub fn json_to_expr(
    json: &Value,
    want: &Arc<Type>,
    opts: &JsonOptions,
) -> Result<Arc<Expr>, EngineError> {
    match (&**want, json) {
        // (Type::UnresolvedVar(_), _) => unimplemented!(),
        // (Type::Var(_), _) => unimplemented!(),
        // (Type::ForAll(_, _, _), _) => unimplemented!(),
        (Type::ADT(adt), _) => {
            if adt.variants.is_empty() {
                match json {
                    Value::Null => Ok(Arc::new(Expr::Named(
                        Span::default(),
                        adt.name.clone(),
                        None,
                    ))),
                    _ => Err(type_error(json, want)),
                }
            } else if adt.variants.len() == 1
                && adt.name == "serde_json::Value"
                && adt.variants[0].name == "serde_json::Value"
            {
                Ok(json.clone().try_encode(Span::default())?)
            } else if adt.variants.len() == 1 {
                if let Some(variant_t) = &adt.variants[0].t {
                    Ok(Arc::new(Expr::Named(
                        Span::default(),
                        adt.variants[0].name.clone(),
                        Some(json_to_expr(json, variant_t, opts)?),
                    )))
                } else {
                    Err(type_error(json, want))
                }
            } else {
                let is_int_enum = opts.int_enums.contains_key(&adt.name);
                for variant in adt.variants.iter() {
                    if let (true, None, Some(discriminant)) =
                        (is_int_enum, &variant.t, variant.discriminant)
                    {
                        if let Value::Number(n) = json {
                            if let Some(n) = n.as_i64() {
                                if n == discriminant {
                                    return Ok(Arc::new(Expr::Named(
                                        Span::default(),
                                        variant.name.clone(),
                                        None,
                                    )));
                                }
                            }
                        }
                    } else {
                        match &variant.t {
                            Some(t) => match json {
                                Value::Object(entries) if entries.len() == 1 => {
                                    if let Some(inner_json) = entries.get(&variant.name) {
                                        let inner_rex = json_to_expr(inner_json, t, opts)?;
                                        return Ok(Arc::new(Expr::Named(
                                            Span::default(),
                                            variant.name.clone(),
                                            Some(inner_rex),
                                        )));
                                    }
                                }
                                _ => {}
                            },
                            None => match json {
                                Value::String(s) if s == &variant.name => {
                                    return Ok(Arc::new(Expr::Named(
                                        Span::default(),
                                        variant.name.clone(),
                                        None,
                                    )));
                                }
                                _ => {}
                            },
                        }
                    }
                }
                Err(type_error(json, want))
            }
        }
        (Type::App(_, _), rhs) => match want.as_applied_type() {
            Some(AppliedType::Result(err_type, ok_type)) => match rhs {
                Value::Object(name_object) if name_object.len() == 1 => {
                    if let Some(ok_value) = name_object.get("Ok") {
                        let inner = json_to_expr(ok_value, ok_type, opts)?;
                        Ok(Arc::new(Expr::Named(
                            Span::default(),
                            "Ok".to_string(),
                            Some(inner),
                        )))
                    } else if let Some(err_value) = name_object.get("Err") {
                        let inner = json_to_expr(err_value, err_type, opts)?;
                        Ok(Arc::new(Expr::Named(
                            Span::default(),
                            "Err".to_string(),
                            Some(inner),
                        )))
                    } else {
                        Err(type_error(json, want))
                    }
                }
                _ => Err(type_error(json, want)),
            },
            Some(AppliedType::Option(inner)) => match json {
                Value::Null => Ok(Arc::new(Expr::Named(
                    Span::default(),
                    "None".to_string(),
                    None,
                ))),
                _ => Ok(Arc::new(Expr::Named(
                    Span::default(),
                    "Some".to_string(),
                    Some(json_to_expr(json, inner, opts)?),
                ))),
            },
            Some(AppliedType::Promise(_)) => Ok(Arc::new(Expr::Promise(
                Span::default(),
                serde_json::from_value(json.clone()).map_err(|_e| type_error(json, want))?,
            ))),
            Some(AppliedType::List(item_type)) => match rhs {
                Value::Array(json_items) => {
                    let mut exprs: Vec<Arc<Expr>> = Vec::new();
                    for json_item in json_items {
                        exprs.push(json_to_expr(json_item, item_type, opts)?);
                    }
                    Ok(Arc::new(Expr::List(Span::default(), exprs)))
                }
                _ => Err(type_error(json, want)),
            },
            _ => Err(type_error(json, want)),
        },
        (Type::Dict(type_entries), Value::Object(json_entries)) => {
            let mut expr_entries: BTreeMap<String, Arc<Expr>> = BTreeMap::new();
            for (k, t) in type_entries.iter() {
                let expr = match json_entries.get(k) {
                    Some(v) => json_to_expr(v, t, opts)?,
                    None => json_to_expr(&serde_json::Value::Null, t, opts)?,
                };
                expr_entries.insert(k.clone(), expr);
            }
            Ok(Arc::new(Expr::Dict(Span::default(), expr_entries)))
        }
        (Type::Tuple(item_types), Value::Array(item_values))
            if item_types.len() == item_values.len() =>
        {
            let mut exprs: Vec<Arc<Expr>> = Vec::new();
            for i in 0..item_types.len() {
                exprs.push(json_to_expr(&item_values[i], &item_types[i], opts)?);
            }
            Ok(Arc::new(Expr::Tuple(Span::default(), exprs)))
        }
        (Type::Con(TypeCon::Bool), Value::Bool(b)) => Ok(Arc::new(Expr::Bool(Span::default(), *b))),
        (Type::Con(TypeCon::Uint), Value::Number(n)) => {
            if let Some(v) = n.as_u64() {
                Ok(Arc::new(Expr::Uint(Span::default(), v)))
            } else {
                Err(type_error(json, want))
            }
        }
        (Type::Con(TypeCon::Int), Value::Number(n)) => {
            if let Some(v) = n.as_i64() {
                Ok(Arc::new(Expr::Int(Span::default(), v)))
            } else {
                Err(type_error(json, want))
            }
        }
        (Type::Con(TypeCon::Float), Value::Number(n)) => {
            if let Some(v) = n.as_f64() {
                Ok(Arc::new(Expr::Float(Span::default(), v)))
            } else {
                Err(type_error(json, want))
            }
        }
        (Type::Con(TypeCon::String), Value::String(s)) => {
            Ok(Arc::new(Expr::String(Span::default(), s.clone())))
        }
        (Type::Con(TypeCon::Uuid), _) => Ok(Arc::new(Expr::Uuid(
            Span::default(),
            serde_json::from_value(json.clone()).map_err(|_| type_error(json, want))?,
        ))),
        (Type::Con(TypeCon::DateTime), _) => Ok(Arc::new(Expr::DateTime(
            Span::default(),
            serde_json::from_value(json.clone()).map_err(|_| type_error(json, want))?,
        ))),
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
pub fn expr_to_json(
    expr: &Arc<Expr>,
    want: &Arc<Type>,
    opts: &JsonOptions,
) -> Result<Value, EngineError> {
    match (&**want, &**expr) {
        // (Type::UnresolvedVar(_), _) => unimplemented!(),
        // (Type::Var(_), _) => unimplemented!(),
        // (Type::ForAll(_, _, _), _) => unimplemented!(),
        (Type::ADT(adt), _) => {
            if adt.variants.is_empty() {
                Ok(Value::Null)
            } else if adt.variants.len() == 1
                && adt.name == "serde_json::Value"
                && adt.variants[0].name == "serde_json::Value"
            {
                serde_json::Value::try_decode(expr)
            } else if adt.variants.len() == 1 {
                match &**expr {
                    Expr::Named(_, n, Some(inner_expr)) if n == &adt.variants[0].name => {
                        if let Some(inner) = &adt.variants[0].t {
                            expr_to_json(inner_expr, inner, opts)
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
                match &**expr {
                    Expr::Named(_, n, opt_inner) => {
                        for variant in adt.variants.iter() {
                            if n == &variant.name {
                                if let Some(discriminant) = &variant.discriminant {
                                    if variant.t.is_none() && opts.int_enums.contains_key(&adt.name)
                                    {
                                        return Ok(Value::Number((*discriminant).into()));
                                    }
                                }
                                match (&variant.t, opt_inner) {
                                    (Some(variant_t), Some(inner)) => {
                                        let mut map: Map<String, Value> = Map::new();
                                        map.insert(
                                            variant.name.clone(),
                                            expr_to_json(inner, variant_t, opts)?,
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
        (Type::App(_, _), rhs) => match want.as_applied_type() {
            Some(AppliedType::Result(err_type, ok_type)) => match rhs {
                Expr::Named(_, n, Some(v)) => {
                    let mut map: Map<String, Value> = Map::new();
                    match n.as_str() {
                        "Ok" => {
                            map.insert("Ok".to_string(), expr_to_json(v, ok_type, opts)?);
                            Ok(Value::Object(map))
                        }
                        "Err" => {
                            map.insert("Err".to_string(), expr_to_json(v, err_type, opts)?);
                            Ok(Value::Object(map))
                        }
                        _ => Err(expr_error(expr, want)),
                    }
                }
                _ => Err(expr_error(expr, want)),
            },
            Some(AppliedType::Promise(_)) => match rhs {
                Expr::Promise(_, u) => {
                    Ok(serde_json::to_value(u).map_err(|_| expr_error(expr, want))?)
                }
                _ => Err(expr_error(expr, want)),
            },
            Some(AppliedType::Option(inner_type)) => match rhs {
                Expr::Named(_, n, ov) => match (n.as_str(), ov) {
                    ("Some", Some(v)) => expr_to_json(v, inner_type, opts),
                    ("None", None) => Ok(Value::Null),
                    _ => Err(expr_error(expr, want)),
                },
                _ => Err(expr_error(expr, want)),
            },
            Some(AppliedType::List(inner_type)) => match rhs {
                Expr::List(_, items) => {
                    let mut values: Vec<Value> = Vec::new();
                    for item in items.iter() {
                        values.push(expr_to_json(item, inner_type, opts)?);
                    }
                    Ok(Value::Array(values))
                }
                _ => Err(expr_error(expr, want)),
            },
            _ => Err(expr_error(expr, want)),
        },
        (Type::Dict(type_entries), Expr::Dict(_, value_entries))
            if type_entries.len() == value_entries.len() =>
        {
            let mut map: Map<String, Value> = Map::new();
            for (k, t) in type_entries.iter() {
                let v = value_entries.get(k).ok_or_else(|| expr_error(expr, want))?;
                map.insert(k.clone(), expr_to_json(v, t, opts)?);
            }
            Ok(Value::Object(map))
        }
        (Type::Tuple(inner_types), Expr::Tuple(_, items)) if inner_types.len() == items.len() => {
            let mut values: Vec<Value> = Vec::new();
            for i in 0..inner_types.len() {
                values.push(expr_to_json(&items[i], &inner_types[i], opts)?);
            }
            Ok(Value::Array(values))
        }
        (Type::Con(TypeCon::Bool), Expr::Bool(_, b)) => Ok(Value::Bool(*b)),
        (Type::Con(TypeCon::Uint), Expr::Uint(_, x)) => Ok(Value::Number((*x).into())),
        (Type::Con(TypeCon::Int), Expr::Int(_, x)) => Ok(Value::Number((*x).into())),
        (Type::Con(TypeCon::Float), Expr::Float(_, x)) => match Number::from_f64(*x) {
            Some(n) => Ok(Value::Number(n)),
            None => Err(expr_error(expr, want)),
        },
        (Type::Con(TypeCon::String), Expr::String(_, x)) => Ok(Value::String(x.clone())),
        (Type::Con(TypeCon::Uuid), Expr::Uuid(_, u)) => {
            Ok(serde_json::to_value(u).map_err(|_| expr_error(expr, want))?)
        }
        (Type::Con(TypeCon::DateTime), Expr::DateTime(_, dt)) => {
            Ok(serde_json::to_value(dt).map_err(|_| expr_error(expr, want))?)
        }
        _ => Err(expr_error(expr, want)),
    }
}

fn type_error(json: &Value, want: &Arc<Type>) -> EngineError {
    EngineError::ExpectedTypeGotJSON {
        expected: want.clone(),
        got: json.clone(),
        trace: Default::default(),
    }
}

fn expr_error(expr: &Arc<Expr>, want: &Arc<Type>) -> EngineError {
    EngineError::ExpectedTypeGotValue {
        expected: want.clone(),
        got: expr.clone(),
        trace: Default::default(),
    }
}
