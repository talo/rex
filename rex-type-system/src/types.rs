use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use chrono::{DateTime, Utc};
use rex_ast::id::Id;
use uuid::Uuid;

pub type TypeEnv = HashMap<String, Arc<Type>>;

pub type ExprTypeEnv = HashMap<Id, Arc<Type>>;

#[derive(Debug, Eq, Hash, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Type {
    UnresolvedVar(String),
    Var(Id),
    ForAll(Id, Arc<Type>, BTreeSet<Id>),

    ADT(ADT),
    Arrow(Arc<Type>, Arc<Type>),
    Result(Arc<Type>, Arc<Type>),
    Option(Arc<Type>),
    Promise(Arc<Type>),
    List(Arc<Type>),
    Dict(BTreeMap<String, Arc<Type>>),
    Tuple(Vec<Arc<Type>>),

    Bool,
    Uint,
    Int,
    Float,
    String,
    Uuid,
    DateTime,
}

impl Type {
    pub fn build_arrow(params: Vec<Arc<Type>>, ret: Arc<Type>) -> Arc<Type> {
        params
            .into_iter()
            .rev()
            .fold(ret, |acc, t| Arc::new(Type::Arrow(t, acc)))
    }

    pub fn num_params(&self) -> usize {
        match self {
            Type::Arrow(_, b) => 1 + b.num_params(),
            _ => 0,
        }
    }

    pub fn evaluated_type(&self) -> &Type {
        match self {
            Type::Arrow(_, b) => b.evaluated_type(),
            _ => self,
        }
    }

    pub fn resolve_vars(&self, assignments: &HashMap<String, Arc<Type>>) -> Arc<Type> {
        match self {
            Type::UnresolvedVar(x) => {
                if let Some(t) = assignments.get(x) {
                    t.clone()
                } else {
                    Arc::new(Type::UnresolvedVar(x.clone()))
                }
            }
            Type::Var(v) => Arc::new(Type::Var(v.clone())),
            Type::ForAll(id, t, ids) => Arc::new(Type::ForAll(
                id.clone(),
                t.resolve_vars(assignments),
                ids.clone(),
            )),
            Type::ADT(adt) => Arc::new(Type::ADT(ADT {
                name: adt.name.clone(),
                variants: adt
                    .variants
                    .iter()
                    .map(|variant| ADTVariant {
                        name: variant.name.clone(),
                        t: variant.t.as_ref().map(|t| t.resolve_vars(assignments)),
                        docs: variant.docs.clone(),
                        t_docs: variant.t_docs.clone(),
                    })
                    .collect(),
                docs: adt.docs.clone(),
            })),
            Type::Arrow(a, b) => Arc::new(Type::Arrow(
                a.resolve_vars(assignments),
                b.resolve_vars(assignments),
            )),
            Type::Result(a, b) => Arc::new(Type::Result(
                a.resolve_vars(assignments),
                b.resolve_vars(assignments),
            )),
            Type::Option(t) => Arc::new(Type::Option(t.resolve_vars(assignments))),
            Type::Promise(t) => Arc::new(Type::Promise(t.resolve_vars(assignments))),
            Type::List(t) => Arc::new(Type::List(t.resolve_vars(assignments))),
            Type::Dict(xs) => Arc::new(Type::Dict(BTreeMap::from_iter(
                xs.iter()
                    .map(|(k, v)| (k.clone(), v.resolve_vars(assignments))),
            ))),
            Type::Tuple(xs) => Arc::new(Type::Tuple(
                xs.iter().map(|x| x.resolve_vars(assignments)).collect(),
            )),
            Type::Bool => Arc::new(Type::Bool),
            Type::Uint => Arc::new(Type::Uint),
            Type::Int => Arc::new(Type::Int),
            Type::Float => Arc::new(Type::Float),
            Type::String => Arc::new(Type::String),
            Type::Uuid => Arc::new(Type::Uuid),
            Type::DateTime => Arc::new(Type::DateTime),
        }
    }

    pub fn unresolved_vars(&self) -> HashSet<String> {
        let mut set = HashSet::new();
        self.unresolved_vars_accum(&mut set);
        set
    }

    pub fn maybe_compatible(&self, other: &Type) -> Result<(), String> {
        match (self, other) {
            (Self::Bool, Self::Bool) => Ok(()),
            (Self::Uint, Self::Uint) => Ok(()),
            (Self::Int, Self::Int) => Ok(()),
            (Self::Float, Self::Float) => Ok(()),
            (Self::String, Self::String) => Ok(()),
            (Self::Uuid, Self::Uuid) => Ok(()),
            (Self::DateTime, Self::DateTime) => Ok(()),

            (Self::Arrow(a1, b1), Self::Arrow(a2, b2)) => {
                a1.maybe_compatible(a2)?;
                b1.maybe_compatible(b2)
            }
            (Self::Result(t1, e1), Self::Result(t2, e2)) => {
                t1.maybe_compatible(t2)?;
                e1.maybe_compatible(e2)
            }
            (Self::Option(t1), Self::Option(t2)) => t1.maybe_compatible(t2),
            (Self::Promise(t1), Self::Promise(t2)) => t1.maybe_compatible(t2),
            (Self::List(t1), Self::List(t2)) => t1.maybe_compatible(t2),
            (Self::Dict(d1), Self::Dict(d2)) => {
                for (k, v1) in d1 {
                    if let Some(v2) = d2.get(k) {
                        v1.maybe_compatible(v2)?;
                    } else {
                        return Err(format!("Incompatible types: {} and {}", self, other));
                    }
                }
                Ok(())
            }
            (Self::Tuple(e1), Self::Tuple(e2)) => {
                if e1.len() != e2.len() {
                    return Err(format!("Incompatible types: {} and {}", self, other));
                }
                for (t1, t2) in e1.iter().zip(e2) {
                    t1.maybe_compatible(t2)?;
                }
                Ok(())
            }

            (Self::ADT(adt1), Self::ADT(adt2)) => {
                if adt1.name != adt2.name {
                    return Err(format!("Incompatible types: {} and {}", self, other));
                }
                if adt1.variants.len() != adt2.variants.len() {
                    return Err(format!("Incompatible types: {} and {}", self, other));
                }
                for (v1, v2) in adt1.variants.iter().zip(&adt2.variants) {
                    if v1.name != v2.name {
                        return Err(format!("Incompatible types: {} and {}", self, other));
                    }
                    if let (Some(t1), Some(t2)) = (&v1.t, &v2.t) {
                        t1.maybe_compatible(t2)?;
                    }
                }
                Ok(())
            }

            // NOTE(loong): I am not sure this is actually correct. My thinking
            // is that we don't have to validate inconsistencies in type
            // assigments for the various type variables, because this should
            // have been handled during type inference. Type compatibility
            // checking should only be used for looking up functions during
            // execution. For example, `a -> a` and `int -> string` are
            // compatible because we have said that a type variable is always
            // compatible with any other type (without considering constraints).
            //
            // We can think about "type compatibility" as a necessary but not
            // sufficient condition for type correctness. This check could be
            // used before full type inference to catch any quick and obvious
            // errors. And then again, after type inference, to disambiguate
            // overloaded functions in the ftable.
            //
            // This is needed because the ftable will store a function like `map
            // : (a -> b) -> [a] -> [b]` but during actual execution this will
            // be executed like something concrete `map : (int -> string) ->
            // [int] -> [string]`.
            //
            // NOTE(loong): We do not support overloaded parametric
            // polymorphism.
            //
            // TODO(loong): We should not allow unresolved type variables at
            // this point. Type variable resolution should have already
            // happened, and unresolved type variables should be compatible with
            // nothing.
            (Self::UnresolvedVar(_), _) => Ok(()),
            (Self::Var(_), _) => Ok(()),
            (Self::ForAll(_, t, _), _) => t.maybe_compatible(other),
            (_, Self::UnresolvedVar(_)) => Ok(()),
            (_, Self::Var(_)) => Ok(()),
            (_, Self::ForAll(_, t, _)) => t.maybe_compatible(other),

            _ => Err(format!("Incompatible types: {} and {}", self, other)),
        }
    }

    fn unresolved_vars_accum(&self, set: &mut HashSet<String>) {
        match self {
            Type::UnresolvedVar(x) => {
                set.insert(x.clone());
            }
            Type::Var(_) => {}
            Type::ForAll(_, t, _) => t.unresolved_vars_accum(set),
            Type::ADT(adt) => {
                for variant in &adt.variants {
                    if let Some(t) = &variant.t {
                        t.unresolved_vars_accum(set);
                    }
                }
            }
            Type::Arrow(a, b) => {
                a.unresolved_vars_accum(set);
                b.unresolved_vars_accum(set);
            }
            Type::Result(a, b) => {
                a.unresolved_vars_accum(set);
                b.unresolved_vars_accum(set);
            }
            Type::Option(t) => t.unresolved_vars_accum(set),
            Type::Promise(t) => t.unresolved_vars_accum(set),
            Type::List(t) => t.unresolved_vars_accum(set),
            Type::Dict(xs) => {
                for (_, v) in xs {
                    v.unresolved_vars_accum(set);
                }
            }
            Type::Tuple(xs) => {
                for x in xs {
                    x.unresolved_vars_accum(set);
                }
            }
            Type::Bool => {}
            Type::Uint => {}
            Type::Int => {}
            Type::Float => {}
            Type::String => {}
            Type::Uuid => {}
            Type::DateTime => {}
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => "bool".fmt(f),
            Type::Uint => "uint".fmt(f),
            Type::Int => "int".fmt(f),
            Type::Float => "float".fmt(f),
            Type::String => "string".fmt(f),
            Type::Uuid => "uuid".fmt(f),
            Type::DateTime => "datetime".fmt(f),
            Type::Option(x) => {
                "Option (".fmt(f)?;
                x.fmt(f)?;
                ')'.fmt(f)
            }
            Type::Promise(x) => {
                "Promise (".fmt(f)?;
                x.fmt(f)?;
                ')'.fmt(f)
            }
            Type::Result(a, b) => {
                "Result (".fmt(f)?;
                a.fmt(f)?;
                ") (".fmt(f)?;
                b.fmt(f)?;
                ')'.fmt(f)
            }
            Type::Tuple(xs) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ')'.fmt(f)
            }
            Type::Dict(xs) => {
                '{'.fmt(f)?;
                for (i, (k, v)) in xs.iter().enumerate() {
                    k.fmt(f)?;
                    " = ".fmt(f)?;
                    v.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                '}'.fmt(f)
            }
            Type::Arrow(a, b) => {
                match a.as_ref() {
                    Type::Arrow(_, _) => {
                        '('.fmt(f)?;
                        a.fmt(f)?;
                        ')'.fmt(f)?;
                    }
                    _ => a.fmt(f)?,
                }
                " → ".fmt(f)?;
                b.fmt(f)
            }
            Type::UnresolvedVar(x) => {
                'τ'.fmt(f)?;
                x.fmt(f)
            }
            Type::Var(x) => {
                'τ'.fmt(f)?;
                x.fmt(f)
            }
            Type::ForAll(x, t, deps) => {
                "∀τ".fmt(f)?;
                x.fmt(f)?;
                ". ".fmt(f)?;
                t.fmt(f)?;
                if !deps.is_empty() {
                    " with {".fmt(f)?;
                    for (i, dep) in deps.iter().enumerate() {
                        dep.fmt(f)?;
                        if i + 1 < deps.len() {
                            ", ".fmt(f)?;
                        }
                    }
                    '}'.fmt(f)?;
                }
                Ok(())
            }
            Type::List(x) => {
                '['.fmt(f)?;
                x.fmt(f)?;
                ']'.fmt(f)
            }
            Type::ADT(x) => x.fmt(f),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct ADT {
    pub name: String,
    pub variants: Vec<ADTVariant>,
    pub docs: Option<String>,
}

impl Display for ADT {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if !self.variants.is_empty() {
            " ∈ ".fmt(f)?;
            for (i, v) in self.variants.iter().enumerate() {
                v.fmt(f)?;
                if i + 1 < self.variants.len() {
                    " | ".fmt(f)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct ADTVariant {
    pub name: String,
    pub t: Option<Arc<Type>>,
    pub docs: Option<String>,
    pub t_docs: Option<BTreeMap<String, String>>,
}

impl Display for ADTVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if let Some(t) = &self.t {
            " (".fmt(f)?;
            t.fmt(f)?;
            ')'.fmt(f)?;
        }
        Ok(())
    }
}

pub trait ToType {
    fn to_type() -> Type;
}

impl ToType for bool {
    fn to_type() -> Type {
        Type::Bool
    }
}

impl ToType for u8 {
    fn to_type() -> Type {
        Type::Uint
    }
}

impl ToType for u16 {
    fn to_type() -> Type {
        Type::Uint
    }
}

impl ToType for u32 {
    fn to_type() -> Type {
        Type::Uint
    }
}

impl ToType for u64 {
    fn to_type() -> Type {
        Type::Uint
    }
}

impl ToType for u128 {
    fn to_type() -> Type {
        Type::Uint
    }
}

impl ToType for i8 {
    fn to_type() -> Type {
        Type::Int
    }
}

impl ToType for i16 {
    fn to_type() -> Type {
        Type::Int
    }
}

impl ToType for i32 {
    fn to_type() -> Type {
        Type::Int
    }
}

impl ToType for i64 {
    fn to_type() -> Type {
        Type::Int
    }
}

impl ToType for i128 {
    fn to_type() -> Type {
        Type::Int
    }
}

impl ToType for f32 {
    fn to_type() -> Type {
        Type::Float
    }
}

impl ToType for f64 {
    fn to_type() -> Type {
        Type::Float
    }
}

impl ToType for str {
    fn to_type() -> Type {
        Type::String
    }
}

impl ToType for String {
    fn to_type() -> Type {
        Type::String
    }
}

impl ToType for Uuid {
    fn to_type() -> Type {
        Type::Uuid
    }
}

impl ToType for DateTime<Utc> {
    fn to_type() -> Type {
        Type::DateTime
    }
}

impl<B> ToType for fn() -> B
where
    B: ToType,
{
    fn to_type() -> Type {
        B::to_type()
    }
}

impl<A0, B> ToType for fn(A0) -> B
where
    A0: ToType,
    B: ToType,
{
    fn to_type() -> Type {
        Type::Arrow(Arc::new(A0::to_type()), Arc::new(B::to_type()))
    }
}

impl<A0, A1, B> ToType for fn(A0, A1) -> B
where
    A0: ToType,
    A1: ToType,
    B: ToType,
{
    fn to_type() -> Type {
        Type::Arrow(
            Arc::new(A0::to_type()),
            Arc::new(Type::Arrow(Arc::new(A1::to_type()), Arc::new(B::to_type()))),
        )
    }
}

impl<A0, A1, A2, B> ToType for fn(A0, A1, A2) -> B
where
    A0: ToType,
    A1: ToType,
    A2: ToType,
    B: ToType,
{
    fn to_type() -> Type {
        Type::Arrow(
            Arc::new(A0::to_type()),
            Arc::new(Type::Arrow(
                Arc::new(A1::to_type()),
                Arc::new(Type::Arrow(Arc::new(A2::to_type()), Arc::new(B::to_type()))),
            )),
        )
    }
}

impl<A0, A1, A2, A3, B> ToType for fn(A0, A1, A2, A3) -> B
where
    A0: ToType,
    A1: ToType,
    A2: ToType,
    A3: ToType,
    B: ToType,
{
    fn to_type() -> Type {
        Type::Arrow(
            Arc::new(A0::to_type()),
            Arc::new(Type::Arrow(
                Arc::new(A1::to_type()),
                Arc::new(Type::Arrow(
                    Arc::new(A2::to_type()),
                    Arc::new(Type::Arrow(Arc::new(A3::to_type()), Arc::new(B::to_type()))),
                )),
            )),
        )
    }
}

impl<T, E> ToType for Result<T, E>
where
    T: ToType,
    E: ToType,
{
    fn to_type() -> Type {
        Type::Result(Arc::new(T::to_type()), Arc::new(E::to_type()))
    }
}

impl<T> ToType for Option<T>
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::Option(Arc::new(T::to_type()))
    }
}

impl<T> ToType for [T]
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::List(Arc::new(T::to_type()))
    }
}

impl<T> ToType for Vec<T>
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::List(Arc::new(T::to_type()))
    }
}

impl<T> ToType for VecDeque<T>
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::List(Arc::new(T::to_type()))
    }
}

impl ToType for () {
    fn to_type() -> Type {
        Type::Tuple(vec![])
    }
}

impl<T0> ToType for (T0,)
where
    T0: ToType,
{
    fn to_type() -> Type {
        Type::Tuple(vec![Arc::new(T0::to_type())])
    }
}

impl<T0, T1> ToType for (T0, T1)
where
    T0: ToType,
    T1: ToType,
{
    fn to_type() -> Type {
        Type::Tuple(vec![Arc::new(T0::to_type()), Arc::new(T1::to_type())])
    }
}

impl<T0, T1, T2> ToType for (T0, T1, T2)
where
    T0: ToType,
    T1: ToType,
    T2: ToType,
{
    fn to_type() -> Type {
        Type::Tuple(vec![
            Arc::new(T0::to_type()),
            Arc::new(T1::to_type()),
            Arc::new(T2::to_type()),
        ])
    }
}

impl<T0, T1, T2, T3> ToType for (T0, T1, T2, T3)
where
    T0: ToType,
    T1: ToType,
    T2: ToType,
    T3: ToType,
{
    fn to_type() -> Type {
        Type::Tuple(vec![
            Arc::new(T0::to_type()),
            Arc::new(T1::to_type()),
            Arc::new(T2::to_type()),
            Arc::new(T3::to_type()),
        ])
    }
}

impl<T0, T1, T2, T3, T4> ToType for (T0, T1, T2, T3, T4)
where
    T0: ToType,
    T1: ToType,
    T2: ToType,
    T3: ToType,
    T4: ToType,
{
    fn to_type() -> Type {
        Type::Tuple(vec![
            Arc::new(T0::to_type()),
            Arc::new(T1::to_type()),
            Arc::new(T2::to_type()),
            Arc::new(T3::to_type()),
            Arc::new(T4::to_type()),
        ])
    }
}

impl<T0, T1, T2, T3, T4, T5> ToType for (T0, T1, T2, T3, T4, T5)
where
    T0: ToType,
    T1: ToType,
    T2: ToType,
    T3: ToType,
    T4: ToType,
    T5: ToType,
{
    fn to_type() -> Type {
        Type::Tuple(vec![
            Arc::new(T0::to_type()),
            Arc::new(T1::to_type()),
            Arc::new(T2::to_type()),
            Arc::new(T3::to_type()),
            Arc::new(T4::to_type()),
            Arc::new(T5::to_type()),
        ])
    }
}

impl<T0, T1, T2, T3, T4, T5, T6> ToType for (T0, T1, T2, T3, T4, T5, T6)
where
    T0: ToType,
    T1: ToType,
    T2: ToType,
    T3: ToType,
    T4: ToType,
    T5: ToType,
    T6: ToType,
{
    fn to_type() -> Type {
        Type::Tuple(vec![
            Arc::new(T0::to_type()),
            Arc::new(T1::to_type()),
            Arc::new(T2::to_type()),
            Arc::new(T3::to_type()),
            Arc::new(T4::to_type()),
            Arc::new(T5::to_type()),
            Arc::new(T6::to_type()),
        ])
    }
}

impl<T0, T1, T2, T3, T4, T5, T6, T7> ToType for (T0, T1, T2, T3, T4, T5, T6, T7)
where
    T0: ToType,
    T1: ToType,
    T2: ToType,
    T3: ToType,
    T4: ToType,
    T5: ToType,
    T6: ToType,
    T7: ToType,
{
    fn to_type() -> Type {
        Type::Tuple(vec![
            Arc::new(T0::to_type()),
            Arc::new(T1::to_type()),
            Arc::new(T2::to_type()),
            Arc::new(T3::to_type()),
            Arc::new(T4::to_type()),
            Arc::new(T5::to_type()),
            Arc::new(T6::to_type()),
            Arc::new(T7::to_type()),
        ])
    }
}
