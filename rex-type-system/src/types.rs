use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use chrono::{DateTime, Utc};
use rex_ast::{expr::Expr, id::Id};
use uuid::Uuid;

pub type TypeEnv = HashMap<String, Arc<Type>>;

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

    pub fn evaluated_type(&self) -> &Type {
        match self {
            Type::Arrow(_, b) => b.evaluated_type(),
            _ => self,
        }
    }

    pub fn resolve_vars(&self, assignments: &HashMap<String, Arc<Type>>) -> Arc<Type> {
        self.transform(|t| {
            if let Type::UnresolvedVar(x) = t {
                if let Some(t) = assignments.get(x) {
                    return Some(t.clone());
                }
            }
            None
        })
    }

    pub fn unresolved_vars(&self) -> HashSet<String> {
        let mut set = HashSet::new();
        self.for_each(|t| {
            if let Type::UnresolvedVar(x) = t {
                set.insert(x.clone());
            }
        });
        set
    }

    fn maybe_compatible(&self, other: &Expr) -> bool {
        // This function is conservative; we only we return false if we're certain
        // there's no match.
        if matches!(other, Expr::Var(..)) {
            return true; // could refer to anything
        }

        match self {
            Type::UnresolvedVar(_) => true,
            Type::Var(_) => true,
            Type::ForAll(_, _, _) => true,
            Type::ADT(adt) => match other {
                Expr::Named(_, n, _) => {
                    for variant in adt.variants.iter() {
                        if *n == variant.name {
                            return true;
                        }
                    }
                    false
                }
                _ => false,
            },
            Type::Arrow(_, _) => true,
            Type::Result(_, _) => match other {
                Expr::Named(_, n, _) => n == "Ok" || n == "Err",
                _ => false,
            },
            Type::Option(_) => match other {
                Expr::Named(_, n, _) => n == "Some" || n == "None",
                _ => false,
            },
            Type::Promise(_) => matches!(other, Expr::Promise(..)),
            Type::List(t) => match other {
                Expr::List(_, es) => es.iter().all(|e| t.maybe_compatible(e)),
                _ => false,
            },
            Type::Dict(_) => {
                true // TODO
            }
            Type::Tuple(types) => match other {
                Expr::Tuple(_, exprs) => {
                    types.len() == exprs.len()
                        && types
                            .iter()
                            .zip(exprs.iter())
                            .all(|(t, e)| t.maybe_compatible(e))
                }
                _ => false,
            },
            Type::Bool => matches!(other, Expr::Bool(..)),
            Type::Uint => matches!(other, Expr::Uint(..)),
            Type::Int => matches!(other, Expr::Int(..)),
            Type::Float => matches!(other, Expr::Float(..)),
            Type::String => matches!(other, Expr::String(..)),
            Type::Uuid => matches!(other, Expr::Uuid(..)),
            Type::DateTime => matches!(other, Expr::DateTime(..)),
        }
    }

    pub fn for_each<F>(&self, mut f: F) -> Arc<Type>
    where
        F: FnMut(&Type),
    {
        self.transform(|t| {
            f(t);
            None
        })
    }

    pub fn transform<F>(&self, mut f: F) -> Arc<Type>
    where
        F: FnMut(&Type) -> Option<Arc<Type>>,
    {
        self.transform_ref(&mut f)
    }

    // Separate function to avoid "reached the recursion limit while instantiating" errors
    fn transform_ref<F>(&self, f: &mut F) -> Arc<Type>
    where
        F: FnMut(&Type) -> Option<Arc<Type>>,
    {
        if let Some(repl) = f(self) {
            return repl;
        }

        match self {
            Type::UnresolvedVar(x) => Arc::new(Type::UnresolvedVar(x.clone())),
            Type::Var(v) => Arc::new(Type::Var(v.clone())),
            Type::ForAll(id, t, ids) => {
                Arc::new(Type::ForAll(id.clone(), t.transform_ref(f), ids.clone()))
            }
            Type::ADT(adt) => Arc::new(Type::ADT(ADT {
                name: adt.name.clone(),
                variants: adt
                    .variants
                    .iter()
                    .map(|variant| ADTVariant {
                        name: variant.name.clone(),
                        t: variant.t.as_ref().map(|t| t.transform_ref(f)),
                        docs: variant.docs.clone(),
                        t_docs: variant.t_docs.clone(),
                    })
                    .collect(),
                docs: adt.docs.clone(),
            })),
            Type::Arrow(a, b) => Arc::new(Type::Arrow(a.transform_ref(f), b.transform_ref(f))),
            Type::Result(a, b) => Arc::new(Type::Result(a.transform_ref(f), b.transform_ref(f))),
            Type::Option(t) => Arc::new(Type::Option(t.transform_ref(f))),
            Type::Promise(t) => Arc::new(Type::Promise(t.transform_ref(f))),
            Type::List(t) => Arc::new(Type::List(t.transform_ref(f))),
            Type::Dict(xs) => Arc::new(Type::Dict(BTreeMap::from_iter(
                xs.iter().map(|(k, v)| (k.clone(), v.transform_ref(f))),
            ))),
            Type::Tuple(xs) => {
                Arc::new(Type::Tuple(xs.iter().map(|x| x.transform_ref(f)).collect()))
            }
            Type::Bool => Arc::new(Type::Bool),
            Type::Uint => Arc::new(Type::Uint),
            Type::Int => Arc::new(Type::Int),
            Type::Float => Arc::new(Type::Float),
            Type::String => Arc::new(Type::String),
            Type::Uuid => Arc::new(Type::Uuid),
            Type::DateTime => Arc::new(Type::DateTime),
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

pub trait Dispatch: Display {
    fn num_params(&self) -> usize;
    fn maybe_accepts_args(&self, args: &[Expr]) -> bool;
}

impl Dispatch for Type {
    fn num_params(&self) -> usize {
        match self {
            Type::Arrow(_, b) => 1 + b.num_params(),
            _ => 0,
        }
    }

    fn maybe_accepts_args(&self, args: &[Expr]) -> bool {
        match self {
            Type::Arrow(a, b) => {
                args.len() >= 1 && a.maybe_compatible(&args[0]) && b.maybe_accepts_args(&args[1..])
            }
            _ => args.len() == 0,
        }
    }
}

impl Dispatch for Arc<Type> {
    fn num_params(&self) -> usize {
        let t: &Type = &**self;
        t.num_params()
    }

    fn maybe_accepts_args(&self, args: &[Expr]) -> bool {
        let t: &Type = &**self;
        t.maybe_accepts_args(args)
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

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::{bool, float, string, uint};
    use rex_ast::{b, f, s, u};

    #[test]
    fn test_dispatch() {
        // uint → string → float → bool
        let t = Type::build_arrow(vec![uint!(), string!(), float!()], bool!());

        // Correct number and types of arguments
        assert!(t.maybe_accepts_args(&[u!(4), s!("Hello"), f!(2.5)]));

        // Too few arguments
        assert!(!t.maybe_accepts_args(&[u!(4), s!("Hello")]));

        // Too many arguments
        assert!(!t.maybe_accepts_args(&[u!(4), s!("Hello"), f!(2.5), b!(true)]));

        // First argument doesn't match
        assert!(!t.maybe_accepts_args(&[f!(4.0), s!("Hello"), f!(2.5)]));

        // Second argument doesn't match
        assert!(!t.maybe_accepts_args(&[u!(4), b!(true), f!(2.5)]));

        // Third argument doesn't match
        assert!(!t.maybe_accepts_args(&[u!(4), s!("Hello"), u!(2)]));
    }
}
