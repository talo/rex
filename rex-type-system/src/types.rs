use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use chrono::{DateTime, Utc};
use rex_ast::{expr::Expr, id::Id};
use uuid::Uuid;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct TypeScheme {
    pub ids: Vec<Id>,
    pub ty: Arc<Type>,
    pub deps: BTreeSet<Id>,
}

impl TypeScheme {
    pub fn maybe_overlaps_with(&self, other: &TypeScheme) -> bool {
        // We can ignore type variables hwere, since any two variables are considered
        // potentially overlapping
        self.ty.maybe_overlaps_with(&other.ty)
    }
}

impl From<Arc<Type>> for TypeScheme {
    fn from(t: Arc<Type>) -> Self {
        TypeScheme {
            ids: vec![],
            ty: t,
            deps: BTreeSet::new(),
        }
    }
}

pub type TypeEnv = HashMap<String, HashSet<TypeScheme>>;

#[derive(Eq, PartialEq, Debug)]
pub enum AppliedType<'a> {
    Arrow(&'a Arc<Type>, &'a Arc<Type>),
    Result(&'a Arc<Type>, &'a Arc<Type>),
    Option(&'a Arc<Type>),
    Promise(&'a Arc<Type>),
    List(&'a Arc<Type>),
}

#[derive(Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub enum TypeCon {
    Arrow,    // 2 args
    Result,   // 2 args
    Option,   // 1 arg
    Promise,  // 1 arg
    List,     // 1 arg
    Bool,     // 0 args
    Uint,     // 0 args
    Int,      // 0 args
    Float,    // 0 args
    String,   // 0 args
    Uuid,     // 0 args
    DateTime, // 0 args
}

impl fmt::Display for TypeCon {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TypeCon::Arrow => "Arrow".fmt(f),
            TypeCon::List => "List".fmt(f),
            TypeCon::Option => "Option".fmt(f),
            TypeCon::Promise => "Promise".fmt(f),
            TypeCon::Result => "Result".fmt(f),
            TypeCon::Bool => "bool".fmt(f),
            TypeCon::Uint => "uint".fmt(f),
            TypeCon::Int => "int".fmt(f),
            TypeCon::Float => "float".fmt(f),
            TypeCon::String => "string".fmt(f),
            TypeCon::Uuid => "uuid".fmt(f),
            TypeCon::DateTime => "datetime".fmt(f),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Type {
    UnresolvedVar(String),
    Var(Id),

    ADT(ADT),
    App(Arc<Type>, Arc<Type>),
    Dict(BTreeMap<String, Arc<Type>>),
    Tuple(Vec<Arc<Type>>),
    Con(TypeCon),
}

impl Type {
    pub fn make_arrow(a: Arc<Type>, b: Arc<Type>) -> Type {
        Type::App(
            Arc::new(Type::App(Arc::new(Type::Con(TypeCon::Arrow)), a)),
            b,
        )
    }

    pub fn make_result(a: Arc<Type>, b: Arc<Type>) -> Type {
        Type::App(
            Arc::new(Type::App(Arc::new(Type::Con(TypeCon::Result)), a)),
            b,
        )
    }

    pub fn build_arrow(params: Vec<Arc<Type>>, ret: Arc<Type>) -> Arc<Type> {
        params
            .into_iter()
            .rev()
            .fold(ret, |acc, t| Arc::new(Type::make_arrow(t, acc)))
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
            Type::App(_, _) => match self.as_applied_type() {
                Some(AppliedType::Arrow(_, _)) => true,
                Some(AppliedType::Result(_, _)) => match other {
                    Expr::Named(_, n, _) => n == "Ok" || n == "Err",
                    _ => false,
                },
                Some(AppliedType::Option(_)) => match other {
                    Expr::Named(_, n, _) => n == "Some" || n == "None",
                    _ => false,
                },
                Some(AppliedType::Promise(_)) => {
                    matches!(other, Expr::Promise(..))
                }
                Some(AppliedType::List(t)) => match other {
                    Expr::List(_, es) => es.iter().all(|e| t.maybe_compatible(e)),
                    _ => false,
                },
                None => true,
            },
            Type::Con(TypeCon::Arrow) => true,
            Type::Con(TypeCon::Result) => true,
            Type::Con(TypeCon::Option) => true,
            Type::Con(TypeCon::Promise) => true,
            Type::Con(TypeCon::List) => true,
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
            Type::Con(TypeCon::Bool) => matches!(other, Expr::Bool(..)),
            Type::Con(TypeCon::Uint) => matches!(other, Expr::Uint(..)),
            Type::Con(TypeCon::Int) => matches!(other, Expr::Int(..)),
            Type::Con(TypeCon::Float) => matches!(other, Expr::Float(..)),
            Type::Con(TypeCon::String) => matches!(other, Expr::String(..)),
            Type::Con(TypeCon::Uuid) => matches!(other, Expr::Uuid(..)),
            Type::Con(TypeCon::DateTime) => matches!(other, Expr::DateTime(..)),
        }
    }

    pub fn as_applied_type(&self) -> Option<AppliedType<'_>> {
        match self {
            Type::App(a, b) => match &**a {
                Type::App(c, d) => match &**c {
                    Type::Con(TypeCon::Arrow) => Some(AppliedType::Arrow(d, b)),
                    Type::Con(TypeCon::Result) => Some(AppliedType::Result(d, b)),
                    _ => None,
                },
                Type::Con(TypeCon::List) => Some(AppliedType::List(b)),
                Type::Con(TypeCon::Option) => Some(AppliedType::Option(b)),
                Type::Con(TypeCon::Promise) => Some(AppliedType::Promise(b)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn maybe_overlaps_with(&self, other: &Type) -> bool {
        // This function is conservative; we only we return false if we're certain
        // there's no match.
        if matches!(other, Type::Var(..)) {
            return true; // could refer to anything
        }
        if matches!(other, Type::UnresolvedVar(..)) {
            return true; // could refer to anything
        }

        match self {
            Type::UnresolvedVar(_) => true,
            Type::Var(_) => true,
            Type::ADT(adt1) => match other {
                Type::ADT(adt2) => adt1.name == adt2.name,
                _ => false,
            },
            Type::App(_, _) => match self.as_applied_type() {
                Some(AppliedType::Arrow(arg1, res1)) => {
                    if let Some(AppliedType::Arrow(arg2, res2)) = other.as_applied_type() {
                        arg1.maybe_overlaps_with(arg2) && res1.maybe_overlaps_with(res2)
                    } else {
                        false
                    }
                }
                Some(AppliedType::Result(err1, ok1)) => {
                    if let Some(AppliedType::Result(err2, ok2)) = other.as_applied_type() {
                        err1.maybe_overlaps_with(err2) && ok1.maybe_overlaps_with(ok2)
                    } else {
                        false
                    }
                }
                Some(AppliedType::Option(lhs)) => {
                    if let Some(AppliedType::Option(rhs)) = other.as_applied_type() {
                        lhs.maybe_overlaps_with(rhs)
                    } else {
                        false
                    }
                }
                Some(AppliedType::Promise(lhs)) => {
                    if let Some(AppliedType::Promise(rhs)) = other.as_applied_type() {
                        lhs.maybe_overlaps_with(rhs)
                    } else {
                        false
                    }
                }
                Some(AppliedType::List(lhs)) => {
                    if let Some(AppliedType::List(rhs)) = other.as_applied_type() {
                        lhs.maybe_overlaps_with(rhs)
                    } else {
                        false
                    }
                }
                None => true,
            },
            Type::Dict(_) => {
                true // TODO
            }
            Type::Tuple(lhs) => match other {
                Type::Tuple(rhs) => {
                    lhs.len() == rhs.len()
                        && lhs
                            .iter()
                            .zip(rhs.iter())
                            .all(|(l, r)| l.maybe_overlaps_with(r))
                }
                _ => false,
            },
            Type::Con(c) => *other == Type::Con(*c),
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
            Type::Var(v) => Arc::new(Type::Var(*v)),
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
                        discriminant: variant.discriminant,
                    })
                    .collect(),
                docs: adt.docs.clone(),
            })),
            Type::App(a, b) => Arc::new(Type::App(a.transform_ref(f), b.transform_ref(f))),
            Type::Dict(xs) => Arc::new(Type::Dict(BTreeMap::from_iter(
                xs.iter().map(|(k, v)| (k.clone(), v.transform_ref(f))),
            ))),
            Type::Tuple(xs) => {
                Arc::new(Type::Tuple(xs.iter().map(|x| x.transform_ref(f)).collect()))
            }
            Type::Con(c) => Arc::new(Type::Con(*c)),
        }
    }
}

impl Display for TypeScheme {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for x in self.ids.iter() {
            "∀τ".fmt(f)?;
            x.fmt(f)?;
            ". ".fmt(f)?;
        }
        self.ty.fmt(f)?;
        if !self.deps.is_empty() {
            " with {".fmt(f)?;
            for (i, dep) in self.deps.iter().enumerate() {
                dep.fmt(f)?;
                if i + 1 < self.deps.len() {
                    ", ".fmt(f)?;
                }
            }
            '}'.fmt(f)?;
        }
        Ok(())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        TypeFormatter::default().fmt_type(self, f)
    }
}

pub struct TypeFormatter {
    var_names: BTreeMap<Id, String>,
    next_var_no: u64,
}

impl Default for TypeFormatter {
    fn default() -> Self {
        TypeFormatter {
            var_names: BTreeMap::new(),
            next_var_no: 1,
        }
    }
}

impl TypeFormatter {
    fn alloc_var_no(&mut self) -> u64 {
        let var_no = self.next_var_no;
        self.next_var_no += 1;
        var_no
    }

    fn get_var_name(&mut self, id: &Id) -> String {
        match self.var_names.get(id) {
            Some(name) => name.clone(),
            None => {
                let var_no = self.alloc_var_no();
                let var_name = format!("τ{}", var_no);
                self.var_names.insert(*id, var_name.clone());
                var_name
            }
        }
    }

    pub fn fmt_var(&mut self, id: &Id, f: &mut Formatter<'_>) -> fmt::Result {
        self.get_var_name(id).fmt(f)
    }

    pub fn fmt_type(&mut self, t: &Type, f: &mut Formatter<'_>) -> fmt::Result {
        match t {
            Type::Con(c) => c.fmt(f),
            Type::Tuple(xs) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    self.fmt_type(x, f)?;
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
                    self.fmt_type(v, f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                '}'.fmt(f)
            }
            Type::App(a, b) => match &**a {
                Type::Con(TypeCon::List) => {
                    '['.fmt(f)?;
                    self.fmt_type(b, f)?;
                    ']'.fmt(f)
                }
                _ => {
                    if let Some(AppliedType::Arrow(_, _)) = t.as_applied_type() {
                        let mut t = t;
                        while let Some(AppliedType::Arrow(a, b)) = t.as_applied_type() {
                            if let Some(AppliedType::Arrow(_, _)) = a.as_applied_type() {
                                '('.fmt(f)?;
                                self.fmt_type(a, f)?;
                                ')'.fmt(f)?;
                            } else {
                                self.fmt_type(a, f)?;
                            }
                            " → ".fmt(f)?;
                            t = b;
                        }
                        self.fmt_type(t, f)
                    } else {
                        self.fmt_type(a, f)?;
                        '<'.fmt(f)?;
                        self.fmt_type(b, f)?;
                        '>'.fmt(f)
                    }
                }
            },
            Type::UnresolvedVar(x) => {
                'τ'.fmt(f)?;
                x.fmt(f)
            }
            Type::Var(x) => self.fmt_var(x, f),
            Type::ADT(x) => x.name.fmt(f), // Only show name
        }
    }
}

pub trait Dispatch: Display {
    fn num_params(&self) -> usize;
    fn maybe_accepts_args(&self, args: &[Arc<Expr>]) -> bool;
}

impl Dispatch for Type {
    fn num_params(&self) -> usize {
        if let Some(AppliedType::Arrow(_, b)) = self.as_applied_type() {
            1 + b.num_params()
        } else {
            0
        }
    }

    fn maybe_accepts_args(&self, args: &[Arc<Expr>]) -> bool {
        if let Some(AppliedType::Arrow(a, b)) = self.as_applied_type() {
            !args.is_empty() && a.maybe_compatible(&args[0]) && b.maybe_accepts_args(&args[1..])
        } else {
            args.is_empty()
        }
    }
}

impl Dispatch for Arc<Type> {
    fn num_params(&self) -> usize {
        Type::num_params(self)
    }

    fn maybe_accepts_args(&self, args: &[Arc<Expr>]) -> bool {
        Type::maybe_accepts_args(self, args)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
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

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct ADTVariant {
    pub name: String,
    pub t: Option<Arc<Type>>,
    pub docs: Option<String>,
    pub t_docs: Option<BTreeMap<String, String>>,
    pub discriminant: Option<i64>,
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
        Type::Con(TypeCon::Bool)
    }
}

impl ToType for u8 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Uint)
    }
}

impl ToType for u16 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Uint)
    }
}

impl ToType for u32 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Uint)
    }
}

impl ToType for u64 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Uint)
    }
}

impl ToType for u128 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Uint)
    }
}

impl ToType for i8 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Int)
    }
}

impl ToType for i16 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Int)
    }
}

impl ToType for i32 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Int)
    }
}

impl ToType for i64 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Int)
    }
}

impl ToType for i128 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Int)
    }
}

impl ToType for f32 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Float)
    }
}

impl ToType for f64 {
    fn to_type() -> Type {
        Type::Con(TypeCon::Float)
    }
}

impl ToType for str {
    fn to_type() -> Type {
        Type::Con(TypeCon::String)
    }
}

impl ToType for String {
    fn to_type() -> Type {
        Type::Con(TypeCon::String)
    }
}

impl ToType for Uuid {
    fn to_type() -> Type {
        Type::Con(TypeCon::Uuid)
    }
}

impl ToType for DateTime<Utc> {
    fn to_type() -> Type {
        Type::Con(TypeCon::DateTime)
    }
}

impl ToType for serde_json::Value {
    fn to_type() -> Type {
        Type::ADT(ADT {
            name: "serde_json::Value".to_string(),
            docs: None,
            variants: vec![ADTVariant {
                name: "serde_json::Value".to_string(),
                t: Some(Arc::new(Type::Con(TypeCon::String))),
                docs: None,
                t_docs: None,
                discriminant: None,
            }],
        })
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
        Type::make_arrow(Arc::new(A0::to_type()), Arc::new(B::to_type()))
    }
}

impl<A0, A1, B> ToType for fn(A0, A1) -> B
where
    A0: ToType,
    A1: ToType,
    B: ToType,
{
    fn to_type() -> Type {
        Type::make_arrow(
            Arc::new(A0::to_type()),
            Arc::new(Type::make_arrow(
                Arc::new(A1::to_type()),
                Arc::new(B::to_type()),
            )),
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
        Type::make_arrow(
            Arc::new(A0::to_type()),
            Arc::new(Type::make_arrow(
                Arc::new(A1::to_type()),
                Arc::new(Type::make_arrow(
                    Arc::new(A2::to_type()),
                    Arc::new(B::to_type()),
                )),
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
        Type::make_arrow(
            Arc::new(A0::to_type()),
            Arc::new(Type::make_arrow(
                Arc::new(A1::to_type()),
                Arc::new(Type::make_arrow(
                    Arc::new(A2::to_type()),
                    Arc::new(Type::make_arrow(
                        Arc::new(A3::to_type()),
                        Arc::new(B::to_type()),
                    )),
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
        Type::make_result(Arc::new(E::to_type()), Arc::new(T::to_type()))
    }
}

impl<T> ToType for Option<T>
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::App(Arc::new(Type::Con(TypeCon::Option)), Arc::new(T::to_type()))
    }
}

impl<T> ToType for [T]
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::App(Arc::new(Type::Con(TypeCon::List)), Arc::new(T::to_type()))
    }
}

impl<T> ToType for Vec<T>
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::App(Arc::new(Type::Con(TypeCon::List)), Arc::new(T::to_type()))
    }
}

impl<T> ToType for VecDeque<T>
where
    T: ToType,
{
    fn to_type() -> Type {
        Type::App(Arc::new(Type::Con(TypeCon::List)), Arc::new(T::to_type()))
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
    use crate::{arrow, bool, float, int, list, option, promise, result, string, uint};
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

    #[test]
    fn test_to_string() {
        let t = arrow!(uint!() => bool!());
        assert_eq!(t.to_string(), "uint → bool");

        let t = arrow!(uint!() => float!() => string!() => bool!());
        assert_eq!(t.to_string(), "uint → float → string → bool");

        let t = arrow!(uint!() => arrow!(float!() => int!()) => string!() => bool!());
        assert_eq!(t.to_string(), "uint → (float → int) → string → bool");
    }

    #[test]
    fn test_fn_totype() {
        assert_eq!(Arc::new(<fn() -> bool as ToType>::to_type()), bool!());
        assert_eq!(
            Arc::new(<fn(u64) -> bool as ToType>::to_type()),
            arrow!(uint!() => bool!())
        );
        assert_eq!(
            Arc::new(<fn(u64, String) -> bool as ToType>::to_type()),
            arrow!(uint!() => string!() => bool!())
        );
        assert_eq!(
            Arc::new(<fn(u64, String, i64) -> bool as ToType>::to_type()),
            arrow!(uint!() => string!() => int!() => bool!())
        );
        assert_eq!(
            Arc::new(<fn(u64, String, i64, f64) -> bool as ToType>::to_type()),
            arrow!(uint!() => string!() => int!() => float!() => bool!())
        );
    }

    #[test]
    fn test_applied_type() {
        assert_eq!(
            arrow!(int!() => bool!()).as_applied_type(),
            Some(AppliedType::Arrow(&int!(), &bool!()))
        );
        assert_eq!(
            arrow!(int!() => bool!() => string!()).as_applied_type(),
            Some(AppliedType::Arrow(&int!(), &arrow!(bool!() => string!())))
        );
        assert_eq!(
            result!(bool!(), string!()).as_applied_type(),
            Some(AppliedType::Result(&bool!(), &string!()))
        );
        assert_eq!(
            option!(string!()).as_applied_type(),
            Some(AppliedType::Option(&string!()))
        );
        assert_eq!(
            promise!(string!()).as_applied_type(),
            Some(AppliedType::Promise(&string!()))
        );
        assert_eq!(
            list!(string!()).as_applied_type(),
            Some(AppliedType::List(&string!()))
        );
    }
}
