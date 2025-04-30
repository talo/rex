use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt,
    sync::Arc,
};

use rex_ast::id::Id;
use rex_lexer::span::Span;

use crate::{
    constraint::{Constraint, ConstraintSystem},
    error::TypeError,
    types::{ADTVariant, Type, TypeScheme, ADT},
};

pub type Subst = HashMap<Id, Arc<Type>>;

// NOTE(loong): We do not support overloaded parametric polymorphism.
pub fn unify_constraints(
    zconstraint_system: &ConstraintSystem,
    errors: &mut BTreeSet<TypeError>,
) -> Subst {
    let mut subst = Subst::new();

    let mut eq_constraints: Vec<Constraint> = Vec::new();
    let mut one_of_constraints: Vec<Constraint> = Vec::new();

    for constraint in zconstraint_system.constraints() {
        match constraint {
            Constraint::Eq(..) => {
                eq_constraints.push(constraint.clone());
            }
            Constraint::OneOf(..) => {
                one_of_constraints.push(constraint.clone());
            }
        }
    }

    // Loop until no more progress is made in resolving. We often need multiple iterations
    // because unifications that happen in later contraints may enable unifications in earlier
    // ones.
    loop {
        let mut did_change = false;

        eq_constraints = unify_eq_constraints(eq_constraints, &mut subst, &mut did_change, errors);
        one_of_constraints =
            unify_one_of_constraints(one_of_constraints, &mut subst, &mut did_change, errors);

        if !did_change {
            break;
        }
    }

    subst
}

fn unify_eq_constraints(
    constraints: Vec<Constraint>,
    subst: &mut Subst,
    did_change: &mut bool,
    errors: &mut BTreeSet<TypeError>,
) -> Vec<Constraint> {
    let mut keep: Vec<Constraint> = Vec::new();

    for constraint in constraints.into_iter() {
        match &constraint {
            Constraint::Eq(span, t1, t2) => {
                match unify_eq(t1, t2, span, subst, did_change, &Path::Empty) {
                    Ok(()) => {
                        keep.push(constraint);
                    }
                    Err(e) => {
                        errors.insert(e);
                    }
                }
            }
            Constraint::OneOf(..) => {}
        }
    }

    keep
}

fn unify_one_of_constraints(
    constraints: Vec<Constraint>,
    subst: &mut Subst,
    did_change: &mut bool,
    errors: &mut BTreeSet<TypeError>,
) -> Vec<Constraint> {
    let mut keep: Vec<Constraint> = Vec::new();

    for constraint in constraints.into_iter() {
        match &constraint {
            Constraint::Eq(..) => {}
            Constraint::OneOf(span, t1, t2_possibilties) => {
                // TODO(loong): doing the naive thing of ? short-circuiting this
                // result does not work. Because it will cause issues for unused
                // overloaded type variables. This is because we are resolving
                // all constraints, not just the ones that are actually used by
                // the expression.
                match unify_one_of(t1, t2_possibilties, span, subst, did_change) {
                    Ok(()) => {
                        keep.push(constraint);
                    }
                    Err(e) => {
                        errors.insert(e);
                    }
                }
            }
        }
    }

    keep
}

pub fn unify_eq(
    t1: &Arc<Type>,
    t2: &Arc<Type>,
    span: &Span,
    subst: &mut Subst,
    did_change: &mut bool,
    path: &Path<'_>,
) -> Result<(), TypeError> {
    // First apply any existing substitutions
    let t1 = apply_subst(t1, subst);
    let t2 = apply_subst(t2, subst);

    match (&*t1, &*t2) {
        // Base types must match exactly
        (Type::Bool, Type::Bool) => Ok(()),
        (Type::Uint, Type::Uint) => Ok(()),
        (Type::Int, Type::Int) => Ok(()),
        (Type::Float, Type::Float) => Ok(()),
        (Type::String, Type::String) => Ok(()),
        (Type::Uuid, Type::Uuid) => Ok(()),
        (Type::DateTime, Type::DateTime) => Ok(()),

        // Tuples
        (Type::Tuple(ts1), Type::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                return Err(TypeError::TupleLengthMismatch(*span, path.to_string()));
            }

            for (i, (t1, t2)) in ts1.iter().zip(ts2.iter()).enumerate() {
                unify_eq(t1, t2, span, subst, did_change, &path.index(i))?;
            }

            Ok(())
        }

        // Lists
        (Type::List(t1), Type::List(t2)) => {
            unify_eq(&t1, &t2, span, subst, did_change, &path.variant("item"))
        }

        // Dictionaries
        (Type::Dict(d1), Type::Dict(d2)) => {
            if d1.len() != d2.len() {
                return Err(TypeError::DictKeysMismatch(
                    *span,
                    path.to_string(),
                    missing_keys_vec(d1, d2),
                ));
            }
            for (key, entry1) in d1.iter() {
                if let Some(entry2) = d2.get(key) {
                    unify_eq(entry1, entry2, span, subst, did_change, &path.property(key))?;
                } else {
                    return Err(TypeError::DictKeysMismatch(
                        *span,
                        path.to_string(),
                        missing_keys_vec(d1, d2),
                    ));
                }
            }
            Ok(())
        }

        // For function types, unify arguments and results
        (Type::Arrow(a1, b1), Type::Arrow(a2, b2)) => {
            unify_eq(&a1, &a2, span, subst, did_change, path)?;
            unify_eq(&b1, &b2, span, subst, did_change, path)
        }

        // Result
        (Type::Result(a1, b1), Type::Result(a2, b2)) => {
            unify_eq(&a1, &a2, span, subst, did_change, &path.variant("Ok"))?;
            unify_eq(&b1, &b2, span, subst, did_change, &path.variant("Err"))
        }

        // Option
        (Type::Option(a1), Type::Option(a2)) => {
            unify_eq(&a1, &a2, span, subst, did_change, &path.variant("Some"))
        }

        // Promise
        (Type::Promise(a1), Type::Promise(a2)) => {
            unify_eq(&a1, &a2, span, subst, did_change, &path.variant("Promise"))
        }

        // Type variable case requires occurs check
        (Type::Var(v1), Type::Var(v2)) => {
            if v1 != v2 {
                subst.insert(v1.clone(), Arc::new(Type::Var(v2.clone())));
                *did_change = true;
            }
            Ok(())
        }

        // Type variable case requires occurs check
        (Type::Var(v), _) => {
            if occurs_check(v, &t2) {
                Err(TypeError::OccursCheckFailed(*span, path.to_string()))
            } else {
                subst.insert(v.clone(), t2);
                *did_change = true;
                Ok(())
            }
        }
        (_, Type::Var(v)) => {
            if occurs_check(v, &t1) {
                Err(TypeError::OccursCheckFailed(*span, path.to_string()))
            } else {
                subst.insert(v.clone(), t1.clone());
                *did_change = true;
                Ok(())
            }
        }

        // ADTs
        (Type::ADT(adt1), Type::ADT(adt2)) => {
            if adt1.name != adt2.name {
                return Err(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
            }

            if adt1.variants.len() != adt2.variants.len() {
                return Err(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
            }

            for i in 0..adt1.variants.len() {
                let v1 = &adt1.variants[i];
                let v2 = &adt2.variants[i];

                if v1.name != v2.name {
                    return Err(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
                }

                match (&v1.t, &v2.t) {
                    (None, None) => (),
                    (Some(vt1), Some(vt2)) => {
                        unify_eq(vt1, vt2, span, subst, did_change, &path.variant(&v1.name))?;
                    }
                    _ => {
                        return Err(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
                    }
                }
            }

            return Ok(());
        }

        // Everything else fails
        (_, _) => Err(TypeError::CannotUnify(*span, path.to_string(), t1, t2)),
    }
}

pub fn unify_one_of(
    t1: &Arc<Type>,
    t2_possibilities: &BTreeSet<Arc<Type>>,
    span: &Span,
    subst: &mut Subst,
    did_change: &mut bool,
) -> Result<(), TypeError> {
    // First apply any existing substitutions
    let t1 = apply_subst(t1, subst);

    let mut successes = Vec::new();

    // Try unifying with each possibility
    for t2 in t2_possibilities {
        let t2 = apply_subst(t2, subst);
        let mut test_subst = subst.clone();
        let mut test_did_change = *did_change;

        if unify_eq(
            &t1,
            &t2,
            span,
            &mut test_subst,
            &mut test_did_change,
            &Path::Empty,
        )
        .is_ok()
        {
            successes.push((t2, test_subst, test_did_change));
        }
    }

    match successes.len() {
        0 => Err(TypeError::IncompatibleCandidates(
            *span,
            t1.clone(),
            BTreeSet::from_iter(t2_possibilities.iter().cloned()),
        )),
        1 => {
            // Use the successful substitution
            *subst = successes[0].1.clone();
            *did_change = successes[0].2.clone();
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn apply_subst(t: &Arc<Type>, subst: &Subst) -> Arc<Type> {
    match &**t {
        Type::UnresolvedVar(_) => todo!("apply_subst should return a result"),
        Type::Var(v) => {
            if let Some(t2) = subst.get(v) {
                apply_subst(t2, subst)
            } else {
                t.clone()
            }
        }
        Type::ADT(adt) => Arc::new(Type::ADT(ADT {
            docs: adt.docs.clone(),
            name: adt.name.clone(),
            variants: adt
                .variants
                .iter()
                .map(|v| ADTVariant {
                    docs: v.docs.clone(),
                    name: v.name.clone(),
                    t: v.t.as_ref().map(|t| apply_subst(t, subst)),
                    t_docs: v.t_docs.clone(),
                    discriminant: v.discriminant,
                })
                .collect(),
        })),
        Type::Arrow(a, b) => Arc::new(Type::Arrow(apply_subst(a, subst), apply_subst(b, subst))),
        Type::Result(t, e) => Arc::new(Type::Result(apply_subst(t, subst), apply_subst(e, subst))),
        Type::Option(t) => Arc::new(Type::Option(apply_subst(t, subst))),
        Type::Promise(t) => Arc::new(Type::Promise(apply_subst(t, subst))),
        Type::List(t) => Arc::new(Type::List(apply_subst(t, subst))),
        Type::Dict(kts) => Arc::new(Type::Dict(
            kts.iter()
                .map(|(k, t)| (k.clone(), apply_subst(t, subst)))
                .collect(),
        )),
        Type::Tuple(ts) => Arc::new(Type::Tuple(
            ts.iter().map(|t| apply_subst(t, subst)).collect(),
        )),

        Type::Bool
        | Type::Uint
        | Type::Int
        | Type::Float
        | Type::String
        | Type::Uuid
        | Type::DateTime => t.clone(),
    }
}

pub fn occurs_check_type_scheme(var: &Id, scheme: &TypeScheme) -> bool {
    // If we're looking for the same variable that's quantified,
    // then it doesn't occur freely (it's bound)

    for id in scheme.ids.iter() {
        if id == var {
            return false;
        }
    }

    occurs_check(var, &scheme.ty)
}

pub fn occurs_check(var: &Id, t: &Arc<Type>) -> bool {
    match &**t {
        Type::UnresolvedVar(_) => false, // TODO(loong): should this function return a result?
        Type::Var(v) => v == var,
        Type::ADT(adt) => adt.variants.iter().any(|v| match &v.t {
            Some(t) => occurs_check(var, t),
            None => false,
        }),
        Type::Arrow(a, b) => occurs_check(var, a) || occurs_check(var, b),
        Type::Result(t, e) => occurs_check(var, t) || occurs_check(var, e),
        Type::Option(t) => occurs_check(var, t),
        Type::Promise(t) => occurs_check(var, t),
        Type::List(t) => occurs_check(var, t),
        Type::Dict(kts) => kts.values().any(|t| occurs_check(var, t)),
        Type::Tuple(ts) => ts.iter().any(|t| occurs_check(var, t)),

        Type::Bool
        | Type::Uint
        | Type::Int
        | Type::Float
        | Type::String
        | Type::Uuid
        | Type::DateTime => false,
    }
}

fn missing_keys_vec(
    d1: &BTreeMap<String, Arc<Type>>,
    d2: &BTreeMap<String, Arc<Type>>,
) -> Vec<String> {
    let mut missing_keys: Vec<String> = Vec::new();
    for k in d1.keys() {
        if !d2.contains_key(k) {
            missing_keys.push(k.clone());
        }
    }

    for k in d2.keys() {
        if !d1.contains_key(k) {
            missing_keys.push(k.clone());
        }
    }

    missing_keys.sort();
    missing_keys
}

// Test cases
#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use rex_ast::id::Id;

    use super::*;

    #[test]
    fn test_basic_unification() {
        let alpha = Id::new();
        let beta = Id::new();

        let mut subst = Subst::new();
        let mut did_change = false;

        // Test case 1: α = Int
        let t1 = Arc::new(Type::Var(alpha));
        let t2 = Arc::new(Type::Int);
        assert!(unify_eq(
            &t1,
            &t2,
            &Span::default(),
            &mut subst,
            &mut did_change,
            &Path::Empty,
        )
        .is_ok());
        assert_eq!(
            apply_subst(&Arc::new(Type::Var(alpha)), &subst),
            Arc::new(Type::Int)
        );

        // Test case 2: (α -> β) = (Int -> Bool)
        let t1 = Arc::new(Type::Arrow(
            Arc::new(Type::Var(alpha)),
            Arc::new(Type::Var(beta)),
        ));
        let t2 = Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Bool)));
        assert!(unify_eq(
            &t1,
            &t2,
            &Span::default(),
            &mut subst,
            &mut did_change,
            &Path::Empty
        )
        .is_ok());
        assert_eq!(
            apply_subst(&Arc::new(Type::Var(alpha)), &subst),
            Arc::new(Type::Int)
        );
        assert_eq!(
            apply_subst(&Arc::new(Type::Var(beta)), &subst),
            Arc::new(Type::Bool)
        );
    }

    #[test]
    fn test_var_unification() {
        let alpha = Id::new();
        let beta = Id::new();

        let mut subst = Subst::new();
        let mut did_change = false;

        // Unify α = β
        let t1 = Arc::new(Type::Var(alpha));
        let t2 = Arc::new(Type::Var(beta));
        assert!(unify_eq(
            &t1,
            &t2,
            &Span::default(),
            &mut subst,
            &mut did_change,
            &Path::Empty
        )
        .is_ok());

        // Now α should be mapped to β
        assert_eq!(
            apply_subst(&Arc::new(Type::Var(alpha)), &subst),
            Arc::new(Type::Var(beta))
        );
    }

    #[test]
    fn test_function_unification() {
        let alpha = Id::new();
        let beta = Id::new();
        let gamma = Id::new();

        let mut subst = Subst::new();
        let mut did_change = false;

        // f : α -> β
        let f_type = Arc::new(Type::Arrow(
            Arc::new(Type::Var(alpha)), // α
            Arc::new(Type::Var(beta)),  // β
        ));

        // g : γ -> γ
        let g_type = Arc::new(Type::Arrow(
            Arc::new(Type::Var(gamma)), // γ
            Arc::new(Type::Var(gamma)), // γ (same type)
        ));

        // Unify f with g directly
        assert!(unify_eq(
            &f_type,
            &g_type,
            &Span::default(),
            &mut subst,
            &mut did_change,
            &Path::Empty
        )
        .is_ok());

        // After unification:
        let final_f = apply_subst(&f_type, &subst);
        let final_g = apply_subst(&g_type, &subst);

        // Both should be unified to the same type: γ -> γ
        assert_eq!(final_f, final_g);

        // And specifically they should both be γ -> γ
        assert_eq!(
            final_f,
            Arc::new(Type::Arrow(
                Arc::new(Type::Var(gamma)),
                Arc::new(Type::Var(gamma))
            ))
        );
    }

    #[test]
    fn test_composition_unification() {
        let alpha = Id::new();
        let beta = Id::new();
        let gamma = Id::new();

        let mut subst = Subst::new();
        let mut did_change = false;

        // f : α -> β
        let f_type = Arc::new(Type::Arrow(
            Arc::new(Type::Var(alpha)), // α
            Arc::new(Type::Var(beta)),  // β
        ));

        // g : γ -> γ
        let g_type = Arc::new(Type::Arrow(
            Arc::new(Type::Var(gamma)), // γ
            Arc::new(Type::Var(gamma)), // γ (same type)
        ));

        // Now unify g's output with f's input
        let g_output = Arc::new(Type::Var(gamma)); // γ
        let f_input = Arc::new(Type::Var(alpha)); // α
        assert!(unify_eq(
            &g_output,
            &f_input,
            &Span::default(),
            &mut subst,
            &mut did_change,
            &Path::Empty
        )
        .is_ok());

        // Let's make g take an Int
        assert!(unify_eq(
            &Arc::new(Type::Var(gamma)),
            &Arc::new(Type::Int),
            &Span::default(),
            &mut subst,
            &mut did_change,
            &Path::Empty
        )
        .is_ok());

        // After unification:
        let final_g = apply_subst(&g_type, &subst);
        let final_f = apply_subst(&f_type, &subst);

        // g should be Int -> Int
        assert_eq!(
            final_g,
            Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int)))
        );

        // f should be Int -> β (where β is still free)
        assert_eq!(
            final_f,
            Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Var(beta))))
        );
    }

    #[test]
    fn test_higher_order_unification() {
        let alpha = Id::new();
        let beta = Id::new();
        let gamma = Id::new();
        let delta = Id::new();

        let mut subst = Subst::new();
        let mut did_change = false;

        // f : (α -> β) -> γ
        let f_type = Arc::new(Type::Arrow(
            Arc::new(Type::Arrow(
                Arc::new(Type::Var(alpha)), // α
                Arc::new(Type::Var(beta)),  // β
            )),
            Arc::new(Type::Var(gamma)), // γ
        ));

        // g : (Int -> Bool) -> δ
        let g_type = Arc::new(Type::Arrow(
            Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Bool))),
            Arc::new(Type::Var(delta)), // δ
        ));

        // Unify f with g
        assert!(unify_eq(
            &f_type,
            &g_type,
            &Span::default(),
            &mut subst,
            &mut did_change,
            &Path::Empty
        )
        .is_ok());

        // After unification:
        let final_f = apply_subst(&f_type, &subst);

        // final_f should be (Int -> Bool) -> δ
        assert_eq!(
            final_f,
            Arc::new(Type::Arrow(
                Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Bool))),
                Arc::new(Type::Var(delta))
            ))
        );
    }

    #[test]
    fn test_occurs_check() {
        let alpha = Id::new();
        let beta = Id::new();

        let var = alpha.clone();

        // Simple variable occurrence
        assert!(occurs_check(&var, &Arc::new(Type::Var(alpha))));
        assert!(!occurs_check(&var, &Arc::new(Type::Var(beta))));

        // Arrow type occurrences
        assert!(occurs_check(
            &var,
            &Arc::new(Type::Arrow(Arc::new(Type::Var(alpha)), Arc::new(Type::Int)))
        ));
        assert!(occurs_check(
            &var,
            &Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Var(alpha))))
        ));

        // Tuple occurrences - would have failed before our fix
        assert!(occurs_check(
            &var,
            &Arc::new(Type::Tuple(vec![
                Arc::new(Type::Int),
                Arc::new(Type::Var(alpha)),
                Arc::new(Type::Bool)
            ]))
        ));
        assert!(!occurs_check(
            &var,
            &Arc::new(Type::Tuple(vec![
                Arc::new(Type::Int),
                Arc::new(Type::Var(beta)),
                Arc::new(Type::Bool)
            ]))
        ));

        // List occurrences - would have failed before our fix
        assert!(occurs_check(
            &var,
            &Arc::new(Type::List(Arc::new(Type::Var(alpha))))
        ));
        assert!(!occurs_check(
            &var,
            &Arc::new(Type::List(Arc::new(Type::Var(beta))))
        ));

        // Nested structures - would have failed before our fix
        assert!(occurs_check(
            &var,
            &Arc::new(Type::Tuple(vec![
                Arc::new(Type::List(Arc::new(Type::Var(alpha)))),
                Arc::new(Type::Arrow(
                    Arc::new(Type::Int),
                    Arc::new(Type::List(Arc::new(Type::Bool)))
                ))
            ]))
        ));

        // ForAll cases - would have failed before our fix
        // Case 1: The variable we're looking for is bound by the ForAll
        assert!(!occurs_check_type_scheme(
            &var,
            &TypeScheme {
                ids: vec![alpha],
                ty: Arc::new(Type::Var(alpha)),
                deps: BTreeSet::new(),
            }
        ));

        // Case 2: The variable we're looking for occurs freely in the body
        let var2 = beta;
        assert!(occurs_check_type_scheme(
            &var2,
            &TypeScheme {
                ids: vec![alpha],
                ty: Arc::new(Type::Arrow(
                    Arc::new(Type::Var(beta)),
                    Arc::new(Type::Var(alpha))
                )),
                deps: BTreeSet::new()
            }
        ));
    }
}

#[derive(Clone)]
pub enum PathItem<'a> {
    Property(&'a str),
    Variant(&'a str),
    Index(usize),
}

pub enum Path<'a> {
    Empty,
    Property(&'a str, &'a Path<'a>),
    Variant(&'a str, &'a Path<'a>),
    Index(usize, &'a Path<'a>),
}

impl<'a> Path<'a> {
    pub fn property(&'a self, name: &'a str) -> Self {
        Path::Property(name, self)
    }

    pub fn variant(&'a self, name: &'a str) -> Self {
        Path::Variant(name, self)
    }

    pub fn index(&'a self, index: usize) -> Self {
        Path::Index(index, self)
    }

    pub fn collect_vec<'b>(mut path: &'b Path<'b>, items: &mut Vec<&'b Path<'b>>) {
        loop {
            match path {
                Path::Empty => break,
                Path::Property(_, parent) => {
                    items.push(path);
                    path = parent;
                }
                Path::Variant(_, parent) => {
                    items.push(path);
                    path = parent;
                }
                Path::Index(_, parent) => {
                    items.push(path);
                    path = parent;
                }
            }
        }
    }

    pub fn string_for(path: &Path) -> Option<String> {
        match path {
            Path::Empty => None,
            _ => Some(path.to_string()),
        }
    }
}

impl<'a> fmt::Display for Path<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let mut items = Vec::new();
        Path::collect_vec(self, &mut items);
        for (i, item) in items.iter().rev().enumerate() {
            if i == 0 {
                write!(f, "In property ")?;
            }
            match item {
                Path::Property(name, _) => {
                    if i > 0 {
                        write!(f, ".")?;
                    }
                    write!(f, "{}", name)?
                }
                Path::Variant(name, _) => {
                    if i > 0 {
                        write!(f, "#")?;
                    }
                    write!(f, "{}", name)?
                }
                Path::Index(i, _) => write!(f, "[{}]", i)?,
                Path::Empty => {}
            }
        }
        Ok(())
    }
}
