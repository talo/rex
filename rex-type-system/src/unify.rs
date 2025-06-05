use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    sync::Arc,
};

use rex_lexer::span::Span;
use rpds::HashTrieMapSync;

use crate::{
    constraint::{Constraint, ConstraintSystem},
    error::TypeError,
    types::{Type, TypeVar},
};

pub type Subst = HashTrieMapSync<TypeVar, Arc<Type>>;

// NOTE(loong): We do not support overloaded parametric polymorphism.
pub fn unify_constraints(
    constraint_system: &mut ConstraintSystem,
    errors: &mut BTreeSet<TypeError>,
) {
    let mut eq_constraints: Vec<Constraint> = Vec::new();
    let mut one_of_constraints: Vec<Constraint> = Vec::new();

    for constraint in constraint_system.constraints() {
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

        eq_constraints = unify_eq_constraints(
            eq_constraints,
            &mut constraint_system.subst,
            &mut did_change,
            errors,
        );
        one_of_constraints = unify_one_of_constraints(
            one_of_constraints,
            &mut constraint_system.subst,
            &mut did_change,
            errors,
        );

        if !did_change {
            break;
        }
    }
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
            Constraint::Eq(span, t1, t2) => match unify_eq(t1, t2, span, subst, did_change) {
                Ok(()) => {
                    keep.push(constraint);
                }
                Err(es) => {
                    errors.extend(es);
                }
            },
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
                match unify_one_of(
                    &Arc::new(Type::Var(*t1)),
                    t2_possibilties,
                    span,
                    subst,
                    did_change,
                ) {
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
) -> Result<(), Vec<TypeError>> {
    let mut errors: Vec<TypeError> = Vec::new();
    unify_eq_r(t1, t2, span, subst, did_change, &mut errors, &Path::Empty);
    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(())
    }
}

pub fn unify_eq_r(
    t1: &Arc<Type>,
    t2: &Arc<Type>,
    span: &Span,
    subst: &mut Subst,
    did_change: &mut bool,
    errors: &mut Vec<TypeError>,
    path: &Path<'_>,
) {
    // First apply any existing substitutions
    let t1 = t1.apply(subst);
    let t2 = t2.apply(subst);

    if t1.kind() != t2.kind() {
        errors.push(TypeError::KindMismatch(
            *span,
            path.to_string(),
            t1.kind(),
            t2.kind(),
        ));
        return;
    }

    match (&*t1, &*t2) {
        // Base types must match exactly
        (Type::Con(lhs), Type::Con(rhs)) if lhs == rhs => {}

        // Tuples
        (Type::Tuple(ts1), Type::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                errors.push(TypeError::TupleLengthMismatch(*span, path.to_string()));
                return;
            }

            for (i, (t1, t2)) in ts1.iter().zip(ts2.iter()).enumerate() {
                unify_eq_r(t1, t2, span, subst, did_change, errors, &path.index(i));
            }
        }

        // Type application
        (Type::App(a1, b1), Type::App(a2, b2)) => {
            unify_eq_r(a1, a2, span, subst, did_change, errors, path);
            unify_eq_r(b1, b2, span, subst, did_change, errors, path);
        }

        // Dictionaries
        (Type::Dict(d1), Type::Dict(d2)) => {
            if d1.len() != d2.len() {
                errors.push(TypeError::DictKeysMismatch(
                    *span,
                    path.to_string(),
                    missing_keys_vec(d1, d2),
                ));
                return;
            }
            for (key, entry1) in d1.iter() {
                if let Some(entry2) = d2.get(key) {
                    unify_eq_r(
                        entry1,
                        entry2,
                        span,
                        subst,
                        did_change,
                        errors,
                        &path.property(key),
                    );
                } else {
                    errors.push(TypeError::DictKeysMismatch(
                        *span,
                        path.to_string(),
                        missing_keys_vec(d1, d2),
                    ));
                }
            }
        }

        // Type variable case requires occurs check
        (Type::Var(v1), Type::Var(v2)) => {
            if v1 != v2 {
                *subst = subst.insert(*v1, Arc::new(Type::Var(*v2)));
                *did_change = true;
            }
        }

        // Type variable case requires occurs check
        (Type::Var(v), _) => {
            if t2.occurs_check(v) {
                errors.push(TypeError::OccursCheckFailed(*span, path.to_string()));
            } else {
                *subst = subst.insert(*v, t2);
                *did_change = true;
            }
        }
        (_, Type::Var(v)) => {
            if t1.occurs_check(v) {
                errors.push(TypeError::OccursCheckFailed(*span, path.to_string()));
            } else {
                *subst = subst.insert(*v, t1.clone());
                *did_change = true;
            }
        }

        // ADTs
        (Type::ADT(adt1), Type::ADT(adt2)) => {
            if adt1.name != adt2.name {
                errors.push(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
                return;
            }

            if adt1.variants.len() != adt2.variants.len() {
                errors.push(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
                return;
            }

            for i in 0..adt1.variants.len() {
                let v1 = &adt1.variants[i];
                let v2 = &adt2.variants[i];

                if v1.name != v2.name {
                    errors.push(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
                    return;
                }

                match (&v1.t, &v2.t) {
                    (None, None) => (),
                    (Some(vt1), Some(vt2)) => {
                        unify_eq_r(
                            vt1,
                            vt2,
                            span,
                            subst,
                            did_change,
                            errors,
                            &path.variant(&v1.name),
                        );
                    }
                    _ => {
                        errors.push(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
                        return;
                    }
                }
            }
        }

        // Everything else fails
        (_, _) => {
            errors.push(TypeError::CannotUnify(*span, path.to_string(), t1, t2));
        }
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
    let t1 = t1.apply(subst);

    let mut successes = Vec::new();

    // Try unifying with each possibility
    for t2 in t2_possibilities {
        let t2 = t2.apply(subst);
        let mut test_subst = subst.clone();
        let mut test_did_change = *did_change;

        if unify_eq(&t1, &t2, span, &mut test_subst, &mut test_did_change).is_ok() {
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
            *did_change = successes[0].2;
            Ok(())
        }
        _ => Ok(()),
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
    use crate::{arrow, bool, int, list, tuple, types::Kind, types::TypeScheme, var};

    use super::*;

    #[test]
    fn test_basic_unification() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();

        let mut subst = Subst::default();
        let mut did_change = false;

        // Test case 1: α = Int
        let t1 = var!(alpha);
        let t2 = int!();
        assert!(unify_eq(&t1, &t2, &Span::default(), &mut subst, &mut did_change).is_ok());
        assert_eq!(var!(alpha).apply(&subst), int!());

        // Test case 2: (α -> β) = (Int -> Bool)
        let t1 = arrow!(var!(alpha) => var!(beta));
        let t2 = arrow!(int!() => bool!());
        assert!(unify_eq(&t1, &t2, &Span::default(), &mut subst, &mut did_change).is_ok());
        assert_eq!(var!(alpha).apply(&subst), int!());
        assert_eq!(var!(beta).apply(&subst), bool!());
    }

    #[test]
    fn test_var_unification() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();

        let mut subst = Subst::default();
        let mut did_change = false;

        // Unify α = β
        let t1 = var!(alpha);
        let t2 = var!(beta);
        assert!(unify_eq(&t1, &t2, &Span::default(), &mut subst, &mut did_change).is_ok());

        // Now α should be mapped to β
        assert_eq!(var!(alpha).apply(&subst), var!(beta));
    }

    #[test]
    fn test_function_unification() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();
        let gamma = TypeVar::new();

        let mut subst = Subst::default();
        let mut did_change = false;

        // f : α -> β
        let f_type = arrow!(var!(alpha) => var!(beta));

        // g : γ -> γ
        let g_type = arrow!(var!(gamma) => var!(gamma));

        // Unify f with g directly
        assert!(unify_eq(
            &f_type,
            &g_type,
            &Span::default(),
            &mut subst,
            &mut did_change
        )
        .is_ok());

        // After unification:
        let final_f = f_type.apply(&subst);
        let final_g = g_type.apply(&subst);

        // Both should be unified to the same type: γ -> γ
        assert_eq!(final_f, final_g);

        // And specifically they should both be γ -> γ
        assert_eq!(final_f, arrow!(var!(gamma) => var!(gamma)));
    }

    #[test]
    fn test_composition_unification() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();
        let gamma = TypeVar::new();

        let mut subst = Subst::default();
        let mut did_change = false;

        // f : α -> β
        let f_type = arrow!(var!(alpha) => var!(beta));

        // g : γ -> γ
        let g_type = arrow!(var!(gamma) => var!(gamma));

        // Now unify g's output with f's input
        let g_output = var!(gamma); // γ
        let f_input = var!(alpha); // α
        assert!(unify_eq(
            &g_output,
            &f_input,
            &Span::default(),
            &mut subst,
            &mut did_change
        )
        .is_ok());

        // Let's make g take an Int
        assert!(unify_eq(
            &var!(gamma),
            &int!(),
            &Span::default(),
            &mut subst,
            &mut did_change
        )
        .is_ok());

        // After unification:
        let final_g = g_type.apply(&subst);
        let final_f = f_type.apply(&subst);

        // g should be Int -> Int
        assert_eq!(final_g, arrow!(int!() => int!()));

        // f should be Int -> β (where β is still free)
        assert_eq!(final_f, arrow!(int!() => var!(beta)));
    }

    #[test]
    fn test_higher_order_unification() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();
        let gamma = TypeVar::new();
        let delta = TypeVar::new();

        let mut subst = Subst::default();
        let mut did_change = false;

        // f : (α -> β) -> γ
        let f_type = arrow!(arrow!(var!(alpha) => var!(beta)) => var!(gamma));

        // g : (Int -> Bool) -> δ
        let g_type = arrow!(arrow!(int!() => bool!()) => var!(delta));

        // Unify f with g
        assert!(unify_eq(
            &f_type,
            &g_type,
            &Span::default(),
            &mut subst,
            &mut did_change
        )
        .is_ok());

        // After unification:
        let final_f = f_type.apply(&subst);

        // final_f should be (Int -> Bool) -> δ
        assert_eq!(final_f, arrow!(arrow!(int!() => bool!()) => var!(delta)));
    }

    #[test]
    fn test_occurs_check() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();

        let var = alpha.clone();

        // Simple variable occurrence
        assert!(var!(alpha).occurs_check(&var));
        assert!(!var!(beta).occurs_check(&var));

        // Arrow type occurrences
        assert!(arrow!(var!(alpha) => int!()).occurs_check(&var));
        assert!(arrow!(int!() => var!(alpha)).occurs_check(&var));

        // Tuple occurrences - would have failed before our fix
        assert!(tuple!(int!(), var!(alpha), bool!()).occurs_check(&var));
        assert!(!tuple!(int!(), var!(beta), bool!()).occurs_check(&var));

        // List occurrences - would have failed before our fix
        assert!(list!(var!(alpha)).occurs_check(&var));
        assert!(!list!(var!(beta)).occurs_check(&var));

        // Nested structures - would have failed before our fix
        assert!(tuple!(list!(var!(alpha)), arrow!(int!() => list!(bool!()))).occurs_check(&var));

        // ForAll cases - would have failed before our fix
        // Case 1: The variable we're looking for is bound by the ForAll
        let scheme = TypeScheme::new(vec![alpha], var!(alpha));
        assert!(!scheme.occurs_check(&var));

        // Case 2: The variable we're looking for occurs freely in the body
        let var2 = beta;
        let scheme = TypeScheme::new(vec![alpha], arrow!(var!(beta) => var!(alpha)));
        assert!(scheme.occurs_check(&var2));
    }

    #[test]
    fn test_kind_mismatch() {
        let alpha = TypeVar::new_with_kind(Kind(1));
        let beta = TypeVar::new_with_kind(Kind(2));

        let mut subst = Subst::default();
        let mut did_change = false;
        match unify_eq(
            &Arc::new(Type::Var(alpha)),
            &Arc::new(Type::Var(beta)),
            &Span::default(),
            &mut subst,
            &mut did_change,
        ) {
            Ok(()) => {
                panic!("Expected error");
            }
            Err(errors) => {
                assert!(errors.len() == 1);
                match errors[0] {
                    TypeError::KindMismatch(_, _, k1, k2) => {
                        assert_eq!(k1, Some(Kind(1)));
                        assert_eq!(k2, Some(Kind(2)));
                    }
                    _ => {
                        panic!("Expected kind mismatch");
                    }
                }
            }
        }
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

impl fmt::Display for Path<'_> {
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
