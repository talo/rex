use std::collections::{HashMap, HashSet};

use rex_ast::id::Id;

use crate::{
    constraint::{Constraint, ConstraintSystem},
    types::{ADTVariant, Type, ADT},
};

pub type Subst = HashMap<Id, Type>;

// NOTE(loong): We do not support overloaded parametric polymorphism.
pub fn unify_constraints(constraint_system: &ConstraintSystem) -> Result<Subst, String> {
    let mut subst = Subst::new();
    // FIXME(loong): loop until no more progress is made in resolving
    // constraints instead of dumbly looping 100 times.
    for _ in 1..100 {
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(t1, t2) => {
                    unify_eq(t1, t2, &mut subst)?;
                }
                Constraint::OneOf(..) => {}
            }
        }
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(..) => {}
                Constraint::OneOf(t1, t2_possibilties) => {
                    // TODO(loong): doing the naive thing of ? short-circuiting this
                    // result does not work. Because it will cause issues for unused
                    // overloaded type variables. This is because we are resolving
                    // all constraints, not just the ones that are actually used by
                    // the expression.
                    unify_one_of(t1, t2_possibilties, &mut subst)?;
                }
            }
        }
    }
    Ok(subst)
}

pub fn unify_eq(t1: &Type, t2: &Type, subst: &mut Subst) -> Result<(), String> {
    // First apply any existing substitutions
    let t1 = apply_subst(t1, subst);
    let t2 = apply_subst(t2, subst);

    match (t1, t2) {
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
                return Err("Tuple lengths do not match".to_string());
            }

            for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                unify_eq(t1, t2, subst)?;
            }

            Ok(())
        }

        // Lists
        (Type::List(t1), Type::List(t2)) => unify_eq(&t1, &t2, subst),

        // Dictionaries
        (Type::Dict(d1), Type::Dict(d2)) => {
            if d1.len() != d2.len() {
                return Err(format!(
                    "Cannot unify {} with {}: different no. of keys",
                    Type::Dict(d1),
                    Type::Dict(d2)
                ));
            }
            for (key, entry1) in d1.iter() {
                if let Some(entry2) = d2.get(key) {
                    unify_eq(entry1, entry2, subst)?;
                } else {
                    return Err(format!(
                        "Cannot unify {} with {}: different keys",
                        Type::Dict(d1),
                        Type::Dict(d2)
                    ));
                }
            }
            Ok(())
        }

        // For function types, unify arguments and results
        (Type::Arrow(a1, b1), Type::Arrow(a2, b2)) => {
            unify_eq(&a1, &a2, subst)?;
            unify_eq(&b1, &b2, subst)
        }

        // Result
        (Type::Result(a1, b1), Type::Result(a2, b2)) => {
            unify_eq(&a1, &a2, subst)?;
            unify_eq(&b1, &b2, subst)
        }

        // Option
        (Type::Option(a1), Type::Option(a2)) => unify_eq(&a1, &a2, subst),

        // Type variable case requires occurs check
        (Type::Var(v1), Type::Var(v2)) => {
            if v1 != v2 {
                subst.insert(v1, Type::Var(v2));
            }
            Ok(())
        }

        // Type variable case requires occurs check
        (Type::Var(v), t) | (t, Type::Var(v)) => {
            if occurs_check(v, &t) {
                Err("Occurs check failed".to_string())
            } else {
                subst.insert(v, t);
                Ok(())
            }
        }

        // ADTs
        (Type::ADT(adt1), Type::ADT(adt2)) => {
            if adt1.name != adt2.name {
                return Err(format!(
                    "Cannot unify {} with {}",
                    Type::ADT(adt1),
                    Type::ADT(adt2)
                ));
            }

            if adt1.variants.len() != adt2.variants.len() {
                return Err(format!(
                    "Cannot unify {} with {}",
                    Type::ADT(adt1),
                    Type::ADT(adt2)
                ));
            }

            for i in 0..adt1.variants.len() {
                let v1 = &adt1.variants[i];
                let v2 = &adt2.variants[i];

                if v1.name != v2.name {
                    return Err(format!(
                        "Cannot unify {} with {}",
                        Type::ADT(adt1),
                        Type::ADT(adt2)
                    ));
                }

                match (&v1.t, &v2.t) {
                    (None, None) => (),
                    (Some(vt1), Some(vt2)) => {
                        unify_eq(vt1, vt2, subst)?;
                    }
                    _ => {
                        return Err(format!(
                            "Cannot unify {} with {}",
                            Type::ADT(adt1),
                            Type::ADT(adt2)
                        ));
                    }
                }
            }

            return Ok(());
        }

        // Everything else fails
        (t1, t2) => Err(format!("Cannot unify {} with {}", t1, t2)),
    }
}

pub fn unify_one_of(
    t1: &Type,
    t2_possibilities: &HashSet<Type>,
    subst: &mut Subst,
) -> Result<(), String> {
    // First apply any existing substitutions
    let t1 = apply_subst(t1, subst);

    let mut successes = Vec::new();

    // Try unifying with each possibility
    for t2 in t2_possibilities {
        let t2 = apply_subst(t2, subst);
        let mut test_subst = subst.clone();

        if unify_eq(&t1, &t2, &mut test_subst).is_ok() {
            successes.push((t2, test_subst));
        }
    }

    match successes.len() {
        0 => Err(format!(
            "Cannot unify {} with incompatible candidates [{}]",
            t1,
            t2_possibilities
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )),
        1 => {
            // Use the successful substitution
            *subst = successes[0].1.clone();
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn apply_subst(t: &Type, subst: &Subst) -> Type {
    match t {
        Type::UnresolvedVar(_) => todo!("apply_subst should return a result"),
        Type::Var(v) => {
            if let Some(t2) = subst.get(v) {
                apply_subst(t2, subst)
            } else {
                t.clone()
            }
        }
        Type::ForAll(id, ty, deps) => {
            Type::ForAll(*id, Box::new(apply_subst(ty, subst)), deps.clone())
        }

        Type::ADT(adt) => Type::ADT(ADT {
            docs: adt.docs.clone(),
            name: adt.name.clone(),
            variants: adt
                .variants
                .iter()
                .map(|v| ADTVariant {
                    docs: v.docs.clone(),
                    name: v.name.clone(),
                    t: v.t.as_ref().map(|t| Box::new(apply_subst(t, subst))),
                    t_docs: v.t_docs.clone(),
                })
                .collect(),
        }),
        Type::Arrow(a, b) => Type::Arrow(
            Box::new(apply_subst(a, subst)),
            Box::new(apply_subst(b, subst)),
        ),
        Type::Result(t, e) => Type::Result(
            Box::new(apply_subst(t, subst)),
            Box::new(apply_subst(e, subst)),
        ),
        Type::Option(t) => Type::Option(Box::new(apply_subst(t, subst))),
        Type::List(t) => Type::List(Box::new(apply_subst(t, subst))),
        Type::Dict(kts) => Type::Dict(
            kts.iter()
                .map(|(k, t)| (k.clone(), apply_subst(t, subst)))
                .collect(),
        ),
        Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| apply_subst(t, subst)).collect()),

        Type::Bool
        | Type::Uint
        | Type::Int
        | Type::Float
        | Type::String
        | Type::Uuid
        | Type::DateTime => t.clone(),
    }
}

pub fn occurs_check(var: Id, t: &Type) -> bool {
    match t {
        Type::UnresolvedVar(_) => false, // TODO(loong): should this function return a result?
        Type::Var(v) => *v == var,
        Type::ForAll(id, ty, _deps) => {
            // If we're looking for the same variable that's quantified,
            // then it doesn't occur freely (it's bound)
            if *id == var {
                false
            } else {
                occurs_check(var, ty)
            }
        }

        Type::ADT(adt) => adt.variants.iter().any(|v| match &v.t {
            Some(t) => occurs_check(var, t),
            None => false,
        }),
        Type::Arrow(a, b) => occurs_check(var, a) || occurs_check(var, b),
        Type::Result(t, e) => occurs_check(var, t) || occurs_check(var, e),
        Type::Option(t) => occurs_check(var, t),
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

        let mut subst = HashMap::new();

        // Test case 1: α = Int
        let t1 = Type::Var(alpha);
        let t2 = Type::Int;
        assert!(unify_eq(&t1, &t2, &mut subst).is_ok());
        assert_eq!(apply_subst(&Type::Var(alpha), &subst), Type::Int);

        // Test case 2: (α -> β) = (Int -> Bool)
        let t1 = Type::Arrow(Box::new(Type::Var(alpha)), Box::new(Type::Var(beta)));
        let t2 = Type::Arrow(Box::new(Type::Int), Box::new(Type::Bool));
        assert!(unify_eq(&t1, &t2, &mut subst).is_ok());
        assert_eq!(apply_subst(&Type::Var(alpha), &subst), Type::Int);
        assert_eq!(apply_subst(&Type::Var(beta), &subst), Type::Bool);
    }

    #[test]
    fn test_var_unification() {
        let alpha = Id::new();
        let beta = Id::new();

        let mut subst = HashMap::new();

        // Unify α = β
        let t1 = Type::Var(alpha);
        let t2 = Type::Var(beta);
        assert!(unify_eq(&t1, &t2, &mut subst).is_ok());

        // Now α should be mapped to β
        assert_eq!(apply_subst(&Type::Var(alpha), &subst), Type::Var(beta));
    }

    #[test]
    fn test_function_unification() {
        let alpha = Id::new();
        let beta = Id::new();
        let gamma = Id::new();

        let mut subst = HashMap::new();

        // f : α -> β
        let f_type = Type::Arrow(
            Box::new(Type::Var(alpha)), // α
            Box::new(Type::Var(beta)),  // β
        );

        // g : γ -> γ
        let g_type = Type::Arrow(
            Box::new(Type::Var(gamma)), // γ
            Box::new(Type::Var(gamma)), // γ (same type)
        );

        // Unify f with g directly
        assert!(unify_eq(&f_type, &g_type, &mut subst).is_ok());

        // After unification:
        let final_f = apply_subst(&f_type, &subst);
        let final_g = apply_subst(&g_type, &subst);

        // Both should be unified to the same type: γ -> γ
        assert_eq!(final_f, final_g);

        // And specifically they should both be γ -> γ
        assert_eq!(
            final_f,
            Type::Arrow(Box::new(Type::Var(gamma)), Box::new(Type::Var(gamma)))
        );
    }

    #[test]
    fn test_composition_unification() {
        let alpha = Id::new();
        let beta = Id::new();
        let gamma = Id::new();

        let mut subst = HashMap::new();

        // f : α -> β
        let f_type = Type::Arrow(
            Box::new(Type::Var(alpha)), // α
            Box::new(Type::Var(beta)),  // β
        );

        // g : γ -> γ
        let g_type = Type::Arrow(
            Box::new(Type::Var(gamma)), // γ
            Box::new(Type::Var(gamma)), // γ (same type)
        );

        // Now unify g's output with f's input
        let g_output = Type::Var(gamma); // γ
        let f_input = Type::Var(alpha); // α
        assert!(unify_eq(&g_output, &f_input, &mut subst).is_ok());

        // Let's make g take an Int
        assert!(unify_eq(&Type::Var(gamma), &Type::Int, &mut subst).is_ok());

        // After unification:
        let final_g = apply_subst(&g_type, &subst);
        let final_f = apply_subst(&f_type, &subst);

        // g should be Int -> Int
        assert_eq!(
            final_g,
            Type::Arrow(Box::new(Type::Int), Box::new(Type::Int))
        );

        // f should be Int -> β (where β is still free)
        assert_eq!(
            final_f,
            Type::Arrow(Box::new(Type::Int), Box::new(Type::Var(beta)))
        );
    }

    #[test]
    fn test_higher_order_unification() {
        let alpha = Id::new();
        let beta = Id::new();
        let gamma = Id::new();
        let delta = Id::new();

        let mut subst = HashMap::new();

        // f : (α -> β) -> γ
        let f_type = Type::Arrow(
            Box::new(Type::Arrow(
                Box::new(Type::Var(alpha)), // α
                Box::new(Type::Var(beta)),  // β
            )),
            Box::new(Type::Var(gamma)), // γ
        );

        // g : (Int -> Bool) -> δ
        let g_type = Type::Arrow(
            Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Bool))),
            Box::new(Type::Var(delta)), // δ
        );

        // Unify f with g
        assert!(unify_eq(&f_type, &g_type, &mut subst).is_ok());

        // After unification:
        let final_f = apply_subst(&f_type, &subst);

        // final_f should be (Int -> Bool) -> δ
        assert_eq!(
            final_f,
            Type::Arrow(
                Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Bool))),
                Box::new(Type::Var(delta))
            )
        );
    }

    #[test]
    fn test_occurs_check() {
        let alpha = Id::new();
        let beta = Id::new();

        let var = alpha.clone();

        // Simple variable occurrence
        assert!(occurs_check(var, &Type::Var(alpha)));
        assert!(!occurs_check(var, &Type::Var(beta)));

        // Arrow type occurrences
        assert!(occurs_check(
            var,
            &Type::Arrow(Box::new(Type::Var(alpha)), Box::new(Type::Int))
        ));
        assert!(occurs_check(
            var,
            &Type::Arrow(Box::new(Type::Int), Box::new(Type::Var(alpha)))
        ));

        // Tuple occurrences - would have failed before our fix
        assert!(occurs_check(
            var,
            &Type::Tuple(vec![Type::Int, Type::Var(alpha), Type::Bool])
        ));
        assert!(!occurs_check(
            var,
            &Type::Tuple(vec![Type::Int, Type::Var(beta), Type::Bool])
        ));

        // List occurrences - would have failed before our fix
        assert!(occurs_check(var, &Type::List(Box::new(Type::Var(alpha)))));
        assert!(!occurs_check(var, &Type::List(Box::new(Type::Var(beta)))));

        // Nested structures - would have failed before our fix
        assert!(occurs_check(
            var,
            &Type::Tuple(vec![
                Type::List(Box::new(Type::Var(alpha))),
                Type::Arrow(
                    Box::new(Type::Int),
                    Box::new(Type::List(Box::new(Type::Bool)))
                )
            ])
        ));

        // ForAll cases - would have failed before our fix
        // Case 1: The variable we're looking for is bound by the ForAll
        assert!(!occurs_check(
            var,
            &Type::ForAll(alpha, Box::new(Type::Var(alpha)), BTreeSet::new())
        ));

        // Case 2: The variable we're looking for occurs freely in the body
        let var2 = beta;
        assert!(occurs_check(
            var2,
            &Type::ForAll(
                alpha,
                Box::new(Type::Arrow(
                    Box::new(Type::Var(beta)),
                    Box::new(Type::Var(alpha))
                )),
                BTreeSet::new()
            )
        ));
    }
}
