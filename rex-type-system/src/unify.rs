use std::collections::HashMap;

use rex_ast::id::Id;

use crate::types::Type;

pub type Subst = HashMap<Id, Type>;

pub fn unify(t1: &Type, t2: &Type, subst: &mut Subst) -> Result<(), String> {
    // First apply any existing substitutions
    let t1 = apply_subst(t1, subst);
    let t2 = apply_subst(t2, subst);

    match (t1, t2) {
        // Base types must match exactly
        (Type::Int, Type::Int) => Ok(()),
        (Type::Bool, Type::Bool) => Ok(()),

        // Tuples
        (Type::Tuple(ts1), Type::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                return Err("Tuple lengths do not match".to_string());
            }

            for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                unify(t1, t2, subst)?;
            }

            Ok(())
        }

        // Lists
        (Type::List(t1), Type::List(t2)) => unify(&t1, &t2, subst),

        // For function types, unify arguments and results
        (Type::Arrow(a1, b1), Type::Arrow(a2, b2)) => {
            unify(&a1, &a2, subst)?;
            unify(&b1, &b2, subst)
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

        // Everything else fails
        (t1, t2) => Err(format!("Cannot unify {:?} with {:?}", t1, t2)),
    }
}

pub fn apply_subst(t: &Type, subst: &Subst) -> Type {
    match t {
        Type::Var(v) => {
            if let Some(t2) = subst.get(v) {
                apply_subst(t2, subst)
            } else {
                t.clone()
            }
        }
        Type::ForAll(id, ty) => Type::ForAll(*id, Box::new(apply_subst(ty, subst))),
        Type::Arrow(a, b) => Type::Arrow(
            Box::new(apply_subst(a, subst)),
            Box::new(apply_subst(b, subst)),
        ),
        Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| apply_subst(t, subst)).collect()),
        Type::List(elem) => Type::List(Box::new(apply_subst(elem, subst))),
        Type::Int | Type::Bool => t.clone(),
    }
}

pub fn occurs_check(var: Id, t: &Type) -> bool {
    match t {
        Type::Var(v) => *v == var,
        Type::ForAll(id, ty) => {
            // If we're looking for the same variable that's quantified,
            // then it doesn't occur freely (it's bound)
            if *id == var {
                false
            } else {
                occurs_check(var, ty)
            }
        }
        Type::Arrow(a, b) => occurs_check(var, a) || occurs_check(var, b),
        Type::List(elem) => occurs_check(var, elem),
        Type::Tuple(elems) => elems.iter().any(|t| occurs_check(var, t)),
        Type::Int | Type::Bool => false,
        _ => false,
    }
}

// Test cases
#[cfg(test)]
mod tests {
    use rex_ast::id::Id;

    use super::*;

    #[test]
    fn test_basic_unification() {
        let mut subst = HashMap::new();

        // Test case 1: α = Int
        let t1 = Type::Var(Id(0));
        let t2 = Type::Int;
        assert!(unify(&t1, &t2, &mut subst).is_ok());
        assert_eq!(apply_subst(&Type::Var(Id(0)), &subst), Type::Int);

        // Test case 2: (α -> β) = (Int -> Bool)
        let t1 = Type::Arrow(Box::new(Type::Var(Id(0))), Box::new(Type::Var(Id(1))));
        let t2 = Type::Arrow(Box::new(Type::Int), Box::new(Type::Bool));
        assert!(unify(&t1, &t2, &mut subst).is_ok());
        assert_eq!(apply_subst(&Type::Var(Id(0)), &subst), Type::Int);
        assert_eq!(apply_subst(&Type::Var(Id(1)), &subst), Type::Bool);
    }

    #[test]
    fn test_var_unification() {
        let mut subst = HashMap::new();

        // Unify α = β
        let t1 = Type::Var(Id(0));
        let t2 = Type::Var(Id(1));
        assert!(unify(&t1, &t2, &mut subst).is_ok());

        // Now α should be mapped to β
        assert_eq!(apply_subst(&Type::Var(Id(0)), &subst), Type::Var(Id(1)));
    }

    #[test]
    fn test_function_unification() {
        let mut subst = HashMap::new();

        // f : α -> β
        let f_type = Type::Arrow(
            Box::new(Type::Var(Id(0))), // α
            Box::new(Type::Var(Id(1))), // β
        );

        // g : γ -> γ
        let g_type = Type::Arrow(
            Box::new(Type::Var(Id(2))), // γ
            Box::new(Type::Var(Id(2))), // γ (same type)
        );

        // Unify f with g directly
        assert!(unify(&f_type, &g_type, &mut subst).is_ok());

        // After unification:
        let final_f = apply_subst(&f_type, &subst);
        let final_g = apply_subst(&g_type, &subst);

        // Both should be unified to the same type: γ -> γ
        assert_eq!(final_f, final_g);

        // And specifically they should both be γ -> γ
        assert_eq!(
            final_f,
            Type::Arrow(Box::new(Type::Var(Id(2))), Box::new(Type::Var(Id(2))))
        );
    }

    #[test]
    fn test_composition_unification() {
        let mut subst = HashMap::new();

        // f : α -> β
        let f_type = Type::Arrow(
            Box::new(Type::Var(Id(0))), // α
            Box::new(Type::Var(Id(1))), // β
        );

        // g : γ -> γ
        let g_type = Type::Arrow(
            Box::new(Type::Var(Id(2))), // γ
            Box::new(Type::Var(Id(2))), // γ (same type)
        );

        // Now unify g's output with f's input
        let g_output = Type::Var(Id(2)); // γ
        let f_input = Type::Var(Id(0)); // α
        assert!(unify(&g_output, &f_input, &mut subst).is_ok());

        // Let's make g take an Int
        assert!(unify(&Type::Var(Id(2)), &Type::Int, &mut subst).is_ok());

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
            Type::Arrow(Box::new(Type::Int), Box::new(Type::Var(Id(1))))
        );
    }

    #[test]
    fn test_higher_order_unification() {
        let mut subst = HashMap::new();

        // f : (α -> β) -> γ
        let f_type = Type::Arrow(
            Box::new(Type::Arrow(
                Box::new(Type::Var(Id(0))), // α
                Box::new(Type::Var(Id(1))), // β
            )),
            Box::new(Type::Var(Id(2))), // γ
        );

        // g : (Int -> Bool) -> δ
        let g_type = Type::Arrow(
            Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Bool))),
            Box::new(Type::Var(Id(3))), // δ
        );

        // Unify f with g
        assert!(unify(&f_type, &g_type, &mut subst).is_ok());

        // After unification:
        let final_f = apply_subst(&f_type, &subst);

        // final_f should be (Int -> Bool) -> δ
        assert_eq!(
            final_f,
            Type::Arrow(
                Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Bool))),
                Box::new(Type::Var(Id(3)))
            )
        );
    }

    #[test]
    fn test_occurs_check() {
        let var = Id(0);

        // Simple variable occurrence
        assert!(occurs_check(var, &Type::Var(Id(0))));
        assert!(!occurs_check(var, &Type::Var(Id(1))));

        // Arrow type occurrences
        assert!(occurs_check(
            var,
            &Type::Arrow(Box::new(Type::Var(Id(0))), Box::new(Type::Int))
        ));
        assert!(occurs_check(
            var,
            &Type::Arrow(Box::new(Type::Int), Box::new(Type::Var(Id(0))))
        ));

        // Tuple occurrences - would have failed before our fix
        assert!(occurs_check(
            var,
            &Type::Tuple(vec![Type::Int, Type::Var(Id(0)), Type::Bool])
        ));
        assert!(!occurs_check(
            var,
            &Type::Tuple(vec![Type::Int, Type::Var(Id(1)), Type::Bool])
        ));

        // List occurrences - would have failed before our fix
        assert!(occurs_check(var, &Type::List(Box::new(Type::Var(Id(0))))));
        assert!(!occurs_check(var, &Type::List(Box::new(Type::Var(Id(1))))));

        // Nested structures - would have failed before our fix
        assert!(occurs_check(
            var,
            &Type::Tuple(vec![
                Type::List(Box::new(Type::Var(Id(0)))),
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
            &Type::ForAll(Id(0), Box::new(Type::Var(Id(0))))
        ));

        // Case 2: The variable we're looking for occurs freely in the body
        let var2 = Id(1);
        assert!(occurs_check(
            var2,
            &Type::ForAll(
                Id(0),
                Box::new(Type::Arrow(
                    Box::new(Type::Var(Id(1))),
                    Box::new(Type::Var(Id(0)))
                ))
            )
        ));
    }
}
