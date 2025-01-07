use std::collections::{HashMap, HashSet};

use rex_ast::id::{Id, IdDispenser};

use crate::{expr::Expr, types::Type, unify};

#[derive(Debug, Clone)]
pub struct Constraint(Type, Type);

// Generate constraints from an expression
fn generate_constraints(
    expr: &Expr,
    env: &HashMap<String, Type>,
    id_dispenser: &mut IdDispenser,
) -> Result<(Type, Vec<Constraint>), String> {
    match expr {
        Expr::Var(name) => {
            if let Some(t) = env.get(name) {
                // If type is polymorphic, instantiate it
                Ok(match t {
                    Type::ForAll(_, _) => (instantiate(t, id_dispenser), vec![]),
                    _ => (t.clone(), vec![]),
                })
            } else {
                panic!("Unbound variable: {}", name)
            }
        }

        Expr::Tuple(exprs) => {
            let mut all_constraints = Vec::new();
            let mut types = Vec::new();

            // Generate constraints for each expression in the tuple
            for expr in exprs {
                let (ty, mut constraints) = generate_constraints(expr, env, id_dispenser)?;
                types.push(ty);
                all_constraints.append(&mut constraints);
            }

            Ok((Type::Tuple(types), all_constraints))
        }

        Expr::App(f, x) => {
            let (f_type, mut f_constraints) = generate_constraints(f, env, id_dispenser)?;
            let (x_type, mut x_constraints) = generate_constraints(x, env, id_dispenser)?;

            let result_type = Type::Var(id_dispenser.next());

            let expected_f_type =
                Type::Arrow(Box::new(x_type.clone()), Box::new(result_type.clone()));

            let mut constraints = vec![Constraint(f_type, expected_f_type)];
            constraints.append(&mut f_constraints);
            constraints.append(&mut x_constraints);

            Ok((result_type, constraints))
        }

        Expr::Lam(param, body) => {
            let param_type = Type::Var(id_dispenser.next());

            let mut new_env = env.clone();
            new_env.insert(param.clone(), param_type.clone());

            let (body_type, body_constraints) = generate_constraints(body, &new_env, id_dispenser)?;

            let result_type = Type::Arrow(Box::new(param_type), Box::new(body_type));

            Ok((result_type, body_constraints))
        }

        Expr::Let(name, def, body) => {
            // First generate constraints for the definition
            let (def_type, def_constraints) = generate_constraints(def, env, id_dispenser)?;

            // Solve definition constraints to get its type
            let mut def_subst = HashMap::new();
            for constraint in &def_constraints {
                unify::unify(&constraint.0, &constraint.1, &mut def_subst)?;
            }
            let solved_def_type = unify::apply_subst(&def_type, &def_subst);

            // Generalize the type
            let gen_type = generalize(env, &solved_def_type);

            // Add generalized type to environment
            let mut new_env = env.clone();
            new_env.insert(name.clone(), gen_type);

            // Generate constraints for the body with the new environment
            generate_constraints(body, &new_env, id_dispenser)
        }

        Expr::Ite(cond, then_branch, else_branch) => {
            // Generate constraints for all parts
            let (cond_type, mut cond_constraints) = generate_constraints(cond, env, id_dispenser)?;
            let (then_type, mut then_constraints) =
                generate_constraints(then_branch, env, id_dispenser)?;
            let (else_type, mut else_constraints) =
                generate_constraints(else_branch, env, id_dispenser)?;

            // Condition must be boolean
            let mut constraints = vec![
                Constraint(cond_type, Type::Bool),
                // Then and else branches must have the same type
                Constraint(then_type.clone(), else_type),
            ];

            // Combine all constraints
            constraints.append(&mut cond_constraints);
            constraints.append(&mut then_constraints);
            constraints.append(&mut else_constraints);

            Ok((then_type, constraints))
        }
    }
}

// For generalization, we need to find free type variables in a type
fn free_vars(ty: &Type) -> HashSet<Id> {
    match ty {
        Type::Var(id) => {
            let mut set = HashSet::new();
            set.insert(*id);
            set
        }
        Type::Arrow(a, b) => {
            let mut set = free_vars(a);
            set.extend(free_vars(b));
            set
        }
        Type::Int | Type::Bool => HashSet::new(),
        Type::Tuple(types) => {
            let mut vars = HashSet::new();
            for ty in types {
                vars.extend(free_vars(ty));
            }
            vars
        }
        Type::List(ty) => free_vars(ty),
        Type::ForAll(id, ty) => {
            let mut set = free_vars(ty);
            set.remove(id);
            set
        }
    }
}

// For generalization, we also need to know which variables are free in the environment
fn env_free_vars(env: &HashMap<String, Type>) -> HashSet<Id> {
    let mut vars = HashSet::new();
    for ty in env.values() {
        vars.extend(free_vars(ty));
    }
    vars
}

// Generalize a type by quantifying over any type variables that aren't free in the environment
fn generalize(env: &HashMap<String, Type>, ty: &Type) -> Type {
    let env_vars = env_free_vars(env);
    let ty_vars = free_vars(ty);

    // Find variables that are free in ty but not in env
    let mut to_quantify: Vec<_> = ty_vars.difference(&env_vars).cloned().collect();
    to_quantify.sort(); // Make generalization deterministic

    // Build the type from inside out
    let mut result = ty.clone();
    for var in to_quantify {
        result = Type::ForAll(var, Box::new(result));
    }
    result
}

// Instantiate a type by replacing quantified variables with fresh ones
fn instantiate(ty: &Type, id_dispenser: &mut IdDispenser) -> Type {
    let mut subst = HashMap::new();

    fn inst_helper(
        ty: &Type,
        subst: &mut HashMap<Id, Type>,
        id_dispenser: &mut IdDispenser,
    ) -> Type {
        let result = match ty {
            Type::ForAll(id, ty) => {
                // Create fresh type variable
                let fresh_id = id_dispenser.next();
                subst.insert(*id, Type::Var(fresh_id));
                inst_helper(ty, subst, id_dispenser)
            }
            Type::Var(id) => match subst.get(id) {
                Some(t) => t.clone(),
                None => Type::Var(*id),
            },
            Type::Arrow(arg, ret) => {
                let new_arg = inst_helper(arg, subst, id_dispenser);
                let new_ret = inst_helper(ret, subst, id_dispenser);
                Type::Arrow(Box::new(new_arg), Box::new(new_ret))
            }
            Type::List(elem) => {
                let new_elem = inst_helper(elem, subst, id_dispenser);
                Type::List(Box::new(new_elem))
            }
            Type::Tuple(elems) => Type::Tuple(
                elems
                    .iter()
                    .map(|t| inst_helper(t, subst, id_dispenser))
                    .collect(),
            ),
            Type::Int | Type::Bool => ty.clone(),
        };
        result
    }

    inst_helper(ty, &mut subst, id_dispenser)
}

#[cfg(test)]
mod tests {
    use crate::unify;

    use super::*;

    #[test]
    fn test_free_vars() {
        // α -> β
        let ty = Type::Arrow(Box::new(Type::Var(Id(0))), Box::new(Type::Var(Id(1))));
        let vars = free_vars(&ty);
        assert_eq!(vars.len(), 2);
        assert!(vars.contains(&Id(0)));
        assert!(vars.contains(&Id(1)));

        // ∀α. α -> β
        let ty = Type::ForAll(
            Id(0),
            Box::new(Type::Arrow(
                Box::new(Type::Var(Id(0))),
                Box::new(Type::Var(Id(1))),
            )),
        );
        let vars = free_vars(&ty);
        assert_eq!(vars.len(), 1);
        assert!(vars.contains(&Id(1)));
    }

    #[test]
    fn test_generalize() {
        let mut env = HashMap::new();

        // Environment with β free
        env.insert("y".to_string(), Type::Var(Id(1)));

        // Type to generalize: α -> β
        let ty = Type::Arrow(Box::new(Type::Var(Id(0))), Box::new(Type::Var(Id(1))));

        // Should become ∀α. α -> β
        // (β isn't quantified because it appears in env)
        let gen_ty = generalize(&env, &ty);
        match gen_ty {
            Type::ForAll(id, ty) => {
                assert_eq!(id, Id(0));
                match *ty {
                    Type::Arrow(arg, ret) => {
                        assert_eq!(*arg, Type::Var(Id(0)));
                        assert_eq!(*ret, Type::Var(Id(1)));
                    }
                    _ => panic!("Expected arrow type"),
                }
            }
            _ => panic!("Expected forall type"),
        }
    }

    #[test]
    fn test_instantiate() {
        let mut id_dispenser = IdDispenser::new();

        let id_0 = id_dispenser.next();
        let id_1 = id_dispenser.next();

        // ∀α. α -> β
        let ty = Type::ForAll(
            id_0,
            Box::new(Type::Arrow(
                Box::new(Type::Var(id_0)),
                Box::new(Type::Var(id_1)),
            )),
        );

        let inst_ty = instantiate(&ty, &mut id_dispenser);

        // Should become γ -> β where γ is fresh
        match inst_ty {
            Type::Arrow(arg, ret) => {
                assert_eq!(*arg, Type::Var(Id(2))); // Fresh variable
                assert_eq!(*ret, Type::Var(id_1)); // Original free variable
            }
            _ => panic!("Expected arrow type"),
        }
    }

    #[test]
    fn test_list_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = HashMap::new();

        // Set up environment with polymorphic head : ∀α. [α] -> α
        let elem_type = id_dispenser.next();
        let head_type = Type::ForAll(
            elem_type,
            Box::new(Type::Arrow(
                Box::new(Type::List(Box::new(Type::Var(elem_type)))),
                Box::new(Type::Var(elem_type)),
            )),
        );
        println!("HEAD TYPE: {:?}", head_type);
        env.insert("head".to_string(), head_type);

        // Add example lists to environment
        env.insert("int_list".to_string(), Type::List(Box::new(Type::Int)));
        env.insert("bool_list".to_string(), Type::List(Box::new(Type::Bool)));

        let expr = Expr::Let(
            "head".to_string(),
            Box::new(Expr::Var("head".to_string())),
            Box::new(Expr::Tuple(vec![
                Expr::App(
                    Box::new(Expr::Var("head".to_string())),
                    Box::new(Expr::Var("int_list".to_string())),
                ),
                Expr::App(
                    Box::new(Expr::Var("head".to_string())),
                    Box::new(Expr::Var("bool_list".to_string())),
                ),
            ])),
        );

        let (ty, constraints) = generate_constraints(&expr, &env, &mut id_dispenser)?;
        println!("CONSTRAINTS: {:?}", constraints);

        // Solve constraints
        let mut subst = HashMap::new();
        for constraint in &constraints {
            println!(
                "TRYING TO UNIFY: {:?} with {:?}",
                constraint.0, constraint.1
            );
            unify::unify(&constraint.0, &constraint.1, &mut subst)?;
            println!("SUBST AFTER: {:?}", subst);
        }

        let final_type = unify::apply_subst(&ty, &subst);
        println!("FINAL TYPE: {:?}", final_type);

        assert_eq!(final_type, Type::Tuple(vec![Type::Int, Type::Bool]));

        Ok(())
    }

    #[test]
    fn test_let() {
        let mut id_dispenser = IdDispenser::new();
        let mut env = HashMap::new();

        // Test expression: let id = (\x -> x) in id 1
        let expr = Expr::Let(
            "id".to_string(),
            Box::new(Expr::Lam(
                "x".to_string(),
                Box::new(Expr::Var("x".to_string())),
            )),
            Box::new(Expr::App(
                Box::new(Expr::Var("id".to_string())),
                Box::new(Expr::Var("one".to_string())),
            )),
        );

        env.insert("one".to_string(), Type::Int);

        let (result_type, constraints) =
            generate_constraints(&expr, &env, &mut id_dispenser).unwrap();

        // Solve constraints
        let mut subst = HashMap::new();
        for Constraint(t1, t2) in constraints {
            assert!(unify::unify(&t1, &t2, &mut subst).is_ok());
        }

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Type::Int);
    }

    #[test]
    fn test_let_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = HashMap::new();

        // let id = \x -> x
        // in (id 1, id true)
        let expr = Expr::Let(
            "id".to_string(),
            // \x -> x
            Box::new(Expr::Lam(
                "x".to_string(),
                Box::new(Expr::Var("x".to_string())),
            )),
            // Create tuple of (id 1, id true)
            Box::new(Expr::Tuple(vec![
                Expr::App(
                    Box::new(Expr::Var("id".to_string())),
                    Box::new(Expr::Var("int_val".to_string())),
                ),
                Expr::App(
                    Box::new(Expr::Var("id".to_string())),
                    Box::new(Expr::Var("bool_val".to_string())),
                ),
            ])),
        );

        // Set up environment
        env.insert("int_val".to_string(), Type::Int);
        env.insert("bool_val".to_string(), Type::Bool);

        let (ty, constraints) = generate_constraints(&expr, &env, &mut id_dispenser)?;

        // Solve constraints
        let mut subst = HashMap::new();
        for Constraint(t1, t2) in constraints {
            unify::unify(&t1, &t2, &mut subst)?;
        }

        // The final type should be (Int, Bool)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Tuple(vec![Type::Int, Type::Bool]));

        Ok(())
    }

    #[test]
    fn test_if_then_else() {
        let mut id_dispenser = IdDispenser::new();
        let mut env = HashMap::new();

        // Test expression: if true then 1 else 2
        let expr = Expr::Ite(
            Box::new(Expr::Var("true".to_string())),
            Box::new(Expr::Var("one".to_string())),
            Box::new(Expr::Var("two".to_string())),
        );

        // Set up environment
        env.insert("true".to_string(), Type::Bool);
        env.insert("one".to_string(), Type::Int);
        env.insert("two".to_string(), Type::Int);

        // Generate constraints
        let (result_type, constraints) =
            generate_constraints(&expr, &env, &mut id_dispenser).unwrap();

        // Solve constraints
        let mut subst = HashMap::new();
        for Constraint(t1, t2) in constraints {
            assert!(unify::unify(&t1, &t2, &mut subst).is_ok());
        }

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Type::Int);
    }

    #[test]
    fn test_compose_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = HashMap::new();

        // let compose = \f -> \g -> \x -> f (g x)
        // in (
        //   compose not not true,  // Bool -> Bool -> Bool -> Bool
        //   compose (\x -> x + 1) (\x -> x + 2) 3  // Int -> Int -> Int -> Int
        // )
        let expr = Expr::Let(
            "compose".to_string(),
            Box::new(Expr::Lam(
                "f".to_string(),
                Box::new(Expr::Lam(
                    "g".to_string(),
                    Box::new(Expr::Lam(
                        "x".to_string(),
                        Box::new(Expr::App(
                            Box::new(Expr::Var("f".to_string())),
                            Box::new(Expr::App(
                                Box::new(Expr::Var("g".to_string())),
                                Box::new(Expr::Var("x".to_string())),
                            )),
                        )),
                    )),
                )),
            )),
            Box::new(Expr::Tuple(vec![
                // compose not not true
                Expr::App(
                    Box::new(Expr::App(
                        Box::new(Expr::App(
                            Box::new(Expr::Var("compose".to_string())),
                            Box::new(Expr::Var("not".to_string())),
                        )),
                        Box::new(Expr::Var("not".to_string())),
                    )),
                    Box::new(Expr::Var("bool_val".to_string())),
                ),
                // compose (+1) (+2) 3
                Expr::App(
                    Box::new(Expr::App(
                        Box::new(Expr::App(
                            Box::new(Expr::Var("compose".to_string())),
                            Box::new(Expr::Var("inc".to_string())),
                        )),
                        Box::new(Expr::Var("inc2".to_string())),
                    )),
                    Box::new(Expr::Var("int_val".to_string())),
                ),
            ])),
        );

        // Set up environment
        env.insert(
            "not".to_string(),
            Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool)),
        );
        env.insert(
            "inc".to_string(),
            Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)),
        );
        env.insert(
            "inc2".to_string(),
            Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)),
        );
        env.insert("bool_val".to_string(), Type::Bool);
        env.insert("int_val".to_string(), Type::Int);

        let (ty, constraints) = generate_constraints(&expr, &env, &mut id_dispenser)?;

        // Solve constraints
        let mut subst = HashMap::new();
        for constraint in constraints {
            unify::unify(&constraint.0, &constraint.1, &mut subst)?;
        }

        // The final type should be (Bool, Int)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Tuple(vec![Type::Bool, Type::Int]));

        Ok(())
    }

    #[test]
    fn test_polymorphic_type_errors() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = HashMap::new();

        // let id = \x -> x in
        // (id true, id 1, id true + 1)  // Last one should fail!
        let expr = Expr::Let(
            "id".to_string(),
            Box::new(Expr::Lam(
                "x".to_string(),
                Box::new(Expr::Var("x".to_string())),
            )),
            Box::new(Expr::Tuple(vec![
                // id true - ok
                Expr::App(
                    Box::new(Expr::Var("id".to_string())),
                    Box::new(Expr::Var("bool_val".to_string())),
                ),
                // id 1 - ok
                Expr::App(
                    Box::new(Expr::Var("id".to_string())),
                    Box::new(Expr::Var("int_val".to_string())),
                ),
                // id true + 1 - should fail!
                Expr::App(
                    Box::new(Expr::Var("plus".to_string())),
                    Box::new(Expr::App(
                        Box::new(Expr::Var("id".to_string())),
                        Box::new(Expr::Var("bool_val".to_string())),
                    )),
                ),
            ])),
        );

        // Set up environment
        env.insert("bool_val".to_string(), Type::Bool);
        env.insert("int_val".to_string(), Type::Int);
        env.insert(
            "plus".to_string(),
            Type::Arrow(
                Box::new(Type::Int),
                Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Int))),
            ),
        );

        // This should fail with a type error
        let result =
            generate_constraints(&expr, &env, &mut id_dispenser).and_then(|(ty, constraints)| {
                let mut subst = HashMap::new();
                for constraint in constraints {
                    unify::unify(&constraint.0, &constraint.1, &mut subst)?;
                }
                Ok(unify::apply_subst(&ty, &subst))
            });

        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Cannot unify Bool with Int"));

        Ok(())
    }
}
