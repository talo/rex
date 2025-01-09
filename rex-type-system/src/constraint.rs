use std::collections::{HashMap, HashSet};

use rex_ast::{
    expr::Expr,
    id::{Id, IdDispenser},
};

use crate::{
    types::{ADTVariant, ExprTypeEnv, Type, TypeEnv, ADT},
    unify,
};

#[derive(Debug, Clone)]
pub enum Constraint {
    Eq(Type, Type),
    OneOf(Type, Vec<Type>),
}

// Generate constraints from an expression
pub fn generate_constraints(
    expr: &Expr,
    env: &TypeEnv,
    expr_env: &mut ExprTypeEnv,
    constraints: &mut Vec<Constraint>,
    id_dispenser: &mut IdDispenser,
) -> Result<Type, String> {
    match expr {
        Expr::Var(var) => match env.get(&var.name) {
            Some(t) => match t {
                Type::ForAll(_, _) => {
                    let instantiated_type = instantiate(t, id_dispenser);
                    expr_env.insert(var.id, instantiated_type.clone());
                    Ok(instantiated_type)
                }
                _ => {
                    expr_env.insert(var.id, t.clone());
                    Ok(t.clone())
                }
            },
            None => Err(format!("Unbound variable: {}", &var.name)),
        },

        Expr::Dict(_id, _span, exprs) => {
            let mut types = Vec::new();

            // Generate constraints for each expression in the tuple
            for (_key, expr) in exprs {
                let ty = generate_constraints(expr, env, expr_env, constraints, id_dispenser)?;
                types.push(ty);
            }

            Ok(Type::Tuple(types))
        }

        Expr::Tuple(_id, _span, exprs) => {
            let mut types = Vec::new();

            // Generate constraints for each expression in the tuple
            for expr in exprs {
                let ty = generate_constraints(expr, env, expr_env, constraints, id_dispenser)?;
                types.push(ty);
            }

            Ok(Type::Tuple(types))
        }

        Expr::List(_id, _span, exprs) => {
            // If list is empty, create a fresh type variable for element type
            if exprs.is_empty() {
                let elem_ty = Type::Var(id_dispenser.next());
                return Ok(Type::List(Box::new(elem_ty)));
            }

            // Generate constraints for all expressions
            let mut types = Vec::new();
            for expr in exprs {
                let ty = generate_constraints(expr, env, expr_env, constraints, id_dispenser)?;
                types.push(ty);
            }

            // Add constraints that all elements must have the same type
            for ty in &types[1..] {
                constraints.push(Constraint::Eq(types[0].clone(), ty.clone()));
            }

            Ok(Type::List(Box::new(types[0].clone())))
        }

        Expr::App(id, _span, f, x) => {
            let f_type = generate_constraints(f, env, expr_env, constraints, id_dispenser)?;
            let x_type = generate_constraints(x, env, expr_env, constraints, id_dispenser)?;

            let result_type = Type::Var(id_dispenser.next());

            let expected_f_type =
                Type::Arrow(Box::new(x_type.clone()), Box::new(result_type.clone()));

            constraints.push(Constraint::Eq(f_type, expected_f_type));

            expr_env.insert(*id, result_type.clone());
            Ok(result_type)
        }

        Expr::Lam(_id, _span, param, body) => {
            let param_type = Type::Var(id_dispenser.next());

            let mut new_env = env.clone();
            new_env.insert(param.name.clone(), param_type.clone());

            // TODO(loong): do we need to clone `expr_env` in the same way that
            // we do for `env`?
            let body_type =
                generate_constraints(body, &new_env, expr_env, constraints, id_dispenser)?;

            let result_type = Type::Arrow(Box::new(param_type), Box::new(body_type));

            Ok(result_type)
        }

        Expr::Let(_id, _span, var, def, body) => {
            // TODO(loong): should this be cloned from `constraints`, or should
            // it be fresh? A direct translation of the algorithm would make a
            // fresh list.
            let mut def_constraints = vec![];

            // First generate constraints for the definition
            let def_type =
                generate_constraints(def, env, expr_env, &mut def_constraints, id_dispenser)?;
            println!("DEF_TYPE: {:?}", def_type);
            println!("ENV: {:?}", env);

            // Solve definition constraints to get its type
            let mut def_subst = HashMap::new();
            for constraint in &def_constraints {
                match constraint {
                    Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut def_subst)?,
                    _ => panic!("Expected equality constraint"),
                }
            }
            let solved_def_type = unify::apply_subst(&def_type, &def_subst);
            println!("DEF_SUBST: {:?}", def_subst);
            println!("SOLVED_DEF_TYPE: {:?}", solved_def_type);

            // Generalize the type
            let gen_type = generalize(env, &solved_def_type);
            println!("GEN_TYPE: {:?}", gen_type);

            // Add generalized type to environment
            let mut new_env = env.clone();
            new_env.insert(var.name.clone(), gen_type);

            // Generate constraints for the body with the new environment
            generate_constraints(body, &new_env, expr_env, constraints, id_dispenser)
        }

        Expr::Ite(_id, _span, cond, then_branch, else_branch) => {
            // Generate constraints for all parts
            let cond_type = generate_constraints(cond, env, expr_env, constraints, id_dispenser)?;
            let then_type =
                generate_constraints(then_branch, env, expr_env, constraints, id_dispenser)?;
            let else_type =
                generate_constraints(else_branch, env, expr_env, constraints, id_dispenser)?;

            // Condition must be boolean
            constraints.push(Constraint::Eq(cond_type, Type::Bool));
            // Then and else branches must have the same type
            constraints.push(Constraint::Eq(then_type.clone(), else_type));

            Ok(then_type)
        }

        Expr::Bool(id, _span, _x) => {
            expr_env.insert(*id, Type::Bool);
            Ok(Type::Bool)
        }
        Expr::Uint(id, _span, _x) => {
            expr_env.insert(*id, Type::Uint);
            Ok(Type::Uint)
        }
        Expr::Int(id, _span, _x) => {
            expr_env.insert(*id, Type::Int);
            Ok(Type::Int)
        }
        Expr::Float(id, _span, _x) => {
            expr_env.insert(*id, Type::Float);
            Ok(Type::Float)
        }
        Expr::String(id, _span, _x) => {
            expr_env.insert(*id, Type::String);
            Ok(Type::String)
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
        Type::ForAll(id, ty) => {
            let mut set = free_vars(ty);
            set.remove(id);
            set
        }

        Type::ADT(adt) => {
            let mut set = HashSet::new();
            for variant in &adt.variants {
                if let Some(t) = &variant.t {
                    set.extend(free_vars(t));
                }
            }
            set
        }
        Type::Arrow(a, b) => {
            let mut set = free_vars(a);
            set.extend(free_vars(b));
            set
        }
        Type::Result(t, e) => {
            let mut set = free_vars(t);
            set.extend(free_vars(e));
            set
        }
        Type::Option(t) => free_vars(t),
        Type::List(t) => free_vars(t),
        Type::Dict(kts) => {
            let mut vars = HashSet::new();
            for t in kts.values() {
                vars.extend(free_vars(t));
            }
            vars
        }
        Type::Tuple(ts) => {
            let mut vars = HashSet::new();
            for t in ts {
                vars.extend(free_vars(t));
            }
            vars
        }

        Type::Bool | Type::Uint | Type::Int | Type::Float | Type::String => HashSet::new(),
    }
}

// For generalization, we also need to know which variables are free in the environment
fn env_free_vars(env: &TypeEnv) -> HashSet<Id> {
    let mut vars = HashSet::new();
    for ty in env.values() {
        vars.extend(free_vars(ty));
    }
    vars
}

// Generalize a type by quantifying over any type variables that aren't free in the environment
fn generalize(env: &TypeEnv, ty: &Type) -> Type {
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
            Type::Var(id) => match subst.get(id) {
                Some(t) => t.clone(),
                None => Type::Var(*id),
            },
            Type::ForAll(id, ty) => {
                // Create fresh type variable
                let fresh_id = id_dispenser.next();
                subst.insert(*id, Type::Var(fresh_id));
                inst_helper(ty, subst, id_dispenser)
            }

            Type::ADT(adt) => Type::ADT(ADT {
                name: adt.name.clone(),
                variants: adt
                    .variants
                    .iter()
                    .map(|v| ADTVariant {
                        name: v.name.clone(),
                        t: v.t
                            .as_ref()
                            .map(|t| Box::new(inst_helper(t, subst, id_dispenser))),
                    })
                    .collect(),
            }),
            Type::Arrow(a, b) => Type::Arrow(
                Box::new(inst_helper(a, subst, id_dispenser)),
                Box::new(inst_helper(b, subst, id_dispenser)),
            ),
            Type::Result(t, e) => Type::Result(
                Box::new(inst_helper(t, subst, id_dispenser)),
                Box::new(inst_helper(e, subst, id_dispenser)),
            ),
            Type::Option(t) => Type::Option(Box::new(inst_helper(t, subst, id_dispenser))),
            Type::List(t) => Type::List(Box::new(inst_helper(t, subst, id_dispenser))),
            Type::Dict(kts) => Type::Dict(
                kts.iter()
                    .map(|(k, t)| (k.clone(), inst_helper(t, subst, id_dispenser)))
                    .collect(),
            ),
            Type::Tuple(ts) => Type::Tuple(
                ts.iter()
                    .map(|t| inst_helper(t, subst, id_dispenser))
                    .collect(),
            ),

            Type::Bool | Type::Uint | Type::Int | Type::Float | Type::String => ty.clone(),
        };
        result
    }

    inst_helper(ty, &mut subst, id_dispenser)
}

#[cfg(test)]
mod tests {
    use rex_ast::expr::Var;
    use rex_lexer::span::Span;

    use crate::{
        types::TypeEnv,
        unify::{self, Subst},
    };

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
        let mut env = TypeEnv::new();

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
    fn test_list_expr() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        // Test [1, 2, 3]
        let expr = Expr::List(
            id_dispenser.next(),
            Span::default(),
            vec![
                Expr::Var(Var::next(&mut id_dispenser, "one")),
                Expr::Var(Var::next(&mut id_dispenser, "two")),
                Expr::Var(Var::next(&mut id_dispenser, "three")),
            ],
        );

        env.insert("one".to_string(), Type::Int);
        env.insert("two".to_string(), Type::Int);
        env.insert("three".to_string(), Type::Int);

        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;

        // Solve constraints
        let mut subst = HashMap::new();
        for constraint in constraints {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst)?,
                _ => panic!("Expected equality constraint"),
            }
        }

        // Final type should be [Int]
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::List(Box::new(Type::Int)));

        // Test that lists of mixed types fail
        let expr = Expr::List(
            id_dispenser.next(),
            Span::default(),
            vec![
                Expr::Var(Var::next(&mut id_dispenser, "one")),
                Expr::Var(Var::next(&mut id_dispenser, "true")),
            ],
        );

        env.insert("true".to_string(), Type::Bool);

        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let _ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;

        // This should fail unification
        let mut subst = Subst::new();
        let result = constraints
            .into_iter()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst),
                _ => panic!("Expected equality constraint"),
            });
        assert!(result.is_err());

        // Test empty list
        let expr = Expr::List(id_dispenser.next(), Span::default(), vec![]);
        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;
        assert!(matches!(ty, Type::List(_)));
        assert!(constraints.is_empty());

        Ok(())
    }

    #[test]
    fn test_list_expr_with_expected_type() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        // let f = \xs -> head xs in f [1, true]
        // should fail, xs can't be both [Int] and [Bool]
        let expr = Expr::Let(
            id_dispenser.next(),
            Span::default(),
            Var::next(&mut id_dispenser, "f"),
            // \xs -> head xs
            Box::new(Expr::Lam(
                id_dispenser.next(),
                Span::default(),
                Var::next(&mut id_dispenser, "xs"),
                Box::new(Expr::App(
                    id_dispenser.next(),
                    Span::default(),
                    Box::new(Expr::Var(Var::next(&mut id_dispenser, "head"))),
                    Box::new(Expr::Var(Var::next(&mut id_dispenser, "xs"))),
                )),
            )),
            // f [1, true]
            Box::new(Expr::App(
                id_dispenser.next(),
                Span::default(),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "f"))),
                Box::new(Expr::List(
                    id_dispenser.next(),
                    Span::default(),
                    vec![
                        Expr::Var(Var::next(&mut id_dispenser, "int_val")),
                        Expr::Var(Var::next(&mut id_dispenser, "bool_val")),
                    ],
                )),
            )),
        );

        // Set up environment
        let elem_type = id_dispenser.next();
        env.insert(
            "head".to_string(),
            Type::ForAll(
                elem_type,
                Box::new(Type::Arrow(
                    Box::new(Type::List(Box::new(Type::Var(elem_type)))),
                    Box::new(Type::Var(elem_type)),
                )),
            ),
        );
        env.insert("int_val".to_string(), Type::Int);
        env.insert("bool_val".to_string(), Type::Bool);

        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let _ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;

        // This should fail unification because the list elements don't match
        let mut subst = HashMap::new();
        let result = constraints
            .into_iter()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst),
                _ => panic!("Expected equality constraint"),
            });
        assert!(result.is_err());

        Ok(())
    }

    #[test]
    fn test_list_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        // Set up environment with polymorphic head : ∀α. [α] -> α
        let elem_type = id_dispenser.next();
        let head_type = Type::ForAll(
            elem_type,
            Box::new(Type::Arrow(
                Box::new(Type::List(Box::new(Type::Var(elem_type)))),
                Box::new(Type::Var(elem_type)),
            )),
        );
        env.insert("head".to_string(), head_type);

        // Add example lists to environment
        env.insert("int_list".to_string(), Type::List(Box::new(Type::Int)));
        env.insert("bool_list".to_string(), Type::List(Box::new(Type::Bool)));

        let expr = Expr::Tuple(
            id_dispenser.next(),
            Span::default(),
            vec![
                Expr::App(
                    id_dispenser.next(),
                    Span::default(),
                    Box::new(Expr::Var(Var::next(&mut id_dispenser, "head"))),
                    Box::new(Expr::Var(Var::next(&mut id_dispenser, "int_list"))),
                ),
                Expr::App(
                    id_dispenser.next(),
                    Span::default(),
                    Box::new(Expr::Var(Var::next(&mut id_dispenser, "head"))),
                    Box::new(Expr::Var(Var::next(&mut id_dispenser, "bool_list"))),
                ),
            ],
        );

        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;

        // Solve constraints
        let mut subst = HashMap::new();
        for constraint in &constraints {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst)?,
                _ => panic!("Expected equality constraint"),
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);

        assert_eq!(final_type, Type::Tuple(vec![Type::Int, Type::Bool]));

        Ok(())
    }

    #[test]
    fn test_let() {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        // Test expression: let id = (\x -> x) in id 1
        let expr = Expr::Let(
            id_dispenser.next(),
            Span::default(),
            Var::next(&mut id_dispenser, "id"),
            Box::new(Expr::Lam(
                id_dispenser.next(),
                Span::default(),
                Var::next(&mut id_dispenser, "x"),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "x"))),
            )),
            Box::new(Expr::App(
                id_dispenser.next(),
                Span::default(),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "id"))),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "one"))),
            )),
        );

        env.insert("one".to_string(), Type::Int);

        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let result_type = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )
        .unwrap();

        // Solve constraints
        let mut subst = HashMap::new();
        for constraint in constraints {
            match constraint {
                Constraint::Eq(t1, t2) => assert!(unify::unify_eq(&t1, &t2, &mut subst).is_ok()),
                _ => panic!("Expected equality constraint"),
            }
        }

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Type::Int);
    }

    #[test]
    fn test_let_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        // let id = \x -> x
        // in (id 1, id true)
        let expr = Expr::Let(
            id_dispenser.next(),
            Span::default(),
            Var::next(&mut id_dispenser, "id"),
            // \x -> x
            Box::new(Expr::Lam(
                id_dispenser.next(),
                Span::default(),
                Var::next(&mut id_dispenser, "x"),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "x"))),
            )),
            // Create tuple of (id 1, id true)
            Box::new(Expr::Tuple(
                id_dispenser.next(),
                Span::default(),
                vec![
                    Expr::App(
                        id_dispenser.next(),
                        Span::default(),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "id"))),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "int_val"))),
                    ),
                    Expr::App(
                        id_dispenser.next(),
                        Span::default(),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "id"))),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "bool_val"))),
                    ),
                ],
            )),
        );

        // Set up environment
        env.insert("int_val".to_string(), Type::Int);
        env.insert("bool_val".to_string(), Type::Bool);

        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;

        // Solve constraints
        let mut subst = Subst::new();
        for constraint in constraints {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst)?,
                _ => panic!("Expected equality constraint"),
            }
        }

        // The final type should be (Int, Bool)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Tuple(vec![Type::Int, Type::Bool]));

        Ok(())
    }

    #[test]
    fn test_if_then_else() {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        // Test expression: if true then 1 else 2
        let expr = Expr::Ite(
            id_dispenser.next(),
            Span::default(),
            Box::new(Expr::Var(Var::next(&mut id_dispenser, "true"))),
            Box::new(Expr::Var(Var::next(&mut id_dispenser, "one"))),
            Box::new(Expr::Var(Var::next(&mut id_dispenser, "two"))),
        );

        // Set up environment
        env.insert("true".to_string(), Type::Bool);
        env.insert("one".to_string(), Type::Int);
        env.insert("two".to_string(), Type::Int);

        // Generate constraints
        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let result_type = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )
        .unwrap();

        // Solve constraints
        let mut subst = HashMap::new();
        for constraint in constraints {
            match constraint {
                Constraint::Eq(t1, t2) => assert!(unify::unify_eq(&t1, &t2, &mut subst).is_ok()),
                _ => panic!("Expected equality constraint"),
            }
        }

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Type::Int);
    }

    #[test]
    fn test_compose_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        // let compose = \f -> \g -> \x -> f (g x)
        // in (
        //   compose not not true,  // Bool -> Bool -> Bool -> Bool
        //   compose (\x -> x + 1) (\x -> x + 2) 3  // Int -> Int -> Int -> Int
        // )
        let expr = Expr::Let(
            id_dispenser.next(),
            Span::default(),
            Var::next(&mut id_dispenser, "compose"),
            Box::new(Expr::Lam(
                id_dispenser.next(),
                Span::default(),
                Var::next(&mut id_dispenser, "f"),
                Box::new(Expr::Lam(
                    id_dispenser.next(),
                    Span::default(),
                    Var::next(&mut id_dispenser, "g"),
                    Box::new(Expr::Lam(
                        id_dispenser.next(),
                        Span::default(),
                        Var::next(&mut id_dispenser, "x"),
                        Box::new(Expr::App(
                            id_dispenser.next(),
                            Span::default(),
                            Box::new(Expr::Var(Var::next(&mut id_dispenser, "f"))),
                            Box::new(Expr::App(
                                id_dispenser.next(),
                                Span::default(),
                                Box::new(Expr::Var(Var::next(&mut id_dispenser, "g"))),
                                Box::new(Expr::Var(Var::next(&mut id_dispenser, "x"))),
                            )),
                        )),
                    )),
                )),
            )),
            Box::new(Expr::Tuple(
                id_dispenser.next(),
                Span::default(),
                vec![
                    // compose not not true
                    Expr::App(
                        id_dispenser.next(),
                        Span::default(),
                        Box::new(Expr::App(
                            id_dispenser.next(),
                            Span::default(),
                            Box::new(Expr::App(
                                id_dispenser.next(),
                                Span::default(),
                                Box::new(Expr::Var(Var::next(&mut id_dispenser, "compose"))),
                                Box::new(Expr::Var(Var::next(&mut id_dispenser, "not"))),
                            )),
                            Box::new(Expr::Var(Var::next(&mut id_dispenser, "not"))),
                        )),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "bool_val"))),
                    ),
                    // compose (+1) (+2) 3
                    Expr::App(
                        id_dispenser.next(),
                        Span::default(),
                        Box::new(Expr::App(
                            id_dispenser.next(),
                            Span::default(),
                            Box::new(Expr::App(
                                id_dispenser.next(),
                                Span::default(),
                                Box::new(Expr::Var(Var::next(&mut id_dispenser, "compose"))),
                                Box::new(Expr::Var(Var::next(&mut id_dispenser, "inc"))),
                            )),
                            Box::new(Expr::Var(Var::next(&mut id_dispenser, "inc2"))),
                        )),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "int_val"))),
                    ),
                ],
            )),
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

        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;

        // Solve constraints
        let mut subst = Subst::new();
        for constraint in constraints {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst)?,
                _ => panic!("Expected equality constraint"),
            }
        }

        // The final type should be (Bool, Int)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Tuple(vec![Type::Bool, Type::Int]));

        Ok(())
    }

    #[test]
    fn test_polymorphic_type_errors() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        // let id = \x -> x in
        // (id true, id 1, id true + 1)  // Last one should fail!
        let expr = Expr::Let(
            id_dispenser.next(),
            Span::default(),
            Var::next(&mut id_dispenser, "id"),
            Box::new(Expr::Lam(
                id_dispenser.next(),
                Span::default(),
                Var::next(&mut id_dispenser, "x"),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "x"))),
            )),
            Box::new(Expr::Tuple(
                id_dispenser.next(),
                Span::default(),
                vec![
                    // id true - ok
                    Expr::App(
                        id_dispenser.next(),
                        Span::default(),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "id"))),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "bool_val"))),
                    ),
                    // id 1 - ok
                    Expr::App(
                        id_dispenser.next(),
                        Span::default(),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "id"))),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "int_val"))),
                    ),
                    // id true + 1 - should fail!
                    Expr::App(
                        id_dispenser.next(),
                        Span::default(),
                        Box::new(Expr::Var(Var::next(&mut id_dispenser, "plus"))),
                        Box::new(Expr::App(
                            id_dispenser.next(),
                            Span::default(),
                            Box::new(Expr::Var(Var::next(&mut id_dispenser, "id"))),
                            Box::new(Expr::Var(Var::next(&mut id_dispenser, "bool_val"))),
                        )),
                    ),
                ],
            )),
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
        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let result = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )
        .and_then(|ty| {
            let mut subst = HashMap::new();
            for constraint in constraints {
                match constraint {
                    Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst)?,
                    _ => panic!("Expected equality constraint"),
                }
            }
            Ok(unify::apply_subst(&ty, &subst))
        });

        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Cannot unify"));

        Ok(())
    }

    #[test]
    fn test_overloading_with_arguments() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = HashMap::new();

        let xor_type_id = id_dispenser.next();
        env.insert("xor".to_string(), Type::Var(xor_type_id));

        // Single-type variables
        env.insert("true".to_string(), Type::Bool);
        env.insert("false".to_string(), Type::Bool);
        env.insert("one".to_string(), Type::Int);
        env.insert("two".to_string(), Type::Int);

        // Test bool version: xor true false
        let bool_expr = Expr::App(
            id_dispenser.next(),
            Span::default(),
            Box::new(Expr::App(
                id_dispenser.next(),
                Span::default(),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "xor"))),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "true"))),
            )),
            Box::new(Expr::Var(Var::next(&mut id_dispenser, "false"))),
        );

        let mut constraints = vec![Constraint::OneOf(
            Type::Var(xor_type_id),
            vec![
                Type::Arrow(
                    Box::new(Type::Bool),
                    Box::new(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool))),
                ),
                Type::Arrow(
                    Box::new(Type::Int),
                    Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Int))),
                ),
            ],
        )];
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &bool_expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;

        let mut subst = HashMap::new();
        for constraint in &constraints {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst)?,
                _ => {}
            }
        }
        for constraint in &constraints {
            match constraint {
                Constraint::Eq(..) => {}
                Constraint::OneOf(t1, t2_possibilties) => {
                    unify::unify_one_of(t1, t2_possibilties, &mut subst)?
                }
            }
        }
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Bool);

        Ok(())
    }

    #[test]
    fn test_overloading_with_return() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = TypeEnv::new();

        let rand_type_id = id_dispenser.next();
        env.insert("rand".to_string(), Type::Var(rand_type_id));

        // Add functions that force return type selection
        env.insert(
            "sum".to_string(),
            Type::Arrow(
                Box::new(Type::List(Box::new(Type::Int))),
                Box::new(Type::Int),
            ),
        );
        env.insert(
            "any".to_string(),
            Type::Arrow(
                Box::new(Type::List(Box::new(Type::Bool))),
                Box::new(Type::Bool),
            ),
        );

        // Test sum [rand, rand]
        let sum_expr = Expr::App(
            id_dispenser.next(),
            Span::default(),
            Box::new(Expr::Var(Var::next(&mut id_dispenser, "sum"))),
            Box::new(Expr::List(
                id_dispenser.next(),
                Span::default(),
                vec![
                    Expr::Var(Var::next(&mut id_dispenser, "rand")),
                    Expr::Var(Var::next(&mut id_dispenser, "rand")),
                ],
            )),
        );

        let mut constraints = vec![Constraint::OneOf(
            Type::Var(rand_type_id),
            vec![Type::Int, Type::Bool],
        )];
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &sum_expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;

        let mut subst = HashMap::new();
        for constraint in &constraints {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst)?,
                Constraint::OneOf(..) => {}
            }
        }
        for constraint in &constraints {
            match constraint {
                Constraint::Eq(..) => {}
                Constraint::OneOf(t1, t2_possibilties) => {
                    unify::unify_one_of(t1, t2_possibilties, &mut subst)?
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Int);

        Ok(())
    }

    #[test]
    fn test_overloading_with_ambiguity() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut env = HashMap::new();

        let rand_type_id = id_dispenser.next();
        env.insert("rand".to_string(), Type::Var(rand_type_id));
        // Test: just rand by itself (should be ambiguous)
        let expr = Expr::Var(Var::next(&mut id_dispenser, "rand"));

        let mut constraints = vec![];
        let mut expr_env = ExprTypeEnv::new();
        let _ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraints,
            &mut id_dispenser,
        )?;
        constraints.push(Constraint::OneOf(
            Type::Var(rand_type_id),
            vec![Type::Int, Type::Bool],
        ));

        let mut subst = HashMap::new();

        // This should fail because both types are possible
        let result = constraints
            .into_iter()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst),
                Constraint::OneOf(t1, t2_possibilties) => {
                    unify::unify_one_of(&t1, &t2_possibilties, &mut subst)
                }
            });
        assert!(result.is_err());

        Ok(())
    }
}
