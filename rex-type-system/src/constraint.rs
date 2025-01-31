use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::{self, Display, Formatter},
};

use rex_ast::{
    expr::Expr,
    id::{Id, IdDispenser},
};

use crate::{
    trace::{sprint_expr_with_type, sprint_type_env},
    types::{ADTVariant, ExprTypeEnv, Type, TypeEnv, ADT},
    unify::{self, Subst},
};

#[derive(Clone, Debug)]
pub struct ConstraintSystem {
    pub local_constraints: Vec<Constraint>,
    pub global_constraints: Vec<Constraint>,
}

impl ConstraintSystem {
    pub fn new() -> Self {
        Self {
            local_constraints: vec![],
            global_constraints: vec![],
        }
    }

    pub fn with_global_constraints(global_constraints: Vec<Constraint>) -> Self {
        Self {
            local_constraints: vec![],
            global_constraints,
        }
    }

    pub fn add_global_constraint(&mut self, constraint: Constraint) {
        if !self.is_constrained_globally(&constraint) {
            self.global_constraints.push(constraint);
        }
    }

    pub fn add_global_constraints(&mut self, constraints: impl Iterator<Item = Constraint>) {
        for constraint in constraints {
            self.add_global_constraint(constraint);
        }
    }

    pub fn add_local_constraints(&mut self, constraints: impl Iterator<Item = Constraint>) {
        for constraint in constraints {
            self.add_local_constraint(constraint);
        }
    }

    pub fn add_local_constraint(&mut self, constraint: Constraint) {
        if !self.is_constrained_globally(&constraint) && !self.is_constrained_locally(&constraint) {
            self.local_constraints.push(constraint);
        }
    }

    pub fn extend(&mut self, other: ConstraintSystem) {
        for constraint in other.global_constraints {
            self.add_global_constraint(constraint);
        }
        for constraint in other.local_constraints {
            self.add_local_constraint(constraint);
        }
    }

    pub fn apply_subst(&mut self, subst: &Subst) {
        Self::apply_subst_to_constraints(subst, self.global_constraints.iter_mut());
        Self::apply_subst_to_constraints(subst, self.local_constraints.iter_mut());
    }

    pub fn is_constrained_globally(&self, constraint: &Constraint) -> bool {
        Self::is_constrained_by_constraints(constraint, self.global_constraints.iter())
    }

    pub fn is_constrained_locally(&self, constraint: &Constraint) -> bool {
        Self::is_constrained_by_constraints(constraint, self.local_constraints.iter())
    }

    pub fn is_constrained(&self, constraint: &Constraint) -> bool {
        Self::is_constrained_by_constraints(constraint, self.constraints())
    }

    pub fn is_empty(&self) -> bool {
        self.local_constraints.is_empty() && self.global_constraints.is_empty()
    }

    pub fn constraints(&self) -> impl Iterator<Item = &Constraint> {
        self.local_constraints
            .iter()
            .chain(self.global_constraints.iter())
    }

    fn is_constrained_by_constraints<'a>(
        constraint: &Constraint,
        constraints: impl Iterator<Item = &'a Constraint>,
    ) -> bool {
        for check in constraints {
            match (constraint, check) {
                (Constraint::Eq(t11, t12), Constraint::Eq(t21, t22)) => {
                    if t11 == t21 && t12 == t22 {
                        return true;
                    }
                }
                (Constraint::OneOf(t1, ts1), Constraint::OneOf(t2, ts2)) => {
                    if t1 == t2 && ts1 == ts2 {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn apply_subst_to_constraints<'a>(
        subst: &Subst,
        constraints: impl Iterator<Item = &'a mut Constraint>,
    ) {
        constraints.for_each(|constraint| match constraint {
            Constraint::Eq(t1, t2) => {
                *t1 = unify::apply_subst(&t1, subst);
                *t2 = unify::apply_subst(&t2, subst);
            }
            Constraint::OneOf(t, ts) => {
                *t = unify::apply_subst(&t, subst);
                *ts = ts.iter().map(|t| unify::apply_subst(&t, subst)).collect();
            }
        });
    }
}

impl Display for ConstraintSystem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "local = [{}], global = [{}]",
            self.local_constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.global_constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Eq(Type, Type),
    OneOf(Type, HashSet<Type>),
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Eq(t1, t2) => write!(f, "{} = {}", t1, t2),
            Constraint::OneOf(t, ts) => {
                write!(f, "{} ∈ {{", t)?;
                for (i, ty) in ts.iter().enumerate() {
                    write!(f, "{}", ty)?;
                    if i + 1 < ts.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

// Generate constraints from an expression
pub fn generate_constraints(
    expr: &Expr,
    env: &TypeEnv,
    expr_env: &mut ExprTypeEnv,
    constraint_system: &mut ConstraintSystem,
    id_dispenser: &mut IdDispenser,
) -> Result<Type, String> {
    match expr {
        Expr::Var(var) => match env.get(&var.name) {
            Some(t) => {
                if let Type::Var(id) = t {
                    // Look for OneOf constraint for this var
                    if let Some(Constraint::OneOf(_, possible_types)) = {
                        let x = constraint_system
                            .constraints()
                            .find(|c| matches!(c, Constraint::OneOf(Type::Var(v), _) if v == id))
                            .cloned();
                        x
                    } {
                        // Create fresh type variable for this use
                        let fresh_id = id_dispenser.next();
                        let fresh_var = Type::Var(fresh_id);

                        // Add OneOf constraint with same possibilities
                        constraint_system.add_local_constraint(Constraint::OneOf(
                            fresh_var.clone(),
                            possible_types,
                        ));

                        let mut new_global_constraints = vec![];
                        for constraint in constraint_system.global_constraints.iter() {
                            match constraint {
                                Constraint::Eq(t1, t2) => {
                                    let new_t1 = if t1 == t { &fresh_var } else { t1 };
                                    let new_t2 = if t2 == t { &fresh_var } else { t2 };
                                    new_global_constraints
                                        .push(Constraint::Eq(new_t1.clone(), new_t2.clone()));
                                }
                                Constraint::OneOf(t, ts) => {
                                    let new_t = if t == t { &fresh_var } else { t };
                                    new_global_constraints
                                        .push(Constraint::OneOf(new_t.clone(), ts.clone()));
                                }
                            }
                        }
                        constraint_system
                            .add_global_constraints(new_global_constraints.into_iter());

                        expr_env.insert(var.id, fresh_var.clone());
                        Ok(fresh_var)
                    } else {
                        // Not an overloaded identifier, handle normally...
                        expr_env.insert(var.id, t.clone());
                        Ok(t.clone())
                    }
                } else {
                    match t {
                        Type::ForAll(_, _, _) => {
                            let instantiated_type = instantiate(t, id_dispenser, constraint_system);
                            expr_env.insert(var.id, instantiated_type.clone());
                            Ok(instantiated_type)
                        }
                        _ => {
                            expr_env.insert(var.id, t.clone());
                            Ok(t.clone())
                        }
                    }
                }
            }
            None => Err(format!("Unbound variable: {}", &var.name)),
        },

        Expr::Dict(id, _span, exprs) => {
            let mut kvs = BTreeMap::new();

            // Generate constraints for each expression in the tuple
            for (key, expr) in exprs {
                let ty =
                    generate_constraints(expr, env, expr_env, constraint_system, id_dispenser)?;
                kvs.insert(key.clone(), ty);
            }

            expr_env.insert(*id, Type::Dict(kvs.clone()));
            Ok(Type::Dict(kvs))
        }

        Expr::Tuple(id, _span, exprs) => {
            let mut types = Vec::new();

            // Generate constraints for each expression in the tuple
            for expr in exprs {
                let ty =
                    generate_constraints(expr, env, expr_env, constraint_system, id_dispenser)?;
                types.push(ty);
            }

            expr_env.insert(*id, Type::Tuple(types.clone()));
            Ok(Type::Tuple(types))
        }

        Expr::List(id, _span, exprs) => {
            // If list is empty, create a fresh type variable for element type
            if exprs.is_empty() {
                let elem_ty = Type::Var(id_dispenser.next());
                return Ok(Type::List(Box::new(elem_ty)));
            }

            // Generate constraints for all expressions
            let mut types = Vec::new();
            for expr in exprs {
                let ty =
                    generate_constraints(expr, env, expr_env, constraint_system, id_dispenser)?;
                types.push(ty);
            }

            // Add constraints that all elements must have the same type
            for ty in &types[1..] {
                constraint_system
                    .add_local_constraint(Constraint::Eq(types[0].clone(), ty.clone()));
            }

            expr_env.insert(*id, Type::List(Box::new(types[0].clone())));
            Ok(Type::List(Box::new(types[0].clone())))
        }

        Expr::App(id, _span, f, x) => {
            let f_type = generate_constraints(f, env, expr_env, constraint_system, id_dispenser)?;
            let x_type = generate_constraints(x, env, expr_env, constraint_system, id_dispenser)?;

            let result_type = Type::Var(id_dispenser.next());

            let expected_f_type =
                Type::Arrow(Box::new(x_type.clone()), Box::new(result_type.clone()));

            constraint_system.add_local_constraint(Constraint::Eq(f_type, expected_f_type));

            println!(
                "APP:\nEXPR: {}\nTYPE_ENV: {}\nCONSTRAINTS: {}\n",
                sprint_expr_with_type(&expr, &expr_env, None),
                sprint_type_env(&env),
                constraint_system,
            );

            expr_env.insert(*id, result_type.clone());
            Ok(result_type)
        }

        Expr::Lam(id, _span, param, body) => {
            let param_type = Type::Var(id_dispenser.next());

            let mut new_env = env.clone();
            new_env.insert(param.name.clone(), param_type.clone());
            expr_env.insert(param.id, param_type.clone());

            println!(
                "LAM (before):\nEXPR: {}\nTYPE_ENV: {}\nCONSTRAINTS: {}\n",
                sprint_expr_with_type(&expr, &expr_env, None),
                sprint_type_env(&new_env),
                constraint_system,
            );

            // TODO(loong): do we need to clone `expr_env` in the same way that
            // we do for `env`?
            let body_type =
                generate_constraints(body, &new_env, expr_env, constraint_system, id_dispenser)?;

            println!(
                "LAM (after):\nEXPR: {}\nTYPE_ENV: {}\nCONSTRAINTS: {}\n",
                sprint_expr_with_type(&expr, &expr_env, None),
                sprint_type_env(&env),
                constraint_system,
            );

            let result_type = Type::Arrow(Box::new(param_type), Box::new(body_type));
            expr_env.insert(*id, result_type.clone());
            Ok(result_type)
        }

        Expr::Let(id, _span, var, def, body) => {
            // First generate constraints for the definition
            let mut def_constraint_system = ConstraintSystem {
                local_constraints: vec![],
                global_constraints: constraint_system.global_constraints.clone(),
            };
            let def_type =
                generate_constraints(def, env, expr_env, &mut def_constraint_system, id_dispenser)?;

            // Solve definition constraints to get its type
            let mut def_subst = HashMap::new();
            for constraint in def_constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut def_subst)?,
                    _ => {}
                }
            }
            for constraint in def_constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(..) => {}
                    Constraint::OneOf(t1, t2_possibilties) => {
                        unify::unify_one_of(t1, t2_possibilties, &mut def_subst);
                    }
                }
            }
            let solved_def_type = unify::apply_subst(&def_type, &def_subst);
            constraint_system.extend(def_constraint_system.clone());

            println!(
                "LET (before):\nEXPR: {}\nTYPE_ENV: {}\nCONSTRAINTS: {}\nDEF_TYPE: {}\n",
                sprint_expr_with_type(&expr, &expr_env, None),
                sprint_type_env(&env),
                constraint_system,
                solved_def_type,
            );

            // Generalize the type
            let gen_deps = def_constraint_system
                .constraints()
                .filter_map(|c| match c {
                    Constraint::OneOf(Type::Var(id), _) => Some(*id),
                    Constraint::Eq(Type::Var(id), _) => Some(*id),
                    _ => None,
                })
                .collect();
            // let gen_deps = vec![];
            let gen_type = generalize(env, &solved_def_type, gen_deps);

            // Add generalized type to environment
            let mut new_env = env.clone();
            new_env.insert(var.name.clone(), gen_type.clone());
            expr_env.insert(var.id, gen_type);

            // Generate constraints for the body with the new environment
            let result_type =
                generate_constraints(body, &new_env, expr_env, constraint_system, id_dispenser)?;
            expr_env.insert(*id, result_type.clone());

            println!(
                "LET (after):\nEXPR: {}\nTYPE_ENV: {}\nCONSTRAINTS: {}\n",
                sprint_expr_with_type(&expr, &expr_env, None),
                sprint_type_env(&new_env),
                constraint_system,
            );

            Ok(result_type)
        }

        Expr::Ite(id, _span, cond, then_branch, else_branch) => {
            // Generate constraints for all parts
            let cond_type =
                generate_constraints(cond, env, expr_env, constraint_system, id_dispenser)?;
            let then_type =
                generate_constraints(then_branch, env, expr_env, constraint_system, id_dispenser)?;
            let else_type =
                generate_constraints(else_branch, env, expr_env, constraint_system, id_dispenser)?;

            // Condition must be boolean
            constraint_system.add_local_constraint(Constraint::Eq(cond_type, Type::Bool));
            // Then and else branches must have the same type
            constraint_system.add_local_constraint(Constraint::Eq(then_type.clone(), else_type));

            expr_env.insert(*id, then_type.clone());
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
        Type::ForAll(id, ty, _deps) => {
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
fn generalize(env: &TypeEnv, ty: &Type, deps: BTreeSet<Id>) -> Type {
    let env_vars = env_free_vars(env);
    let ty_vars = free_vars(ty);

    // Find variables that are free in ty but not in env
    let mut to_quantify: Vec<_> = ty_vars.difference(&env_vars).cloned().collect();
    to_quantify.sort(); // Make generalization deterministic

    // Build the type from inside out
    let mut result = ty.clone();
    // Add deps to first (innermost) ForAll
    if let Some(first_var) = to_quantify.pop() {
        result = Type::ForAll(first_var, Box::new(result), deps.into_iter().collect());
    }
    // Rest without deps
    for var in to_quantify {
        result = Type::ForAll(var, Box::new(result), BTreeSet::new());
    }
    result
}

// Instantiate a type by replacing quantified variables with fresh ones
fn instantiate(
    ty: &Type,
    id_dispenser: &mut IdDispenser,
    constraint_system: &mut ConstraintSystem,
) -> Type {
    let mut subst = HashMap::new();

    println!(
        "INSTANTIATE (before):\nCONSTRAINTS: {}\nTYPE: {}\n",
        &constraint_system, ty,
    );

    fn inst_helper(
        ty: &Type,
        subst: &mut HashMap<Id, Type>,
        id_dispenser: &mut IdDispenser,
        constraint_system: &mut ConstraintSystem,
    ) -> Type {
        let result = match ty {
            Type::Var(id) => match subst.get(id) {
                Some(t) => t.clone(),
                None => Type::Var(*id),
            },
            Type::ForAll(id, ty, deps) => {
                // Create fresh type variable
                let fresh_id = id_dispenser.next();
                subst.insert(*id, Type::Var(fresh_id));

                println!("  INSTANTIATE: {} -> {}", id, fresh_id);

                // Create fresh vars for dependencies
                let mut new_constraint_sytem = ConstraintSystem::new();
                for dep_id in deps {
                    let fresh_dep_id = id_dispenser.next();
                    // Add equality constraint between old and new var
                    subst.insert(*dep_id, Type::Var(fresh_dep_id));
                    println!("    DEP: {} -> {}", dep_id, fresh_dep_id);

                    for constraint in constraint_system.constraints() {
                        match constraint {
                            Constraint::Eq(Type::Var(t1), t2) => {
                                if t1 == dep_id {
                                    new_constraint_sytem.add_local_constraint(Constraint::Eq(
                                        Type::Var(fresh_dep_id),
                                        t2.clone(),
                                    ));
                                }
                            }
                            Constraint::OneOf(Type::Var(t1), ts) => {
                                if t1 == dep_id {
                                    new_constraint_sytem.add_local_constraint(Constraint::OneOf(
                                        Type::Var(fresh_dep_id),
                                        ts.clone(),
                                    ));
                                }
                            }
                            _ => {}
                        }
                    }
                }

                new_constraint_sytem.apply_subst(&subst);
                constraint_system.extend(new_constraint_sytem);

                println!("  NEW_FIXED_DEP_CONSTRAINTS: {}", constraint_system);

                inst_helper(ty, subst, id_dispenser, constraint_system)
            }
            Type::ADT(adt) => Type::ADT(ADT {
                name: adt.name.clone(),
                variants: adt
                    .variants
                    .iter()
                    .map(|v| ADTVariant {
                        name: v.name.clone(),
                        t: v.t.as_ref().map(|t| {
                            Box::new(inst_helper(t, subst, id_dispenser, constraint_system))
                        }),
                    })
                    .collect(),
            }),
            Type::Arrow(a, b) => Type::Arrow(
                Box::new(inst_helper(a, subst, id_dispenser, constraint_system)),
                Box::new(inst_helper(b, subst, id_dispenser, constraint_system)),
            ),
            Type::Result(t, e) => Type::Result(
                Box::new(inst_helper(t, subst, id_dispenser, constraint_system)),
                Box::new(inst_helper(e, subst, id_dispenser, constraint_system)),
            ),
            Type::Option(t) => Type::Option(Box::new(inst_helper(
                t,
                subst,
                id_dispenser,
                constraint_system,
            ))),
            Type::List(t) => Type::List(Box::new(inst_helper(
                t,
                subst,
                id_dispenser,
                constraint_system,
            ))),
            Type::Dict(kts) => Type::Dict(
                kts.iter()
                    .map(|(k, t)| {
                        (
                            k.clone(),
                            inst_helper(t, subst, id_dispenser, constraint_system),
                        )
                    })
                    .collect(),
            ),
            Type::Tuple(ts) => Type::Tuple(
                ts.iter()
                    .map(|t| inst_helper(t, subst, id_dispenser, constraint_system))
                    .collect(),
            ),

            Type::Bool | Type::Uint | Type::Int | Type::Float | Type::String => ty.clone(),
        };
        result
    }

    let res = inst_helper(ty, &mut subst, id_dispenser, constraint_system);
    // let res = unify::apply_subst(&res, &subst);

    println!(
        "INSTANTIATE (after):\nCONSTRAINTS: {}\nDTYPE: {}\n",
        &constraint_system, res,
    );

    res
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use rex_ast::expr::Var;
    use rex_lexer::span::Span;

    use crate::{
        trace::{sprint_expr_type_env, sprint_expr_with_type, sprint_subst, sprint_type_env},
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
            BTreeSet::new(),
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
        let gen_ty = generalize(&env, &ty, BTreeSet::new());
        match gen_ty {
            Type::ForAll(id, ty, _deps) => {
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
            BTreeSet::new(),
        );

        let inst_ty = instantiate(&ty, &mut id_dispenser, &mut ConstraintSystem::new());

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

        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        // Solve constraints
        let mut subst = HashMap::new();
        for constraint in constraint_system.constraints() {
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

        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let _ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        // This should fail unification
        let mut subst = Subst::new();
        let result = constraint_system
            .constraints()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(&t1, &t2, &mut subst),
                _ => panic!("Expected equality constraint"),
            });
        assert!(result.is_err());

        // Test empty list
        let expr = Expr::List(id_dispenser.next(), Span::default(), vec![]);
        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;
        assert!(matches!(ty, Type::List(_)));
        assert!(constraint_system.is_empty());

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
                BTreeSet::new(),
            ),
        );
        env.insert("int_val".to_string(), Type::Int);
        env.insert("bool_val".to_string(), Type::Bool);

        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let _ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        // This should fail unification because the list elements don't match
        let mut subst = HashMap::new();
        let result = constraint_system
            .constraints()
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
            BTreeSet::new(),
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

        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        let final_type = unify::apply_subst(&ty, &subst);

        assert_eq!(final_type, Type::Tuple(vec![Type::Int, Type::Bool]));

        Ok(())
    }

    #[test]
    fn test_let() -> Result<(), String> {
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

        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let result_type = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Type::Int);

        Ok(())
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

        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        // The final type should be (Int, Bool)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Tuple(vec![Type::Int, Type::Bool]));

        Ok(())
    }

    #[test]
    fn test_if_then_else() -> Result<(), String> {
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
        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let result_type = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Type::Int);

        Ok(())
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

        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

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
        let mut constraint_system = ConstraintSystem::new();
        let mut expr_env = ExprTypeEnv::new();
        let result = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .and_then(|ty| {
            let mut subst = HashMap::new();
            for constraint in constraint_system.constraints() {
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

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
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
                ]
                .into_iter()
                .collect(),
            )]);
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &bool_expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Bool);

        Ok(())
    }

    #[test]
    fn test_overloading_with_arguments_and_multiple_uses() -> Result<(), String> {
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
        // Test int version: xor true false
        let int_expr = Expr::App(
            id_dispenser.next(),
            Span::default(),
            Box::new(Expr::App(
                id_dispenser.next(),
                Span::default(),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "xor"))),
                Box::new(Expr::Var(Var::next(&mut id_dispenser, "one"))),
            )),
            Box::new(Expr::Var(Var::next(&mut id_dispenser, "two"))),
        );
        let tuple_expr = Expr::Tuple(
            id_dispenser.next(),
            Span::default(),
            vec![bool_expr, int_expr],
        );
        // let xor_lam_expr = Expr::Lam(
        //     id_dispenser.next(),
        //     Span::default(),
        //     Var::next(&mut id_dispenser, "x"),
        //     Box::new(Expr::Lam(
        //         id_dispenser.next(),
        //         Span::default(),
        //         Var::next(&mut id_dispenser, "y"),
        //         Box::new(Expr::App(
        //             id_dispenser.next(),
        //             Span::default(),
        //             Box::new(Expr::App(
        //                 id_dispenser.next(),
        //                 Span::default(),
        //                 Box::new(Expr::Var(Var::next(&mut id_dispenser, "xor"))),
        //                 Box::new(Expr::Var(Var::next(&mut id_dispenser, "x"))),
        //             )),
        //             Box::new(Expr::Var(Var::next(&mut id_dispenser, "y"))),
        //         )),
        //     )),
        // );
        // let let_expr = Expr::Let(
        //     id_dispenser.next(),
        //     Span::default(),
        //     Var::next(&mut id_dispenser, "f"),
        //     // Box::new(xor_lam_expr),
        //     Box::new(Expr::Var(Var::next(&mut id_dispenser, "xor"))),
        //     Box::new(tuple_expr),
        // );

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
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
                ]
                .into_iter()
                .collect(),
            )]);
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &tuple_expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        println!(
            "TYPES:\n{}\nEXPR TYPES:\n{}\nCONSTRAINTS:\n{}",
            sprint_type_env(&env),
            sprint_expr_type_env(&expr_env),
            &constraint_system
        );

        let subst = unify::unify_constraints(&constraint_system)?;

        let final_type = unify::apply_subst(&ty, &subst);

        println!("{}", &tuple_expr);
        println!(
            "{}",
            sprint_expr_with_type(&tuple_expr, &expr_env, Some(&subst))
        );
        println!("SUBST: {}", sprint_subst(&subst));
        println!("FINAL TYPE: {}", final_type);

        assert_eq!(final_type, Type::Tuple(vec![Type::Bool, Type::Int]));

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

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Type::Var(rand_type_id),
                vec![Type::Int, Type::Bool].into_iter().collect(),
            )]);
        let mut expr_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &sum_expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        let subst = unify::unify_constraints(&constraint_system)?;

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

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Type::Var(rand_type_id),
                vec![Type::Int, Type::Bool].into_iter().collect(),
            )]);
        let mut expr_env = ExprTypeEnv::new();
        let _ty = generate_constraints(
            &expr,
            &env,
            &mut expr_env,
            &mut constraint_system,
            &mut id_dispenser,
        )?;

        let mut subst = HashMap::new();

        // This should fail because both types are possible
        let result = constraint_system
            .constraints()
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
