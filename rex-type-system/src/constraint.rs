use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use rex_ast::{expr::Expr, id::Id};

use crate::{
    types::{ADTVariant, Type, TypeEnv, ADT},
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
        if !self.has_constraint_globally(&constraint) {
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
        if !self.has_constraint_globally(&constraint) && !self.has_constraint_locally(&constraint) {
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
        Self::apply_subst_to_constraints(subst, self.constraints_mut());
    }

    pub fn has_constraint_globally(&self, constraint: &Constraint) -> bool {
        Self::constraints_has_constraint(constraint, self.global_constraints.iter())
    }

    pub fn has_constraint_locally(&self, constraint: &Constraint) -> bool {
        Self::constraints_has_constraint(constraint, self.local_constraints.iter())
    }

    pub fn has_constraint(&self, constraint: &Constraint) -> bool {
        Self::constraints_has_constraint(constraint, self.constraints())
    }

    pub fn is_empty(&self) -> bool {
        self.local_constraints.is_empty() && self.global_constraints.is_empty()
    }

    pub fn constraints(&self) -> impl Iterator<Item = &Constraint> {
        self.local_constraints
            .iter()
            .chain(self.global_constraints.iter())
    }

    pub fn constraints_mut(&mut self) -> impl Iterator<Item = &mut Constraint> {
        self.local_constraints
            .iter_mut()
            .chain(self.global_constraints.iter_mut())
    }

    fn constraints_has_constraint<'a>(
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
                *t1 = unify::apply_subst(t1, subst);
                *t2 = unify::apply_subst(t2, subst);
            }
            Constraint::OneOf(t, ts) => {
                *t = unify::apply_subst(t, subst);
                *ts = ts.iter().map(|t| unify::apply_subst(t, subst)).collect();
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
                .join(", \n"),
            self.global_constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(", \n")
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Constraint {
    Eq(Arc<Type>, Arc<Type>),
    // NOTE(loong): this constraint is mostly used to define overloaded
    // functions.
    OneOf(Arc<Type>, HashSet<Arc<Type>>),
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

// NOTE(loong): We do not support overloaded parametric polymorphism.
//
// Generate constraints from an expression.
pub fn generate_constraints(
    expr: &Expr,
    env: &TypeEnv,
    constraint_system: &mut ConstraintSystem,
) -> Result<Arc<Type>, String> {
    match expr {
        Expr::Var(var) => match env.get(&var.name) {
            Some(t) => {
                if let Type::Var(id) = &**t {
                    // Look for OneOf constraint for this var
                    if let Some(Constraint::OneOf(_, possible_types)) = {
                        let x = constraint_system
                            .constraints()
                            .find(|c| match c {
                                Constraint::OneOf(x, _) => match &**x {
                                    Type::Var(v) if v == id => true,
                                    _ => false,
                                },
                                _ => false,
                            })
                            .cloned();
                        x
                    } {
                        // Create fresh type variable for this use
                        let fresh_id = Id::new();
                        let fresh_var = Arc::new(Type::Var(fresh_id));

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
                                Constraint::OneOf(t1, t2s) => {
                                    let new_t = if t1 == t { &fresh_var } else { t1 };
                                    new_global_constraints
                                        .push(Constraint::OneOf(new_t.clone(), t2s.clone()));
                                }
                            }
                        }
                        constraint_system
                            .add_global_constraints(new_global_constraints.into_iter());

                        Ok(fresh_var)
                    } else {
                        // Not an overloaded identifier, handle normally...
                        Ok(t.clone())
                    }
                } else {
                    match &**t {
                        Type::ForAll(_, _, _) => {
                            let instantiated_type = instantiate(t, constraint_system);
                            Ok(instantiated_type)
                        }
                        _ => Ok(t.clone()),
                    }
                }
            }
            None => Err(format!("Unbound variable: {}", &var.name)),
        },

        Expr::Dict(_span, exprs) => {
            let mut kvs = BTreeMap::new();

            // Generate constraints for each expression in the tuple
            for (key, expr) in exprs {
                let ty = generate_constraints(expr, env, constraint_system)?;
                kvs.insert(key.clone(), ty);
            }

            let res = Arc::new(Type::Dict(kvs));
            Ok(res)
        }

        Expr::Named(..) => {
            // Named expressions are for values created by constructors of sum types
            // (e.g. Result or Option). They should not be present in the source tree.
            //
            // TODO: Consider using a separate type for representing values created at
            // runtime vs. expressions parsed from the source file. Some expressions
            // (if/then/else) will not be part of the value type, and some values (Named, Curry)
            // will not be part of the expression type.
            unimplemented!("Named expressions are not expected to be present in the AST")
        }

        Expr::Promise(..) => {
            // As for Named
            unimplemented!("Promise expressions are not expected to be present in the AST")
        }

        Expr::Tuple(_span, exprs) => {
            let mut types = Vec::new();

            // Generate constraints for each expression in the tuple
            for expr in exprs {
                let ty = generate_constraints(expr, env, constraint_system)?;
                types.push(ty);
            }

            let res = Arc::new(Type::Tuple(types));
            Ok(res)
        }

        Expr::List(_span, exprs) => {
            // If list is empty, create a fresh type variable for element type
            if exprs.is_empty() {
                let elem_ty = Type::Var(Id::new());
                return Ok(Arc::new(Type::List(Arc::new(elem_ty))));
            }

            // Generate constraints for all expressions
            let mut types = Vec::new();
            for expr in exprs {
                let ty = generate_constraints(expr, env, constraint_system)?;
                types.push(ty);
            }

            // Add constraints that all elements must have the same type
            for ty in &types[1..] {
                constraint_system
                    .add_local_constraint(Constraint::Eq(types[0].clone(), ty.clone()));
            }

            let res = Arc::new(Type::List(types[0].clone()));
            Ok(res)
        }

        Expr::App(_span, f, x) => {
            let f_type = generate_constraints(f, env, constraint_system)?;
            let x_type = generate_constraints(x, env, constraint_system)?;

            let result_type = Arc::new(Type::Var(Id::new()));

            let expected_f_type = Arc::new(Type::Arrow(x_type.clone(), result_type.clone()));

            constraint_system.add_local_constraint(Constraint::Eq(f_type, expected_f_type));

            Ok(result_type)
        }

        Expr::Lam(_span, _scope, param, body) => {
            let param_type = Arc::new(Type::Var(Id::new()));

            let mut new_env = env.clone();
            new_env.insert(param.name.clone(), param_type.clone());

            // TODO(loong): do we need to clone `expr_env` in the same way that
            // we do for `env`?
            let body_type = generate_constraints(body, &new_env, constraint_system)?;

            let result_type = Arc::new(Type::Arrow(param_type, body_type));
            Ok(result_type)
        }

        Expr::Let(_span, var, def, body) => {
            // TODO(loong): why were we dropping local constraints here? There
            // was a very specific reason and we need to make sure it won't
            // cause unexpected problems elsewhere...
            //
            // The code was written a while ago though, so maybe the specific
            // issue just isn't relevant anymore. Our existing test suite is
            // pretty thorough.
            //
            // ```rs
            // First generate constraints for the definition let mut
            // let mut def_constraint_system = ConstraintSystem {
            //     local_constraints: vec![],
            //     global_constraints: constraint_system.global_constraints.clone(),
            // };
            // ```
            let mut def_constraint_system = constraint_system.clone();

            let def_type = generate_constraints(def, env, &mut def_constraint_system)?;

            // Solve definition constraints to get its type
            let def_subst = unify::unify_constraints(&def_constraint_system)?;
            let solved_def_type = unify::apply_subst(&def_type, &def_subst);
            constraint_system.extend(def_constraint_system.clone());

            // Generalize the type
            let gen_deps = def_constraint_system
                .constraints()
                .filter_map(|c| match c {
                    Constraint::OneOf(x, _) => match &**x {
                        Type::Var(id) => Some(*id),
                        _ => None,
                    },
                    Constraint::Eq(x, _) => match &**x {
                        Type::Var(id) => Some(*id),
                        _ => None,
                    },
                })
                .collect();

            // TODO(loong): is this safe to do? We only generalize the
            // let-binding if it is a function. It is possible that the binding
            // is a type variable that itself could later resolve to be a
            // function. We do this because when we generalize expressions like
            //
            // ```rex
            // (λx → let y = id x in y) 6.9
            // ```
            //
            // the generalization of `y` causes issues with resolving the final
            // type of the expression. Weirdly, this manifests as `y` having a
            // different type variable at the `in` expression than at the `let`
            // expression. This could be because the free variables are being
            // computed incorrectly, or it could be the way we instantiate `y`
            // when we see it at the `in` expression.
            //
            // Old version:
            // ```rex
            // let gen_type = generalize(env, &solved_def_type, gen_deps);
            // ```
            //
            // New version:
            // ```rex
            let gen_type = match &*solved_def_type {
                Type::Arrow(..) => generalize(env, &solved_def_type, gen_deps),
                _ => solved_def_type,
            };
            // ```

            // Add generalized type to environment
            let mut new_env = env.clone();
            new_env.insert(var.name.clone(), gen_type.clone());

            // Generate constraints for the body with the new environment
            let result_type = generate_constraints(body, &new_env, constraint_system)?;

            Ok(result_type)
        }

        Expr::Ite(_span, cond, then_branch, else_branch) => {
            // Generate constraints for all parts
            let cond_type = generate_constraints(cond, env, constraint_system)?;
            let then_type = generate_constraints(then_branch, env, constraint_system)?;
            let else_type = generate_constraints(else_branch, env, constraint_system)?;

            // Condition must be boolean
            constraint_system.add_local_constraint(Constraint::Eq(cond_type, Arc::new(Type::Bool)));
            // Then and else branches must have the same type
            constraint_system.add_local_constraint(Constraint::Eq(then_type.clone(), else_type));

            Ok(then_type)
        }

        Expr::Bool(_span, _x) => {
            let res = Arc::new(Type::Bool);
            Ok(res)
        }
        Expr::Uint(_span, _x) => {
            let res = Arc::new(Type::Uint);
            Ok(res)
        }
        Expr::Int(_span, _x) => {
            let res = Arc::new(Type::Int);
            Ok(res)
        }
        Expr::Float(_span, _x) => {
            let res = Arc::new(Type::Float);
            Ok(res)
        }
        Expr::String(_span, _x) => {
            let res = Arc::new(Type::String);
            Ok(res)
        }
        Expr::Uuid(_span, _x) => {
            let res = Arc::new(Type::Uuid);
            Ok(res)
        }
        Expr::DateTime(_span, _x) => {
            let res = Arc::new(Type::DateTime);
            Ok(res)
        }

        Expr::Curry(..) => {
            todo!("generate_constraints for Expr::Curry just like we do for Expr::App")
        }
    }
}

// For generalization, we need to find free type variables in a type
fn free_vars(ty: &Type) -> HashSet<Id> {
    match ty {
        Type::UnresolvedVar(_) => todo!("free_vars should return a result"),
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
        Type::Promise(t) => free_vars(t),
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

        Type::Bool
        | Type::Uint
        | Type::Int
        | Type::Float
        | Type::String
        | Type::Uuid
        | Type::DateTime => HashSet::new(),
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
fn generalize(env: &TypeEnv, ty: &Arc<Type>, deps: BTreeSet<Id>) -> Arc<Type> {
    let env_vars = env_free_vars(env);
    let ty_vars = free_vars(ty);

    // Find variables that are free in ty but not in env
    let mut to_quantify: Vec<_> = ty_vars.difference(&env_vars).cloned().collect();
    to_quantify.sort(); // Make generalization deterministic

    // Build the type from inside out
    let mut result = ty.clone();
    // Add deps to first (innermost) ForAll
    if let Some(first_var) = to_quantify.pop() {
        result = Arc::new(Type::ForAll(first_var, result, deps.into_iter().collect()));
    }
    // Rest without deps
    for var in to_quantify {
        result = Arc::new(Type::ForAll(var, result, BTreeSet::new()));
    }
    result
}

// Instantiate a type by replacing quantified variables with fresh ones
fn instantiate(ty: &Arc<Type>, constraint_system: &mut ConstraintSystem) -> Arc<Type> {
    let mut subst = Subst::new();

    fn inst_helper(
        ty: &Arc<Type>,
        subst: &mut Subst,
        constraint_system: &mut ConstraintSystem,
    ) -> Arc<Type> {
        let result = match &**ty {
            Type::UnresolvedVar(_) => todo!("instantiate/inst_helper should return a result"),
            Type::Var(id) => match subst.get(id) {
                Some(t) => t.clone(),
                None => Arc::new(Type::Var(*id)),
            },
            Type::ForAll(id, ty, deps) => {
                // Create fresh type variable
                let fresh_id = Id::new();
                subst.insert(*id, Arc::new(Type::Var(fresh_id)));

                // Create fresh vars for dependencies
                let mut new_constraint_sytem = ConstraintSystem::new();
                for dep_id in deps {
                    let fresh_dep_id = Id::new();
                    // Add equality constraint between old and new var
                    subst.insert(*dep_id, Arc::new(Type::Var(fresh_dep_id)));

                    for constraint in constraint_system.constraints() {
                        match constraint {
                            Constraint::Eq(x1, t2) => match &**x1 {
                                Type::Var(t1) => {
                                    if t1 == dep_id {
                                        new_constraint_sytem.add_local_constraint(Constraint::Eq(
                                            Arc::new(Type::Var(fresh_dep_id)),
                                            t2.clone(),
                                        ));
                                    }
                                }
                                _ => {}
                            },
                            Constraint::OneOf(x1, ts) => match &**x1 {
                                Type::Var(t1) => {
                                    if t1 == dep_id {
                                        new_constraint_sytem.add_local_constraint(
                                            Constraint::OneOf(
                                                Arc::new(Type::Var(fresh_dep_id)),
                                                ts.clone(),
                                            ),
                                        );
                                    }
                                }
                                _ => {}
                            },
                        }
                    }
                }

                new_constraint_sytem.apply_subst(subst);
                constraint_system.extend(new_constraint_sytem);

                inst_helper(ty, subst, constraint_system)
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
                        t: v.t
                            .as_ref()
                            .map(|t| inst_helper(t, subst, constraint_system)),
                        t_docs: v.t_docs.clone(),
                    })
                    .collect(),
            })),
            Type::Arrow(a, b) => Arc::new(Type::Arrow(
                inst_helper(a, subst, constraint_system),
                inst_helper(b, subst, constraint_system),
            )),
            Type::Result(t, e) => Arc::new(Type::Result(
                inst_helper(t, subst, constraint_system),
                inst_helper(e, subst, constraint_system),
            )),
            Type::Option(t) => Arc::new(Type::Option(inst_helper(t, subst, constraint_system))),
            Type::Promise(t) => Arc::new(Type::Promise(inst_helper(t, subst, constraint_system))),
            Type::List(t) => Arc::new(Type::List(inst_helper(t, subst, constraint_system))),
            Type::Dict(kts) => Arc::new(Type::Dict(
                kts.iter()
                    .map(|(k, t)| (k.clone(), inst_helper(t, subst, constraint_system)))
                    .collect(),
            )),
            Type::Tuple(ts) => Arc::new(Type::Tuple(
                ts.iter()
                    .map(|t| inst_helper(t, subst, constraint_system))
                    .collect(),
            )),

            Type::Bool
            | Type::Uint
            | Type::Int
            | Type::Float
            | Type::String
            | Type::Uuid
            | Type::DateTime => ty.clone(),
        };
        result
    }

    inst_helper(ty, &mut subst, constraint_system)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::collections::HashMap;

    use rex_ast::expr::{Scope, Var};
    use rex_lexer::span::Span;

    use crate::{
        trace::{sprint_subst, sprint_type_env},
        types::TypeEnv,
        unify,
    };

    use super::*;

    #[test]
    fn test_free_vars() {
        let alpha = Id::new();
        let beta = Id::new();

        // α -> β
        let ty = Type::Arrow(Arc::new(Type::Var(alpha)), Arc::new(Type::Var(beta)));
        let vars = free_vars(&ty);
        assert_eq!(vars.len(), 2);
        assert!(vars.contains(&alpha));
        assert!(vars.contains(&beta));

        // ∀α. α -> β
        let ty = Type::ForAll(
            alpha,
            Arc::new(Type::Arrow(
                Arc::new(Type::Var(alpha)),
                Arc::new(Type::Var(beta)),
            )),
            BTreeSet::new(),
        );
        let vars = free_vars(&ty);
        assert_eq!(vars.len(), 1);
        assert!(vars.contains(&beta));
    }

    #[test]
    fn test_generalize() {
        let alpha = Id::new();
        let beta = Id::new();

        let mut env = TypeEnv::new();

        // Environment with β free
        env.insert("y".to_string(), Arc::new(Type::Var(beta)));

        // Type to generalize: α -> β
        let ty = Arc::new(Type::Arrow(
            Arc::new(Type::Var(alpha)),
            Arc::new(Type::Var(beta)),
        ));

        // Should become ∀α. α -> β
        // (β isn't quantified because it appears in env)
        let gen_ty = generalize(&env, &ty, BTreeSet::new());
        match &*gen_ty {
            Type::ForAll(id, ty, _deps) => {
                assert_eq!(*id, alpha);
                match &**ty {
                    Type::Arrow(arg, ret) => {
                        assert_eq!(**arg, Type::Var(alpha));
                        assert_eq!(**ret, Type::Var(beta));
                    }
                    _ => panic!("Expected arrow type"),
                }
            }
            _ => panic!("Expected forall type"),
        }
    }

    // TODO: get this test working again
    // #[test]
    fn _test_instantiate() {
        let alpha = Id::new();
        let beta = Id::new();
        let gamma = Id::new();

        // ∀α. α -> β
        let ty = Arc::new(Type::ForAll(
            alpha,
            Arc::new(Type::Arrow(
                Arc::new(Type::Var(alpha)),
                Arc::new(Type::Var(beta)),
            )),
            BTreeSet::new(),
        ));

        let inst_ty = instantiate(&ty, &mut ConstraintSystem::new());

        // Should become γ -> β where γ is fresh
        match &*inst_ty {
            Type::Arrow(arg, ret) => {
                assert_eq!(*arg, Arc::new(Type::Var(gamma))); // Fresh variable
                assert_eq!(*ret, Arc::new(Type::Var(beta))); // Original free variable
            }
            _ => panic!("Expected arrow type"),
        }
    }

    #[test]
    fn test_list_expr() -> Result<(), String> {
        let mut env = TypeEnv::new();

        // Test [1, 2, 3]
        let expr = Expr::List(
            Span::default(),
            vec![
                Expr::Var(Var::new("one")),
                Expr::Var(Var::new("two")),
                Expr::Var(Var::new("three")),
            ],
        );

        env.insert("one".to_string(), Arc::new(Type::Int));
        env.insert("two".to_string(), Arc::new(Type::Int));
        env.insert("three".to_string(), Arc::new(Type::Int));

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system)?;

        // Solve constraints
        let mut subst = Subst::new();
        let mut did_change = false;
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst, &mut did_change)?,
                _ => panic!("Expected equality constraint"),
            }
        }

        // Final type should be [Int]
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Arc::new(Type::List(Arc::new(Type::Int))));

        // Test that lists of mixed types fail
        let expr = Expr::List(
            Span::default(),
            vec![Expr::Var(Var::new("one")), Expr::Var(Var::new("true"))],
        );

        env.insert("true".to_string(), Arc::new(Type::Bool));

        let mut constraint_system = ConstraintSystem::new();
        let _ty = generate_constraints(&expr, &env, &mut constraint_system)?;

        // This should fail unification
        let mut subst = Subst::new();
        let mut did_change = false;
        let result = constraint_system
            .constraints()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst, &mut did_change),
                _ => panic!("Expected equality constraint"),
            });
        assert!(result.is_err());

        // Test empty list
        let expr = Expr::List(Span::default(), vec![]);
        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system)?;
        assert!(matches!(&*ty, Type::List(_)));
        assert!(constraint_system.is_empty());

        Ok(())
    }

    #[test]
    fn test_list_expr_with_expected_type() -> Result<(), String> {
        let mut env = TypeEnv::new();

        // let f = \xs -> head xs in f [1, true]
        // should fail, xs can't be both [Int] and [Bool]
        let expr = Expr::Let(
            Span::default(),
            Var::new("f"),
            // \xs -> head xs
            Box::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("xs"),
                Box::new(Expr::App(
                    Span::default(),
                    Box::new(Expr::Var(Var::new("head"))),
                    Box::new(Expr::Var(Var::new("xs"))),
                )),
            )),
            // f [1, true]
            Box::new(Expr::App(
                Span::default(),
                Box::new(Expr::Var(Var::new("f"))),
                Box::new(Expr::List(
                    Span::default(),
                    vec![
                        Expr::Var(Var::new("int_val")),
                        Expr::Var(Var::new("bool_val")),
                    ],
                )),
            )),
        );

        // Set up environment
        let elem_type = Id::new();
        env.insert(
            "head".to_string(),
            Arc::new(Type::ForAll(
                elem_type,
                Arc::new(Type::Arrow(
                    Arc::new(Type::List(Arc::new(Type::Var(elem_type)))),
                    Arc::new(Type::Var(elem_type)),
                )),
                BTreeSet::new(),
            )),
        );
        env.insert("int_val".to_string(), Arc::new(Type::Int));
        env.insert("bool_val".to_string(), Arc::new(Type::Bool));

        let mut constraint_system = ConstraintSystem::new();
        let _ty = generate_constraints(&expr, &env, &mut constraint_system)?;

        // This should fail unification because the list elements don't match
        let mut subst = Subst::new();
        let mut did_change = false;
        let result = constraint_system
            .constraints()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst, &mut did_change),
                _ => panic!("Expected equality constraint"),
            });
        assert!(result.is_err());

        Ok(())
    }

    #[test]
    fn test_list_polymorphism() -> Result<(), String> {
        let mut env = TypeEnv::new();

        // Set up environment with polymorphic head : ∀α. [α] -> α
        let elem_type = Id::new();
        let head_type = Type::ForAll(
            elem_type,
            Arc::new(Type::Arrow(
                Arc::new(Type::List(Arc::new(Type::Var(elem_type)))),
                Arc::new(Type::Var(elem_type)),
            )),
            BTreeSet::new(),
        );
        env.insert("head".to_string(), Arc::new(head_type));

        // Add example lists to environment
        env.insert(
            "int_list".to_string(),
            Arc::new(Type::List(Arc::new(Type::Int))),
        );
        env.insert(
            "bool_list".to_string(),
            Arc::new(Type::List(Arc::new(Type::Bool))),
        );

        let expr = Expr::Tuple(
            Span::default(),
            vec![
                Expr::App(
                    Span::default(),
                    Box::new(Expr::Var(Var::new("head"))),
                    Box::new(Expr::Var(Var::new("int_list"))),
                ),
                Expr::App(
                    Span::default(),
                    Box::new(Expr::Var(Var::new("head"))),
                    Box::new(Expr::Var(Var::new("bool_list"))),
                ),
            ],
        );

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system)?;

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        let final_type = unify::apply_subst(&ty, &subst);

        assert_eq!(
            final_type,
            Arc::new(Type::Tuple(vec![Arc::new(Type::Int), Arc::new(Type::Bool)]))
        );

        Ok(())
    }

    #[test]
    fn test_let() -> Result<(), String> {
        let mut env = TypeEnv::new();

        // Test expression: let id = (\x -> x) in id 1
        let expr = Expr::Let(
            Span::default(),
            Var::new("id"),
            Box::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("x"),
                Box::new(Expr::Var(Var::new("x"))),
            )),
            Box::new(Expr::App(
                Span::default(),
                Box::new(Expr::Var(Var::new("id"))),
                Box::new(Expr::Var(Var::new("one"))),
            )),
        );

        env.insert("one".to_string(), Arc::new(Type::Int));

        let mut constraint_system = ConstraintSystem::new();
        let result_type = generate_constraints(&expr, &env, &mut constraint_system).unwrap();

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Arc::new(Type::Int));

        Ok(())
    }

    #[test]
    fn test_let_polymorphism() -> Result<(), String> {
        let mut env = TypeEnv::new();

        // let id = \x -> x
        // in (id 1, id true)
        let expr = Expr::Let(
            Span::default(),
            Var::new("id"),
            // \x -> x
            Box::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("x"),
                Box::new(Expr::Var(Var::new("x"))),
            )),
            // Create tuple of (id 1, id true)
            Box::new(Expr::Tuple(
                Span::default(),
                vec![
                    Expr::App(
                        Span::default(),
                        Box::new(Expr::Var(Var::new("id"))),
                        Box::new(Expr::Var(Var::new("int_val"))),
                    ),
                    Expr::App(
                        Span::default(),
                        Box::new(Expr::Var(Var::new("id"))),
                        Box::new(Expr::Var(Var::new("bool_val"))),
                    ),
                ],
            )),
        );

        // Set up environment
        env.insert("int_val".to_string(), Arc::new(Type::Int));
        env.insert("bool_val".to_string(), Arc::new(Type::Bool));

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system)?;

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        // The final type should be (Int, Bool)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(
            final_type,
            Arc::new(Type::Tuple(vec![Arc::new(Type::Int), Arc::new(Type::Bool)]))
        );

        Ok(())
    }

    #[test]
    fn test_if_then_else() -> Result<(), String> {
        let mut env = TypeEnv::new();

        // Test expression: if true then 1 else 2
        let expr = Expr::Ite(
            Span::default(),
            Box::new(Expr::Var(Var::new("true"))),
            Box::new(Expr::Var(Var::new("one"))),
            Box::new(Expr::Var(Var::new("two"))),
        );

        // Set up environment
        env.insert("true".to_string(), Arc::new(Type::Bool));
        env.insert("one".to_string(), Arc::new(Type::Int));
        env.insert("two".to_string(), Arc::new(Type::Int));

        // Generate constraints
        let mut constraint_system = ConstraintSystem::new();
        let result_type = generate_constraints(&expr, &env, &mut constraint_system).unwrap();

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Arc::new(Type::Int));

        Ok(())
    }

    #[test]
    fn test_compose_polymorphism() -> Result<(), String> {
        let mut env = TypeEnv::new();

        // let compose = \f -> \g -> \x -> f (g x)
        // in (
        //   compose not not true,  // Bool -> Bool -> Bool -> Bool
        //   compose (\x -> x + 1) (\x -> x + 2) 3  // Int -> Int -> Int -> Int
        // )
        let expr = Expr::Let(
            Span::default(),
            Var::new("compose"),
            Box::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("f"),
                Box::new(Expr::Lam(
                    Span::default(),
                    Scope::new_sync(),
                    Var::new("g"),
                    Box::new(Expr::Lam(
                        Span::default(),
                        Scope::new_sync(),
                        Var::new("x"),
                        Box::new(Expr::App(
                            Span::default(),
                            Box::new(Expr::Var(Var::new("f"))),
                            Box::new(Expr::App(
                                Span::default(),
                                Box::new(Expr::Var(Var::new("g"))),
                                Box::new(Expr::Var(Var::new("x"))),
                            )),
                        )),
                    )),
                )),
            )),
            Box::new(Expr::Tuple(
                Span::default(),
                vec![
                    // compose not not true
                    Expr::App(
                        Span::default(),
                        Box::new(Expr::App(
                            Span::default(),
                            Box::new(Expr::App(
                                Span::default(),
                                Box::new(Expr::Var(Var::new("compose"))),
                                Box::new(Expr::Var(Var::new("not"))),
                            )),
                            Box::new(Expr::Var(Var::new("not"))),
                        )),
                        Box::new(Expr::Var(Var::new("bool_val"))),
                    ),
                    // compose (+1) (+2) 3
                    Expr::App(
                        Span::default(),
                        Box::new(Expr::App(
                            Span::default(),
                            Box::new(Expr::App(
                                Span::default(),
                                Box::new(Expr::Var(Var::new("compose"))),
                                Box::new(Expr::Var(Var::new("inc"))),
                            )),
                            Box::new(Expr::Var(Var::new("inc2"))),
                        )),
                        Box::new(Expr::Var(Var::new("int_val"))),
                    ),
                ],
            )),
        );

        // Set up environment
        env.insert(
            "not".to_string(),
            Arc::new(Type::Arrow(Arc::new(Type::Bool), Arc::new(Type::Bool))),
        );
        env.insert(
            "inc".to_string(),
            Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
        );
        env.insert(
            "inc2".to_string(),
            Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
        );
        env.insert("bool_val".to_string(), Arc::new(Type::Bool));
        env.insert("int_val".to_string(), Arc::new(Type::Int));

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system)?;

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system)?;

        // The final type should be (Bool, Int)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(
            final_type,
            Arc::new(Type::Tuple(vec![Arc::new(Type::Bool), Arc::new(Type::Int)]))
        );

        Ok(())
    }

    #[test]
    fn test_polymorphic_type_errors() -> Result<(), String> {
        let mut env = TypeEnv::new();

        // let id = \x -> x in
        // (id true, id 1, id true + 1)  // Last one should fail!
        let expr = Expr::Let(
            Span::default(),
            Var::new("id"),
            Box::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("x"),
                Box::new(Expr::Var(Var::new("x"))),
            )),
            Box::new(Expr::Tuple(
                Span::default(),
                vec![
                    // id true - ok
                    Expr::App(
                        Span::default(),
                        Box::new(Expr::Var(Var::new("id"))),
                        Box::new(Expr::Var(Var::new("bool_val"))),
                    ),
                    // id 1 - ok
                    Expr::App(
                        Span::default(),
                        Box::new(Expr::Var(Var::new("id"))),
                        Box::new(Expr::Var(Var::new("int_val"))),
                    ),
                    // id true + 1 - should fail!
                    Expr::App(
                        Span::default(),
                        Box::new(Expr::Var(Var::new("plus"))),
                        Box::new(Expr::App(
                            Span::default(),
                            Box::new(Expr::Var(Var::new("id"))),
                            Box::new(Expr::Var(Var::new("bool_val"))),
                        )),
                    ),
                ],
            )),
        );

        // Set up environment
        env.insert("bool_val".to_string(), Arc::new(Type::Bool));
        env.insert("int_val".to_string(), Arc::new(Type::Int));
        env.insert(
            "plus".to_string(),
            Arc::new(Type::Arrow(
                Arc::new(Type::Int),
                Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
            )),
        );

        // This should fail with a type error
        let mut constraint_system = ConstraintSystem::new();
        let result = generate_constraints(&expr, &env, &mut constraint_system).and_then(|ty| {
            let mut subst = Subst::new();
            let mut did_change = false;
            for constraint in constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst, &mut did_change)?,
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
        let mut env = HashMap::new();

        let xor_type_id = Id::new();
        env.insert("xor".to_string(), Arc::new(Type::Var(xor_type_id)));

        // Single-type variables
        env.insert("true".to_string(), Arc::new(Type::Bool));
        env.insert("false".to_string(), Arc::new(Type::Bool));
        env.insert("one".to_string(), Arc::new(Type::Int));
        env.insert("two".to_string(), Arc::new(Type::Int));

        // Test bool version: xor true false
        let bool_expr = Expr::App(
            Span::default(),
            Box::new(Expr::App(
                Span::default(),
                Box::new(Expr::Var(Var::new("xor"))),
                Box::new(Expr::Var(Var::new("true"))),
            )),
            Box::new(Expr::Var(Var::new("false"))),
        );

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Arc::new(Type::Var(xor_type_id)),
                vec![
                    Arc::new(Type::Arrow(
                        Arc::new(Type::Bool),
                        Arc::new(Type::Arrow(Arc::new(Type::Bool), Arc::new(Type::Bool))),
                    )),
                    Arc::new(Type::Arrow(
                        Arc::new(Type::Int),
                        Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
                    )),
                ]
                .into_iter()
                .collect(),
            )]);
        let ty = generate_constraints(&bool_expr, &env, &mut constraint_system)?;

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Arc::new(Type::Bool));

        Ok(())
    }

    #[test]
    fn test_overloading_with_arguments_and_multiple_uses() -> Result<(), String> {
        let mut env = HashMap::new();

        let xor_type_id = Id::new();
        env.insert("xor".to_string(), Arc::new(Type::Var(xor_type_id)));

        // Single-type variables
        env.insert("true".to_string(), Arc::new(Type::Bool));
        env.insert("false".to_string(), Arc::new(Type::Bool));
        env.insert("one".to_string(), Arc::new(Type::Int));
        env.insert("two".to_string(), Arc::new(Type::Int));

        // Test bool version: xor true false
        let bool_expr = Expr::App(
            Span::default(),
            Box::new(Expr::App(
                Span::default(),
                Box::new(Expr::Var(Var::new("xor"))),
                Box::new(Expr::Var(Var::new("true"))),
            )),
            Box::new(Expr::Var(Var::new("false"))),
        );
        // Test int version: xor true false
        let int_expr = Expr::App(
            Span::default(),
            Box::new(Expr::App(
                Span::default(),
                Box::new(Expr::Var(Var::new("xor"))),
                Box::new(Expr::Var(Var::new("one"))),
            )),
            Box::new(Expr::Var(Var::new("two"))),
        );
        let tuple_expr = Expr::Tuple(Span::default(), vec![bool_expr, int_expr]);
        // let xor_lam_expr = Expr::Lam(
        //     Span::default(),
        //     Var::new("x"),
        //     Box::new(Expr::Lam(
        //         Span::default(),
        //         Var::new("y"),
        //         Box::new(Expr::App(
        //             Span::default(),
        //             Box::new(Expr::App(
        //                 Span::default(),
        //                 Box::new(Expr::Var(Var::new("xor"))),
        //                 Box::new(Expr::Var(Var::new("x"))),
        //             )),
        //             Box::new(Expr::Var(Var::new("y"))),
        //         )),
        //     )),
        // );
        // let let_expr = Expr::Let(
        //     Span::default(),
        //     Var::new("f"),
        //     // Box::new(xor_lam_expr),
        //     Box::new(Expr::Var(Var::new("xor"))),
        //     Box::new(tuple_expr),
        // );

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Arc::new(Type::Var(xor_type_id)),
                vec![
                    Arc::new(Type::Arrow(
                        Arc::new(Type::Bool),
                        Arc::new(Type::Arrow(Arc::new(Type::Bool), Arc::new(Type::Bool))),
                    )),
                    Arc::new(Type::Arrow(
                        Arc::new(Type::Int),
                        Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
                    )),
                ]
                .into_iter()
                .collect(),
            )]);
        let ty = generate_constraints(&tuple_expr, &env, &mut constraint_system)?;

        println!(
            "TYPES:\n{}\n\nCONSTRAINTS:\n{}",
            sprint_type_env(&env),
            &constraint_system
        );

        let subst = unify::unify_constraints(&constraint_system)?;

        let final_type = unify::apply_subst(&ty, &subst);

        println!("SUBST: {}", sprint_subst(&subst));
        println!("FINAL TYPE: {}", final_type);

        assert_eq!(
            final_type,
            Arc::new(Type::Tuple(vec![Arc::new(Type::Bool), Arc::new(Type::Int)]))
        );

        Ok(())
    }

    #[test]
    fn test_overloading_with_return() -> Result<(), String> {
        let mut env = TypeEnv::new();

        let rand_type_id = Id::new();
        env.insert("rand".to_string(), Arc::new(Type::Var(rand_type_id)));

        // Add functions that force return type selection
        env.insert(
            "sum".to_string(),
            Arc::new(Type::Arrow(
                Arc::new(Type::List(Arc::new(Type::Int))),
                Arc::new(Type::Int),
            )),
        );
        env.insert(
            "any".to_string(),
            Arc::new(Type::Arrow(
                Arc::new(Type::List(Arc::new(Type::Bool))),
                Arc::new(Type::Bool),
            )),
        );

        // Test sum [rand, rand]
        let sum_expr = Expr::App(
            Span::default(),
            Box::new(Expr::Var(Var::new("sum"))),
            Box::new(Expr::List(
                Span::default(),
                vec![Expr::Var(Var::new("rand")), Expr::Var(Var::new("rand"))],
            )),
        );

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Arc::new(Type::Var(rand_type_id)),
                vec![Arc::new(Type::Int), Arc::new(Type::Bool)]
                    .into_iter()
                    .collect(),
            )]);
        let ty = generate_constraints(&sum_expr, &env, &mut constraint_system)?;

        let subst = unify::unify_constraints(&constraint_system)?;

        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Arc::new(Type::Int));

        Ok(())
    }

    // TODO: get this test working again
    // #[test]
    fn _test_overloading_with_ambiguity() -> Result<(), String> {
        let mut env = HashMap::new();

        let rand_type_id = Id::new();
        env.insert("rand".to_string(), Arc::new(Type::Var(rand_type_id)));
        // Test: just rand by itself (should be ambiguous)
        let expr = Expr::Var(Var::new("rand"));

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Arc::new(Type::Var(rand_type_id)),
                vec![Arc::new(Type::Int), Arc::new(Type::Bool)]
                    .into_iter()
                    .collect(),
            )]);
        let _ty = generate_constraints(&expr, &env, &mut constraint_system)?;

        let mut subst = Subst::new();
        let mut did_change = false;

        // This should fail because both types are possible
        let result = constraint_system
            .constraints()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst, &mut did_change),
                Constraint::OneOf(t1, t2_possibilties) => {
                    unify::unify_one_of(t1, &t2_possibilties, &mut subst, &mut did_change)
                }
            });
        assert!(result.is_err());

        Ok(())
    }
}
