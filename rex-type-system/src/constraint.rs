use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use rex_ast::expr::Expr;
use rex_lexer::span::Span;

use crate::{
    error::TypeError,
    types::{ADTVariant, AppliedType, Type, TypeCon, TypeEnv, TypeScheme, TypeVar, ADT},
    unify::{self, Subst},
};

#[derive(Debug, Default)]
pub struct ConstraintSystem {
    pub constraints: Vec<Constraint>,
    pub subst: Subst,
    pub errors: BTreeSet<TypeError>,
}

impl ConstraintSystem {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            subst: Subst::default(),
            errors: BTreeSet::new(),
        }
    }

    pub fn add_eq(&mut self, span: Span, t1: Arc<Type>, t2: Arc<Type>) {
        self.add_constraint(Constraint::Eq(span, t1, t2));
    }

    pub fn add_one_of(&mut self, span: Span, var: TypeVar, ts: BTreeSet<Arc<Type>>) {
        self.add_constraint(Constraint::OneOf(span, var, ts));
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        if !self.has_constraint(&constraint) {
            self.constraints.push(constraint);
        }
    }

    pub fn extend(&mut self, other: ConstraintSystem) {
        for constraint in other.constraints {
            self.add_constraint(constraint);
        }
    }

    pub fn apply_subst(&mut self, type_subst: &Subst, var_subst: &HashMap<TypeVar, TypeVar>) {
        self.constraints
            .iter_mut()
            .for_each(|constraint| match constraint {
                Constraint::Eq(_, t1, t2) => {
                    *t1 = t1.apply(type_subst);
                    *t2 = t2.apply(type_subst);
                }
                Constraint::OneOf(_, v, ts) => {
                    if let Some(new_v) = var_subst.get(v) {
                        *v = *new_v;
                    }
                    *ts = ts.iter().map(|t| t.apply(type_subst)).collect();
                }
            });
    }

    pub fn has_constraint(&self, constraint: &Constraint) -> bool {
        Self::constraints_has_constraint(constraint, self.constraints())
    }

    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    pub fn constraints(&self) -> impl Iterator<Item = &Constraint> {
        self.constraints.iter()
    }

    pub fn constraints_mut(&mut self) -> impl Iterator<Item = &mut Constraint> {
        self.constraints.iter_mut()
    }

    fn constraints_has_constraint<'a>(
        constraint: &Constraint,
        constraints: impl Iterator<Item = &'a Constraint>,
    ) -> bool {
        for check in constraints {
            match (constraint, check) {
                (Constraint::Eq(_, t11, t12), Constraint::Eq(_, t21, t22)) => {
                    if t11 == t21 && t12 == t22 {
                        return true;
                    }
                }
                (Constraint::OneOf(_, t1, ts1), Constraint::OneOf(_, t2, ts2)) => {
                    if t1 == t2 && ts1 == ts2 {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }
}

impl Display for ConstraintSystem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(", \n"),
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Constraint {
    Eq(Span, Arc<Type>, Arc<Type>),
    // NOTE(loong): this constraint is mostly used to define overloaded
    // functions.
    OneOf(Span, TypeVar, BTreeSet<Arc<Type>>),
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Eq(_, t1, t2) => write!(f, "{} = {}", t1, t2),
            Constraint::OneOf(_, t, ts) => {
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
) -> Arc<Type> {
    match expr {
        Expr::Var(var) => {
            let possible_schemes = match env.get(&var.name) {
                Some(ps) => ps,
                None => {
                    constraint_system
                        .errors
                        .insert(TypeError::UnboundVariable(*expr.span(), var.name.clone()));
                    return Arc::new(Type::Var(TypeVar::new()));
                }
            };

            let mut possible_types: BTreeSet<Arc<Type>> = BTreeSet::new();
            for scheme in possible_schemes.iter() {
                possible_types.insert(instantiate(scheme, expr.span(), constraint_system));
            }

            if possible_types.len() == 1 {
                possible_types.into_iter().next().unwrap()
            } else {
                // Create fresh type variable for this use
                let fresh_var = TypeVar::new();

                // Add OneOf constraint with same possibilities
                constraint_system.add_one_of(*expr.span(), fresh_var, possible_types.clone());

                Arc::new(Type::Var(fresh_var))
            }
        }

        Expr::Dict(_span, exprs) => {
            let mut kvs = BTreeMap::new();

            // Generate constraints for each expression in the tuple
            for (key, expr) in exprs {
                let ty = generate_constraints(expr, env, constraint_system);
                kvs.insert(key.clone(), ty);
            }

            Arc::new(Type::Dict(kvs))
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
                let ty = generate_constraints(expr, env, constraint_system);
                types.push(ty);
            }

            Arc::new(Type::Tuple(types))
        }

        Expr::List(span, exprs) => {
            // If list is empty, create a fresh type variable for element type
            if exprs.is_empty() {
                let elem_ty = Arc::new(Type::Var(TypeVar::new()));
                return Type::list(elem_ty);
            }

            // Generate constraints for all expressions
            let mut types = Vec::new();
            for expr in exprs {
                let ty = generate_constraints(expr, env, constraint_system);
                types.push(ty);
            }

            // Add constraints that all elements must have the same type
            for ty in &types[1..] {
                constraint_system.add_eq(*span, types[0].clone(), ty.clone());
            }

            Type::list(types[0].clone())
        }

        Expr::App(span, f, x) => {
            let f_type = generate_constraints(f, env, constraint_system);
            let x_type = generate_constraints(x, env, constraint_system);

            let result_type = Arc::new(Type::Var(TypeVar::new()));

            let expected_f_type = Type::arrow(x_type.clone(), result_type.clone());

            constraint_system.add_eq(*span, f_type, expected_f_type);

            result_type
        }

        Expr::Lam(_span, _scope, param, body) => {
            let param_type = Arc::new(Type::Var(TypeVar::new()));

            let mut new_env = env.clone();
            new_env.insert(
                param.name.clone(),
                HashSet::from([TypeScheme::from(param_type.clone())]),
            );

            let body_type = generate_constraints(body, &new_env, constraint_system);

            Type::arrow(param_type, body_type)
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
            let old_len = constraint_system.constraints.len();
            let def_type = generate_constraints(def, env, constraint_system);

            // Solve definition constraints to get its type
            unify::unify_constraints(constraint_system);
            let solved_def_type = def_type.apply(&constraint_system.subst);

            // Generalize the type
            let mut gen_deps: BTreeSet<TypeVar> = BTreeSet::new();
            for i in old_len..constraint_system.constraints.len() {
                match &constraint_system.constraints[i] {
                    Constraint::OneOf(_, v, _) => {
                        gen_deps.insert(*v);
                    }
                    Constraint::Eq(_, x, _) => {
                        if let Type::Var(id) = &**x {
                            gen_deps.insert(*id);
                        }
                    }
                }
            }

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
            let type_scheme = match solved_def_type.as_applied_type() {
                Some(AppliedType::Arrow(_, _)) => generalize(env, &solved_def_type, gen_deps),
                _ => TypeScheme::from(solved_def_type),
            };
            // ```

            // Add generalized type to environment
            let mut new_env = env.clone();
            new_env.insert(var.name.clone(), HashSet::from([type_scheme]));

            // Generate constraints for the body with the new environment
            generate_constraints(body, &new_env, constraint_system)
        }

        Expr::Ite(span, cond, then_branch, else_branch) => {
            // Generate constraints for all parts
            let cond_type = generate_constraints(cond, env, constraint_system);
            let then_type = generate_constraints(then_branch, env, constraint_system);
            let else_type = generate_constraints(else_branch, env, constraint_system);

            // Condition must be boolean
            constraint_system.add_eq(*cond.span(), cond_type, Arc::new(Type::Con(TypeCon::Bool)));
            // Then and else branches must have the same type
            constraint_system.add_eq(*span, then_type.clone(), else_type);

            then_type
        }

        Expr::Bool(_span, _x) => Arc::new(Type::Con(TypeCon::Bool)),
        Expr::Uint(_span, _x) => Arc::new(Type::Con(TypeCon::Uint)),
        Expr::Int(_span, _x) => Arc::new(Type::Con(TypeCon::Int)),
        Expr::Float(_span, _x) => Arc::new(Type::Con(TypeCon::Float)),
        Expr::String(_span, _x) => Arc::new(Type::Con(TypeCon::String)),
        Expr::Uuid(_span, _x) => Arc::new(Type::Con(TypeCon::Uuid)),
        Expr::DateTime(_span, _x) => Arc::new(Type::Con(TypeCon::DateTime)),

        Expr::Curry(..) => {
            todo!("generate_constraints for Expr::Curry just like we do for Expr::App")
        }
    }
}

// For generalization, we also need to know which variables are free in the environment
fn env_free_vars(env: &TypeEnv) -> HashSet<TypeVar> {
    let mut vars = HashSet::new();
    for entry in env.values() {
        for scheme in entry {
            vars.extend(scheme.free_vars())
        }
    }
    vars
}

// Generalize a type by quantifying over any type variables that aren't free in the environment
fn generalize(env: &TypeEnv, ty: &Arc<Type>, deps: BTreeSet<TypeVar>) -> TypeScheme {
    let env_vars = env_free_vars(env);
    let ty_vars = ty.free_vars();

    // Find variables that are free in ty but not in env
    let mut to_quantify: Vec<_> = ty_vars.difference(&env_vars).cloned().collect();
    to_quantify.sort(); // Make generalization deterministic
    to_quantify.reverse(); // TODO: remove this

    TypeScheme {
        vars: to_quantify,
        ty: ty.clone(),
        deps: deps.into_iter().collect(),
    }
}

// Instantiate a type scheme by replacing quantified variables with fresh ones
fn instantiate(
    scheme: &TypeScheme,
    span: &Span,
    constraint_system: &mut ConstraintSystem,
) -> Arc<Type> {
    if scheme.vars.is_empty() {
        return scheme.ty.clone();
    }

    let mut type_subst = Subst::default();
    let mut var_subst: HashMap<TypeVar, TypeVar> = HashMap::new();

    // Create fresh type variables
    for var in scheme.vars.iter() {
        let fresh_var = TypeVar::new_with_kind(var.kind());
        type_subst = type_subst.insert(*var, Arc::new(Type::Var(fresh_var)));
        var_subst.insert(*var, fresh_var);
    }

    // Create fresh vars for dependencies
    let mut new_constraint_sytem = ConstraintSystem::new();
    for dep_var in scheme.deps.iter() {
        let fresh_dep_var = TypeVar::new_with_kind(dep_var.kind());
        // Add equality constraint between old and new var
        type_subst = type_subst.insert(*dep_var, Arc::new(Type::Var(fresh_dep_var)));

        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(_, x1, t2) => {
                    if let Type::Var(t1) = &**x1 {
                        if t1 == dep_var {
                            new_constraint_sytem.add_eq(
                                *span,
                                Arc::new(Type::Var(fresh_dep_var)),
                                t2.clone(),
                            );
                        }
                    }
                }
                Constraint::OneOf(_, t1, ts) => {
                    if t1 == dep_var {
                        new_constraint_sytem.add_one_of(*span, fresh_dep_var, ts.clone());
                    }
                }
            }
        }
    }

    new_constraint_sytem.apply_subst(&type_subst, &var_subst);
    constraint_system.extend(new_constraint_sytem);

    inst_helper(&scheme.ty, &mut type_subst)
}

fn inst_helper(ty: &Arc<Type>, subst: &mut Subst) -> Arc<Type> {
    match &**ty {
        Type::UnresolvedVar(_) => todo!("instantiate/inst_helper should return a result"),
        Type::Var(id) => match subst.get(id) {
            Some(t) => t.clone(),
            None => Arc::new(Type::Var(*id)),
        },
        Type::ADT(adt) => Arc::new(Type::ADT(ADT {
            docs: adt.docs.clone(),
            name: adt.name.clone(),
            variants: adt
                .variants
                .iter()
                .map(|v| ADTVariant {
                    docs: v.docs.clone(),
                    name: v.name.clone(),
                    t: v.t.as_ref().map(|t| inst_helper(t, subst)),
                    t_docs: v.t_docs.clone(),
                    discriminant: v.discriminant,
                })
                .collect(),
        })),
        Type::App(a, b) => Arc::new(Type::App(inst_helper(a, subst), inst_helper(b, subst))),
        Type::Dict(kts) => Arc::new(Type::Dict(
            kts.iter()
                .map(|(k, t)| (k.clone(), inst_helper(t, subst)))
                .collect(),
        )),
        Type::Tuple(ts) => Arc::new(Type::Tuple(
            ts.iter().map(|t| inst_helper(t, subst)).collect(),
        )),
        Type::Con(_) => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::collections::HashMap;

    use rex_ast::expr::{Scope, Var};
    use rex_lexer::span::Span;

    use crate::{
        arrow, bool, int, list,
        trace::{sprint_subst, sprint_type_env},
        tuple,
        types::TypeEnv,
        unify, var,
    };

    use super::*;

    fn insert_type(env: &mut HashMap<String, HashSet<TypeScheme>>, n: &str, t: Arc<Type>) {
        env.insert(n.to_string(), HashSet::from([TypeScheme::from(t)]));
    }

    #[test]
    fn test_free_vars() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();

        // α -> β
        let ty = arrow!(var!(alpha) => var!(beta));
        let vars = ty.free_vars();
        assert_eq!(vars.len(), 2);
        assert!(vars.contains(&alpha));
        assert!(vars.contains(&beta));

        // ∀α. α -> β
        let scheme = TypeScheme::new(vec![alpha], arrow!(var!(alpha) => var!(beta)));
        let vars = scheme.free_vars();
        assert_eq!(vars.len(), 1);
        assert!(vars.contains(&beta));
    }

    #[test]
    fn test_generalize() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();

        let mut env = TypeEnv::new();

        // Environment with β free
        insert_type(&mut env, "y", var!(beta));

        // Type to generalize: α -> β
        let ty = arrow!(var!(alpha) => var!(beta));

        // Should become ∀α. α -> β
        // (β isn't quantified because it appears in env)
        let scheme = generalize(&env, &ty, BTreeSet::new());
        assert_eq!(*scheme.vars, vec![alpha]);
        if let Some(AppliedType::Arrow(arg, ret)) = scheme.ty.as_applied_type() {
            assert_eq!(*arg, var!(alpha));
            assert_eq!(*ret, var!(beta));
        } else {
            panic!("Expected arrow type");
        }
    }

    #[test]
    fn test_instantiate() {
        let alpha = TypeVar::new();
        let beta = TypeVar::new();

        // ∀α. α -> β
        let scheme = TypeScheme::new(vec![alpha], arrow!(var!(alpha) => var!(beta)));

        let inst_ty = instantiate(&scheme, &Span::default(), &mut ConstraintSystem::new());

        // Should become γ -> β where γ is fresh
        if let Some(AppliedType::Arrow(arg, ret)) = inst_ty.as_applied_type() {
            // Arg should be a fresh variable
            assert_ne!(*arg, var!(alpha));
            assert_ne!(*arg, var!(beta));

            // Result should be original free variable
            assert_eq!(*ret, var!(beta));
        } else {
            panic!("Expected arrow type");
        }
    }

    #[test]
    fn test_list_expr() {
        let mut env = TypeEnv::new();

        // Test [1, 2, 3]
        let expr = Expr::List(
            Span::default(),
            vec![
                Arc::new(Expr::Var(Var::new("one"))),
                Arc::new(Expr::Var(Var::new("two"))),
                Arc::new(Expr::Var(Var::new("three"))),
            ],
        );

        insert_type(&mut env, "one", int!());
        insert_type(&mut env, "two", int!());
        insert_type(&mut env, "three", int!());

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // Solve constraints
        let mut subst = Subst::default();
        let mut did_change = false;
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(_, t1, t2) => {
                    unify::unify_eq(t1, t2, &Span::default(), &mut subst, &mut did_change).unwrap();
                }
                _ => panic!("Expected equality constraint"),
            }
        }

        // Final type should be [Int]
        let final_type = ty.apply(&subst);
        assert_eq!(final_type, list!(int!()));

        // Test that lists of mixed types fail
        let expr = Expr::List(
            Span::default(),
            vec![
                Arc::new(Expr::Var(Var::new("one"))),
                Arc::new(Expr::Var(Var::new("true"))),
            ],
        );

        insert_type(&mut env, "true", bool!());

        let mut constraint_system = ConstraintSystem::new();
        let _ty = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // This should fail unification
        let mut subst = Subst::default();
        let mut did_change = false;
        let result = constraint_system
            .constraints()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(_, t1, t2) => {
                    unify::unify_eq(t1, t2, &Span::default(), &mut subst, &mut did_change)
                }
                _ => panic!("Expected equality constraint"),
            });
        assert!(result.is_err());

        // Test empty list
        let expr = Expr::List(Span::default(), vec![]);
        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);
        match &*ty {
            Type::App(a, _) => {
                assert!(matches!(&**a, Type::Con(TypeCon::List)));
            }
            _ => {
                panic!("Expected list");
            }
        }
        assert!(constraint_system.is_empty());
    }

    #[test]
    fn test_list_expr_with_expected_type() {
        let mut env = TypeEnv::new();

        // let f = \xs -> head xs in f [1, true]
        // should fail, xs can't be both [Int] and [Bool]
        let expr = Expr::Let(
            Span::default(),
            Var::new("f"),
            // \xs -> head xs
            Arc::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("xs"),
                Arc::new(Expr::App(
                    Span::default(),
                    Arc::new(Expr::Var(Var::new("head"))),
                    Arc::new(Expr::Var(Var::new("xs"))),
                )),
            )),
            // f [1, true]
            Arc::new(Expr::App(
                Span::default(),
                Arc::new(Expr::Var(Var::new("f"))),
                Arc::new(Expr::List(
                    Span::default(),
                    vec![
                        Arc::new(Expr::Var(Var::new("int_val"))),
                        Arc::new(Expr::Var(Var::new("bool_val"))),
                    ],
                )),
            )),
        );

        // Set up environment
        let elem_var = TypeVar::new();
        env.insert(
            "head".to_string(),
            HashSet::from([TypeScheme::new(
                vec![elem_var],
                arrow!(list!(var!(elem_var)) => var!(elem_var)),
            )]),
        );
        insert_type(&mut env, "int_val", int!());
        insert_type(&mut env, "bool_val", bool!());

        let mut constraint_system = ConstraintSystem::new();
        let _ty = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // This should fail unification because the list elements don't match
        let mut subst = Subst::default();
        let mut did_change = false;
        let result = constraint_system
            .constraints()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(_, t1, t2) => {
                    unify::unify_eq(t1, t2, &Span::default(), &mut subst, &mut did_change)
                }
                _ => panic!("Expected equality constraint"),
            });
        assert!(result.is_err());
    }

    #[test]
    fn test_list_polymorphism() {
        let mut env = TypeEnv::new();

        // Set up environment with polymorphic head : ∀α. [α] -> α
        let elem_var = TypeVar::new();
        let head_scheme = TypeScheme::new(
            vec![elem_var],
            arrow!(list!(var!(elem_var)) => var!(elem_var)),
        );
        env.insert("head".to_string(), HashSet::from([head_scheme]));

        // Add example lists to environment
        insert_type(&mut env, "int_list", list!(int!()));
        insert_type(&mut env, "bool_list", list!(bool!()));

        let expr = Expr::Tuple(
            Span::default(),
            vec![
                Arc::new(Expr::App(
                    Span::default(),
                    Arc::new(Expr::Var(Var::new("head"))),
                    Arc::new(Expr::Var(Var::new("int_list"))),
                )),
                Arc::new(Expr::App(
                    Span::default(),
                    Arc::new(Expr::Var(Var::new("head"))),
                    Arc::new(Expr::Var(Var::new("bool_list"))),
                )),
            ],
        );

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // Solve constraints
        unify::unify_constraints(&mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        let final_type = ty.apply(&constraint_system.subst);
        assert_eq!(final_type, tuple!(int!(), bool!()));
    }

    #[test]
    fn test_let() {
        let mut env = TypeEnv::new();

        // Test expression: let id = (\x -> x) in id 1
        let expr = Expr::Let(
            Span::default(),
            Var::new("id"),
            Arc::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("x"),
                Arc::new(Expr::Var(Var::new("x"))),
            )),
            Arc::new(Expr::App(
                Span::default(),
                Arc::new(Expr::Var(Var::new("id"))),
                Arc::new(Expr::Var(Var::new("one"))),
            )),
        );

        insert_type(&mut env, "one", int!());

        let mut constraint_system = ConstraintSystem::new();
        let result_type = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // Solve constraints
        unify::unify_constraints(&mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // Result should be Int
        let final_result = result_type.apply(&constraint_system.subst);
        assert_eq!(final_result, int!());
    }

    #[test]
    fn test_let_polymorphism() {
        let mut env = TypeEnv::new();

        // let id = \x -> x
        // in (id 1, id true)
        let expr = Expr::Let(
            Span::default(),
            Var::new("id"),
            // \x -> x
            Arc::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("x"),
                Arc::new(Expr::Var(Var::new("x"))),
            )),
            // Create tuple of (id 1, id true)
            Arc::new(Expr::Tuple(
                Span::default(),
                vec![
                    Arc::new(Expr::App(
                        Span::default(),
                        Arc::new(Expr::Var(Var::new("id"))),
                        Arc::new(Expr::Var(Var::new("int_val"))),
                    )),
                    Arc::new(Expr::App(
                        Span::default(),
                        Arc::new(Expr::Var(Var::new("id"))),
                        Arc::new(Expr::Var(Var::new("bool_val"))),
                    )),
                ],
            )),
        );

        // Set up environment
        insert_type(&mut env, "int_val", int!());
        insert_type(&mut env, "bool_val", bool!());

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // Solve constraints
        unify::unify_constraints(&mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // The final type should be (Int, Bool)
        let final_type = ty.apply(&constraint_system.subst);
        assert_eq!(final_type, tuple!(int!(), bool!()));
    }

    #[test]
    fn test_if_then_else() {
        let mut env = TypeEnv::new();

        // Test expression: if true then 1 else 2
        let expr = Expr::Ite(
            Span::default(),
            Arc::new(Expr::Var(Var::new("true"))),
            Arc::new(Expr::Var(Var::new("one"))),
            Arc::new(Expr::Var(Var::new("two"))),
        );

        // Set up environment
        insert_type(&mut env, "true", bool!());
        insert_type(&mut env, "one", int!());
        insert_type(&mut env, "two", int!());

        // Generate constraints
        let mut constraint_system = ConstraintSystem::new();
        let result_type = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // Solve constraints
        unify::unify_constraints(&mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // Result should be Int
        let final_result = result_type.apply(&constraint_system.subst);
        assert_eq!(final_result, int!());
    }

    #[test]
    fn test_compose_polymorphism() {
        let mut env = TypeEnv::new();

        // let compose = \f -> \g -> \x -> f (g x)
        // in (
        //   compose not not true,  // Bool -> Bool -> Bool -> Bool
        //   compose (\x -> x + 1) (\x -> x + 2) 3  // Int -> Int -> Int -> Int
        // )
        let expr = Expr::Let(
            Span::default(),
            Var::new("compose"),
            Arc::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("f"),
                Arc::new(Expr::Lam(
                    Span::default(),
                    Scope::new_sync(),
                    Var::new("g"),
                    Arc::new(Expr::Lam(
                        Span::default(),
                        Scope::new_sync(),
                        Var::new("x"),
                        Arc::new(Expr::App(
                            Span::default(),
                            Arc::new(Expr::Var(Var::new("f"))),
                            Arc::new(Expr::App(
                                Span::default(),
                                Arc::new(Expr::Var(Var::new("g"))),
                                Arc::new(Expr::Var(Var::new("x"))),
                            )),
                        )),
                    )),
                )),
            )),
            Arc::new(Expr::Tuple(
                Span::default(),
                vec![
                    // compose not not true
                    Arc::new(Expr::App(
                        Span::default(),
                        Arc::new(Expr::App(
                            Span::default(),
                            Arc::new(Expr::App(
                                Span::default(),
                                Arc::new(Expr::Var(Var::new("compose"))),
                                Arc::new(Expr::Var(Var::new("not"))),
                            )),
                            Arc::new(Expr::Var(Var::new("not"))),
                        )),
                        Arc::new(Expr::Var(Var::new("bool_val"))),
                    )),
                    // compose (+1) (+2) 3
                    Arc::new(Expr::App(
                        Span::default(),
                        Arc::new(Expr::App(
                            Span::default(),
                            Arc::new(Expr::App(
                                Span::default(),
                                Arc::new(Expr::Var(Var::new("compose"))),
                                Arc::new(Expr::Var(Var::new("inc"))),
                            )),
                            Arc::new(Expr::Var(Var::new("inc2"))),
                        )),
                        Arc::new(Expr::Var(Var::new("int_val"))),
                    )),
                ],
            )),
        );

        // Set up environment
        insert_type(&mut env, "not", arrow!(bool!() => bool!()));
        insert_type(&mut env, "inc", arrow!(int!() => int!()));
        insert_type(&mut env, "inc2", arrow!(int!() => int!()));
        insert_type(&mut env, "bool_val", bool!());
        insert_type(&mut env, "int_val", int!());

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // Solve constraints
        unify::unify_constraints(&mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        // The final type should be (Bool, Int)
        let final_type = ty.apply(&constraint_system.subst);
        assert_eq!(final_type, tuple!(bool!(), int!()));
    }

    #[test]
    fn test_polymorphic_type_errors() {
        let mut env = TypeEnv::new();

        // let id = \x -> x in
        // (id true, id 1, id true + 1)  // Last one should fail!
        let expr = Expr::Let(
            Span::default(),
            Var::new("id"),
            Arc::new(Expr::Lam(
                Span::default(),
                Scope::new_sync(),
                Var::new("x"),
                Arc::new(Expr::Var(Var::new("x"))),
            )),
            Arc::new(Expr::Tuple(
                Span::default(),
                vec![
                    // id true - ok
                    Arc::new(Expr::App(
                        Span::default(),
                        Arc::new(Expr::Var(Var::new("id"))),
                        Arc::new(Expr::Var(Var::new("bool_val"))),
                    )),
                    // id 1 - ok
                    Arc::new(Expr::App(
                        Span::default(),
                        Arc::new(Expr::Var(Var::new("id"))),
                        Arc::new(Expr::Var(Var::new("int_val"))),
                    )),
                    // id true + 1 - should fail!
                    Arc::new(Expr::App(
                        Span::default(),
                        Arc::new(Expr::Var(Var::new("plus"))),
                        Arc::new(Expr::App(
                            Span::default(),
                            Arc::new(Expr::Var(Var::new("id"))),
                            Arc::new(Expr::Var(Var::new("bool_val"))),
                        )),
                    )),
                ],
            )),
        );

        // Set up environment
        insert_type(&mut env, "bool_val", bool!());
        insert_type(&mut env, "int_val", int!());
        insert_type(&mut env, "plus", arrow!(int!() => int!() => int!()));

        // This should fail with a type error
        let mut constraint_system = ConstraintSystem::new();
        let result: Result<Arc<Type>, Vec<TypeError>> =
            Ok(generate_constraints(&expr, &env, &mut constraint_system)).and_then(|ty| {
                assert_eq!(constraint_system.errors.len(), 0);
                let mut subst = Subst::default();
                let mut did_change = false;
                for constraint in constraint_system.constraints() {
                    match constraint {
                        Constraint::Eq(_, t1, t2) => {
                            unify::unify_eq(t1, t2, &Span::default(), &mut subst, &mut did_change)?
                        }
                        _ => panic!("Expected equality constraint"),
                    }
                }
                Ok(ty.apply(&subst))
            });

        match result {
            Ok(_) => panic!("Expected an error"),
            Err(es) => {
                assert_eq!(es.len(), 1);
                assert!(matches!(es[0], TypeError::CannotUnify(_, _, _, _)));
            }
        }
    }

    #[test]
    fn test_overloading_with_arguments() {
        let mut env = HashMap::new();

        let xor_type_var = TypeVar::new();
        insert_type(&mut env, "xor", var!(xor_type_var));

        // Single-type variables
        insert_type(&mut env, "true", bool!());
        insert_type(&mut env, "false", bool!());
        insert_type(&mut env, "one", int!());
        insert_type(&mut env, "two", int!());

        // Test bool version: xor true false
        let bool_expr = Expr::App(
            Span::default(),
            Arc::new(Expr::App(
                Span::default(),
                Arc::new(Expr::Var(Var::new("xor"))),
                Arc::new(Expr::Var(Var::new("true"))),
            )),
            Arc::new(Expr::Var(Var::new("false"))),
        );

        let mut constraint_system = ConstraintSystem::new();
        constraint_system.add_one_of(
            Span::default(),
            xor_type_var,
            vec![
                arrow!(bool!() => bool!() => bool!()),
                arrow!(int!() => int!() => int!()),
            ]
            .into_iter()
            .collect(),
        );
        let ty = generate_constraints(&bool_expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        unify::unify_constraints(&mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);
        let final_type = ty.apply(&constraint_system.subst);
        assert_eq!(final_type, bool!());
    }

    #[test]
    fn test_overloading_with_arguments_and_multiple_uses() {
        let mut env = HashMap::new();

        env.insert(
            "xor".to_string(),
            vec![
                TypeScheme::new(Vec::new(), arrow!(bool!() => bool!() => bool!())),
                TypeScheme::new(Vec::new(), arrow!(int!() => int!() => int!())),
            ]
            .into_iter()
            .collect(),
        );

        // Single-type variables
        insert_type(&mut env, "true", bool!());
        insert_type(&mut env, "false", bool!());
        insert_type(&mut env, "one", int!());
        insert_type(&mut env, "two", int!());

        // Test bool version: xor true false
        let bool_expr = Expr::App(
            Span::default(),
            Arc::new(Expr::App(
                Span::default(),
                Arc::new(Expr::Var(Var::new("xor"))),
                Arc::new(Expr::Var(Var::new("true"))),
            )),
            Arc::new(Expr::Var(Var::new("false"))),
        );
        // Test int version: xor true false
        let int_expr = Expr::App(
            Span::default(),
            Arc::new(Expr::App(
                Span::default(),
                Arc::new(Expr::Var(Var::new("xor"))),
                Arc::new(Expr::Var(Var::new("one"))),
            )),
            Arc::new(Expr::Var(Var::new("two"))),
        );
        let tuple_expr = Expr::Tuple(
            Span::default(),
            vec![Arc::new(bool_expr), Arc::new(int_expr)],
        );
        // let xor_lam_expr = Expr::Lam(
        //     Span::default(),
        //     Scope::default(),
        //     Var::new("x"),
        //     Arc::new(Expr::Lam(
        //         Span::default(),
        //         Scope::default(),
        //         Var::new("y"),
        //         Arc::new(Expr::App(
        //             Span::default(),
        //             Arc::new(Expr::App(
        //                 Span::default(),
        //                 Arc::new(Expr::Var(Var::new("xor"))),
        //                 Arc::new(Expr::Var(Var::new("x"))),
        //             )),
        //             Arc::new(Expr::Var(Var::new("y"))),
        //         )),
        //     )),
        // );
        // let let_expr = Expr::Let(
        //     Span::default(),
        //     Var::new("f"),
        //     // Box::new(xor_lam_expr),
        //     Arc::new(Expr::Var(Var::new("xor"))),
        //     Arc::new(tuple_expr),
        // );

        let mut constraint_system = ConstraintSystem::new();
        let ty = generate_constraints(&tuple_expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        println!(
            "TYPES:\n{}\n\nCONSTRAINTS:\n{}",
            sprint_type_env(&env),
            &constraint_system
        );

        unify::unify_constraints(&mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        let final_type = ty.apply(&constraint_system.subst);

        println!("SUBST: {}", sprint_subst(&constraint_system.subst));
        println!("FINAL TYPE: {}", final_type);

        assert_eq!(final_type, tuple!(bool!(), int!()));
    }

    #[test]
    fn test_overloading_with_return() {
        let mut env = TypeEnv::new();

        let rand_type_var = TypeVar::new();
        insert_type(&mut env, "rand", var!(rand_type_var));

        // Add functions that force return type selection
        insert_type(&mut env, "sum", arrow!(list!(int!()) => int!()));
        insert_type(&mut env, "any", arrow!(list!(bool!()) => bool!()));

        // Test sum [rand, rand]
        let sum_expr = Expr::App(
            Span::default(),
            Arc::new(Expr::Var(Var::new("sum"))),
            Arc::new(Expr::List(
                Span::default(),
                vec![
                    Arc::new(Expr::Var(Var::new("rand"))),
                    Arc::new(Expr::Var(Var::new("rand"))),
                ],
            )),
        );

        let mut constraint_system = ConstraintSystem::new();
        constraint_system.add_one_of(
            Span::default(),
            rand_type_var,
            vec![int!(), bool!()].into_iter().collect(),
        );
        let ty = generate_constraints(&sum_expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        unify::unify_constraints(&mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        let final_type = ty.apply(&constraint_system.subst);
        assert_eq!(final_type, int!());
    }

    // TODO: get this test working again
    // #[test]
    fn _test_overloading_with_ambiguity() {
        let mut env = HashMap::new();

        let rand_type_var = TypeVar::new();
        insert_type(&mut env, "rand", var!(rand_type_var));
        // Test: just rand by itself (should be ambiguous)
        let expr = Expr::Var(Var::new("rand"));

        let mut constraint_system = ConstraintSystem::new();
        constraint_system.add_one_of(
            Span::default(),
            rand_type_var,
            vec![int!(), bool!()].into_iter().collect(),
        );
        let _ty = generate_constraints(&expr, &env, &mut constraint_system);
        assert_eq!(constraint_system.errors.len(), 0);

        let mut subst = Subst::default();
        let mut did_change = false;

        // This should fail because both types are possible
        let result = constraint_system
            .constraints()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(_, t1, t2) => {
                    unify::unify_eq(t1, t2, &Span::default(), &mut subst, &mut did_change)
                }
                Constraint::OneOf(_, t1, t2_possibilties) => unify::unify_one_of(
                    &Arc::new(Type::Var(*t1)),
                    &t2_possibilties,
                    &Span::default(),
                    &mut subst,
                    &mut did_change,
                )
                .map_err(|e| vec![e]),
            });
        assert!(result.is_err());
    }
}
