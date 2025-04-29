use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use rex_ast::{expr::Expr, id::Id};
use rex_lexer::span::Span;

use crate::{
    error::TypeError,
    types::{ADTVariant, Type, TypeEnv, TypeScheme, ADT},
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

    fn apply_subst_to_constraints<'a>(
        subst: &Subst,
        constraints: impl Iterator<Item = &'a mut Constraint>,
    ) {
        constraints.for_each(|constraint| match constraint {
            Constraint::Eq(_, t1, t2) => {
                *t1 = unify::apply_subst(t1, subst);
                *t2 = unify::apply_subst(t2, subst);
            }
            Constraint::OneOf(_, t, ts) => {
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
    Eq(Span, Arc<Type>, Arc<Type>),
    // NOTE(loong): this constraint is mostly used to define overloaded
    // functions.
    OneOf(Span, Arc<Type>, BTreeSet<Arc<Type>>),
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
    errors: &mut BTreeSet<TypeError>,
) -> Arc<Type> {
    match expr {
        Expr::Var(var) => {
            let possible_schemes = match env.get(&var.name) {
                Some(ps) => ps,
                None => {
                    errors.insert(TypeError::UnboundVariable(*expr.span(), var.name.clone()));
                    return Arc::new(Type::Var(Id::new()));
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
                let fresh_id = Id::new();
                let fresh_var = Arc::new(Type::Var(fresh_id));

                // Add OneOf constraint with same possibilities
                constraint_system.add_local_constraint(Constraint::OneOf(
                    *expr.span(),
                    fresh_var.clone(),
                    possible_types.clone(),
                ));

                fresh_var
            }
        }

        Expr::Dict(_span, exprs) => {
            let mut kvs = BTreeMap::new();

            // Generate constraints for each expression in the tuple
            for (key, expr) in exprs {
                let ty = generate_constraints(expr, env, constraint_system, errors);
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
                let ty = generate_constraints(expr, env, constraint_system, errors);
                types.push(ty);
            }

            Arc::new(Type::Tuple(types))
        }

        Expr::List(span, exprs) => {
            // If list is empty, create a fresh type variable for element type
            if exprs.is_empty() {
                let elem_ty = Type::Var(Id::new());
                return Arc::new(Type::List(Arc::new(elem_ty)));
            }

            // Generate constraints for all expressions
            let mut types = Vec::new();
            for expr in exprs {
                let ty = generate_constraints(expr, env, constraint_system, errors);
                types.push(ty);
            }

            // Add constraints that all elements must have the same type
            for ty in &types[1..] {
                constraint_system.add_local_constraint(Constraint::Eq(
                    *span,
                    types[0].clone(),
                    ty.clone(),
                ));
            }

            Arc::new(Type::List(types[0].clone()))
        }

        Expr::App(span, f, x) => {
            let f_type = generate_constraints(f, env, constraint_system, errors);
            let x_type = generate_constraints(x, env, constraint_system, errors);

            let result_type = Arc::new(Type::Var(Id::new()));

            let expected_f_type = Arc::new(Type::Arrow(x_type.clone(), result_type.clone()));

            constraint_system.add_local_constraint(Constraint::Eq(*span, f_type, expected_f_type));

            result_type
        }

        Expr::Lam(_span, _scope, param, body) => {
            let param_type = Arc::new(Type::Var(Id::new()));

            let mut new_env = env.clone();
            new_env.insert(
                param.name.clone(),
                HashSet::from([TypeScheme::from(param_type.clone())]),
            );

            let body_type = generate_constraints(body, &new_env, constraint_system, errors);

            Arc::new(Type::Arrow(param_type, body_type))
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

            let def_type = generate_constraints(def, env, &mut def_constraint_system, errors);

            // Solve definition constraints to get its type
            let def_subst = unify::unify_constraints(&def_constraint_system, errors);
            let solved_def_type = unify::apply_subst(&def_type, &def_subst);
            constraint_system.extend(def_constraint_system.clone());

            // Generalize the type
            let gen_deps = def_constraint_system
                .constraints()
                .filter_map(|c| match c {
                    Constraint::OneOf(_, x, _) => match &**x {
                        Type::Var(id) => Some(*id),
                        _ => None,
                    },
                    Constraint::Eq(_, x, _) => match &**x {
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
            let type_scheme = match &*solved_def_type {
                Type::Arrow(..) => generalize(env, &solved_def_type, gen_deps),
                _ => TypeScheme::from(solved_def_type),
            };
            // ```

            // Add generalized type to environment
            let mut new_env = env.clone();
            new_env.insert(var.name.clone(), HashSet::from([type_scheme]));

            // Generate constraints for the body with the new environment
            let result_type = generate_constraints(body, &new_env, constraint_system, errors);

            result_type
        }

        Expr::Ite(span, cond, then_branch, else_branch) => {
            // Generate constraints for all parts
            let cond_type = generate_constraints(cond, env, constraint_system, errors);
            let then_type = generate_constraints(then_branch, env, constraint_system, errors);
            let else_type = generate_constraints(else_branch, env, constraint_system, errors);

            // Condition must be boolean
            constraint_system.add_local_constraint(Constraint::Eq(
                *cond.span(),
                cond_type,
                Arc::new(Type::Bool),
            ));
            // Then and else branches must have the same type
            constraint_system.add_local_constraint(Constraint::Eq(
                *span,
                then_type.clone(),
                else_type,
            ));

            then_type
        }

        Expr::Bool(_span, _x) => Arc::new(Type::Bool),
        Expr::Uint(_span, _x) => Arc::new(Type::Uint),
        Expr::Int(_span, _x) => Arc::new(Type::Int),
        Expr::Float(_span, _x) => Arc::new(Type::Float),
        Expr::String(_span, _x) => Arc::new(Type::String),
        Expr::Uuid(_span, _x) => Arc::new(Type::Uuid),
        Expr::DateTime(_span, _x) => Arc::new(Type::DateTime),

        Expr::Curry(..) => {
            todo!("generate_constraints for Expr::Curry just like we do for Expr::App")
        }
    }
}

// For generalization, we need to find free type variables in a type
fn free_vars_type_scheme(scheme: &TypeScheme) -> HashSet<Id> {
    let mut set = free_vars(&scheme.ty);
    for id in scheme.ids.iter() {
        set.remove(id);
    }
    set
}

fn free_vars(ty: &Type) -> HashSet<Id> {
    match ty {
        Type::UnresolvedVar(_) => todo!("free_vars should return a result"),
        Type::Var(id) => {
            let mut set = HashSet::new();
            set.insert(*id);
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
    for entry in env.values() {
        for scheme in entry {
            vars.extend(free_vars_type_scheme(scheme))
        }
    }
    vars
}

// Generalize a type by quantifying over any type variables that aren't free in the environment
fn generalize(env: &TypeEnv, ty: &Arc<Type>, deps: BTreeSet<Id>) -> TypeScheme {
    let env_vars = env_free_vars(env);
    let ty_vars = free_vars(ty);

    // Find variables that are free in ty but not in env
    let mut to_quantify: Vec<_> = ty_vars.difference(&env_vars).cloned().collect();
    to_quantify.sort(); // Make generalization deterministic
    to_quantify.reverse(); // TODO: remove this

    TypeScheme {
        ids: to_quantify,
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
    if scheme.ids.len() == 0 {
        return scheme.ty.clone();
    }

    let mut subst = Subst::new();

    // Create fresh type variables
    for id in scheme.ids.iter() {
        let fresh_id = Id::new();
        subst.insert(*id, Arc::new(Type::Var(fresh_id)));
    }

    // Create fresh vars for dependencies
    let mut new_constraint_sytem = ConstraintSystem::new();
    for dep_id in scheme.deps.iter() {
        let fresh_dep_id = Id::new();
        // Add equality constraint between old and new var
        subst.insert(*dep_id, Arc::new(Type::Var(fresh_dep_id)));

        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(_, x1, t2) => match &**x1 {
                    Type::Var(t1) => {
                        if t1 == dep_id {
                            new_constraint_sytem.add_local_constraint(Constraint::Eq(
                                *span,
                                Arc::new(Type::Var(fresh_dep_id)),
                                t2.clone(),
                            ));
                        }
                    }
                    _ => {}
                },
                Constraint::OneOf(_, x1, ts) => match &**x1 {
                    Type::Var(t1) => {
                        if t1 == dep_id {
                            new_constraint_sytem.add_local_constraint(Constraint::OneOf(
                                *span,
                                Arc::new(Type::Var(fresh_dep_id)),
                                ts.clone(),
                            ));
                        }
                    }
                    _ => {}
                },
            }
        }
    }

    new_constraint_sytem.apply_subst(&subst);
    constraint_system.extend(new_constraint_sytem);

    inst_helper(&scheme.ty, span, &mut subst, constraint_system)
}

fn inst_helper(
    ty: &Arc<Type>,
    span: &Span,
    subst: &mut Subst,
    constraint_system: &mut ConstraintSystem,
) -> Arc<Type> {
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
                    t: v.t
                        .as_ref()
                        .map(|t| inst_helper(t, span, subst, constraint_system)),
                    t_docs: v.t_docs.clone(),
                    discriminant: v.discriminant,
                })
                .collect(),
        })),
        Type::Arrow(a, b) => Arc::new(Type::Arrow(
            inst_helper(a, span, subst, constraint_system),
            inst_helper(b, span, subst, constraint_system),
        )),
        Type::Result(t, e) => Arc::new(Type::Result(
            inst_helper(t, span, subst, constraint_system),
            inst_helper(e, span, subst, constraint_system),
        )),
        Type::Option(t) => Arc::new(Type::Option(inst_helper(t, span, subst, constraint_system))),
        Type::Promise(t) => Arc::new(Type::Promise(inst_helper(
            t,
            span,
            subst,
            constraint_system,
        ))),
        Type::List(t) => Arc::new(Type::List(inst_helper(t, span, subst, constraint_system))),
        Type::Dict(kts) => Arc::new(Type::Dict(
            kts.iter()
                .map(|(k, t)| (k.clone(), inst_helper(t, span, subst, constraint_system)))
                .collect(),
        )),
        Type::Tuple(ts) => Arc::new(Type::Tuple(
            ts.iter()
                .map(|t| inst_helper(t, span, subst, constraint_system))
                .collect(),
        )),

        Type::Bool
        | Type::Uint
        | Type::Int
        | Type::Float
        | Type::String
        | Type::Uuid
        | Type::DateTime => ty.clone(),
    }
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

    fn insert_type(env: &mut HashMap<String, HashSet<TypeScheme>>, n: &str, t: Arc<Type>) {
        env.insert(n.to_string(), HashSet::from([TypeScheme::from(t)]));
    }

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
        let scheme = TypeScheme {
            ids: vec![alpha],
            ty: Arc::new(Type::Arrow(
                Arc::new(Type::Var(alpha)),
                Arc::new(Type::Var(beta)),
            )),
            deps: BTreeSet::new(),
        };
        let vars = free_vars_type_scheme(&scheme);
        assert_eq!(vars.len(), 1);
        assert!(vars.contains(&beta));
    }

    #[test]
    fn test_generalize() {
        let alpha = Id::new();
        let beta = Id::new();

        let mut env = TypeEnv::new();

        // Environment with β free
        insert_type(&mut env, "y", Arc::new(Type::Var(beta)));

        // Type to generalize: α -> β
        let ty = Arc::new(Type::Arrow(
            Arc::new(Type::Var(alpha)),
            Arc::new(Type::Var(beta)),
        ));

        // Should become ∀α. α -> β
        // (β isn't quantified because it appears in env)
        let scheme = generalize(&env, &ty, BTreeSet::new());
        assert_eq!(*scheme.ids, vec![alpha]);
        match &*scheme.ty {
            Type::Arrow(arg, ret) => {
                assert_eq!(**arg, Type::Var(alpha));
                assert_eq!(**ret, Type::Var(beta));
            }
            _ => panic!("Expected arrow type"),
        }
    }

    #[test]
    fn test_instantiate() {
        let alpha = Id::new();
        let beta = Id::new();

        // ∀α. α -> β
        let scheme = TypeScheme {
            ids: vec![alpha],
            ty: Arc::new(Type::Arrow(
                Arc::new(Type::Var(alpha)),
                Arc::new(Type::Var(beta)),
            )),
            deps: BTreeSet::new(),
        };

        let inst_ty = instantiate(&scheme, &Span::default(), &mut ConstraintSystem::new());

        // Should become γ -> β where γ is fresh
        match &*inst_ty {
            Type::Arrow(arg, ret) => {
                // Arg should be a fresh variable
                assert_ne!(*arg, Arc::new(Type::Var(alpha)));
                assert_ne!(*arg, Arc::new(Type::Var(beta)));

                // Result should be original free variable
                assert_eq!(*ret, Arc::new(Type::Var(beta)));
            }
            _ => panic!("Expected arrow type"),
        }
    }

    #[test]
    fn test_list_expr() {
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

        insert_type(&mut env, "one", Arc::new(Type::Int));
        insert_type(&mut env, "two", Arc::new(Type::Int));
        insert_type(&mut env, "three", Arc::new(Type::Int));

        let mut constraint_system = ConstraintSystem::new();
        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // Solve constraints
        let mut subst = Subst::new();
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
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Arc::new(Type::List(Arc::new(Type::Int))));

        // Test that lists of mixed types fail
        let expr = Expr::List(
            Span::default(),
            vec![Expr::Var(Var::new("one")), Expr::Var(Var::new("true"))],
        );

        insert_type(&mut env, "true", Arc::new(Type::Bool));

        let mut constraint_system = ConstraintSystem::new();
        let mut errors = BTreeSet::new();
        let _ty = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // This should fail unification
        let mut subst = Subst::new();
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
        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);
        assert!(matches!(&*ty, Type::List(_)));
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
            HashSet::from([TypeScheme {
                ids: vec![elem_type],
                ty: Arc::new(Type::Arrow(
                    Arc::new(Type::List(Arc::new(Type::Var(elem_type)))),
                    Arc::new(Type::Var(elem_type)),
                )),
                deps: BTreeSet::new(),
            }]),
        );
        insert_type(&mut env, "int_val", Arc::new(Type::Int));
        insert_type(&mut env, "bool_val", Arc::new(Type::Bool));

        let mut constraint_system = ConstraintSystem::new();
        let mut errors = BTreeSet::new();
        let _ty = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // This should fail unification because the list elements don't match
        let mut subst = Subst::new();
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
        let elem_id = Id::new();
        let head_scheme = TypeScheme {
            ids: vec![elem_id],
            ty: Arc::new(Type::Arrow(
                Arc::new(Type::List(Arc::new(Type::Var(elem_id)))),
                Arc::new(Type::Var(elem_id)),
            )),
            deps: BTreeSet::new(),
        };
        env.insert("head".to_string(), HashSet::from([head_scheme]));

        // Add example lists to environment
        insert_type(
            &mut env,
            "int_list",
            Arc::new(Type::List(Arc::new(Type::Int))),
        );
        insert_type(
            &mut env,
            "bool_list",
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
        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        let final_type = unify::apply_subst(&ty, &subst);

        assert_eq!(
            final_type,
            Arc::new(Type::Tuple(vec![Arc::new(Type::Int), Arc::new(Type::Bool)]))
        );
    }

    #[test]
    fn test_let() {
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

        insert_type(&mut env, "one", Arc::new(Type::Int));

        let mut constraint_system = ConstraintSystem::new();
        let mut errors = BTreeSet::new();
        let result_type = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Arc::new(Type::Int));
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
        insert_type(&mut env, "int_val", Arc::new(Type::Int));
        insert_type(&mut env, "bool_val", Arc::new(Type::Bool));

        let mut constraint_system = ConstraintSystem::new();
        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // The final type should be (Int, Bool)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(
            final_type,
            Arc::new(Type::Tuple(vec![Arc::new(Type::Int), Arc::new(Type::Bool)]))
        );
    }

    #[test]
    fn test_if_then_else() {
        let mut env = TypeEnv::new();

        // Test expression: if true then 1 else 2
        let expr = Expr::Ite(
            Span::default(),
            Box::new(Expr::Var(Var::new("true"))),
            Box::new(Expr::Var(Var::new("one"))),
            Box::new(Expr::Var(Var::new("two"))),
        );

        // Set up environment
        insert_type(&mut env, "true", Arc::new(Type::Bool));
        insert_type(&mut env, "one", Arc::new(Type::Int));
        insert_type(&mut env, "two", Arc::new(Type::Int));

        // Generate constraints
        let mut constraint_system = ConstraintSystem::new();
        let mut errors = BTreeSet::new();
        let result_type = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // Result should be Int
        let final_result = unify::apply_subst(&result_type, &subst);
        assert_eq!(final_result, Arc::new(Type::Int));
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
        insert_type(
            &mut env,
            "not",
            Arc::new(Type::Arrow(Arc::new(Type::Bool), Arc::new(Type::Bool))),
        );
        insert_type(
            &mut env,
            "inc",
            Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
        );
        insert_type(
            &mut env,
            "inc2",
            Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
        );
        insert_type(&mut env, "bool_val", Arc::new(Type::Bool));
        insert_type(&mut env, "int_val", Arc::new(Type::Int));

        let mut constraint_system = ConstraintSystem::new();
        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // Solve constraints
        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        // The final type should be (Bool, Int)
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(
            final_type,
            Arc::new(Type::Tuple(vec![Arc::new(Type::Bool), Arc::new(Type::Int)]))
        );
    }

    #[test]
    fn test_polymorphic_type_errors() {
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
        insert_type(&mut env, "bool_val", Arc::new(Type::Bool));
        insert_type(&mut env, "int_val", Arc::new(Type::Int));
        insert_type(
            &mut env,
            "plus",
            Arc::new(Type::Arrow(
                Arc::new(Type::Int),
                Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
            )),
        );

        // This should fail with a type error
        let mut constraint_system = ConstraintSystem::new();
        let mut errors = BTreeSet::new();
        let result = Ok(generate_constraints(
            &expr,
            &env,
            &mut constraint_system,
            &mut errors,
        ))
        .and_then(|ty| {
            assert_eq!(errors.len(), 0);
            let mut subst = Subst::new();
            let mut did_change = false;
            for constraint in constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(_, t1, t2) => {
                        unify::unify_eq(t1, t2, &Span::default(), &mut subst, &mut did_change)?
                    }
                    _ => panic!("Expected equality constraint"),
                }
            }
            Ok(unify::apply_subst(&ty, &subst))
        });

        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            TypeError::CannotUnify(_, _, _)
        ));
    }

    #[test]
    fn test_overloading_with_arguments() {
        let mut env = HashMap::new();

        let xor_type_id = Id::new();
        insert_type(&mut env, "xor", Arc::new(Type::Var(xor_type_id)));

        // Single-type variables
        insert_type(&mut env, "true", Arc::new(Type::Bool));
        insert_type(&mut env, "false", Arc::new(Type::Bool));
        insert_type(&mut env, "one", Arc::new(Type::Int));
        insert_type(&mut env, "two", Arc::new(Type::Int));

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
                Span::default(),
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
        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&bool_expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Arc::new(Type::Bool));
    }

    #[test]
    fn test_overloading_with_arguments_and_multiple_uses() {
        let mut env = HashMap::new();

        env.insert(
            "xor".to_string(),
            vec![
                TypeScheme {
                    ids: Vec::new(),
                    ty: Arc::new(Type::Arrow(
                        Arc::new(Type::Bool),
                        Arc::new(Type::Arrow(Arc::new(Type::Bool), Arc::new(Type::Bool))),
                    )),
                    deps: BTreeSet::new(),
                },
                TypeScheme {
                    ids: Vec::new(),
                    ty: Arc::new(Type::Arrow(
                        Arc::new(Type::Int),
                        Arc::new(Type::Arrow(Arc::new(Type::Int), Arc::new(Type::Int))),
                    )),
                    deps: BTreeSet::new(),
                },
            ]
            .into_iter()
            .collect(),
        );

        // Single-type variables
        insert_type(&mut env, "true", Arc::new(Type::Bool));
        insert_type(&mut env, "false", Arc::new(Type::Bool));
        insert_type(&mut env, "one", Arc::new(Type::Int));
        insert_type(&mut env, "two", Arc::new(Type::Int));

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

        let mut constraint_system = ConstraintSystem::with_global_constraints(vec![]);
        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&tuple_expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        println!(
            "TYPES:\n{}\n\nCONSTRAINTS:\n{}",
            sprint_type_env(&env),
            &constraint_system
        );

        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        let final_type = unify::apply_subst(&ty, &subst);

        println!("SUBST: {}", sprint_subst(&subst));
        println!("FINAL TYPE: {}", final_type);

        assert_eq!(
            final_type,
            Arc::new(Type::Tuple(vec![Arc::new(Type::Bool), Arc::new(Type::Int)]))
        );
    }

    #[test]
    fn test_overloading_with_return() {
        let mut env = TypeEnv::new();

        let rand_type_id = Id::new();
        insert_type(&mut env, "rand", Arc::new(Type::Var(rand_type_id)));

        // Add functions that force return type selection
        insert_type(
            &mut env,
            "sum",
            Arc::new(Type::Arrow(
                Arc::new(Type::List(Arc::new(Type::Int))),
                Arc::new(Type::Int),
            )),
        );
        insert_type(
            &mut env,
            "any",
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
                Span::default(),
                Arc::new(Type::Var(rand_type_id)),
                vec![Arc::new(Type::Int), Arc::new(Type::Bool)]
                    .into_iter()
                    .collect(),
            )]);
        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&sum_expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Arc::new(Type::Int));
    }

    // TODO: get this test working again
    // #[test]
    fn _test_overloading_with_ambiguity() {
        let mut env = HashMap::new();

        let rand_type_id = Id::new();
        insert_type(&mut env, "rand", Arc::new(Type::Var(rand_type_id)));
        // Test: just rand by itself (should be ambiguous)
        let expr = Expr::Var(Var::new("rand"));

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Span::default(),
                Arc::new(Type::Var(rand_type_id)),
                vec![Arc::new(Type::Int), Arc::new(Type::Bool)]
                    .into_iter()
                    .collect(),
            )]);
        let mut errors = BTreeSet::new();
        let _ty = generate_constraints(&expr, &env, &mut constraint_system, &mut errors);
        assert_eq!(errors.len(), 0);

        let mut subst = Subst::new();
        let mut did_change = false;

        // This should fail because both types are possible
        let result = constraint_system
            .constraints()
            .try_for_each(|constraint| match constraint {
                Constraint::Eq(_, t1, t2) => {
                    unify::unify_eq(t1, t2, &Span::default(), &mut subst, &mut did_change)
                }
                Constraint::OneOf(_, t1, t2_possibilties) => unify::unify_one_of(
                    t1,
                    &t2_possibilties,
                    &Span::default(),
                    &mut subst,
                    &mut did_change,
                ),
            });
        assert!(result.is_err());
    }
}
