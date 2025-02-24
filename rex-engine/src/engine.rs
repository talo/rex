use std::{
    collections::{HashMap, HashSet},
    future::Future,
    pin::Pin,
};

use rex_type_system::{
    constraint::{Constraint, ConstraintSystem},
    types::{ToType, Type, TypeEnv},
};

use crate::{
    codec::{Decode, Encode, Func},
    error::Error,
    eval::{apply, Context},
    ftable::{Ftable, A, B, C, D},
};

macro_rules! impl_register_fn_core {
    ($self:expr, $n:expr, $f:expr, $name:ident $(,$($param:ident),*)?) => {{
        let n = $n.to_string();
        let t = <fn($($($param,)*)?) -> B as ToType>::to_type();

        let unresolved_vars = t.unresolved_vars();

        match $self.fconstraints.get(&n) {
            None if !unresolved_vars.is_empty() => {
                if $self.ftenv.contains_key(&n) {
                    return Err(Error::ParametricOverload {
                        name: n,
                        prev_insts: vec![],
                        curr_inst: t,
                    });
                }

                let mut assignments = ::std::collections::HashMap::new();
                for var in &unresolved_vars {
                    if let None = assignments.get(var) {
                        assignments.insert(
                            var.clone(),
                            Type::Var(rex_ast::id::Id::new()),
                        );
                    }
                }

                // Temporarily make t mutable so we can resolve its type
                // unresolved variables
                let t = {
                    let mut t = t;
                    t.resolve_vars(&assignments);
                    t
                };

                // Build the type from inside out
                let mut for_all = t;
                for var in assignments.into_values() {
                    if let ::rex_type_system::types::Type::Var(var) = var {
                        for_all = ::rex_type_system::types::Type::ForAll(
                            var,
                            Box::new(for_all),
                            ::std::collections::BTreeSet::new()
                        );
                    } else {
                        panic!("Expected a type variable");
                    }
                }

                $self.ftenv.insert(
                    n.to_string(),
                    for_all,
                );
            }
            None => {
                let new_id = rex_ast::id::Id::new();
                $self.fconstraints
                    .insert(n.clone(), Constraint::Eq(Type::Var(new_id), t));
                $self.ftenv.insert(n.clone(), Type::Var(new_id));
            }
            Some(_) if !unresolved_vars.is_empty() => {
                return Err(Error::ParametricOverload {
                    name: n,
                    prev_insts: vec![],
                    curr_inst: t,
                });
            }
            Some(Constraint::Eq(tid, prev_t)) => {
                let mut new_ts = HashSet::new();
                new_ts.insert(prev_t.clone());
                new_ts.insert(t);

                $self.fconstraints
                    .insert(n.clone(), Constraint::OneOf(tid.clone(), new_ts));
            }
            Some(Constraint::OneOf(tid, prev_ts)) => {
                let mut new_ts = prev_ts.clone();
                new_ts.insert(t);

                $self.fconstraints
                    .insert(n.clone(), Constraint::OneOf(tid.clone(), new_ts));
            }
        }

        $self.ftable.$name(n, $f);

        Ok(())
    }};
}

macro_rules! impl_register_fn {
    ($name:ident $(,$($param:ident),*)?) => {
        pub fn $name <$($($param,)*)? B, F>(
            &mut self,
            n: impl ToString,
            f: F,
        ) ->
            Result<(), Error>
        where
            $($($param : Decode + Send + ToType,)*)?
            B: Encode + ToType,
            F: Fn(
                &Context<State>,
                $($($param,)*)?
            ) -> Result<B, Error>
                + Clone
                + Send
                + Sync
                + 'static
        {
            impl_register_fn_core!(self, n, f, $name $(,$($param),*)?)
        }
    };
}

macro_rules! impl_register_fn_async {
    ($name:ident, $($param:ident),*) => {
        #[allow(unused_assignments)] // This is a workaround for the unused_assignments lint for the last `i += 1` in the macro expansion.
        pub fn $name <$($param,)* B, F>(
            &mut self,
            n: impl ToString,
            f: F,
        ) ->
            Result<(), Error>
        where
            $($param : Decode + Send + ToType,)*
            B: Encode + ToType,
            for<'c> F: Fn(
                &'c Context<State>,
                $($param,)*
            ) -> Pin<Box<dyn Future<Output = Result<B, Error>> + Send + 'c>>
                + Clone
                + Send
                + Sync
                + 'static,
        {
            impl_register_fn_core!(self, n, f, $name, $($param),*)
        }
    };
}

pub struct Builder<State>
where
    State: Clone + Sync + 'static,
{
    pub fconstraints: HashMap<String, Constraint>,
    pub ftable: Ftable<State>,
    pub ftenv: TypeEnv,
}

impl<State> Builder<State>
where
    State: Clone + Send + Sync + 'static,
{
    pub fn with_prelude() -> Result<Self, Error> {
        let mut this = Self {
            ftable: Ftable::new(),
            fconstraints: Default::default(),
            ftenv: Default::default(),
        };

        this.register_fn1("swap", |_ctx: &Context<_>, (x, y): (A, B)| Ok((y, x)))?;

        this.register_fn0("zero", |_ctx: &Context<_>| Ok(0u64))?;
        this.register_fn0("zero", |_ctx: &Context<_>| Ok(0i64))?;
        this.register_fn0("zero", |_ctx: &Context<_>| Ok(0f64))?;

        this.register_fn1("uint", |_ctx: &Context<_>, x: i64| Ok(x as u64))?;
        this.register_fn1("uint", |_ctx: &Context<_>, x: f64| Ok(x as u64))?;
        this.register_fn1("uint", |_ctx: &Context<_>, x: String| {
            x.parse::<u64>().map_err(|e| e.into())
        })?;

        this.register_fn1("int", |_ctx: &Context<_>, x: u64| Ok(x as i64))?;
        this.register_fn1("int", |_ctx: &Context<_>, x: f64| Ok(x as i64))?;
        this.register_fn1("int", |_ctx: &Context<_>, x: String| {
            x.parse::<i64>().map_err(|e| e.into())
        })?;

        this.register_fn1("float", |_ctx: &Context<_>, x: u64| Ok(x as f64))?;
        this.register_fn1("float", |_ctx: &Context<_>, x: i64| Ok(x as f64))?;
        this.register_fn1("float", |_ctx: &Context<_>, x: String| {
            x.parse::<f64>().map_err(|e| e.into())
        })?;

        this.register_fn1("negate", |_ctx: &Context<_>, x: u64| Ok(-(x as i64)))?;
        this.register_fn1("negate", |_ctx: &Context<_>, x: i64| Ok(-x))?;
        this.register_fn1("negate", |_ctx: &Context<_>, x: f64| Ok(-x))?;

        this.register_fn2("&&", |_ctx: &Context<_>, x: bool, y: bool| Ok(x && y))?;
        this.register_fn2("||", |_ctx: &Context<_>, x: bool, y: bool| Ok(x || y))?;

        this.register_fn2("+", |_ctx: &Context<_>, x: u64, y: u64| Ok(x + y))?;
        this.register_fn2("+", |_ctx: &Context<_>, x: i64, y: i64| Ok(x + y))?;
        this.register_fn2("+", |_ctx: &Context<_>, x: f64, y: f64| Ok(x + y))?;

        this.register_fn2("-", |_ctx: &Context<_>, x: u64, y: u64| Ok(x - y))?;
        this.register_fn2("-", |_ctx: &Context<_>, x: i64, y: i64| Ok(x - y))?;
        this.register_fn2("-", |_ctx: &Context<_>, x: f64, y: f64| Ok(x - y))?;

        this.register_fn2("*", |_ctx: &Context<_>, x: u64, y: u64| Ok(x * y))?;
        this.register_fn2("*", |_ctx: &Context<_>, x: i64, y: i64| Ok(x * y))?;
        this.register_fn2("*", |_ctx: &Context<_>, x: f64, y: f64| Ok(x * y))?;

        this.register_fn2("/", |_ctx: &Context<_>, x: u64, y: u64| Ok(x / y))?;
        this.register_fn2("/", |_ctx: &Context<_>, x: i64, y: i64| Ok(x / y))?;
        this.register_fn2("/", |_ctx: &Context<_>, x: f64, y: f64| Ok(x / y))?;

        this.register_fn1("abs", |_ctx: &Context<_>, x: i64| Ok(x.abs()))?;
        this.register_fn1("abs", |_ctx: &Context<_>, x: f64| Ok(x.abs()))?;

        this.register_fn1("sqrt", |_ctx: &Context<_>, x: f64| Ok(x.sqrt()))?;

        this.register_fn2("pow", |_ctx: &Context<_>, x: f64, y: i32| Ok(x.powi(y)))?;
        this.register_fn2("pow", |_ctx: &Context<_>, x: f64, y: f64| Ok(x.powf(y)))?;

        this.register_fn1("id", |_ctx: &Context<_>, x: A| Ok(x))?;

        this.register_fn2("get", |_ctx: &Context<_>, n: u64, xs: Vec<A>| {
            Ok(xs[n as usize].clone())
        })?;

        this.register_fn1("elem0", |_ctx: &Context<_>, xs: (A,)| Ok(xs.0.clone()))?;
        this.register_fn1("elem1", |_ctx: &Context<_>, xs: (A, B)| Ok(xs.1.clone()))?;
        this.register_fn1("elem2", |_ctx: &Context<_>, xs: (A, B, C)| Ok(xs.2.clone()))?;
        this.register_fn1("elem3", |_ctx: &Context<_>, xs: (A, B, C, D)| {
            Ok(xs.3.clone())
        })?;

        this.register_fn2("++", |_ctx: &Context<_>, xs: Vec<A>, ys: Vec<A>| {
            let mut zs = Vec::with_capacity(xs.len() + ys.len());
            zs.extend(xs);
            zs.extend(ys);
            Ok(zs)
        })?;

        this.register_fn2("take", |_ctx: &Context<_>, n: u64, xs: Vec<A>| {
            Ok(xs.into_iter().take(n as usize).collect::<Vec<_>>())
        })?;

        this.register_fn2("skip", |_ctx: &Context<_>, n: u64, xs: Vec<A>| {
            Ok(xs.into_iter().take(n as usize).collect::<Vec<_>>())
        })?;

        this.register_fn2("zip", |_ctx: &Context<_>, xs: Vec<A>, ys: Vec<B>| {
            Ok(xs.into_iter().zip(ys.into_iter()).collect::<Vec<_>>())
        })?;

        this.register_fn1("unzip", |_ctx: &Context<_>, zs: Vec<(A, B)>| {
            let mut xs = Vec::with_capacity(zs.len());
            let mut ys = Vec::with_capacity(zs.len());
            for (x, y) in zs {
                xs.push(x);
                ys.push(y);
            }
            Ok((xs, ys))
        })?;

        this.register_fn_async3(
            "flip",
            |ctx: &Context<_>, f: Func<A, Func<B, C>>, x: B, y: A| {
                Box::pin(async move {
                    let g = apply(ctx, &f, &y).await?;
                    let z = apply(ctx, &g, &x).await?;
                    Ok(C(z))
                })
            },
        )?;

        this.register_fn_async2("map", |ctx, f: Func<A, B>, xs: Vec<A>| {
            Box::pin(async move {
                let mut ys: Vec<B> = Vec::with_capacity(xs.len());
                for x in xs {
                    let y = apply(ctx, &f, &x).await?;
                    ys.push(B(y));
                }
                Ok(ys)
            })
        })?;

        this.register_fn_async3(".", |ctx, f: Func<B, C>, g: Func<A, B>, x: A| {
            Box::pin(async move {
                let x = apply(ctx, &g, &x).await?;
                let x = apply(ctx, &f, &x).await?;
                Ok(C(x))
            })
        })?;

        Ok(this)
    }

    impl_register_fn!(register_fn0);
    impl_register_fn!(register_fn1, A0);
    impl_register_fn!(register_fn2, A0, A1);
    impl_register_fn!(register_fn3, A0, A1, A2);

    impl_register_fn_async!(register_fn_async1, A0);
    impl_register_fn_async!(register_fn_async2, A0, A1);
    impl_register_fn_async!(register_fn_async3, A0, A1, A2);

    pub fn build(self) -> (ConstraintSystem, Ftable<State>, TypeEnv) {
        let mut constraint_system = ConstraintSystem::new();
        for (_, constraint) in &self.fconstraints {
            constraint_system.add_global_constraint(constraint.clone());
        }
        (constraint_system, self.ftable, self.ftenv)
    }
}
