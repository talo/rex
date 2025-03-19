use rex_type_system::{
    constraint::{Constraint, ConstraintSystem},
    types::{ToType, Type, TypeEnv},
}; 
use std::{
    collections::{HashMap, HashSet, BTreeSet},
    future::Future,
    pin::Pin,
};

use crate::{
    codec::{Decode, Encode, Func},
    error::Error,
    eval::{apply, Context},
    ftable::{Ftable, FtableFn, A, B, C, D, E, F},
};
use chrono::{DateTime, Utc};
use regex::Regex;
use rex_ast::expr::Expr;
use rex_ast::id::Id;
use rex_lexer::span::Span;
use uuid::Uuid;

fn register_fn_core<State>(builder: &mut Builder<State>, n: &str, t: Type) -> Result<(), Error>
where
    State: Clone + Send + Sync + 'static,
{
    let unresolved_vars = t.unresolved_vars();

    match builder.fconstraints.get(n) {
        None if !unresolved_vars.is_empty() => {
            if builder.ftenv.contains_key(n) {
                return Err(Error::ParametricOverload {
                    name: n.to_string(),
                    prev_insts: vec![],
                    curr_inst: t,
                });
            }

            let mut assignments = HashMap::new();
            for var in &unresolved_vars {
                if let None = assignments.get(var) {
                    assignments.insert(
                        var.clone(),
                        Type::Var(Id::new()),
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
                if let Type::Var(var) = var {
                    for_all = Type::ForAll(
                        var,
                        Box::new(for_all),
                        BTreeSet::new()
                    );
                } else {
                    panic!("Expected a type variable");
                }
            }

            builder.ftenv.insert(
                n.to_string(),
                for_all,
            );
        }
        None => {
            let new_id = Id::new();
            builder.fconstraints
                .insert(n.to_string(), Constraint::Eq(Type::Var(new_id), t));
            builder.ftenv.insert(n.to_string(), Type::Var(new_id));
        }
        Some(_) if !unresolved_vars.is_empty() => {
            return Err(Error::ParametricOverload {
                name: n.to_string(),
                prev_insts: vec![],
                curr_inst: t,
            });
        }
        Some(Constraint::Eq(tid, prev_t)) => {
            let mut new_ts = HashSet::new();
            new_ts.insert(prev_t.clone());
            new_ts.insert(t);

            builder.fconstraints
                .insert(n.to_string(), Constraint::OneOf(tid.clone(), new_ts));
        }
        Some(Constraint::OneOf(tid, prev_ts)) => {
            let mut new_ts = prev_ts.clone();
            new_ts.insert(t);

            builder.fconstraints
                .insert(n.to_string(), Constraint::OneOf(tid.clone(), new_ts));
        }
    }

    Ok(())
}

macro_rules! impl_register_fn_core {
    ($self:expr, $n:expr, $f:expr, $name:ident $(,$($param:ident),*)?) => {{
        let n = $n.to_string();
        let t = <fn($($($param,)*)?) -> B as ToType>::to_type();
        register_fn_core($self, &n, t)?;
        $self.ftable.$name(n, $f);
        Ok(())
    }}
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

        this.register_fn2("==", |_ctx: &Context<_>, x: u64, y: u64| Ok(x == y))?;
        this.register_fn2("==", |_ctx: &Context<_>, x: i64, y: i64| Ok(x == y))?;
        this.register_fn2("==", |_ctx: &Context<_>, x: f64, y: f64| Ok(x == y))?;

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

        // Registers function with two parameters to match a regex pattern,
        // Returns True if there is a match, orelse False
        // pattern : Pattern to convert into regex (re),
        // hay : to match against the re-created from pattern

        // Possibly a costly computation while looping, since fn essentially recompiles pattern multiple times (no cache)
        this.register_fn2(
            "regex_matches",
            |_ctx: &Context<_>, pattern: String, hay: String| match Regex::new(&pattern)
                .map_err(Error::from)
            {
                Ok(re) => Ok(re.is_match(&hay)),
                Err(err) => Err(err), // Return Err(Error)
            },
        )?;

        // Registers function with two parameters to return successive non-overlapping matches in given haystack
        // pattern : Pattern to convert into regex (re),
        // hay : to match against the re-created from pattern

        // Possibly a costly computation while looping, since fn essentially recompiles pattern multiple times (no cache)
        this.register_fn2(
            "regex_captures",
            |_ctx: &Context<_>, pattern: String, hay: String| match Regex::new(&pattern)
                .map_err(Error::from)
            {
                Ok(re) => {
                    let matches: Vec<_> =
                        re.find_iter(&hay).map(|m| m.as_str().to_string()).collect();
                    return Ok(matches);
                }
                Err(err) => Err(err),
            },
        )?;

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

        this.register_fn_async3(
            "foldl",
            |ctx, f: Func<A, Func<B, A>>, base: A, xs: Vec<B>| {
                Box::pin(async move {
                    let mut res = base;
                    for x in xs {
                        let ares1 = apply(ctx, &f, &res).await?;
                        let ares2 = apply(ctx, &ares1, &x).await?;
                        res = A(ares2)
                    }
                    Ok(res)
                })
            },
        )?;

        this.register_fn_async3(
            "foldr",
            |ctx, f: Func<A, Func<B, B>>, base: B, xs: Vec<A>| {
                Box::pin(async move {
                    let mut res = base;
                    for x in xs.iter().rev() {
                        let ares1 = apply(ctx, &f, x).await?;
                        let ares2 = apply(ctx, &ares1, &res).await?;
                        res = B(ares2);
                    }
                    Ok(res)
                })
            },
        )?;

        this.register_fn_async3(".", |ctx, f: Func<B, C>, g: Func<A, B>, x: A| {
            Box::pin(async move {
                let x = apply(ctx, &g, &x).await?;
                let x = apply(ctx, &f, &x).await?;
                Ok(C(x))
            })
        })?;

        // Result
        this.register_fn1("Ok", |_ctx: &Context<_>, x: A| Ok(Ok::<A, B>(x)))?;
        this.register_fn1("Err", |_ctx: &Context<_>, x: B| Ok(Err::<A, B>(x)))?;
        this.register_fn_async2("map_result", |ctx, f: Func<A, B>, x: Result<A, E>| {
            Box::pin(async move {
                match x {
                    Ok(x) => Ok(Ok(B(apply(ctx, &f, &x).await?))),
                    Err(e) => Ok(Err(e)),
                }
            })
        })?;
        this.register_fn_async2(
            "and_then_result",
            |ctx, f: Func<A, Result<B, E>>, x: Result<A, E>| {
                Box::pin(async move {
                    match x {
                        Ok(x) => Ok(Result::<B, E>::try_decode(&apply(ctx, &f, &x).await?)?),
                        Err(e) => Ok(Err(e)),
                    }
                })
            },
        )?;
        this.register_fn_async2(
            "or_else_result",
            |ctx, f: Func<E, Result<A, F>>, x: Result<A, E>| {
                Box::pin(async move {
                    match x {
                        Ok(x) => Ok(Ok(x)),
                        Err(x) => Ok(Result::<A, F>::try_decode(&apply(ctx, &f, &x).await?)?),
                    }
                })
            },
        )?;
        this.register_fn_async2(
            "unwrap_or_else_result",
            |ctx, f: Func<E, A>, x: Result<A, E>| {
                Box::pin(async move {
                    match x {
                        Ok(x) => Ok(x),
                        Err(x) => Ok(A(apply(ctx, &f, &x).await?)),
                    }
                })
            },
        )?;

        // Option
        this.register_fn0("None", |_ctx: &Context<_>| Ok(None::<A>))?;
        this.register_fn1("Some", |_ctx: &Context<_>, x: A| Ok(Some(x)))?;
        this.register_fn_async2("map_option", |ctx, f: Func<A, B>, x: Option<A>| {
            Box::pin(async move {
                match x {
                    Some(x) => Ok(Some(B(apply(ctx, &f, &x).await?))),
                    None => Ok(None),
                }
            })
        })?;
        this.register_fn_async2(
            "and_then_option",
            |ctx, f: Func<A, Option<B>>, x: Option<A>| {
                Box::pin(async move {
                    match x {
                        Some(x) => Ok(Option::<B>::try_decode(&apply(ctx, &f, &x).await?)?),
                        None => Ok(None),
                    }
                })
            },
        )?;
        this.register_fn_async2(
            "or_else_option",
            |ctx, f: Func<(), Option<A>>, x: Option<A>| {
                Box::pin(async move {
                    match x {
                        Some(x) => Ok(Some(x)),
                        None => {
                            let x_id = Id::new();
                            let x = Expr::Tuple(x_id, Span::default(), vec![]);
                            ctx.env.write().await.insert(x_id, <()>::to_type());
                            let res = apply(ctx, &f, &x).await?;
                            ctx.env.write().await.remove(&x_id);
                            Ok(Option::<A>::try_decode(&res)?)
                        }
                    }
                })
            },
        )?;
        this.register_fn_async2(
            "unwrap_or_else_option",
            |ctx, f: Func<(), A>, x: Option<A>| {
                Box::pin(async move {
                    match x {
                        Some(x) => Ok(x),
                        None => {
                            let x_id = Id::new();
                            let x = Expr::Tuple(x_id, Span::default(), vec![]);
                            ctx.env.write().await.insert(x_id, <()>::to_type());
                            let res = apply(ctx, &f, &x).await?;
                            ctx.env.write().await.remove(&x_id);
                            Ok(A(res))
                        }
                    }
                })
            },
        )?;

        // Uuid
        this.register_fn1("string", |_ctx: &Context<_>, x: Uuid| Ok(format!("{}", x)))?;
        this.register_fn0("random_uuid", |_ctx: &Context<_>| Ok(Uuid::new_v4()))?;

        // DateTime
        this.register_fn1("string", |_ctx: &Context<_>, x: DateTime<Utc>| {
            Ok(format!("{}", x))
        })?;
        this.register_fn0("now", |_ctx: &Context<_>| Ok(Utc::now()))?;

        Ok(this)
    }

    pub fn register_fn_core_with_name(&mut self, name: &str, t: &Type, f: FtableFn<State>) -> Result<(), Error> {
        register_fn_core(self, name, t.clone())?;
        self.ftable.0.entry(name.to_string()).or_default().push((t.clone(), f));
        Ok(())
    }

    pub fn register_adt(&mut self, adt_type: &Type) -> Result<(), Error>
    {
        let Type::ADT(adt) = adt_type else {
            panic!("register_adt) called with non-ADT type: {}", adt_type);
        };

        // TODO: Avoid cloning args in the functions. This applies more generally across
        // the evaluation code. We should either pass owned Expr values to the functions
        // or use Rc<Expr> to make cloning cheap.

        // TODO: panic instead of returning Error; this requires register_fn_core to be
        // modified to do the same.

        for variant in &adt.variants {
            // We do not support multiple ADTs with overlapping constructor names. The reason
            // is because in the general case, each constructor may have a different number of
            // arguments, so we cannot consider them to be overloaded functions. Haskell has the
            // same restriction.
            if self.ftable.contains(&variant.name) {
                panic!("Duplicate constructor name: {}", variant.name);
            }

            // The functions we register here work directly with Expr values; there is no need
            // to convert to and from the corresponding native Rust type.
            let variant_name = variant.name.to_string();
            match variant.t.as_ref().map(|t| &**t) {
                None => {
                    self.register_fn_core_with_name(&variant.name, adt_type,
                        Box::new(move |_, _| {
                            let variant_name = variant_name.clone();
                            Box::pin(async move {
                                Ok(Expr::Named(
                                    Id::new(),
                                    Span::default(),
                                    variant_name,
                                    None))
                            })
                        }),
                    )?;
                }
                Some(Type::Tuple(fields)) => {
                    let mut fun_type = adt_type.clone();
                    for field in fields.iter().rev() {
                        fun_type = Type::Arrow(
                            Box::new(field.clone()),
                            Box::new(fun_type));
                    }
                    self.register_fn_core_with_name(&variant.name, &fun_type,
                        Box::new(move |_, args| {
                            let variant_name = variant_name.clone();
                            Box::pin(async move {
                                Ok(Expr::Named(
                                    Id::new(),
                                    Span::default(),
                                    variant_name,
                                    Some(Box::new(Expr::Tuple(
                                        Id::new(),
                                        Span::default(),
                                        args.clone())))))
                            })
                        }),
                    )?;
                }
                Some(t) => {
                    let fun_type = Type::Arrow(Box::new(t.clone()), Box::new(adt_type.clone()));
                    self.register_fn_core_with_name(&variant.name, &fun_type,
                        Box::new(move |_, args| {
                            let variant_name = variant_name.clone();
                            Box::pin(async move {
                                let val = args[0].clone();
                                Ok(Expr::Named(
                                    Id::new(),
                                    Span::default(),
                                    variant_name,
                                    Some(Box::new(val))))
                            })
                        }),
                    )?;
                }
            }
        }
        Ok(())
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
