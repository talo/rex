use rex_type_system::types::{Dispatch, ToType, Type, TypeEnv, TypeScheme, ADT};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt,
    future::Future,
    pin::Pin,
    str::FromStr,
    sync::Arc,
};

use crate::{
    codec::{Decode, Encode, Func},
    error::Error,
    eval::{apply, Context},
    ftable::{Ftable, FtableFn, A, B, C, E, F},
};
use chrono::{DateTime, Utc};
use regex::Regex;
use rex_ast::expr::Expr;
use rex_ast::id::Id;
use rex_lexer::span::Span;
use uuid::Uuid;

const MAX_TUPLE_LEN: usize = 8; // register elem_M_N functions for tuple sizes up to M

fn register_fn_core<State>(builder: &mut Builder<State>, n: &str, t: Arc<Type>) -> Result<(), Error>
where
    State: Clone + Send + Sync + 'static,
{
    let unresolved_vars = t.unresolved_vars();

    let scheme = if !unresolved_vars.is_empty() {
        let mut assignments = HashMap::new();
        for var in &unresolved_vars {
            if let None = assignments.get(var) {
                assignments.insert(var.clone(), Arc::new(Type::Var(Id::new())));
            }
        }

        let t = t.resolve_vars(&assignments);

        let mut ids: Vec<Id> = Vec::new();
        for var in assignments.into_values() {
            if let Type::Var(var) = *var {
                ids.push(var);
            } else {
                panic!("Expected a type variable");
            }
        }
        let for_all = TypeScheme {
            ids,
            ty: t,
            deps: BTreeSet::new(),
        };
        for_all
    } else {
        TypeScheme::from(t)
    };

    match builder.builtins.get_mut(n) {
        None => {
            let mut ts = HashSet::new();
            ts.insert(scheme);
            builder.builtins.insert(n.to_string(), ts);
        }
        Some(set) => {
            for s in set.iter() {
                if scheme.maybe_overlaps_with(s) {
                    return Err(Error::OverlappingFunctions(
                        n.to_string(),
                        scheme.clone(),
                        s.clone(),
                    ));
                }
            }

            set.insert(scheme);
        }
    }

    Ok(())
}

macro_rules! impl_register_fn_core {
    ($self:expr, $n:expr, $f:expr, $name:ident $(,$($param:ident),*)?) => {{
        let n = $n.to_string();
        let t = Arc::new(<fn($($($param,)*)?) -> B as ToType>::to_type());
        register_fn_core($self, &n, t).unwrap();
        $self.ftable.$name(n, $f);
    }}
}

macro_rules! impl_register_fn {
    ($name:ident $(,$($param:ident),*)?) => {
        pub fn $name <$($($param,)*)? B, F>(
            &mut self,
            n: impl ToString,
            f: F,
        )
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
        )
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
    pub builtins: BTreeMap<String, HashSet<TypeScheme>>,
    pub ftable: Ftable<State>,
    pub adts: BTreeMap<String, ADT>,
    pub accessors: HashSet<String>,
}

impl<State> Builder<State>
where
    State: Clone + Send + Sync + 'static,
{
    pub fn with_prelude() -> Result<Self, Error> {
        let mut this = Self {
            builtins: BTreeMap::new(),
            ftable: Ftable::new(),
            adts: Default::default(),
            accessors: Default::default(),
        };

        this.register_fn1("swap", |_ctx: &Context<_>, (x, y): (A, B)| Ok((y, x)));

        this.register_fn1("uint", |_ctx: &Context<_>, x: i64| Ok(x as u64));
        this.register_fn1("uint", |_ctx: &Context<_>, x: f64| Ok(x as u64));
        this.register_fn1("uint", |_ctx: &Context<_>, x: String| {
            x.parse::<u64>().map_err(|e| e.into())
        });

        this.register_fn1("int", |_ctx: &Context<_>, x: u64| Ok(x as i64));
        this.register_fn1("int", |_ctx: &Context<_>, x: f64| Ok(x as i64));
        this.register_fn1("int", |_ctx: &Context<_>, x: String| {
            x.parse::<i64>().map_err(|e| e.into())
        });

        this.register_fn1("float", |_ctx: &Context<_>, x: u64| Ok(x as f64));
        this.register_fn1("float", |_ctx: &Context<_>, x: i64| Ok(x as f64));
        this.register_fn1("float", |_ctx: &Context<_>, x: String| {
            x.parse::<f64>().map_err(|e| e.into())
        });

        this.register_fn1("negate", |_ctx: &Context<_>, x: u64| Ok(-(x as i64)));
        this.register_fn1("negate", |_ctx: &Context<_>, x: i64| Ok(-x));
        this.register_fn1("negate", |_ctx: &Context<_>, x: f64| Ok(-x));

        this.register_fn2("&&", |_ctx: &Context<_>, x: bool, y: bool| Ok(x && y));
        this.register_fn2("||", |_ctx: &Context<_>, x: bool, y: bool| Ok(x || y));

        this.register_fn2("==", |_ctx: &Context<_>, x: u64, y: u64| Ok(x == y));
        this.register_fn2("==", |_ctx: &Context<_>, x: i64, y: i64| Ok(x == y));
        this.register_fn2("==", |_ctx: &Context<_>, x: f64, y: f64| Ok(x == y));
        this.register_fn2("==", |_ctx: &Context<_>, x: String, y: String| Ok(x == y));

        this.register_fn2("!=", |_ctx: &Context<_>, x: u64, y: u64| Ok(x != y));
        this.register_fn2("!=", |_ctx: &Context<_>, x: i64, y: i64| Ok(x != y));
        this.register_fn2("!=", |_ctx: &Context<_>, x: f64, y: f64| Ok(x != y));
        this.register_fn2("!=", |_ctx: &Context<_>, x: String, y: String| Ok(x != y));

        this.register_fn2(">", |_ctx: &Context<_>, x: u64, y: u64| Ok(x > y));
        this.register_fn2(">", |_ctx: &Context<_>, x: i64, y: i64| Ok(x > y));
        this.register_fn2(">", |_ctx: &Context<_>, x: f64, y: f64| Ok(x > y));
        this.register_fn2(">", |_ctx: &Context<_>, x: String, y: String| Ok(x > y));

        this.register_fn2(">=", |_ctx: &Context<_>, x: u64, y: u64| Ok(x >= y));
        this.register_fn2(">=", |_ctx: &Context<_>, x: i64, y: i64| Ok(x >= y));
        this.register_fn2(">=", |_ctx: &Context<_>, x: f64, y: f64| Ok(x >= y));
        this.register_fn2(">=", |_ctx: &Context<_>, x: String, y: String| Ok(x >= y));

        this.register_fn2("<", |_ctx: &Context<_>, x: u64, y: u64| Ok(x < y));
        this.register_fn2("<", |_ctx: &Context<_>, x: i64, y: i64| Ok(x < y));
        this.register_fn2("<", |_ctx: &Context<_>, x: f64, y: f64| Ok(x < y));
        this.register_fn2("<", |_ctx: &Context<_>, x: String, y: String| Ok(x < y));

        this.register_fn2("<=", |_ctx: &Context<_>, x: u64, y: u64| Ok(x <= y));
        this.register_fn2("<=", |_ctx: &Context<_>, x: i64, y: i64| Ok(x <= y));
        this.register_fn2("<=", |_ctx: &Context<_>, x: f64, y: f64| Ok(x <= y));
        this.register_fn2("<=", |_ctx: &Context<_>, x: String, y: String| Ok(x <= y));

        this.register_fn2("+", |_ctx: &Context<_>, x: u64, y: u64| Ok(x + y));
        this.register_fn2("+", |_ctx: &Context<_>, x: i64, y: i64| Ok(x + y));
        this.register_fn2("+", |_ctx: &Context<_>, x: f64, y: f64| Ok(x + y));

        this.register_fn2("-", |_ctx: &Context<_>, x: u64, y: u64| Ok(x - y));
        this.register_fn2("-", |_ctx: &Context<_>, x: i64, y: i64| Ok(x - y));
        this.register_fn2("-", |_ctx: &Context<_>, x: f64, y: f64| Ok(x - y));

        this.register_fn2("*", |_ctx: &Context<_>, x: u64, y: u64| Ok(x * y));
        this.register_fn2("*", |_ctx: &Context<_>, x: i64, y: i64| Ok(x * y));
        this.register_fn2("*", |_ctx: &Context<_>, x: f64, y: f64| Ok(x * y));

        this.register_fn2("/", |_ctx: &Context<_>, x: u64, y: u64| Ok(x / y));
        this.register_fn2("/", |_ctx: &Context<_>, x: i64, y: i64| Ok(x / y));
        this.register_fn2("/", |_ctx: &Context<_>, x: f64, y: f64| Ok(x / y));

        this.register_fn2("%", |_ctx: &Context<_>, x: u64, y: u64| Ok(x % y));
        this.register_fn2("%", |_ctx: &Context<_>, x: i64, y: i64| Ok(x % y));
        this.register_fn2("%", |_ctx: &Context<_>, x: f64, y: f64| Ok(x % y));

        this.register_fn1("abs", |_ctx: &Context<_>, x: i64| Ok(x.abs()));
        this.register_fn1("abs", |_ctx: &Context<_>, x: f64| Ok(x.abs()));

        this.register_fn1("sqrt", |_ctx: &Context<_>, x: f64| Ok(x.sqrt()));

        this.register_fn2("pow", |_ctx: &Context<_>, x: f64, y: i32| Ok(x.powi(y)));
        this.register_fn2("pow", |_ctx: &Context<_>, x: f64, y: f64| Ok(x.powf(y)));

        this.register_fn1("identity", |_ctx: &Context<_>, x: A| Ok(x));

        this.register_fn2("get", |_ctx: &Context<_>, n: u64, xs: Vec<A>| {
            Ok(xs[n as usize].clone())
        });

        this.register_elem_functions();

        this.register_fn2("++", |_ctx: &Context<_>, xs: Vec<A>, ys: Vec<A>| {
            let mut zs = Vec::with_capacity(xs.len() + ys.len());
            zs.extend(xs);
            zs.extend(ys);
            Ok(zs)
        });

        this.register_fn2("++", |_ctx: &Context<_>, a: String, b: String| {
            Ok(format!("{}{}", a, b))
        });

        this.register_fn2("take", |_ctx: &Context<_>, n: u64, xs: Vec<A>| {
            Ok(xs.into_iter().take(n as usize).collect::<Vec<_>>())
        });

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
        );

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
        );

        this.register_fn2("skip", |_ctx: &Context<_>, n: u64, xs: Vec<A>| {
            Ok(xs.into_iter().take(n as usize).collect::<Vec<_>>())
        });

        this.register_fn2("zip", |_ctx: &Context<_>, xs: Vec<A>, ys: Vec<B>| {
            Ok(xs.into_iter().zip(ys.into_iter()).collect::<Vec<_>>())
        });

        this.register_fn1("unzip", |_ctx: &Context<_>, zs: Vec<(A, B)>| {
            let mut xs = Vec::with_capacity(zs.len());
            let mut ys = Vec::with_capacity(zs.len());
            for (x, y) in zs {
                xs.push(x);
                ys.push(y);
            }
            Ok((xs, ys))
        });

        this.register_fn_async3(
            "flip",
            |ctx: &Context<_>, f: Func<A, Func<B, C>>, x: B, y: A| {
                Box::pin(async move {
                    let g = apply(ctx, &f, &y).await?;
                    let z = apply(ctx, &g, &x).await?;
                    Ok(C(z))
                })
            },
        );

        this.register_fn_async2("map", |ctx, f: Func<A, B>, xs: Vec<A>| {
            Box::pin(async move {
                let mut ys: Vec<B> = Vec::with_capacity(xs.len());
                for x in xs {
                    let y = apply(ctx, &f, &x).await?;
                    ys.push(B(y));
                }
                Ok(ys)
            })
        });

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
        );

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
        );

        this.register_fn_async3(".", |ctx, f: Func<B, C>, g: Func<A, B>, x: A| {
            Box::pin(async move {
                let x = apply(ctx, &g, &x).await?;
                let x = apply(ctx, &f, &x).await?;
                Ok(C(x))
            })
        });

        // Result
        this.register_fn1("Ok", |_ctx: &Context<_>, x: A| Ok(Ok::<A, B>(x)));
        this.register_fn1("Err", |_ctx: &Context<_>, x: B| Ok(Err::<A, B>(x)));
        this.register_fn_async2("map", |ctx, f: Func<A, B>, x: Result<A, E>| {
            Box::pin(async move {
                match x {
                    Ok(x) => Ok(Ok(B(apply(ctx, &f, &x).await?))),
                    Err(e) => Ok(Err(e)),
                }
            })
        });
        this.register_fn_async2(
            "and_then",
            |ctx, f: Func<A, Result<B, E>>, x: Result<A, E>| {
                Box::pin(async move {
                    match x {
                        Ok(x) => Ok(Result::<B, E>::try_decode(&apply(ctx, &f, &x).await?)?),
                        Err(e) => Ok(Err(e)),
                    }
                })
            },
        );
        this.register_fn_async2(
            "or_else",
            |ctx, f: Func<E, Result<A, F>>, x: Result<A, E>| {
                Box::pin(async move {
                    match x {
                        Ok(x) => Ok(Ok(x)),
                        Err(x) => Ok(Result::<A, F>::try_decode(&apply(ctx, &f, &x).await?)?),
                    }
                })
            },
        );
        this.register_fn_async2("unwrap_or_else", |ctx, f: Func<E, A>, x: Result<A, E>| {
            Box::pin(async move {
                match x {
                    Ok(x) => Ok(x),
                    Err(x) => Ok(A(apply(ctx, &f, &x).await?)),
                }
            })
        });
        this.register_fn_async1("unwrap", |_ctx, x: Result<A, E>| {
            Box::pin(async move {
                match x {
                    Ok(x) => Ok(x),
                    Err(e) => Err(Error::Custom {
                        error: format!(
                            "unwrap called with Err: {}",
                            e.try_encode(Span::default())?
                        ),
                        trace: Default::default(),
                    }),
                }
            })
        });

        // Option
        this.register_fn0("None", |_ctx: &Context<_>| Ok(None::<A>));
        this.register_fn1("Some", |_ctx: &Context<_>, x: A| Ok(Some(x)));
        this.register_fn_async2("map", |ctx, f: Func<A, B>, x: Option<A>| {
            Box::pin(async move {
                match x {
                    Some(x) => Ok(Some(B(apply(ctx, &f, &x).await?))),
                    None => Ok(None),
                }
            })
        });
        this.register_fn_async2("and_then", |ctx, f: Func<A, Option<B>>, x: Option<A>| {
            Box::pin(async move {
                match x {
                    Some(x) => Ok(Option::<B>::try_decode(&apply(ctx, &f, &x).await?)?),
                    None => Ok(None),
                }
            })
        });
        this.register_fn_async2("or_else", |ctx, f: Func<(), Option<A>>, x: Option<A>| {
            Box::pin(async move {
                match x {
                    Some(x) => Ok(Some(x)),
                    None => {
                        let x = Expr::Tuple(Span::default(), vec![]);
                        let res = apply(ctx, &f, &x).await?;
                        Ok(Option::<A>::try_decode(&res)?)
                    }
                }
            })
        });
        this.register_fn_async2("unwrap_or_else", |ctx, f: Func<(), A>, x: Option<A>| {
            Box::pin(async move {
                match x {
                    Some(x) => Ok(x),
                    None => {
                        let x = Expr::Tuple(Span::default(), vec![]);
                        let res = apply(ctx, &f, &x).await?;
                        Ok(A(res))
                    }
                }
            })
        });
        this.register_fn_async1("unwrap", |_ctx, x: Option<A>| {
            Box::pin(async move {
                match x {
                    Some(x) => Ok(x),
                    None => Err(Error::Custom {
                        error: format!("unwrap called with None"),
                        trace: Default::default(),
                    }),
                }
            })
        });

        // Uuid
        this.register_fn1("string", |_ctx: &Context<_>, x: Uuid| Ok(format!("{}", x)));
        this.register_fn0("random_uuid", |_ctx: &Context<_>| Ok(Uuid::new_v4()));
        this.register_fn1(
            "uuid",
            |_ctx: &Context<_>, x: String| -> Result<Uuid, Error> {
                Ok(Uuid::from_str(&x).map_err(|_| Error::Custom {
                    error: format!("Invalid UUID {:?}", x),
                    trace: Default::default(),
                })?)
            },
        );

        // DateTime
        this.register_fn1("string", |_ctx: &Context<_>, x: DateTime<Utc>| {
            Ok(format!("{}", x))
        });
        this.register_fn0("now", |_ctx: &Context<_>| Ok(Utc::now()));

        // This function highlights a bug in the tracking of Expr types  in the current evaluator.
        // When constructing a nested Expr such as a list, the inner Exprs will not have their
        // types recorded in the type environment, and this will cause a panic to occur when an
        // attempt is made to look up the type of one of these expressions. This function is
        // implemented to directly operate on Exprs (i.e. not using register_fn2) to illustrate
        // how it is currently necessary to manually record the types of all nested Exprs.
        this.register_fn_core_with_name(
            "range",
            Type::build_arrow(
                vec![Arc::new(Type::Uint), Arc::new(Type::Uint)],
                Arc::new(Type::List(Arc::new(Type::Uint))),
            ),
            Box::new(move |_ctx, args| {
                Box::pin(async move {
                    let Some(Expr::Uint(_, start)) = args.get(0) else {
                        return Err(Error::MissingArgument { argument: 0 });
                    };

                    let Some(Expr::Uint(_, end)) = args.get(1) else {
                        return Err(Error::MissingArgument { argument: 1 });
                    };
                    let start = *start;
                    let end = *end;

                    let mut items: Vec<Expr> = Vec::new();
                    for i in start..end {
                        let expr = Expr::Uint(Span::default(), i);
                        items.push(expr);
                    }

                    // We can get by without recording the type of the resulting list, since
                    // this is done at the end of apply()
                    let list_expr = Expr::List(Span::default(), items);
                    Ok(list_expr)
                })
            }),
        )
        .unwrap();

        Ok(this)
    }

    pub fn register_elem_functions(&mut self) {
        // Note: Each of these has a unique name, because we don't currently support overloading
        // of functions containing generic types.
        for tuple_len in 1..=MAX_TUPLE_LEN {
            let mut element_types: Vec<Arc<Type>> = Vec::new();
            for i in 0..tuple_len {
                element_types.push(Arc::new(Type::UnresolvedVar(format!("T{}", i))));
            }
            let tuple_type = Arc::new(Type::Tuple(element_types.clone()));

            for tuple_index in 0..tuple_len {
                let fun_name = format!("elem{}", tuple_index);
                let fun_type = Arc::new(Type::Arrow(
                    tuple_type.clone(),
                    element_types[tuple_index].clone(),
                ));
                let tuple_type = tuple_type.clone();
                self.register_fn_core_with_name(
                    &fun_name,
                    fun_type,
                    Box::new(move |_, args| {
                        let tuple_type = tuple_type.clone();
                        Box::pin(async move {
                            match args.get(0) {
                                Some(Expr::Tuple(_, elems)) if elems.len() == tuple_len => {
                                    Ok(elems[tuple_index].clone())
                                }
                                Some(arg) => Err(Error::ExpectedTypeGotValue {
                                    expected: tuple_type.clone(),
                                    got: arg.clone(),
                                }),
                                _ => Err(Error::MissingArgument { argument: 0 }),
                            }
                        })
                    }),
                )
                .unwrap();
            }
        }
    }

    pub fn register_fn_core_with_name(
        &mut self,
        name: &str,
        t: Arc<Type>,
        f: FtableFn<State>,
    ) -> Result<(), Error> {
        register_fn_core(self, name, t.clone())?;
        self.ftable.add_fn(name, Box::new(t), f)?;
        Ok(())
    }

    pub fn register_adt(
        &mut self,
        adt_type: &Arc<Type>,
        prefix: Option<&str>,
        defaults: Option<&BTreeMap<String, FtableFn<State>>>,
    ) -> Result<(), Error> {
        let Type::ADT(adt) = &**adt_type else {
            panic!("register_adt) called with non-ADT type: {}", adt_type);
        };

        let full_adt_name = make_full_name(prefix, &adt.name);
        if let Some(existing) = self.adts.get(&full_adt_name) {
            if existing == adt {
                // An identical ADT already exists; allow this
                return Ok(());
            } else {
                // A different ADT with the same name is already registered
                return Err(Error::ADTNameConflict {
                    name: full_adt_name.clone(),
                    new: adt.clone(),
                    existing: existing.clone(),
                });
            }
        }
        self.adts.insert(full_adt_name.clone(), adt.clone());

        if adt.variants.len() != 1 && defaults.is_some() {
            panic!(
                "register_adt) called with defaults and multiple variants {}",
                adt_type
            );
        }

        if adt.variants.len() == 0 {
            self.register_adt_variant(adt_type, &adt.name, &None, &full_adt_name, defaults, false)?;
        } else if adt.variants.len() == 1 && adt.variants[0].name == adt.name {
            // Only register accessors if there is exactly one variant
            self.register_adt_variant(
                adt_type,
                &adt.name,
                &adt.variants[0].t,
                &full_adt_name,
                defaults,
                true,
            )?;
        } else {
            for variant in &adt.variants {
                let constructor_name = make_full_name(Some(&full_adt_name), &variant.name);
                self.register_adt_variant(
                    adt_type,
                    &variant.name,
                    &variant.t,
                    &constructor_name,
                    defaults,
                    false,
                )?;
            }
        }

        Ok(())
    }

    pub fn register_adt_variant(
        &mut self,
        adt_type: &Arc<Type>,
        variant_name: &str,
        variant_type: &Option<Arc<Type>>,
        constructor_name: &str,
        defaults: Option<&BTreeMap<String, FtableFn<State>>>,
        accessors: bool,
    ) -> Result<(), Error> {
        let base_name = variant_name.to_string();

        // TODO: Avoid cloning args in the functions. This applies more generally across
        // the evaluation code. We should either pass owned Expr values to the functions
        // or use Rc<Expr> to make cloning cheap.

        // We do not support multiple ADTs with overlapping constructor names. The reason
        // is because in the general case, each constructor may have a different number of
        // arguments, so we cannot consider them to be overloaded functions. Haskell has the
        // same restriction.
        if self.ftable.contains(&constructor_name) {
            panic!("Duplicate constructor name: {}", constructor_name);
        }

        // The functions we register here work directly with Expr values; there is no need
        // to convert to and from the corresponding native Rust type.
        match (variant_type.as_ref().map(|t| &**t), defaults) {
            (Some(Type::Tuple(fields)), _) => {
                let mut fun_type = adt_type.clone();
                for field in fields.iter().rev() {
                    fun_type = Arc::new(Type::Arrow(field.clone(), fun_type));
                }
                self.register_fn_core_with_name(
                    &constructor_name,
                    fun_type,
                    Box::new(move |_, args| {
                        let base_name = base_name.clone();
                        Box::pin(async move {
                            Ok(Expr::Named(
                                Span::default(),
                                base_name,
                                Some(Box::new(Expr::Tuple(Span::default(), args.clone()))),
                            ))
                        })
                    }),
                )
            }
            (Some(Type::Dict(entries)), Some(defaults)) => {
                let mut entries_without_defaults: BTreeMap<String, Arc<Type>> = BTreeMap::new();
                for (k, v) in entries.iter() {
                    if !defaults.contains_key(k) {
                        entries_without_defaults.insert(k.clone(), v.clone());
                    }
                }
                let t = Arc::new(Type::Dict(entries_without_defaults));

                let fun_type = Arc::new(Type::Arrow(t.clone(), adt_type.clone()));
                let defaults: BTreeMap<String, FtableFn<State>> = (*defaults).clone();
                let base_name1 = base_name.clone();
                self.register_fn_core_with_name(
                    &constructor_name,
                    fun_type,
                    Box::new(move |ctx, args| {
                        let base_name = base_name1.clone();
                        let defaults = defaults.clone();
                        Box::pin(async move {
                            let mut val = args[0].clone();

                            if let Expr::Dict(span, entries) = &val {
                                let mut new_entries: BTreeMap<String, Expr> = entries.clone();
                                for (k, vf) in defaults.iter() {
                                    let v = vf(ctx, &vec![]).await?;
                                    new_entries.insert(k.clone(), v);
                                }
                                val = Expr::Dict(*span, new_entries);
                            }

                            Ok(Expr::Named(Span::default(), base_name, Some(Box::new(val))))
                        })
                    }),
                )?;

                if accessors {
                    self.register_accessors(adt_type, entries)?;
                }
                Ok(())
            }
            _ => {
                if let Some(t) = variant_type {
                    let fun_type = Arc::new(Type::Arrow(t.clone(), adt_type.clone()));
                    self.register_fn_core_with_name(
                        &constructor_name,
                        fun_type,
                        Box::new(move |_, args| {
                            let base_name = base_name.clone();
                            Box::pin(async move {
                                let val = args[0].clone();
                                Ok(Expr::Named(Span::default(), base_name, Some(Box::new(val))))
                            })
                        }),
                    )?;

                    if accessors {
                        if let Type::Dict(entries) = &**t {
                            self.register_accessors(adt_type, entries)?;
                        }
                    }
                    Ok(())
                } else {
                    self.register_fn_core_with_name(
                        &constructor_name,
                        adt_type.clone(),
                        Box::new(move |_, _| {
                            let base_name = base_name.clone();
                            Box::pin(
                                async move { Ok(Expr::Named(Span::default(), base_name, None)) },
                            )
                        }),
                    )
                }
            }
        }
    }

    fn register_accessors(
        &mut self,
        adt_type: &Arc<Type>,
        entries: &BTreeMap<String, Arc<Type>>,
    ) -> Result<(), Error> {
        for (entry_key, entry_type) in entries.iter() {
            let this_accessor_fun_type =
                Arc::new(Type::Arrow(adt_type.clone(), entry_type.clone()));

            // Register the type
            match register_fn_core(self, entry_key, this_accessor_fun_type.clone()) {
                Ok(()) => {}
                Err(Error::OverlappingFunctions(_, t1, t2)) if t1 == t2 => {
                    // Ignore this case; it can happen if there are multiple ADTs imported
                    // from different tengu modules that have the same name but different
                    // prefixes
                }
                Err(e) => return Err(e),
            }

            // Register the implementation, if one does not already exist
            if !self.accessors.contains(entry_key) {
                self.accessors.insert(entry_key.to_string());
                self.ftable
                    .add_fn(entry_key, Box::new(Accessor), make_accessor_fn(entry_key))?;
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

    pub fn build(self) -> (Ftable<State>, TypeEnv) {
        let mut ftenv = HashMap::new();
        for (n, b) in self.builtins.into_iter() {
            ftenv.insert(n.clone(), b);
        }
        (self.ftable, ftenv)
    }
}

struct Accessor;

impl fmt::Display for Accessor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        "Accessor".fmt(f)
    }
}

impl Dispatch for Accessor {
    fn num_params(&self) -> usize {
        1
    }

    fn maybe_accepts_args(&self, args: &[Expr]) -> bool {
        args.len() == 1 && matches!(args[0], Expr::Named(..))
    }
}

fn make_full_name(prefix: Option<&str>, name: &str) -> String {
    match prefix {
        Some(prefix) => format!("{}::{}", prefix, name),
        None => name.to_string(),
    }
}

fn make_accessor_fn<State>(entry_key: &str) -> FtableFn<State>
where
    State: Clone + Sync + 'static,
{
    let entry_key = entry_key.to_string();
    Box::new(move |_, args: &Vec<Expr>| {
        let entry_key = entry_key.clone();
        Box::pin(async move {
            match &args[0] {
                Expr::Named(_, _, Some(inner)) => match &**inner {
                    Expr::Dict(_, entries) => match entries.get(&entry_key) {
                        Some(v) => Ok(v.clone()),
                        None => Err(Error::Custom {
                            error: format!("Missing entry {:?}", entry_key),
                            trace: Default::default(),
                        }),
                    },
                    _ => Err(Error::Custom {
                        error: "Expected a dict".to_string(),
                        trace: Default::default(),
                    }),
                },
                _ => Err(Error::Custom {
                    error: "Expected a Named".to_string(),
                    trace: Default::default(),
                }),
            }
        })
    })
}
