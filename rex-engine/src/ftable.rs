use std::{collections::HashMap, future::Future, pin::Pin};

use rex_ast::{expr::Expr, id::Id};
use rex_lexer::span::Span;
use rex_type_system::types::{ToType, Type};

use crate::{
    codec::{Decode, Encode, Func},
    error::Error,
    eval::{eval_app, Context},
};

macro_rules! impl_register_fn {
    ($name:ident, $($param:ident),*) => {
        pub fn $name <$($param,)* B, F>(
            &mut self,
            n: impl ToString,
            f: F,
        ) where
            $($param : Decode + Send + ToType,)*
            B: Encode + ToType,
            F: 'static
                + Clone
                + Send
                + Sync
                + Fn(
                    &Context,
                    $($param,)*
                ) -> Result<B, Error>
        {
            self.0.entry(n.to_string()).or_default().push((
                <fn($($param,)*) -> B as ToType>::to_type(),
                Box::new(move |ctx, args| {
                    let f = f.clone();
                    Box::pin(async move {
                        let mut i: isize = -1;
                        let r = f(ctx, $(decode_arg::<$param>(args, { i += 1; i as usize })?),*)?;
                        r.try_encode(Id(u64::MAX), Span::default())
                    })
                }),
            ))
        }
    };
}

macro_rules! impl_register_fn_async {
    ($name:ident, $($param:ident),*) => {
        pub fn $name <$($param,)* B, F>(
            &mut self,
            n: impl ToString,
            f: F,
        ) where
            $($param : Decode + Send + ToType,)*
            B: Encode + ToType,
            F: 'static
                + Clone
                + Send
                + Sync
                + for<'c> Fn(
                    &'c Context,
                    $($param,)*
                ) -> Pin<Box<dyn Future<Output = Result<B, Error>> + Send + 'c>>
        {
            self.0.entry(n.to_string()).or_default().push((
                <fn($($param,)*) -> B as ToType>::to_type(),
                Box::new(move |ctx, args| {
                    let f = f.clone();
                    Box::pin(async move {
                        let mut i: isize = -1;
                        let r = f(ctx, $(decode_arg::<$param>(args, { i += 1; i as usize })?),*).await?;
                        r.try_encode(Id(u64::MAX), Span::default())
                    })
                }),
            ))
        }
    };
}

macro_rules! define_parametrically_polymorphic_types {
    ($($ty:ident),*) => {
        $(
            pub struct $ty(::rex_ast::expr::Expr);

            impl ToType for $ty {
                fn to_type() -> ::rex_type_system::types::Type {
                    Type::UnresolvedVar(stringify!($ty).to_string())
                }
            }

            impl Encode for $ty {
                fn try_encode(self, _id: Id, _span: Span) -> Result<::rex_ast::expr::Expr, Error> {
                    Ok(self.0)
                }
            }

            impl Decode for $ty {
                fn try_decode(v: &::rex_ast::expr::Expr) -> Result<Self, Error> {
                    Ok(Self(v.clone()))
                }
            }
        )*
    };
}

define_parametrically_polymorphic_types![
    A, A0, A1, A2, A3, B, B0, B1, B2, B3, C, C0, C1, C2, C3, D, D0, D1, D2, D3, E, E0, E1, E2, E3,
    F, F0, F1, F2, F3, G, G0, G1, G2, G3, H, H0, H1, H2, H3, I, I0, I1, I2, I3, J, J0, J1, J2, J3,
    K, K0, K1, K2, K3, L, L0, L1, L2, L3, M, M0, M1, M2, M3, N, N0, N1, N2, N3, O, O0, O1, O2, O3,
    P, P0, P1, P2, P3, Q, Q0, Q1, Q2, Q3, R, R0, R1, R2, R3, S, S0, S1, S2, S3, T, T0, T1, T2, T3,
    U, U0, U1, U2, U3, V, V0, V1, V2, V3, W, W0, W1, W2, W3, X, X0, X1, X2, X3, Y, Y0, Y1, Y2, Y3,
    Z, Z0, Z1, Z2, Z3
];

pub trait Fx<'r>:
    Fn(&'r Context, &'r Vec<Expr>) -> Pin<Box<dyn Future<Output = Result<Expr, Error>> + Send + 'r>>
    + Sync
    + Send
{
    fn clone_box(&self) -> FtableFn;
}

impl Clone for Box<dyn for<'r> Fx<'r>> {
    fn clone(&self) -> Self {
        (**self).clone_box()
    }
}

impl<'r, G> Fx<'r> for G
where
    for<'q> G: Fn(
            &'q Context,
            &'q Vec<Expr>,
        ) -> Pin<Box<dyn Future<Output = Result<Expr, Error>> + Send + 'q>>
        + Sync
        + Send
        + Clone
        + 'q,
{
    fn clone_box(&self) -> FtableFn {
        Box::new((*self).clone())
    }
}

pub type FtableFn = Box<dyn for<'r> Fx<'r>>;

#[derive(Clone)]
pub struct Ftable(pub HashMap<String, Vec<(Type, FtableFn)>>);

impl Ftable {
    pub fn new() -> Self {
        Self(Default::default())
    }

    pub fn with_prelude() -> Self {
        let mut ftable = Self(Default::default());

        ftable.register_fn1("uint", |_ctx: &Context, x: i64| Ok(x as u64));
        ftable.register_fn1("uint", |_ctx: &Context, x: f64| Ok(x as u64));
        ftable.register_fn1("uint", |_ctx: &Context, x: String| {
            x.parse::<u64>().map_err(|e| e.into())
        });

        ftable.register_fn1("int", |_ctx: &Context, x: u64| Ok(x as i64));
        ftable.register_fn1("int", |_ctx: &Context, x: f64| Ok(x as i64));
        ftable.register_fn1("int", |_ctx: &Context, x: String| {
            x.parse::<i64>().map_err(|e| e.into())
        });

        ftable.register_fn1("float", |_ctx: &Context, x: u64| Ok(x as f64));
        ftable.register_fn1("float", |_ctx: &Context, x: i64| Ok(x as f64));
        ftable.register_fn1("float", |_ctx: &Context, x: String| {
            x.parse::<f64>().map_err(|e| e.into())
        });

        ftable.register_fn1("negate", |_ctx: &Context, x: u64| Ok(-(x as i64)));
        ftable.register_fn1("negate", |_ctx: &Context, x: i64| Ok(-x));
        ftable.register_fn1("negate", |_ctx: &Context, x: f64| Ok(-x));

        ftable.register_fn2("+", |_ctx: &Context, x: u64, y: u64| Ok(x + y));
        ftable.register_fn2("+", |_ctx: &Context, x: i64, y: i64| Ok(x + y));
        ftable.register_fn2("+", |_ctx: &Context, x: f64, y: f64| Ok(x + y));

        ftable.register_fn2("-", |_ctx: &Context, x: u64, y: u64| Ok(x - y));
        ftable.register_fn2("-", |_ctx: &Context, x: i64, y: i64| Ok(x - y));
        ftable.register_fn2("-", |_ctx: &Context, x: f64, y: f64| Ok(x - y));

        ftable.register_fn2("*", |_ctx: &Context, x: u64, y: u64| Ok(x * y));
        ftable.register_fn2("*", |_ctx: &Context, x: i64, y: i64| Ok(x * y));
        ftable.register_fn2("*", |_ctx: &Context, x: f64, y: f64| Ok(x * y));

        ftable.register_fn2("/", |_ctx: &Context, x: u64, y: u64| Ok(x / y));
        ftable.register_fn2("/", |_ctx: &Context, x: i64, y: i64| Ok(x / y));
        ftable.register_fn2("/", |_ctx: &Context, x: f64, y: f64| Ok(x / y));

        ftable.register_fn1("abs", |_ctx: &Context, x: i64| Ok(x.abs()));
        ftable.register_fn1("abs", |_ctx: &Context, x: f64| Ok(x.abs()));

        ftable.register_fn1("sqrt", |_ctx: &Context, x: f64| Ok(x.sqrt()));

        ftable.register_fn2("pow", |_ctx: &Context, x: f64, y: i32| Ok(x.powi(y)));
        ftable.register_fn2("pow", |_ctx: &Context, x: f64, y: f64| Ok(x.powf(y)));

        ftable.register_fn1("id", |_ctx: &Context, x: A| Ok(x));

        ftable.register_fn2("take", |_ctx: &Context, n: u64, xs: Vec<A>| {
            Ok(xs.into_iter().take(n as usize).collect::<Vec<_>>())
        });

        ftable.register_fn2("skip", |_ctx: &Context, n: u64, xs: Vec<A>| {
            Ok(xs.into_iter().take(n as usize).collect::<Vec<_>>())
        });

        ftable.register_fn2("zip", |_ctx: &Context, xs: Vec<A>, ys: Vec<B>| {
            Ok(xs.into_iter().zip(ys.into_iter()).collect::<Vec<_>>())
        });

        ftable.register_fn1("unzip", |_ctx: &Context, zs: Vec<(A, B)>| {
            let mut xs = Vec::with_capacity(zs.len());
            let mut ys = Vec::with_capacity(zs.len());
            for (x, y) in zs {
                xs.push(x);
                ys.push(y);
            }
            Ok((xs, ys))
        });

        ftable.register_fn_async2("map", |ctx, f: Func<A, B>, xs: Vec<A>| {
            Box::pin(async move {
                let mut ys: Vec<B> = Vec::with_capacity(xs.len());
                for x in xs {
                    let y = eval_app(
                        ctx,
                        f.expr.id().clone(),
                        f.expr.span().clone(),
                        f.expr.clone(),
                        x.0,
                    )
                    .await?;
                    ys.push(B(y));
                }
                Ok(ys)
            })
        });

        ftable.register_fn_async3("compose", |ctx, f: Func<B, C>, g: Func<A, B>, x: A| {
            Box::pin(async move {
                let x = eval_app(
                    ctx,
                    g.expr.id().clone(),
                    g.expr.span().clone(),
                    g.expr.clone(),
                    x.0,
                )
                .await?;

                let x = eval_app(
                    ctx,
                    f.expr.id().clone(),
                    f.expr.span().clone(),
                    f.expr.clone(),
                    x,
                )
                .await?;

                Ok(C(x))
            })
        });

        ftable
    }

    impl_register_fn!(register_fn1, A0);
    impl_register_fn!(register_fn2, A0, A1);
    impl_register_fn!(register_fn3, A0, A1, A2);
    impl_register_fn!(register_fn4, A0, A1, A2);

    impl_register_fn_async!(register_fn_async1, A0);
    impl_register_fn_async!(register_fn_async2, A0, A1);
    impl_register_fn_async!(register_fn_async3, A0, A1, A2);
    impl_register_fn_async!(register_fn_async4, A0, A1, A2);

    // NOTE(loong): We do not support overloaded parametric polymorphism.
    pub fn lookup_fns(&self, n: &str, t: Type) -> impl Iterator<Item = &FtableFn> {
        self.0
            .get(n)
            .map(|v| v.iter())
            .into_iter()
            .flatten()
            .filter_map(move |(ftype, f)| match ftype.maybe_compatible(&t) {
                Ok(()) => Some(f),
                Err(_e) => None,
            })
    }
}

pub fn decode_arg<A>(args: &Vec<Expr>, i: usize) -> Result<A, Error>
where
    A: Decode,
{
    args.get(i)
        .ok_or(Error::MissingArgument { argument: i })
        .and_then(|a0| A::try_decode(a0))
}
