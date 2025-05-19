use std::{collections::HashMap, fmt, future::Future, pin::Pin, sync::Arc};

use rex_ast::expr::Expr;
use rex_lexer::span::Span;
use rex_type_system::types::{Dispatch, ToType};

use crate::{
    codec::{Decode, Encode},
    error::Error,
    eval::Context,
};

macro_rules! impl_register_fn {
    ($name:ident $(, $($param:ident),*)?) => {
        #[allow(unused_assignments)] // This is a workaround for the unused_assignments lint for the last `i += 1` in the macro expansion.
        #[allow(unused_mut)] // This is a workaround for the unused_assignments lint for `let mut i = 0` not being needed for functions with zero parameters.
        #[allow(unused_variables)] // This is a workaround for the unused_assignments lint for `args` and `i` not being needed for functions with zero parameters.
        pub fn $name <$($($param,)*)? B, F>(
            &mut self,
            n: impl ToString,
            f: F,
        ) where
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
            let t = Arc::new(<fn($($($param,)*)?) -> B as ToType>::to_type());
            let t_num_params = t.num_params();

            self.add_fn(
                n,
                Box::new(t),
                Box::new(move |ctx, args| {
                    let f = f.clone();
                    Box::pin(async move {
                        let mut i = 0;
                        let mut r = f(ctx $(, $(decode_arg::<$param>(args, { let j = i; i += 1; j })?),*)?)?
                            .try_encode(Span::default())?; // FIXME(loong): assign a proper span
                        while i < args.len() {
                            r = $crate::eval::apply(ctx, r, &args[{ let j = i; i += 1; j }], None).await?;
                        }
                        Ok(r)
                    })
                }),
            ).unwrap()
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
        ) where
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
            self.add_fn(
                n,
                Box::new(Arc::new(<fn($($param,)*) -> B as ToType>::to_type())),
                Box::new(move |ctx, args| {
                    let f = f.clone();
                    Box::pin(async move {
                        let mut i = 0;
                        let mut r = f(ctx, $(decode_arg::<$param>(args, { let j = i; i += 1; j })?),*)
                            .await?
                            .try_encode(Span::default())?; // FIXME(loong): assign a proper span
                        while i < args.len() {
                            r = $crate::eval::apply(ctx, r, &args[{ let j = i; i += 1; j }], None).await?;
                        }
                        Ok(r)
                    })
                }),
            ).unwrap()
        }
    };
}

pub type FtableFn<State> = Box<dyn for<'r> Fx<'r, State>>;

pub trait Fx<'r, State>:
    Fn(
        &'r Context<State>,
        &'r Vec<Expr>,
    ) -> Pin<Box<dyn Future<Output = Result<Expr, Error>> + Send + 'r>>
    + Send
    + Sync
where
    State: Clone + Sync + 'static,
{
    fn clone_box(&self) -> FtableFn<State>;
}

impl<State> Clone for Box<dyn for<'r> Fx<'r, State>>
where
    State: Clone + Sync + 'static,
{
    fn clone(&self) -> Self {
        (**self).clone_box()
    }
}

impl<Gx, State> Fx<'_, State> for Gx
where
    for<'q> Gx: Fn(
            &'q Context<State>,
            &'q Vec<Expr>,
        ) -> Pin<Box<dyn Future<Output = Result<Expr, Error>> + Send + 'q>>
        + Clone
        + Send
        + Sync
        + 'q,
    State: Clone + Sync + 'static,
{
    fn clone_box(&self) -> FtableFn<State> {
        Box::new((*self).clone())
    }
}

pub struct Entry<State>
where
    State: Clone + Sync + 'static,
{
    pub num_params: usize,
    pub items: Vec<(Box<dyn Dispatch + Send + Sync>, FtableFn<State>)>,
}

pub struct Ftable<State>(pub HashMap<String, Entry<State>>)
where
    State: Clone + Sync + 'static;

impl<State> Default for Ftable<State>
where
    State: Clone + Send + Sync + 'static,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<State> Ftable<State>
where
    State: Clone + Send + Sync + 'static,
{
    pub fn new() -> Self {
        Default::default()
    }

    pub fn contains(&self, n: &str) -> bool {
        self.0.contains_key(n)
    }

    pub fn add_fn(
        &mut self,
        n: impl ToString,
        t: Box<dyn Dispatch + Send + Sync>,
        f: FtableFn<State>,
    ) -> Result<(), Error> {
        let num_params = t.num_params();
        let n = n.to_string();
        match self.0.get_mut(&n) {
            Some(entry) => {
                if num_params != entry.num_params {
                    return Err(Error::OverloadParamCountMismatch {
                        name: n,
                        new: num_params,
                        existing: entry.num_params,
                        trace: Default::default(),
                    });
                }
                entry.items.push((t, f));
            }
            None => {
                self.0.insert(
                    n,
                    Entry {
                        num_params,
                        items: vec![(t, f)],
                    },
                );
            }
        }
        Ok(())
    }

    impl_register_fn!(register_fn0);
    impl_register_fn!(register_fn1, A0);
    impl_register_fn!(register_fn2, A0, A1);
    impl_register_fn!(register_fn3, A0, A1, A2);
    impl_register_fn!(register_fn4, A0, A1, A2);

    impl_register_fn_async!(register_fn_async1, A0);
    impl_register_fn_async!(register_fn_async2, A0, A1);
    impl_register_fn_async!(register_fn_async3, A0, A1, A2);
    impl_register_fn_async!(register_fn_async4, A0, A1, A2);
}

impl<State> fmt::Display for Ftable<State>
where
    State: Clone + Sync + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Ftable")?;
        let mut items = self.0.iter().collect::<Vec<_>>();
        items.sort_by(|(n0, _), (n1, _)| n0.cmp(n1));

        for (name, entries) in items.iter() {
            if entries.items.len() == 1 {
                write!(f, "\n    {} :: {}", name, entries.items[0].0)?;
            } else {
                write!(f, "\n    {} ::", name)?;
                for entry in entries.items.iter() {
                    write!(f, "\n        {}", entry.0)?;
                }
            }
        }
        Ok(())
    }
}

pub fn decode_arg<A>(args: &[Expr], i: usize) -> Result<A, Error>
where
    A: Decode,
{
    args.get(i)
        .ok_or(Error::MissingArgument {
            argument: i,
            trace: Default::default(),
        })
        .and_then(|a0| A::try_decode(a0))
}

macro_rules! define_polymorphic_types {
    ($($ty:ident),*) => {
        $(
            #[derive(Clone, Debug)]
            pub struct $ty(pub ::rex_ast::expr::Expr);

            #[allow(clippy::from_over_into)]
            impl Into<::rex_ast::expr::Expr> for $ty {
                fn into(self) -> ::rex_ast::expr::Expr {
                    self.0
                }
            }

            impl From<::rex_ast::expr::Expr> for $ty {
                fn from(e: ::rex_ast::expr::Expr) -> Self {
                    Self(e)
                }
            }

            impl ::std::borrow::Borrow<::rex_ast::expr::Expr> for $ty {
                fn borrow(&self) -> &::rex_ast::expr::Expr {
                    &self.0
                }
            }

            impl ::std::borrow::Borrow<::rex_ast::expr::Expr> for &$ty {
                fn borrow(&self) -> &::rex_ast::expr::Expr {
                    &self.0
                }
            }

            impl ::std::borrow::Borrow<::rex_ast::expr::Expr> for &mut $ty {
                fn borrow(&self) -> &::rex_ast::expr::Expr {
                    &self.0
                }
            }

            impl ::std::borrow::BorrowMut<::rex_ast::expr::Expr> for $ty {
                fn borrow_mut(&mut self) -> &mut ::rex_ast::expr::Expr {
                    &mut self.0
                }
            }

            impl ::std::borrow::BorrowMut<::rex_ast::expr::Expr> for &mut $ty {
                fn borrow_mut(&mut self) -> &mut ::rex_ast::expr::Expr {
                    &mut self.0
                }
            }

            impl AsRef<::rex_ast::expr::Expr> for $ty {
                fn as_ref(&self) -> &::rex_ast::expr::Expr {
                    &self.0
                }
            }

            impl AsMut<::rex_ast::expr::Expr> for $ty {
                fn as_mut(&mut self) -> &mut ::rex_ast::expr::Expr {
                    &mut self.0
                }
            }

            impl ToType for $ty {
                fn to_type() -> ::rex_type_system::types::Type {
                    ::rex_type_system::types::Type::UnresolvedVar(stringify!($ty).to_string())
                }
            }

            impl Encode for $ty {
                fn try_encode(self, _span: Span) -> Result<::rex_ast::expr::Expr, Error> {
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

define_polymorphic_types![
    A, A0, A1, A2, A3, B, B0, B1, B2, B3, C, C0, C1, C2, C3, D, D0, D1, D2, D3, E, E0, E1, E2, E3,
    F, F0, F1, F2, F3, G, G0, G1, G2, G3, H, H0, H1, H2, H3, I, I0, I1, I2, I3, J, J0, J1, J2, J3,
    K, K0, K1, K2, K3, L, L0, L1, L2, L3, M, M0, M1, M2, M3, N, N0, N1, N2, N3, O, O0, O1, O2, O3,
    P, P0, P1, P2, P3, Q, Q0, Q1, Q2, Q3, R, R0, R1, R2, R3, S, S0, S1, S2, S3, T, T0, T1, T2, T3,
    U, U0, U1, U2, U3, V, V0, V1, V2, V3, W, W0, W1, W2, W3, X, X0, X1, X2, X3, Y, Y0, Y1, Y2, Y3,
    Z, Z0, Z1, Z2, Z3
];
