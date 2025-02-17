use std::{collections::HashMap, future::Future, pin::Pin};

use rex_ast::{expr::Expr, id::Id};
use rex_lexer::span::Span;
use rex_type_system::types::{ToType, Type};

use crate::{
    codec::{Decode, Encode},
    error::Error,
    eval::Context,
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
            F: Fn(
                &Context<State>,
                $($param,)*
            ) -> Result<B, Error>
                + Clone
                + Send
                + Sync
                + 'static
        {
            self.0.entry(n.to_string()).or_default().push((
                <fn($($param,)*) -> B as ToType>::to_type(),
                Box::new(move |ctx, args| {
                    let f = f.clone();
                    Box::pin(async move {
                        let mut i: isize = -1;
                        let r = f(ctx, $(decode_arg::<$param>(args, { i += 1; i as usize })?),*)?;

                        // FIXME(loong): it is absolutely critical to assign a
                        // proper ID here. We need unique IDs for created
                        // expressions, because we need to associate a type with
                        // them in the expression-type environment.
                        r.try_encode(Id(rand::random()), Span::default())
                    })
                }),
            ))
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
            self.0.entry(n.to_string()).or_default().push((
                <fn($($param,)*) -> B as ToType>::to_type(),
                Box::new(move |ctx, args| {
                    let f = f.clone();
                    Box::pin(async move {
                        let mut i = 0;
                        let r = f(ctx, $(decode_arg::<$param>(args, { let j = i; i += 1; j })?),*).await?;

                        // FIXME(loong): it is absolutely critical to assign a
                        // proper ID here. We need unique IDs for created
                        // expressions, because we need to associate a type with
                        // them in the expression-type environment.
                        r.try_encode(Id(rand::random()), Span::default())
                    })
                }),
            ))
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

impl<'r, Gx, State> Fx<'r, State> for Gx
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

#[derive(Clone)]
pub struct Ftable<State>(pub HashMap<String, Vec<(Type, FtableFn<State>)>>)
where
    State: Clone + Sync + 'static;

impl<State> Ftable<State>
where
    State: Clone + Sync + 'static,
{
    pub fn new() -> Self {
        Self(Default::default())
    }

    // NOTE(loong): We do not support overloaded parametric polymorphism.
    pub fn lookup_fns(&self, n: &str, t: Type) -> impl Iterator<Item = (&FtableFn<State>, &Type)> {
        self.0
            .get(n)
            .map(|v| v.iter())
            .into_iter()
            .flatten()
            .filter_map(move |(ftype, f)| match ftype.maybe_compatible(&t) {
                Ok(()) => Some((f, ftype)),
                Err(_e) => None,
            })
            .collect::<Vec<_>>()
            .into_iter()
    }

    impl_register_fn!(register_fn1, A0);
    impl_register_fn!(register_fn2, A0, A1);
    impl_register_fn!(register_fn3, A0, A1, A2);
    impl_register_fn!(register_fn4, A0, A1, A2);

    impl_register_fn_async!(register_fn_async1, A0);
    impl_register_fn_async!(register_fn_async2, A0, A1);
    impl_register_fn_async!(register_fn_async3, A0, A1, A2);
    impl_register_fn_async!(register_fn_async4, A0, A1, A2);
}

pub fn decode_arg<A>(args: &Vec<Expr>, i: usize) -> Result<A, Error>
where
    A: Decode,
{
    args.get(i)
        .ok_or(Error::MissingArgument { argument: i })
        .and_then(|a0| A::try_decode(a0))
}

macro_rules! define_polymorphic_types {
    ($($ty:ident),*) => {
        $(
            pub struct $ty(pub ::rex_ast::expr::Expr);

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

define_polymorphic_types![
    A, A0, A1, A2, A3, B, B0, B1, B2, B3, C, C0, C1, C2, C3, D, D0, D1, D2, D3, E, E0, E1, E2, E3,
    F, F0, F1, F2, F3, G, G0, G1, G2, G3, H, H0, H1, H2, H3, I, I0, I1, I2, I3, J, J0, J1, J2, J3,
    K, K0, K1, K2, K3, L, L0, L1, L2, L3, M, M0, M1, M2, M3, N, N0, N1, N2, N3, O, O0, O1, O2, O3,
    P, P0, P1, P2, P3, Q, Q0, Q1, Q2, Q3, R, R0, R1, R2, R3, S, S0, S1, S2, S3, T, T0, T1, T2, T3,
    U, U0, U1, U2, U3, V, V0, V1, V2, V3, W, W0, W1, W2, W3, X, X0, X1, X2, X3, Y, Y0, Y1, Y2, Y3,
    Z, Z0, Z1, Z2, Z3
];
