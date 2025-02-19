#[macro_export]
macro_rules! assert_expr_eq {
    ($lhs:expr, $rhs:expr) => {{
        // override the id so we can assert equality without worrying about
        // them, because they are usually randomly generated UUIDs
        let mut lhs = $lhs.clone();
        lhs.reset_ids();

        let mut rhs = $rhs.clone();
        rhs.reset_ids();

        assert_eq!(lhs, rhs);
    }};

    ($lhs:expr, $rhs:expr; with id) => {{
        // override nothing
        assert_eq!($lhs, $rhs);
    }};
}

#[macro_export]
macro_rules! b {
    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Bool($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Bool(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! u {
    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Uint($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Uint(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! i {
    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Int($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Int(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! f {
    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Float($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Float(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! s {
    ($span:expr; $x:expr) => {
        $crate::expr::Expr::String($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::String(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! tup {
    ($span:expr; $($xs:expr),* $(,)?) => {
        $crate::expr::Expr::Tuple($crate::id::Id::new(), ($span).into(), vec![$($xs),*])
    };

    ($id:expr, $span:expr; $($xs:expr),* $(,)?) => {
        $crate::expr::Expr::Tuple(($id).into(), ($span).into(), vec![$($xs),*])
    };
}

#[macro_export]
macro_rules! l {
    ($span:expr; $($xs:expr),* $(,)?) => {
        $crate::expr::Expr::List($crate::id::Id::new(), ($span).into(), vec![$($xs),*])
    };

    ($id:expr, $span:expr; $($xs:expr),* $(,)?) => {
        $crate::expr::Expr::List(($id).into(), ($span).into(), vec![$($xs),*])
    };
}

#[macro_export]
macro_rules! d {
    ($span:expr; $($k:expr => $v:expr),* $(,)?) => {
        $crate::expr::Expr::Dict($crate::id::Id::new(), ($span).into(), {
            let mut map = ::std::collections::BTreeMap::new();
            $(map.insert($k.to_string(), $v);)*
            map
        })
    };

    ($id:expr, $span:expr; $($k:expr => $v:expr),* $(,)?) => {
        $crate::expr::Expr::Dict(($id).into(), ($span).into(), {
            let mut map = ::std::collections::BTreeMap::new();
            $(map.insert($k.to_string(), $v);)*
            map
        })
    };
}

#[macro_export]
macro_rules! v {
    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Var($crate::expr::Var {
            id: $crate::id::Id::new(),
            span: ($span).into(),
            name: ($x).to_string(),
        })
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Var($crate::expr::Var {
            id: ($id).into(),
            span: ($span).into(),
            name: ($x).to_string(),
        })
    };
}

#[macro_export]
macro_rules! app {
    ($span:expr; $f:expr, $x:expr) => {
        $crate::expr::Expr::App(
            $crate::id::Id::new(),
            ($span).into(),
            ($f).into(),
            ($x).into(),
        )
    };

    ($id:expr, $span:expr; $f:expr, $x:expr) => {
        $crate::expr::Expr::App(($id).into(), ($span).into(), ($f).into(), ($x).into())
    };
}

#[macro_export]
macro_rules! lam {
    ($span:expr; λ $x:ident -> $e:expr) => {
        $crate::expr::Expr::Lam(
            $crate::id::Id::new(),
            ($span).into(),
            $crate::expr::Scope::new_sync(),
            ($x).into(),
            ($e).into(),
        )
    };

    ($id:expr, $span:expr; λ $x:ident -> $e:expr) => {
        $crate::expr::Expr::Lam(
            ($id).into(),
            ($span).into(),
            $crate::expr::Scope::new_sync(),
            ($x).into(),
            ($e).into(),
        )
    };
}

#[macro_export]
macro_rules! let_in {
    ($span:expr; let $x:ident = ($e1:expr) in $e2:expr) => {
        $crate::expr::Expr::Let(
            $crate::id::Id::new(),
            ($span).into(),
            ($x).into(),
            ($e1).into(),
            ($e2).into(),
        )
    };

    ($id:expr, $span:expr; let $x:ident = ($e1:expr) in $e2:expr) => {
        $crate::expr::Expr::Let(
            ($id).into(),
            ($span).into(),
            ($x).into(),
            ($e1).into(),
            ($e2).into(),
        )
    };
}

#[macro_export]
macro_rules! ite {
    ($span:expr; if ($e1:expr) { $e2:expr } else { $e3:expr }) => {
        $crate::expr::Expr::Ite(
            $crate::id::Id::new(),
            ($span).into(),
            ($e1).into(),
            ($e2).into(),
            ($e3).into(),
        )
    };

    ($id:expr, $span:expr; if ($e1:expr) { $e2:expr } else { $e3:expr }) => {
        $crate::expr::Expr::Ite(
            ($id).into(),
            ($span).into(),
            ($e1).into(),
            ($e2).into(),
            ($e3).into(),
        )
    };
}
