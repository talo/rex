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

    ($lhs:expr, $rhs:expr; ignore span) => {{
        // override the id so we can assert equality without worrying about
        // them, because they are usually randomly generated UUIDs
        let mut lhs = $lhs.clone();
        lhs.reset_ids();
        lhs.reset_spans();

        let mut rhs = $rhs.clone();
        rhs.reset_ids();
        rhs.reset_spans();

        assert_eq!(lhs, rhs);
    }};
}

#[macro_export]
macro_rules! b {
    ($x:expr) => {
        $crate::expr::Expr::Bool(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            $x,
        )
    };

    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Bool($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Bool(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! u {
    ($x:expr) => {
        $crate::expr::Expr::Uint(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            $x,
        )
    };

    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Uint($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Uint(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! i {
    ($x:expr) => {
        $crate::expr::Expr::Int(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            $x,
        )
    };

    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Int($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Int(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! f {
    ($x:expr) => {
        $crate::expr::Expr::Float(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            $x,
        )
    };

    ($span:expr; $x:expr) => {
        $crate::expr::Expr::Float($crate::id::Id::new(), ($span).into(), $x)
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::Float(($id).into(), ($span).into(), $x)
    };
}

#[macro_export]
macro_rules! s {
    ($x:expr) => {
        $crate::expr::Expr::String(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            $x.to_string(),
        )
    };

    ($span:expr; $x:expr) => {
        $crate::expr::Expr::String($crate::id::Id::new(), ($span).into(), ($x).to_string())
    };

    ($id:expr, $span:expr; $x:expr) => {
        $crate::expr::Expr::String(($id).into(), ($span).into(), ($x).to_string())
    };
}

#[macro_export]
macro_rules! tup {
    ($($xs:expr),* $(,)?) => {
        $crate::expr::Expr::Tuple($crate::id::Id::new(), ::rex_lexer::span::Span::default(), vec![$($xs),*])
    };

    ($span:expr; $($xs:expr),* $(,)?) => {
        $crate::expr::Expr::Tuple($crate::id::Id::new(), ($span).into(), vec![$($xs),*])
    };

    ($id:expr, $span:expr; $($xs:expr),* $(,)?) => {
        $crate::expr::Expr::Tuple(($id).into(), ($span).into(), vec![$($xs),*])
    };
}

#[macro_export]
macro_rules! l {
    ($($xs:expr),* $(,)?) => {
        $crate::expr::Expr::List($crate::id::Id::new(), ::rex_lexer::span::Span::default(), vec![$($xs),*])
    };

    ($span:expr; $($xs:expr),* $(,)?) => {
        $crate::expr::Expr::List($crate::id::Id::new(), ($span).into(), vec![$($xs),*])
    };

    ($id:expr, $span:expr; $($xs:expr),* $(,)?) => {
        $crate::expr::Expr::List(($id).into(), ($span).into(), vec![$($xs),*])
    };
}

#[macro_export]
macro_rules! d {
    ($($k:ident = $v:expr),* $(,)?) => {
        $crate::expr::Expr::Dict($crate::id::Id::new(), ::rex_lexer::span::Span::default(), {
            let mut map = ::std::collections::BTreeMap::new();
            $(map.insert(stringify!($k).to_string(), $v);)*
            map
        })
    };

    ($span:expr; $($k:ident = $v:expr),* $(,)?) => {
        $crate::expr::Expr::Dict($crate::id::Id::new(), ($span).into(), {
            let mut map = ::std::collections::BTreeMap::new();
            $(map.insert(stringify!($k).to_string(), $v);)*
            map
        })
    };

    ($id:expr, $span:expr; $($k:ident = $v:expr),* $(,)?) => {
        $crate::expr::Expr::Dict(($id).into(), ($span).into(), {
            let mut map = ::std::collections::BTreeMap::new();
            $(map.insert(stringify!($k).to_string(), $v);)*
            map
        })
    };
}

#[macro_export]
macro_rules! n {
    ($n: expr, $x:expr) => {
        $crate::expr::Expr::Named(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            ($n).to_string(),
            ($x).map(|x| Box::new(x)),
        )
    };
    ($span:expr; $n: expr, $x:expr) => {
        $crate::expr::Expr::Named(
            $crate::id::Id::new(),
            ($span).into(),
            ($n).to_string(),
            ($x).map(|x| Box::new(x)),
        )
    };
    ($id:expr, $span:expr; $n: expr, $x:expr) => {
        $crate::expr::Expr::Named(
            ($id).into(),
            ($span).into(),
            ($n).to_string(),
            ($x).map(|x| Box::new(x)),
        )
    };
}

#[macro_export]
macro_rules! v {
    ($x:expr) => {
        $crate::expr::Expr::Var($crate::expr::Var {
            id: $crate::id::Id::new(),
            span: ::rex_lexer::span::Span::default(),
            name: ($x).to_string(),
        })
    };

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
    ($f:expr, $x:expr) => {
        $crate::expr::Expr::App(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            ($f).into(),
            ($x).into(),
        )
    };

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
    (λ $x:ident -> $e:expr) => {
        $crate::expr::Expr::Lam(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            $crate::expr::Scope::new_sync(),
            ($x).into(),
            ($e).into(),
        )
    };

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
    (let $x:ident = ($e1:expr) in $e2:expr) => {
        $crate::expr::Expr::Let(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            ($x).into(),
            ($e1).into(),
            ($e2).into(),
        )
    };

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
    (if ($e1:expr) { $e2:expr } else { $e3:expr }) => {
        $crate::expr::Expr::Ite(
            $crate::id::Id::new(),
            ::rex_lexer::span::Span::default(),
            ($e1).into(),
            ($e2).into(),
            ($e3).into(),
        )
    };

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
