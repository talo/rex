use rex_lexer::span::Span;

#[derive(Debug)]
pub enum Expr {
    Var(String, Option<Span>),                          // Variables like x, y
    App(Box<Expr>, Box<Expr>, Option<Span>),            // Function application (f x)
    Lam(String, Box<Expr>, Option<Span>),               // Lambda (\x -> e)
    Let(String, Box<Expr>, Box<Expr>, Option<Span>),    // Let bindings (let x = e1 in e2)
    Ite(Box<Expr>, Box<Expr>, Box<Expr>, Option<Span>), // if e1 then e2 else e3
    Tuple(Vec<Expr>, Option<Span>),                     // Tuple (e1, e2, e3)
    List(Vec<Expr>, Option<Span>),                      // List [e1, e2, e3]
    Dict(Vec<(String, Expr)>, Option<Span>),            // Dictionary {k1 = v1, k2 = v2}
    Bool(bool, Option<Span>),
    Uint(u64, Option<Span>),
    Int(i64, Option<Span>),
    Float(f64, Option<Span>),
    String(String, Option<Span>),
}
