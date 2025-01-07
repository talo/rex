#[derive(Debug)]
pub enum Expr {
    Var(String),                          // Variables like x, y
    App(Box<Expr>, Box<Expr>),            // Function application (f x)
    Lam(String, Box<Expr>),               // Lambda (\x -> e)
    Let(String, Box<Expr>, Box<Expr>),    // Let bindings (let x = e1 in e2)
    Ite(Box<Expr>, Box<Expr>, Box<Expr>), // if e1 then e2 else e3
    Tuple(Vec<Expr>),                     // Tuple (e1, e2, e3)
    List(Vec<Expr>),                      // List [e1, e2, e3]
}
