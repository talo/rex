use std::rc::Rc;
use crate::extras::{Id, Assump, Scheme};
use crate::debug::Output;

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
    Str(String),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Var(Id),
    Lit(Literal),
    Const(Assump),
    Ap(Rc<Expr>, Rc<Expr>),
    Let(BindGroup, Rc<Expr>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct BindGroup(pub Expl, pub Impl);

#[derive(PartialEq, Clone, Debug)]
pub struct Expl(pub Id, pub Scheme, pub Alt);

#[derive(PartialEq, Clone, Debug)]
pub struct Impl(pub Id, pub Alt);

#[derive(PartialEq, Clone, Debug)]
pub struct Alt(pub Vec<Id>, pub Rc<Expr>);

pub fn dump_expr(expr: &Rc<Expr>, output: &mut Output, prefix: &str, indent: &str) {
    match &**expr {
        Expr::Var(_) => {
            output.add_line(format!("{}Var", prefix), "");
        }
        Expr::Lit(_) => {
            output.add_line(format!("{}Lit", prefix), "");
        }
        Expr::Const(_) => {
            output.add_line(format!("{}Const", prefix), "");
        }
        Expr::Ap(l, r) => {
            output.add_line(format!("{}Ap", prefix), "");
            dump_expr(l, output, &format!("{}├── ", indent), &format!("{}├── ", indent));
            dump_expr(r, output, &format!("{}└── ", indent), &format!("{}    ", indent));
        }
        Expr::Let(g, e) => {
            output.add_line(format!("{}Let", prefix), "");
            dump_bind_group(g, output, &format!("{}├── ", indent), &format!("{}├── ", indent));
            dump_expr(e, output, &format!("{}└── ", indent), &format!("{}    ", indent));
        }
    }
}

pub fn dump_bind_group(_group: &BindGroup, _output: &mut Output, _prefix: &str, _indent: &str) {
    unimplemented!()
}
