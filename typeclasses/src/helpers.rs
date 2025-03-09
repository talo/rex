use std::rc::Rc;
use std::borrow::Borrow;
use crate::extras::{Id, Pred};
use crate::types::{Type, Tyvar, Tycon};
use crate::kind::Kind;
use crate::expr::{Literal, Expr, BindGroup};

pub fn id(s: impl Into<String>) -> Id {
    Id(s.into())
}

pub fn star() -> Rc<Kind> {
    Rc::new(Kind::Star)
}

pub fn kfun(l: Rc<Kind>, r: Rc<Kind>) -> Rc<Kind> {
    Rc::new(Kind::Kfun(l, r))
}

pub fn tyvar(name: &str, kind: Rc<Kind>) -> Rc<Tyvar> {
    Rc::new(Tyvar(Id(name.to_string()), kind))
}

pub fn tycon(name: &str, kind: Rc<Kind>) -> Rc<Tycon> {
    Rc::new(Tycon(Id(name.to_string()), kind))
}

pub fn tvar(v: impl Borrow<Rc<Tyvar>>) -> Rc<Type> {
    Rc::new(Type::TVar(v.borrow().clone()))
}

pub fn tcon(c: impl Borrow<Rc<Tycon>>) -> Rc<Type> {
    Rc::new(Type::TCon(c.borrow().clone()))
}

pub fn tap(l: impl Borrow<Rc<Type>>, r: impl Borrow<Rc<Type>>) -> Rc<Type> {
    Rc::new(Type::TAp(l.borrow().clone(), r.borrow().clone()))
}

pub fn tgen(i: u64) -> Rc<Type> {
    Rc::new(Type::TGen(i))
}

pub fn mapsto(v: impl Borrow<Rc<Tyvar>>, t: impl Borrow<Rc<Type>>) -> (Rc<Tyvar>, Rc<Type>) {
    (v.borrow().clone(), t.borrow().clone())
}

pub fn isin(name: impl Into<String>, t: impl Borrow<Rc<Type>>) -> Pred {
    Pred(Id(name.into()), t.borrow().clone())
}

pub fn evar(name: impl Into<String>) -> Rc<Expr> {
    Rc::new(Expr::Var(Id(name.into())))
}

pub fn eint(value: i64) -> Rc<Expr> {
    Rc::new(Expr::Lit(Literal::Int(value)))
}

pub fn efloat(value: f64) -> Rc<Expr> {
    Rc::new(Expr::Lit(Literal::Float(value)))
}

pub fn echar(value: char) -> Rc<Expr> {
    Rc::new(Expr::Lit(Literal::Char(value)))
}

pub fn estring(value: impl Into<String>) -> Rc<Expr> {
    Rc::new(Expr::Lit(Literal::Str(value.into())))
}

pub fn eap(l: impl Borrow<Rc<Expr>>, r: impl Borrow<Rc<Expr>>) -> Rc<Expr> {
    Rc::new(Expr::Ap(l.borrow().clone(), r.borrow().clone()))
}

pub fn elet(g: BindGroup, e: impl Borrow<Rc<Expr>>) -> Rc<Expr> {
    Rc::new(Expr::Let(g, e.borrow().clone()))
}
