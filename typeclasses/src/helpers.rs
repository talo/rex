use std::rc::Rc;
use std::borrow::Borrow;
use crate::types::{Id, Kind, Type, Tyvar, Tycon, Pred};

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
    Pred::IsIn(Id(name.into()), t.borrow().clone())
}
