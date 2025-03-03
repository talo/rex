use std::rc::Rc;
use crate::types::{Id, Kind, Type, Tyvar, Qual, Pred, ClassEnv, Assump, Scheme, enum_id};
use crate::subst::{Subst, find, concat};
use crate::error::{TypeError};
use crate::{t_char, t_string};
use crate::helpers::*;
use crate::t_arrow;


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
pub struct BindGroup;

pub struct TI {
    pub subst: Subst,
    pub var_id: u64,
}

impl TI {
    pub fn new() -> Self {
        TI {
            subst: Subst(vec![]),
            var_id: 0,
        }
    }

    pub fn new_tvar(&mut self, k: &Rc<Kind>) -> Rc<Type> {
        let v = Rc::new(Tyvar(enum_id(self.var_id), k.clone()));
        self.var_id += 1;
        Rc::new(Type::TVar(v))
    }

    pub fn fresh_inst(&mut self, scheme: &Scheme) -> Qual<Rc<Type>> {
        match scheme {
            Scheme::Forall(ks, qt) => {
                let ts = ks.iter().map(|k| self.new_tvar(k)).collect::<Vec<_>>();
                qt.inst(&ts)
            }
        }
    }

    pub fn unify(&mut self, _t1: &Rc<Type>, _t2: &Rc<Type>) -> Result<(), TypeError> {
        unimplemented!()
    }
}

pub trait Instantiate {
    fn inst(&self, ts: &[Rc<Type>]) -> Self;
}

impl Instantiate for Rc<Type> {
    fn inst(&self, ts: &[Rc<Type>]) -> Self {
        match &**self {
            Type::TAp(l, r) => Rc::new(Type::TAp(l.inst(ts), r.inst(ts))),
            Type::TGen(n) => ts[*n as usize].clone(),
            _ => self.clone(),
        }
    }
}

impl<T> Instantiate for Vec<T> where T : Instantiate {
    fn inst(&self, ts: &[Rc<Type>]) -> Self {
        self.iter().map(|x| x.inst(ts)).collect()
    }
}

impl<T> Instantiate for Qual<T> where T : Instantiate {
    fn inst(&self, ts: &[Rc<Type>]) -> Self {
        let ps = &self.0;
        let t = &self.1;
        Qual(ps.inst(ts), t.inst(ts))
    }
}

impl Instantiate for Pred {
    fn inst(&self, ts: &[Rc<Type>]) -> Self {
        match self {
            Pred::IsIn(c, t) => Pred::IsIn(c.clone(), t.inst(ts)),
        }

    }
}

pub fn ti_lit(
    ti: &mut TI,
    l: &Literal,
) -> Result<(Vec<Pred>, Rc<Type>), TypeError> {
    match l {
        Literal::Char(_) => Ok((vec![], t_char())),
        Literal::Str(_) => Ok((vec![], t_string())),
        Literal::Int(_) => {
            let v = ti.new_tvar(&Rc::new(Kind::Star));
            Ok((vec![isin("Num", &v)], v.clone()))
        }
        Literal::Float(_) => {
            let v = ti.new_tvar(&Rc::new(Kind::Star));
            Ok((vec![isin("Fractional", &v)], v.clone()))
        }
    }
}

pub fn ti_expr(
    ti: &mut TI,
    ce: &ClassEnv,
    as_: &[Assump],
    e: &Rc<Expr>,
) -> Result<(Vec<Pred>, Rc<Type>), TypeError> {
    match &**e {
        Expr::Var(i) => {
            let sc = find(i, as_)?;
            let Qual(ps, t) = ti.fresh_inst(&sc);
            Ok((ps, t))
        }
        Expr::Const(Assump(_, sc)) => {
            let Qual(ps, t) = ti.fresh_inst(&sc);
            Ok((ps, t))
        }
        Expr::Lit(l) => {
            let (ps, t) = ti_lit(ti, l)?;
            Ok((ps, t))
        }
        Expr::Ap(e, f) => {
            let (ps, te) = ti_expr(ti, ce, as_, e)?;
            let (qs, tf) = ti_expr(ti, ce, as_, f)?;
            let t = ti.new_tvar(&Rc::new(Kind::Star));
            let fn_ = tap(tap(t_arrow(), &tf), &t);
            ti.unify(&fn_, &te)?;
            Ok((concat(ps, qs), t))
        }
        Expr::Let(bg, e) => {
            let (ps, as_2) = ti_bindgroup(ti, ce, as_, bg)?;
            let (qs, t) = ti_expr(ti, ce, &concat(as_2, as_.to_vec()), e)?;
            Ok((concat(ps, qs), t))
        }
    }
}

pub fn split(
    _cs: &ClassEnv,
    _fs: &[Rc<Tyvar>], // fixed variables, appearing free in the assumptions
    _gs: &[Rc<Tyvar>], // variables over which we would like to quantify
    _ps: &[Pred],
) -> Result<(Vec<Pred>, Vec<Pred>), TypeError> {
    unimplemented!()
}

pub fn ti_bindgroup(
    _ti: &mut TI,
    _ce: &ClassEnv,
    _as_: &[Assump],
    _bg: &BindGroup
) -> Result<(Vec<Pred>, Vec<Assump>), TypeError> {
    unimplemented!()
}
