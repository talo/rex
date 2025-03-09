use std::rc::Rc;
use std::borrow::Borrow;
use crate::classes::ClassEnv;
use crate::extras::{Qual, Pred, Assump, Scheme, enum_id, Instantiate};
use crate::types::{Type, Tyvar};
use crate::kind::Kind;
use crate::subst::{Subst, find, concat, mgu, Types};
use crate::error::{TypeError};
use crate::{t_char, t_string};
use crate::helpers::*;
use crate::t_arrow;
use crate::expr::{Literal, Expr, BindGroup};

pub struct Program(pub Vec<BindGroup>);

pub struct TI {
    pub subst: Subst,
    pub var_id: u64,
    pub expr_types: Vec<(Rc<Expr>, Rc<Pred>, Rc<Type>)>,
    pub trace_enabled: bool,
    pub trace_depth: usize,
}

impl TI {
    pub fn new() -> Self {
        TI {
            subst: Subst(vec![]),
            var_id: 0,
            expr_types: vec![],
            trace_enabled: false,
            trace_depth: 0,
        }
    }

    pub fn new_tvar(&mut self, k: &Rc<Kind>) -> Rc<Type> {
        let v = Rc::new(Tyvar(enum_id(self.var_id), k.clone()));
        self.var_id += 1;
        Rc::new(Type::TVar(v))
    }

    pub fn fresh_inst(&mut self, scheme: &Scheme) -> Qual<Rc<Type>> {
        let ts = scheme.kinds().iter().map(|k| self.new_tvar(k)).collect::<Vec<_>>();
        scheme.qualified_type().inst(&ts)
    }

    pub fn unify(&mut self, t1: &Rc<Type>, t2: &Rc<Type>) -> Result<(), TypeError> {
        // println!("unify {} and {}", t1, t2);
        let u = mgu(&t1.apply(&self.subst), &t2.apply(&self.subst))?;
        self.ext_subst(u);
        Ok(())
    }

    pub fn ext_subst(&mut self, s: impl Borrow<Subst>) {
        self.subst = s.borrow().atat(&self.subst)
    }

    pub fn get_expr_type(&self, expr: &Rc<Expr>) -> Option<(Rc<Pred>, Rc<Type>)> {
        for et in self.expr_types.iter() {
            if Rc::ptr_eq(expr, &et.0) {
                return Some((et.1.clone(), et.2.clone()));
            }
        }
        None
    }

    pub fn trace(&mut self, msg: impl Into<String>) {
        if self.trace_enabled {
            println!("{:<indent$}{}", " ", msg.into(), indent=4 * self.trace_depth);
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
    ti.trace_depth += 1;
    ti.trace(format!(">expr {:?}", e));
    let res = ti_expr1(ti, ce, as_, e);
    match &res {
        Ok((ps, t)) => {
            ti.trace(format!("<expr {:?} => {:?} {}", e, ps, t));
        }
        Err(err) => {
            ti.trace(format!("<expr {:?} => {}", e, err));
        }
    }
    ti.trace_depth -= 1;
    res
}

pub fn ti_expr1(
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

pub fn ti_program(
    _ce: &ClassEnv,
    _as_: &[Assump],
    _p: &Program,
) -> Vec<Assump> {
    // tiProgram ce as bgs = runTI $
    //                       do (ps, as') <- tiSeq tiBindGroup ce as bgs
    //                          s         <- getSubst
    //                          rs        <- reduce ce (apply s ps)
    //                          s'        <- defaultSubst ce [] rs
    //                          return (apply (s'@@s) as')
    unimplemented!()
}
