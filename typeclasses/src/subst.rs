use std::rc::Rc;
use std::fmt;

use crate::types::*;
use crate::error::TypeError;

#[derive(PartialEq, Eq, Debug)]
pub struct Subst (pub Vec<(Rc<Tyvar>, Rc<Type>)>);

impl Subst {
    pub fn atat(&self, s2: &Subst) -> Subst {
        let s1 = self;
        Subst([
            s2.0.iter().map(|(u, t)| (u.clone(), t.apply(s1))).collect(),
            s1.0.clone(),
        ].concat())
    }

    pub fn merge(&self, s2: &Subst) -> Result<Subst, TypeError> {
        let s1 = self;
        let s1_vars = s1.0.iter().map(|(u, _)| u.clone()).collect::<Vec<_>>();
        let s2_vars = s2.0.iter().map(|(u, _)| u.clone()).collect::<Vec<_>>();
        let vars = intersect_(&s1_vars, &s2_vars);
        for v in vars {
            let tv = Rc::new(Type::TVar(v.clone()));
            if tv.apply(s1) != tv.apply(s2) {
                return Err(TypeError::MergeFails);
            }
        }
        Ok(Subst([s1.0.clone(), s2.0.clone()].concat()))
    }
}

impl fmt::Display for Subst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "[")?;
        for (i, (v, t)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            write!(f, "({},{})", v, t)?;
        }
        write!(f, "]")
    }
}

pub fn null_subst() -> Subst {
    Subst(vec![])
}

pub fn unary_subst(v: &Rc<Tyvar>, t: &Rc<Type>) -> Subst {
    Subst(vec![(v.clone(), t.clone())])
}

pub trait Types {
    fn apply(&self, s: &Subst) -> Self;
    fn tv(&self) -> Vec<Rc<Tyvar>>;
}

impl Types for Rc<Type> {
    fn apply(&self, s: &Subst) -> Self {
        match &**self {
            Type::TVar(u) => {
                match lookup(u, s) {
                    Some(t) => t.clone(),
                    None => self.clone(),
                }
            }
            Type::TAp(l, r) => Rc::new(Type::TAp(l.apply(s), r.apply(s))),
            _ => self.clone()
        }
    }

    fn tv(&self) -> Vec<Rc<Tyvar>> {
        match &**self {
            Type::TVar(u) => vec![u.clone()],
            Type::TAp(l, r) => union_(l.tv(), r.tv()),
            _ => vec![],
        }
    }
}

impl<T> Types for Vec<T> where T : Types {
    fn apply(&self, s: &Subst) -> Self {
        self.iter().map(|x| x.apply(s)).collect()
    }

    fn tv(&self) -> Vec<Rc<Tyvar>> {
        let mut result: Vec<Rc<Tyvar>> = Vec::new();
        for item in self.iter() {
            let mut others = item.tv();
            result.append(&mut others);
        }
        dedup_unsorted(&mut result);
        result
    }
}

impl<T> Types for Qual<T> where T : Types {
    fn apply(&self, s: &Subst) -> Self {
        let ps = &self.0;
        let t = &self.1;
        Qual(ps.apply(s), t.apply(s))
    }

    fn tv(&self) -> Vec<Rc<Tyvar>> {
        let ps = &self.0;
        let t = &self.1;
        union_(ps.tv(), t.tv())
    }
}

impl Types for Pred {
    fn apply(&self, s: &Subst) -> Self {
        match self {
            Pred::IsIn(i, t) => Pred::IsIn(i.clone(), t.apply(s)),
        }
    }

    fn tv(&self) -> Vec<Rc<Tyvar>> {
        match self {
            Pred::IsIn(_, t) => t.tv(),
        }
    }
}

impl Types for Scheme {
    fn apply(&self, s: &Subst) -> Self {
        match self {
            Scheme::Forall(ks, qt) => Scheme::Forall(ks.clone(), qt.apply(s)),
        }
    }

    fn tv(&self) -> Vec<Rc<Tyvar>> {
        match self {
            Scheme::Forall(_, qt) => qt.tv(),
        }
    }
}

impl Types for Assump {
    fn apply(&self, s: &Subst) -> Self {
        Assump(self.0.clone(), Rc::new(self.1.apply(s)))
    }

    fn tv(&self) -> Vec<Rc<Tyvar>> {
        self.1.tv()
    }
}

fn lookup(u: &Rc<Tyvar>, s: &Subst) -> Option<Rc<Type>> {
    for (v, t) in s.0.iter() {
        if v == u {
            return Some(t.clone())
        }
    }
    None
}

fn union_<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> where T : PartialEq {
    let mut result: Vec<T> = Vec::new();
    result.append(&mut a);
    result.append(&mut b);
    dedup_unsorted(&mut result);
    result
}

pub fn concat<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> where T : Clone {
    let mut result: Vec<T> = Vec::new();
    result.append(&mut a);
    result.append(&mut b);
    result
}

fn dedup_unsorted<T>(items: &mut Vec<T>) where T: PartialEq {
    let mut i = 1;
    while i < items.len() {
        if items[0..i].contains(&items[i]) {
            items.remove(i);
        }
        else {
            i += 1;
        }
    }
}

fn intersect_<T>(a: &[T], b: &[T]) -> Vec<T> where T : PartialEq + Clone {
    let mut result: Vec<T> = Vec::new();
    for ai in a {
        if b.contains(ai) {
            result.push(ai.clone());
        }
    }
    result
}

pub fn mgu(t1: &Rc<Type>, t2: &Rc<Type>) -> Result<Subst, TypeError> {
    match (&**t1, &**t2) {
        (Type::TAp(l1, r1), Type::TAp(l2, r2)) => {
            let s1 = mgu(l1, l2)?;
            let s2 = mgu(&r1.apply(&s1), &r2.apply(&s1))?;
            Ok(s2.atat(&s1))
        }
        (Type::TVar(u), _) => var_bind(u, t2),
        (_, Type::TVar(u)) => var_bind(u, t1),
        (Type::TCon(tc1), Type::TCon(tc2)) if tc1 == tc2 => Ok(null_subst()),
        _ => Err(TypeError::TypesDoNotUnify),
    }
}

pub fn mgu_pred(p1: &Pred, p2: &Pred) -> Result<Subst, TypeError> {
    match (p1, p2) {
        (Pred::IsIn(i1, t1), Pred::IsIn(i2, t2)) if i1 == i2 => mgu(t1, t2),
        _ => Err(TypeError::ClassesDiffer),
    }
}

pub fn var_bind(u: &Rc<Tyvar>, t: &Rc<Type>) -> Result<Subst, TypeError> {
    if matches!(&**t, Type::TVar(v) if u == v) {
        Ok(null_subst())
    }
    else if t.tv().contains(u) {
        Err(TypeError::OccursCheckFails)
    }
    else if u.kind() != t.kind() {
        Err(TypeError::KindsDoNotMatch)
    }
    else {
        Ok(Subst(vec![(u.clone(), t.clone())]))
    }
}

pub fn match_(t1: &Rc<Type>, t2: &Rc<Type>) -> Result<Subst, TypeError> {
    match (&**t1, &**t2) {
        (Type::TAp(l1, r1), Type::TAp(l2, r2)) => {
            let sl = match_(l1, l2)?;
            let sr = match_(r1, r2)?;
            sl.merge(&sr)
        }
        (Type::TVar(u), t) if u.kind() == t.kind() => Ok(Subst(vec![(u.clone(), t2.clone())])),
        (Type::TCon(tc1), Type::TCon(tc2)) if tc1 == tc2 => Ok(null_subst()),
        _ => Err(TypeError::TypesDoNotMatch),
    }
}

pub fn match_pred(p1: &Pred, p2: &Pred) -> Result<Subst, TypeError> {
    match (p1, p2) {
        (Pred::IsIn(i1, t1), Pred::IsIn(i2, t2)) if i1 == i2 => match_(t1, t2),
        _ => Err(TypeError::ClassesDiffer),
    }
}

pub fn find(i: &Id, assumptions: &[Assump]) -> Result<Rc<Scheme>, TypeError> {
    for a in assumptions.iter() {
        if *i == a.0 {
            return Ok(a.1.clone());
        }
    }
    Err(TypeError::UnboundIdentifier(i.0.clone()))
}
