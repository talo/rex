use std::borrow::Borrow;
use std::fmt;
use std::rc::Rc;
use crate::subst::{Types, Subst};
use crate::kind::Kind;
use crate::types::{Type, Tyvar, Tycon};

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Id(pub String);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Qual<T>(pub Vec<Pred>, pub T);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Pred(pub Id, pub Rc<Type>);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Scheme(pub Vec<Rc<Kind>>, pub Qual<Rc<Type>>);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Assump(pub Id, pub Rc<Scheme>);

impl Pred {
    pub fn id(&self) -> &Id {
        &self.0
    }

    pub fn t(&self) -> &Rc<Type> {
        &self.1
    }
}

impl Scheme {
    pub fn kinds(&self) -> &[Rc<Kind>] {
        &self.0
    }

    pub fn qualified_type(&self) -> &Qual<Rc<Type>> {
        &self.1
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Forall [")?;
        for (i, kind) in self.kinds().iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", kind)?;
        }
        write!(f, "] {}", self.qualified_type())
    }
}

pub trait HasKind {
    fn kind(&self) -> Rc<Kind>;
}

impl HasKind for Tyvar {
    fn kind(&self) -> Rc<Kind> {
        self.1.clone()
    }
}

impl HasKind for Tycon {
    fn kind(&self) -> Rc<Kind> {
        self.1.clone()
    }
}

impl HasKind for Type {
    fn kind(&self) -> Rc<Kind> {
        match self {
            Type::TCon(tc) => tc.kind(),
            Type::TVar(u) => u.kind(),
            Type::TAp(t, _) => match &*t.kind() {
                Kind::Star => panic!("Type::TApp applied to *"),
                Kind::Kfun(_, k) => k.clone(),
            }
            Type::TGen(_) => panic!("Type::TGen has no kind"),
        }
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
        Pred(self.id().clone(), self.t().inst(ts))
    }
}

impl fmt::Display for Pred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match &*self.1 {
            Type::TVar(_) | Type::TCon(_) => write!(f, "{} {}", self.0.0, self.1),
            _ => write!(f, "{} ({})", self.0.0, self.1),
        }
    }
}

impl<T> fmt::Display for Qual<T> where T : fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "[")?;
        for (i, pred) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", pred)?;
        }
        write!(f, "] :=> {}", self.1)
    }
}


impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self.0)
    }
}

pub fn enum_id(i: u64) -> Id {
    Id(format!("v{}", i))
}

pub fn quantify(vs: &[Rc<Tyvar>], qt: &Qual<Rc<Type>>) -> Rc<Scheme> {
    let mut vs2: Vec<Rc<Tyvar>> = Vec::new();
    for v in qt.tv().iter() {
        if vs.contains(v) {
            vs2.push(v.clone());
        }
    }
    let mut subst_items: Vec<(Rc<Tyvar>, Rc<Type>)> = Vec::new();
    let mut gen_id = 0;
    for v in vs2.iter() {
        subst_items.push((v.clone(), Rc::new(Type::TGen(gen_id))));
        gen_id += 1;
    }
    let ks: Vec<Rc<Kind>> = vs2.iter().map(|v| v.kind()).collect::<Vec<_>>();
    Rc::new(Scheme(ks, qt.apply(&Subst(subst_items))))
}

pub fn to_scheme(t: impl Borrow<Rc<Type>>) -> Rc<Scheme> {
    Rc::new(Scheme(vec![], Qual(vec![], t.borrow().clone())))
}
