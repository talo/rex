use std::fmt;
use std::rc::Rc;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Id(pub String);

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Kind {
    Star,                      // *
    Kfun(Rc<Kind>, Rc<Kind>),  // k1 -> k2
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Type {
    TVar(Rc<Tyvar>),
    TCon(Rc<Tycon>),
    TAp(Rc<Type>, Rc<Type>),
    TGen(u64),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Tyvar(pub Id, pub Rc<Kind>);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Tycon(pub Id, pub Rc<Kind>);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Qual<T>(pub Vec<Pred>, pub T);

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Pred {
    IsIn(Id, Rc<Type>),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Scheme {
    Forall(Vec<Rc<Kind>>, Qual<Rc<Type>>)
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Assump(pub Id, pub Rc<Scheme>);

pub type Class = (Vec<Id>, Vec<Inst>);
pub type Inst = Qual<Pred>;

pub struct ClassEnv;

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

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self.0)
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Kind::Star => write!(f, "Star"),
            Kind::Kfun(l, r) => write!(f, "(Kfun {} {})", l, r),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Type::TVar(v) => write!(f, "TVar ({})", v),
            Type::TCon(c) => write!(f, "TCon ({})", c),
            Type::TAp(l, r) => write!(f, "TAp ({}) ({})", l, r),
            Type::TGen(i) => write!(f, "TGen {}", i),
        }
    }
}

pub struct TypeNoKind<'a>(pub &'a Type);
impl<'a> fmt::Display for TypeNoKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt_type_nokind(self.0, false, f)
    }
}

pub fn fmt_type_nokind(t: &Type, paren: bool, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
    match t {
        Type::TVar(v) => write!(f, "{}", v.0.0)?,
        Type::TCon(c) => write!(f, "{}", c.0.0)?,
        Type::TAp(l, r) => {
            if paren {
                write!(f, "(")?;
            }

            fmt_type_nokind(l, true, f)?;
            write!(f, " ")?;
            fmt_type_nokind(r, false, f)?;

            if paren {
                write!(f, ")")?;
            }

        }
        Type::TGen(i) => write!(f, "{}", i)?,
    }
    Ok(())
}

impl fmt::Display for Tyvar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Tyvar {} {}", self.0, self.1)
    }
}

impl fmt::Display for Tycon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Tycon {} {}", self.0, self.1)
    }
}

pub fn enum_id(i: u64) -> Id {
    Id(format!("v{}", i))
}
