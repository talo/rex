use std::rc::Rc;
use std::borrow::Borrow;
use std::collections::BTreeMap;
use crate::extras::{Id, Qual, Pred};
use crate::types::Type;
use crate::error::TypeError;
use crate::subst::{mgu_pred, match_pred, Types};

#[derive(Clone, Debug)]
pub struct Class(pub Vec<Id>, pub Vec<Inst>);
pub type Inst = Qual<Pred>;

#[derive(Clone, Debug)]
pub struct ClassEnv {
    pub classes: BTreeMap<String, Class>,
    pub defaults: Vec<Rc<Type>>,
}

impl Class {
    pub fn superclasses(&self) -> &[Id] {
        &self.0
    }

    pub fn instances(&self) -> &[Inst] {
        &self.1
    }
}

impl ClassEnv {
    pub fn new() -> Self {
        ClassEnv {
            classes: BTreeMap::new(),
            defaults: Vec::new(),
        }
    }

    pub fn superclasses(&self, id: impl Borrow<Id>) -> Option<Vec<Id>> {
        let id: &Id = id.borrow();
        match self.classes.get(&id.0) {
            Some(Class(is, _)) => Some(is.clone()),
            None => None,
        }
    }

    pub fn instances(&self, id: impl Borrow<Id>) -> Option<Vec<Inst>> {
        let id: &Id = id.borrow();
        match self.classes.get(&id.0) {
            Some(Class(_, its)) => Some(its.clone()),
            None => None,
        }
    }

    // p14
    pub fn add_class(&mut self, i: &str, is: &[&str]) -> Result<(), TypeError> {
        let i: Id = Id(i.into());
        let is: Vec<Id> = is.into_iter().map(|x| Id(x.to_string())).collect::<Vec<_>>();
        if self.classes.contains_key(&i.0) {
            return Err(TypeError::ClassAlreadyDefined(i.0.clone()));
        }
        for inst_name in is.iter() {
            if !self.classes.contains_key(&inst_name.0) {
                return Err(TypeError::SuperclassNotDefined(inst_name.0.clone()));
            }
        }
        let c: Class = Class(is, vec![]);
        self.classes.insert(i.0, c);
        Ok(())
    }

    // p15
    pub fn add_inst(&mut self, ps: &[Pred], p: impl Borrow<Pred>) -> Result<(), TypeError> {
        let p = p.borrow();
        let i: &Id = p.id();
        let cls: &mut Class = self.classes.get_mut(&i.0)
            .ok_or_else(|| TypeError::NoClassForInstance(i.0.clone()))?;
        for inst in cls.1.iter() {
            if overlap(p, &inst.1) {
                return Err(TypeError::OverlappingInstance);
            }
        }
        cls.1.push(Qual(ps.to_vec(), p.clone()));
        Ok(())
    }

    // p16
    pub fn by_super(&self, p: impl Borrow<Pred>) -> Vec<Pred> {
        let p = p.borrow();
        let mut result: Vec<Pred> = Vec::new();
        result.push(p.clone());
        for super_id in self.superclasses(p.id()).unwrap().iter() {
            result.extend_from_slice(&self.by_super(Pred(super_id.clone(), p.t().clone())));
        }
        result
    }

    // p16
    pub fn by_inst(&self, p: impl Borrow<Pred>) -> Option<Vec<Pred>> {
        let p = p.borrow();
        for it in self.instances(p.id()).unwrap().iter() {
            if let Ok(u) = match_pred(&it.1, p) {
                return Some(it.0.iter().map(|z| z.apply(&u)).collect::<Vec<_>>());
            }
        }
        None
    }

    // p17
    pub fn entail(&self, ps: &[Pred], p: impl Borrow<Pred>) -> bool {
        let p = p.borrow();
        for ps_item in ps.iter() {
            if self.by_super(ps_item).contains(p) {
                return true;
            }
        }

        if let Some(qs) = self.by_inst(p) {
            return qs.iter().all(|q| self.entail(ps, q));
        }

        false
    }

    pub fn dump(&self) {
        println!("ClassEnv");
        for (name, class) in self.classes.iter() {
            println!("    Class {:?}", name);
            println!(
                "        superclasses: {:?}",
                class
                    .superclasses()
                    .iter()
                    .map(|x| &x.0).collect::<Vec<_>>());
            println!("        instances:");
            for inst in class.instances().iter() {
                println!("            {}", inst);
            }
        }
    }
}

// p15
pub fn overlap(p: &Pred, q: &Pred) -> bool {
    mgu_pred(p, q).is_ok()
    // unimplemented!()
}

pub fn super_(ce: &ClassEnv, id: &Id) -> Vec<Id> {
    ce.superclasses(id).unwrap()
}

pub fn insts(ce: &ClassEnv, id: &Id) -> Vec<Inst> {
    ce.instances(id).unwrap()
}

pub fn defined<T>(x: Option<T>) -> bool {
    x.is_some()
}

pub fn modify(ce: &ClassEnv, i: &Id, c: &Class) -> ClassEnv {
    let mut ce = ce.clone();
    ce.classes.insert(i.0.clone(), c.clone());
    ce
}

// p14
pub fn add_core_classes(ce: &mut ClassEnv) -> Result<(), TypeError> {
    ce.add_class("Eq", &[])?;
    ce.add_class("Ord", &["Eq"])?;
    ce.add_class("Show", &[])?;
    ce.add_class("Read", &[])?;
    ce.add_class("Bounded", &[])?;
    ce.add_class("Enum", &[])?;
    ce.add_class("Functor", &[])?;
    ce.add_class("Monad", &[])?;

    use crate::helpers::*;
    use crate::*;
    let a = tyvar("a", star());
    let b = tyvar("b", star());

    let eq = ce.classes.get_mut("Eq").unwrap();
    eq.1.push(Qual(vec![], isin("Ord", t_unit())));
    eq.1.push(Qual(vec![], isin("Ord", t_char())));
    eq.1.push(Qual(vec![], isin("Ord", t_int())));
    eq.1.push(Qual(vec![isin("Ord", tvar(&a))], isin("Ord", list(tvar(&a)))));
    eq.1.push(Qual(vec![
        isin("Ord", tvar(&a)),
        isin("Ord", tvar(&b)),
        ], isin("Ord", pair(tvar(&a), tvar(&b)))));

    Ok(())
}

// p14
pub fn add_num_classes(ce: &mut ClassEnv) -> Result<(), TypeError> {
    ce.add_class("Num", &["Eq", "Show"])?;
    ce.add_class("Real", &["Num", "Ord"])?;
    ce.add_class("Fractional", &["Num"])?;
    ce.add_class("Integral", &["Real", "Enum"])?;
    ce.add_class("RealFrac", &["Real", "Fractional"])?;
    ce.add_class("Floating", &["Fractional"])?;
    ce.add_class("RealFloat", &["RealFrac", "Floating"])?;

    Ok(())
}
