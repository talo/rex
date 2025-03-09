use std::rc::Rc;
use std::borrow::Borrow;

pub mod classes;
pub mod debug;
pub mod error;
pub mod expr;
pub mod extras;
pub mod helpers;
pub mod inference;
pub mod kind;
pub mod subst;
pub mod types;
pub mod util;

use kind::Kind;
use classes::{Class};
use extras::{Id, Qual};
use types::{Type, Tycon};

pub fn t_unit() -> Rc<Type> {
    Rc::new(Type::TCon(Rc::new(Tycon(Id("()".to_string()), Rc::new(Kind::Star)))))
}

pub fn t_bool() -> Rc<Type> {
    Rc::new(Type::TCon(Rc::new(Tycon(Id("Bool".to_string()), Rc::new(Kind::Star)))))
}

pub fn t_char() -> Rc<Type> {
    Rc::new(Type::TCon(Rc::new(Tycon(Id("Char".to_string()), Rc::new(Kind::Star)))))
}

pub fn t_int() -> Rc<Type> {
    Rc::new(Type::TCon(Rc::new(Tycon(Id("Int".to_string()), Rc::new(Kind::Star)))))
}

pub fn t_integer() -> Rc<Type> {
    Rc::new(Type::TCon(Rc::new(Tycon(Id("Integer".to_string()), Rc::new(Kind::Star)))))
}

pub fn t_float() -> Rc<Type> {
    Rc::new(Type::TCon(Rc::new(Tycon(Id("Float".to_string()), Rc::new(Kind::Star)))))
}

pub fn t_double() -> Rc<Type> {
    Rc::new(Type::TCon(Rc::new(Tycon(Id("Double".to_string()), Rc::new(Kind::Star)))))
}

pub fn t_list() -> Rc<Type> {
    Rc::new(Type::TCon(
        Rc::new(Tycon(Id("[]".to_string()),
        Rc::new(Kind::Kfun(Rc::new(Kind::Star), Rc::new(Kind::Star)))))))
}

pub fn t_arrow() -> Rc<Type> {
    Rc::new(Type::TCon(
        Rc::new(Tycon(Id("->".to_string()),
        Rc::new(Kind::Kfun(Rc::new(Kind::Star),
            Rc::new(Kind::Kfun(Rc::new(Kind::Star), Rc::new(Kind::Star)))))))))
}

pub fn t_tuple2() -> Rc<Type> {
    Rc::new(Type::TCon(
        Rc::new(Tycon(Id("(,)".to_string()),
            Rc::new(Kind::Kfun(
                Rc::new(Kind::Star),
                Rc::new(Kind::Kfun(Rc::new(Kind::Star), Rc::new(Kind::Star)))))))))
}

pub fn t_string() -> Rc<Type> {
    Rc::new(Type::TAp(t_list(), t_char()))
}

pub fn list(a: impl Borrow<Rc<Type>>) -> Rc<Type> {
    Rc::new(Type::TAp(t_list(), a.borrow().clone()))
}

pub fn fun(a: impl Borrow<Rc<Type>>, b: impl Borrow<Rc<Type>>) -> Rc<Type> {
    Rc::new(Type::TAp(
        Rc::new(Type::TAp(t_arrow(), a.borrow().clone())),
        b.borrow().clone()))
}

pub fn pair(a: impl Borrow<Rc<Type>>, b: impl Borrow<Rc<Type>>) -> Rc<Type> {
    Rc::new(Type::TAp(
        Rc::new(Type::TAp(t_tuple2(), a.borrow().clone())),
        b.borrow().clone()))
}

pub fn haskell_eq_class() -> Class {
    use helpers::*;
    Class(vec![id("Eq")], vec![
        Qual(vec![], isin("Ord", t_unit())),
        Qual(vec![], isin("Ord", t_char())),
        Qual(vec![], isin("Ord", t_int())),
        Qual(vec![
                isin("Ord", tvar(tyvar("a", star()))),
                isin("Ord", tvar(tyvar("b", star())))
            ],
            isin("Ord", pair(
                tvar(tyvar("a", star())),
                tvar(tyvar("b", star())))))
    ])
}

pub fn make_fn(a: Type, b: Type) -> Rc<Type> {
    Rc::new(Type::TAp(
        Rc::new(Type::TAp(
            t_arrow(),
            Rc::new(a))),
        Rc::new(b)))
}

pub fn make_list(t : Type) -> Rc<Type> {
    Rc::new(Type::TAp(t_list(), Rc::new(t)))
}

pub fn make_pair(a: Type, b: Type) -> Rc<Type> {
    Rc::new(Type::TAp(
        Rc::new(Type::TAp(
            t_tuple2(),
            Rc::new(a))),
        Rc::new(b)))
}

#[cfg(test)]
pub mod test {
    use super::*;
    use helpers::*;
    use error::TypeError;
    use classes::ClassEnv;
    use extras::{quantify, Scheme, HasKind};
    use subst::{Subst, mgu, var_bind, null_subst, match_, Types};

    #[test]
    fn test_basic_types() {
        assert_eq!(t_unit(), tcon(tycon("()", star())));
        assert_eq!(t_char(), tcon(tycon("Char", star())));
        assert_eq!(t_int(), tcon(tycon("Int", star())));
        assert_eq!(t_integer(), tcon(tycon("Integer", star())));
        assert_eq!(t_float(), tcon(tycon("Float", star())));
        assert_eq!(t_double(), tcon(tycon("Double", star())));
        assert_eq!(t_list(), tcon(tycon("[]", kfun(star(), star()))));
        assert_eq!(t_arrow(), tcon(tycon("->", kfun(star(), kfun(star(), star())))));
        assert_eq!(t_tuple2(), tcon(tycon("(,)", kfun(star(), kfun(star(), star())))));
        assert_eq!(t_string(), tap(t_list(), t_char()));
    }

    #[test]
    fn test_kind_display() {
        assert_eq!(format!("{}", star()), "*");
        assert_eq!(format!("{}", kfun(star(), star())), "* -> *");
        assert_eq!(
            format!("{}", kfun(star(), kfun(star(), kfun(star(), star())))),
            "* -> * -> * -> *");
        assert_eq!(
            format!("{}", kfun(kfun(kfun(star(), star()), star()), star())),
            "((* -> *) -> *) -> *");
        assert_eq!(
            format!("{}", kfun(
                kfun(star(), star()),
                kfun(star(), star()))),
            "(* -> *) -> * -> *");
        assert_eq!(
            format!("{}", kfun(
                kfun(star(), star()),
                kfun(kfun(star(), star()),
                    kfun(star(), star())))),
            "(* -> *) -> (* -> *) -> * -> *");
    }

    #[test]
    fn test_type_display() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());
        let c = tyvar("c", star());
        let d = tyvar("d", star());

        let t0 = fun(fun(fun(tvar(&a), tvar(&b)), tvar(&c)), tvar(&d));
        assert_eq!(format!("{}", t0), "((a -> b) -> c) -> d");

        let tx = fun(fun(tvar(&a), tvar(&b)), fun(tvar(&c), tvar(&d)));
        assert_eq!(format!("{}", tx), "(a -> b) -> c -> d");

        let t1 = fun(tvar(&a), fun(tvar(&b), fun(tvar(&c), tvar(&d))));
        assert_eq!(format!("{}", t1), "a -> b -> c -> d");

        let t2 = tap(tvar(&a), tap(tvar(&b), tap(tvar(&c), tvar(&d))));
        assert_eq!(format!("{}", t2), "TAp a TAp b TAp c d");

        let t3 = tap(tap(tap(tvar(&a), tvar(&b)), tvar(&c)), tvar(&d));
        assert_eq!(format!("{}", t3), "TAp (TAp (TAp a b) c) d");

        let t4 = fun(pair(tvar(&a), tvar(&b)), pair(tvar(&c), tvar(&d)));
        assert_eq!(format!("{}", t4), "(a , b) -> c , d");

        let t5 = pair(fun(tvar(&a), tvar(&b)), fun(tvar(&c), tvar(&d)));
        assert_eq!(format!("{}", t5), "a -> b , c -> d");

        assert_eq!(format!("{}", list(t_int())), "[Int]");
        assert_eq!(format!("{}", fun(list(t_int()), list(t_char()))), "[Int] -> [Char]");
        assert_eq!(format!("{}", list(fun(t_int(), t_char()))), "[Int -> Char]");
    }

    #[test]
    fn test_basic_types_kind() {
        assert_eq!(t_unit().kind(), star());
        assert_eq!(t_char().kind(), star());
        assert_eq!(t_int().kind(), star());
        assert_eq!(t_integer().kind(), star());
        assert_eq!(t_float().kind(), star());
        assert_eq!(t_double().kind(), star());
        assert_eq!(t_list().kind(), kfun(star(), star()));
        assert_eq!(t_arrow().kind(), kfun(star(), kfun(star(), star())));
        assert_eq!(t_tuple2().kind(), kfun(star(), kfun(star(), star())));
        assert_eq!(t_string().kind(), star());
    }

    #[test]
    fn test_type_tvars() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());
        let t1 = tap(&tvar(&a), &tap(&tvar(&b), &tap(&tvar(&a), &tvar(&b))));
        assert_eq!(t1.tv(), vec![a.clone(), b.clone()]);

        let ts = vec![tvar(&a), tvar(&b), tvar(&a), tvar(&b)];
        assert_eq!(ts.tv(), vec![a.clone(), b.clone()]);
    }

    #[test]
    fn test_type_apply() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());
        let t = tap(tvar(&a), tap(tvar(&b), tap(tvar(&a), tvar(&b))));
        let s = Subst(vec![mapsto(&a, t_int()), mapsto(&b, t_char())]);
        assert_eq!(
            t.apply(&s),
            tap(t_int(), tap(t_char(), tap(t_int(), t_char()))));

        let ts = vec![tap(tvar(&a), tvar(&b)), tap(tvar(&b), tvar(&a))];
        assert_eq!(
            ts.apply(&s),
            vec![tap(t_int(), t_char()), tap(t_char(), t_int())]);

    }

    #[test]
    fn test_atat() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());
        let s1 = Subst(vec![mapsto(&a, tvar(&b))]);
        let s2 = Subst(vec![mapsto(&b, t_int())]);
        let s12 = s1.atat(&s2);
        let s21 = s2.atat(&s1);
        assert_eq!(s12, Subst(vec![mapsto(&b, t_int()), mapsto(&a, tvar(&b))]));
        assert_eq!(s21, Subst(vec![mapsto(&a, t_int()), mapsto(&b, t_int())]));
        let t = tap(tvar(&a), tvar(&b));
        assert_eq!(t.apply(&s12), tap(tvar(&b), t_int()));
        assert_eq!(t.apply(&s21), tap(t_int(), t_int()));
    }

    #[test]
    fn test_merge() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());
        let c = tyvar("c", star());

        let s1 = Subst(vec![
            mapsto(&a, t_int()),
            mapsto(&b, t_char()),
        ]);
        let s2 = Subst(vec![
            mapsto(&b, t_char()),
            mapsto(&c, t_float()),
        ]);
        assert_eq!(s1.merge(&s2), Ok(Subst(vec![
            mapsto(&a, t_int()),
            mapsto(&b, t_char()),
            mapsto(&b, t_char()),
            mapsto(&c, t_float()),
        ])));

        let s3 = Subst(vec![
            mapsto(&b, t_float()),
            mapsto(&c, t_float()),
        ]);
        assert_eq!(s1.merge(&s3), Err(TypeError::MergeFails));
    }

    #[test]
    fn test_mgu() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());
        let x = tyvar("x", kfun(star(), star()));
        let y = tyvar("y", kfun(star(), star()));
        let t1 = tap(tvar(&x), tvar(&a));
        let t2 = tap(tvar(&y), tvar(&b));

        assert_eq!(
            mgu(&tvar(&a), &tvar(&b)),
            Ok(Subst(vec![mapsto(&a, tvar(&b))])));
        assert_eq!(
            mgu(&t1, &t2),
            Ok(Subst(vec![
                mapsto(&x, &tvar(&y)),
                mapsto(&a, &tvar(&b)),
            ])));
        assert_eq!(
            mgu(&t_int(), &t_char()),
            Err(TypeError::TypesDoNotUnify(t_int(), t_char())));
        assert_eq!(
            mgu(&t_int(), &t_int()),
            Ok(Subst(vec![])));
        assert_eq!(
            mgu(&tvar(&a), &tvar(&x)),
            Err(TypeError::KindsDoNotMatch));
    }

    #[test]
    fn test_var_bind() {
        let a = tyvar("a", star());
        let b = tyvar("b", kfun(star(), star()));
        let c = tyvar("c", star());

        assert_eq!(
            var_bind(&a, &tvar(&a)),
            Ok(null_subst()));
        assert_eq!(
            var_bind(&a, &tap(tvar(&a), t_int())),
            Err(TypeError::OccursCheckFails));
        assert_eq!(
            var_bind(&a, &tap(t_int(), tvar(&a))),
            Err(TypeError::OccursCheckFails));
        assert_eq!(
            var_bind(&a, &tvar(&b)),
            Err(TypeError::KindsDoNotMatch));
        assert_eq!(
            var_bind(&a, &tvar(&c)),
            Ok(Subst(vec![mapsto(&a, tvar(&c))])));
        assert_eq!(
            var_bind(&c, &tap(tvar(&b), t_int())),
            Ok(Subst(vec![mapsto(&c, &tap(tvar(&b), t_int()))])));
    }

    #[test]
    fn test_match() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());
        let x = tyvar("x", kfun(star(), star()));

        assert_eq!(
            match_(&tvar(&a), &tvar(&b)),
            Ok(Subst(vec![mapsto(&a, tvar(&b))])));
        assert_eq!(
            match_(&t_int(), &t_int()),
            Ok(Subst(vec![])));
        assert_eq!(
            match_(&t_int(), &t_char()),
            Err(TypeError::TypesDoNotMatch));
        assert_eq!(
            match_(&tvar(&a), &t_int()),
            Ok(Subst(vec![mapsto(&a, t_int())])));
        assert_eq!(
            match_(&t_int(), &tvar(&a)),
            Err(TypeError::TypesDoNotMatch));
        assert_eq!(
            match_(
                &tap(t_list(), tvar(&a)),
                &tap(t_list(), tvar(&b))),
            Ok(Subst(vec![mapsto(&a, tvar(&b))])));
        assert_eq!(
            match_(
                &tap(t_list(), tvar(&a)),
                &tap(t_list(), tvar(&x))),
            Err(TypeError::TypesDoNotMatch));
        assert_eq!(
            match_(
                &tap(t_list(), tvar(&x)),
                &tap(t_list(), tvar(&b))),
            Err(TypeError::TypesDoNotMatch));
    }

    #[test]
    fn test_add_class() {
        let mut ce = ClassEnv::new();
        ce.add_class("Eq", &[]).unwrap();
        assert_eq!(ce.add_class("Eq", &[]), Err(TypeError::ClassAlreadyDefined("Eq".to_string())));
    }

    #[test]
    fn test_add_inst_overlap1() {
        let mut ce = ClassEnv::new();
        ce.add_class("Eq", &[]).unwrap();
        ce.add_inst(&[], isin("Eq", t_int())).unwrap();
        assert_eq!(
            ce.add_inst(&[], isin("Eq", t_int())),
            Err(TypeError::OverlappingInstance));
    }

    #[test]
    fn test_add_inst_overlap2() {
        let mut ce = ClassEnv::new();
        ce.add_class("Eq", &[]).unwrap();
        ce.add_inst(&[], isin("Eq", t_int())).unwrap();
        assert_eq!(
            ce.add_inst(&[], isin("Eq", tvar(tyvar("a", star())))),
            Err(TypeError::OverlappingInstance));
    }

    #[test]
    fn test_add_inst_overlap3() {
        let mut ce = ClassEnv::new();
        ce.add_class("Eq", &[]).unwrap();
        ce.add_inst(&[], isin("Eq", pair(tvar(tyvar("a", star())), t_bool()))).unwrap();
        assert_eq!(
            ce.add_inst(&[], isin("Eq", pair(t_int(), tvar(tyvar("b", star()))))),
            Err(TypeError::OverlappingInstance));
    }

    #[test]
    fn test_quantify() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());

        let t1 = fun(tvar(&a), tvar(&b));
        assert_eq!(
            quantify(&[a.clone(), b.clone()], &Qual(vec![], t1.clone())),
            Rc::new(Scheme(vec![star(), star()], Qual(vec![], fun(tgen(0), tgen(1))))));

        assert_eq!(
            quantify(&[a.clone()], &Qual(vec![], t1.clone())),
            Rc::new(Scheme(vec![star()], Qual(vec![], fun(tgen(0), tvar(&b))))));

        assert_eq!(
            quantify(&[b.clone()], &Qual(vec![], t1.clone())),
            Rc::new(Scheme(vec![star()], Qual(vec![], fun(tvar(&a), tgen(0))))));

        assert_eq!(
            quantify(&[], &Qual(vec![], t1.clone())),
            Rc::new(Scheme(vec![], Qual(vec![], fun(tvar(&a), tvar(&b))))));

        let t2 = fun(&tvar(&a), &fun(&tvar(&b), &fun(&tvar(&a), &tvar(&b))));
        assert_eq!(
            quantify(
                &[a.clone(), b.clone()],
                &Qual(
                    vec![isin("Foo", tvar(&a)), isin("Bar", tvar(&b))],
                    t2.clone())),
            Rc::new(Scheme(
                vec![star(), star()],
                Qual(
                    vec![isin("Foo", tgen(0)), isin("Bar", tgen(1))],
                    fun(tgen(0), fun(tgen(1), fun(tgen(0), tgen(1))))))));
    }

    #[test]
    fn test_by_super1() {
        let mut ce = ClassEnv::new();
        ce.add_class("A", &[]).unwrap();
        ce.add_class("B", &["A"]).unwrap();
        ce.add_class("C", &["B"]).unwrap();
        ce.add_class("Z", &[]).unwrap();
        let x = tyvar("x", star());
        let res = ce.by_super(isin("C", tvar(&x)));
        assert_eq!(
            res,
            vec![
                isin("C", tvar(&x)),
                isin("B", tvar(&x)),
                isin("A", tvar(&x)),
            ]);

        assert!(ce.entail(&res, &isin("A", tvar(&x))));
        assert!(ce.entail(&res, &isin("B", tvar(&x))));
        assert!(ce.entail(&res, &isin("C", tvar(&x))));
        assert!(!ce.entail(&res, &isin("Z", tvar(&x))));
    }

    #[test]
    fn test_by_super2() {
        let mut ce = ClassEnv::new();
        ce.add_class("A", &[]).unwrap();
        ce.add_class("B", &[]).unwrap();
        ce.add_class("C", &["A", "B"]).unwrap();
        ce.add_class("Z", &[]).unwrap();
        let x = tyvar("x", star());
        let res = ce.by_super(isin("C", tvar(&x)));
        assert_eq!(
            res,
            vec![
                isin("C", tvar(&x)),
                isin("A", tvar(&x)),
                isin("B", tvar(&x)),
            ]);

        assert!(ce.entail(&res, isin("A", tvar(&x))));
        assert!(ce.entail(&res, isin("B", tvar(&x))));
        assert!(ce.entail(&res, isin("C", tvar(&x))));
        assert!(!ce.entail(&res, isin("Z", tvar(&x))));
    }

    #[test]
    fn test_by_super3() {
        let mut ce = ClassEnv::new();
        ce.add_class("A", &[]).unwrap();
        ce.add_class("B", &[]).unwrap();
        ce.add_class("C", &["A"]).unwrap();
        ce.add_class("D", &["B"]).unwrap();
        ce.add_class("E", &["C", "D"]).unwrap();
        ce.add_class("Z", &[]).unwrap();

        let x = tyvar("x", star());
        let res = ce.by_super(isin("E", tvar(&x)));
        assert_eq!(
            res,
            vec![
                isin("E", tvar(&x)),
                isin("C", tvar(&x)),
                isin("A", tvar(&x)),
                isin("D", tvar(&x)),
                isin("B", tvar(&x)),
            ]);


        assert!(ce.entail(&res, isin("A", tvar(&x))));
        assert!(ce.entail(&res, isin("B", tvar(&x))));
        assert!(ce.entail(&res, isin("C", tvar(&x))));
        assert!(ce.entail(&res, isin("D", tvar(&x))));
        assert!(ce.entail(&res, isin("E", tvar(&x))));
        assert!(!ce.entail(&res, isin("Z", tvar(&x))));
    }

    #[test]
    fn test_by_super4() {
        let mut ce = ClassEnv::new();
        ce.add_class("A", &[]).unwrap();
        ce.add_class("B", &["A"]).unwrap();
        ce.add_class("C", &["A"]).unwrap();
        ce.add_class("D", &["B", "C"]).unwrap();
        ce.add_class("Z", &[]).unwrap();

        let x = tyvar("x", star());
        let res = ce.by_super(isin("D", tvar(&x)));
        assert_eq!(
            res,
            vec![
                isin("D", tvar(&x)),
                isin("B", tvar(&x)),
                isin("A", tvar(&x)),
                isin("C", tvar(&x)),
                isin("A", tvar(&x)),
            ]);

        assert!(ce.entail(&res, isin("A", tvar(&x))));
        assert!(ce.entail(&res, isin("B", tvar(&x))));
        assert!(ce.entail(&res, isin("C", tvar(&x))));
        assert!(ce.entail(&res, isin("D", tvar(&x))));
        assert!(!ce.entail(&res, isin("Z", tvar(&x))));
    }

    #[test]
    fn test_by_inst() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());

        let mut ce = ClassEnv::new();
        ce.add_class("Ord", &[]).unwrap();
        ce.add_inst(&[], isin("Ord", t_unit())).unwrap();
        ce.add_inst(&[], isin("Ord", t_char())).unwrap();
        ce.add_inst(&[], isin("Ord", t_int())).unwrap();
        ce.add_inst(
            &[isin("Ord", tvar(&a)), isin("Ord", tvar(&b))],
            isin("Ord", pair(tvar(&a), tvar(&b)))).unwrap();

        assert_eq!(ce.by_inst(isin("Ord", t_unit())), Some(vec![]));
        assert_eq!(ce.by_inst(isin("Ord", t_char())), Some(vec![]));
        assert_eq!(ce.by_inst(isin("Ord", t_int())), Some(vec![]));
        assert_eq!(
            ce.by_inst(isin("Ord", pair(t_int(), t_char()))),
            Some(vec![isin("Ord", t_int()), isin("Ord", t_char())]));
        assert_eq!(
            ce.by_inst(isin("Ord", pair(tvar(&b), tvar(&a)))),
            Some(vec![isin("Ord", tvar(&b)), isin("Ord", tvar(&a))]));
        assert_eq!(ce.by_inst(isin("Ord", fun(tvar(&b), tvar(&a)))), None);

        assert!(ce.entail(&[], isin("Ord", t_unit())));
        assert!(ce.entail(&[], isin("Ord", t_char())));
        assert!(ce.entail(&[], isin("Ord", t_int())));

        // (int, a)
        let t = pair(t_int(), tvar(&a));
        assert!(!ce.entail(&[], isin("Ord", &t)));
        assert!(ce.entail(&[isin("Ord", tvar(&a))], isin("Ord", &t)));

        // (a, int)
        let t = pair(tvar(&a), t_int());
        assert!(!ce.entail(&[], isin("Ord", &t)));
        assert!(ce.entail(&[isin("Ord", tvar(&a))], isin("Ord", &t)));

        // (a, a)
        let t = pair(tvar(&a), tvar(&a));
        assert!(!ce.entail(&[], isin("Ord", &t)));
        assert!(ce.entail(&[isin("Ord", tvar(&a))], isin("Ord", &t)));

        // (a, b)
        let t = pair(tvar(&a), tvar(&b));
        assert!(!ce.entail(&[], isin("Ord", &t)));
        assert!(!ce.entail(&[isin("Ord", tvar(&a))], isin("Ord", &t)));
        assert!(!ce.entail(&[isin("Ord", tvar(&b))], isin("Ord", &t)));
        assert!(ce.entail(&[isin("Ord", tvar(&a)), isin("Ord", tvar(&b))], isin("Ord", &t)));
    }

    #[test]
    fn test_entail() {
        let a = tyvar("a", star());
        let b = tyvar("b", star());

        let mut ce = ClassEnv::new();
        ce.add_class("Eq", &[]).unwrap();
        ce.add_class("Ord", &["Eq"]).unwrap();
        ce.add_inst(&[], isin("Ord", t_int())).unwrap();
        ce.add_inst(
            &[isin("Eq", tvar(&a)), isin("Eq", tvar(&b))],
            isin("Eq", pair(tvar(&a), tvar(&b)))).unwrap();

        assert!(ce.entail(&[isin("Ord", tvar(&a))], isin("Eq", tvar(&a))));
        assert!(!ce.entail(&[isin("Eq", tvar(&a))], isin("Ord", tvar(&a))));
        assert!(ce.entail(
            &[isin("Ord", tvar(&a)), isin("Ord", tvar(&b))],
            isin("Eq", pair(tvar(&a), tvar(&b)))));
    }

}
