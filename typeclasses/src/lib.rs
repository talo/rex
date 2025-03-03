use std::rc::Rc;

pub mod helpers;
pub mod types;
pub mod subst;
pub mod error;
pub mod inference;
pub use types::{Id, Kind, Type, Tycon, Tyvar, Class, Qual, Pred, HasKind};
pub use subst::{Subst, mgu, var_bind, null_subst, match_, Types};

pub fn t_unit() -> Rc<Type> {
    Rc::new(Type::TCon(Rc::new(Tycon(Id("()".to_string()), Rc::new(Kind::Star)))))
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

pub fn pair(a: &Rc<Type>, b: &Rc<Type>) -> Rc<Type> {
    Rc::new(Type::TAp(
        Rc::new(Type::TAp(t_tuple2(), a.clone())),
        b.clone()))
}

pub fn haskell_eq_class() -> Class {
    use helpers::*;
    (vec![id("Eq")], vec![
        Qual(vec![], Pred::IsIn(id("Ord"), t_unit())),
        Qual(vec![], Pred::IsIn(id("Ord"), t_char())),
        Qual(vec![], Pred::IsIn(id("Ord"), t_int())),
        Qual(vec![
                Pred::IsIn(id("Ord"), tvar(tyvar("a", star()))),
                Pred::IsIn(id("Ord"), tvar(tyvar("b", star())))
            ],
            Pred::IsIn(id("Ord"), pair(
                &tvar(tyvar("a", star())),
                &tvar(tyvar("b", star())))))
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

    #[test]
    fn test_basic_types() {
        assert_eq!(format!("{}", t_unit()),
            r#"TCon (Tycon "()" Star)"#);
        assert_eq!(format!("{}", t_char()),
            r#"TCon (Tycon "Char" Star)"#);
        assert_eq!(format!("{}", t_int()),
            r#"TCon (Tycon "Int" Star)"#);
        assert_eq!(format!("{}", t_integer()),
            r#"TCon (Tycon "Integer" Star)"#);
        assert_eq!(format!("{}", t_float()),
            r#"TCon (Tycon "Float" Star)"#);
        assert_eq!(format!("{}", t_double()),
            r#"TCon (Tycon "Double" Star)"#);
        assert_eq!(format!("{}", t_list()),
            r#"TCon (Tycon "[]" (Kfun Star Star))"#);
        println!("t_arrow = {}", t_arrow());
        assert_eq!(format!("{}", t_arrow()),
            r#"TCon (Tycon "->" (Kfun Star (Kfun Star Star)))"#);
        assert_eq!(format!("{}", t_tuple2()),
            r#"TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))"#);
        assert_eq!(format!("{}", t_string()),
            r#"TAp (TCon (Tycon "[]" (Kfun Star Star))) (TCon (Tycon "Char" Star))"#);
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
            Err(TypeError::TypesDoNotUnify));
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
}
