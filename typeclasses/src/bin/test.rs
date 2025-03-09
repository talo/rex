use std::collections::BTreeMap;
use typeclasses::*;
use typeclasses::types::*;
use typeclasses::extras::*;
use typeclasses::inference::*;
use typeclasses::helpers::*;
use typeclasses::classes::*;
use typeclasses::subst::*;
use std::rc::Rc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let a = tyvar("a", star());
    let b = tyvar("b", star());

    let subtract_type =
    tap(
        tap(
            tcon(tycon("->", kfun(star(), kfun(star(), star())))),
            tgen(0)),
        tap(
            tap(
                tcon(tycon("->", kfun(star(), kfun(star(), star())))),
                tgen(0)),
            tgen(0)));
    println!("subtract_type = {}", subtract_type);
    let subtract_qual: Qual<Rc<Type>> =
        Qual(vec![isin("Num", tgen(0))], subtract_type);
    let subtract_scheme: Scheme = Scheme(vec![star()], subtract_qual);
    let subtract_assump = Assump(id("subtract"), Rc::new(subtract_scheme));

    let class_env = ClassEnv {
        classes: BTreeMap::new(),
        defaults: Vec::new(),
    };
    let assumptions: Vec<Assump> = vec![subtract_assump];
    let mut ti = TI::new();
    let expr = eap(eap(evar("subtract"), echar('x')), eint(3));

    let (expr_pred, expr_type) = ti_expr(&mut ti, &class_env, &assumptions, &expr)?;
    for (i, _pred) in expr_pred.iter().enumerate() {
        println!("expr_pred[{}] = {:?}", i, expr_pred[i]);
    }
    println!("expr_type raw   = {}", expr_type);
    println!("expr_type subst = {}", expr_type.apply(&ti.subst));
    for (i, (v, t)) in ti.subst.0.iter().enumerate() {
        println!("subst[{}] = {} ---> {}", i, v, t);
    }

    println!();

    let mut ce = ClassEnv::new();
    ce.add_class("Eq", &[])?;
    ce.add_class("Show", &[])?;
    ce.add_class("Ord", &[])?;
    ce.add_inst(&[], &isin("Ord", t_int()))?;
    ce.add_inst(&[
        isin("Show", tvar(&a)),
        isin("Show", tvar(&b)),

        ], &isin("Show", tap(tvar(&a), tvar(&b))))?;
    ce.dump();


    Ok(())
}
