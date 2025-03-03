#![allow(warnings)]

use typeclasses::*;
use typeclasses::helpers::*;
use std::rc::Rc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let a = tyvar("a", star());
    let b = tyvar("b", star());

    let x = tyvar("x", kfun(star(), star()));
    let y = tyvar("y", kfun(star(), star()));

    let t1 = tap(tvar(&x), tvar(&a));
    let t2 = tap(tvar(&y), tvar(&b));

    let s = match_(
        &tap(t_list(), tvar(&a)),
        &tap(t_list(), tvar(&b)))?;

    println!("s = {}", s);

    Ok(())
}
