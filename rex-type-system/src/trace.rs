// use rex_ast::expr::{Expr, Var};

use crate::{types::TypeEnv, unify::Subst};

pub fn sprint_subst(subst: &Subst) -> String {
    let mut s = String::new();
    s.push_str("{\n");
    for (k, v) in subst.iter() {
        s.push_str(&format!("  τ{}: {}\n", k, v));
    }
    s.push('}');
    s
}

pub fn sprint_type_env(env: &TypeEnv) -> String {
    let mut s = String::new();
    s.push_str("{\n");
    for (k, v) in env {
        s.push_str(&format!("  {}: {}\n", k, v));
    }
    s.push('}');
    s
}
