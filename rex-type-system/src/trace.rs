use rex_ast::expr::{Expr, Var};

use crate::{
    types::{ExprTypeEnv, TypeEnv},
    unify::{apply_subst, Subst},
};

pub fn sprint_subst(subst: &Subst) -> String {
    let mut s = String::new();
    s.push_str("{\n");
    for (k, v) in subst {
        s.push_str(&format!("  {}: {}\n", k, v));
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

pub fn sprint_expr_type_env(env: &ExprTypeEnv) -> String {
    let mut s = String::new();
    s.push_str("{\n");
    for (k, v) in env {
        s.push_str(&format!("  {}: {}\n", k, v));
    }
    s.push('}');
    s
}

pub fn sprint_expr_with_type(expr: &Expr, env: &ExprTypeEnv, subst: Option<&Subst>) -> String {
    match expr {
        Expr::Bool(id, _, b) => format!(
            "{}:{}",
            b,
            env.get(id)
                .and_then(|t| subst.map(|s| apply_subst(t, s)))
                .map(|t| t.to_string())
                .unwrap_or("_".to_string())
        ),
        Expr::Uint(id, _, n) => format!(
            "{}:{}",
            n,
            env.get(id)
                .and_then(|t| subst.map(|s| apply_subst(t, s)))
                .map(|t| t.to_string())
                .unwrap_or("_".to_string())
        ),
        Expr::Int(id, _, n) => format!(
            "{}:{}",
            n,
            env.get(id)
                .and_then(|t| subst.map(|s| apply_subst(t, s)))
                .map(|t| t.to_string())
                .unwrap_or("_".to_string())
        ),
        Expr::Float(id, _, n) => format!(
            "{}:{}",
            n,
            env.get(id)
                .and_then(|t| subst.map(|s| apply_subst(t, s)))
                .map(|t| t.to_string())
                .unwrap_or("_".to_string())
        ),
        Expr::String(id, _, s) => format!(
            "{}:{}",
            s,
            env.get(id)
                .and_then(|t| subst.map(|s| apply_subst(t, s)))
                .map(|t| t.to_string())
                .unwrap_or("_".to_string())
        ),
        Expr::List(id, _, xs) => {
            let mut s = String::new();
            s.push('[');
            for (i, x) in xs.iter().enumerate() {
                s.push('(');
                s.push_str(&sprint_expr_with_type(x, env, subst));
                s.push(')');
                if i + 1 < xs.len() {
                    s.push_str(", ");
                }
            }
            s.push_str("]:");
            s.push_str(
                format!(
                    "{}",
                    env.get(id)
                        .and_then(|t| subst.map(|s| apply_subst(t, s)))
                        .map(|t| t.to_string())
                        .unwrap_or("_".to_string())
                )
                .as_str(),
            );
            s
        }
        Expr::Tuple(id, _, xs) => {
            let mut s = String::new();
            s.push('(');
            for (i, x) in xs.iter().enumerate() {
                s.push('(');
                s.push_str(&sprint_expr_with_type(x, env, subst));
                s.push(')');
                if i + 1 < xs.len() {
                    s.push_str(", ");
                }
            }
            s.push_str("):");
            s.push_str(
                format!(
                    "{}",
                    env.get(id)
                        .and_then(|t| subst.map(|s| apply_subst(t, s)))
                        .map(|t| t.to_string())
                        .unwrap_or("_".to_string())
                )
                .as_str(),
            );
            s
        }
        Expr::Dict(id, _, kvs) => {
            let mut s = String::new();
            s.push('{');
            for (i, (k, v)) in kvs.iter().enumerate() {
                s.push_str(k);
                s.push_str(" = (");
                s.push_str(&sprint_expr_with_type(v, env, subst));
                s.push(')');
                if i + 1 < kvs.len() {
                    s.push_str(", ");
                }
            }
            s.push_str("}:");
            s.push_str(
                format!(
                    "{}",
                    env.get(id)
                        .and_then(|t| subst.map(|s| apply_subst(t, s)))
                        .map(|t| t.to_string())
                        .unwrap_or("_".to_string())
                )
                .as_str(),
            );
            s
        }
        Expr::Var(Var { id, name, .. }) => format!(
            "{}:{}",
            name,
            env.get(id)
                .map(|t| t.to_string())
                .unwrap_or("_".to_string())
        ),
        Expr::App(id, _, g, x) => {
            let mut s = String::new();
            s.push('(');
            s.push_str(&sprint_expr_with_type(g, env, subst));
            s.push_str(") (");
            s.push_str(&sprint_expr_with_type(x, env, subst));
            s.push(')');
            s
        }
        Expr::Lam(id, _, param, body) => {
            let mut s = String::new();
            s.push('λ');
            s.push_str(&sprint_expr_with_type(
                &Expr::Var(param.clone()),
                env,
                subst,
            ));
            s.push_str(" → ");
            s.push_str(&sprint_expr_with_type(body, env, subst));
            s
        }
        Expr::Let(id, _, var, def, body) => {
            let mut s = String::new();
            s.push_str("let ");
            s.push_str(&sprint_expr_with_type(&Expr::Var(var.clone()), env, subst));
            s.push_str(" = ");
            s.push_str(&sprint_expr_with_type(def, env, subst));
            s.push_str(" in ");
            s.push_str(&sprint_expr_with_type(body, env, subst));
            s
        }
        Expr::Ite(id, _, cond, then, r#else) => {
            let mut s = String::new();
            s.push_str("if ");
            s.push_str(&sprint_expr_with_type(cond, env, subst));
            s.push_str(" then ");
            s.push_str(&sprint_expr_with_type(then, env, subst));
            s.push_str(" else ");
            s.push_str(&sprint_expr_with_type(r#else, env, subst));
            s
        }
    }
}
