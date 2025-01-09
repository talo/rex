use std::collections::HashMap;

use rex_ast::{
    expr::{Expr, Var},
    id::{Id, IdDispenser},
};
use rex_lexer::span::Span;

use crate::error::Error;

pub mod error;

#[derive(Clone, Debug)]
pub struct Scope {
    pub vars: HashMap<String, Id>,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

pub fn resolve(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    expr: Expr,
) -> Result<Expr, Error> {
    match expr {
        Expr::Bool(id, span, x) => Ok(Expr::Bool(id, span, x)),
        Expr::Uint(id, span, x) => Ok(Expr::Uint(id, span, x)),
        Expr::Int(id, span, x) => Ok(Expr::Int(id, span, x)),
        Expr::Float(id, span, x) => Ok(Expr::Float(id, span, x)),
        Expr::String(id, span, x) => Ok(Expr::String(id, span, x)),

        Expr::Tuple(id, span, xs) => resolve_tuple(id_dispenser, scope, id, span, xs),
        Expr::Dict(id, span, kvs) => resolve_dict(id_dispenser, scope, id, span, kvs),
        Expr::List(id, span, xs) => resolve_list(id_dispenser, scope, id, span, xs),

        Expr::Var(var) => resolve_var(id_dispenser, scope, var),
        Expr::App(id, span, f, x) => resolve_app(id_dispenser, scope, id, span, *f, *x),
        Expr::Lam(id, span, param, body) => {
            resolve_lam(id_dispenser, scope, id, span, param, *body)
        }
        Expr::Let(id, span, var, def, body) => {
            resolve_let(id_dispenser, scope, id, span, var, *def, *body)
        }
        Expr::Ite(id, span, cond, then, r#else) => {
            resolve_ite(id_dispenser, scope, id, span, *cond, *then, *r#else)
        }
    }
}

fn resolve_tuple(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    id: Id,
    span: Span,
    xs: Vec<Expr>,
) -> Result<Expr, Error> {
    Ok(Expr::Tuple(
        id,
        span,
        xs.into_iter()
            .map(|x| resolve(id_dispenser, scope, x))
            .collect::<Result<_, _>>()?,
    ))
}

fn resolve_dict(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    id: Id,
    span: Span,
    kvs: Vec<(String, Expr)>,
) -> Result<Expr, Error> {
    Ok(Expr::Dict(
        id,
        span,
        kvs.into_iter()
            .map(|(k, v)| resolve(id_dispenser, scope, v).map(|v| (k, v)))
            .collect::<Result<_, _>>()?,
    ))
}

fn resolve_list(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    id: Id,
    span: Span,
    xs: Vec<Expr>,
) -> Result<Expr, Error> {
    Ok(Expr::List(
        id,
        span,
        xs.into_iter()
            .map(|x| resolve(id_dispenser, scope, x))
            .collect::<Result<_, _>>()?,
    ))
}

fn resolve_var(
    _id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    var: Var,
) -> Result<Expr, Error> {
    match scope.vars.get(&var.name) {
        Some(id) => Ok(Expr::Var(Var::new(*id, var.span, var.name))),
        None => Err(Error::UseOfUndefined(var.name)),
    }
}

fn resolve_app(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    id: Id,
    span: Span,
    f: Expr,
    x: Expr,
) -> Result<Expr, Error> {
    let f = resolve(id_dispenser, scope, f)?;
    let x = resolve(id_dispenser, scope, x)?;
    Ok(Expr::App(id, span, Box::new(f), Box::new(x)))
}

fn resolve_lam(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    id: Id,
    span: Span,
    param: Var,
    body: Expr,
) -> Result<Expr, Error> {
    let mut new_scope = Scope {
        vars: scope.vars.clone(),
    };
    new_scope.vars.insert(param.name.clone(), param.id);
    Ok(Expr::Lam(
        id,
        span,
        param,
        Box::new(resolve(id_dispenser, &mut new_scope, body)?),
    ))
}

fn resolve_let(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    id: Id,
    span: Span,
    var: Var,
    def: Expr,
    body: Expr,
) -> Result<Expr, Error> {
    let mut new_scope = Scope {
        vars: scope.vars.clone(),
    };
    new_scope.vars.insert(var.name.clone(), var.id);
    Ok(Expr::Let(
        id,
        span,
        var,
        Box::new(resolve(id_dispenser, scope, def)?),
        Box::new(resolve(id_dispenser, &mut new_scope, body)?),
    ))
}

fn resolve_ite(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    id: Id,
    span: Span,
    cond: Expr,
    then: Expr,
    r#else: Expr,
) -> Result<Expr, Error> {
    Ok(Expr::Ite(
        id,
        span,
        Box::new(resolve(id_dispenser, scope, cond)?),
        Box::new(resolve(id_dispenser, scope, then)?),
        Box::new(resolve(id_dispenser, scope, r#else)?),
    ))
}
