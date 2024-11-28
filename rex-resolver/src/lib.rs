use std::collections::HashMap;

use rex_ast::{
    ast::{Call, Ctor, Fields, IfThenElse, Lambda, LetIn, NamedFields, UnnamedFields, Var, AST},
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

pub fn resolve(id_dispenser: &mut IdDispenser, scope: &mut Scope, node: AST) -> Result<AST, Error> {
    match node {
        // Comments
        AST::Comment(span, tokens) => Ok(AST::Comment(span, tokens)),

        // Simple literals do not need resolving
        AST::Null(span) => Ok(AST::Null(span)),
        AST::Bool(span, x) => Ok(AST::Bool(span, x)),
        AST::Uint(span, x) => Ok(AST::Uint(span, x)),
        AST::Int(span, x) => Ok(AST::Int(span, x)),
        AST::Float(span, x) => Ok(AST::Float(span, x)),
        AST::String(span, x) => Ok(AST::String(span, x)),

        // Literal lists, tuples, and dictionaries need resolving because they
        // contain sub-expressions
        AST::List(span, x) => resolve_list(id_dispenser, scope, span, x),
        AST::Tuple(span, x) => resolve_tuple(id_dispenser, scope, span, x),
        AST::Dict(span, x) => resolve_dict(id_dispenser, scope, span, x),

        // Expressions
        AST::Var(var) => resolve_var(id_dispenser, scope, var),
        AST::Call(call) => resolve_call(id_dispenser, scope, call),
        AST::Lambda(lam) => resolve_lambda(id_dispenser, scope, lam),
        AST::LetIn(let_in) => resolve_let_in(id_dispenser, scope, let_in),
        AST::IfThenElse(ite) => resolve_ite(id_dispenser, scope, ite),

        // ADT type construction
        AST::Ctor(cons) => resolve_cons(id_dispenser, scope, cons),
    }
}

fn resolve_list(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    span: Span,
    x: Vec<AST>,
) -> Result<AST, Error> {
    Ok(AST::List(
        span,
        x.into_iter()
            .map(|v| resolve(id_dispenser, scope, v))
            .collect::<Result<_, _>>()?,
    ))
}

fn resolve_tuple(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    span: Span,
    x: Vec<AST>,
) -> Result<AST, Error> {
    Ok(AST::Tuple(
        span,
        x.into_iter()
            .map(|v| resolve(id_dispenser, scope, v))
            .collect::<Result<_, _>>()?,
    ))
}

fn resolve_dict(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    span: Span,
    x: Vec<(Var, AST)>,
) -> Result<AST, Error> {
    Ok(AST::Dict(
        span,
        x.into_iter()
            .map(|(k, v)| resolve(id_dispenser, scope, v).map(|v| (k, v)))
            .collect::<Result<_, _>>()?,
    ))
}

fn resolve_var(_id_dispenser: &mut IdDispenser, scope: &mut Scope, var: Var) -> Result<AST, Error> {
    match scope.vars.get(&var.name) {
        Some(id) => Ok(AST::Var(Var::new(var.span, *id, var.name))),
        None => Err(Error::UseOfUndefined(var.clone())),
    }
}

fn resolve_call(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    call: Call,
) -> Result<AST, Error> {
    let base = resolve(id_dispenser, scope, *call.base)?;
    let arg = resolve(id_dispenser, scope, *call.arg)?;
    Ok(AST::Call(Call::new(
        call.span,
        id_dispenser.next(),
        base,
        arg,
    )))
}

fn resolve_lambda(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    lam: Lambda,
) -> Result<AST, Error> {
    let mut new_scope = Scope {
        vars: scope.vars.clone(),
    };
    new_scope.vars.insert(lam.var.name.clone(), lam.var.id);
    Ok(AST::Lambda(Lambda::new(
        lam.span,
        id_dispenser.next(),
        lam.var,
        resolve(id_dispenser, &mut new_scope, *lam.body)?,
    )))
}

fn resolve_let_in(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    let_in: LetIn,
) -> Result<AST, Error> {
    let mut new_scope = Scope {
        vars: scope.vars.clone(),
    };
    new_scope
        .vars
        .insert(let_in.var.name.clone(), let_in.var.id);
    Ok(AST::LetIn(LetIn::new(
        let_in.span,
        id_dispenser.next(),
        let_in.var.clone(),
        resolve(id_dispenser, scope, *let_in.def)?,
        resolve(id_dispenser, &mut new_scope, *let_in.body)?,
    )))
}

fn resolve_ite(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    ite: IfThenElse,
) -> Result<AST, Error> {
    Ok(AST::IfThenElse(IfThenElse::new(
        ite.span,
        id_dispenser.next(),
        resolve(id_dispenser, scope, *ite.cond)?,
        resolve(id_dispenser, scope, *ite.then)?,
        resolve(id_dispenser, scope, *ite.r#else)?,
    )))
}

fn resolve_cons(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    cons: Ctor,
) -> Result<AST, Error> {
    Ok(AST::Ctor(Ctor::new(
        cons.span,
        id_dispenser.next(),
        cons.name,
        resolve_fields(id_dispenser, scope, cons.fields)?,
    )))
}

fn resolve_fields(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    fields: Fields,
) -> Result<Fields, Error> {
    match fields {
        Fields::Named(named_fields) => {
            resolve_named_fields(id_dispenser, scope, named_fields).map(Fields::Named)
        }
        Fields::Unnamed(unnamed_fields) => {
            resolve_unnamed_fields(id_dispenser, scope, unnamed_fields).map(Fields::Unnamed)
        }
    }
}

fn resolve_named_fields(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    named_fields: NamedFields,
) -> Result<NamedFields, Error> {
    Ok(NamedFields {
        span: named_fields.span,
        fields: named_fields
            .fields
            .into_iter()
            .map(|(k, v)| resolve(id_dispenser, scope, v).map(|v| (k, v)))
            .collect::<Result<_, _>>()?,
    })
}

fn resolve_unnamed_fields(
    id_dispenser: &mut IdDispenser,
    scope: &mut Scope,
    named_fields: UnnamedFields,
) -> Result<UnnamedFields, Error> {
    Ok(UnnamedFields {
        span: named_fields.span,
        fields: named_fields
            .fields
            .into_iter()
            .map(|v| resolve(id_dispenser, scope, v))
            .collect::<Result<_, _>>()?,
    })
}
