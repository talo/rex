use std::collections::HashMap;

use crate::resolver::{Id, Variable, IR};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeVarId(u64);

impl TypeVarId {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn inc(&mut self) -> Self {
        let id = self.0;
        self.0 += 1;
        Self(id)
    }
}

impl Default for TypeVarId {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
    String,
    List(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Variable(TypeVarId),
    Union(Vec<Type>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEnv {
    env: HashMap<Id, Type>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            env: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: Id, ty: Type) {
        self.env.insert(id, ty);
    }

    pub fn get(&self, id: &Id) -> Option<&Type> {
        self.env.get(id)
    }
}

// Substitution
#[derive(Clone, Debug, PartialEq)]
pub struct Substitution {
    subs: HashMap<TypeVarId, Type>,
}

impl Default for Substitution {
    fn default() -> Self {
        Self::new()
    }
}

impl Substitution {
    pub fn new() -> Self {
        Substitution {
            subs: HashMap::new(),
        }
    }

    pub fn insert(&mut self, var: TypeVarId, ty: Type) {
        self.subs.insert(var, ty);
    }

    pub fn get(&self, var: &TypeVarId) -> Option<&Type> {
        self.subs.get(var)
    }

    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Variable(v) => {
                if let Some(t) = self.get(v) {
                    self.apply(t)
                } else {
                    ty.clone()
                }
            }
            Type::List(inner) => Type::List(Box::new(self.apply(inner))),
            Type::Function(params, ret) => Type::Function(
                params.iter().map(|p| self.apply(p)).collect(),
                Box::new(self.apply(ret)),
            ),
            _ => ty.clone(),
        }
    }

    pub fn compose(&mut self, other: &Substitution) {
        for (var, ty) in &other.subs {
            self.insert(*var, self.apply(ty));
        }
    }
}

impl TypeEnv {
    // Extend the type environment
    pub fn extend(&self, id: Id, ty: Type) -> Self {
        let mut new_env = self.clone();
        new_env.insert(id, ty);
        new_env
    }
}

pub fn unify(t1: &Type, t2: &Type) -> Result<Substitution, Error> {
    match (t1, t2) {
        (Type::Bool, Type::Bool) => Ok(Substitution::new()),
        (Type::Int, Type::Int) => Ok(Substitution::new()),
        (Type::Float, Type::Float) => Ok(Substitution::new()),
        (Type::String, Type::String) => Ok(Substitution::new()),
        (Type::List(l1), Type::List(l2)) => unify(l1, l2),
        (Type::Function(p1, r1), Type::Function(p2, r2)) => {
            if p1.len() != p2.len() {
                return Err(Error::ArityMismatch(p1.len(), p2.len()));
            }
            let mut s = Substitution::new();
            for (a1, a2) in p1.iter().zip(p2.iter()) {
                let s1 = unify(&s.apply(a1), &s.apply(a2))?;
                s.compose(&s1);
            }
            let s2 = unify(&s.apply(r1), &s.apply(r2))?;
            s.compose(&s2);
            Ok(s)
        }
        (Type::Variable(v), ty) => var_bind(*v, ty),
        (ty, Type::Variable(v)) => var_bind(*v, ty),
        _ => Err(Error::TypeMismatch(t1.clone(), t2.clone())),
    }
}

fn var_bind(var: TypeVarId, ty: &Type) -> Result<Substitution, Error> {
    if let Type::Variable(v) = ty {
        if *v == var {
            return Ok(Substitution::new());
        }
    }
    if occurs_check(var, ty) {
        Err(Error::InfiniteType(var, ty.clone()))
    } else {
        let mut s = Substitution::new();
        s.insert(var, ty.clone());
        Ok(s)
    }
}

fn occurs_check(var: TypeVarId, ty: &Type) -> bool {
    match ty {
        Type::Variable(v) => *v == var,
        Type::List(inner) => occurs_check(var, inner),
        Type::Function(params, ret) => {
            params.iter().any(|p| occurs_check(var, p)) || occurs_check(var, ret)
        }
        _ => false,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    TypeMismatch(Type, Type),
    UnboundVariable(Variable),
    ArityMismatch(usize, usize),
    InfiniteType(TypeVarId, Type),
    UnimplementedIR(IR),
    NoFunctionFound(IR, Vec<Error>),
}

pub struct TypeInferer {
    pub curr_id: TypeVarId,
}

impl Default for TypeInferer {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInferer {
    pub fn new() -> Self {
        TypeInferer {
            curr_id: TypeVarId::new(),
        }
    }

    pub fn infer(
        &mut self,
        env: &mut TypeEnv,
        ir: IR,
        expected: Option<&Type>,
    ) -> Result<(Type, Substitution), Error> {
        match ir {
            IR::Null(_) => Ok((Type::Bool, Substitution::new())), // Assuming Null is of type Bool for simplicity
            IR::Bool(_, _) => Ok((Type::Bool, Substitution::new())),
            IR::Uint(_, _) => Ok((Type::Int, Substitution::new())),
            IR::Float(_, _) => Ok((Type::Float, Substitution::new())),
            IR::String(_, _) => Ok((Type::String, Substitution::new())),

            IR::Variable(var) => {
                if let Some(ty) = env.get(&var.id) {
                    Ok((ty.clone(), Substitution::new()))
                } else {
                    Err(Error::UnboundVariable(var))
                }
            }

            IR::Call(call) => {
                let (base_ty, mut subst1) = self.infer(env, *call.base.clone(), None)?;
                let mut arg_types = Vec::new();
                for arg in &call.args {
                    let (arg_ty, subst2) = self.infer(env, arg.clone(), None)?;
                    subst1.compose(&subst2);
                    arg_types.push(arg_ty);
                }

                let ret_ty = match expected {
                    Some(expected_ty) => expected_ty.clone(),
                    None => Type::Variable(self.curr_id.inc()),
                };

                match base_ty {
                    Type::Union(types) => {
                        let mut errors = vec![];
                        for ty in types {
                            if let Type::Function(params, ret) = ty {
                                if params.len() == call.args.len() {
                                    let mut subst3 = Substitution::new();
                                    let mut success = true;
                                    for (param, arg) in params.iter().zip(&arg_types) {
                                        match unify(param, arg) {
                                            Ok(s) => subst3.compose(&s),
                                            Err(e) => {
                                                success = false;
                                                errors.push(e);
                                                break;
                                            }
                                        }
                                    }
                                    if success {
                                        match unify(&subst3.apply(&ret), &ret_ty) {
                                            Ok(subst4) => {
                                                subst1.compose(&subst3);
                                                subst1.compose(&subst4);
                                                return Ok((subst1.apply(&ret_ty), subst1));
                                            }
                                            Err(e) => errors.push(e),
                                        }
                                    }
                                }
                            }
                        }
                        Err(Error::NoFunctionFound(*call.base, errors))
                    }
                    Type::Function(..) => {
                        let subst3 = unify(
                            &Type::Function(arg_types, Box::new(ret_ty.clone())),
                            &base_ty,
                        )?;
                        subst1.compose(&subst3);
                        Ok((subst1.apply(&ret_ty), subst1))
                    }
                    _ => Err(Error::TypeMismatch(
                        base_ty,
                        Type::Function(arg_types, Box::new(ret_ty)),
                    )),
                }
            }

            IR::Lambda(lambda) => {
                let mut new_env = env.clone();
                let mut param_types = Vec::new();
                for param in &lambda.params {
                    let param_ty = Type::Variable(self.curr_id.inc());
                    new_env.insert(param.id, param_ty.clone());
                    param_types.push(param_ty);
                }
                let (body_ty, subst) = self.infer(&mut new_env, *lambda.body, None)?;
                Ok((Type::Function(param_types, Box::new(body_ty)), subst))
            }

            IR::List(elems, _) => {
                let mut elem_ty = Type::Variable(self.curr_id.inc());
                let mut subst = Substitution::new();

                for elem in elems {
                    let (current_elem_ty, s) = self.infer(env, elem, None)?;
                    subst.compose(&s);
                    let unified_ty = unify(&subst.apply(&elem_ty), &subst.apply(&current_elem_ty))?;
                    subst.compose(&unified_ty);
                    elem_ty = subst.apply(&elem_ty);
                }

                let list_type = Type::List(Box::new(subst.apply(&elem_ty)));
                Ok((list_type, subst))
            }

            ir => Err(Error::UnimplementedIR(ir)),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{hmts::Type, lexer::Token, parser::Parser, resolver::Resolver};

    use super::{TypeEnv, TypeInferer};

    #[test]
    fn test_type_inferer() {
        let mut parser = Parser::new(Token::tokenize("test.rex", "true").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut inferer = TypeInferer::new();
        let mut type_env = TypeEnv::new();
        let (ty, subs) = inferer.infer(&mut type_env, ir, None).unwrap();
        dbg!(ty, subs);

        let mut parser = Parser::new(Token::tokenize("test.rex", "(\\x -> x) true").unwrap());
        let mut resolver = Resolver::new();
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();
        let mut inferer = TypeInferer::new();
        let mut type_env = TypeEnv::new();
        let (ty, subs) = inferer.infer(&mut type_env, ir, None).unwrap();
        dbg!(ty, subs);

        let mut parser = Parser::new(Token::tokenize("test.rex", "1 + 42").unwrap());
        let mut resolver = Resolver::new();
        let id_op_add = resolver.inject_builtin("+");
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();

        let mut inferer = TypeInferer::new();
        let mut type_env = TypeEnv::new();
        type_env.insert(
            id_op_add,
            Type::Union(vec![
                Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                Type::Function(vec![Type::Float, Type::Float], Box::new(Type::Float)),
            ]),
        );
        let (ty, subs) = inferer.infer(&mut type_env, ir, None).unwrap();
        dbg!(ty, subs);

        let mut parser = Parser::new(Token::tokenize("test.rex", "1.0 + 3.14").unwrap());
        let mut resolver = Resolver::new();
        let id_op_add = resolver.inject_builtin("+");
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();

        let mut inferer = TypeInferer::new();
        let mut type_env = TypeEnv::new();
        type_env.insert(
            id_op_add,
            Type::Union(vec![
                Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                Type::Function(vec![Type::Float, Type::Float], Box::new(Type::Float)),
            ]),
        );
        let (ty, subs) = inferer.infer(&mut type_env, ir, None).unwrap();
        dbg!(ty, subs);

        let mut parser = Parser::new(Token::tokenize("test.rex", "[]").unwrap());
        let mut resolver = Resolver::new();
        let id_op_add = resolver.inject_builtin("+");
        let id_op_foo = resolver.inject_builtin("foo");
        let ir = resolver.resolve(parser.parse_expr().unwrap()).unwrap();

        let mut inferer = TypeInferer::new();
        let mut type_env = TypeEnv::new();
        type_env.insert(
            id_op_add,
            Type::Union(vec![
                Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
                Type::Function(vec![Type::Float, Type::Float], Box::new(Type::Float)),
            ]),
        );
        type_env.insert(
            id_op_foo,
            Type::Union(vec![
                Type::Function(vec![Type::Int], Box::new(Type::Int)),
                Type::Function(vec![Type::Int], Box::new(Type::Float)),
            ]),
        );
        let (ty, subs) = inferer.infer(&mut type_env, ir, None).unwrap();
        dbg!(ty, subs);
        dbg!(type_env);
    }
}
