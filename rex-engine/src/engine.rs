use std::collections::{HashMap, HashSet};

use rex_ast::id::IdDispenser;
use rex_type_system::{
    arrow,
    constraint::{Constraint, ConstraintSystem},
    types::Type,
};

use crate::ftable::Ftable;

pub struct Builder {
    pub ftable: Ftable,
    pub fconstraints: HashMap<String, Constraint>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            ftable: Ftable::with_prelude(),
            fconstraints: Default::default(),
        }
    }

    // pub fn register_fn<F, A, B>(&mut self, id_dispenser: &mut IdDispenser, n: impl ToString, f: F)
    // where
    //     F: CallFn<A, B> + Clone + Sync + Send + 'static,
    // {
    //     let n = n.to_string();
    //     let t = arrow!(F::a_type() => F::b_type());

    //     match self.fconstraints.get(&n) {
    //         None => {
    //             let new_id = id_dispenser.next();
    //             self.fconstraints
    //                 .insert(n.clone(), Constraint::Eq(Type::Var(new_id), t));
    //         }
    //         Some(Constraint::Eq(tid, prev_t)) => {
    //             let mut new_ts = HashSet::new();
    //             new_ts.insert(prev_t.clone());
    //             new_ts.insert(t);

    //             self.fconstraints
    //                 .insert(n.clone(), Constraint::OneOf(tid.clone(), new_ts));
    //         }
    //         Some(Constraint::OneOf(tid, prev_ts)) => {
    //             let mut new_ts = prev_ts.clone();
    //             new_ts.insert(t);

    //             self.fconstraints
    //                 .insert(n.clone(), Constraint::OneOf(tid.clone(), new_ts));
    //         }
    //     }

    //     self.ftable.register_fn(n, f);
    // }

    pub fn build_constraints(self) -> ConstraintSystem {
        let mut constraint_system = ConstraintSystem::new();
        for (_, constraint) in self.fconstraints {
            constraint_system.add_global_constraint(constraint);
        }
        constraint_system
    }
}
