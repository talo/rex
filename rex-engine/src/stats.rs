use crate::{error::Error, ftable::Ftable, value::Value, Context};

use eval_metrics::{
    // classification::m_auc as eval_m_auc,
    regression::{corr as eval_pearson_corr, mae as eval_mae, rmse as eval_rmse},
};

use rstats::Vecg;

// spearman_corr
// (trait is on the vector)
pub async fn spearman_corr<S: Send + Sync + 'static>(
    _ctx: &Context,
    _state: &S,
    _ftable: &Ftable<S>,
    args: &[Value],
) -> Result<Value, Error> {
    if args.len() != 2 {
        return Err(Error::Custom {
            error: "spearman_corr function expects 2 arguments: a list of predicted values and a list of true values".to_string(),
            trace: Default::default(),
        });
    }

    let predicted = match &args[0] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "First argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    let true_values = match &args[1] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "Second argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    if predicted.len() != true_values.len() {
        return Err(Error::Custom {
            error: "Lists must be of the same length".to_string(),
            trace: Default::default(),
        });
    }

    let mut o_predicted_values = vec![];
    let mut o_true_values: Vec<f64> = vec![];

    for (vp, vt) in predicted.iter().zip(true_values.iter()) {
        match (vp, vt) {
            (Value::Float(p), Value::Float(t)) => {
                o_predicted_values.push(*p);
                o_true_values.push(*t);
            }
            _ => {
                return Err(Error::Custom {
                    error: "Lists must contain only numbers".to_string(),
                    trace: Default::default(),
                })
            }
        }
    }

    Ok(Value::Float(
        o_predicted_values.spearmancorr(&o_true_values),
    ))
}

// pearson_corr
pub async fn pearson_corr<S: Send + Sync + 'static>(
    _ctx: &Context,
    _state: &S,
    _ftable: &Ftable<S>,
    args: &[Value],
) -> Result<Value, Error> {
    if args.len() != 2 {
        return Err(Error::Custom {
            error: "pearson_corr function expects 2 arguments: a list of predicted values and a list of true values".to_string(),
            trace: Default::default(),
        });
    }

    let predicted = match &args[0] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "First argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    let true_values = match &args[1] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "Second argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    if predicted.len() != true_values.len() {
        return Err(Error::Custom {
            error: "Lists must be of the same length".to_string(),
            trace: Default::default(),
        });
    }

    let mut o_predicted_values = vec![];
    let mut o_true_values: Vec<f64> = vec![];

    for (vp, vt) in predicted.iter().zip(true_values.iter()) {
        match (vp, vt) {
            (Value::Float(p), Value::Float(t)) => {
                o_predicted_values.push(*p);
                o_true_values.push(*t);
            }
            _ => {
                return Err(Error::Custom {
                    error: "Lists must contain only numbers".to_string(),
                    trace: Default::default(),
                })
            }
        }
    }

    Ok(Value::Float(
        eval_pearson_corr(&o_predicted_values, &o_true_values).map_err(|e| Error::Custom {
            error: format!("Failed to eval pearson_corr: {}", e),
            trace: Default::default(),
        })?,
    ))
}

// rmse
pub async fn rmse<S: Send + Sync + 'static>(
    _ctx: &Context,
    _state: &S,
    _ftable: &Ftable<S>,
    args: &[Value],
) -> Result<Value, Error> {
    if args.len() != 2 {
        return Err(Error::Custom {
            error: "rmse function expects 2 arguments: a list of predicted values and a list of true values".to_string(),
            trace: Default::default(),
        });
    }

    let predicted = match &args[0] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "First argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    let true_values = match &args[1] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "Second argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    if predicted.len() != true_values.len() {
        return Err(Error::Custom {
            error: "Lists must be of the same length".to_string(),
            trace: Default::default(),
        });
    }

    let mut o_predicted_values = vec![];
    let mut o_true_values: Vec<f64> = vec![];

    for (vp, vt) in predicted.iter().zip(true_values.iter()) {
        match (vp, vt) {
            (Value::Float(p), Value::Float(t)) => {
                o_predicted_values.push(*p);
                o_true_values.push(*t);
            }
            _ => {
                return Err(Error::Custom {
                    error: "Lists must contain only numbers".to_string(),
                    trace: Default::default(),
                })
            }
        }
    }

    Ok(Value::Float(
        eval_rmse(&o_predicted_values, &o_true_values).map_err(|e| Error::Custom {
            error: format!("Failed to eval rmse: {}", e),
            trace: Default::default(),
        })?,
    ))
}

// m_auc
// requires binary truths
// async fn m_auc<S: StateInterface>(
//     _ctx: &Context,
//     _state: &S,
//     _ftable: &Ftable<S>,
//     args: &[Value],
// ) -> Result<Value, Error> {
//     if args.len() != 2 {
//         return Err(Error::Custom {
//             error: "m_auc function expects 2 arguments: a list of predicted values and a list of true values".to_string(),
//             trace: Default::default(),
//         });
//     }

//     let predicted = match &args[0] {
//         Value::List(l) => l,
//         _ => {
//             return Err(Error::Custom {
//                 error: "First argument must be a list".to_string(),
//                 trace: Default::default(),
//             })
//         }
//     };

//     let true_values = match &args[1] {
//         Value::List(l) => l,
//         _ => {
//             return Err(Error::Custom {
//                 error: "Second argument must be a list".to_string(),
//                 trace: Default::default(),
//             })
//         }
//     };

//     if predicted.len() != true_values.len() {
//         return Err(Error::Custom {
//             error: "Lists must be of the same length".to_string(),
//             trace: Default::default(),
//         });
//     }

//     let mut predicted_values = vec![];
//     let mut true_values = vec![];

//     for (p, t) in predicted.iter().zip(true_values.iter()) {
//         match (p, t) {
//             (Value::Float(p), Value::Float(t)) => {
//                 predicted_values.push(*p);
//                 true_values.push(*t);
//             }
//             _ => {
//                 return Err(Error::Custom {
//                     error: "Lists must contain only numbers".to_string(),
//                     trace: Default::default(),
//                 })
//             }
//         }
//     }

//     Ok(Value::Float(eval_m_auc(&predicted_values, &true_values))?)
// }

// mae
pub async fn mae<S: Send + Sync + 'static>(
    _ctx: &Context,
    _state: &S,
    _ftable: &Ftable<S>,
    args: &[Value],
) -> Result<Value, Error> {
    if args.len() != 2 {
        return Err(Error::Custom {
            error: "mae function expects 2 arguments: a list of predicted values and a list of true values".to_string(),
            trace: Default::default(),
        });
    }

    let predicted = match &args[0] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "First argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    let true_values = match &args[1] {
        Value::List(l) => l,
        _ => {
            return Err(Error::Custom {
                error: "Second argument must be a list".to_string(),
                trace: Default::default(),
            })
        }
    };

    if predicted.len() != true_values.len() {
        return Err(Error::Custom {
            error: "Lists must be of the same length".to_string(),
            trace: Default::default(),
        });
    }

    let mut o_predicted_values = vec![];
    let mut o_true_values = vec![];

    for (p, t) in predicted.iter().zip(true_values.iter()) {
        match (p, t) {
            (Value::Float(p), Value::Float(t)) => {
                o_predicted_values.push(*p);
                o_true_values.push(*t);
            }
            _ => {
                return Err(Error::Custom {
                    error: "Lists must contain only numbers".to_string(),
                    trace: Default::default(),
                })
            }
        }
    }

    Ok(Value::Float(
        eval_mae(&o_predicted_values, &o_true_values).map_err(|e| Error::Custom {
            error: format!("Failed to eval mae: {}", e),
            trace: Default::default(),
        })?,
    ))
}
