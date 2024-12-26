use crate::backend::structure::ast::*;
use std::{collections::HashMap, fmt::format, ops::Index, result};
use ansi_term::Colour::Red;
use super::bigint::fraction::Fraction;

#[derive(Debug, Clone)]
pub struct Env {
    storage: HashMap<String, PrimaryExprReduced>,
    parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            storage: HashMap::new(),
            parent: None,
        }
    }
    pub fn from(store: HashMap<String, PrimaryExprReduced>) -> Self {
        Env {
            storage: store,
            parent: None,
        }
    }
    fn new_with_parent(par: Box<Env>) -> Self {
        Env {
            storage: HashMap::new(),
            parent: Some(par),
        }
    }
    pub fn get(&self, name: &str) -> Option<&PrimaryExprReduced> {
        match self.storage.get(name) {
            Some(expr) => Some(expr),
            None => self.parent.as_ref().and_then(|parent| parent.get(name)),
        }
    }
    pub fn set(&mut self, name: String, pexpr: PrimaryExprReduced) {
        self.storage.insert(name, pexpr);
    }

    pub fn clear(&mut self) {
        self.storage.clear();
    }
}

pub fn interpret(tu: TransUnit, env: &mut Env) {
    for stmt in tu.block.stmts {
        interpret_stmt(&stmt, env);
    }
}

fn interpret_block(block: &Block, env: &mut Env) {
    // println!("interpret_block: {:?}", env);
    for stmt in &block.stmts {
        interpret_stmt(stmt, env);
    }
}

fn interpret_stmt(stmt: &Stmt, env: &mut Env) {
    // println!("interpret_stmt: {:?}", env);
    match stmt {
        Stmt::Expr(expr) => {
            let result = match eval_expr(&expr, env) {
                Ok(result) => result,
                Err(err) => {
                    println!("{}: {}", Red.paint("Error"), err);
                    return;
                }
            };
            println!("{}", result.to_string());
        }
        Stmt::Decl(decl) => {
            match decl {
                Decl::Var(var) => {
                    let result = match eval_expr(&var.expr, env) {
                        Ok(result) => result,
                        Err(err) => {
                            println!("{}: {}", Red.paint("Error"), err);
                            return;
                        }
                    };
                    env.set(var.name.clone(), result.clone());
                }
                Decl::Const(const_decl) => {
                    let result = match eval_expr(&const_decl.expr, env) {
                        Ok(result) => result,
                        Err(err) => {
                            println!("{}: {}", Red.paint("Error"), err);
                            return;
                        }
                    };
                    env.set(const_decl.name.clone(), result);
                }
            }
        }
        Stmt::Assign(assign) => {
            let result = match eval_expr(&assign.rhs, env) {
                Ok(result) => result,
                Err(err) => {
                    println!("{}: {}", Red.paint("Error"), err);
                    return;
                }
            };
            env.set(assign.lhs.clone(), result);
        }
        Stmt::If(if_stmt) => {
            let cond = match eval_expr(&if_stmt.cond, env) {
                Ok(cond) => cond,
                Err(err) => {
                    println!("{}: {}", Red.paint("Error"), err);
                    return;
                }
            };
            if let PrimaryExprReduced::Boolean(true) = cond {
                interpret_block(&if_stmt.then, env);
            } else if let Some(els) = &if_stmt.els {
                interpret_block(els, env);
            }
        }
        Stmt::While(while_stmt) => {
            while let Ok(PrimaryExprReduced::Boolean(true)) = eval_expr(&while_stmt.cond, env) {
                interpret_block(&while_stmt.body, env);
            }
        }
        Stmt::For(for_stmt) => {
            interpret_stmt(&for_stmt.init, env);
            while let Ok(PrimaryExprReduced::Boolean(true)) = eval_expr(&for_stmt.cond, env) {
                interpret_block(&for_stmt.body, env);
                interpret_stmt(&for_stmt.post, env);
            }
        }
        Stmt::Return(return_stmt) => {
            let result = eval_expr(&return_stmt.expr, env);
            println!("Return: {:?}", result);
        }
    }
}

fn eval_expr(expr: &Expr, env: &mut Env) -> Result<PrimaryExprReduced, String> {
    // println!("eval_expr: {:?}", env);
    match expr {
        Expr::Primary(primary) => eval_primary(primary, env),
        Expr::Prefix(prefix) => eval_prefix(prefix, env),
        Expr::Infix(infix) => eval_infix(infix, env),
        Expr::Postfix(postfix) => eval_postfix(postfix, env),
        Expr::Reduced(reduced) => eval_reduced(reduced, env),
    }
}

fn eval_expr_short(expr: &Expr_short, env: &mut Env) -> Result<PrimaryExprReduced_short, String> {
    // println!("eval_expr_short: {:?}", env);
    match expr {
        Expr_short::PrimaryShort(primaryshort) => eval_primary_short(primaryshort, env),
        Expr_short::Prefix(prefix) => eval_prefix_short(prefix, env),
        Expr_short::Infix(infix) => eval_infix_short(infix, env),
        Expr_short::Postfix(postfix) => eval_postfix_short(postfix, env),
        Expr_short::Reduced(reduced) => eval_reduced_short(reduced, env),
    }
}

fn eval_primary(primary: &PrimaryExpr, env: &mut Env) -> Result<PrimaryExprReduced, String> {
    // println!("eval_primary: {:?}", env);
    match primary {
        PrimaryExpr::Ident(ident) => {
            match env.get(ident) {
                Some(expr) => Ok(expr.clone()),
                None => Err(format!("Variable {} not found", ident)),
            }
        },
        PrimaryExpr::Integer(int) => Ok(PrimaryExprReduced::Integer(int.clone())),
        PrimaryExpr::Fraction(num, denom) => {
            let num = match eval_expr_to_reduced(env, num) {
                Ok(value) => value,
                Err(value) => return Err(value),
            };
            let denom = match eval_expr_to_reduced(env, denom) {
                Ok(value) => value,
                Err(value) => return Err(value),
            };
            let num_clone = num.clone();
            let denom_clone = denom.clone();
            return match (num, denom) {
                (PrimaryExprReduced_short::Integer(num), PrimaryExprReduced_short::Integer(denom)) => 
                    Ok(PrimaryExprReduced::Fraction(num.fraction(denom))),
                (PrimaryExprReduced_short::Integer(num), PrimaryExprReduced_short::Fraction(denom)) => 
                    Ok(PrimaryExprReduced::Fraction(num / denom)),
                (PrimaryExprReduced_short::Fraction(num), PrimaryExprReduced_short::Integer(denom)) => 
                    Ok(PrimaryExprReduced::Fraction(num / denom)),
                (PrimaryExprReduced_short::Fraction(num), PrimaryExprReduced_short::Fraction(denom)) =>
                    Ok(PrimaryExprReduced::Fraction(num / denom)),
                _ => return Err(format!("Invalid fraction: {:?}/{:?}", num_clone, denom_clone)),
            };
        },
        PrimaryExpr::Vector(vec) => Ok(PrimaryExprReduced::Vector(vec.clone())),
        PrimaryExpr::Matrix(mat) => Ok(PrimaryExprReduced::Matrix(mat.clone())),
        PrimaryExpr::Expr(expr) => eval_expr(expr, env),
        PrimaryExpr::Boolean(bool) => Ok(PrimaryExprReduced::Boolean(*bool)),
        }
    }

fn eval_primary_short(primaryshort: &PrimaryExpr_short, env: &mut Env) -> Result<PrimaryExprReduced_short, String> {
    // println!("eval_primary_short: {:?}", env);
    match primaryshort {
        PrimaryExpr_short::Ident(ident) => {
            match env.get(ident) {
                Some(expr) => match expr {
                    PrimaryExprReduced::Ident(ident) => Ok(PrimaryExprReduced_short::Ident(ident.clone())),
                    PrimaryExprReduced::Integer(int) => Ok(PrimaryExprReduced_short::Integer(int.clone())),
                    PrimaryExprReduced::Fraction(frac) => Ok(PrimaryExprReduced_short::Fraction(frac.clone())),
                    _ => Err(format!("Unsupported expression: {:?}", expr)),
                },
                None => Err(format!("Variable {} not found", ident)),
            }
        },
        PrimaryExpr_short::Integer(int) => Ok(PrimaryExprReduced_short::Integer(int.clone())),
        PrimaryExpr_short::Fraction(num, denom) => {
            let num = match eval_expr_to_reduced(env, num) {
                Ok(value) => value,
                Err(value) => return Err(value),
            };
            let denom = match eval_expr_to_reduced(env, denom) {
                Ok(value) => value,
                Err(value) => return Err(value),
            };
            let num_clone = num.clone();
            let denom_clone = denom.clone();
            return match (num, denom) {
                (PrimaryExprReduced_short::Integer(num), PrimaryExprReduced_short::Integer(denom)) => 
                    Ok(PrimaryExprReduced_short::Fraction(num.fraction(denom))),
                (PrimaryExprReduced_short::Integer(num), PrimaryExprReduced_short::Fraction(denom)) => 
                    Ok(PrimaryExprReduced_short::Fraction(num / denom)),
                (PrimaryExprReduced_short::Fraction(num), PrimaryExprReduced_short::Integer(denom)) => 
                    Ok(PrimaryExprReduced_short::Fraction(num / denom)),
                (PrimaryExprReduced_short::Fraction(num), PrimaryExprReduced_short::Fraction(denom)) =>
                    Ok(PrimaryExprReduced_short::Fraction(num / denom)),
                _ => return Err(format!("Invalid fraction: {:?}/{:?}", num_clone, denom_clone)),
            };
        },
        PrimaryExpr_short::Expr(expr) => {
            match eval_expr_short(expr, env) {
                Ok(expr_val) => Ok(expr_val),
                Err(err) => return Err(err),
            }
        },
    }
}

fn eval_prefix(prefix: &PrefixExpr, env: &mut Env) -> Result<PrimaryExprReduced, String> {
    // println!("eval_prefix: {:?}", env);
    let expr = match eval_expr(&prefix.expr, env) {
        Ok(expr) => expr,
        Err(err) => return Err(err),
    };
    match prefix.op {
        PrefixOp::Neg => match expr {
            PrimaryExprReduced::Ident(ident) => {
                match env.get(&ident) {
                    Some(expr) => eval_prefix( &PrefixExpr {
                        op: PrefixOp::Neg,
                        expr: Box::new(Expr::Reduced(Box::new(expr.clone()))),
                    }, env),
                    None => Err(format!("Variable {} not found", ident)),
                }
            },
            PrimaryExprReduced::Integer(int) => Ok(PrimaryExprReduced::Integer(-int)),
            PrimaryExprReduced::Fraction(frac) => Ok(PrimaryExprReduced::Fraction(-frac)),
            PrimaryExprReduced::Vector(vec) => Ok(PrimaryExprReduced::Vector(-vec)),
            PrimaryExprReduced::Matrix(mat) => Ok(PrimaryExprReduced::Matrix(-mat)),
            _ => Err("Invalid operation".to_string())
        }
        PrefixOp::Pos => match expr {
                PrimaryExprReduced::Ident(ident) => {
                    match env.get(&ident) {
                        Some(expr) => eval_prefix( &PrefixExpr {
                            op: PrefixOp::Pos,
                            expr: Box::new(Expr::Reduced(Box::new(expr.clone()))),
                        }, env),
                    None => Err(format!("Variable {} not found", ident)),
                }
            },
            PrimaryExprReduced::Integer(int) => Ok(PrimaryExprReduced::Integer(int)),
            PrimaryExprReduced::Fraction(frac) => Ok(PrimaryExprReduced::Fraction(frac)),
            PrimaryExprReduced::Vector(vec) => Ok(PrimaryExprReduced::Vector(vec)),
            PrimaryExprReduced::Matrix(mat) => Ok(PrimaryExprReduced::Matrix(mat)),
            _ => Err("Invalid operation".to_string())
        }
        PrefixOp::Abs => match expr {
            PrimaryExprReduced::Ident(ident) => {
                match env.get(&ident) {
                    Some(expr) => eval_prefix( &PrefixExpr {
                        op: PrefixOp::Abs,
                        expr: Box::new(Expr::Reduced(Box::new(expr.clone()))),
                    }, env),
                    None => Err(format!("Variable {} not found", ident)),
                }
            },
            PrimaryExprReduced::Integer(int) => Ok(PrimaryExprReduced::Integer(int.abs())),
            PrimaryExprReduced::Fraction(frac) => Ok(PrimaryExprReduced::Fraction(frac.abs())),
            _ => Err("Invalid operation".to_string())
        }
    }
}

fn eval_prefix_short(prefix: &PrefixExpr_short, env: &mut Env) -> Result<PrimaryExprReduced_short, String> {
    // println!("eval_prefix_short: {:?}", env);
    let expr = match eval_expr_short(&prefix.expr, env) {
        Ok(expr) => expr,
        Err(err) => return Err(err),
    };
    match prefix.op {
        PrefixOp::Neg => match expr {
            PrimaryExprReduced_short::Ident(ident) => {
                match env.get(&ident) {
                    Some(expr) => eval_prefix_short( &&PrefixExpr_short {
                        op: PrefixOp::Neg,
                        expr: Box::new(Expr_short::Reduced(Box::new(expr.clone().into()))),
                    }, env),
                    None => Err(format!("Variable {} not found", ident)),
                }
            },
            PrimaryExprReduced_short::Integer(int) => Ok(PrimaryExprReduced_short::Integer(-int)),
            PrimaryExprReduced_short::Fraction(frac) => Ok(PrimaryExprReduced_short::Fraction(-frac)),
            // _ => Err("Invalid operation".to_string())
        }
        PrefixOp::Pos => match expr {
            PrimaryExprReduced_short::Ident(ident) => {
                match env.get(&ident) {
                    Some(expr) => eval_prefix_short( &PrefixExpr_short {
                        op: PrefixOp::Pos,
                        expr: Box::new(Expr_short::Reduced(Box::new(expr.clone().into()))),
                    }, env),
                    None => Err(format!("Variable {} not found", ident)),
                }
            },
            PrimaryExprReduced_short::Integer(int) => Ok(PrimaryExprReduced_short::Integer(int)),
            PrimaryExprReduced_short::Fraction(frac) => Ok(PrimaryExprReduced_short::Fraction(frac)),
            // _ => Err("Invalid operation".to_string())
        }
        PrefixOp::Abs => match expr {
            PrimaryExprReduced_short::Ident(ident) => {
                match env.get(&ident) {
                    Some(expr) => eval_prefix_short( &PrefixExpr_short {
                        op: PrefixOp::Abs,
                        expr: Box::new(Expr_short::Reduced(Box::new(expr.clone().into()))),
                    }, env),
                    None => Err(format!("Variable {} not found", ident)),
                }
            },
            PrimaryExprReduced_short::Integer(int) => Ok(PrimaryExprReduced_short::Integer(int.abs())),
            PrimaryExprReduced_short::Fraction(frac) => Ok(PrimaryExprReduced_short::Fraction(frac.abs())),
            // _ => Err("Invalid operation".to_string())
        }
    }
}

fn eval_expr_to_reduced(env: &mut Env, expr_short: &Box<Expr_short>) -> Result<PrimaryExprReduced_short, String> {
    // println!("eval_expr_to_reduced: {:?}", env);
    eval_expr_short(expr_short, env)
}

fn eval_infix(infix: &InfixExpr, env: &mut Env) -> Result<PrimaryExprReduced, String> {
    // println!("eval_infix: {:?}", env);
    let lhs = match eval_expr(&infix.lhs, env) {
        Ok(lhs) => lhs,
        Err(err) => return Err(err),
    };
    let rhs = match eval_expr(&infix.rhs, env) {
        Ok(rhs) => rhs,
        Err(err) => return Err(err),
    };
    match (lhs, rhs) {
        (PrimaryExprReduced::Ident(ident), rhs) | (rhs, PrimaryExprReduced::Ident(ident)) => {
            match env.get(&ident) {
                Some(expr) => eval_infix(&InfixExpr { 
                    lhs: Box::new(Expr::Reduced(Box::new(expr.clone()))), 
                    op: infix.op.clone(), 
                    rhs: Box::new(Expr::Reduced(Box::new(rhs.clone())))
                }, env),
                None => Err(format!("Variable {} not found", ident)),
            }
        }
        (PrimaryExprReduced::Integer(lhs), PrimaryExprReduced::Integer(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExprReduced::Integer(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExprReduced::Integer(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExprReduced::Integer(lhs * rhs)),
            InfixOp::Div0 => Ok(PrimaryExprReduced::Integer(lhs / rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Fraction(
                Fraction::new(lhs, rhs)
            )),
            InfixOp::Mod => Ok(PrimaryExprReduced::Integer(lhs % rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `/`, `//`, `%`"))
        },
        (PrimaryExprReduced::Fraction(lhs), PrimaryExprReduced::Fraction(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExprReduced::Fraction(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExprReduced::Fraction(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExprReduced::Fraction(lhs * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Fraction(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `/`"))
        },
        (PrimaryExprReduced::Vector(lhs), PrimaryExprReduced::Vector(rhs)) => match infix.op {
            InfixOp::Add => {
            if lhs.len() == rhs.len() {
                Ok(PrimaryExprReduced::Vector(lhs + rhs))
            } else {
                Err(format!("Invalid operation, vectors must be of the same length"))
            }
            }
            InfixOp::Sub => {
            if lhs.len() == rhs.len() {
                Ok(PrimaryExprReduced::Vector(lhs - rhs))
            } else {
                Err(format!("Invalid operation, vectors must be of the same length"))
            }
            }
            InfixOp::Mul => {
            if lhs.len() == rhs.len() {
                Ok(PrimaryExprReduced::Integer(lhs * rhs))
            } else {
                Err(format!("Invalid operation, vectors must be of the same length"))
            }
            }
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`"))
        },
        (PrimaryExprReduced::Matrix(lhs), PrimaryExprReduced::Matrix(rhs)) => match infix.op {
            InfixOp::Add => {
            if lhs.rows() == rhs.rows() && lhs.cols() == rhs.cols() {
                Ok(PrimaryExprReduced::Matrix(lhs + rhs))
            } else {
                Err(format!("Invalid operation, matrices must be of the same dimensions"))
            }
            }
            InfixOp::Sub => {
            if lhs.rows() == rhs.rows() && lhs.cols() == rhs.cols() {
                Ok(PrimaryExprReduced::Matrix(lhs - rhs))
            } else {
                Err(format!("Invalid operation, matrices must be of the same dimensions"))
            }
            }
            InfixOp::Mul => {
            if lhs.cols() == rhs.rows() {
                Ok(PrimaryExprReduced::Matrix(lhs * rhs))
            } else {
                Err(format!("Invalid operation, matrix A columns must match matrix B rows"))
            }
            }
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`"))
        },
        (PrimaryExprReduced::Integer(lhs), PrimaryExprReduced::Fraction(rhs)) | (PrimaryExprReduced::Fraction(rhs), PrimaryExprReduced::Integer(lhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExprReduced::Fraction(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExprReduced::Fraction(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExprReduced::Fraction(lhs * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Fraction(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `/`"))
        },
        (PrimaryExprReduced::Integer(lhs), PrimaryExprReduced::Vector(rhs)) | (PrimaryExprReduced::Vector(rhs), PrimaryExprReduced::Integer(lhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Vector(lhs * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        (PrimaryExprReduced::Integer(lhs), PrimaryExprReduced::Matrix(rhs)) | (PrimaryExprReduced::Matrix(rhs), PrimaryExprReduced::Integer(lhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Matrix(lhs * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        (PrimaryExprReduced::Vector(lhs), PrimaryExprReduced::Matrix(rhs)) => match infix.op {
            InfixOp::Mul => {
            if lhs.len() == rhs.rows() {
                Ok(PrimaryExprReduced::Vector(lhs * rhs))
            } else {
                Err(format!("Invalid operation, vector length must match matrix row count"))
            }
            }
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        (PrimaryExprReduced::Matrix(lhs), PrimaryExprReduced::Vector(rhs)) => match infix.op {
            InfixOp::Mul => {
            if lhs.cols() == rhs.len() {
                Ok(PrimaryExprReduced::Vector(lhs * rhs))
            } else {
                Err(format!("Invalid operation, matrix column count must match vector length"))
            }
            }
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        _ => Err("Invalid operation".to_string())
    }
}

fn eval_infix_short(infix: &InfixExpr_short, env: &mut Env) -> Result<PrimaryExprReduced_short, String> {
    // println!("eval_infix_short: {:?}", env);
    let lhs = match eval_expr_short(&infix.lhs, env) {
        Ok(lhs) => lhs,
        Err(err) => return Err(err),
    };
    let rhs = match eval_expr_short(&infix.rhs, env) {
        Ok(rhs) => rhs,
        Err(err) => return Err(err),
    };
    return match (lhs, rhs) {
        // Ident
        (PrimaryExprReduced_short::Ident(lhs), PrimaryExprReduced_short::Ident(rhs)) => {
            let lhs_val = match env.get(&lhs) {
                Some(lhs_val) => lhs_val,
                None => return Err(format!("Variable `{}` not found", lhs)),
            };
            let rhs_val = match env.get(&rhs) {
                Some(rhs_val) => rhs_val,
                None => return Err(format!("Variable `{}` not found", rhs)),
            };
            let lhs = match lhs_val {
                PrimaryExprReduced::Ident(ident) => PrimaryExprReduced_short::Ident(ident.clone()),
                PrimaryExprReduced::Integer(int) => PrimaryExprReduced_short::Integer(int.clone()),
                PrimaryExprReduced::Fraction(lhs) => PrimaryExprReduced_short::Fraction(lhs.clone()),
                _ => return Err(format!("Unsupported Variable type"))
            };
            let rhs = match rhs_val {
                PrimaryExprReduced::Ident(ident) => PrimaryExprReduced_short::Ident(ident.clone()),
                PrimaryExprReduced::Integer(int) => PrimaryExprReduced_short::Integer(int.clone()),
                PrimaryExprReduced::Fraction(rhs) => PrimaryExprReduced_short::Fraction(rhs.clone()),
                _ => return Err(format!("Unsupported Variable type"))
            };
            eval_infix_short(&InfixExpr_short {
                lhs: Box::new(Expr_short::Reduced(Box::new(lhs))),
                rhs: Box::new(Expr_short::Reduced(Box::new(rhs))),
                op: infix.op.clone(),
            }, env)
        }
        (PrimaryExprReduced_short::Ident(lhs), rhs) | (rhs, PrimaryExprReduced_short::Ident(lhs)) => {
            let lhs_val = match env.get(&lhs) {
                Some(lhs_val) => lhs_val,
                None => return Err(format!("Variable `{}` not found", lhs)),
            };
            let lhs = match lhs_val {
                PrimaryExprReduced::Ident(ident) => PrimaryExprReduced_short::Ident(ident.clone()),
                PrimaryExprReduced::Integer(int) => PrimaryExprReduced_short::Integer(int.clone()),
                PrimaryExprReduced::Fraction(lhs) => PrimaryExprReduced_short::Fraction(lhs.clone()),
                _ => return Err(format!("Unsupported Variable type"))
            };
            eval_infix_short(&InfixExpr_short {
                lhs: Box::new(Expr_short::Reduced(Box::new(lhs))),
                rhs: Box::new(Expr_short::Reduced(Box::new(rhs))),
                op: infix.op.clone(),
            }, env)
        }
        (PrimaryExprReduced_short::Integer(lhs), PrimaryExprReduced_short::Integer(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExprReduced_short::Integer(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExprReduced_short::Integer(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExprReduced_short::Integer(lhs * rhs)),
            InfixOp::Div0 => Ok(PrimaryExprReduced_short::Integer(lhs / rhs)),
            InfixOp::Mod => Ok(PrimaryExprReduced_short::Integer(lhs % rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced_short::Integer(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `//`, `%`, `/`"))
        }
        (PrimaryExprReduced_short::Integer(lhs), PrimaryExprReduced_short::Fraction(rhs)) | (PrimaryExprReduced_short::Fraction(rhs), PrimaryExprReduced_short::Integer(lhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExprReduced_short::Fraction(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExprReduced_short::Fraction(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExprReduced_short::Fraction(lhs * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced_short::Fraction(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `//`"))
        },
        (PrimaryExprReduced_short::Fraction(lhs), PrimaryExprReduced_short::Fraction(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExprReduced_short::Fraction(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExprReduced_short::Fraction(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExprReduced_short::Fraction(lhs * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced_short::Fraction(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `//`"))
        },
    }
}

fn eval_postfix(postfix: &PostfixExpr, env: &mut Env) -> Result<PrimaryExprReduced, String> {
    // println!("eval_postfix: {:?}", env);
    let expr = eval_expr(&postfix.expr, env);
    let expr = match expr {
        Ok(expr) => expr,
        Err(err) => return Err(err),
    };
    match (expr, &postfix.op) {
        (PrimaryExprReduced::Integer(int), PostfixOp::Factorial) => Ok(PrimaryExprReduced::Integer(int.factorial())),
        (PrimaryExprReduced::Matrix(mat), PostfixOp::Transpose) => Ok(PrimaryExprReduced::Matrix(mat.transpose())),
        _ => Err(format!("Invalid operation, expecting `!` for Int, `^T` for Mat"))
    }
}

fn eval_postfix_short(postfix: &PostfixExpr_short, env: &mut Env) -> Result<PrimaryExprReduced_short, String> {
    // println!("eval_postfix_short: {:?}", env);
    let expr = eval_expr_short(&postfix.expr, env);
    let expr = match expr {
        Ok(expr) => expr,
        Err(err) => return Err(err),
    };
    match (expr, &postfix.op) {
        (PrimaryExprReduced_short::Integer(int), PostfixOp::Factorial) => Ok(PrimaryExprReduced_short::Integer(int.factorial())),
        _ => Err(format!("Invalid operation, expecting `!` for Int"))
    }
}

fn eval_reduced(expr: &PrimaryExprReduced, env: &mut Env) -> Result<PrimaryExprReduced, String> {
    todo!()
}

fn eval_reduced_short(expr: &PrimaryExprReduced_short, env: &mut Env) -> Result<PrimaryExprReduced_short, String> {
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::backend::bigint::BigInt;

    use super::*;

    #[test]
    fn test_env_get_set() {
        let mut env = Env::new();
        env.set("x".to_string(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())));
        assert_eq!(env.get("x"), Some(&PrimaryExprReduced::Integer(BigInt::from("42".to_string()))));
    }

    #[test]
    fn test_env_parent() {
        let mut parent_env = Env::new();
        parent_env.set("x".to_string(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())));
        let child_env = Env::new_with_parent(Box::new(parent_env));
        assert_eq!(child_env.get("x"), Some(&PrimaryExprReduced::Integer(BigInt::from("42".to_string()))));
    }

    #[test]
    fn test_eval_expr_integer() {
        let mut env = Env::new();
        let expr = Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("42".to_string()))));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_addition() {
        let mut env = Env::new();
        let expr = Expr::Infix(Box::new(InfixExpr {
                    lhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("40".to_string()))))),
                    rhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("2".to_string()))))),
                    op: InfixOp::Add,
                }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_subtraction() {
        let mut env = Env::new();
        let expr = Expr::Infix(Box::new(InfixExpr {
            lhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("44".to_string()))))),
            rhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("2".to_string()))))),
            op: InfixOp::Sub,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_multiplication() {
        let mut env = Env::new();
        let expr = Expr::Infix(Box::new(InfixExpr {
            lhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("21".to_string()))))),
            rhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("2".to_string()))))),
            op: InfixOp::Mul,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_division() {
        let mut env = Env::new();
        let expr = Expr::Infix(Box::new(InfixExpr {
            lhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("84".to_string()))))),
            rhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("2".to_string()))))),
            op: InfixOp::Div0,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_negation() {
        let mut env = Env::new();
        let expr = Expr::Prefix(Box::new(PrefixExpr {
            expr: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("42".to_string()))))),
            op: PrefixOp::Neg,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExprReduced::Integer(-BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_abs() {
        let mut env = Env::new();
        let expr = Expr::Prefix(Box::new(PrefixExpr {
            expr: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(-BigInt::from("42".to_string()))))),
            op: PrefixOp::Abs,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())));
    }

    // test fraction
    #[test]
    fn test_eval_expr_fraction() {
        let mut env = Env::new();
        let expr = Expr::Primary(Box::new(PrimaryExpr::Fraction(
            Box::new(Expr_short::Infix(Box::new(InfixExpr_short {
            lhs: Box::new(Expr_short::PrimaryShort(PrimaryExpr_short::Integer(BigInt::from("1".to_string())))),
            rhs: Box::new(Expr_short::PrimaryShort(PrimaryExpr_short::Integer(BigInt::from("1".to_string())))),
            op: InfixOp::Sub,
            }))),
            Box::new(Expr_short::Infix(Box::new(InfixExpr_short {
            lhs: Box::new(Expr_short::PrimaryShort(PrimaryExpr_short::Integer(BigInt::from("1".to_string())))),
            rhs: Box::new(Expr_short::PrimaryShort(PrimaryExpr_short::Integer(BigInt::from("1".to_string())))),
            op: InfixOp::Add,
            })))
        )));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExprReduced::Fraction(Fraction::new(BigInt::from("0".to_string()), BigInt::from("2".to_string()))));
    }
}
