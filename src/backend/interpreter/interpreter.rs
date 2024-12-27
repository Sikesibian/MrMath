use crate::backend::structure::ast::*;
use std::{collections::HashMap, fmt::format, ops::Index, result};
use ansi_term::Colour::Red;
use crate::backend::bigint::{
    fraction::Fraction,
    matrix::{
        VectorElement, IntoVectorElement, Vector, Matrix,
    }
};

#[derive(Debug, Clone)]
pub struct Env {
    storage: HashMap<String, StoredVar>,
    parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            storage: HashMap::new(),
            parent: None,
        }
    }
    pub fn from(store: HashMap<String, StoredVar>) -> Self {
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
            Some(var) => Some(&var.expr),
            None => self.parent.as_ref().and_then(|parent| parent.get(name)),
        }
    }
    pub fn set(&mut self, name: String, pexpr: PrimaryExprReduced, _ty: Type) {
        self.storage.insert(name, StoredVar{ ty: _ty, expr: pexpr });
    }

    pub fn check_in(&self, name: &str) -> bool {
        self.storage.contains_key(name)
    }

    pub fn check_type(&self, name: &str, ty: Type) -> bool {
        match self.storage.get(name) {
            Some(var) => var.ty == ty,
            None => false,
        }
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
                    if env.check_in(&var.name) {
                        println!("{}: {}", Red.paint("Error"), format!("Variable `{}` already exists", var.name));
                        return;
                    }
                    let result = match eval_expr(&var.expr, env) {
                        Ok(result) => result,
                        Err(err) => {
                            println!("{}: {}", Red.paint("Error"), err);
                            return;
                        }
                    };
                    env.set(var.name.clone(), result.clone(), Type::Mutable );
                }
                Decl::Const(const_decl) => {
                    if env.check_in(&const_decl.name) {
                        println!("{}: {}", Red.paint("Error"), format!("Constant `{}` already exists", const_decl.name));
                        return;
                    }
                    let result = match eval_expr(&const_decl.expr, env) {
                        Ok(result) => result,
                        Err(err) => {
                            println!("{}: {}", Red.paint("Error"), err);
                            return;
                        }
                    };
                    env.set(const_decl.name.clone(), result, Type::Immutable );
                }
            }
        }
        Stmt::Assign(assign) => {
            if !env.check_in(&assign.name) {
                println!("{}: {}", Red.paint("Error"), format!("Variable/Constant `{}` does not exist", assign.name));
                return;
            }
            if !env.check_type(&assign.name, Type::Mutable) {
                println!("{}: {}", Red.paint("Error"), format!("Constant `{}` is not mutable", assign.name));
                return;
            }
            let result = match eval_expr(&assign.expr, env) {
                Ok(result) => result,
                Err(err) => {
                    println!("{}: {}", Red.paint("Error"), err);
                    return;
                }
            };
            env.set(assign.name.clone(), result, Type::Mutable );
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
        Expr::Reduced(reduced) => {
            println!("eval_reduced: {:?}", env);
            Ok((**reduced).clone()) // eval_reduced(reduced, env),
        }
    }
}

fn eval_expr_short(expr: &Expr_short, env: &mut Env) -> Result<PrimaryExprReduced_short, String> {
    // println!("eval_expr_short: {:?}", expr);
    match expr {
        Expr_short::PrimaryShort(primaryshort) => eval_primary_short(primaryshort, env),
        Expr_short::Prefix(prefix) => eval_prefix_short(prefix, env),
        Expr_short::Infix(infix) => eval_infix_short(infix, env),
        Expr_short::Postfix(postfix) => eval_postfix_short(postfix, env),
        Expr_short::Reduced(reduced) => {
            println!("eval_reduced_short: {:?}", env);
            Ok((**reduced).clone()) // eval_reduced_short(reduced, env),
        }
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
            let num_clone = eval_expr_short(num, env)?;
            let denom_clone = eval_expr_short(denom, env)?;
            return match (num_clone, denom_clone) {
                (PrimaryExprReduced_short::Integer(num), PrimaryExprReduced_short::Integer(denom)) => 
                    Ok(PrimaryExprReduced::Fraction(num.fraction(denom))),
                (PrimaryExprReduced_short::Integer(num), PrimaryExprReduced_short::Fraction(denom)) => 
                    Ok(PrimaryExprReduced::Fraction(num / denom)),
                (PrimaryExprReduced_short::Fraction(num), PrimaryExprReduced_short::Integer(denom)) => 
                    Ok(PrimaryExprReduced::Fraction(num / denom)),
                (PrimaryExprReduced_short::Fraction(num), PrimaryExprReduced_short::Fraction(denom)) =>
                    Ok(PrimaryExprReduced::Fraction(num / denom)),
                _ => return Err(format!("Invalid fraction: {:?}/{:?}", num, denom)),
            };
        },
        PrimaryExpr::Vector(vec) => {
            let mut evaluated_vec = vec![];
            for elem in vec.iter() {
                let evaluated_elem = eval_expr_short(elem, env)?;
                match evaluated_elem {
                    PrimaryExprReduced_short::Ident(s) => {
                        match env.get(&s) {
                            Some(PrimaryExprReduced::Integer(int)) => {
                                evaluated_vec.push(int.clone().into_vector_element());
                            },
                            Some(PrimaryExprReduced::Fraction(vec)) => {
                                evaluated_vec.push(vec.clone().into_vector_element());
                            },
                            _ => return Err(format!("Invalid expression, expected integer or fraction."))
                        }
                    },
                    PrimaryExprReduced_short::Integer(i) => {
                        evaluated_vec.push(i.into_vector_element());
                    }
                    PrimaryExprReduced_short::Fraction(f) => {
                        evaluated_vec.push(f.into_vector_element());
                    }
                }
            }
            Ok(PrimaryExprReduced::Vector(Vector::from(evaluated_vec)))
        },
        PrimaryExpr::Matrix(mat) => {
            let mut evaluated_vec = Vec::new();
            for row in mat.iter() {
                let mut row_vec = Vec::new();
                for elem in row.iter() {
                    let evaluated_elem = eval_expr_short(elem, env)?;
                    match evaluated_elem {
                        PrimaryExprReduced_short::Ident(s) => {
                            match env.get(&s) {
                                Some(PrimaryExprReduced::Integer(int)) => {
                                    row_vec.push(int.clone().into_vector_element());
                                },
                                Some(PrimaryExprReduced::Fraction(vec)) => {
                                    row_vec.push(vec.clone().into_vector_element());
                                },
                                _ => return Err(format!("Invalid expression, expected integer or fraction."))
                            }
                        },
                        PrimaryExprReduced_short::Integer(i) => {
                            row_vec.push(i.into_vector_element());
                        }
                        PrimaryExprReduced_short::Fraction(f) => {
                            row_vec.push(f.into_vector_element());
                        }
                    }
                }
                evaluated_vec.push(row_vec);
            }
            Ok(PrimaryExprReduced::Matrix(Matrix::from(evaluated_vec)))
        },
        PrimaryExpr::Expr(expr) => eval_expr(expr, env),
        PrimaryExpr::Boolean(bool) => Ok(PrimaryExprReduced::Boolean(*bool)),
        }
    }

fn eval_primary_short(primaryshort: &PrimaryExpr_short, env: &mut Env) -> Result<PrimaryExprReduced_short, String> {
    // println!("eval_primary_short: {:?}", primaryshort);
    match primaryshort {
        PrimaryExpr_short::Ident(ident) => {
            match env.get(ident) {
                Some(expr) => {
                    // println!("eval_primary_short: env: {:?}", env);
                    // println!("eval_primary_short: Ident: {:?}", expr);
                    Ok(PrimaryExprReduced_short::from(expr.clone()))
                },
                None => Err(format!("Variable {} not found", ident)),
            }
        },
        PrimaryExpr_short::Integer(int) => Ok(PrimaryExprReduced_short::Integer(int.clone())),
        PrimaryExpr_short::Fraction(num, denom) => {
            let num = match eval_expr_short(num, env) {
                Ok(value) => value,
                Err(value) => return Err(value),
            };
            let denom = match eval_expr_short(denom, env) {
                Ok(value) => value,
                Err(value) => return Err(value),
            };
            let num_clone = num.clone();
            let denom_clone = denom.clone();
            // println!("eval_primary_short: Fraction: num: {:?}", num);
            // println!("eval_primary_short: Fraction: denom: {:?}", denom);
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
                match lhs * rhs {
                    VectorElement::BigInt(res) => Ok(PrimaryExprReduced::Integer(res)),
                    VectorElement::Fraction(res) => Ok(PrimaryExprReduced::Fraction(res)),
                    _ => Err(format!("Invalid operation, vectors must be of the same length"))
                }
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
        (PrimaryExprReduced::Integer(lhs), PrimaryExprReduced::Fraction(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExprReduced::Fraction(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExprReduced::Fraction(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExprReduced::Fraction(lhs * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Fraction(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `/`"))
        },
        (PrimaryExprReduced::Fraction(lhs), PrimaryExprReduced::Integer(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExprReduced::Fraction(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExprReduced::Fraction(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExprReduced::Fraction(lhs * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Fraction(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `/`"))
        },
        (PrimaryExprReduced::Integer(lhs), PrimaryExprReduced::Vector(rhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Vector(VectorElement::BigInt(lhs) * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        (PrimaryExprReduced::Vector(rhs), PrimaryExprReduced::Integer(lhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Vector(VectorElement::BigInt(lhs) * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Vector(rhs / VectorElement::BigInt(lhs))),
            _ => Err(format!("Invalid operation, expecting `*` or `/`"))
        },
        (PrimaryExprReduced::Integer(lhs), PrimaryExprReduced::Matrix(rhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Matrix(VectorElement::BigInt(lhs) * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        (PrimaryExprReduced::Matrix(rhs), PrimaryExprReduced::Integer(lhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Matrix(VectorElement::BigInt(lhs) * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Matrix(rhs / VectorElement::BigInt(lhs))),
            _ => Err(format!("Invalid operation, expecting `*` or `/`"))
        },
        (PrimaryExprReduced::Fraction(lhs), PrimaryExprReduced::Vector(rhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Vector(VectorElement::Fraction(lhs) * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        }, 
        (PrimaryExprReduced::Vector(rhs), PrimaryExprReduced::Fraction(lhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Vector(VectorElement::Fraction(lhs) * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Vector(rhs / VectorElement::Fraction(lhs))),
            _ => Err(format!("Invalid operation, expecting `*` or `/`"))
        },
        (PrimaryExprReduced::Fraction(lhs), PrimaryExprReduced::Matrix(rhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Matrix(VectorElement::Fraction(lhs) * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        (PrimaryExprReduced::Matrix(rhs), PrimaryExprReduced::Fraction(lhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExprReduced::Matrix(VectorElement::Fraction(lhs) * rhs)),
            InfixOp::Div1 => Ok(PrimaryExprReduced::Matrix(rhs / VectorElement::Fraction(lhs))),
            _ => Err(format!("Invalid operation, expecting `*` or `/`"))
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
    // println!("eval_infix_short: {:?}", infix);
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
            eval_infix_short(&InfixExpr_short {
                lhs: Box::new(Expr_short::Reduced(Box::new(PrimaryExprReduced_short::from(lhs_val.clone())))),
                rhs: Box::new(Expr_short::Reduced(Box::new(PrimaryExprReduced_short::from(rhs_val.clone())))),
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
            InfixOp::Div1 => Ok(PrimaryExprReduced_short::Fraction(lhs.fraction(rhs))),
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

#[cfg(test)]
mod tests {
    use crate::backend::bigint::BigInt;

    use super::*;

    #[test]
    fn test_env_get_set() {
        let mut env = Env::new();
        env.set("x".to_string(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())), Type::Mutable );
        assert_eq!(env.get("x"), Some(&PrimaryExprReduced::Integer(BigInt::from("42".to_string()))));
    }

    #[test]
    fn test_env_parent() {
        let mut parent_env = Env::new();
        parent_env.set("x".to_string(), PrimaryExprReduced::Integer(BigInt::from("42".to_string())), Type::Mutable );
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
