use crate::backend::structure::ast::*;
use std::{collections::HashMap, fmt::format, result};
use super::bigint::fraction::Fraction;

struct Env {
    storage: HashMap<String, PrimaryExpr>,
    parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            storage: HashMap::new(),
            parent: None,
        }
    }
    pub fn from(store: HashMap<String, PrimaryExpr>) -> Self {
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
    pub fn get(&self, name: &str) -> Option<&PrimaryExpr> {
        match self.storage.get(name) {
            Some(expr) => Some(expr),
            None => self.parent.as_ref().and_then(|parent| parent.get(name)),
        }
    }
    pub fn set(&mut self, name: String, pexpr: PrimaryExpr) {
        self.storage.insert(name, pexpr);
    }
}

pub fn interpret(tu: TransUnit) {
    let mut env = Env::new();
    for stmt in tu.block.stmts {
        interpret_stmt(&stmt, &mut env);
    }
}

fn interpret_block(block: &Block, env: &mut Env) {
    for stmt in &block.stmts {
        interpret_stmt(stmt, env);
    }
}

fn interpret_stmt(stmt: &Stmt, env: &mut Env) {
    match stmt {
        Stmt::Expr(expr) => {
            let result = match eval_expr(&expr, env) {
                Ok(result) => result,
                Err(err) => {
                    println!("Error: {}", err);
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
                            println!("Error: {}", err);
                            return;
                        }
                    };
                    env.set(var.name.clone(), result);
                }
                Decl::Const(const_decl) => {
                    let result = match eval_expr(&const_decl.expr, env) {
                        Ok(result) => result,
                        Err(err) => {
                            println!("Error: {}", err);
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
                    println!("Error: {}", err);
                    return;
                }
            };
            env.set(assign.lhs.clone(), result);
        }
        Stmt::If(if_stmt) => {
            let cond = match eval_expr(&if_stmt.cond, env) {
                Ok(cond) => cond,
                Err(err) => {
                    println!("Error: {}", err);
                    return;
                }
            };
            if let PrimaryExpr::Boolean(true) = cond {
                interpret_block(&if_stmt.then, env);
            } else if let Some(els) = &if_stmt.els {
                interpret_block(els, env);
            }
        }
        Stmt::While(while_stmt) => {
            while let Ok(PrimaryExpr::Boolean(true)) = eval_expr(&while_stmt.cond, env) {
                interpret_block(&while_stmt.body, env);
            }
        }
        Stmt::For(for_stmt) => {
            interpret_stmt(&for_stmt.init, env);
            while let Ok(PrimaryExpr::Boolean(true)) = eval_expr(&for_stmt.cond, env) {
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

fn eval_expr(expr: &Expr, env: &mut Env) -> Result<PrimaryExpr, String> {
    match expr {
        Expr::Primary(primary) => eval_primary(primary, env),
        Expr::Prefix(prefix) => eval_prefix(prefix, env),
        Expr::Infix(infix) => eval_infix(infix, env),
        Expr::Postfix(postfix) => eval_postfix(postfix, env),
    }
}

fn eval_expr_short(expr: &Expr_short, env: &mut Env) -> PrimaryExpr_short {
    todo!("Not implemented");
}

fn eval_primary(primary: &PrimaryExpr, env: &mut Env) -> Result<PrimaryExpr, String> {
    match primary {
        PrimaryExpr::Integer(int) => Ok(PrimaryExpr::Integer(int.clone())),
        PrimaryExpr::Fraction(frac) => Ok(PrimaryExpr::Fraction(frac.clone())),
        PrimaryExpr::Vector(vec) => Ok(PrimaryExpr::Vector(vec.clone())),
        PrimaryExpr::Matrix(mat) => Ok(PrimaryExpr::Matrix(mat.clone())),
        PrimaryExpr::Expr(expr) => eval_expr(expr, env),
        PrimaryExpr::Boolean(bool) => Ok(PrimaryExpr::Boolean(*bool)),
        PrimaryExpr::Ident(ident) => todo!("Not implemented")
        }
    }

fn eval_primary_short(primaryshort: &PrimaryExpr_short, env: &mut Env) -> PrimaryExpr_short {
    todo!("Not implemented")
}

fn eval_prefix(prefix: &PrefixExpr, env: &mut Env) -> Result<PrimaryExpr, String> {
    let expr = match eval_expr(&prefix.expr, env) {
        Ok(expr) => expr,
        Err(err) => return Err(err),
    };
    match prefix.op {
        PrefixOp::Neg => match expr {
            PrimaryExpr::Integer(int) => Ok(PrimaryExpr::Integer(-int)),
            PrimaryExpr::Fraction(frac) => Ok(PrimaryExpr::Fraction(-frac)),
            _ => Err("Invalid operation".to_string())
        }
        PrefixOp::Pos => match expr {
            PrimaryExpr::Integer(int) => Ok(PrimaryExpr::Integer(int)),
            PrimaryExpr::Fraction(frac) => Ok(PrimaryExpr::Fraction(frac)),
            _ => Err("Invalid operation".to_string())
        }
        PrefixOp::Abs => match expr {
            PrimaryExpr::Integer(int) => Ok(PrimaryExpr::Integer(int.abs())),
            PrimaryExpr::Fraction(frac) => Ok(PrimaryExpr::Fraction(frac.abs())),
            _ => Err("Invalid operation".to_string())
        }
    }
}

fn eval_infix(infix: &InfixExpr, env: &mut Env) -> Result<PrimaryExpr, String> {
    let lhs = match eval_expr(&infix.lhs, env) {
        Ok(lhs) => lhs,
        Err(err) => return Err(err),
    };
    let rhs = match eval_expr(&infix.rhs, env) {
        Ok(rhs) => rhs,
        Err(err) => return Err(err),
    };
    match (lhs, rhs) {
        (PrimaryExpr::Integer(lhs), PrimaryExpr::Integer(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExpr::Integer(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExpr::Integer(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExpr::Integer(lhs * rhs)),
            InfixOp::Div0 => Ok(PrimaryExpr::Integer(lhs / rhs)),
            InfixOp::Div1 => Ok(PrimaryExpr::Fraction(lhs.fraction(rhs))),
            InfixOp::Mod => Ok(PrimaryExpr::Integer(lhs % rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `/`, `//`, `%`"))
        },
        (PrimaryExpr::Fraction(lhs), PrimaryExpr::Fraction(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExpr::Fraction(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExpr::Fraction(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExpr::Fraction(lhs * rhs)),
            InfixOp::Div1 => Ok(PrimaryExpr::Fraction(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `/`"))
        },
        (PrimaryExpr::Vector(lhs), PrimaryExpr::Vector(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExpr::Vector(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExpr::Vector(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExpr::Integer(lhs * rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`"))
        },
        (PrimaryExpr::Matrix(lhs), PrimaryExpr::Matrix(rhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExpr::Matrix(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExpr::Matrix(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExpr::Matrix(lhs * rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`"))
        },
        (PrimaryExpr::Integer(lhs), PrimaryExpr::Fraction(rhs)) | (PrimaryExpr::Fraction(rhs), PrimaryExpr::Integer(lhs)) => match infix.op {
            InfixOp::Add => Ok(PrimaryExpr::Fraction(lhs + rhs)),
            InfixOp::Sub => Ok(PrimaryExpr::Fraction(lhs - rhs)),
            InfixOp::Mul => Ok(PrimaryExpr::Fraction(lhs * rhs)),
            InfixOp::Div0 => Ok(PrimaryExpr::Fraction(lhs / rhs)),
            _ => Err(format!("Invalid operation, expecting `+`, `-`, `*`, `//`"))
        },
        (PrimaryExpr::Integer(lhs), PrimaryExpr::Vector(rhs)) | (PrimaryExpr::Vector(rhs), PrimaryExpr::Integer(lhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExpr::Vector(lhs * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        (PrimaryExpr::Integer(lhs), PrimaryExpr::Matrix(rhs)) | (PrimaryExpr::Matrix(rhs), PrimaryExpr::Integer(lhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExpr::Matrix(lhs * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        (PrimaryExpr::Vector(lhs), PrimaryExpr::Matrix(rhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExpr::Vector(lhs * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        | (PrimaryExpr::Matrix(lhs), PrimaryExpr::Vector(rhs)) => match infix.op {
            InfixOp::Mul => Ok(PrimaryExpr::Vector(lhs * rhs)),
            _ => Err(format!("Invalid operation, expecting `*`"))
        },
        _ => Err("Invalid operation".to_string())
    }
}

fn eval_postfix(postfix: &PostfixExpr, env: &mut Env) -> Result<PrimaryExpr, String> {
    let expr = eval_expr(&postfix.expr, env);
    let expr = match expr {
        Ok(expr) => expr,
        Err(err) => return Err(err),
    };
    match (expr, &postfix.op) {
        (PrimaryExpr::Integer(int), PostfixOp::Factorial) => Ok(PrimaryExpr::Integer(int.factorial())),
        (PrimaryExpr::Matrix(mat), PostfixOp::Transpose) => Ok(PrimaryExpr::Matrix(mat.transpose())),
        _ => Err(format!("Invalid operation, expecting `!` for Int, `^T` for Mat"))
    }
}

#[cfg(test)]
mod tests {
    use crate::backend::bigint::BigInt;

    use super::*;

    #[test]
    fn test_env_get_set() {
        let mut env = Env::new();
        env.set("x".to_string(), PrimaryExpr::Integer(BigInt::from("42".to_string())));
        assert_eq!(env.get("x"), Some(&PrimaryExpr::Integer(BigInt::from("42".to_string()))));
    }

    #[test]
    fn test_env_parent() {
        let mut parent_env = Env::new();
        parent_env.set("x".to_string(), PrimaryExpr::Integer(BigInt::from("42".to_string())));
        let child_env = Env::new_with_parent(Box::new(parent_env));
        assert_eq!(child_env.get("x"), Some(&PrimaryExpr::Integer(BigInt::from("42".to_string()))));
    }

    #[test]
    fn test_eval_expr_integer() {
        let mut env = Env::new();
        let expr = Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("42".to_string()))));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExpr::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_addition() {
        let mut env = Env::new();
        let expr = Expr::Infix(Box::new(InfixExpr {
                    lhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("40".to_string()))))),
                    rhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("2".to_string()))))),
                    op: InfixOp::Add,
                }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExpr::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_subtraction() {
        let mut env = Env::new();
        let expr = Expr::Infix(Box::new(InfixExpr {
            lhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("44".to_string()))))),
            rhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("2".to_string()))))),
            op: InfixOp::Sub,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExpr::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_multiplication() {
        let mut env = Env::new();
        let expr = Expr::Infix(Box::new(InfixExpr {
            lhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("21".to_string()))))),
            rhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("2".to_string()))))),
            op: InfixOp::Mul,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExpr::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_division() {
        let mut env = Env::new();
        let expr = Expr::Infix(Box::new(InfixExpr {
            lhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("84".to_string()))))),
            rhs: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("2".to_string()))))),
            op: InfixOp::Div0,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExpr::Integer(BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_boolean() {
        let mut env = Env::new();
        let expr = Expr::Primary(Box::new(PrimaryExpr::Boolean(true)));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExpr::Boolean(true));
    }

    #[test]
    fn test_eval_expr_negation() {
        let mut env = Env::new();
        let expr = Expr::Prefix(Box::new(PrefixExpr {
            expr: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(BigInt::from("42".to_string()))))),
            op: PrefixOp::Neg,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExpr::Integer(-BigInt::from("42".to_string())));
    }

    #[test]
    fn test_eval_expr_abs() {
        let mut env = Env::new();
        let expr = Expr::Prefix(Box::new(PrefixExpr {
            expr: Box::new(Expr::Primary(Box::new(PrimaryExpr::Integer(-BigInt::from("42".to_string()))))),
            op: PrefixOp::Abs,
        }));
        assert_eq!(eval_expr(&expr, &mut env).unwrap(), PrimaryExpr::Integer(BigInt::from("42".to_string())));
    }
}
