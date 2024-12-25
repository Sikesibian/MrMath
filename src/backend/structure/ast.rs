use crate::backend::bigint::{BigInt, fraction::Fraction, matrix::{Matrix, Vector}};
use crate::backend::interpreter::*;

#[derive(PartialEq, Clone, Debug)]
pub struct TransUnit {
    pub block: Block,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Expr(Expr),
    Decl(Decl),
    Assign(Assign),
    If(If),
    While(While),
    For(For),
    Return(Return),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Primary(Box<PrimaryExpr>),
    Prefix(Box<PrefixExpr>),
    Infix(Box<InfixExpr>),
    Postfix(Box<PostfixExpr>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr_short {
    PrimaryShort(PrimaryExpr_short),
    Prefix(Box<PrefixExpr>),
    Infix(Box<InfixExpr>),
    Postfix(Box<PostfixExpr>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum PrimaryExpr {
    Ident(String),
    Integer(BigInt),
    Fraction(Fraction),
    Vector(Vector),
    Matrix(Matrix),
    Expr(Box<Expr>),
    Boolean(bool),
}

#[derive(PartialEq, Clone, Debug)]
pub enum PrimaryExpr_short {
    Ident(String),
    Integer(BigInt),
    Fraction(Fraction),
    Boolean(bool),
}

impl PrimaryExpr {
    pub fn get_type(&self) -> Type {
        match self {
            PrimaryExpr::Integer(_) => Type::Int,
            PrimaryExpr::Fraction(_) => Type::Frac,
            PrimaryExpr::Vector(_) => Type::Vector,
            PrimaryExpr::Matrix(_) => Type::Matrix,
            PrimaryExpr::Expr(_) => panic!("PrimaryExpr::Expr should not be used"),
            PrimaryExpr::Boolean(_) => panic!("PrimaryExpr::Boolean should not be used temporarily"),
            PrimaryExpr::Ident(_) => panic!("PrimaryExpr::Ident should not be used")
        }
    }

    pub fn as_int(&self) -> BigInt {
        match self {
            PrimaryExpr::Integer(i) => i.clone(),
            _ => panic!("PrimaryExpr::as_int should not be used")
        }
    }

    pub fn as_frac(&self) -> Fraction {
        match self {
            PrimaryExpr::Fraction(f) => f.clone(),
            _ => panic!("PrimaryExpr::as_frac should not be used")
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            PrimaryExpr::Ident(s) => s.clone(),
            PrimaryExpr::Integer(i) => i.to_string(),
            PrimaryExpr::Fraction(f) => f.to_string(),
            PrimaryExpr::Vector(v) => v.to_string(),
            PrimaryExpr::Matrix(m) => m.to_string(),
            PrimaryExpr::Expr(_) => panic!("PrimaryExpr::Expr should not be used"),
            PrimaryExpr::Boolean(_) => panic!("PrimaryExpr::Boolean should not be used temporarily")
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum PrefixOp {
    Neg,
    Pos,
    Abs
}

#[derive(PartialEq, Clone, Debug)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div0,
    Pow,
    Mod,
    Div1,
    // Eq,
    // Ne,
    // Gt,
    // Lt,
    // Ge,
    // Le,
    // And,
    // Or,
    // Xor,
    // Not,
    // LAnd,
    // LOr,
    // LNot,
}

#[derive(PartialEq, Clone, Debug)]
pub enum PostfixOp {
    Factorial,
    Transpose,
}

#[derive(PartialEq, Clone, Debug)]
pub struct PrefixExpr {
    pub op: PrefixOp,
    pub expr: Box<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct InfixExpr {
    pub lhs: Box<Expr>,
    pub op: InfixOp,
    pub rhs: Box<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct PostfixExpr {
    pub expr: Box<Expr>,
    pub op: PostfixOp,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Bool,
    Int,
    Frac,
    Vector,
    Matrix,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Decl {
    Var(VarDecl),
    Const(ConstDecl),
}

#[derive(PartialEq, Clone, Debug)]
pub struct VarDecl {
    pub name: String,
    pub ty: Type,
    pub expr: Box<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ConstDecl {
    pub name: String,
    pub ty: Type,
    pub expr: Box<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Assign {
    pub lhs: String,
    pub rhs: Box<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Block>,
    pub els: Option<Box<Block>>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct While {
    pub cond: Box<Expr>,
    pub body: Box<Block>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct For {
    pub init: Box<Stmt>,
    pub cond: Box<Expr>,
    pub post: Box<Stmt>,
    pub body: Box<Block>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Return {
    pub expr: Box<Expr>,
}
