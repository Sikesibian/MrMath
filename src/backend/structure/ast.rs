use crate::backend::bigint::{BigInt, fraction::Fraction, matrix::{Matrix, Vector}};

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
    Reduced(Box<PrimaryExprReduced>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum PrimaryExprReduced {
    Ident(String),
    Integer(BigInt),
    Fraction(Fraction),
    Vector(Vector),
    Matrix(Matrix),
    Boolean(bool),
}

impl PrimaryExprReduced {
    pub fn to_string(&self) -> String {
        match self {
            PrimaryExprReduced::Ident(s) => s.clone(),
            PrimaryExprReduced::Integer(i) => i.to_string(),
            PrimaryExprReduced::Fraction(f) => f.to_string(),
            PrimaryExprReduced::Vector(v) => v.to_string(),
            PrimaryExprReduced::Matrix(m)=> m.to_string(),
            _ => panic!("PrimaryExprReduced::to_string should not be used"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum PrimaryExpr {
    Ident(String),
    Integer(BigInt),
    Fraction(Box<Expr>, Box<Expr>),
    Vector(Vec<Expr>),
    Matrix(Vec<Vec<Expr>>),
    Expr(Box<Expr>),
    Boolean(bool),
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
pub struct StoredVar {
    pub ty: Type,
    pub expr: PrimaryExprReduced,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Mutable,
    Immutable,
    // Unit,
    // UnitType,
    // Never,
    // Unknown,
    // Any
}

#[derive(PartialEq, Clone, Debug)]
pub enum Decl {
    Var(VarDecl),
    Const(ConstDecl),
}

#[derive(PartialEq, Clone, Debug)]
pub struct VarDecl {
    pub name: String,
    // pub ty: Type,
    pub expr: Box<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ConstDecl {
    pub name: String,
    // pub ty: Type,
    pub expr: Box<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Assign {
    pub name: String,
    pub expr: Box<Expr>,
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
