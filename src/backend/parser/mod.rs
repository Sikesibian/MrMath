use crate::backend::structure::ast::*;
use crate::backend::bigint::{BigInt, matrix::{Matrix, Vector}};
use pest_derive::Parser;
use pest::{pratt_parser::PrattParser, Parser, iterators::Pair};

#[derive(Parser)]
#[grammar = "backend/parser/token.pest"]
pub struct MrMathParser {}

lazy_static::lazy_static! {
    static ref MRMATH_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Op, Assoc::*};
        // Precedence : low to high
        PrattParser::new()
        .op(Op::infix(Rule::ADD_OP, Left))
        .op(Op::infix(Rule::MUL_OP, Left))
        .op(Op::infix(Rule::MOD_OP, Left))
        .op(Op::prefix(Rule::PREFIX_OP))
        .op(Op::postfix(Rule::POSTFIX_OP))
    };
}

pub fn parse(input: &str) -> Result<TransUnit, pest::error::Error<Rule>> {
    let mut pairs = MrMathParser::parse(Rule::GRAMMAR, input)?;
    let tu = parse_grammar(pairs.next().unwrap())?;
    Ok(tu)
}

// GRAMMAR = { SOI ~ TRANS_UNIT ~ EOI }
pub fn parse_grammar(pair: Pair<Rule>) -> Result<TransUnit, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::GRAMMAR);
    parse_trans_unit(pair.into_inner().next().unwrap())
}

// TRANS_UNIT = { BLOCK }
pub fn parse_trans_unit(pair: Pair<Rule>) -> Result<TransUnit, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::TRANS_UNIT);
    let block = parse_block(pair.into_inner().next().unwrap())?;
    Ok(TransUnit {
        block: block,
    })
}

// BLOCK = { (STMT ~ NEWLINE) + }
pub fn parse_block(pair: Pair<Rule>) -> Result<Block, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::BLOCK);
    let mut stmts = Vec::new();
    for stmt in pair.into_inner() {
        stmts.push(parse_stmt(stmt)?);
    }
    Ok(Block {
        stmts: stmts,
    })
}

// STMT = { VAR_STMT | EXPR_STMT | PRINT_STMT }
pub fn parse_stmt(pair: Pair<Rule>) -> Result<Stmt, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::STMT);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::VAR_STMT => parse_var_stmt(inner),
        Rule::EXPR_STMT => parse_expr_stmt(inner),
        Rule::PRINT_STMT => parse_print_stmt(inner),
        _ => panic!("Invalid statement"),
    }
}

// VAR_STMT = { VAR ~ WHITE_SPACE* ~ IDENTIFIER ~ WHITE_SPACE* ~ "=" ~ WHITE_SPACE* ~ TYPE ~ WHITE_SPACE* ~ EXPR ~ WHITE_SPACE* ~ ";" }
pub fn parse_var_stmt(pair: Pair<Rule>) -> Result<Stmt, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::VAR_STMT);
    let mut inner = pair.into_inner();
    let _ = inner.next().unwrap();
    let id = inner.next().unwrap();
    // let ty =inner.next().unwrap();
    let expr = inner.next().unwrap();
    Ok(Stmt::Decl(Decl::Var(VarDecl {
        name: id.as_str().to_string(),
        // ty: parse_type(ty)?,
        expr: Box::new(parse_expr(expr)?),
    })))
}

// EXPR_STMT = { EXPR ~ WHITE_SPACE* }
pub fn parse_expr_stmt(pair: Pair<Rule>) -> Result<Stmt, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::EXPR_STMT);
    let expr = pair.into_inner().next().unwrap();
    Ok(Stmt::Expr(parse_expr(expr)?))
}

// PRINT_STMT = { "print" ~ EXPR ~ ";" }
pub fn parse_print_stmt(pair: Pair<Rule>) -> Result<Stmt, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::PRINT_STMT);
    let expr = pair.into_inner().next().unwrap();
    Ok(Stmt::Expr(parse_expr(expr)?))
}

// TYPE = { INT_TYPE | FRAC_TYPE | VECTOR_TYPE | MATRIX_TYPE }
pub fn parse_type(pair: Pair<Rule>) -> Result<Type, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::TYPE);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::INT_TYPE => Ok(Type::Int),
        Rule::FRAC_TYPE => Ok(Type::Frac),
        Rule::VECTOR_TYPE => Ok(Type::Vector),
        Rule::MATRIX_TYPE => Ok(Type::Matrix),
        _ => panic!("Invalid type"),
    }
}

// EXPR = { ATOM_EXPR ~ (INFIX_OPS ~ ATOM_EXPR)*  }
pub fn parse_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::EXPR);
    let mut inner = pair.into_inner();
    let first_expr = parse_atom_expr(inner.next().unwrap())?;
    let mut expr = first_expr;

    while let Some(op) = inner.next() {
        let infix_op = parse_infix_ops(op)?;
        let next_expr = parse_atom_expr(inner.next().unwrap())?;
        expr = Expr::Infix(Box::new(InfixExpr {
            lhs: Box::new(expr),
            op: infix_op,
            rhs: Box::new(next_expr),
        }));
    }

    Ok(expr)
}

// INFIX_OPS = { WHITE_SPACE* ~ INFIX_OP ~ WHITE_SPACE*}
pub fn parse_infix_ops(pair: Pair<Rule>) -> Result<InfixOp, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::INFIX_OPS);
    let inner = pair.into_inner().next().unwrap();
    parse_infix(inner)
}

// ATOM_EXPR = { PREFIX_OPS ~ PRIMARY_EXPR ~ POSTFIX_OPS }
pub fn parse_atom_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::ATOM_EXPR);
    let mut inner = pair.into_inner();
    let prefix_ops = parse_prefix_ops(inner.next().unwrap())?;
    let primary_expr = parse_primary_expr(inner.next().unwrap())?;
    let postfix_ops = parse_postfix_ops(inner.next().unwrap())?;
    let mut expr = Expr::Primary(Box::new(primary_expr));

    for op in postfix_ops {
        expr = Expr::Postfix(Box::new(PostfixExpr {
            expr: Box::new(expr),
            op,
        }));
    }

    for op in prefix_ops.into_iter().rev() {
        expr = Expr::Prefix(Box::new(PrefixExpr {
            op,
            expr: Box::new(expr),
        }));
    }

    Ok(expr)
}

// PRIMARY_EXPR = { IDENTIFIER | INTEGER | FRACTION | VECTOR | MATRIX | "(" ~ WHITE_SPACE* ~ EXPR ~ WHITE_SPACE* ~ ")" }
pub fn parse_primary_expr(pair: Pair<Rule>) -> Result<PrimaryExpr, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::PRIMARY_EXPR);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::IDENTIFIER => parse_identifier(inner),
        Rule::INTEGER => parse_integer(inner),
        Rule::FRACTION => parse_fraction(inner),
        Rule::VECTOR => parse_vector(inner),
        Rule::MATRIX => parse_matrix(inner),
        Rule::EXPR => Ok(PrimaryExpr::Expr(Box::new(parse_expr(inner)?))),
        _ => panic!("Invalid primary expression"),
    }
}

// EXPR_SHORT = { ATOM_EXPR_SHORT ~ (INFIX_OPS ~ ATOM_EXPR_SHORT)*  }
pub fn parse_expr_short(pair: Pair<Rule>) -> Result<Expr_short, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::EXPR_SHORT);
    let mut inner = pair.into_inner();
    let first_expr = parse_atom_expr_short(inner.next().unwrap())?;
    let mut expr = first_expr;

    while let Some(op) = inner.next() {
        let infix_op = parse_infix_ops(op)?;
        let next_expr = parse_atom_expr_short(inner.next().unwrap())?;
        expr = Expr_short::Infix(Box::new(InfixExpr_short {
            lhs: Box::new(expr),
            op: infix_op,
            rhs: Box::new(next_expr),
        }));
    }

    Ok(expr)
}

// ATOM_EXPR_SHORT = { PREFIX_OPS ~ PRIMARY_EXPR ~ POSTFIX_OPS }
pub fn parse_atom_expr_short(pair: Pair<Rule>) -> Result<Expr_short, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::ATOM_EXPR_SHORT);
    let mut inner = pair.into_inner();
    let prefix_ops = parse_prefix_ops(inner.next().unwrap())?;
    let primary_expr = parse_primary_expr_short(inner.next().unwrap())?;
    let postfix_ops = parse_postfix_ops(inner.next().unwrap())?;
    let mut expr = Expr_short::PrimaryShort(primary_expr);
    
    for op in postfix_ops {
        expr = Expr_short::Postfix(Box::new(PostfixExpr_short {
            expr: Box::new(expr),
            op,
        }));
    }

    for op in prefix_ops.into_iter().rev() {
        expr = Expr_short::Prefix(Box::new(PrefixExpr_short {
            op,
            expr: Box::new(expr),
        }));
    }
    
    Ok(expr)
}

// PRIMARY_EXPR_SHORT = { IDENTIFIER | INTEGER | FRACTION | "(" ~ WHITE_SPACE* ~ EXPR_SHORT ~ WHITE_SPACE* ~ ")" }
pub fn parse_primary_expr_short(pair: Pair<Rule>) -> Result<PrimaryExpr_short, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::PRIMARY_EXPR_SHORT);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::IDENTIFIER => Ok(PrimaryExpr_short::Ident(inner.as_str().to_string())),
        Rule::INTEGER => Ok(PrimaryExpr_short::Integer(BigInt::from(inner.as_str().parse::<i64>().unwrap().to_string()))),
        Rule::FRACTION => {
            let mut inner = inner.into_inner();
            let num = parse_expr_short(inner.next().unwrap())?;
            let denom = parse_expr_short(inner.next().unwrap())?;
            Ok(PrimaryExpr_short::Fraction(Box::new(num), Box::new(denom)))
        },
        Rule::EXPR_SHORT => Ok(PrimaryExpr_short::Expr(Box::new(parse_expr_short(inner)?))),
        _ => panic!("Invalid primary expression"),
    }
}

// PREFIX_OPS = { (WHITE_SPACE* ~ PREFIX_OP)* ~ WHITE_SPACE* }
pub fn parse_prefix_ops(pair: Pair<Rule>) -> Result<Vec<PrefixOp>, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::PREFIX_OPS);
    let mut ops = Vec::new();
    for op in pair.into_inner() {
        ops.push(parse_prefix(op)?);
    }
    Ok(ops)
}

// PREFIX_OP = { "abs" | "+" | "-" }
pub fn parse_prefix(pair: Pair<Rule>) -> Result<PrefixOp, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::PREFIX_OP);
    match pair.as_str() {
        "abs" => Ok(PrefixOp::Abs),
        "+" => Ok(PrefixOp::Pos),
        "-" => Ok(PrefixOp::Neg),
        _ => panic!("Invalid prefix operator"),
    }
}

// POSTFIX_OPS = { (WHITE_SPACE* ~ POSTFIX_OP)* }
pub fn parse_postfix_ops(pair: Pair<Rule>) -> Result<Vec<PostfixOp>, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::POSTFIX_OPS);
    let mut ops = Vec::new();
    for op in pair.into_inner() {
        ops.push(parse_postfix(op)?);
    }
    Ok(ops)
}

// POSTFIX_OP = { "!" | "^T" }
pub fn parse_postfix(pair: Pair<Rule>) -> Result<PostfixOp, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::POSTFIX_OP);
    match pair.as_str() {
        "!" => Ok(PostfixOp::Factorial),
        "^T" => Ok(PostfixOp::Transpose),
        _ => panic!("Invalid postfix operator"),
   }
}

// INFIX_OP = { "+" | "-" | "*" | "//" | "/" | "**" | "%" }
pub fn parse_infix(pair: Pair<Rule>) -> Result<InfixOp, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::INFIX_OP);
    match pair.as_str() {
        "+" => Ok(InfixOp::Add),
        "-" => Ok(InfixOp::Sub),
        "*" => Ok(InfixOp::Mul),
        "//" => Ok(InfixOp::Div0),
        "/" => Ok(InfixOp::Div1),
        "**" => Ok(InfixOp::Pow),
        "%" => Ok(InfixOp::Mod),
        _ => panic!("Invalid infix operator"),
    }
}

// IDENTIFIER = { ASCII_ALPHA ~ ASCII_ALPHA_NUM* }
pub fn parse_identifier(pair: Pair<Rule>) -> Result<PrimaryExpr, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::IDENTIFIER);
    Ok(PrimaryExpr::Ident(pair.as_str().to_string()))
}

// INTEGER = { ASCII_DIGIT+ }
fn parse_integer(pair: Pair<Rule>) -> Result<PrimaryExpr, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::INTEGER);
    Ok(PrimaryExpr::Integer(BigInt::from(pair.as_str().parse::<i64>().unwrap().to_string())))
}

// FRACTION = { "Frac" ~ "[" ~ WHITE_SPACE* ~ EXPR_SHORT ~ WHITE_SPACE* ~ "," ~ WHITE_SPACE* ~ EXPR_SHORT ~ WHITE_SPACE* ~ "]" }
fn parse_fraction(pair: Pair<Rule>) -> Result<PrimaryExpr, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::FRACTION);
    let mut inner = pair.into_inner();
    let num = parse_expr_short(inner.next().unwrap())?;
    let denom = parse_expr_short(inner.next().unwrap())?;
    Ok(PrimaryExpr::Fraction(Box::new(num), Box::new(denom)))
}

// MATRIX = { "Mat" ~ "[" ~ MATRIX_ROWS ~ "]" }
fn parse_matrix(pair: Pair<Rule>) -> Result<PrimaryExpr, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::MATRIX);
    let mut inner = pair.into_inner();
    // let _ = inner.next().unwrap();
    let rows = inner.next().unwrap();
    let matrix_rows = parse_matrix_rows(rows)?
        .into_iter()
        .map(|row| row.into_iter().map(|expr| match expr {
            Expr::Primary(expr) => match *expr {
                PrimaryExpr::Integer(bigint) => bigint,
                _ => panic!("Expected integer in matrix row"),
            },
            _ => panic!("Expected integer in matrix row"),
        }).collect())
        .collect();
    Ok(PrimaryExpr::Matrix(Matrix::from_vecvec(matrix_rows)))
}

// MATRIX_ROWS = { VECTOR_ROW ~ ("," ~ VECTOR_ROW)* }
fn parse_matrix_rows(pair: Pair<Rule>) -> Result<Vec<Vec<Expr>>, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::MATRIX_ROWS);
    let mut inner = pair.into_inner();
    let mut vec = Vec::new();
    while let Some(row) = inner.next() {
        vec.push(parse_vector_row(row)?);
    }
    Ok(vec)
}

// VECTOR = { "Vec" ~ VECTOR_ROW }
fn parse_vector(pair: Pair<Rule>) -> Result<PrimaryExpr, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::VECTOR);
    let mut inner = pair.into_inner();
    // let _ = inner.next().unwrap();
    let rows = inner.next().unwrap();
    let exprs = parse_vector_row(rows)?;
    let bigints = exprs.into_iter().map(|expr| match expr {
        Expr::Primary(boxed_expr) => match *boxed_expr {
            PrimaryExpr::Integer(bigint) => bigint,
            _ => panic!("Expected integer in vector row"),
        },
        _ => panic!("Expected integer in vector row"),
    }).collect();
    Ok(PrimaryExpr::Vector(Vector::new(bigints)))
}

// VECTOR_ROW = { "[" ~ INTEGER ~ ("," ~ INTEGER)* ~ "]" }
fn parse_vector_row(pair: Pair<Rule>) -> Result<Vec<Expr>, pest::error::Error<Rule>> {
    assert_eq!(pair.as_rule(), Rule::VECTOR_ROW);
    let mut inner = pair.into_inner();
    // let _ = inner.next().unwrap();
    // println!("parse_vector_row: {:?}", inner);
    let mut vec = Vec::new();
    while let Some(row) = inner.next() {
        // println!("parse_vector_row while: {:?}", row);
        vec.push(Expr::Primary(Box::new(parse_integer(row)?)));
    }
    Ok(vec)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    #[test]
    fn test_parse_integer() {
        let input = "123";
        let pair = MrMathParser::parse(Rule::INTEGER, input).unwrap().next().unwrap();
        let result = parse_integer(pair).unwrap();
        println!("{:#?}", result);
        assert_eq!(result, PrimaryExpr::Integer(BigInt::from("123".to_string())));
    }

    #[test]
    fn test_parse_fraction() {
        let input = "Frac[x*y-1, Frac[es3, 2]]";
        let pair = MrMathParser::parse(Rule::FRACTION, input).unwrap().next().unwrap();
        let result = parse_fraction(pair).unwrap();
        println!("{:#?}", result);
    }

    #[test]
    fn test_parse_vector() {
        let input = "Vec[1,2 ,3]";
        let pair = MrMathParser::parse(Rule::VECTOR, input).unwrap().next().unwrap();
        let result = parse_vector(pair).unwrap();
        println!("{:#?}", result);
        assert_eq!(result, PrimaryExpr::Vector(Vector::new(vec![
            BigInt::from("1".to_string()),
            BigInt::from("2".to_string()),
            BigInt::from("3".to_string()),
        ])));
    }

    #[test]
    fn test_parse_matrix() {
        let input = "Mat[  [1, 2], [3, 4]];";
        let pair = MrMathParser::parse(Rule::MATRIX, input).unwrap().next().unwrap();
        let result = parse_matrix(pair).unwrap();
        println!("{:#?}", result);
        assert_eq!(result, PrimaryExpr::Matrix(Matrix::new(vec![
            Vector::new(vec![
                BigInt::from("1".to_string()),
                BigInt::from("2".to_string()),
            ]),
            Vector::new(vec![
                BigInt::from("3".to_string()),
                BigInt::from("4".to_string()),
            ]),
        ])));
    }

    #[test]
    fn test_parse_expr() {
        let input = "(Vec[ 3, 8] + 1 !) ^T";
        let pair = MrMathParser::parse(Rule::EXPR, input).unwrap().next().unwrap();
        let result = parse_expr(pair).unwrap();
        // Add appropriate assertions based on the expected structure of the parsed expression
        println!("{:#?}", result);
        let input = "(3 + 1 * 8 !) ^T";
        let pair = MrMathParser::parse(Rule::EXPR, input).unwrap().next().unwrap();
        let result = parse_expr(pair).unwrap();
        // Add appropriate assertions based on the expected structure of the parsed expression
        println!("{:#?}", result);
    }

    #[test]
    fn test_parse_stmt() {
        let input = "var x = Vec Vec[1, 2];";
        let pair = MrMathParser::parse(Rule::STMT, input).unwrap().next().unwrap();
        let result = parse_stmt(pair).unwrap();
        println!("{:#?}", result);
    }

    #[test]
    fn test_parse_complex_expr() {
        let input = "Mat[ [1, 2], [3, 4]] * Vec[5, 6] + Frac[7, 8] - (9 + 10) ^T";
        let pair = MrMathParser::parse(Rule::EXPR, input).unwrap().next().unwrap();
        let result = parse_expr(pair).unwrap();
        println!("{:#?}", result);
    }

    #[test]
    fn test_parse_nested_expr() {
        let input = "((1 + 2) * (3 - 4)) / (5 + (6 * 7))";
        let pair = MrMathParser::parse(Rule::EXPR, input).unwrap().next().unwrap();
        let result = parse_expr(pair).unwrap();
        println!("{:#?}", result);
        // Add appropriate assertions based on the expected structure of the parsed expression
    }

    #[test]
    fn test_parse_complex_stmt() {
        let input = "var matrix = Mat[ [1, 2], [3, 4]]; print matrix * Vec[5, 6];";
        let pair = MrMathParser::parse(Rule::BLOCK, input).unwrap().next().unwrap();
        let result = parse_block(pair).unwrap();
        println!("{:#?}", result);
        // Add appropriate assertions based on the expected structure of the parsed block
    }
}
