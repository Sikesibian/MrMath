use crate::backend::structure::ast::{
    Expr_short,
    PrimaryExpr_short,
};
use crate::backend::bigint::fraction::Fraction;

#[derive(PartialEq, Clone, Debug)]
pub struct LazyFraction {
    numerator: Box<Expr_short>,
    denominator: Box<Expr_short>,
}
