use crate::backend::bigint::BigInt;
use std::ops::{Add, Sub, Mul, Div, Neg};
use std::cmp::Ordering;


#[derive(Debug, Clone)]
pub struct Fraction {
    numerator: BigInt,
    denominator: BigInt,
}

impl Fraction {
    pub fn new(numerator: BigInt, denominator: BigInt) -> Self {
        if denominator.is_zero() {
            panic!("Denominator cannot be zero");
        }
        Fraction { numerator, denominator }.reduce()
    }

    pub fn numerator(&self) -> &BigInt {
        &self.numerator
    }

    pub fn denominator(&self) -> &BigInt {
        &self.denominator
    }

    fn reduce(mut self) -> Self {
        let gcd = self.numerator.gcd(&self.denominator);
        self.numerator = self.numerator / gcd.clone();
        self.denominator = self.denominator / gcd;
        if self.numerator.sign == self.denominator.sign {
            self.numerator = self.numerator.abs();
            self.denominator = self.denominator.abs();
        }
        else {
            self.denominator = self.denominator.abs();
            self.numerator = -self.numerator.abs();
        }
        self
    }
}

impl PartialEq<Fraction> for Fraction {
    fn eq(&self, other: &Self) -> bool {
        self.numerator.clone() * other.denominator.clone() == self.denominator.clone() * other.numerator.clone()
    }
}

impl PartialEq<BigInt> for Fraction {
    fn eq(&self, other: &BigInt) -> bool {
        self.numerator.clone() == other.clone() * self.denominator.clone()
    }
}

impl PartialOrd<Fraction> for Fraction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.denominator.clone() == other.denominator.clone() {
            self.numerator.partial_cmp(&other.numerator)
        }
        else {
            let numerator = self.numerator.clone() * other.denominator.clone();
            let other_numerator = other.numerator.clone() * self.denominator.clone();
            numerator.partial_cmp(&other_numerator)
        }
    }
}

impl PartialOrd<BigInt> for Fraction {
    fn partial_cmp(&self, other: &BigInt) -> Option<Ordering> {
        if self.denominator == BigInt::one() {
            self.numerator.partial_cmp(other)
        }
        else {
            let numerator = self.numerator.clone() * other.clone();
            numerator.partial_cmp(&self.denominator)
       }
    }
}

impl Eq for Fraction {}

impl Add for Fraction {
    type Output = Fraction;

    fn add(self, other: Fraction) -> Fraction {
        let numerator = self.numerator.clone() * other.denominator.clone() + other.numerator.clone() * self.denominator.clone();
        let denominator = self.denominator.clone() * other.denominator.clone();
        Fraction::new(numerator, denominator)
    }
}

impl Sub for Fraction {
    type Output = Fraction;

    fn sub(self, other: Fraction) -> Fraction {
        let numerator = self.numerator.clone() * other.denominator.clone() - other.numerator.clone() * self.denominator.clone();
        let denominator = self.denominator.clone() * other.denominator.clone();
        Fraction::new(numerator, denominator)
    }
}

impl Mul for Fraction {
    type Output = Fraction;

    fn mul(self, other: Fraction) -> Fraction {
        let numerator = self.numerator * other.numerator;
        let denominator = self.denominator * other.denominator;
        Fraction::new(numerator, denominator)
    }
}

impl Div for Fraction {
    type Output = Fraction;

    fn div(self, other: Fraction) -> Fraction {
        let numerator = self.numerator * other.denominator;
        let denominator = self.denominator * other.numerator;
        Fraction::new(numerator, denominator)
    }
}

impl Add<BigInt> for Fraction {
    type Output = Fraction;

    fn add(self, other: BigInt) -> Fraction {
        let numerator = self.numerator + other * self.denominator.clone();
        Fraction::new(numerator, self.denominator)
    }
}

impl Sub<BigInt> for Fraction {
    type Output = Fraction;

    fn sub(self, other: BigInt) -> Fraction {
        let numerator = self.numerator - other * self.denominator.clone();
        Fraction::new(numerator, self.denominator)
    }
}

impl Mul<BigInt> for Fraction {
    type Output = Fraction;

    fn mul(self, other: BigInt) -> Fraction {
        let numerator = self.numerator * other;
        Fraction::new(numerator, self.denominator)
    }
}

impl Div<BigInt> for Fraction {
    type Output = Fraction;

    fn div(self, other: BigInt) -> Fraction {
        let denominator = self.denominator * other;
        Fraction::new(self.numerator, denominator)
    }
}

impl Add<Fraction> for BigInt {
    type Output = Fraction;

    fn add(self, other: Fraction) -> Fraction {
        other + self
    }
}

impl Sub<Fraction> for BigInt {
    type Output = Fraction;

    fn sub(self, other: Fraction) -> Fraction {
        Fraction::new(self * other.denominator.clone() - other.numerator, other.denominator)
    }
}

impl Mul<Fraction> for BigInt {
    type Output = Fraction;

    fn mul(self, other: Fraction) -> Fraction {
        other * self
    }
}

impl Div<Fraction> for BigInt {
    type Output = Fraction;

    fn div(self, other: Fraction) -> Fraction {
        Fraction::new(self * other.denominator.clone(), other.numerator)
    }
}

impl Neg for Fraction {
    type Output = Fraction;

    fn neg(self) -> Fraction {
        Fraction::new(-self.numerator, self.denominator)
    }
}

impl From<BigInt> for Fraction {
    fn from(value: BigInt) -> Self {
        Fraction::new(value, BigInt::from("1".to_string()))
    }    
}

impl Fraction {
    pub fn to_string(&self) -> String {
        if self.numerator.sign {
            // format!("-Frac[{},{}]", self.numerator.clone().abs().to_string(), self.denominator.to_string())
            if self.denominator == BigInt::one() {
                format!("-{}", self.numerator.clone().abs().to_string())
            } else {
                format!("-{}/{}", self.numerator.clone().abs().to_string(), self.denominator.to_string())
            }
        }
        else {
            // format!("Frac[{},{}]", self.numerator.to_string(), self.denominator.to_string())
            if self.denominator == BigInt::one() {
                format!("{}", self.numerator.clone().abs().to_string())
            } else {
                format!("{}/{}", self.numerator.to_string(), self.denominator.to_string())
            }
        }
    }

    pub fn abs(&self) -> Fraction {
        Fraction::new(self.numerator.clone().abs(), self.denominator.clone().abs())
    }

    pub fn zero() -> Fraction {
        Fraction::new(BigInt::zero(), BigInt::one())
    }

    pub fn one() -> Fraction {
        Fraction::new(BigInt::one(), BigInt::one())
    }

    pub fn is_zero(&self) -> bool {
        self.numerator == BigInt::zero()
    }

    pub fn pow(&self, exponent: BigInt) -> Fraction {
        if exponent == BigInt::zero() {
            Fraction::one()
        }
        else if exponent == BigInt::one() {
            self.clone()
        }
        else if exponent.sign {
            Fraction::new(self.numerator.clone().pow(exponent.clone().abs()), self.denominator.clone().pow(exponent.clone().abs()))
        }
        else {
            Fraction::new(
                self.numerator.clone().pow(exponent.clone()),
                self.denominator.clone().pow(exponent.clone())
            )
        }
    }
}