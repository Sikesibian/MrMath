pub mod fraction;
pub mod matrix;
pub mod polynomial;

use std::cmp::Ordering;
use std::ops::{Add, Sub, Mul, Div, Rem, Neg, Not};

use fraction::Fraction;

const BASE: i64 = 10;

#[derive(Debug, Clone)]
pub struct BigInt {
    digits: Vec<i64>,
    sign: bool, // false: positive, true: negative
}

impl BigInt {
    pub fn new(mut digits: Vec<i64>, sign: bool) -> Self {
        while digits.len() > 1 && digits.last() == Some(&0) {
            digits.pop();
        }
        BigInt { digits, sign }
    }

    pub fn to_string(&self) -> String{
        let sign = if self.sign { "-" } else { "" };
        let digits = self.digits.iter().rev().map(|&x| x.to_string()).collect::<Vec<_>>().join("");
        format!("{}{}", sign, digits)
    }
}

impl PartialEq for BigInt {
    fn eq(&self, other: &Self) -> bool {
        (self.digits == other.digits && self.sign == other.sign) ||
        (self.is_zero() && other.is_zero())
    }
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl Eq for BigInt {}

impl PartialOrd for BigInt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.sign != other.sign {
            return Some(if self.sign { Ordering::Less } else { Ordering::Greater });
        }
        if self.digits.len() != other.digits.len() {
            return Some(self.digits.len().cmp(&other.digits.len()));
        }
        Some(self.cmp(other))
    }
}

impl Ord for BigInt {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.sign != other.sign {
            return if self.sign { Ordering::Less } else { Ordering::Greater };
        }
        let len_cmp = self.digits.len().cmp(&other.digits.len());
        if len_cmp != Ordering::Equal {
            return len_cmp;
        }
        for (a, b) in self.digits.iter().rev().zip(other.digits.iter().rev()) {
            let cmp = a.cmp(b);
            if cmp != Ordering::Equal {
                return cmp;
            }
        }
        Ordering::Equal
    }
}

impl From<String> for BigInt {
    fn from(mut s: String) -> Self {
        let mut digits = Vec::new();
        // remove zero
        while s.starts_with('0') && s.len() > 1 {
            s.remove(0);
        }
        for c in s.chars() {
            digits.push(c.to_digit(10).unwrap() as i64);
        }
        digits.reverse();
        BigInt { digits, sign: false }
    }
}

impl Neg for BigInt {
    type Output = Self;
    fn neg(self) -> Self {
        BigInt { digits: self.digits, sign: !self.sign }
    }
}

impl Add for BigInt {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        if self.sign != other.sign {
            if self.digits == other.digits {
                return BigInt::zero();
            }
            return self - (-other);
        }
        let mut result = Vec::new();
        let mut carry = 0;
        let max_len = self.digits.len().max(other.digits.len());

        for i in 0..max_len {
            let a = *self.digits.get(i).unwrap_or(&0);
            let b = *other.digits.get(i).unwrap_or(&0);
            let sum = a + b + carry;
            result.push(sum % BASE);
            carry = sum / BASE;
        }

        if carry > 0 {
            result.push(carry);
        }

        while result.len() > 1 && result.last() == Some(&0) {
            result.pop();
        }

        BigInt { digits: result , sign: self.sign }
    }
}

impl Sub for BigInt {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        if self.sign == other.sign && self.digits == other.digits {
            return BigInt::zero();
        }
        if self.sign != other.sign {
            return self + (-other);
        }
        let mut result = Vec::new();
        let mut borrow = 0;
        let mut sign = self.sign;
        let (larger, smaller) = if self >= other {
            (self, other)
        } else {
            sign = !self.sign;
            (other, self)
        };

        for i in 0..larger.digits.len() {
            let a = *larger.digits.get(i).unwrap_or(&0);
            let b = *smaller.digits.get(i).unwrap_or(&0);
            let mut diff = a - b - borrow;
            if diff < 0 {
                diff += BASE;
                borrow = 1;
            } else {
                borrow = 0;
            }
            result.push(diff);
        }

        while result.len() > 1 && result.last() == Some(&0) {
            result.pop();
        }

        BigInt { digits: result, sign }
    }
}

impl Mul for BigInt {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let mut result = vec![0; self.digits.len() + other.digits.len()];

        for (i, &a) in self.digits.iter().enumerate() {
            let mut carry = 0;
            for (j, &b) in other.digits.iter().enumerate() {
            let sum = result[i + j] + a * b + carry;
            result[i + j] = sum % BASE;
            carry = sum / BASE;
            }
            result[i + other.digits.len()] += carry;
        }

        while result.len() > 1 && result.last() == Some(&0) {
            result.pop();
        }

        BigInt { digits: result, sign: self.sign ^ other.sign }
    }
}

impl Div for BigInt {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        if other.digits.is_empty() || other.is_zero() {
            eprintln!("Warning: Division by zero");
            return BigInt::zero();
        }
        let mut remainder = self.clone().abs();
        let mut quotient = BigInt { digits: vec![], sign: self.sign ^ other.sign };
        let mut divisor = other.abs();
        if remainder < divisor {
            return BigInt { digits: vec![0], sign: self.sign };
        }
        let divisor_len = divisor.digits.len();
        while divisor.digits.len() < remainder.digits.len() {
            divisor.digits.insert(0, 0);
        }
        while divisor.digits.len() >= divisor_len {
            let mut quotient_digit = 0;
            while remainder >= divisor {
                remainder = remainder - divisor.clone();
                quotient_digit += 1;
            }
            quotient.digits.insert(0, quotient_digit);
            divisor.digits.remove(0);
        }
        
        while quotient.digits.len() > 1 && quotient.digits.last() == Some(&0) {
            quotient.digits.pop();
        }

        quotient
    }
}

impl Rem for BigInt {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        if other.digits.is_empty() || other.is_zero() {
            eprintln!("Warning: Division by zero");
            return BigInt::zero();
        }
        if other.sign {
            eprintln!("Warning: The moduli cannot be negative!");
            return BigInt::zero();
        }
        let mut remainder = self.clone().abs();
        let mut divisor = other.clone().abs();
        if remainder < divisor {
            if self.sign && !remainder.is_zero() {
                remainder = other.abs() - remainder;
            }
            return remainder;
        }
        let divisor_len = divisor.digits.len();
        while divisor.digits.len() < remainder.digits.len() {
            divisor.digits.insert(0, 0);
        }
        while divisor.digits.len() >= divisor_len {
            while remainder >= divisor {
                remainder = remainder - divisor.clone();
            }
            divisor.digits.remove(0);
        }

        if self.sign && !remainder.is_zero() {
            remainder = other.abs() - remainder;
        }
        remainder
    }
}

impl Not for BigInt {
    type Output = Self;
    fn not(self) -> Self {
        BigInt { digits: self.digits.iter().map(|x| !x).collect(), sign: !self.sign }
    }
}

impl BigInt {
    pub fn zero() -> Self {
        BigInt { digits: vec![0], sign: false }
    }
    pub fn one() -> Self {
        BigInt { digits: vec![1], sign: false }
    }
}

impl BigInt {
    pub fn fraction(self, other: Self) -> Fraction {
        if other.digits.is_empty() || other.is_zero() {
            eprintln!("Warning: Division by zero");
            return Fraction::zero();
        }
        return Fraction::new(self, other);
    }
    pub fn pow(self, exp: Self) -> Self
    where
        Self: Clone + Mul<Output = Self> + PartialOrd + From<BigInt> + PartialEq,
    {
        let zero = Self::zero();
        let one = Self::one();
        let two = BigInt { digits: vec![2], sign: false };
        if self.is_zero() {
            println!("Cannot raise zero to a negative power");
            return zero;
        }
        if exp < zero {
            println!("Exponent must be non-negative");
            return zero;
        }

        if exp == zero {
            return one;
        }

        let mut base = self;
        let mut exp = exp;
        let mut result = one.clone();

        while exp.clone() > zero {
            if exp.clone() % two.clone() == one {
                result = result * base.clone();
            }
            base = base.clone() * base.clone();
            exp = exp / two.clone();
        }

        result
    }

    pub fn mod_pow(self, exp: u32, modulus: Self) -> Self {
        if modulus.digits.is_empty() || (modulus.digits.len() == 1 && modulus.digits[0] == 0) {
            panic!("Modulus cannot be zero");
        }
        let mut base = self % modulus.clone();
        let mut exp = exp;
        let mut result = BigInt::one() % modulus.clone();
        while exp > 0 {
            if exp % 2 == 1 {
                result = (result * base.clone()) % modulus.clone();
            }
            base = (base.clone() * base) % modulus.clone();
            exp /= 2;
        }
        
        result
    }

    pub fn is_zero(&self) -> bool {
        self.digits.is_empty() || (self.digits.len() == 1 && self.digits[0] == 0)
    }

    pub fn is_one(&self) -> bool {
        self.digits.len() == 1 && self.digits[0] == 1
    }

    pub fn is_negative(&self) -> bool {
        self.digits.first().map_or(false, |&digit| digit < 0)
    }

    pub fn gcd(&self, other: &Self) -> Self {
        let mut a = self.clone().abs();
        let mut b = other.clone().abs();

        while !b.is_zero() {
            let temp = b.clone();
            b = a % b;
            a = temp;
        }

        a
    }

    pub fn abs(mut self) -> Self {
        self.sign = false;
        self
    }

    pub fn factorial(self) -> Self {
        if self.is_negative() {
            eprintln!("Warning: Factorial of a negative number is undefined");
            return BigInt::zero();
        }
        let mut result = BigInt::one();
        let mut i = BigInt::one();
        while i <= self.clone() {
            result = result * i.clone();
            i = i + BigInt::one();
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use crate::backend::bigint::BigInt;

    #[test]
    fn test_creation() {
        let bigint = BigInt { digits: vec![1, 2, 3], sign: false };
        assert_eq!(bigint.digits, vec![1, 2, 3]);
    }

    #[test]
    fn test_comparison() {
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let b = BigInt { digits: vec![1, 2, 3], sign: false };
        assert_eq!(a, b);
        let c = BigInt { digits: vec![1, 2, 4], sign: false };
        assert_ne!(a, c);
        assert!(a < c);
        let d = BigInt { digits: vec![1, 9], sign: false };
        assert!(a > d);
        let e = BigInt { digits: vec![1, 2, 3], sign: true };
        assert!(e < a);
    }

    #[test]
    fn test_negation() {
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let result = -a;
        assert_eq!(result.digits, vec![1, 2, 3]);
        assert_eq!(result.sign, true);
    }

    #[test]
    fn test_addition() {
        let a = BigInt { digits: vec![6, 2, 3], sign: false };
        let b = BigInt { digits: vec![4, 5, 6], sign: false };
        let result = a + b;
        assert_eq!(result.digits, vec![0, 8, 9]);
        assert_eq!(result.sign, false);
        let d = BigInt { digits: vec![1, 2, 3], sign: false };
        let c = BigInt { digits: vec![1, 2, 3], sign: true };
        let result = d + c;
        assert_eq!(result, BigInt::zero());
        let e = BigInt { digits: vec![1, 2, 3], sign: true };
        let f = BigInt { digits: vec![1, 2, 2], sign: false };
        let result = e + f;
        assert_eq!(result.digits, vec![0, 0, 1]);
        assert_eq!(result.sign, true);
    }

    #[test]
    fn test_subtraction() {
        let a = BigInt { digits: vec![5, 1, 9], sign: false };
        let b = BigInt { digits: vec![1, 2, 3], sign: false };
        let result = a - b;
        assert_eq!(result.digits, vec![4, 9, 5]);
        assert_eq!(result.sign, false);
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let b = BigInt { digits: vec![5, 1, 9], sign: false };
        let result = a - b;
        assert_eq!(result.digits, vec![4, 9, 5]);
        assert_eq!(result.sign, true);
        let a = BigInt { digits: vec![1, 2, 3], sign: true };
        let b = BigInt { digits: vec![1, 2, 3], sign: false };
        let result = a - b;
        assert_eq!(result.digits, vec![2, 4, 6]);
        assert_eq!(result.sign, true);
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let b = BigInt { digits: vec![1, 3, 3], sign: false };
        let result = a - b;
        assert_eq!(result.digits, vec![0, 1]);
        assert_eq!(result.sign, true);
    }

    #[test]
    fn test_multiplication() {
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let b = BigInt { digits: vec![4, 5, 6], sign: false };
        let result = a * b;
        assert_eq!(result.digits, vec![4, 3, 9, 9, 0, 2]);
        assert_eq!(result.sign, false);
        let a = BigInt { digits: vec![1, 2, 3], sign: true };
        let b = BigInt { digits: vec![4, 5, 6], sign: false };
        let result = a * b;
        assert_eq!(result.digits, vec![4, 3, 9, 9, 0, 2]);
        assert_eq!(result.sign, true);
    }

    #[test]
    fn test_division() {
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let b = BigInt { digits: vec![3], sign: false };
        let result = a / b;
        assert_eq!(result.digits, vec![7, 0, 1]);
        assert_eq!(result.sign, false);

        let a = BigInt { digits: vec![1, 2, 3], sign: true };
        let b = BigInt { digits: vec![4], sign: false };
        let result = a / b;
        assert_eq!(result.digits, vec![0, 8]);
        assert_eq!(result.sign, true);

        // let a = BigInt { digits: vec![1, 2, 3], sign: false };
        // let b = BigInt { digits: vec![0], sign: false };
        // let result = a / b;
        // println!("{:?}", result);
    }

    #[test]
    fn test_remainder() {
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let b = BigInt { digits: vec![1, 2], sign: false };
        let result = a % b;
        assert_eq!(result.digits, vec![6]);
        let c = BigInt { digits: vec![1, 2, 4], sign: false };
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let result = c % a;
        assert_eq!(result.digits, vec![0, 0, 1]);
        let d = BigInt { digits: vec![1, 2, 3], sign: true };
        let e = BigInt { digits: vec![1, 2, 3], sign: false };
        let result = d % e;
        assert_eq!(result.digits, vec![0]);
        let d = BigInt { digits: vec![1, 2, 4], sign: true };
        let e = BigInt { digits: vec![1, 2, 3], sign: false };
        let result = d % e;
        assert_eq!(result.digits, vec![1, 2, 2]);
        assert_eq!(result.sign, false);
    }

    #[test]
    fn test_pow() {
        let a = BigInt { digits: vec![2, 2], sign: false };
        let b = BigInt { digits: vec![3], sign: false };
        let result = a.pow(b.clone());
        assert_eq!(result.digits, vec![8, 4, 6, 0, 1]);
        assert_eq!(result.sign, false);
        let a = BigInt { digits: vec![2, 2], sign: true };
        let result = a.pow(b);
        assert_eq!(result.digits, vec![8, 4, 6, 0, 1]);
        assert_eq!(result.sign, true);
    }

    #[test]
    fn test_mod_pow() {
        let a = BigInt { digits: vec![2, 1], sign: false };
        let b = BigInt { digits: vec![9, 9], sign: false };
        let result = a.mod_pow(3, b);
        assert_eq!(result.digits, vec![5, 4]);
        assert_eq!(result.sign, false);
        let a = BigInt { digits: vec![2, 1], sign: true };
        let b = BigInt { digits: vec![9, 9], sign: false };
        let result = a.mod_pow(3, b);
        assert_eq!(result.digits, vec![4, 5]);
        assert_eq!(result.sign, false);
    }

    #[test]
    fn test_is_zero() {
        let a = BigInt { digits: vec![0], sign: false };
        assert!(a.is_zero());

        let b = BigInt { digits: vec![1], sign: false };
        assert!(!b.is_zero());
    }

    #[test]
    fn test_is_one() {
        let a = BigInt { digits: vec![1], sign: false };
        assert!(a.is_one());

        let b = BigInt { digits: vec![0], sign: false };
        assert!(!b.is_one());
    }

    #[test]
    fn test_is_negative() {
        let a = BigInt { digits: vec![-1], sign: false };
        assert!(a.is_negative());

        let b = BigInt { digits: vec![1], sign: false };
        assert!(!b.is_negative());
    }

    #[test]
    fn test_gcd() {
        let a = BigInt { digits: vec![2, 2], sign: false };
        let b = BigInt { digits: vec![3, 3], sign: false };
        let result = a.gcd(&b);
        assert_eq!(result.digits, vec![1, 1]);
        assert_eq!(result.sign, false);
        let a = BigInt { digits: vec![3, 3], sign: true };
        let b = BigInt { digits: vec![2, 2], sign: false };
        let result = a.gcd(&b);
        assert_eq!(result.digits, vec![1, 1]);
        assert_eq!(result.sign, false);
        let a = BigInt { digits: vec![3, 3], sign: true };
        let b = BigInt { digits: vec![3, 3], sign: false };
        let result = a.gcd(&b);
        assert_eq!(result.digits, vec![3, 3]);
        assert_eq!(result.sign, false);
    }

    #[test]
    fn test_abs() {
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let result = a.abs();
        assert_eq!(result.digits, vec![1, 2, 3]);
        assert_eq!(result.sign, false);

        let b = BigInt { digits: vec![1, 2, 3], sign: true };
        let result = b.abs();
        assert_eq!(result.digits, vec![1, 2, 3]);
        assert_eq!(result.sign, false);
    }

    #[test]
    fn test_comparison_with_zero() {
        let a = BigInt { digits: vec![1, 2, 3], sign: false };
        let b = BigInt { digits: vec![0], sign: false };
        assert!(a > b);
    }

    #[test]
    fn test_comparison_with_negative_zero() {
        let a = BigInt { digits: vec![0], sign: false };
        let b = BigInt { digits: vec![-0], sign: false };
        assert!(a == b);
    }

    #[test]
    fn test_from_str() {
        let a = BigInt::from("123".to_string());
        assert_eq!(a.digits, vec![3, 2, 1]);

        let b = BigInt::from("00123".to_string());
        assert_eq!(b.digits, vec![3, 2, 1]);

        let c = BigInt::from("0".to_string());
        assert_eq!(c.digits, vec![0]);

        let d = BigInt::from("0000".to_string());
        assert_eq!(d.digits, vec![0]);

        let e = BigInt::from("9876543210".to_string());
        assert_eq!(e.digits, vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    }

    #[test]
    fn test_factorial() {
        let a = BigInt { digits: vec![0, 1], sign: false };
        let result = a.factorial();
        println!("{}", result.to_string());
    }
}
