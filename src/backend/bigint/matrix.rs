use crate::backend::bigint::{BigInt, fraction::Fraction};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VectorElement {
    BigInt(BigInt),
    Fraction(Fraction),
}

pub trait IntoVectorElement {
    fn into_vector_element(self) -> VectorElement;
}

impl IntoVectorElement for BigInt {
    fn into_vector_element(self) -> VectorElement {
        VectorElement::BigInt(self)
    }
}

impl IntoVectorElement for Fraction {
    fn into_vector_element(self) -> VectorElement {
        VectorElement::Fraction(self)
    }
}

impl From<BigInt> for VectorElement {
    fn from(x: BigInt) -> VectorElement {
        VectorElement::BigInt(x)
    }
}

impl From<Fraction> for VectorElement {
    fn from(x: Fraction) -> VectorElement {
        VectorElement::Fraction(x)
    }
}

impl Neg for VectorElement {
    type Output = VectorElement;

    fn neg(self) -> Self::Output {
        match self {
            VectorElement::BigInt(bi) => VectorElement::BigInt(-bi),
            VectorElement::Fraction(fr) => VectorElement::Fraction(-fr),
        }
    }
}

impl Add for VectorElement {
    type Output = VectorElement;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VectorElement::BigInt(l), VectorElement::BigInt(r)) => VectorElement::BigInt(l + r),
            (VectorElement::Fraction(l), VectorElement::Fraction(r)) => VectorElement::Fraction(l + r),
            (VectorElement::BigInt(l), VectorElement::Fraction(r)) => VectorElement::Fraction(l + r),
            (VectorElement::Fraction(l), VectorElement::BigInt(r)) => VectorElement::Fraction(l + r),
        }
    }
}

impl Sub for VectorElement {
    type Output = VectorElement;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VectorElement::BigInt(l), VectorElement::BigInt(r)) => VectorElement::BigInt(l - r),
            (VectorElement::Fraction(l), VectorElement::Fraction(r)) => VectorElement::Fraction(l - r),
            (VectorElement::BigInt(l), VectorElement::Fraction(r)) => VectorElement::Fraction(l - r),
            (VectorElement::Fraction(l), VectorElement::BigInt(r)) => VectorElement::Fraction(l - r),
        }
    }
}

impl Mul for VectorElement {
    type Output = VectorElement;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VectorElement::BigInt(l), VectorElement::BigInt(r)) => VectorElement::BigInt(l * r),
            (VectorElement::Fraction(l), VectorElement::Fraction(r)) => VectorElement::Fraction(l * r),
            (VectorElement::BigInt(l), VectorElement::Fraction(r)) => VectorElement::Fraction(l * r),
            (VectorElement::Fraction(l), VectorElement::BigInt(r)) => VectorElement::Fraction(l * r),
        }
    }
}

impl Div for VectorElement {
    type Output = VectorElement;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VectorElement::BigInt(l), VectorElement::BigInt(r)) => VectorElement::BigInt(l / r),
            (VectorElement::Fraction(l), VectorElement::Fraction(r)) => VectorElement::Fraction(l / r),
            (VectorElement::BigInt(l), VectorElement::Fraction(r)) => VectorElement::Fraction(l / r),
            (VectorElement::Fraction(l), VectorElement::BigInt(r)) => VectorElement::Fraction(l / r),
        }
    }
}

impl VectorElement {
    fn to_string(&self) -> String {
        match self {
            VectorElement::BigInt(x) => x.to_string(),
            VectorElement::Fraction(x) => x.to_string(),
        }
    }

    fn abs(&self) -> VectorElement {
        match self {
            VectorElement::BigInt(x) => VectorElement::BigInt(x.clone().abs()),
            VectorElement::Fraction(x) => VectorElement::Fraction(x.abs()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Vector {
    elements: Vec<VectorElement>,
}

impl Vector {
    pub fn new<T: IntoVectorElement>(elements: Vec<T>) -> Self {
        let elements = elements.into_iter().map(|e| e.into_vector_element()).collect();
        Vector { elements }
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn to_string(&self) -> String {
        format!("Vector [{}]", self.elements.iter()
            .map(|elem| elem.to_string())
            .collect::<Vec<_>>()
            .join(", "))
    }

    pub fn abs(&self) -> Vector {
        let elements = self.elements.clone().into_iter()
            .map(|elem| elem.abs())
            .collect();
        Vector { elements }
    }

    pub fn is_zero(&self) -> bool {
        self.elements.iter().all(|elem| elem.to_string() == "0")
    }
}

impl From<Vec<VectorElement>> for Vector {
    fn from(elements: Vec<VectorElement>) -> Vector {
        Vector { elements }
    }
}

impl Neg for Vector {
    type Output = Vector;

    fn neg(self) -> Vector {
        let elements = self.elements.into_iter()
            .map(|a| -a)
            .collect();
        Vector { elements }
    }
}

impl Add for Vector {
    type Output = Vector;

    fn add(self, other: Vector) -> Vector {
        assert_eq!(self.len(), other.len());
        let elements = self.elements.into_iter().zip(other.elements.into_iter())
            .map(|(a, b)| a + b)
            .collect();
        Vector { elements }
    }
}

impl Sub for Vector {
    type Output = Vector;

    fn sub(self, other: Vector) -> Vector {
        assert_eq!(self.len(), other.len());
        let elements = self.elements.into_iter().zip(other.elements.into_iter())
            .map(|(a, b)| a - b)
            .collect();
        Vector { elements }
    }
}

impl Mul for Vector {
    type Output = VectorElement;
    fn mul(self, other: Vector) -> VectorElement {
        assert_eq!(self.len(), other.len());
        self.elements.into_iter().zip(other.elements.into_iter())
            .map(|(a, b)| a * b)
            .fold(VectorElement::BigInt(BigInt::zero()), |acc, x| acc + x)
    }
}

impl Mul<Matrix> for Vector {
    type Output = Vector;

    fn mul(self, other: Matrix) -> Vector {
        assert_eq!(self.len(), other.rows());
        let elements = other.transpose().rows.into_iter()
            .map(|row| self.clone() * row)
            .collect();
        Vector { elements }
    }
}

impl Mul<VectorElement> for Matrix {
    type Output = Matrix;

    fn mul(self, scalar: VectorElement) -> Matrix {
        let rows = self.rows.into_iter()
            .map(|row| row * scalar.clone())
            .collect();
        Matrix { rows }
    }
}

impl Mul<Matrix> for VectorElement {
    type Output = Matrix;

    fn mul(self, matrix: Matrix) -> Matrix {
        matrix * self
    }
}

impl Mul<VectorElement> for Vector {
    type Output = Vector;

    fn mul(self, scalar: VectorElement) -> Vector {
        let elements = self.elements.into_iter()
            .map(|elem| elem * scalar.clone())
            .collect();
        Vector { elements }
    }
}

impl Mul<Vector> for VectorElement {
    type Output = Vector;

    fn mul(self, vector: Vector) -> Vector {
        vector * self
    }
}

impl Div<VectorElement> for Vector {
    type Output = Vector;
    fn div(self, scalar: VectorElement) -> Vector {
        (VectorElement::Fraction(Fraction::one()) / scalar) * self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matrix {
    rows: Vec<Vector>,
}

impl Matrix {
    pub fn new(rows: Vec<Vector>) -> Self {
        let row_len = rows[0].len();
        assert!(rows.iter().all(|row| row.len() == row_len));
        Matrix { rows }
    }

    pub fn to_string(&self) -> String {
        let col_widths: Vec<usize> = (0..self.cols())
            .map(|col| {
                self.rows
                    .iter()
                    .map(|row| row.elements[col].to_string().len())
                    .max()
                    .unwrap_or(0)
            })
            .collect();

        let rows_str = self.rows.iter()
            .map(|row| {
                row.elements.iter()
                    .enumerate()
                    .map(|(i, elem)| format!("  {:>width$}", elem.to_string(), width = col_widths[i]))
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!("Matrix {}x{} [\n{}\n]", self.rows(), self.cols(), rows_str)
    }

    pub fn rows(&self) -> usize {
        self.rows.len()
    }

    pub fn cols(&self) -> usize {
        self.rows[0].len()
    }

    pub fn transpose(&self) -> Matrix {
        let mut transposed_elements = vec![vec![VectorElement::BigInt(BigInt::zero()); self.rows()]; self.cols()];
        for i in 0..self.rows() {
            for j in 0..self.cols() {
                transposed_elements[j][i] = self.rows[i].elements[j].clone();
            }
        }
        let transposed_rows = transposed_elements.into_iter()
            .map(|row| Vector { elements: row })
            .collect();
        Matrix { rows: transposed_rows }
    }

    pub fn abs(&self) -> Matrix {
        let rows = self.rows.clone().into_iter()
            .map(|row| row.abs())
            .collect();
        Matrix { rows }
    }

    pub fn is_zero(&self) -> bool {
        self.rows.iter().all(|row| row.is_zero())
    }
}

impl From<Vec<Vec<VectorElement>>> for Matrix {
    fn from(rows: Vec<Vec<VectorElement>>) -> Self {
        // assert!(rows.iter().all(|row| row.len() == rows[0].len()));
        // if !rows.iter().all(|row| row.len() == rows[0].len()) {
        //     println!("Matrix is valid, each row should have the same length");
        //     return Matrix::from(vec![]);
        // }
        let rows = rows.into_iter()
            .map(|row| Vector { elements: row })
            .collect();
        Matrix { rows }
    }
}

impl Neg for Matrix {
    type Output = Matrix;

    fn neg(self) -> Matrix {
        let rows = self.rows.into_iter()
            .map(|row| -row)
            .collect();
        Matrix { rows }
    }
}

impl Add for Matrix {
    type Output = Matrix;

    fn add(self, other: Matrix) -> Matrix {
        assert_eq!(self.rows(), other.rows());
        assert_eq!(self.cols(), other.cols());
        let rows = self.rows.into_iter().zip(other.rows.into_iter())
            .map(|(a, b)| a + b)
            .collect();
        Matrix { rows }
    }
}

impl Sub for Matrix {
    type Output = Matrix;

    fn sub(self, other: Matrix) -> Matrix {
        assert_eq!(self.rows(), other.rows());
        assert_eq!(self.cols(), other.cols());
        let rows = self.rows.into_iter().zip(other.rows.into_iter())
            .map(|(a, b)| a - b)
            .collect();
        Matrix { rows }
    }
}

impl Mul for Matrix {
    type Output = Matrix;

    fn mul(self, other: Matrix) -> Matrix {
        assert_eq!(self.cols(), other.rows());
        let mut result_elements = vec![vec![VectorElement::BigInt(BigInt::zero()); other.cols()]; self.rows()];
        for i in 0..self.rows() {
            for j in 0..other.cols() {
                for k in 0..self.cols() {
                    result_elements[i][j] = result_elements[i][j].clone() + (self.rows[i].elements[k].clone() * other.rows[k].elements[j].clone());
                }
            }
        }
        let result_rows = result_elements.into_iter()
            .map(|row| Vector { elements: row })
            .collect();
        Matrix { rows: result_rows }
    }
}

impl Mul<Vector> for Matrix {
    type Output = Vector;

    fn mul(self, other: Vector) -> Vector {
        assert_eq!(self.cols(), other.len());
        let elements = self.rows.into_iter()
            .map(|row| row.clone() * other.clone())
            .collect();
        Vector { elements }
    }
}

impl Div<VectorElement> for Matrix {
    type Output = Matrix;
    fn div(self, other: VectorElement) -> Matrix {
        let rows = self.rows.into_iter()
            .map(|row| row / other.clone())
            .collect();
        Matrix { rows }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
fn test_vector_add() {
    let a = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
    let b = Vector::new(vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]);
    let c = a + b;
    assert_eq!(
        c.elements,
        vec![
            VectorElement::BigInt(BigInt::from("4".to_string())),
            VectorElement::BigInt(BigInt::from("6".to_string()))
        ]
    );
}

    #[test]
    fn test_vector_sub() {
        let a = Vector::new(vec![BigInt::from("5".to_string()), BigInt::from("6".to_string())]);
        let b = Vector::new(vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]);
        let c = a - b;
        assert_eq!(
            c.elements,
            vec![
                BigInt::from("2".to_string()).into_vector_element(),
                BigInt::from("2".to_string()).into_vector_element()
            ]
        );
    }

    #[test]
    fn test_vector_mul() {
        let a = Vector::new(vec![BigInt::from("2".to_string()), BigInt::from("3".to_string())]);
        let b = Vector::new(vec![BigInt::from("4".to_string()), BigInt::from("5".to_string())]);
        let c = a * b;
        assert_eq!(c, 
            BigInt::from("23".to_string()).into_vector_element()
        ); // 2*4 + 3*5 = 8 + 15 = 23
    }

    #[test]
    fn test_vector_neg() {
        let a = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let b = -a;
        assert_eq!(b.elements, vec![-BigInt::from("1".to_string()).into_vector_element(), -BigInt::from("2".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_vector_len() {
        let a = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string()), BigInt::from("3".to_string())]);
        assert_eq!(a.len(), 3);
    }

    #[test]
    fn test_matrix_add() {
        let a = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()], 
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]
        ]);
        let b = Matrix::from(vec![
            vec![BigInt::from("5".to_string()).into_vector_element(), BigInt::from("6".to_string()).into_vector_element()], 
            vec![BigInt::from("7".to_string()).into_vector_element(), BigInt::from("8".to_string()).into_vector_element()]
        ]);
        let c = a + b;
        assert_eq!(c.rows(), 2);
        assert_eq!(c.cols(), 2);
        assert_eq!(c.rows[0].elements, vec![BigInt::from("6".to_string()).into_vector_element(), BigInt::from("8".to_string()).into_vector_element()]);
        assert_eq!(c.rows[1].elements, vec![BigInt::from("10".to_string()).into_vector_element(), BigInt::from("12".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_matrix_sub() {
        let a = Matrix::from(vec![
            vec![BigInt::from("5".to_string()).into_vector_element(), BigInt::from("6".to_string()).into_vector_element()], 
            vec![BigInt::from("7".to_string()).into_vector_element(), BigInt::from("8".to_string()).into_vector_element()]
        ]);
        let b = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()], 
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]
        ]);
        let c = a - b;
        assert_eq!(c.rows(), 2);
        assert_eq!(c.cols(), 2);
        assert_eq!(c.rows[0].elements, vec![BigInt::from("4".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]);
        assert_eq!(c.rows[1].elements, vec![BigInt::from("4".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_matrix_mul() {
        let a = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()], 
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]
        ]);
        let b = Matrix::from(vec![
            vec![BigInt::from("2".to_string()).into_vector_element(), BigInt::from("0".to_string()).into_vector_element()], 
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()]
        ]);
        let c = a * b;
        assert_eq!(c.rows(), 2);
        assert_eq!(c.cols(), 2);
        assert_eq!(c.rows[0].elements, vec![BigInt::from("4".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]);
        assert_eq!(c.rows[1].elements, vec![BigInt::from("10".to_string()).into_vector_element(), BigInt::from("8".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_matrix_neg() {
        let a = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()], 
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]
        ]);
        let b = -a;
        assert_eq!(b.rows(), 2);
        assert_eq!(b.cols(), 2);
        assert_eq!(b.rows[0].elements, vec![-BigInt::from("1".to_string()).into_vector_element(), -BigInt::from("2".to_string()).into_vector_element()]);
        assert_eq!(b.rows[1].elements, vec![-BigInt::from("3".to_string()).into_vector_element(), -BigInt::from("4".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_matrix_transpose() {
        let a = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()], 
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]
        ]);
        let b = a.transpose();
        assert_eq!(b.rows(), 2);
        assert_eq!(b.cols(), 2);
        assert_eq!(b.rows[0].elements, vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("3".to_string()).into_vector_element()]);
        assert_eq!(b.rows[1].elements, vec![BigInt::from("2".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_vector_matrix_mul() {
        let v = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let m = Matrix::from(vec![
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()],
            vec![BigInt::from("5".to_string()).into_vector_element(), BigInt::from("6".to_string()).into_vector_element()]
        ]);
        let result = v * m;
        assert_eq!(result.elements, vec![BigInt::from("13".to_string()).into_vector_element(), BigInt::from("16".to_string()).into_vector_element()]); // 1*3 + 2*5 = 13, 1*4 + 2*6 = 16
    }

    #[test]
    fn test_matrix_vector_mul() {
        let m = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()],
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]
        ]);
        let v = Vector::new(vec![BigInt::from("2".to_string()), BigInt::from("1".to_string())]);
        let result = m * v;
        assert_eq!(result.elements, vec![BigInt::from("4".to_string()).into_vector_element(), BigInt::from("10".to_string()).into_vector_element()]); // 1*2 + 2*1 = 4, 3*2 + 4*1 = 10
    }

    #[test]
    fn test_vector_scalar_mul() {
        let v = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let scalar = BigInt::from("3".to_string()).into_vector_element();
        let result = v * scalar;
        assert_eq!(result.elements, vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("6".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_scalar_vector_mul() {
        let scalar = BigInt::from("3".to_string()).into_vector_element();
        let v = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let result = scalar * v;
        assert_eq!(result.elements, vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("6".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_matrix_scalar_mul() {
        let m = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()],
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]
        ]);
        let scalar = BigInt::from("2".to_string()).into_vector_element();
        let result = m * scalar;
        assert_eq!(result.rows[0].elements, vec![BigInt::from("2".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]);
        assert_eq!(result.rows[1].elements, vec![BigInt::from("6".to_string()).into_vector_element(), BigInt::from("8".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_scalar_matrix_mul() {
        let scalar = BigInt::from("2".to_string()).into_vector_element();
        let m = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()],
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]
        ]);
        let result = scalar * m;
        assert_eq!(result.rows[0].elements, vec![BigInt::from("2".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()]);
        assert_eq!(result.rows[1].elements, vec![BigInt::from("6".to_string()).into_vector_element(), BigInt::from("8".to_string()).into_vector_element()]);
    }

    #[test]
    fn test_matrix_scalar_multiplication_with_fraction() {
        let scalar = Fraction::new(BigInt::from("2".to_string()), BigInt::from("3".to_string())).into_vector_element();
        let matrix = Matrix::from(vec![
            vec![BigInt::from("1".to_string()).into_vector_element(), BigInt::from("2".to_string()).into_vector_element()],
            vec![BigInt::from("3".to_string()).into_vector_element(), BigInt::from("4".to_string()).into_vector_element()],
        ]);
        let result = matrix * scalar;
        println!("{}", result.to_string())
    }
}
