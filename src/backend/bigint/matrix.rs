use crate::backend::bigint::BigInt;
use std::ops::{Add, Sub, Mul, Neg};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Vector {
    elements: Vec<BigInt>,
}

impl Vector {
    pub fn new(elements: Vec<BigInt>) -> Self {
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
        if self.len() != other.len() {
            eprintln!("Warning: Vectors have different lengths");
            return Vector { elements: vec![] };
        }
        let elements = self.elements.into_iter().zip(other.elements.into_iter())
            .map(|(a, b)| a + b)
            .collect();
        Vector { elements }
    }
}

impl Sub for Vector {
    type Output = Vector;

    fn sub(self, other: Vector) -> Vector {
        if self.len() != other.len() {
            eprintln!("Warning: Vectors have different lengths");
            return Vector { elements: vec![] };
        }
        let elements = self.elements.into_iter().zip(other.elements.into_iter())
            .map(|(a, b)| a - b)
            .collect();
        Vector { elements }
    }
}

impl Mul for Vector {
    type Output = BigInt;

    fn mul(self, other: Vector) -> BigInt {
        if self.len() != other.len() {
            eprintln!("Warning: Vectors have different lengths");
            return BigInt::zero();
        }
        self.elements.into_iter().zip(other.elements.into_iter())
            .map(|(a, b)| a * b)
            .fold(BigInt::zero(), |acc, x| acc + x)
    }
}

impl Mul<Matrix> for Vector {
    type Output = Vector;

    fn mul(self, other: Matrix) -> Vector {
        if self.len() != other.rows() {
            eprintln!("Warning: Vector length does not match the number of rows in the matrix");
            return Vector { elements: vec![] };
        }
        let elements = other.transpose().rows.into_iter()
            .map(|row| self.clone() * row)
            .collect();
        Vector { elements }
    }
}

impl Mul<BigInt> for Matrix {
    type Output = Matrix;

    fn mul(self, scalar: BigInt) -> Matrix {
        let rows = self.rows.into_iter()
            .map(|row| row * scalar.clone())
            .collect();
        Matrix { rows }
    }
}

impl Mul<Matrix> for BigInt {
    type Output = Matrix;

    fn mul(self, matrix: Matrix) -> Matrix {
        matrix * self
    }
}

impl Mul<BigInt> for Vector {
    type Output = Vector;

    fn mul(self, scalar: BigInt) -> Vector {
        let elements = self.elements.into_iter()
            .map(|elem| elem * scalar.clone())
            .collect();
        Vector { elements }
    }
}

impl Mul<Vector> for BigInt {
    type Output = Vector;

    fn mul(self, vector: Vector) -> Vector {
        vector * self
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

    pub fn from_vecvec(elements: Vec<Vec<BigInt>>) -> Self {
        let rows = elements.into_iter()
            .map(Vector::new)
            .collect();
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
        let mut transposed_elements = vec![vec![BigInt::zero(); self.rows()]; self.cols()];
        for i in 0..self.rows() {
            for j in 0..self.cols() {
                transposed_elements[j][i] = self.rows[i].elements[j].clone();
            }
        }
        let transposed_rows = transposed_elements.into_iter()
            .map(Vector::new)
            .collect();
        Matrix { rows: transposed_rows }
        }
    }

    impl Add for Matrix {
        type Output = Matrix;

        fn add(self, other: Matrix) -> Matrix {
        if self.rows() != other.rows() || self.cols() != other.cols() {
            eprintln!("Warning: Matrices have different dimensions");
            return Matrix { rows: vec![] };
        }
        let rows = self.rows.into_iter().zip(other.rows.into_iter())
            .map(|(a, b)| a + b)
            .collect();
        Matrix { rows }
    }
}

impl Sub for Matrix {
    type Output = Matrix;

    fn sub(self, other: Matrix) -> Matrix {
        if self.rows() != other.rows() || self.cols() != other.cols() {
            eprintln!("Warning: Matrices have different dimensions");
            return Matrix { rows: vec![] };
        }
        let rows = self.rows.into_iter().zip(other.rows.into_iter())
            .map(|(a, b)| a - b)
            .collect();
        Matrix { rows }
    }
}

impl Mul for Matrix {
    type Output = Matrix;

    fn mul(self, other: Matrix) -> Matrix {
        if self.cols() != other.rows() {
            eprintln!("Warning: Matrices have incompatible dimensions for multiplication");
            return Matrix { rows: vec![] };
        }
        let mut result_elements = vec![vec![BigInt::zero(); other.cols()]; self.rows()];
        for i in 0..self.rows() {
            for j in 0..other.cols() {
                for k in 0..self.cols() {
                    result_elements[i][j] = result_elements[i][j].clone() + (self.rows[i].elements[k].clone() * other.rows[k].elements[j].clone());
                }
            }
        }
        let result_rows = result_elements.into_iter()
            .map(Vector::new)
            .collect();
        Matrix { rows: result_rows }
    }
}

impl Mul<Vector> for Matrix {
    type Output = Vector;

    fn mul(self, other: Vector) -> Vector {
        if self.cols() != other.len() {
            eprintln!("Warning: Matrix rows do not match vector length");
            return Vector { elements: vec![] };
        }
        let elements = self.rows.into_iter()
            .map(|row| row.clone() * other.clone())
            .collect();
        Vector { elements }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_add() {
        let a = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let b = Vector::new(vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]);
        let c = a + b;
        assert_eq!(c.elements, vec![BigInt::from("4".to_string()), BigInt::from("6".to_string())]);
    }

    #[test]
    fn test_vector_sub() {
        let a = Vector::new(vec![BigInt::from("5".to_string()), BigInt::from("6".to_string())]);
        let b = Vector::new(vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]);
        let c = a - b;
        assert_eq!(c.elements, vec![BigInt::from("2".to_string()), BigInt::from("2".to_string())]);
    }

    #[test]
    fn test_vector_mul() {
        let a = Vector::new(vec![BigInt::from("2".to_string()), BigInt::from("3".to_string())]);
        let b = Vector::new(vec![BigInt::from("4".to_string()), BigInt::from("5".to_string())]);
        let c = a * b;
        assert_eq!(c, BigInt::from("23".to_string())); // 2*4 + 3*5 = 8 + 15 = 23
    }

    #[test]
    fn test_vector_neg() {
        let a = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let b = -a;
        assert_eq!(b.elements, vec![-BigInt::from("1".to_string()), -BigInt::from("2".to_string())]);
    }

    #[test]
    fn test_vector_len() {
        let a = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string()), BigInt::from("3".to_string())]);
        assert_eq!(a.len(), 3);
    }

    #[test]
    fn test_matrix_add() {
        let a = Matrix::from_vecvec(vec![
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())], 
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]
        ]);
        let b = Matrix::from_vecvec(vec![
            vec![BigInt::from("5".to_string()), BigInt::from("6".to_string())], 
            vec![BigInt::from("7".to_string()), BigInt::from("8".to_string())]
        ]);
        let c = a + b;
        assert_eq!(c.rows(), 2);
        assert_eq!(c.cols(), 2);
        assert_eq!(c.rows[0].elements, vec![BigInt::from("6".to_string()), BigInt::from("8".to_string())]);
        assert_eq!(c.rows[1].elements, vec![BigInt::from("10".to_string()), BigInt::from("12".to_string())]);
    }

    #[test]
    fn test_matrix_sub() {
        let a = Matrix::from_vecvec(vec![
            vec![BigInt::from("5".to_string()), BigInt::from("6".to_string())], 
            vec![BigInt::from("7".to_string()), BigInt::from("8".to_string())]
        ]);
        let b = Matrix::from_vecvec(vec![
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())], 
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]
        ]);
        let c = a - b;
        assert_eq!(c.rows(), 2);
        assert_eq!(c.cols(), 2);
        assert_eq!(c.rows[0].elements, vec![BigInt::from("4".to_string()), BigInt::from("4".to_string())]);
        assert_eq!(c.rows[1].elements, vec![BigInt::from("4".to_string()), BigInt::from("4".to_string())]);
    }

    #[test]
    fn test_matrix_mul() {
        let a = Matrix::from_vecvec(vec![
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())], 
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]
        ]);
        let b = Matrix::from_vecvec(vec![
            vec![BigInt::from("2".to_string()), BigInt::from("0".to_string())], 
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]
        ]);
        let c = a * b;
        assert_eq!(c.rows(), 2);
        assert_eq!(c.cols(), 2);
        assert_eq!(c.rows[0].elements, vec![BigInt::from("4".to_string()), BigInt::from("4".to_string())]);
        assert_eq!(c.rows[1].elements, vec![BigInt::from("10".to_string()), BigInt::from("8".to_string())]);
    }

    #[test]
    fn test_matrix_neg() {
        let a = Matrix::from_vecvec(vec![
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())], 
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]
        ]);
        let b = -a;
        assert_eq!(b.rows(), 2);
        assert_eq!(b.cols(), 2);
        assert_eq!(b.rows[0].elements, vec![-BigInt::from("1".to_string()), -BigInt::from("2".to_string())]);
        assert_eq!(b.rows[1].elements, vec![-BigInt::from("3".to_string()), -BigInt::from("4".to_string())]);
    }

    #[test]
    fn test_matrix_transpose() {
        let a = Matrix::from_vecvec(vec![
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())], 
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]
        ]);
        let b = a.transpose();
        assert_eq!(b.rows(), 2);
        assert_eq!(b.cols(), 2);
        assert_eq!(b.rows[0].elements, vec![BigInt::from("1".to_string()), BigInt::from("3".to_string())]);
        assert_eq!(b.rows[1].elements, vec![BigInt::from("2".to_string()), BigInt::from("4".to_string())]);
    }

    #[test]
    fn test_vector_matrix_mul() {
        let v = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let m = Matrix::from_vecvec(vec![
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())],
            vec![BigInt::from("5".to_string()), BigInt::from("6".to_string())]
        ]);
        let result = v * m;
        assert_eq!(result.elements, vec![BigInt::from("13".to_string()), BigInt::from("16".to_string())]); // 1*3 + 2*5 = 13, 1*4 + 2*6 = 16
    }

    #[test]
    fn test_matrix_vector_mul() {
        let m = Matrix::from_vecvec(vec![
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())],
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]
        ]);
        let v = Vector::new(vec![BigInt::from("2".to_string()), BigInt::from("1".to_string())]);
        let result = m * v;
        assert_eq!(result.elements, vec![BigInt::from("4".to_string()), BigInt::from("10".to_string())]); // 1*2 + 2*1 = 4, 3*2 + 4*1 = 10
    }

    #[test]
    fn test_vector_scalar_mul() {
        let v = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let scalar = BigInt::from("3".to_string());
        let result = v * scalar;
        assert_eq!(result.elements, vec![BigInt::from("3".to_string()), BigInt::from("6".to_string())]);
    }

    #[test]
    fn test_scalar_vector_mul() {
        let scalar = BigInt::from("3".to_string());
        let v = Vector::new(vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())]);
        let result = scalar * v;
        assert_eq!(result.elements, vec![BigInt::from("3".to_string()), BigInt::from("6".to_string())]);
    }

    #[test]
    fn test_matrix_scalar_mul() {
        let m = Matrix::from_vecvec(vec![
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())],
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]
        ]);
        let scalar = BigInt::from("2".to_string());
        let result = m * scalar;
        assert_eq!(result.rows[0].elements, vec![BigInt::from("2".to_string()), BigInt::from("4".to_string())]);
        assert_eq!(result.rows[1].elements, vec![BigInt::from("6".to_string()), BigInt::from("8".to_string())]);
    }

    #[test]
    fn test_scalar_matrix_mul() {
        let scalar = BigInt::from("2".to_string());
        let m = Matrix::from_vecvec(vec![
            vec![BigInt::from("1".to_string()), BigInt::from("2".to_string())],
            vec![BigInt::from("3".to_string()), BigInt::from("4".to_string())]
        ]);
        let result = scalar * m;
        assert_eq!(result.rows[0].elements, vec![BigInt::from("2".to_string()), BigInt::from("4".to_string())]);
        assert_eq!(result.rows[1].elements, vec![BigInt::from("6".to_string()), BigInt::from("8".to_string())]);
    }
}
