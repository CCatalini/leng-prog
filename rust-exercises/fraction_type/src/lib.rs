use std::ops;

#[derive(Debug, PartialEq, Eq)]
pub struct Fraction(pub i32, pub i32);

impl Fraction {
    pub fn add(&self, f: Fraction) -> Fraction{
        let (n1, d1) = (self.0, self.1);
        let (n2, d2) = (f.0, f.1);
        simplify((n1 * d2) + (n2 *d1), d1 * d2)
    }

    pub fn sub(&self, f: Fraction) -> Fraction{
        self.add(Fraction(-f.0,f.1))
    }

    pub fn mul(&self, f: Fraction) -> Fraction{
        let (n1, d1) = (self.0, self.1);
        let (n2, d2) = (f.0, f.1);
        simplify(n1 *  n2, d1 * d2)
    }

    pub fn divide(&self, f: Fraction) -> Fraction{
        self.mul(Fraction(f.1,f.0))
    }
}

impl ops::Add for Fraction {
    type Output = Self;
    fn add(self, f: Self) -> Self{
        let (n1, d1) = (self.0, self.1);
        let (n2, d2) = (f.0, f.1);
        simplify((n1 * d2) + (n2 *d1), d1 * d2)
    }
}

/// Calculate the Highest common factor between 2 numbers
fn hcf(a: i32, b: i32) -> i32 {
    if b == 0 { a } else { hcf(b, a % b) }
}

fn simplify(n: i32, d: i32) -> Fraction {
    let h = hcf(n, d);
    Fraction(n/h, d/h)
}