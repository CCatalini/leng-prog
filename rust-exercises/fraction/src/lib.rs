type Fraction = (i32, i32);

/// Add 2 fractions
pub fn add((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    simplify(n1*d2 + n2*d1,d1*d2)
}

/// Subtract 2 fractions
pub fn sub((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    add((n1,d1),(-n2,d2))
}

/// Multiply 2 fractions
pub fn mul((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    simplify(n1*n2,d1*d2)
}

/// Divide 2 fractions
pub fn divide((n1, d1): Fraction, (n2, d2): Fraction) -> Fraction {
    mul((n1,d1),(d2,n2))
}

/// Calculate the Highest common factor between 2 numbers
pub fn hcf(a: i32, b: i32) -> i32 {
    let mut temporal;
    let mut a1 = a;
    let mut b1 = b;
    while b1 != 0 {
        temporal = b1;
        b1 = a1 % b1;
        a1 = temporal;
    }
    return a1;
}

/// Create a fraction simplifying with the arguments simplified by the `hcf`
pub fn simplify(n: i32, d: i32) -> Fraction {

    let factor = hcf(n,d);
    (n/factor,d/factor)
}