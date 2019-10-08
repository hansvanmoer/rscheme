///
/// A scheme number
///

#[derive(PartialEq, Debug)]
pub struct UnsignedFraction{
    divident: String,
    divisor: String
}

#[derive(PartialEq, Debug)]
pub struct UnsignedDecimal{
    significant: String,
    exponent_sign: bool,
    exponent_value: String,
    mantissa_width: u32
}

#[derive(PartialEq, Debug)]
pub enum UnsignedReal{
    Fraction(UnsignedFraction),
    Decimal(UnsignedDecimal)
}

#[derive(PartialEq, Debug)]
pub struct RealValue{
    sign: bool,
    value: UnsignedReal,
}

impl RealValue{

    pub fn new_fraction(sign: bool, divident: String, divisor: String) -> RealValue{
        RealValue{sign, value: UnsignedReal::Fraction(UnsignedFraction{divident, divisor})}
    }

    pub fn new_unit_fraction() -> RealValue{
        RealValue{sign: false, value: UnsignedReal::Fraction(UnsignedFraction{divident: String::from("1"), divisor: String::from("1")})}
    }

    pub fn new_zero_fraction() -> RealValue{
    RealValue{sign: false, value: UnsignedReal::Fraction(UnsignedFraction{divident: String::from("0"), divisor: String::from("1")})}
    }
    
    pub fn new_decimal(sign: bool, significant: String, exponent_sign: bool, exponent_value: String, mantissa_width: u32) -> RealValue{
        RealValue{sign, value: UnsignedReal::Decimal(UnsignedDecimal{significant, exponent_sign, exponent_value, mantissa_width})}
    }
}

#[derive(PartialEq, Debug)]
pub enum Real{
    Value(RealValue),
    PosNan,
    NegNan,
    PosInfinity,
    NegInfinity
}

impl Real{

    pub fn new_zero() -> Real{
        Real::Value(RealValue::new_zero_fraction())
    }

    pub fn new_unit() -> Real{
        Real::Value(RealValue::new_unit_fraction())
    }
}

#[derive(PartialEq, Debug)]
pub struct Number {
    radix: u32,
    exactness: bool,
    real: Real,
    imag: Real
}

impl Number {

    pub fn new(radix: u32, exactness: bool, real: Real, imag: Real) -> Number {
        Number{radix, exactness, real, imag}
    }

    pub fn new_unit(radix: u32, exactness: bool) -> Number{
        Number{radix, exactness, real: Real::Value(RealValue::new_unit_fraction()), imag: Real::Value(RealValue::new_zero_fraction())}
    }
}
