use crate::internal::sys::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    U128(u128),
    I128(i128),
    Usize(usize),
    Isize(isize),
    F32(f32),
    F64(f64),
}

macro_rules! impl_as_int_for_number {
    ($name:ident, $type:ty, $variant:ident) => {
        pub fn $name(&self) -> Result<$type> {
            if let Number::$variant(n) = self {
                return Ok(*n);
            }

            match self {
                Number::U8(n) => impl_as_int_for_number!(@from_integer $type, *n, u8),
                Number::I8(n) => impl_as_int_for_number!(@from_integer $type, *n, i8),
                Number::U16(n) => impl_as_int_for_number!(@from_integer $type, *n, u16),
                Number::I16(n) => impl_as_int_for_number!(@from_integer $type, *n, i16),
                Number::U32(n) => impl_as_int_for_number!(@from_integer $type, *n, u32),
                Number::I32(n) => impl_as_int_for_number!(@from_integer $type, *n, i32),
                Number::U64(n) => impl_as_int_for_number!(@from_integer $type, *n, u64),
                Number::I64(n) => impl_as_int_for_number!(@from_integer $type, *n, i64),
                Number::U128(n) => impl_as_int_for_number!(@from_integer $type, *n, u128),
                Number::I128(n) => impl_as_int_for_number!(@from_integer $type, *n, i128),
                Number::Usize(n) => impl_as_int_for_number!(@from_integer $type, *n, usize),
                Number::Isize(n) => impl_as_int_for_number!(@from_integer $type, *n, isize),
                Number::F32(n) => impl_as_int_for_number!(@from_float $type, *n, f32),
                Number::F64(n) => impl_as_int_for_number!(@from_float $type, *n, f64),
            }
        }
    };


    (@from_integer $type:ty, $number:expr, $source:ty) => {{
        <$type>::try_from($number).map_err(|_| {
            Error::new(format!(
                "Cannot convert `{}` number {} to `{}`",
                stringify!($source),
                $number,
                stringify!($type),
            ))
        })
    }};

    (@from_float $type:ty, $number:expr, $source:ty) => {{
        if !$number.is_finite() {
            return Err(Error::new(format!(
                "Cannot convert non-finite `{}` number {} to `{}`",
                stringify!($source),
                $number,
                stringify!($type),
            )));
        }

        if $number < <$type>::MIN as $source || $number > <$type>::MAX as $source {
            return Err(Error::new(format!(
                "`{}` number {} exceeds the bounds of `{}`",
                stringify!($source),
                $number,
                stringify!($type)
            )));
        }

        let n_int = $number as $type;

        if n_int as $source != $number {
            return Err(Error::new(format!(
                "Cannot convert `{}` number {} to `{}` without loss of precision",
                stringify!($source),
                $number,
                stringify!($type)
            )));
        }

        Ok(n_int)
    }};
}

impl Number {
    impl_as_int_for_number!(as_u8, u8, U8);
    impl_as_int_for_number!(as_i8, i8, I8);
    impl_as_int_for_number!(as_u16, u16, U16);
    impl_as_int_for_number!(as_i16, i16, I16);
    impl_as_int_for_number!(as_u32, u32, U32);
    impl_as_int_for_number!(as_i32, i32, I32);
    impl_as_int_for_number!(as_u64, u64, U64);
    impl_as_int_for_number!(as_i64, i64, I64);
    impl_as_int_for_number!(as_u128, u128, U128);
    impl_as_int_for_number!(as_i128, i128, I128);
    impl_as_int_for_number!(as_usize, usize, Usize);
    impl_as_int_for_number!(as_isize, isize, Isize);

    pub fn as_f32(&self) -> Result<f32> {
        match self {
            Number::F32(n) => Ok(*n),
            Number::F64(n) => {
                if !n.is_finite() {
                    return Ok(*n as f32);
                }

                let n_f32 = *n as f32;

                if n_f32.is_infinite() {
                    return Err(Error::new(format!(
                        "`f64` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                if *n != 0.0 && n_f32 == 0.0 {
                    return Err(Error::new(format!(
                        "`f64` value {} underflowed to zero for `f32`",
                        n,
                    )));
                }

                Ok(n_f32)
            }
            Number::U8(n) => Ok(*n as f32),
            Number::I8(n) => Ok(*n as f32),
            Number::U16(n) => Ok(*n as f32),
            Number::I16(n) => Ok(*n as f32),
            Number::U32(n) => {
                // `f32` has 23 bits of mantissa precision, max integers value is 2^24 - 1
                if *n > (1u32 << 24) - 1 {
                    return Err(Error::new(format!(
                        "`u32` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                let n_f32 = *n as f32;

                if n_f32 as u32 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `u32` number {} to `f32` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f32)
            }
            Number::I32(n) => {
                // `f32` has 23 bits of mantissa precision, max integers value is 2^24 - 1
                if *n < -(1i32 << 24) + 1 || *n > (1i32 << 24) - 1 {
                    return Err(Error::new(format!(
                        "`i32` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                let n_f32 = *n as f32;

                if n_f32 as i32 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `i32` number {} to `f32` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f32)
            }
            Number::U64(n) => {
                // `f32` has 23 bits of mantissa precision, max integers value is 2^24 - 1
                if *n > (1u64 << 24) - 1 {
                    return Err(Error::new(format!(
                        "`u64` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                let n_f32 = *n as f32;

                if n_f32 as u64 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `u64` number {} to `f32` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f32)
            }
            Number::I64(n) => {
                // `f32` has 23 bits of mantissa precision, max integers value is 2^24 - 1
                if *n < -(1i64 << 24) + 1 || *n > (1i64 << 24) - 1 {
                    return Err(Error::new(format!(
                        "`i64` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                let n_f32 = *n as f32;

                if n_f32 as i64 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `i64` number {} to `f32` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f32)
            }
            Number::U128(n) => {
                // `f32` has 23 bits of mantissa precision, max integers value is 2^24 - 1
                if *n > (1u128 << 24) - 1 {
                    return Err(Error::new(format!(
                        "`u128` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                let n_f32 = *n as f32;

                if n_f32 as u128 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `u128` number {} to `f32` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f32)
            }
            Number::I128(n) => {
                // `f32` has 23 bits of mantissa precision, max integers value is 2^24 - 1
                if *n < -(1i128 << 24) + 1 || *n > (1i128 << 24) - 1 {
                    return Err(Error::new(format!(
                        "`i128` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                let n_f32 = *n as f32;

                if n_f32 as i128 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `i128` number {} to `f32` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f32)
            }
            Number::Usize(n) => {
                // `f32` has 23 bits of mantissa precision, max integers value is 2^24 - 1
                if *n > (1usize << 24) - 1 {
                    return Err(Error::new(format!(
                        "`usize` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                let n_f32 = *n as f32;

                if n_f32 as usize != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `usize` number {} to `f32` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f32)
            }
            Number::Isize(n) => {
                // `f32` has 23 bits of mantissa precision, max integers value is 2^24 - 1
                if *n < -(1isize << 24) + 1 || *n > (1isize << 24) - 1 {
                    return Err(Error::new(format!(
                        "`isize` value {} exceeds the bounds of `f32`",
                        n,
                    )));
                }

                let n_f32 = *n as f32;

                if n_f32 as isize != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `isize` number {} to `f32` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f32)
            }
        }
    }

    pub fn as_f64(&self) -> Result<f64> {
        match self {
            Number::F64(n) => Ok(*n),
            Number::F32(n) => Ok(*n as f64),
            Number::U8(n) => Ok(*n as f64),
            Number::I8(n) => Ok(*n as f64),
            Number::U16(n) => Ok(*n as f64),
            Number::I16(n) => Ok(*n as f64),
            Number::U32(n) => Ok(*n as f64),
            Number::I32(n) => Ok(*n as f64),
            Number::U64(n) => {
                // `f64` has 52 bits of mantissa precision, max integers value is 2^53 - 1
                if *n > (1u64 << 53) - 1 {
                    return Err(Error::new(format!(
                        "`u64` value {} exceeds the bounds of `f64`",
                        n,
                    )));
                }

                let n_f64 = *n as f64;

                if n_f64 as u64 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `u64` number {} to `f64` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f64)
            }
            Number::I64(n) => {
                // `f64` has 52 bits of mantissa precision, max integers value is 2^53 - 1
                if *n < -(1i64 << 53) + 1 || *n > (1i64 << 53) - 1 {
                    return Err(Error::new(format!(
                        "`i64` value {} exceeds the bounds of `f64`",
                        n,
                    )));
                }

                let n_f64 = *n as f64;

                if n_f64 as i64 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `i64` number {} to `f64` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f64)
            }
            Number::U128(n) => {
                // `f64` has 52 bits of mantissa precision, max integers value is 2^53 - 1
                if *n > (1u128 << 53) - 1 {
                    return Err(Error::new(format!(
                        "`u128` value {} exceeds the bounds of `f64`",
                        n,
                    )));
                }

                let n_f64 = *n as f64;

                if n_f64 as u128 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `u128` number {} to `f64` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f64)
            }
            Number::I128(n) => {
                // `f64` has 52 bits of mantissa precision, max integers value is 2^53 - 1
                if *n < -(1i128 << 53) + 1 || *n > (1i128 << 53) - 1 {
                    return Err(Error::new(format!(
                        "`i128` value {} exceeds the bounds of `f64`",
                        n,
                    )));
                }

                let n_f64 = *n as f64;

                if n_f64 as i128 != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `i128` number {} to `f64` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f64)
            }
            Number::Usize(n) => {
                // `f64` has 52 bits of mantissa precision, max integers value is 2^53 - 1
                if *n > (1usize << 53) - 1 {
                    return Err(Error::new(format!(
                        "`usize` value {} exceeds the bounds of `f64`",
                        n,
                    )));
                }

                let n_f64 = *n as f64;

                if n_f64 as usize != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `usize` number {} to `f64` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f64)
            }
            Number::Isize(n) => {
                // `f64` has 52 bits of mantissa precision, max integers value is 2^53 - 1
                if *n < -(1isize << 53) + 1 || *n > (1isize << 53) - 1 {
                    return Err(Error::new(format!(
                        "`isize` value {} exceeds the bounds of `f64`",
                        n,
                    )));
                }

                let n_f64 = *n as f64;

                if n_f64 as isize != *n {
                    return Err(Error::new(format!(
                        "Cannot convert `isize` number {} to `f64` without loss of precision",
                        n,
                    )));
                }

                Ok(n_f64)
            }
        }
    }
}

impl core::fmt::Display for Number {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Number::U8(n) => core::write!(f, "{}", n),
            Number::I8(n) => core::write!(f, "{}", n),
            Number::U16(n) => core::write!(f, "{}", n),
            Number::I16(n) => core::write!(f, "{}", n),
            Number::U32(n) => core::write!(f, "{}", n),
            Number::I32(n) => core::write!(f, "{}", n),
            Number::U64(n) => core::write!(f, "{}", n),
            Number::I64(n) => core::write!(f, "{}", n),
            Number::U128(n) => core::write!(f, "{}", n),
            Number::I128(n) => core::write!(f, "{}", n),
            Number::Usize(n) => core::write!(f, "{}", n),
            Number::Isize(n) => core::write!(f, "{}", n),
            Number::F32(n) => core::write!(f, "{}", n),
            Number::F64(n) => core::write!(f, "{}", n),
        }
    }
}

macro_rules! impl_from_for_number {
    ($type:ty, $variant:ident) => {
        impl From<$type> for Number {
            fn from(value: $type) -> Self {
                Self::$variant(value)
            }
        }
    };
}

impl_from_for_number!(u8, U8);
impl_from_for_number!(i8, I8);
impl_from_for_number!(u16, U16);
impl_from_for_number!(i16, I16);
impl_from_for_number!(u32, U32);
impl_from_for_number!(i32, I32);
impl_from_for_number!(u64, U64);
impl_from_for_number!(i64, I64);
impl_from_for_number!(u128, U128);
impl_from_for_number!(i128, I128);
impl_from_for_number!(usize, Usize);
impl_from_for_number!(isize, Isize);
impl_from_for_number!(f32, F32);
impl_from_for_number!(f64, F64);

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Boolean(bool),
    Number(Number),
    Text(String),
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Object(BTreeMap<String, Value>),
    Struct(Vec<(String, Value)>),
}

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub context: Option<String>,
    pub line: Option<usize>,
    pub column: Option<usize>,
}

impl Error {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            context: None,
            line: None,
            column: None,
        }
    }

    pub fn with_context(
        message: impl Into<String>,
        context: impl Into<String>,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            message: message.into(),
            context: Some(context.into()),
            line: Some(line),
            column: Some(column),
        }
    }
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match (self.line, self.column) {
            (Some(line), Some(column)) => {
                write!(
                    f,
                    "Parse error at line {}, column {}: {}",
                    line, column, self.message
                )?;

                if let Some(context) = &self.context {
                    write!(f, "\nContext: {}", context)?;
                }

                Ok(())
            }
            _ => {
                write!(f, "Error: {}", self.message)?;

                Ok(())
            }
        }
    }
}

impl core::error::Error for Error {}

pub type Result<T> = core::result::Result<T, Error>;

pub trait Serialize {
    fn serialize(&self) -> Result<Value>;
}

pub trait Deserialize: Sized {
    fn deserialize(value: Value) -> Result<Self>;
}
