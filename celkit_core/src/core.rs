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

impl Number {
    pub fn as_f64(&self) -> f64 {
        match self {
            Number::U8(n) => *n as f64,
            Number::I8(n) => *n as f64,
            Number::U16(n) => *n as f64,
            Number::I16(n) => *n as f64,
            Number::U32(n) => *n as f64,
            Number::I32(n) => *n as f64,
            Number::U64(n) => *n as f64,
            Number::I64(n) => *n as f64,
            Number::U128(n) => *n as f64,
            Number::I128(n) => *n as f64,
            Number::Usize(n) => *n as f64,
            Number::Isize(n) => *n as f64,
            Number::F32(n) => *n as f64,
            Number::F64(n) => *n as f64,
        }
    }

    pub fn equals(&self, other: impl Into<Number>) -> bool {
        match (self, other.into()) {
            (Number::U8(a), Number::U8(b)) => *a == b,
            (Number::I8(a), Number::I8(b)) => *a == b,
            (Number::U16(a), Number::U16(b)) => *a == b,
            (Number::I16(a), Number::I16(b)) => *a == b,
            (Number::U32(a), Number::U32(b)) => *a == b,
            (Number::I32(a), Number::I32(b)) => *a == b,
            (Number::U64(a), Number::U64(b)) => *a == b,
            (Number::I64(a), Number::I64(b)) => *a == b,
            (Number::U128(a), Number::U128(b)) => *a == b,
            (Number::I128(a), Number::I128(b)) => *a == b,
            (Number::Usize(a), Number::Usize(b)) => *a == b,
            (Number::Isize(a), Number::Isize(b)) => *a == b,
            (Number::F32(a), Number::F32(b)) => (*a - b).abs() < f32::EPSILON,
            (Number::F64(a), Number::F64(b)) => (*a - b).abs() < f64::EPSILON,
            (a, b) => a.as_f64() == b.as_f64(),
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
