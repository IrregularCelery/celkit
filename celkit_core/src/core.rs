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
    F32(f32),
    F64(f64),
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
            Number::F32(n) => core::write!(f, "{}", n),
            Number::F64(n) => core::write!(f, "{}", n),
        }
    }
}

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

// TODO: Probably better to move this into `decode` module and have a separate one for `encode`
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
