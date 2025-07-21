use core::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Number {
    I64(i64),
    F64(f64),
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Group(Vec<Value>),
    Object(HashMap<String, Value>),
    Struct(String, HashMap<String, Value>),
}

#[derive(Debug)]
struct Error {
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

    pub fn with_position(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            message: message.into(),
            context: None,
            line: Some(line),
            column: Some(column),
        }
    }

    pub fn with_context_and_suggestion(
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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn deserialize(&self) -> Result<Self>;
}
