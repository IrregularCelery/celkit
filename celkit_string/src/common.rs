use celkit_core::internal::sys::*;
use celkit_core::internal::Number;

#[derive(PartialEq)]
pub(crate) enum Token {
    StructMarker,       // @
    ArrayOpen,          // [
    ArrayClose,         // ]
    TupleOpen,          // (
    TupleClose,         // )
    ObjectOpen,         // {
    ObjectClose,        // }
    FieldAssign,        // =
    KeyAssign,          // :
    Separator,          // ,
    Literal(String),    // "quoted string"
    Numeric(Number),    // 69, 4.20, etc.
    Boolean(bool),      // true/false
    Identifier(String), // unquoted_identifier
    Null,               // null
    Eof,                // End of input
}

impl Token {
    pub fn get_char(&self) -> Option<char> {
        match self {
            Token::StructMarker => Some('@'),
            Token::ArrayOpen => Some('['),
            Token::ArrayClose => Some(']'),
            Token::TupleOpen => Some('('),
            Token::TupleClose => Some(')'),
            Token::ObjectOpen => Some('{'),
            Token::ObjectClose => Some('}'),
            Token::FieldAssign => Some('='),
            Token::KeyAssign => Some(':'),
            Token::Separator => Some(','),
            Token::Literal(_) => None,
            Token::Numeric(_) => None,
            Token::Boolean(_) => None,
            Token::Identifier(_) => None,
            Token::Null => None,
            Token::Eof => None,
        }
    }
}

impl core::fmt::Display for Token {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Token::StructMarker => core::write!(f, "@"),
            Token::ArrayOpen => core::write!(f, "["),
            Token::ArrayClose => core::write!(f, "]"),
            Token::TupleOpen => core::write!(f, "("),
            Token::TupleClose => core::write!(f, ")"),
            Token::ObjectOpen => core::write!(f, "{{"),
            Token::ObjectClose => core::write!(f, "}}"),
            Token::FieldAssign => core::write!(f, "="),
            Token::KeyAssign => core::write!(f, ":"),
            Token::Separator => core::write!(f, ","),
            Token::Literal(l) => core::write!(f, "\"{}\"", l),
            Token::Numeric(n) => core::write!(f, "{}", n),
            Token::Boolean(b) => core::write!(f, "{}", b),
            Token::Identifier(i) => core::write!(f, "{}", i),
            Token::Null => core::write!(f, "null"),
            Token::Eof => core::write!(f, ""),
        }
    }
}
