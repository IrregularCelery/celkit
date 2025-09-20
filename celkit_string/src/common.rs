// TODO: Make error messages more consistant across components
// TODO: It's probably better to have multiple Error::new() methods for separating serializing
// and deserializing, e.g. Error::serialize(), Error::deserializ() to make better error message.
// TODO: Change the `abs_n` to use the bitwise of the float instead of manual check.
// TODO: Perhaps it'd be nice to have an attribute, that changes the way the special types are
// represented; for example, #[celkit(as = "tuple")] for Range, would serialize into (1, 3)
// instead of {"start": 1, "end": 3}, or maybe it must be a setting for the encoders?

use celkit_core::internal::sys::*;
use celkit_core::internal::Number;

macro_rules! impl_for_token {
    ($($variant:ident => $char:literal),* $(,)?) => {
        impl Token {
            pub fn to_char(&self) -> Option<char> {
                match self {
                    $(Token::$variant => Some($char),)*
                    _ => None,
                }
            }

            pub fn from_char(c: char) -> Option<Token> {
                match c {
                    $($char => Some(Token::$variant),)*
                    _ => None,
                }
            }
        }

        impl core::fmt::Display for Token {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match self {
                    $(Token::$variant => core::write!(f, "{}", $char),)*
                    Token::Literal(l) => core::write!(f, "\"{}\"", l),
                    Token::Numeric(n) => core::write!(f, "{}", n),
                    Token::Boolean(b) => core::write!(f, "{}", b),
                    Token::Identifier(i) => core::write!(f, "{}", i),
                    Token::Null => core::write!(f, "null"),
                    Token::Eof => core::write!(f, ""),
                }
            }
        }
    }
}

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
    CommentMarker,      // /
    CommentMultiline,   // *
    Literal(String),    // "quoted string"
    Numeric(Number),    // 69, 4.20, etc.
    Boolean(bool),      // true/false
    Identifier(String), // unquoted_identifier
    Null,               // null
    Eof,                // End of input
}

impl_for_token! {
    StructMarker => '@',
    ArrayOpen => '[',
    ArrayClose => ']',
    TupleOpen => '(',
    TupleClose => ')',
    ObjectOpen => '{',
    ObjectClose => '}',
    FieldAssign => '=',
    KeyAssign => ':',
    Separator => ',',
    CommentMarker =>  '/',
    CommentMultiline =>  '*',
}
