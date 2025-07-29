fn escape_text(input: &str) -> String {
    let mut output = String::new();

    for c in input.chars() {
        match c {
            '\x08' => output.push_str("\\b"), // Backspace                \b
            '\x0C' => output.push_str("\\f"), // Formfeed Page Break      \f
            '\n' => output.push_str("\\n"),   // Newline (Line Feed)      \n
            '\r' => output.push_str("\\r"),   // Carriage Return          \r
            '\t' => output.push_str("\\t"),   // Horizontal Tab           \t
            '\\' => output.push_str("\\\\"),  // Backslash                \\
            '"' => output.push_str("\\\""),   // Double quotation mark    \"
            c if c.is_control() => {
                output.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => output.push(c),
        }
    }

    output
}

/// Minified encoding (single-line)
mod mini {
    use crate::encode::escape_text;
    use celkit_core::internal::{Result, Value};

    pub struct Encoder<'a> {
        value: &'a Value,
    }

    impl<'a> Encoder<'a> {
        pub fn new(value: &'a Value) -> Self {
            Self { value }
        }

        pub fn encode(self) -> Result<String> {
            self.encode_value(self.value)
        }

        fn encode_value(&self, value: &Value) -> Result<String> {
            match value {
                Value::Null => Ok("null".to_string()),
                Value::Boolean(b) => Ok(b.to_string()),
                Value::Number(n) => Ok(n.to_string()),
                Value::Text(t) => Ok(format!("\"{}\"", escape_text(t))),
                Value::Array(a) => {
                    let items: Result<Vec<String>> =
                        a.iter().map(|v| self.encode_value(v)).collect();
                    let items = items?;

                    Ok(format!("[{}]", items.join(",")))
                }
                Value::Tuple(t) => {
                    let members: Result<Vec<String>> =
                        t.iter().map(|v| self.encode_value(v)).collect();
                    let members = members?;

                    Ok(format!("({})", members.join(",")))
                }
                Value::Object(o) => {
                    let entries: Result<Vec<String>> = o
                        .iter()
                        .map(|(k, v)| {
                            Ok(format!("\"{}\":{}", escape_text(k), self.encode_value(v)?))
                        })
                        .collect();
                    let entries = entries?;

                    Ok(format!("{{{}}}", entries.join(",")))
                }
                Value::Struct(_, s) => {
                    let fields: Result<Vec<String>> = s
                        .iter()
                        .map(|(k, v)| Ok(format!("{}={}", k, self.encode_value(v)?)))
                        .collect();
                    let fields = fields?;

                    Ok(format!("@({})", fields.join(",")))
                }
            }
        }
    }
}

mod pretty {}

pub fn to_string<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<String> {
    let _serialized = value.serialize()?;

    // TODO: CHANGE THIS!!!
    // `to_string()` is supposed to be the default function with the prettified format
    todo!()
}

pub fn to_mini<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<String> {
    let serialized = value.serialize()?;

    mini::Encoder::new(&serialized).encode()
}
