// Minified formatting (single-line compact)
mod general {
    pub(crate) fn escape_text(input: &str) -> String {
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
}

mod mini {
    use crate::encode::general::escape_text;
    use celkit_core::internal::{Result, Value};

    pub(crate) fn encode_value(value: &Value) -> Result<String> {
        match value {
            Value::Null => Ok("null".to_string()),
            Value::Boolean(b) => Ok(b.to_string()),
            Value::Number(n) => Ok(n.to_string()),
            Value::Text(t) => Ok(format!("\"{}\"", escape_text(t))),
            Value::Array(a) => {
                let items: Result<Vec<String>> = a.iter().map(encode_value).collect();
                let items = items?;

                Ok(format!("[{}]", items.join(",")))
            }
            Value::Tuple(t) => {
                let members: Result<Vec<String>> = t.iter().map(encode_value).collect();
                let members = members?;

                Ok(format!("({})", members.join(",")))
            }
            Value::Object(o) => {
                let mut entries = Vec::new();

                for (key, value) in o {
                    entries.push(format!("\"{}\":{}", escape_text(key), encode_value(value)?));
                }

                Ok(format!("{{{}}}", entries.join(",")))
            }
            Value::Struct(_, s) => {
                let mut fields = Vec::new();

                for (key, value) in s {
                    fields.push(format!("{}={}", key, encode_value(value)?));
                }

                Ok(format!("@({})", fields.join(",")))
            }
        }
    }
}

pub fn to_string<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<String> {
    let serialized = value.serialize()?;

    // TODO: CHANGE THIS!!!
    // `to_string()` is supposed to be the default function with the prettified format
    mini::encode_value(&serialized)
}
