use celkit_core::internal::sys::*;

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
    use celkit_core::internal::sys::*;
    use celkit_core::internal::{Number, Result, Value};

    pub struct Encoder {
        value: Value,
    }

    impl Encoder {
        pub fn new(value: Value) -> Self {
            Self { value }
        }

        pub fn encode(self) -> Result<String> {
            self.encode_value(&self.value)
        }

        fn encode_null(&self) -> Result<String> {
            Ok("null".to_string())
        }

        fn encode_boolean(&self, value: &bool) -> Result<String> {
            Ok(value.to_string())
        }

        fn encode_number(&self, value: &Number) -> Result<String> {
            Ok(value.to_string())
        }

        fn encode_text(&self, value: &String) -> Result<String> {
            Ok(format!("\"{}\"", escape_text(value)))
        }

        fn encode_array(&self, value: &Vec<Value>) -> Result<String> {
            let items: Result<Vec<String>> = value.iter().map(|v| self.encode_value(v)).collect();
            let items = items?;

            Ok(format!("[{}]", items.join(",")))
        }

        fn encode_tuple(&self, value: &Vec<Value>) -> Result<String> {
            let members: Result<Vec<String>> = value.iter().map(|v| self.encode_value(v)).collect();
            let members = members?;

            Ok(format!("({})", members.join(",")))
        }

        fn encode_object(&self, value: &BTreeMap<String, Value>) -> Result<String> {
            let entries: Result<Vec<String>> = value
                .iter()
                .map(|(k, v)| Ok(format!("\"{}\":{}", escape_text(k), self.encode_value(v)?)))
                .collect();
            let entries = entries?;

            Ok(format!("{{{}}}", entries.join(",")))
        }

        fn encode_struct(&self, value: &BTreeMap<String, Value>) -> Result<String> {
            let fields: Result<Vec<String>> = value
                .iter()
                .map(|(k, v)| Ok(format!("{}={}", k, self.encode_value(v)?)))
                .collect();
            let fields = fields?;

            Ok(format!("@({})", fields.join(",")))
        }

        fn encode_value(&self, value: &Value) -> Result<String> {
            match value {
                Value::Null => self.encode_null(),
                Value::Boolean(b) => self.encode_boolean(b),
                Value::Number(n) => self.encode_number(n),
                Value::Text(t) => self.encode_text(t),
                Value::Array(a) => self.encode_array(a),
                Value::Tuple(t) => self.encode_tuple(t),
                Value::Object(o) => self.encode_object(o),
                Value::Struct(_, s) => self.encode_struct(s),
            }
        }
    }
}

/// Prettified encoding (multi-line)
mod pretty {
    use crate::encode::escape_text;
    use celkit_core::internal::sys::*;
    use celkit_core::internal::{Result, Value};

    pub struct Encoder {
        value: Value,
        indent_size: usize,
        max_line_length: usize,
        trailing_comma: bool,
    }

    impl Encoder {
        pub fn new(value: Value) -> Self {
            Self {
                value,
                indent_size: 2,
                max_line_length: 100,
                trailing_comma: true,
            }
        }

        pub fn indent_size(mut self, size: usize) -> Self {
            self.indent_size = size;

            self
        }

        pub fn max_line_length(mut self, length: usize) -> Self {
            self.max_line_length = length;

            self
        }

        pub fn trailing_comma(mut self, enabled: bool) -> Self {
            self.trailing_comma = enabled;

            self
        }

        pub fn encode(self) -> Result<String> {
            self.encode_value(&self.value)
        }

        fn encode_value(&self, value: &Value) -> Result<String> {
            todo!()
        }
    }
}

pub fn to_string<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<String> {
    let serialized = value.serialize()?;

    pretty::Encoder::new(serialized).encode()
}

pub fn to_mini<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<mini::Encoder> {
    let serialized = value.serialize()?;

    Ok(mini::Encoder::new(serialized))
}

pub fn to_pretty<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<pretty::Encoder> {
    let serialized = value.serialize()?;

    Ok(pretty::Encoder::new(serialized))
}
