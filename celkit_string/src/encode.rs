use celkit_core::internal::sys::*;

fn escape_text(input: &str) -> String {
    let mut output = String::new();

    for c in input.chars() {
        match c {
            // Standard escape sequences
            '\x00' => output.push_str("\\0"), // Null character           \0
            '\x07' => output.push_str("\\a"), // Alert (Beep, Bell)       \a
            '\x08' => output.push_str("\\b"), // Backspace                \b
            '\x1B' => output.push_str("\\e"), // Escape character         \e
            '\x0C' => output.push_str("\\f"), // Formfeed Page Break      \f
            '\x0A' => output.push_str("\\n"), // Newline (Line Feed)      \n
            '\x0D' => output.push_str("\\r"), // Carriage Return          \r
            '\x09' => output.push_str("\\t"), // Horizontal Tab           \t
            '\x0B' => output.push_str("\\v"), // Vertical Tab             \v

            // `Cel` Format-specific characters
            '\x5C' => output.push_str("\\\\"), // Backslash                \\
            '\x22' => output.push_str("\\\""), // Double quotation mark    \"

            // Control characters
            c if c.is_control() => {
                output.push_str(&format!("\\u{:04X}", c as u32));
            }

            // Everything else passes through
            c => output.push(c),
        }
    }

    output
}

/// Minified encoding (single-line)
mod mini {
    use crate::common::Token;
    use crate::encode::escape_text;
    use celkit_core::internal::sys::*;
    use celkit_core::internal::{Number, Result, Value};

    pub struct Encoder {
        input: Value,
    }

    impl Encoder {
        pub fn new(input: Value) -> Self {
            Self { input }
        }

        pub fn encode(self) -> Result<String> {
            self.encode_value(&self.input)
        }

        fn encode_null(&self) -> Result<String> {
            Ok(Token::Null.to_string())
        }

        fn encode_boolean(&self, value: &bool) -> Result<String> {
            Ok(value.to_string())
        }

        fn encode_number(&self, value: &Number) -> Result<String> {
            macro_rules! encode_float {
                ($num:expr) => {{
                    let n = $num;

                    if n.is_nan() {
                        return Ok("nan".to_string());
                    }

                    if n.is_infinite() {
                        if n.is_sign_positive() {
                            return Ok("inf".to_string());
                        }

                        return Ok("-inf".to_string());
                    }

                    if *n == 0.0 {
                        if n.is_sign_positive() {
                            return Ok("0.0".to_string());
                        }

                        return Ok("-0.0".to_string());
                    }

                    let abs_n = if *n < 0.0 { -*n } else { *n };

                    // For the minified version we almost always use the exponent notation
                    let upper_threshold = 1e4;
                    let lower_threshold = 1e-4;

                    if abs_n > upper_threshold || (abs_n <= lower_threshold && abs_n > 0.0) {
                        return Ok(format!("{:e}", n));
                    }

                    let string = n.to_string();

                    // If the string doesn't have a decimal point or an exponent, add '.0' for
                    // better distinction between floats and integers
                    if !string.contains('.') && !string.contains('e') && !string.contains('E') {
                        return Ok(format!("{}.0", string));
                    }

                    Ok(string)
                }};
            }

            match value {
                Number::F32(n) => encode_float!(n),
                Number::F64(n) => encode_float!(n),
                integer => return Ok(integer.to_string()),
            }
        }

        fn encode_text(&self, value: &String) -> Result<String> {
            Ok(format!("\"{}\"", escape_text(value)))
        }

        fn encode_array(&self, value: &Vec<Value>) -> Result<String> {
            let items: Result<Vec<String>> = value.iter().map(|v| self.encode_value(v)).collect();
            let items = items?;

            Ok(format!(
                "{}{}{}",
                Token::ArrayOpen,
                items.join(&Token::Separator.to_string()),
                Token::ArrayClose
            ))
        }

        fn encode_tuple(&self, value: &Vec<Value>) -> Result<String> {
            let members: Result<Vec<String>> = value.iter().map(|v| self.encode_value(v)).collect();
            let members = members?;

            Ok(format!(
                "{}{}{}",
                Token::TupleOpen,
                members.join(&Token::Separator.to_string()),
                Token::TupleClose
            ))
        }

        fn encode_object(&self, value: &BTreeMap<String, Value>) -> Result<String> {
            let entries: Result<Vec<String>> = value
                .iter()
                .map(|entry| {
                    Ok(format!(
                        "\"{}\"{}{}",
                        escape_text(entry.0), // Entry key
                        Token::KeyAssign,
                        self.encode_value(entry.1)? // Entry value
                    ))
                })
                .collect();
            let entries = entries?;

            Ok(format!(
                "{}{}{}",
                Token::ObjectOpen,
                entries.join(&Token::Separator.to_string()),
                Token::ObjectClose
            ))
        }

        fn encode_struct(&self, value: &Vec<(String, Value)>) -> Result<String> {
            let fields: Result<Vec<String>> = value
                .iter()
                .map(|field| {
                    // Handle tuple(unnamed fields) structs (e.g. MyStruct(i32, bool, String))
                    if field.0.is_empty() {
                        return Ok(self.encode_value(&field.1)?);
                    }

                    Ok(format!(
                        "{}{}{}",
                        field.0, // Field name
                        Token::FieldAssign,
                        self.encode_value(&field.1)? // Field value
                    ))
                })
                .collect();
            let fields = fields?;

            Ok(format!(
                "{}{}{}{}",
                Token::StructMarker,
                Token::TupleOpen,
                fields.join(&Token::Separator.to_string()),
                Token::TupleClose
            ))
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
                Value::Struct(s) => self.encode_struct(s),
            }
        }
    }
}

/// Prettified encoding (multi-line)
mod pretty {
    use crate::common::Token;
    use crate::encode::escape_text;
    use celkit_core::internal::sys::*;
    use celkit_core::internal::{Number, Result, Value};

    pub struct Encoder {
        input: Value,
        indent_size: usize,
        max_line_length: usize,
        trailing_comma: bool,
    }

    impl Encoder {
        pub fn new(input: Value) -> Self {
            Self {
                input,
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
            let depth = 0;
            let available_line_length = None;

            self.encode_value(&self.input, depth, available_line_length)
        }

        fn indent(&self, level: usize) -> String {
            " ".repeat(level * self.indent_size)
        }

        fn encode_null(&self) -> Result<String> {
            Ok(Token::Null.to_string())
        }

        fn encode_boolean(&self, value: &bool) -> Result<String> {
            Ok(value.to_string())
        }

        fn encode_number(&self, value: &Number) -> Result<String> {
            macro_rules! encode_float {
                ($num:expr) => {{
                    let n = $num;

                    if n.is_nan() {
                        return Ok("nan".to_string());
                    }

                    if n.is_infinite() {
                        if n.is_sign_positive() {
                            return Ok("inf".to_string());
                        }

                        return Ok("-inf".to_string());
                    }

                    if *n == 0.0 {
                        if n.is_sign_positive() {
                            return Ok("0.0".to_string());
                        }

                        return Ok("-0.0".to_string());
                    }

                    let abs_n = if *n < 0.0 { -*n } else { *n };

                    // Use exponent notation for very large or very small numbers
                    let upper_threshold = 1e15;
                    let lower_threshold = 1e-5;

                    if abs_n > upper_threshold || (abs_n < lower_threshold && abs_n > 0.0) {
                        return Ok(format!("{:e}", n));
                    }

                    let string = n.to_string();

                    // If the string doesn't have a decimal point or an exponent, add '.0' for
                    // better distinction between floats and integers
                    if !string.contains('.') && !string.contains('e') && !string.contains('E') {
                        return Ok(format!("{}.0", string));
                    }

                    Ok(string)
                }};
            }

            match value {
                Number::F32(n) => encode_float!(n),
                Number::F64(n) => encode_float!(n),
                integer => return Ok(integer.to_string()),
            }
        }

        fn encode_text(&self, value: &String) -> Result<String> {
            Ok(format!("\"{}\"", escape_text(value)))
        }

        fn encode_array(
            &self,
            value: &Vec<Value>,
            depth: usize,
            available_line_length: Option<usize>,
        ) -> Result<String> {
            if value.is_empty() {
                return Ok(format!("{}{}", Token::ArrayOpen, Token::ArrayClose));
            }

            let current_indent = self.indent(depth);
            let next_indent = self.indent(depth + 1);

            let mut items = Vec::new();
            let mut single_line_length = 0;
            let mut has_struct_item = false;

            single_line_length += 1; // ArrayOpen

            for (i, item) in value.iter().enumerate() {
                if !has_struct_item && matches!(item, Value::Struct(_)) {
                    has_struct_item = true;
                }

                let encoded_item = self.encode_value(item, depth + 1, None)?;

                items.push(encoded_item);

                single_line_length += items[i].len();

                if i < value.len() - 1 {
                    single_line_length += 2; // Separator and space
                }
            }

            single_line_length += 1; // ArrayClose

            // It's safe to assume this value is a child (nested) element if
            // the `depth` is non-zero. So, we add `1` to the length of the line
            // to account for a possible comma from the parent.
            let comma_allowance = if depth > 0 { 1 } else { 0 };

            let default_max_length = self
                .max_line_length
                .saturating_sub(current_indent.len())
                .saturating_sub(comma_allowance);
            let single_line_max_length = available_line_length.unwrap_or(default_max_length);

            // Check if it exceeded the line limit
            let can_fit_single_line = single_line_length <= single_line_max_length;

            if can_fit_single_line && !has_struct_item {
                return Ok(format!(
                    "{}{}{}",
                    Token::ArrayOpen,
                    items.join(&format!("{} ", Token::Separator)),
                    Token::ArrayClose
                ));
            }

            if has_struct_item {
                // Each item has its own line

                let mut output = String::new();

                output.push_str(&Token::ArrayOpen.to_string());
                output.push('\n');

                for (i, encoded_item) in items.into_iter().enumerate() {
                    let mut formatted_item = encoded_item;

                    if i < value.len() - 1 || self.trailing_comma {
                        formatted_item.push_str(&format!("{}", Token::Separator));
                    }

                    output.push_str(&format!("{}{}", next_indent, formatted_item.trim_end()));
                    output.push('\n');
                }

                output.push_str(&current_indent);
                output.push_str(&Token::ArrayClose.to_string());

                return Ok(output);
            }

            let mut output = String::new();
            let mut current_line = next_indent.clone();
            let empty_line_len = next_indent.len();

            output.push_str(&Token::ArrayOpen.to_string());
            output.push('\n');

            for (i, encoded_item) in items.into_iter().enumerate() {
                let mut formatted_item = encoded_item;

                if i < value.len() - 1 || self.trailing_comma {
                    formatted_item.push_str(&format!("{} ", Token::Separator));
                }

                // Check if this item would fit in the current line
                if current_line.len() + formatted_item.len() <= self.max_line_length
                    || current_line.len() <= empty_line_len
                {
                    current_line.push_str(&formatted_item);

                    continue;
                }

                // Current line has content and would exceed the limit, wrap to next line
                output.push_str(&current_line.trim_end());
                output.push('\n');

                current_line = format!("{}{}", next_indent, formatted_item);
            }

            // Add the last line if it has content
            if current_line.len() > empty_line_len {
                output.push_str(&current_line.trim_end());
                output.push('\n');
            }

            output.push_str(&current_indent);
            output.push_str(&Token::ArrayClose.to_string());

            Ok(output)
        }

        fn encode_tuple(
            &self,
            value: &Vec<Value>,
            depth: usize,
            available_line_length: Option<usize>,
        ) -> Result<String> {
            if value.is_empty() {
                return Ok(format!("{}{}", Token::TupleOpen, Token::TupleClose));
            }

            let current_indent = self.indent(depth);
            let next_indent = self.indent(depth + 1);

            let mut members = Vec::new();
            let mut single_line_length = 0;
            let mut has_struct_member = false;

            single_line_length += 1; // TupleOpen

            for (i, member) in value.iter().enumerate() {
                if matches!(member, Value::Struct(_)) {
                    has_struct_member = true;
                }

                let encoded_member = self.encode_value(member, depth + 1, None)?;

                members.push(encoded_member);

                single_line_length += members[i].len();

                if i < value.len() - 1 {
                    single_line_length += 2; // Separator and space
                }
            }

            single_line_length += 1; // TupleClose

            // It's safe to assume this value is a child (nested) element if
            // the `depth` is non-zero. So, we add `1` to the length of the line
            // to account for a possible comma from the parent.
            let comma_allowance = if depth > 0 { 1 } else { 0 };

            let default_max_length = self
                .max_line_length
                .saturating_sub(current_indent.len())
                .saturating_sub(comma_allowance);
            let single_line_max_length = available_line_length.unwrap_or(default_max_length);

            // Check if it exceeded the line limit
            let can_fit_single_line = single_line_length <= single_line_max_length;

            if can_fit_single_line && !has_struct_member {
                return Ok(format!(
                    "{}{}{}",
                    Token::TupleOpen,
                    members.join(&format!("{} ", Token::Separator)),
                    Token::TupleClose
                ));
            }

            if has_struct_member {
                // Each member has its own line

                let mut output = String::new();

                output.push_str(&Token::TupleOpen.to_string());
                output.push('\n');

                for (i, encoded_member) in members.into_iter().enumerate() {
                    let mut formatted_member = encoded_member;

                    if i < value.len() - 1 || self.trailing_comma {
                        formatted_member.push_str(&format!("{}", Token::Separator));
                    }

                    output.push_str(&format!("{}{}", next_indent, formatted_member.trim_end()));
                    output.push('\n');
                }

                output.push_str(&current_indent);
                output.push_str(&Token::TupleClose.to_string());

                return Ok(output);
            }

            let mut output = String::new();
            let mut current_line = next_indent.clone();
            let empty_line_len = next_indent.len();

            output.push_str(&Token::TupleOpen.to_string());
            output.push('\n');

            for (i, encoded_member) in members.into_iter().enumerate() {
                let mut formatted_member = encoded_member;

                if i < value.len() - 1 || self.trailing_comma {
                    formatted_member.push_str(&format!("{} ", Token::Separator));
                }

                // Check if this member would fit in the current line
                if current_line.len() + formatted_member.len() <= self.max_line_length
                    || current_line.len() <= empty_line_len
                {
                    current_line.push_str(&formatted_member);

                    continue;
                }

                // Current line has content and would exceed the limit, wrap to next line
                output.push_str(&current_line.trim_end());
                output.push('\n');

                current_line = format!("{}{}", next_indent, formatted_member);
            }

            // Add the last line if it has content
            if current_line.len() > empty_line_len {
                output.push_str(&current_line.trim_end());
                output.push('\n');
            }

            output.push_str(&current_indent);
            output.push_str(&Token::TupleClose.to_string());

            Ok(output)
        }

        fn encode_object(
            &self,
            value: &BTreeMap<String, Value>,
            depth: usize,
            available_line_length: Option<usize>,
        ) -> Result<String> {
            if value.is_empty() {
                return Ok(format!("{}{}", Token::ObjectOpen, Token::ObjectClose));
            }

            let current_indent = self.indent(depth);
            let next_indent = self.indent(depth + 1);

            let mut entries = Vec::new();
            let mut single_line_length = 0;
            let mut has_struct_entry = false;

            single_line_length += 1; // ObjectOpen

            for (i, entry) in value.iter().enumerate() {
                if !has_struct_entry && matches!(entry.1, Value::Struct(_)) {
                    has_struct_entry = true;
                }

                let key_length = entry.0.len() + 4; // 2 double quotations + KeyAssign + space

                let available_length = self
                    .max_line_length
                    .saturating_sub(next_indent.len())
                    .saturating_sub(key_length);

                let encoded_entry = format!(
                    "\"{}\"{} {}",
                    escape_text(entry.0), // Entry key
                    Token::KeyAssign,
                    self.encode_value(entry.1, depth + 1, Some(available_length))? // Entry value
                );

                entries.push(encoded_entry);

                single_line_length += entries[i].len();

                if i < value.len() - 1 {
                    single_line_length += 2; // Separator and space
                }
            }

            single_line_length += 1; // ObjectClose

            // It's safe to assume this value is a child (nested) element if
            // the `depth` is non-zero. So, we add `1` to the length of the line
            // to account for a possible comma from the parent.
            let comma_allowance = if depth > 0 { 1 } else { 0 };

            let default_max_length = self
                .max_line_length
                .saturating_sub(current_indent.len())
                .saturating_sub(comma_allowance);
            let single_line_max_length = available_line_length.unwrap_or(default_max_length);

            // Check if it exceeded the line limit
            let can_fit_single_line = single_line_length <= single_line_max_length;

            if can_fit_single_line && !has_struct_entry {
                return Ok(format!(
                    "{}{}{}",
                    Token::ObjectOpen,
                    entries.join(&format!("{} ", Token::Separator)),
                    Token::ObjectClose
                ));
            }

            if has_struct_entry {
                // Each entry has its own line

                let mut output = String::new();

                output.push_str(&Token::ObjectOpen.to_string());
                output.push('\n');

                for (i, encoded_entry) in entries.into_iter().enumerate() {
                    let mut formatted_entry = encoded_entry;

                    if i < value.len() - 1 || self.trailing_comma {
                        formatted_entry.push_str(&format!("{}", Token::Separator));
                    }

                    output.push_str(&format!("{}{}", next_indent, formatted_entry.trim_end()));
                    output.push('\n');
                }

                output.push_str(&current_indent);
                output.push_str(&Token::ObjectClose.to_string());

                return Ok(output);
            }

            let mut output = String::new();
            let mut current_line = next_indent.clone();
            let empty_line_len = next_indent.len();

            output.push_str(&Token::ObjectOpen.to_string());
            output.push('\n');

            for (i, encoded_entry) in entries.into_iter().enumerate() {
                let mut formatted_entry = encoded_entry;

                if i < value.len() - 1 || self.trailing_comma {
                    formatted_entry.push_str(&format!("{} ", Token::Separator));
                }

                // Check if this entry would fit in the current line
                if current_line.len() + formatted_entry.len() <= self.max_line_length
                    || current_line.len() <= empty_line_len
                {
                    current_line.push_str(&formatted_entry);

                    continue;
                }

                // Current line has content and would exceed the limit, wrap to next line
                output.push_str(&current_line.trim_end());
                output.push('\n');

                current_line = format!("{}{}", next_indent, formatted_entry);
            }

            // Add the last line if it has content
            if current_line.len() > empty_line_len {
                output.push_str(&current_line.trim_end());
                output.push('\n');
            }

            output.push_str(&current_indent);
            output.push_str(&Token::ObjectClose.to_string());

            Ok(output)
        }

        fn encode_struct(
            &self,
            value: &Vec<(String, Value)>,
            depth: usize,
            _available_line_length: Option<usize>,
        ) -> Result<String> {
            if value.is_empty() {
                return Ok(format!(
                    "{}{}{}",
                    Token::StructMarker,
                    Token::TupleOpen,
                    Token::TupleClose
                ));
            }

            let current_indent = self.indent(depth);
            let next_indent = self.indent(depth + 1);

            let fields: Result<Vec<String>> = value
                .iter()
                .map(|field| {
                    let key_length = field.0.len() + 3; // FieldAssign + 2 spaces

                    let available_length = self
                        .max_line_length
                        .saturating_sub(next_indent.len())
                        .saturating_sub(key_length);

                    // Handle tuple(unnamed fields) structs (e.g. MyStruct(i32, bool, String))
                    if field.0.is_empty() {
                        return Ok(self.encode_value(
                            &field.1,
                            depth + 1,
                            Some(available_length),
                        )?);
                    }

                    Ok(format!(
                        "{} {} {}",
                        field.0, // Field name
                        Token::FieldAssign,
                        self.encode_value(&field.1, depth + 1, Some(available_length))? // Field value
                    ))
                })
                .collect();
            let fields = fields?;

            let mut output = String::new();
            let mut current_line = next_indent.clone();
            let empty_line_len = next_indent.len();

            output.push_str(&format!("{}{}", Token::StructMarker, Token::TupleOpen));

            for (i, encoded_field) in fields.into_iter().enumerate() {
                let mut formatted_field = encoded_field;

                if i < value.len() - 1 || self.trailing_comma {
                    formatted_field.push_str(&format!("{} ", Token::Separator));
                }

                // Each field has its own line
                output.push_str(&current_line.trim_end());
                output.push('\n');

                current_line = format!("{}{}", next_indent, formatted_field);
            }

            // Add the last line if it has content
            if current_line.len() > empty_line_len {
                output.push_str(&current_line.trim_end());
                output.push('\n');
            }

            output.push_str(&current_indent);
            output.push_str(&Token::TupleClose.to_string());

            Ok(output)
        }

        fn encode_value(
            &self,
            value: &Value,
            depth: usize,
            available_line_length: Option<usize>,
        ) -> Result<String> {
            match value {
                Value::Null => self.encode_null(),
                Value::Boolean(b) => self.encode_boolean(b),
                Value::Number(n) => self.encode_number(n),
                Value::Text(t) => self.encode_text(t),
                Value::Array(a) => self.encode_array(a, depth, available_line_length),
                Value::Tuple(t) => self.encode_tuple(t, depth, available_line_length),
                Value::Object(o) => self.encode_object(o, depth, available_line_length),
                Value::Struct(s) => self.encode_struct(s, depth, available_line_length),
            }
        }
    }
}

pub fn to_string<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<String> {
    let serialized = T::serialize(&value)?;

    pretty::Encoder::new(serialized).encode()
}

pub fn to_mini<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<mini::Encoder> {
    let serialized = T::serialize(&value)?;

    Ok(mini::Encoder::new(serialized))
}

pub fn to_pretty<T: ?Sized + celkit_core::Serialize>(
    value: &T,
) -> celkit_core::internal::Result<pretty::Encoder> {
    let serialized = T::serialize(&value)?;

    Ok(pretty::Encoder::new(serialized))
}
