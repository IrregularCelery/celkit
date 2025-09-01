use crate::common::Token;
use celkit_core::internal::sys::*;
use celkit_core::internal::{Error, Number, Result, Value};

pub struct Decoder {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
    line: usize,
    column: usize,
}

impl Decoder {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current_char = chars.get(0).copied();

        Self {
            input: chars,
            position: 0,
            current_char,
            line: 1,
            column: 1,
        }
    }

    pub fn decode(mut self) -> Result<Value> {
        self.decode_value()
    }

    fn error(&self, message: impl Into<String>) -> Error {
        let radius = 10; // Radius around the position for context
        let start = self.position.saturating_sub(radius);
        let end = (self.position + radius + 1).min(self.input.len());

        let mut context: String = self.input[start..end].iter().collect();

        context.push('\n');
        context.push_str(&" ".repeat(self.position - start));
        context.push('^');

        Error::with_context(message, context, self.line, self.column)
    }

    fn save_checkpoint(&self) -> (usize, Option<char>, usize, usize) {
        (self.position, self.current_char, self.line, self.column)
    }

    fn load_checkpoint(&mut self, checkpoint: (usize, Option<char>, usize, usize)) {
        let (position, currect_char, line, column) = checkpoint;

        self.position = position;
        self.current_char = currect_char;
        self.line = line;
        self.column = column;
    }

    fn peek(&self) -> Option<&char> {
        self.input.get(self.position + 1)
    }

    fn advance(&mut self) {
        match self.current_char {
            Some('\n') => {
                self.line += 1;
                self.column = 1;
            }
            Some(_) => {
                self.column += 1;
            }
            None => {} // End of input
        }

        self.position += 1;
        self.current_char = self.input.get(self.position).copied();
    }

    fn skip_comment_and_whitespace(&mut self) -> Result<()> {
        let comment_marker = Token::CommentMarker
            .to_char()
            .expect("This SHOULD never happen for single-char tokens!");
        let comment_multiline = Token::CommentMultiline
            .to_char()
            .expect("This SHOULD never happen for single-char tokens!");

        loop {
            // Skip whitespaces
            while let Some(c) = self.current_char {
                if !c.is_whitespace() {
                    break;
                }

                self.advance(); // Consume the whitespace
            }

            // Check for first comment character
            if self.current_char != Some(comment_marker) {
                break; // No more comment or whitespace
            }

            // Check for second comment character
            let Some(next_c) = self.peek() else {
                break; // End of input
            };

            if *next_c == comment_marker {
                // Single-line comment

                self.advance(); // Consume first CommentMarker
                self.advance(); // Consume second CommentMarker

                while let Some(c) = self.current_char {
                    if c == '\n' {
                        self.advance(); // Consume the newline

                        break;
                    }

                    self.advance();
                }

                continue;
            }

            if *next_c == comment_multiline {
                // Multi-line comment

                self.advance(); // Consume CommentMarker
                self.advance(); // Consume CommentMultiline

                let mut depth = 1;

                while let Some(c) = self.current_char {
                    if c == comment_marker {
                        // Check for nested comment

                        let Some(next_c) = self.peek() else {
                            self.advance(); // Consume the character to trigger end of input

                            continue;
                        };

                        if *next_c == comment_multiline {
                            self.advance(); // Consume CommentMarker
                            self.advance(); // Consume CommentMultiline

                            depth += 1;

                            continue;
                        }
                    }

                    if c == comment_multiline {
                        // Check for end of multi-line comment

                        let Some(next_c) = self.peek() else {
                            self.advance(); // Consume the character to trigger end of input

                            continue;
                        };

                        if *next_c == comment_marker {
                            self.advance(); // Consume CommentMultiline
                            self.advance(); // Consume CommentMarker

                            depth -= 1;

                            if depth == 0 {
                                break; // End of outermost comment
                            }

                            continue;
                        }
                    }

                    self.advance(); // Consume the character in the comment
                }

                if depth > 0 {
                    return Err(self.error("Unterminated multi-line comment"));
                }

                continue;
            }

            break; // Not a comment
        }

        Ok(())
    }

    fn find_char(&mut self, c: char) -> Result<Token> {
        if let Some(token) = Token::from_char(c) {
            self.advance();

            return Ok(token);
        }

        Err(self.error(format!("Unexpected character: '{}'", c)))
    }

    fn find_literal(&mut self) -> Result<Token> {
        let mut literal = String::new();
        let start = (self.line, self.column);

        self.advance(); // Consume the opening double quotes

        while let Some(c) = self.current_char {
            match c {
                '"' => {
                    self.advance(); // Consume the closing double quotes

                    return Ok(Token::Literal(literal));
                }
                '\\' => {
                    self.advance(); // Consume the backslash

                    match self.current_char {
                        // Standard escape sequences
                        Some('0') => literal.push('\x00'), // Null character           \0
                        Some('a') => literal.push('\x07'), // Alert (Beep, Bell)       \a
                        Some('b') => literal.push('\x08'), // Backspace                \b
                        Some('e') => literal.push('\x1B'), // Escape character         \e
                        Some('f') => literal.push('\x0C'), // Formfeed Page Break      \f
                        Some('n') => literal.push('\x0A'), // Newline (Line Feed)      \n
                        Some('r') => literal.push('\x0D'), // Carriage Return          \r
                        Some('t') => literal.push('\x09'), // Horizontal Tab           \t
                        Some('v') => literal.push('\x0B'), // Vertical Tab             \v

                        // `Cel` Format-specific characters
                        Some('\\') => literal.push('\x5C'), // Backslash                \\
                        Some('"') => literal.push('\x22'),  // Double quotation mark    \"

                        // Unicode codes
                        Some('u') => {
                            self.advance(); // Consume character 'u'

                            let mut hex = String::new();

                            // Read the next 4 characters
                            for i in 0..4 {
                                match self.current_char {
                                    Some(c) if c.is_ascii_hexdigit() => {
                                        hex.push(c);

                                        self.advance(); // Consume the hex digit character
                                    }
                                    _ => {
                                        return Err(self.error(format!(
                                            "Invalid Unicode escape sequence: \
                                            Expected 4 hex digits found {}",
                                            i
                                        )));
                                    }
                                }
                            }

                            // Parse the hex string to u32
                            match u32::from_str_radix(&hex, 16) {
                                Ok(code_point) => match char::from_u32(code_point) {
                                    Some(c) => literal.push(c),
                                    None => {
                                        return Err(self.error(format!(
                                            "Invalid Unicode code point: U+{:04X}",
                                            code_point
                                        )))
                                    }
                                },
                                Err(_) => {
                                    return Err(
                                        self.error("Invalid hex digits in Unicode escape sequence")
                                    )
                                }
                            }

                            continue; // Already consumed the characters
                        }

                        // Invalid character
                        Some(c) => {
                            return Err(
                                self.error(format!("Invalid escape sequence character: \\{}", c))
                            )
                        }

                        // Unexpected end
                        None => {
                            return Err(
                                self.error("Unexpected end of string literal escape sequence")
                            )
                        }
                    }

                    self.advance(); // Consume the escaped character
                }
                c => {
                    literal.push(c);

                    self.advance(); // Consume the character
                }
            }
        }

        Err(self.error(format!(
            "Unterminated string literal starting at line {}, column {}",
            start.0, start.1
        )))
    }

    fn find_numeric(&mut self) -> Result<Token> {
        let mut numeric = String::new();
        let mut has_decimal = false;
        let mut has_exponent = false;

        // Handle positive/negative sign
        if let Some(c) = self.current_char {
            if c == '+' || c == '-' {
                numeric.push(c);

                self.advance(); // Consume the sign character
            }
        }

        if let Some(_) = self.current_char {
            // Check if enough characters for "inf" or "nan"
            if self.position + 3 <= self.input.len() {
                let keyword: String = self.input[self.position..self.position + 3]
                    .iter()
                    .collect();
                let keyword = keyword.to_lowercase();

                if keyword == "inf" {
                    // Consume the three characters
                    for _ in 0..3 {
                        self.advance();
                    }

                    return Ok(Token::Numeric(Number::F64(if numeric != "-" {
                        f64::INFINITY
                    } else {
                        f64::NEG_INFINITY
                    })));
                }

                if keyword == "nan" {
                    // Consume the three characters
                    for _ in 0..3 {
                        self.advance();
                    }

                    // NaN doesn't have a sign so if the input contained one, we ignore it
                    return Ok(Token::Numeric(Number::F64(f64::NAN)));
                }
            }
        }

        // Must start with digit if not a keyword
        if !matches!(self.current_char, Some(c) if c.is_ascii_digit()) {
            return Err(self.error("Expected number".to_string()));
        }

        while let Some(c) = self.current_char {
            if c.is_ascii_digit() {
                numeric.push(c);

                self.advance(); // Consume the digit

                continue;
            }

            if c == '.' && !has_decimal {
                has_decimal = true;

                numeric.push(c);

                self.advance(); // Consume character '.'

                // Must have digit after decimal point
                if !matches!(self.current_char, Some(c) if c.is_ascii_digit()) {
                    return Err(self.error("Expected digit after decimal point".to_string()));
                }

                continue;
            }

            if (c == 'e' || c == 'E') && !has_exponent {
                has_exponent = true;

                numeric.push(c);

                self.advance(); // Consume character 'e'

                // Handle positive/negative sign after exponent
                if let Some(c) = self.current_char {
                    if c == '+' || c == '-' {
                        numeric.push(c);

                        self.advance(); // Consume the sign character
                    }
                }

                // Must have digit after exponent
                if !matches!(self.current_char, Some(c) if c.is_ascii_digit()) {
                    return Err(self.error("Expected digit after exponent".to_string()));
                }

                continue;
            }

            break;
        }

        if has_decimal || has_exponent {
            return match numeric.parse::<f64>() {
                Ok(f) => Ok(Token::Numeric(Number::F64(f))),
                Err(_) => Err(self.error(format!("Invalid float: '{}'", numeric))),
            };
        }

        macro_rules! try_parse {
            ($type:ty, $variant:ident) => {
                if let Ok(n) = numeric.parse::<$type>() {
                    return Ok(Token::Numeric(Number::$variant(n)));
                }
            };
        }

        // Try to fit in the smallest possible integer type
        if numeric.starts_with('-') {
            try_parse!(i8, I8);
            try_parse!(i16, I16);
            try_parse!(i32, I32);
            try_parse!(i64, I64);
            try_parse!(i128, I128);

            return Err(self.error(format!(
                "Integer is too large and exceeds the bounds of singed int: '{}'",
                numeric
            )));
        }

        // Try unsigned types first, then signed
        try_parse!(u8, U8);
        try_parse!(i8, I8);
        try_parse!(u16, U16);
        try_parse!(i16, I16);
        try_parse!(u32, U32);
        try_parse!(i32, I32);
        try_parse!(u64, U64);
        try_parse!(i64, I64);
        try_parse!(u128, U128);
        try_parse!(i128, I128);

        Err(self.error(format!(
            "Integer is too large and exceeds the bounds of unsinged int: '{}'",
            numeric
        )))
    }

    fn find_identifier_or_keyword(&mut self, expected_identifier: bool) -> Result<Token> {
        let start = self.position;

        // Handle raw identifier (r#keyword)
        let mut is_raw_identifier = false;

        if self.position + 1 < self.input.len() {
            is_raw_identifier =
                self.current_char == Some('r') && self.input[self.position + 1] == '#';
        }

        if is_raw_identifier {
            self.advance(); // Consume character 'r'
            self.advance(); // Consume character '#'
        }

        while let Some(c) = self.current_char {
            if !c.is_alphanumeric() && c != '_' {
                break;
            }

            self.advance();
        }

        let identifier: String = self.input[start..self.position].iter().collect();
        let identifier = identifier.to_lowercase();

        if expected_identifier {
            return Ok(Token::Identifier(identifier));
        }

        match identifier.as_str() {
            "inf" => Ok(Token::Numeric(Number::F64(f64::INFINITY))),
            "nan" => Ok(Token::Numeric(Number::F64(f64::NAN))),
            "true" => Ok(Token::Boolean(true)),
            "false" => Ok(Token::Boolean(false)),
            _ if identifier == Token::Null.to_string() => Ok(Token::Null),
            _ => Ok(Token::Identifier(identifier)),
        }
    }

    fn next_token_with_context(&mut self, expected_value: bool) -> Result<Token> {
        self.skip_comment_and_whitespace()?;

        match self.current_char {
            None => Ok(Token::Eof),
            Some('"') => self.find_literal(),
            Some(c) if c.is_ascii_digit() || c == '+' || c == '-' => self.find_numeric(),
            Some(c) if c.is_alphabetic() => self.find_identifier_or_keyword(!expected_value),
            Some(c) => self.find_char(c),
        }
    }

    fn next_token(&mut self) -> Result<Token> {
        self.next_token_with_context(true)
    }

    fn expect_token(&mut self, expected: Token, context: &str) -> Result<()> {
        let token = self.next_token()?;

        if token != expected {
            return Err(match token {
                Token::Eof => {
                    self.error(format!("Unexpected end of input while parsing {}", context))
                }
                _ => self.error(format!("Expected '{}' in {}", expected, context)),
            });
        }

        Ok(())
    }

    fn decode_struct(&mut self) -> Result<Value> {
        self.expect_token(
            Token::TupleOpen,
            &format!(
                "struct definition: (expected '{}' after '{}')",
                Token::TupleOpen,
                Token::StructMarker,
            ),
        )?;

        let mut fields = Vec::new();
        let mut is_empty = true;
        let mut is_unnamed_struct: Option<bool> = None; // e.g. MyStruct(i32, bool, String)

        loop {
            self.skip_comment_and_whitespace()?;

            // Check for TupleClose for handling empty structs
            if self.current_char == Token::TupleClose.to_char() {
                self.advance(); // Consume TupleClose

                break;
            }

            if !is_empty {
                self.expect_token(Token::Separator, "struct fields")?;

                self.skip_comment_and_whitespace()?;

                if self.current_char == Token::TupleClose.to_char() {
                    self.advance(); // Consume TupleClose

                    break;
                }
            }

            if is_unnamed_struct.is_none() {
                self.skip_comment_and_whitespace()?;

                is_unnamed_struct = match self.current_char {
                    // These indicate tuple(unnamed fields) struct (values, not names)
                    Some(c) if c == '"' || c.is_ascii_digit() || c == '+' || c == '-' => Some(true),
                    // Possibly an identifier
                    Some(c) if c.is_alphabetic() || c == 'r' => {
                        let checkpoint = self.save_checkpoint();

                        let is_tuple_struct = match self.find_identifier_or_keyword(false) {
                            Ok(Token::Identifier(_)) => {
                                self.skip_comment_and_whitespace()?;

                                match self.current_char {
                                    c if c == Token::FieldAssign.to_char()
                                        || c == Token::KeyAssign.to_char() =>
                                    {
                                        false
                                    }
                                    _ => true, // Assign character not found, possibly a value
                                }
                            }
                            _ => true, // Keywords(values)
                        };

                        self.load_checkpoint(checkpoint);

                        Some(is_tuple_struct)
                    }
                    _ => Some(true),
                }
            }

            // Decode field
            match is_unnamed_struct {
                Some(true) => {
                    let value = self.decode_value()?;

                    fields.push((String::new(), value));
                }
                Some(false) => {
                    let is_expecting_value = false;

                    // Next token must be an identifier
                    let token = self.next_token_with_context(is_expecting_value)?;

                    match token {
                        Token::Identifier(i /* Field name */) => {
                            let token = self.next_token()?;

                            if token != Token::FieldAssign {
                                return Err(match token {
                                    Token::KeyAssign => self.error(format!(
                                        "Found '{}' but expected '{}' after struct field name",
                                        Token::KeyAssign,
                                        Token::FieldAssign
                                    )),
                                    Token::Eof => self.error(format!(
                                        "Unexpected end of input after field name '{}'",
                                        i
                                    )),
                                    _ => self.error(format!(
                                        "Expected '{}' after struct field name '{}'",
                                        Token::FieldAssign,
                                        i
                                    )),
                                });
                            }

                            let value = self.decode_value()?;

                            fields.push((i, value));
                        }
                        Token::Eof => {
                            return Err(self.error(format!(
                                "Unexpected end of input while parsing struct: \
                                Missing closing '{}'",
                                Token::TupleClose
                            )))
                        }
                        Token::TupleClose => {
                            // The end of the struct or an empty struct, already handled
                        }
                        _ => return Err(self.error("Expected field name in struct")),
                    }
                }
                None => unreachable!(
                    "This SHOULD never happen because `is_unnamed_struct` is always something!"
                ),
            }

            is_empty = false;
        }

        Ok(Value::Struct(fields))
    }

    fn decode_array(&mut self) -> Result<Value> {
        let mut items = Vec::new();
        let mut is_empty = true;

        loop {
            self.skip_comment_and_whitespace()?;

            // Check for ArrayClose for handling empty arrays
            if self.current_char == Token::ArrayClose.to_char() {
                self.advance(); // Consume ArrayClose

                break;
            }

            if !is_empty {
                self.expect_token(Token::Separator, "array items")?;

                self.skip_comment_and_whitespace()?;

                if self.current_char == Token::ArrayClose.to_char() {
                    self.advance(); // Consume ArrayClose

                    break;
                }
            }

            // Decode item
            let value = self.decode_value()?;

            items.push(value);

            is_empty = false;
        }

        Ok(Value::Array(items))
    }

    fn decode_tuple(&mut self) -> Result<Value> {
        let mut members = Vec::new();
        let mut is_empty = true;

        loop {
            // Check for TupleClose for handling empty tuples
            self.skip_comment_and_whitespace()?;

            if self.current_char == Token::TupleClose.to_char() {
                self.advance(); // Consume TupleClose

                break;
            }

            if !is_empty {
                self.expect_token(Token::Separator, "tuple members")?;

                self.skip_comment_and_whitespace()?;

                if self.current_char == Token::TupleClose.to_char() {
                    self.advance(); // Consume TupleClose

                    break;
                }
            }

            // Decode member
            let value = self.decode_value()?;

            members.push(value);

            is_empty = false;
        }

        Ok(Value::Tuple(members))
    }

    fn decode_object(&mut self) -> Result<Value> {
        let mut entries = BTreeMap::new();
        let mut is_empty = true;

        loop {
            // Check for ObjectClose for handling empty objects
            self.skip_comment_and_whitespace()?;

            if self.current_char == Token::ObjectClose.to_char() {
                self.advance(); // Consume ObjectClose

                break;
            }

            if !is_empty {
                self.expect_token(Token::Separator, "object entries")?;

                self.skip_comment_and_whitespace()?;

                if self.current_char == Token::ObjectClose.to_char() {
                    self.advance(); // Consume ObjectClose

                    break;
                }
            }

            // Decode entry
            let token = self.next_token()?;

            match token {
                Token::Literal(l /* Entry key */) => {
                    let token = self.next_token()?;

                    if token != Token::KeyAssign {
                        return Err(match token {
                            Token::FieldAssign => self.error(format!(
                                "Found '{}' but expected '{}' after object key",
                                Token::FieldAssign,
                                Token::KeyAssign
                            )),
                            Token::Eof => self
                                .error(format!("Unexpected end of input after object key '{}'", l)),
                            _ => self.error(format!(
                                "Expected '{}' after object key '{}'",
                                Token::KeyAssign,
                                l
                            )),
                        });
                    }

                    let value = self.decode_value()?;

                    entries.insert(l, value);
                }
                Token::Eof => {
                    return Err(self.error(format!(
                        "Unexpected end of input while parsing object: Missing closing '{}'",
                        Token::TupleClose
                    )))
                }
                Token::ObjectClose => {} // The end of the object or an empty object
                Token::Identifier(i) => {
                    return Err(self.error(format!(
                        "Found unquoted identifier '{}' where object key was expected",
                        i
                    )))
                }
                _ => return Err(self.error("Expected string key in object")),
            }

            is_empty = false;
        }

        Ok(Value::Object(entries))
    }

    fn decode_value(&mut self) -> Result<Value> {
        let token = self.next_token()?;

        match token {
            Token::StructMarker => self.decode_struct(),
            Token::ArrayOpen => self.decode_array(),
            Token::ArrayClose => Err(self.error(format!(
                "Unexpected '{}' - found closing bracket without matching opening bracket",
                Token::ArrayClose
            ))),
            Token::TupleOpen => self.decode_tuple(),
            Token::TupleClose => Err(self.error(format!(
                "Unexpected '{}' - found closing parenthesis without matching opening parenthesis",
                Token::TupleClose,
            ))),
            Token::ObjectOpen => self.decode_object(),
            Token::ObjectClose => Err(self.error(format!(
                "Unexpected '{}' - found closing brace without matching opening brace",
                Token::ObjectClose
            ))),
            Token::FieldAssign => Err(self.error(format!(
                "Unexpected '{}' - found equals sign where a value was expected",
                Token::FieldAssign
            ))),
            Token::KeyAssign => Err(self.error(format!(
                "Unexpected '{}' - found colon where a value was expected",
                Token::KeyAssign
            ))),
            Token::Separator => Err(self.error(format!(
                "Unexpected '{}' - found comma where a value was expected",
                Token::Separator
            ))),
            Token::CommentMarker | Token::CommentMultiline => {
                unreachable!("This SHOULD never happen because comments are already skipped!")
            }
            Token::Literal(l) => Ok(Value::Text(l)),
            Token::Numeric(n) => Ok(Value::Number(n)),
            Token::Boolean(b) => Ok(Value::Boolean(b)),
            Token::Identifier(i) => Err(self.error(format!(
                "Unexpected identifier '{}' - identifiers are only valid as struct field names",
                i
            ))),
            Token::Null => Ok(Value::Null),
            Token::Eof => Err(self.error("Unexpected end of input while expecting a value")),
        }
    }
}

pub fn from_str<T: celkit_core::Deserialize>(text: &str) -> celkit_core::internal::Result<T> {
    let deserialized = Decoder::new(text).decode()?;

    T::deserialize(deserialized)
}
