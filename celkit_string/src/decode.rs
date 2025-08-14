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
        let mut context = String::new();
        let radius = 10; // Radius around the position for context

        let start = self.position.saturating_sub(radius);
        let end = (self.position + radius + 1).min(self.input.len());
        let content: String = self.input[start..end].iter().collect();

        context.push_str(&content);
        context.push('\n');
        context.push_str(&" ".repeat(self.position - start));
        context.push('^');

        Error::with_context(message, context, self.line, self.column)
    }

    fn advance(&mut self) {
        self.position += 1;
        self.current_char = self.input.get(self.position).copied();

        if let Some('\n') = self.current_char {
            self.line += 1;
            self.column = 1;

            return;
        }

        self.column += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if !c.is_whitespace() {
                break;
            }

            self.advance();
        }
    }

    fn find_char(&mut self, token: Token) -> Result<Token> {
        self.advance();

        Ok(token)
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

        // Must start with digit or minus sign
        if !matches!(self.current_char, Some(c) if c.is_ascii_digit() || c == '-') {
            return Err(self.error("Expected number".to_string()));
        }

        // Handle negative sign
        if let Some('-') = self.current_char {
            numeric.push('-');

            self.advance(); // Consume character '-'

            // Must have digit after negative sign
            if !matches!(self.current_char, Some(c) if c.is_ascii_digit()) {
                return Err(self.error("Expected digit after '-' in number".to_string()));
            }
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

            break;
        }

        if has_decimal {
            return match numeric.parse::<f64>() {
                Ok(f) if f.is_finite() => Ok(Token::Numeric(Number::F64(f))),
                Ok(_) => Err(self.error(format!("Float is too large: '{}'", numeric))),
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

            return Err(self.error(format!("Integer is too large: '{}'", numeric)));
        } else {
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

            return Err(self.error(format!("Integer is too large: '{}'", numeric)));
        }
    }

    fn find_identifier_or_keyword(&mut self) -> Result<Token> {
        let start = self.position;

        while let Some(c) = self.current_char {
            if !c.is_alphanumeric() && c != '_' {
                break;
            }

            self.advance();
        }

        let identifer: String = self.input[start..self.position].iter().collect();

        match identifer.as_str() {
            "true" => Ok(Token::Boolean(true)),
            "false" => Ok(Token::Boolean(false)),
            "null" => Ok(Token::Null),
            _ => Ok(Token::Identifier(identifer)),
        }
    }

    fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        match self.current_char {
            None => Ok(Token::Eof),
            Some('@') => self.find_char(Token::StructMarker),
            Some('[') => self.find_char(Token::ArrayOpen),
            Some(']') => self.find_char(Token::ArrayClose),
            Some('(') => self.find_char(Token::OpenParenthesis),
            Some(')') => self.find_char(Token::CloseParenthesis),
            Some('{') => self.find_char(Token::OpenBrace),
            Some('}') => self.find_char(Token::CloseBrace),
            Some('=') => self.find_char(Token::Equals),
            Some(':') => self.find_char(Token::Colon),
            Some(',') => self.find_char(Token::Comma),
            Some('"') => self.find_literal(),
            Some(c) if c.is_ascii_digit() || c == '-' => self.find_numeric(),
            Some(c) if c.is_alphabetic() => self.find_identifier_or_keyword(),
            Some(c) => Err(self.error(format!("Unexpected character: '{}'", c))),
        }
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
            Token::OpenParenthesis,
            &format!(
                "struct definition: (expected '{}' after '{}')",
                Token::OpenParenthesis,
                Token::StructMarker,
            ),
        )?;

        let mut fields = BTreeMap::new();
        let mut empty = true;

        loop {
            // Check for CloseParenthesis for handling empty structs and trailing commas
            self.skip_whitespace();

            if self.current_char == Token::CloseParenthesis.get_char() {
                self.advance(); // Consume CloseParenthesis

                break;
            }

            if !empty {
                self.expect_token(Token::Comma, "struct fields")?;

                // Check again for CloseParenthesis for handling trailing comma
                self.skip_whitespace();

                if self.current_char == Token::CloseParenthesis.get_char() {
                    self.advance(); // Consume CloseParenthesis

                    break;
                }
            }

            // Decode field
            let token = self.next_token()?;

            match token {
                Token::Identifier(i /* Field name */) => {
                    let token = self.next_token()?;

                    if token != Token::Equals {
                        return Err(match token {
                            Token::Colon => self.error(format!(
                                "Found '{}' but expected '{}' after struct field name",
                                Token::Colon,
                                Token::Equals
                            )),
                            Token::Eof => self
                                .error(format!("Unexpected end of input after field name '{}'", i)),
                            _ => self.error(format!(
                                "Expected '{}' after struct field name '{}'",
                                Token::Equals,
                                i
                            )),
                        });
                    }

                    let value = self.decode_value()?;

                    fields.insert(i, value);
                }
                Token::Eof => {
                    return Err(self.error(format!(
                        "Unexpected end of input while parsing struct: Missing closing '{}'",
                        Token::CloseParenthesis
                    )))
                }
                Token::CloseParenthesis => {
                    // The end of the struct or an empty struct, already handled
                }
                _ => return Err(self.error("Expected field name in struct")),
            }

            empty = false;
        }

        Ok(Value::Struct(fields))
    }

    fn decode_array(&mut self) -> Result<Value> {
        let mut items = Vec::new();
        let mut empty = true;

        loop {
            // Check for ArrayClose for handling empty arrays and trailing commas
            self.skip_whitespace();

            if self.current_char == Token::ArrayClose.get_char() {
                self.advance(); // Consume ArrayClose

                break;
            }

            if !empty {
                self.expect_token(Token::Comma, "array items")?;

                // Check again for ArrayClose for handling trailing comma
                self.skip_whitespace();

                if self.current_char == Token::ArrayClose.get_char() {
                    self.advance(); // Consume ArrayClose

                    break;
                }
            }

            // Decode item
            let value = self.decode_value()?;

            items.push(value);

            empty = false;
        }

        Ok(Value::Array(items))
    }

    fn decode_tuple(&mut self) -> Result<Value> {
        let mut members = Vec::new();
        let mut empty = true;

        loop {
            // Check for CloseParenthesis for handling empty tuples and trailing commas
            self.skip_whitespace();

            if self.current_char == Token::CloseParenthesis.get_char() {
                self.advance(); // Consume CloseParenthesis

                break;
            }

            if !empty {
                self.expect_token(Token::Comma, "tuple members")?;

                // Check again for CloseParenthesis for handling trailing comma
                self.skip_whitespace();

                if self.current_char == Token::CloseParenthesis.get_char() {
                    self.advance(); // Consume CloseParenthesis

                    break;
                }
            }

            // Decode member
            let value = self.decode_value()?;

            members.push(value);

            empty = false;
        }

        Ok(Value::Tuple(members))
    }

    fn decode_object(&mut self) -> Result<Value> {
        let mut entries = BTreeMap::new();
        let mut empty = true;

        loop {
            // Check for CloseBrace for handling empty objects and trailing commas
            self.skip_whitespace();

            if self.current_char == Token::CloseBrace.get_char() {
                self.advance(); // Consume CloseBrace

                break;
            }

            if !empty {
                self.expect_token(Token::Comma, "object entries")?;

                // Check again for CloseBrace for handling trailing comma
                self.skip_whitespace();

                if self.current_char == Token::CloseBrace.get_char() {
                    self.advance(); // Consume CloseBrace

                    break;
                }
            }

            // Decode entry
            let token = self.next_token()?;

            match token {
                Token::Literal(l /* Entry key */) => {
                    let token = self.next_token()?;

                    if token != Token::Colon {
                        return Err(match token {
                            Token::Equals => self.error(format!(
                                "Found '{}' but expected '{}' after object key",
                                Token::Equals,
                                Token::Colon
                            )),
                            Token::Eof => self
                                .error(format!("Unexpected end of input after object key '{}'", l)),
                            _ => self.error(format!(
                                "Expected '{}' after object key '{}'",
                                Token::Colon,
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
                        Token::CloseParenthesis
                    )))
                }
                Token::CloseBrace => {} // The end of the object or an empty object
                Token::Identifier(i) => {
                    return Err(self.error(format!(
                        "Found unquoted identifier '{}' where object key was expected",
                        i
                    )))
                }
                _ => return Err(self.error("Expected string key in object")),
            }

            empty = false;
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
            Token::OpenParenthesis => self.decode_tuple(),
            Token::CloseParenthesis => Err(self.error(format!(
                "Unexpected '{}' - found closing parenthesis without matching opening parenthesis",
                Token::CloseParenthesis,
            ))),
            Token::OpenBrace => self.decode_object(),
            Token::CloseBrace => Err(self.error(format!(
                "Unexpected '{}' - found closing brace without matching opening brace",
                Token::CloseBrace
            ))),
            Token::Equals => Err(self.error(format!(
                "Unexpected '{}' - found equals sign where a value was expected",
                Token::Equals
            ))),
            Token::Colon => Err(self.error(format!(
                "Unexpected '{}' - found colon where a value was expected",
                Token::Colon
            ))),
            Token::Comma => Err(self.error(format!(
                "Unexpected '{}' - found comma where a value was expected",
                Token::Comma
            ))),
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

pub fn from_string<T: celkit_core::Deserialize>(text: &str) -> celkit_core::internal::Result<T> {
    let deserialized = Decoder::new(text).decode()?;

    T::deserialize(deserialized)
}
