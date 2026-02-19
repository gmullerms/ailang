/// JSON parsing and serialization for AILang
/// A simple recursive descent parser that converts between JSON text and AILang Values.
/// No external dependencies — hand-written parser.

use crate::interpreter::{RuntimeError, Value};

// ---------------------------------------------------------------------------
// JSON Parser — recursive descent
// ---------------------------------------------------------------------------

struct JsonParser {
    chars: Vec<char>,
    pos: usize,
}

impl JsonParser {
    fn new(input: &str) -> Self {
        JsonParser {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.get(self.pos).copied();
        if ch.is_some() {
            self.pos += 1;
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn expect(&mut self, expected: char) -> Result<(), RuntimeError> {
        self.skip_whitespace();
        match self.advance() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(RuntimeError {
                message: format!("JSON parse error: expected '{}', got '{}'", expected, ch),
            }),
            None => Err(RuntimeError {
                message: format!("JSON parse error: expected '{}', got end of input", expected),
            }),
        }
    }

    fn parse_value(&mut self) -> Result<Value, RuntimeError> {
        self.skip_whitespace();
        match self.peek() {
            Some('"') => self.parse_string(),
            Some('{') => self.parse_object(),
            Some('[') => self.parse_array(),
            Some('t') => self.parse_true(),
            Some('f') => self.parse_false(),
            Some('n') => self.parse_null(),
            Some(ch) if ch == '-' || ch.is_ascii_digit() => self.parse_number(),
            Some(ch) => Err(RuntimeError {
                message: format!("JSON parse error: unexpected character '{}'", ch),
            }),
            None => Err(RuntimeError {
                message: "JSON parse error: unexpected end of input".to_string(),
            }),
        }
    }

    fn parse_string(&mut self) -> Result<Value, RuntimeError> {
        Ok(Value::Text(self.parse_string_raw()?))
    }

    fn parse_string_raw(&mut self) -> Result<String, RuntimeError> {
        self.expect('"')?;
        let mut s = String::new();
        loop {
            match self.advance() {
                Some('"') => return Ok(s),
                Some('\\') => {
                    match self.advance() {
                        Some('"') => s.push('"'),
                        Some('\\') => s.push('\\'),
                        Some('/') => s.push('/'),
                        Some('b') => s.push('\u{0008}'),
                        Some('f') => s.push('\u{000C}'),
                        Some('n') => s.push('\n'),
                        Some('r') => s.push('\r'),
                        Some('t') => s.push('\t'),
                        Some('u') => {
                            let mut hex = String::new();
                            for _ in 0..4 {
                                match self.advance() {
                                    Some(ch) if ch.is_ascii_hexdigit() => hex.push(ch),
                                    _ => {
                                        return Err(RuntimeError {
                                            message: "JSON parse error: invalid unicode escape"
                                                .to_string(),
                                        })
                                    }
                                }
                            }
                            let code = u32::from_str_radix(&hex, 16).map_err(|_| RuntimeError {
                                message: "JSON parse error: invalid unicode escape".to_string(),
                            })?;
                            match char::from_u32(code) {
                                Some(ch) => s.push(ch),
                                None => {
                                    return Err(RuntimeError {
                                        message: "JSON parse error: invalid unicode codepoint"
                                            .to_string(),
                                    })
                                }
                            }
                        }
                        _ => {
                            return Err(RuntimeError {
                                message: "JSON parse error: invalid escape sequence".to_string(),
                            })
                        }
                    }
                }
                Some(ch) => s.push(ch),
                None => {
                    return Err(RuntimeError {
                        message: "JSON parse error: unterminated string".to_string(),
                    })
                }
            }
        }
    }

    fn parse_number(&mut self) -> Result<Value, RuntimeError> {
        let mut num_str = String::new();
        let mut is_float = false;

        // Optional minus
        if self.peek() == Some('-') {
            num_str.push('-');
            self.advance();
        }

        // Integer part
        match self.peek() {
            Some('0') => {
                num_str.push('0');
                self.advance();
            }
            Some(ch) if ch.is_ascii_digit() => {
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_digit() {
                        num_str.push(ch);
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            _ => {
                return Err(RuntimeError {
                    message: "JSON parse error: invalid number".to_string(),
                })
            }
        }

        // Fractional part
        if self.peek() == Some('.') {
            is_float = true;
            num_str.push('.');
            self.advance();
            let mut has_digit = false;
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    num_str.push(ch);
                    self.advance();
                    has_digit = true;
                } else {
                    break;
                }
            }
            if !has_digit {
                return Err(RuntimeError {
                    message: "JSON parse error: expected digit after decimal point".to_string(),
                });
            }
        }

        // Exponent
        if let Some(ch) = self.peek() {
            if ch == 'e' || ch == 'E' {
                is_float = true;
                num_str.push('e');
                self.advance();
                if let Some(sign) = self.peek() {
                    if sign == '+' || sign == '-' {
                        num_str.push(sign);
                        self.advance();
                    }
                }
                let mut has_digit = false;
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_digit() {
                        num_str.push(ch);
                        self.advance();
                        has_digit = true;
                    } else {
                        break;
                    }
                }
                if !has_digit {
                    return Err(RuntimeError {
                        message: "JSON parse error: expected digit in exponent".to_string(),
                    });
                }
            }
        }

        if is_float {
            let f: f64 = num_str.parse().map_err(|_| RuntimeError {
                message: format!("JSON parse error: invalid number '{}'", num_str),
            })?;
            Ok(Value::Float(f))
        } else {
            let n: i64 = num_str.parse().map_err(|_| RuntimeError {
                message: format!("JSON parse error: invalid number '{}'", num_str),
            })?;
            Ok(Value::Int(n))
        }
    }

    fn parse_object(&mut self) -> Result<Value, RuntimeError> {
        self.expect('{')?;
        self.skip_whitespace();

        let mut pairs = Vec::new();

        if self.peek() == Some('}') {
            self.advance();
            return Ok(Value::Map(pairs));
        }

        loop {
            self.skip_whitespace();
            let key = self.parse_string_raw()?;
            self.skip_whitespace();
            self.expect(':')?;
            let value = self.parse_value()?;
            pairs.push((Value::Text(key), value));

            self.skip_whitespace();
            match self.peek() {
                Some(',') => {
                    self.advance();
                }
                Some('}') => {
                    self.advance();
                    return Ok(Value::Map(pairs));
                }
                _ => {
                    return Err(RuntimeError {
                        message: "JSON parse error: expected ',' or '}' in object".to_string(),
                    })
                }
            }
        }
    }

    fn parse_array(&mut self) -> Result<Value, RuntimeError> {
        self.expect('[')?;
        self.skip_whitespace();

        let mut items = Vec::new();

        if self.peek() == Some(']') {
            self.advance();
            return Ok(Value::List(items));
        }

        loop {
            let value = self.parse_value()?;
            items.push(value);

            self.skip_whitespace();
            match self.peek() {
                Some(',') => {
                    self.advance();
                }
                Some(']') => {
                    self.advance();
                    return Ok(Value::List(items));
                }
                _ => {
                    return Err(RuntimeError {
                        message: "JSON parse error: expected ',' or ']' in array".to_string(),
                    })
                }
            }
        }
    }

    fn parse_true(&mut self) -> Result<Value, RuntimeError> {
        self.expect_keyword("true")?;
        Ok(Value::Bool(true))
    }

    fn parse_false(&mut self) -> Result<Value, RuntimeError> {
        self.expect_keyword("false")?;
        Ok(Value::Bool(false))
    }

    fn parse_null(&mut self) -> Result<Value, RuntimeError> {
        self.expect_keyword("null")?;
        Ok(Value::Null)
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), RuntimeError> {
        for expected_ch in keyword.chars() {
            match self.advance() {
                Some(ch) if ch == expected_ch => {}
                _ => {
                    return Err(RuntimeError {
                        message: format!("JSON parse error: expected '{}'", keyword),
                    })
                }
            }
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a JSON string into an AILang Value
pub fn json_parse(input: &str) -> Result<Value, RuntimeError> {
    let mut parser = JsonParser::new(input);
    let value = parser.parse_value()?;
    parser.skip_whitespace();
    if parser.pos != parser.chars.len() {
        return Err(RuntimeError {
            message: "JSON parse error: unexpected trailing content".to_string(),
        });
    }
    Ok(value)
}

/// Serialize an AILang Value to a JSON string
pub fn json_stringify(value: &Value) -> Result<String, RuntimeError> {
    match value {
        Value::Null => Ok("null".to_string()),
        Value::Bool(b) => Ok(if *b { "true" } else { "false" }.to_string()),
        Value::Int(n) => Ok(n.to_string()),
        Value::Float(f) => {
            // Ensure floats always have decimal representation
            if f.is_infinite() || f.is_nan() {
                Ok("null".to_string())
            } else {
                let s = f.to_string();
                // If the float string doesn't contain a '.', add .0
                if s.contains('.') {
                    Ok(s)
                } else {
                    Ok(format!("{}.0", s))
                }
            }
        }
        Value::Text(s) => Ok(json_escape_string(s)),
        Value::List(items) => {
            let mut parts = Vec::new();
            for item in items {
                parts.push(json_stringify(item)?);
            }
            Ok(format!("[{}]", parts.join(",")))
        }
        Value::Map(pairs) => {
            let mut parts = Vec::new();
            for (k, v) in pairs {
                let key_str = match k {
                    Value::Text(s) => json_escape_string(s),
                    other => json_escape_string(&format!("{}", other)),
                };
                let val_str = json_stringify(v)?;
                parts.push(format!("{}:{}", key_str, val_str));
            }
            Ok(format!("{{{}}}", parts.join(",")))
        }
        Value::Tuple(items) => {
            // Serialize tuples as JSON arrays
            let mut parts = Vec::new();
            for item in items {
                parts.push(json_stringify(item)?);
            }
            Ok(format!("[{}]", parts.join(",")))
        }
        Value::Ok(inner) => json_stringify(inner),
        Value::Err(msg) => Ok(json_escape_string(msg)),
        Value::Void => Ok("null".to_string()),
        Value::FnRef(name) => Ok(json_escape_string(name)),
        Value::Lambda { .. } => Ok("null".to_string()),
    }
}

fn json_escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{0008}' => out.push_str("\\b"),
            '\u{000C}' => out.push_str("\\f"),
            c if c < '\u{0020}' => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Navigate a nested structure by path components.
/// Each path component is either a string key (for maps) or an integer index (for lists).
pub fn json_get(data: &Value, path: &[Value]) -> Value {
    let mut current = data.clone();
    for component in path {
        current = match (&current, component) {
            (Value::Map(pairs), Value::Text(key)) => {
                let found = pairs.iter().find(|(k, _)| {
                    if let Value::Text(k_str) = k {
                        k_str == key
                    } else {
                        false
                    }
                });
                match found {
                    Some((_, v)) => v.clone(),
                    None => return Value::Null,
                }
            }
            (Value::Map(pairs), Value::Int(idx)) => {
                // Allow integer key lookup as string
                let key_str = idx.to_string();
                let found = pairs.iter().find(|(k, _)| {
                    if let Value::Text(k_str) = k {
                        k_str == &key_str
                    } else {
                        false
                    }
                });
                match found {
                    Some((_, v)) => v.clone(),
                    None => return Value::Null,
                }
            }
            (Value::List(items), Value::Int(idx)) => {
                if *idx < 0 {
                    return Value::Null;
                }
                match items.get(*idx as usize) {
                    Some(v) => v.clone(),
                    None => return Value::Null,
                }
            }
            _ => return Value::Null,
        };
    }
    current
}

/// Set a value at a nested path, returning a new structure.
/// path must have at least one component. The last element of `path` is the key/index
/// to set, and `new_val` is the value to place there.
pub fn json_set(data: &Value, path: &[Value], new_val: &Value) -> Result<Value, RuntimeError> {
    if path.is_empty() {
        return Ok(new_val.clone());
    }

    let component = &path[0];

    if path.len() == 1 {
        // Base case: set value at this level
        match (data, component) {
            (Value::Map(pairs), Value::Text(key)) => {
                let mut new_pairs: Vec<(Value, Value)> = pairs
                    .iter()
                    .filter(|(k, _)| {
                        if let Value::Text(k_str) = k {
                            k_str != key
                        } else {
                            true
                        }
                    })
                    .cloned()
                    .collect();
                new_pairs.push((Value::Text(key.clone()), new_val.clone()));
                Ok(Value::Map(new_pairs))
            }
            (Value::List(items), Value::Int(idx)) => {
                let i = *idx as usize;
                if i < items.len() {
                    let mut new_items = items.clone();
                    new_items[i] = new_val.clone();
                    Ok(Value::List(new_items))
                } else {
                    // If index is out of bounds, extend with nulls
                    let mut new_items = items.clone();
                    while new_items.len() <= i {
                        new_items.push(Value::Null);
                    }
                    new_items[i] = new_val.clone();
                    Ok(Value::List(new_items))
                }
            }
            (Value::Null, Value::Text(key)) => {
                // Auto-create map
                Ok(Value::Map(vec![(
                    Value::Text(key.clone()),
                    new_val.clone(),
                )]))
            }
            (Value::Null, Value::Int(idx)) => {
                // Auto-create list
                let i = *idx as usize;
                let mut new_items = Vec::new();
                while new_items.len() <= i {
                    new_items.push(Value::Null);
                }
                new_items[i] = new_val.clone();
                Ok(Value::List(new_items))
            }
            _ => Err(RuntimeError {
                message: format!(
                    "jset: cannot set on {} with key {}",
                    data.type_name(),
                    component
                ),
            }),
        }
    } else {
        // Recursive case: descend into nested structure
        let rest = &path[1..];
        match (data, component) {
            (Value::Map(pairs), Value::Text(key)) => {
                let child = pairs
                    .iter()
                    .find(|(k, _)| {
                        if let Value::Text(k_str) = k {
                            k_str == key
                        } else {
                            false
                        }
                    })
                    .map(|(_, v)| v.clone())
                    .unwrap_or(Value::Null);
                let new_child = json_set(&child, rest, new_val)?;
                let mut new_pairs: Vec<(Value, Value)> = pairs
                    .iter()
                    .filter(|(k, _)| {
                        if let Value::Text(k_str) = k {
                            k_str != key
                        } else {
                            true
                        }
                    })
                    .cloned()
                    .collect();
                new_pairs.push((Value::Text(key.clone()), new_child));
                Ok(Value::Map(new_pairs))
            }
            (Value::List(items), Value::Int(idx)) => {
                let i = *idx as usize;
                let child = items.get(i).cloned().unwrap_or(Value::Null);
                let new_child = json_set(&child, rest, new_val)?;
                let mut new_items = items.clone();
                while new_items.len() <= i {
                    new_items.push(Value::Null);
                }
                new_items[i] = new_child;
                Ok(Value::List(new_items))
            }
            (Value::Null, Value::Text(key)) => {
                let new_child = json_set(&Value::Null, rest, new_val)?;
                Ok(Value::Map(vec![(Value::Text(key.clone()), new_child)]))
            }
            (Value::Null, Value::Int(idx)) => {
                let i = *idx as usize;
                let new_child = json_set(&Value::Null, rest, new_val)?;
                let mut new_items = Vec::new();
                while new_items.len() <= i {
                    new_items.push(Value::Null);
                }
                new_items[i] = new_child;
                Ok(Value::List(new_items))
            }
            _ => Err(RuntimeError {
                message: format!(
                    "jset: cannot navigate into {} with key {}",
                    data.type_name(),
                    component
                ),
            }),
        }
    }
}
