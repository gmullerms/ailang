/// Lexer for AILang
/// Converts source text into a flat stream of tokens.
/// Designed for the sigil-delimited, prefix-notation structure of AILang.

use crate::token::{Token, TokenKind};

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace_no_newline();

            if self.is_at_end() {
                break;
            }

            let ch = self.peek();

            match ch {
                '\n' | '\r' => {
                    self.consume_newline();
                    tokens.push(Token::new(TokenKind::Newline, self.line, 1));
                    // Check for indentation on the new line
                    if self.peek() == ' ' && self.peek_at(1) == ' ' && self.col == 1 {
                        self.advance(); // consume first space
                        self.advance(); // consume second space
                        tokens.push(Token::new(TokenKind::Indent, self.line, 1));
                    }
                }
                '#' => {
                    tokens.push(self.lex_sigil()?);
                }
                '"' => {
                    tokens.push(self.lex_string()?);
                }
                '-' => {
                    if self.peek_at(1) == '-' {
                        self.skip_line_comment();
                    } else if self.peek_at(1) == '>' {
                        let tok = Token::new(TokenKind::Arrow, self.line, self.col);
                        self.advance();
                        self.advance();
                        tokens.push(tok);
                    } else if self.peek_at(1).is_ascii_digit() {
                        tokens.push(self.lex_number()?);
                    } else {
                        tokens.push(Token::new(TokenKind::Minus, self.line, self.col));
                        self.advance();
                    }
                }
                '|' => {
                    if self.peek_at(1) == '>' {
                        let tok = Token::new(TokenKind::Pipe, self.line, self.col);
                        self.advance();
                        self.advance();
                        tokens.push(tok);
                    } else {
                        return Err(self.error("unexpected character '|'"));
                    }
                }
                '=' => {
                    if self.peek_at(1) == '=' {
                        let tok = Token::new(TokenKind::Eq, self.line, self.col);
                        self.advance();
                        self.advance();
                        tokens.push(tok);
                    } else if self.peek_at(1) == '>' {
                        let tok = Token::new(TokenKind::FatArrow, self.line, self.col);
                        self.advance();
                        self.advance();
                        tokens.push(tok);
                    } else {
                        tokens.push(Token::new(TokenKind::Assign, self.line, self.col));
                        self.advance();
                    }
                }
                '!' => {
                    if self.peek_at(1) == '=' {
                        let tok = Token::new(TokenKind::Neq, self.line, self.col);
                        self.advance();
                        self.advance();
                        tokens.push(tok);
                    } else {
                        // ! as part of result type — handled as part of type parsing
                        return Err(self.error("unexpected character '!'"));
                    }
                }
                '<' => {
                    if self.peek_at(1) == '=' {
                        let tok = Token::new(TokenKind::Lte, self.line, self.col);
                        self.advance();
                        self.advance();
                        tokens.push(tok);
                    } else {
                        tokens.push(Token::new(TokenKind::Lt, self.line, self.col));
                        self.advance();
                    }
                }
                '>' => {
                    if self.peek_at(1) == '=' {
                        let tok = Token::new(TokenKind::Gte, self.line, self.col);
                        self.advance();
                        self.advance();
                        tokens.push(tok);
                    } else {
                        tokens.push(Token::new(TokenKind::Gt, self.line, self.col));
                        self.advance();
                    }
                }
                '?' => {
                    tokens.push(Token::new(TokenKind::Question, self.line, self.col));
                    self.advance();
                }
                '+' => {
                    tokens.push(Token::new(TokenKind::Plus, self.line, self.col));
                    self.advance();
                }
                '*' => {
                    tokens.push(Token::new(TokenKind::Star, self.line, self.col));
                    self.advance();
                }
                '/' => {
                    tokens.push(Token::new(TokenKind::Slash, self.line, self.col));
                    self.advance();
                }
                '%' => {
                    tokens.push(Token::new(TokenKind::Percent, self.line, self.col));
                    self.advance();
                }
                '(' => {
                    tokens.push(Token::new(TokenKind::LParen, self.line, self.col));
                    self.advance();
                }
                ')' => {
                    tokens.push(Token::new(TokenKind::RParen, self.line, self.col));
                    self.advance();
                }
                '[' => {
                    tokens.push(Token::new(TokenKind::LBrack, self.line, self.col));
                    self.advance();
                }
                ']' => {
                    tokens.push(Token::new(TokenKind::RBrack, self.line, self.col));
                    self.advance();
                }
                '{' => {
                    tokens.push(Token::new(TokenKind::LBrace, self.line, self.col));
                    self.advance();
                }
                '}' => {
                    tokens.push(Token::new(TokenKind::RBrace, self.line, self.col));
                    self.advance();
                }
                ':' => {
                    tokens.push(Token::new(TokenKind::Colon, self.line, self.col));
                    self.advance();
                }
                '.' => {
                    tokens.push(Token::new(TokenKind::Dot, self.line, self.col));
                    self.advance();
                }
                '_' => {
                    if self.peek_at(1).is_alphanumeric() {
                        tokens.push(self.lex_ident());
                    } else {
                        tokens.push(Token::new(TokenKind::Underscore, self.line, self.col));
                        self.advance();
                    }
                }
                c if c.is_ascii_digit() => {
                    tokens.push(self.lex_number()?);
                }
                c if c.is_alphabetic() || c == '_' => {
                    tokens.push(self.lex_ident());
                }
                _ => {
                    return Err(self.error(&format!("unexpected character '{}'", ch)));
                }
            }
        }

        tokens.push(Token::new(TokenKind::Eof, self.line, self.col));
        Ok(tokens)
    }

    fn lex_sigil(&mut self) -> Result<Token, LexError> {
        let line = self.line;
        let col = self.col;
        self.advance(); // consume '#'

        let word = self.consume_while(|c| c.is_alphabetic());

        let kind = match word.as_str() {
            "fn" => TokenKind::Fn,
            "type" => TokenKind::Type,
            "enum" => TokenKind::Enum,
            "const" => TokenKind::Const,
            "use" => TokenKind::Use,
            "entry" => TokenKind::Entry,
            "test" => TokenKind::Test,
            "err" => TokenKind::Err,
            _ => return Err(LexError {
                message: format!("unknown sigil '#{}'", word),
                line,
                col,
            }),
        };

        Ok(Token::new(kind, line, col))
    }

    fn lex_string(&mut self) -> Result<Token, LexError> {
        let line = self.line;
        let col = self.col;
        self.advance(); // consume opening '"'

        let mut s = String::new();
        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\\' {
                self.advance();
                if self.is_at_end() {
                    return Err(self.error("unterminated string escape"));
                }
                match self.peek() {
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    'r' => s.push('\r'),
                    '\\' => s.push('\\'),
                    '"' => s.push('"'),
                    '{' => s.push('{'),
                    '}' => s.push('}'),
                    c => {
                        return Err(self.error(&format!("unknown escape '\\{}'", c)));
                    }
                }
                self.advance();
            } else if self.peek() == '\n' {
                return Err(self.error("unterminated string (newline in string)"));
            } else {
                s.push(self.peek());
                self.advance();
            }
        }

        if self.is_at_end() {
            return Err(LexError {
                message: "unterminated string".to_string(),
                line,
                col,
            });
        }

        self.advance(); // consume closing '"'
        Ok(Token::new(TokenKind::TextLit(s), line, col))
    }

    fn lex_number(&mut self) -> Result<Token, LexError> {
        let line = self.line;
        let col = self.col;
        let mut s = String::new();

        if self.peek() == '-' {
            s.push('-');
            self.advance();
        }

        // Hex literal
        if self.peek() == '0' && self.peek_at(1) == 'x' {
            s.push('0');
            self.advance();
            s.push('x');
            self.advance();
            let hex = self.consume_while(|c| c.is_ascii_hexdigit());
            if hex.is_empty() {
                return Err(self.error("expected hex digits after 0x"));
            }
            s.push_str(&hex);
            let val = i64::from_str_radix(&s[if s.starts_with('-') { 3 } else { 2 }..], 16)
                .map_err(|_| self.error("invalid hex literal"))?;
            let val = if s.starts_with('-') { -val } else { val };
            return Ok(Token::new(TokenKind::IntLit(val), line, col));
        }

        let digits = self.consume_while(|c| c.is_ascii_digit());
        s.push_str(&digits);

        if self.peek() == '.' && self.peek_at(1).is_ascii_digit() {
            s.push('.');
            self.advance();
            let frac = self.consume_while(|c| c.is_ascii_digit());
            s.push_str(&frac);
            let val: f64 = s.parse().map_err(|_| self.error("invalid float literal"))?;
            Ok(Token::new(TokenKind::FloatLit(val), line, col))
        } else {
            let val: i64 = s.parse().map_err(|_| self.error("invalid integer literal"))?;
            Ok(Token::new(TokenKind::IntLit(val), line, col))
        }
    }

    fn lex_ident(&mut self) -> Token {
        let line = self.line;
        let col = self.col;
        let word = self.consume_while(|c| c.is_alphanumeric() || c == '_');

        let kind = match word.as_str() {
            "call" => TokenKind::Call,
            "select" => TokenKind::Select,
            "cond" => TokenKind::Cond,
            "match" => TokenKind::Match,
            "map" => TokenKind::Map,
            "filter" => TokenKind::Filter,
            "fold" => TokenKind::Fold,
            "each" => TokenKind::Each,
            "flatmap" => TokenKind::FlatMap,
            "zip" => TokenKind::Zip,
            "async" => TokenKind::Async,
            "await" => TokenKind::Await,
            "par" => TokenKind::Par,
            "try" => TokenKind::Try,
            "unwrap" => TokenKind::Unwrap,
            "ok" => TokenKind::Ok,
            "assert" => TokenKind::Assert,
            "log" => TokenKind::Log,
            "tool" => TokenKind::Tool,
            "prompt" => TokenKind::Prompt,
            "cast" => TokenKind::Cast,
            "typeof" => TokenKind::Typeof,
            "is" => TokenKind::Is,
            "true" => TokenKind::BoolLit(true),
            "false" => TokenKind::BoolLit(false),
            "null" => TokenKind::NullLit,
            "panic" => TokenKind::Panic,
            "retry" => TokenKind::Retry,
            "fallback" => TokenKind::Fallback,
            "send" => TokenKind::Send,
            "recv" => TokenKind::Recv,
            "chan" => TokenKind::Chan,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "band" => TokenKind::Band,
            "bor" => TokenKind::Bor,
            "bxor" => TokenKind::Bxor,
            "bnot" => TokenKind::Bnot,
            "shl" => TokenKind::Shl,
            "shr" => TokenKind::Shr,
            "neg" => TokenKind::Neg,
            _ => TokenKind::Ident(word),
        };

        Token::new(kind, line, col)
    }

    fn skip_whitespace_no_newline(&mut self) {
        while !self.is_at_end() && self.peek() == ' ' && !(self.col == 1) {
            self.advance();
        }
        // Also skip spaces that aren't at column 1 (non-indent spaces)
        while !self.is_at_end() && self.peek() == ' ' && self.col > 2 {
            self.advance();
        }
    }

    fn skip_line_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn consume_newline(&mut self) {
        if self.peek() == '\r' {
            self.advance();
        }
        if !self.is_at_end() && self.peek() == '\n' {
            self.pos += 1;
            self.line += 1;
            self.col = 1;
        }
    }

    fn consume_while<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
        let mut s = String::new();
        while !self.is_at_end() && pred(self.peek()) {
            s.push(self.peek());
            self.advance();
        }
        s
    }

    fn peek(&self) -> char {
        if self.pos < self.source.len() {
            self.source[self.pos]
        } else {
            '\0'
        }
    }

    fn peek_at(&self, offset: usize) -> char {
        let idx = self.pos + offset;
        if idx < self.source.len() {
            self.source[idx]
        } else {
            '\0'
        }
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            if self.source[self.pos] == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn error(&self, message: &str) -> LexError {
        LexError {
            message: message.to_string(),
            line: self.line,
            col: self.col,
        }
    }
}

#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lex error at {}:{}: {}", self.line, self.col, self.message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_fn_block() {
        let source = "#fn add :i32 a:i32 b:i32\n  = + a b";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::Fn));
        assert!(matches!(tokens[1].kind, TokenKind::Ident(ref s) if s == "add"));
        assert!(matches!(tokens[2].kind, TokenKind::Colon));
    }

    #[test]
    fn test_lex_numbers() {
        let mut lexer = Lexer::new("42 3.14 -7 0xFF");
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::IntLit(42)));
        assert!(matches!(tokens[1].kind, TokenKind::FloatLit(f) if (f - 3.14).abs() < f64::EPSILON));
        assert!(matches!(tokens[2].kind, TokenKind::IntLit(-7)));
        assert!(matches!(tokens[3].kind, TokenKind::IntLit(255)));
    }

    #[test]
    fn test_lex_string() {
        let mut lexer = Lexer::new("\"hello world\"");
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::TextLit(ref s) if s == "hello world"));
    }

    #[test]
    fn test_lex_operators() {
        let mut lexer = Lexer::new("+ - * / == != <= >=");
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::Plus));
        assert!(matches!(tokens[1].kind, TokenKind::Minus));
        assert!(matches!(tokens[2].kind, TokenKind::Star));
        assert!(matches!(tokens[3].kind, TokenKind::Slash));
        assert!(matches!(tokens[4].kind, TokenKind::Eq));
        assert!(matches!(tokens[5].kind, TokenKind::Neq));
        assert!(matches!(tokens[6].kind, TokenKind::Lte));
        assert!(matches!(tokens[7].kind, TokenKind::Gte));
    }
}
