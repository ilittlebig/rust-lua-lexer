use std::time::{Instant};
use std::fs;
use std::str;

#[derive(Debug)]
pub enum TokenType {
    And,
    Goto,
    Function,
    End,
    False,
    For,
    Else,
    ElseIf,
    Do,
    Break,

    Local,
    If,
    In,
    Nil,
    Not,
    Repeat,
    Or,
    Then,
    True,
    While,
    Until,
    Return,

    GreaterOrEqual,
    Concat, // ..
    Dots, // ...
    Equal,
    LessOrEqual,
    NotEqual,
    ShiftLeft, // <<
    ShiftRight, // >>
    DoubleColon,

    Float,
    Int,
    Identifier,

    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,

    Add,
    Minus,
    Mul,
    Div,
    Mod,
    Pow,
    Len,

    Assign,
    Less,
    Greater,

    BAnd, // &
    BOr, // |
    BXor, // ~

    Colon, // :
    Comma, // ,
    Semicolon, // ;
    Attr, // .

    SingleComment,
    MultiLineComment,
    StringLiteral,
    UnclosedStringLiteral,

    Eof,
    Whitespace,
    Unidentified
}

impl TokenType {
    pub fn convert_to_token_type(word: &str) -> Option<TokenType> {
        match word {
            "and" => Some(TokenType::And),
            "goto" => Some(TokenType::Goto),
            "function" => Some(TokenType::Function),
            "end" => Some(TokenType::End),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "else" => Some(TokenType::Else),
            "elseif" => Some(TokenType::ElseIf),
            "do" => Some(TokenType::Do),
            "break" => Some(TokenType::Break),
            "local" => Some(TokenType::Local),
            "if" => Some(TokenType::If),
            "in" => Some(TokenType::In),
            "nil" => Some(TokenType::Nil),
            "not" => Some(TokenType::Not),
            "repeat" => Some(TokenType::Repeat),
            "or" => Some(TokenType::Or),
            "then" => Some(TokenType::Then),
            "true" => Some(TokenType::True),
            "while" => Some(TokenType::While),
            "until" => Some(TokenType::Until),
            "return" => Some(TokenType::Return),
            _ => None
        }
    }
}

#[derive(Debug)]
enum TokenValue {
    None,
    Int(i64),
    Float(f64),
    Str(String)
}

type LexResult = Result<(TokenType, TokenValue), ()>;

struct Tokenizer<'a> {
    input: &'a String,
    pos: usize,
    tokens: Vec<(TokenType, TokenValue)>
}

impl<'a> Tokenizer<'a> {
    fn byte_at(&self, offset: usize) -> u8 {
        if let Some(_t) = self.input.as_bytes().get(self.pos + offset) {
            self.input.as_bytes()[self.pos + offset]
        } else {
            return 003 // EOF
        }
    }

    fn has_at_least(&self, n: usize) -> bool {
        self.pos + n < self.input.len()
    }

    fn starts_with(&self, needle: &[u8]) -> bool {
        self.input.as_bytes()[self.pos..].starts_with(needle)
    }

    fn is_eof(&self) -> bool {
        !self.has_at_least(0)
    }

    fn is_whitespace(&self) -> bool {
        match self.byte_at(0) {
            b' ' | b'\t' | b'\x0B' | b'\x0C' => true,
            _ => false
        }
    }

    fn is_escape_char(&self) -> bool {
        match self.byte_at(0) {
            b'\x41' | b'\n' | b'\r' | b'\t' | b'\\' | b'\0' | b'\x7F' => true,
            _ => false
        }
    }

    fn is_digit(c: u8) -> bool {
        match c {
            b'0' ..=b'9' => true,
            _ => false
        }
    }

    fn is_hex_digit(c: u8) -> bool {
        match c {
            b'a' | b'b' | b'c' | b'd' | b'e' | b'f' | b'A' | b'B' | b'C' | b'D' | b'E' | b'F' => {
                true
            },
            _ if Tokenizer::is_digit(c) => true,
            _ => false
        }
    }

    fn is_alpha(&self) -> bool {
        match self.byte_at(0) {
            b'a' ..=b'z' | b'A' ..=b'Z' => {
                true
            },
            _ => false
        }
    }

    fn is_valid_ident_start(&self) -> bool {
        self.is_alpha() || self.byte_at(0) == b'_'
    }

    fn is_valid_ident(&self) -> bool {
        self.is_alpha() || Tokenizer::is_digit(self.byte_at(0)) || self.is_valid_ident_start()
    }

    fn next(&mut self) -> LexResult {
        next_token(self)
    }

    #[allow(irrefutable_let_patterns)]
    fn read_single_line_comment(&mut self) -> LexResult {
        let mut comment: Vec<u8> = Vec::new();

        loop {
            if self.byte_at(0) != b'\n' {
                comment.push(self.byte_at(0));
                self.pos += 1;
            } else if self.byte_at(0) == b'\n' {
                self.pos += 1;
                break;
            } else {
                break;
            }
        }

        if let comment = str::from_utf8(&comment) {
            Ok((TokenType::SingleComment, TokenValue::Str(comment.unwrap().to_string()))) // TODO: add real token value
        } else {
            Ok((TokenType::Unidentified, TokenValue::None))
        }
    }

    #[allow(irrefutable_let_patterns)]
    fn read_multi_line_comment(&mut self) -> LexResult {
        let mut comment: Vec<u8> = Vec::new();

        loop {
            if self.is_escape_char() {
                self.pos += 1;
            } else if self.starts_with(b"]]") {
                self.pos += 2;
                break;
            } else {
                comment.push(self.byte_at(0));
                self.pos += 1;
            }
        }

        if let comment = str::from_utf8(&comment) {
            Ok((TokenType::MultiLineComment, TokenValue::Str(comment.unwrap().to_string())))
        } else {
            Ok((TokenType::Unidentified, TokenValue::None))
        }
    }

    #[allow(irrefutable_let_patterns)]
    fn read_string(&mut self) -> LexResult {
        let mut string: Vec<u8> = Vec::new();
        let mut is_closed = false;

        let start_char = self.byte_at(0);
        self.pos += 1;

        loop {
            if self.byte_at(0) == b'\\' {
                string.push(self.byte_at(0));
                string.push(self.byte_at(1));
                self.pos += 2;
            }
            if self.byte_at(0) != start_char && self.byte_at(0) != 3 {
                string.push(self.byte_at(0));
                self.pos += 1;
            } else if self.byte_at(0) == start_char {
                is_closed = true;
                self.pos += 1;
                break;
            } else {
                break;
            }
        }

        if let string = str::from_utf8(&string) {
            if is_closed {
                Ok((TokenType::StringLiteral, TokenValue::Str(string.unwrap().to_string())))
            } else {
                Ok((TokenType::UnclosedStringLiteral, TokenValue::None))
            }
        } else {
            Ok((TokenType::Unidentified, TokenValue::None))
        }
    }

    #[allow(irrefutable_let_patterns)]
    fn read_digit(&mut self) -> LexResult {
        let mut num_str: Vec<u8> = Vec::new();
        let mut hex = false;

        if self.byte_at(0) == b'0' && (self.byte_at(1) == b'x' || self.byte_at(1) == b'X') {
            num_str.push(self.byte_at(0));
            num_str.push(self.byte_at(1));

            self.pos += 2;
            hex = true;
        }

        loop {
            if !hex && (Tokenizer::is_digit(self.byte_at(0)) || self.byte_at(0) == b'.') {
                num_str.push(self.byte_at(0));
                self.pos += 1;
            } else if hex && Tokenizer::is_hex_digit(self.byte_at(0)) {
                num_str.push(self.byte_at(0));
                self.pos += 1;
            } else if self.byte_at(0) == b'e' || self.byte_at(0) == b'E' {
                num_str.push(self.byte_at(0));
                self.pos += 1;

                if self.byte_at(0) == b'-' || self.byte_at(0) == b'+' {
                    num_str.push(self.byte_at(0));
                    self.pos += 1;
                }
            } else {
                break;
            }
        }

        if let string = str::from_utf8(&num_str) {
            let num = self.string_to_number(string.unwrap()).unwrap();
            match num.0 {
                TokenType::Int => Ok((TokenType::Int, num.1)),
                TokenType::Float => Ok((TokenType::Float, num.1)),
                _ => Ok((TokenType::Unidentified, TokenValue::None))
            }
        } else {
            Ok((TokenType::Unidentified, TokenValue::None))
        }
    }

    fn string_to_number(&mut self, string: &str) -> Result<(TokenType, TokenValue), ()> {
        if let Some(n) = self.string_to_int(string) {
            Ok((TokenType::Int, n))
        } else if let Some(t) = self.string_to_float(string) {
            Ok((TokenType::Float, t))
        } else {
            Ok((TokenType::Unidentified, TokenValue::None))
        }
    }

    fn string_to_int(&mut self, string: &str) -> Option<TokenValue> {
        let bytes = string.as_bytes();
        let len = bytes.len();

        let mut decimal = 0;
        let mut base: i64 = 1;

        if bytes.starts_with(b"0x") {
            for i in (0..len).rev() {
                if bytes[i] >= b'0' && bytes[i] <= b'9' {
                    decimal += (bytes[i] as i64 - 48 as i64) * base;
                } else if bytes[i] >= b'A' && bytes[i] <= b'F' {
                    decimal += (bytes[i] as i64 - 55 as i64) * base;
                } else if bytes[i] >= b'a' && bytes[i] <= b'f' {
                    decimal += (bytes[i] as i64 - 87 as i64) * base;
                }
                base *= 16;
            }

            Some(TokenValue::Int(decimal.into()))
        } else {
            if let Ok(i) = string.parse::<i64>() {
                Some(TokenValue::Int(i))
            } else {
                None
            }
        }
    }

    fn string_to_float(&self, string: &str) -> Option<TokenValue> {
        if let Ok(f) = string.parse::<f64>() {
            Some(TokenValue::Float(f))
        } else {
            None
        }
    }

    fn read_other_tokens(&mut self) -> LexResult {
        let token_type = match self.byte_at(0) {
            b';' => {
                self.pos += 1;
                Some(TokenType::Semicolon)
            },
            b',' => {
                self.pos += 1;
                Some(TokenType::Comma)
            },
            b'&' => {
                self.pos += 1;
                Some(TokenType::BAnd)
            },
            b'|' => {
                self.pos += 1;
                Some(TokenType::BOr)
            },
            b'(' => {
                self.pos += 1;
                Some(TokenType::LeftParenthesis)
            },
            b')' => {
                self.pos += 1;
                Some(TokenType::RightParenthesis)
            },
            b']' => {
                self.pos += 1;
                Some(TokenType::RightSquareBracket)
            },
            b'{' => {
                self.pos += 1;
                Some(TokenType::LeftCurlyBracket)
            },
            b'}' => {
                self.pos += 1;
                Some(TokenType::RightCurlyBracket)
            },
            b'+' => {
                self.pos += 1;
                Some(TokenType::Add)
            },
            b'*' => {
                self.pos += 1;
                Some(TokenType::Mul)
            },
            b'/' => {
                self.pos += 1;
                Some(TokenType::Div)
            },
            b'%' => {
                self.pos += 1;
                Some(TokenType::Mod)
            },
            b'^' => {
                self.pos += 1;
                Some(TokenType::Pow)
            },
            b'#' => {
                self.pos += 1;
                Some(TokenType::Len)
            },
            _ => None
        };

        if let Some(t) = token_type {
            Ok((t, TokenValue::None))
        } else if self.is_valid_ident_start() {
            let mut word: Vec<u8> = Vec::new();

            word.push(self.byte_at(0));
            self.pos += 1;

            while self.is_valid_ident() {
                word.push(self.byte_at(0));
                self.pos += 1;
            }

            if let Ok(s) = str::from_utf8(&word) {
                if let Some(t) = TokenType::convert_to_token_type(s) {
                    Ok((t, TokenValue::None))
                } else {
                    Ok((TokenType::Identifier, TokenValue::Str(s.to_string())))
                }
            } else {
                Ok((TokenType::Unidentified, TokenValue::None))
            }
        } else {
            Ok((TokenType::Unidentified, TokenValue::None))
        }
    }
}

fn next_token<'a>(tokenizer: &mut Tokenizer<'a>) -> LexResult {
    match tokenizer.byte_at(0) {
        _ if tokenizer.is_whitespace() => {
            tokenizer.pos += 1;
            Ok((TokenType::Whitespace, TokenValue::None))
        }
        _ if Tokenizer::is_digit(tokenizer.byte_at(0)) => Ok(tokenizer.read_digit()?),
        _ if tokenizer.is_escape_char() => {tokenizer.pos += 1; Ok((TokenType::Whitespace, TokenValue::None))}

        b'\"' | b'\'' => Ok(tokenizer.read_string()?),
        b'>' => {
            if tokenizer.starts_with(b">=") {tokenizer.pos += 2; Ok((TokenType::GreaterOrEqual, TokenValue::None))}
            else if tokenizer.starts_with(b">>") {tokenizer.pos += 2; Ok((TokenType::ShiftRight, TokenValue::None))}
            else {tokenizer.pos += 1; Ok((TokenType::Greater, TokenValue::None))}
        },
        b'.' => {
            if tokenizer.starts_with(b"..") {tokenizer.pos += 2; Ok((TokenType::Concat, TokenValue::None))}
            else if tokenizer.starts_with(b"...") {tokenizer.pos += 3; Ok((TokenType::Dots, TokenValue::None))}
            else {
                match tokenizer.byte_at(1) {
                    b'0'..=b'9' => {(); Ok(tokenizer.read_digit()?)},
                    _ => {tokenizer.pos += 1; Ok((TokenType::Attr, TokenValue::None))}
                }
            }
        },
        b'=' => {
            if tokenizer.starts_with(b"==") {tokenizer.pos += 2; Ok((TokenType::Concat, TokenValue::None))}
            else {tokenizer.pos += 1; Ok((TokenType::Assign, TokenValue::None))}
        },
        b'<' => {
            if tokenizer.starts_with(b"<=") {tokenizer.pos += 2; Ok((TokenType::LessOrEqual, TokenValue::None))}
            else if tokenizer.starts_with(b"<<") {tokenizer.pos += 2; Ok((TokenType::ShiftLeft, TokenValue::None))}
            else {tokenizer.pos += 1; Ok((TokenType::Less, TokenValue::None))}
        },
        b'~' => {
            if tokenizer.starts_with(b"~=") {tokenizer.pos += 2; Ok((TokenType::NotEqual, TokenValue::None))}
            else {tokenizer.pos += 1; Ok((TokenType::BXor, TokenValue::None))}
        },
        b':' => {
            if tokenizer.starts_with(b"::") {tokenizer.pos += 2; Ok((TokenType::DoubleColon, TokenValue::None))}
            else {tokenizer.pos += 1; Ok((TokenType::Colon, TokenValue::None))}
        },
        b'[' => {
            if tokenizer.starts_with(b"[[") {tokenizer.pos += 2; Ok(tokenizer.read_multi_line_comment()?)}
            else {tokenizer.pos += 1; Ok((TokenType::LeftSquareBracket, TokenValue::None))}
        },
        b'-' => {
            if tokenizer.starts_with(b"--[[") {tokenizer.pos += 4; Ok(tokenizer.read_multi_line_comment()?)}
            else if tokenizer.starts_with(b"--") {tokenizer.pos += 2; Ok(tokenizer.read_single_line_comment()?)}
            else {tokenizer.pos += 1; Ok((TokenType::Minus, TokenValue::None))}
        },
        _ => Ok(tokenizer.read_other_tokens()?)
    }
}


fn main() {
    let input = fs::read_to_string("src/file_test.txt").unwrap();

    let mut tokenizer = Tokenizer {
        input: &input,
        pos: 0,
        tokens: Vec::new()
    };

    let start = Instant::now();
    while !tokenizer.is_eof() {
        let token_type = tokenizer.next().unwrap();
        tokenizer.tokens.push(token_type);
    }
    tokenizer.tokens.push((TokenType::Eof, TokenValue::None));

    #[cfg(debug_assertions)]
    for token in &tokenizer.tokens {
        println!("type: {:#?}, value: {:#?}", token.0, token.1);
    }

    let duration = start.elapsed();
    println!("tokens: {:#?}", tokenizer.tokens.len());
    println!("duration: {:#?}", duration);
}
