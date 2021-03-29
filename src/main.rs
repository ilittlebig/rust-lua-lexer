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

struct Tokenizer<'a> {
    input: &'a str,
    pos: usize,
    tokens: Vec<TokenType>
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

    fn next(&mut self) -> Result<Option<TokenType>, ()> {
        next_token(self)
    }

    fn to_hex_digit(c: u8) -> u8 {
        if c >= b'0' && c <= b'9' {
            return c - b'0';
        } else {
            return ((c as char).to_ascii_lowercase() as u8) - b'a' + 10;
        }
    }

    #[allow(irrefutable_let_patterns)]
    fn read_single_line_comment(&mut self) -> Result<Option<TokenType>, ()> {
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

        if let _comment = str::from_utf8(&comment) {
            Ok(Some(TokenType::SingleComment))
        } else {
            Ok(Some(TokenType::Unidentified))
        }
    }

    #[allow(irrefutable_let_patterns)]
    fn read_multi_line_comment(&mut self) -> Result<Option<TokenType>, ()> {
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

        if let _comment = str::from_utf8(&comment) {
            Ok(Some(TokenType::MultiLineComment))
        } else {
            Ok(Some(TokenType::Unidentified))
        }
    }

    #[allow(irrefutable_let_patterns)]
    fn read_string(&mut self) -> Result<Option<TokenType>, ()> {
        let mut string: Vec<u8> = Vec::new();
        let mut is_closed = false;

        self.pos += 1;

        loop {
            if self.byte_at(0) != b'\"' && self.byte_at(0) != b'\'' && self.byte_at(0) != 3 {
                string.push(self.byte_at(0));
                self.pos += 1;
            } else if self.byte_at(0) == b'\"' || self.byte_at(0) == b'\'' {
                is_closed = true;
                self.pos += 1;
                break;
            } else {
                break;
            }
        }

        if let _string = str::from_utf8(&string) {
            if is_closed {
                Ok(Some(TokenType::StringLiteral))
            } else {
                Ok(Some(TokenType::UnclosedStringLiteral))
            }
        } else {
            Ok(Some(TokenType::Unidentified))
        }
    }

    #[allow(irrefutable_let_patterns)]
    fn read_digit(&mut self) -> Result<Option<TokenType>, ()> {
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
            let num = self.string_to_number(string.unwrap());
            match num {
                Some(TokenType::Int) => Ok(Some(TokenType::Int)),
                Some(TokenType::Float) => Ok(Some(TokenType::Float)),
                _ => Ok(None)
            }
        } else {
            Ok(Some(TokenType::Unidentified))
        }
    }

    fn string_to_number(&mut self, string: &str) -> Option<TokenType> {
        if let Some(_n) = self.string_to_int(string) {
            Some(TokenType::Int)
        } else if let Some(_t) = self.string_to_float(string) {
            Some(TokenType::Float)
        } else {
            None
        }
    }

    fn string_to_int(&mut self, string: &str) -> Option<TokenType> {
        let bytes = string.as_bytes();
        let len = bytes.len();

        let mut r = 0;
        let mut i = 0;

        if bytes.starts_with(b"0x") {
            i += 2;
            while i < len && Tokenizer::is_hex_digit(bytes[i]) {
                r = (r << 4) + Tokenizer::to_hex_digit(bytes[i]);
                i += 1;
            }
            Some(TokenType::Int)
        } else {
            if let Ok(_i) = string.parse::<i32>() {
                Some(TokenType::Int)
            } else {
                None
            }
        }
    }

    fn string_to_float(&self, string: &str) -> Option<TokenType> {
        if let Ok(_i) = string.parse::<f32>() {
            Some(TokenType::Float)
        } else {
            None
        }
    }
}

fn next_token<'a>(tokenizer: &mut Tokenizer<'a>) -> Result<Option<TokenType>, ()> {
    let token_type = match tokenizer.byte_at(0) {
        _ if tokenizer.is_whitespace() => {
            tokenizer.pos += 1;
            Some(TokenType::Whitespace)
        }
        _ if Tokenizer::is_digit(tokenizer.byte_at(0)) => tokenizer.read_digit()?,
        _ if tokenizer.is_escape_char() => {tokenizer.pos += 1; Some(TokenType::Whitespace)}

        b'\"' | b'\'' => tokenizer.read_string()?,
        b'>' => {
            if tokenizer.starts_with(b">=") {tokenizer.pos += 2; Some(TokenType::GreaterOrEqual)}
            else if tokenizer.starts_with(b">>") {tokenizer.pos += 2; Some(TokenType::ShiftRight)}
            else {tokenizer.pos += 1; Some(TokenType::Greater)}
        },
        b'.' => {
            if tokenizer.starts_with(b"..") {tokenizer.pos += 2; Some(TokenType::Concat)}
            else if tokenizer.starts_with(b"...") {tokenizer.pos += 3; Some(TokenType::Dots)}
            else {
                match tokenizer.byte_at(1) {
                    b'0'..=b'9' => {(); tokenizer.read_digit()?},
                    _ => {tokenizer.pos += 1; Some(TokenType::Attr)}
                }
            }
        },
        b'=' => {
            if tokenizer.starts_with(b"==") {tokenizer.pos += 2; Some(TokenType::Concat)}
            else {tokenizer.pos += 1; Some(TokenType::Assign)}
        },
        b'<' => {
            if tokenizer.starts_with(b"<=") {tokenizer.pos += 2; Some(TokenType::LessOrEqual)}
            else if tokenizer.starts_with(b"<<") {tokenizer.pos += 2; Some(TokenType::ShiftLeft)}
            else {tokenizer.pos += 1; Some(TokenType::Less)}
        },
        b'~' => {
            if tokenizer.starts_with(b"~=") {tokenizer.pos += 2; Some(TokenType::NotEqual)}
            else {tokenizer.pos += 1; Some(TokenType::BXor)}
        },
        b':' => {
            if tokenizer.starts_with(b"::") {tokenizer.pos += 2; Some(TokenType::DoubleColon)}
            else {tokenizer.pos += 1; Some(TokenType::Colon)}
        },
        b';' => {
            tokenizer.pos += 1;
            Some(TokenType::Semicolon)
        },
        b',' => {
            tokenizer.pos += 1;
            Some(TokenType::Comma)
        },
        b'&' => {
            tokenizer.pos += 1;
            Some(TokenType::BAnd)
        },
        b'|' => {
            tokenizer.pos += 1;
            Some(TokenType::BOr)
        },

        b'(' => {
            tokenizer.pos += 1;
            Some(TokenType::LeftParenthesis)
        },
        b')' => {
            tokenizer.pos += 1;
            Some(TokenType::RightParenthesis)
        },
        b'[' => {
            if tokenizer.starts_with(b"[[") {tokenizer.pos += 2; tokenizer.read_multi_line_comment()?}
            else {tokenizer.pos += 1; Some(TokenType::LeftSquareBracket)}
        },
        b']' => {
            tokenizer.pos += 1;
            Some(TokenType::RightSquareBracket)
        },
        b'{' => {
            tokenizer.pos += 1;
            Some(TokenType::LeftCurlyBracket)
        },
        b'}' => {
            tokenizer.pos += 1;
            Some(TokenType::RightCurlyBracket)
        },

        b'+' => {
            tokenizer.pos += 1;
            Some(TokenType::Add)
        },
        b'-' => {
            if tokenizer.starts_with(b"--[[") {tokenizer.pos += 4; tokenizer.read_multi_line_comment()?}
            else if tokenizer.starts_with(b"--") {tokenizer.pos += 2; tokenizer.read_single_line_comment()?}
            else {tokenizer.pos += 1; Some(TokenType::Minus)}
        },
        b'*' => {
            tokenizer.pos += 1;
            Some(TokenType::Mul)
        },
        b'/' => {
            tokenizer.pos += 1;
            Some(TokenType::Div)
        },
        b'%' => {
            tokenizer.pos += 1;
            Some(TokenType::Mod)
        },
        b'^' => {
            tokenizer.pos += 1;
            Some(TokenType::Pow)
        },
        b'#' => {
            tokenizer.pos += 1;
            Some(TokenType::Len)
        },

        _ => None
    };

    if let Some(t) = token_type {
        Ok(Some(t))
    } else if tokenizer.is_valid_ident_start() {
        let mut word: Vec<u8> = Vec::new();

        word.push(tokenizer.byte_at(0));
        tokenizer.pos += 1;

        while tokenizer.is_valid_ident() {
            word.push(tokenizer.byte_at(0));
            tokenizer.pos += 1;
        }

        if let Ok(s) = str::from_utf8(&word) {
            if let Some(t) = TokenType::convert_to_token_type(s) {
                Ok(Some(t))
            } else {
                Ok(Some(TokenType::Identifier))
            }
        } else {
            Ok(Some(TokenType::Unidentified))
        }
    } else {
        Ok(Some(TokenType::Unidentified))
    }
}


fn main() {
    let input = ".52
0x02
<<
>>
<
function
>
hello.world
func()
.
hello
-- hello this is a comment
--[[ HELLO
THIS IS
A
MULTILINE COMMENT
]]
\"Local Hello\"
ident = \"hello\"
end
521.224
,
3.1415
[[ MULTI
LINE
    COMMENT
]]
10E+4
12.13
151";

    let mut tokenizer = Tokenizer {
        input: input,
        pos: 0,
        tokens: Vec::new()
    };

    while !tokenizer.is_eof() {
        let token_type = tokenizer.next().unwrap().unwrap();
        tokenizer.tokens.push(token_type);
    }
    tokenizer.tokens.push(TokenType::Eof);

    for token in tokenizer.tokens {
        println!("{:#?}", token);
    }
}
