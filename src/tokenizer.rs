use std::iter::Peekable;
use std::str::Chars;

// --- Data Structures ---

#[derive(Debug, PartialEq, Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Fn,
    Let,
    // Types
    TypeU8,
    TypeI8,
    TypeU16,
    TypeI16,
    TypeU32,
    TypeI32,
    TypeU64,
    TypeI64,
    TypeStr,
    // Symbols
    LBrace,
    RBrace,
    LParen,
    RParen,
    LSquare,
    RSquare,
    Colon,
    DoubleColon,
    Semicolon,
    Equals,
    LAngle,
    RAngle,
    Comma,
    // Literals & Identifiers
    Identifier(String),
    IntLiteral(String),
    StringLiteral(String),

    // Unknown / Error
    Unknown(char),
}

// --- Emitter Interface ---

pub trait Emitter {
    type Output;
    fn emit(&mut self, token: Token, line: usize, col: usize) -> Self::Output;
}

/// Implementation 1: Original behavior (Raw Tokens)
pub struct TokenEmitter;
impl Emitter for TokenEmitter {
    type Output = Token;
    fn emit(&mut self, token: Token, _: usize, _: usize) -> Self::Output {
        token
    }
}

/// Implementation 2: Full Source Info
#[derive(Debug, PartialEq, Clone)]
pub struct LocatableToken {
    pub token: Token,
    pub loc: SourceLocation,
}

pub struct FullEmitter;
impl Emitter for FullEmitter {
    type Output = LocatableToken;
    fn emit(&mut self, token: Token, line: usize, col: usize) -> Self::Output {
        LocatableToken {
            token,
            loc: SourceLocation { line, col },
        }
    }
}

// --- The Lexer ---

pub struct Lexer<'a, E: Emitter> {
    input: Peekable<Chars<'a>>,
    emitter: E,
    line: usize,
    col: usize,
}

impl<'a, E: Emitter> Lexer<'a, E> {
    pub fn new(input: &'a str, emitter: E) -> Self {
        Lexer {
            input: input.chars().peekable(),
            emitter,
            line: 1,
            col: 1,
        }
    }

    // --- Internal Helpers ---

    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.input.next()?;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.next_char();
            true
        } else {
            false
        }
    }

    fn consume_while<F>(&mut self, mut predicate: F) -> String
    where
        F: FnMut(char) -> bool,
    {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if predicate(c) {
                s.push(c);
                self.next_char();
            } else {
                break;
            }
        }
        s
    }

    // --- Core Logic ---

    pub fn tokenize(&mut self) -> Vec<E::Output> {
        let mut tokens = Vec::new();

        while let Some(c) = self.peek() {
            let (l, c_idx) = (self.line, self.col);

            macro_rules! emit {
                ($tok:expr) => {{
                    let val = $tok;
                    tokens.push(self.emitter.emit(val, l, c_idx))
                }};
            }
            macro_rules! emit_adv {
                ($tok:expr) => {{
                    self.next_char();
                    emit!($tok);
                }};
            }

            match c {
                c if c.is_whitespace() => {
                    self.next_char();
                }

                '/' => {
                    self.next_char();
                    if self.match_next('/') {
                        self.consume_while(|c| c != '\n');
                    } else {
                        emit!(Token::Unknown('/'));
                    }
                }

                '{' => emit_adv!(Token::LBrace),
                '}' => emit_adv!(Token::RBrace),
                '(' => emit_adv!(Token::LParen),
                ')' => emit_adv!(Token::RParen),
                '[' => emit_adv!(Token::LSquare),
                ']' => emit_adv!(Token::RSquare),
                ';' => emit_adv!(Token::Semicolon),
                '=' => emit_adv!(Token::Equals),
                '<' => emit_adv!(Token::LAngle),
                '>' => emit_adv!(Token::RAngle),
                ',' => emit_adv!(Token::Comma),

                ':' => {
                    self.next_char();
                    emit!(if self.match_next(':') {
                        Token::DoubleColon
                    } else {
                        Token::Colon
                    });
                }

                '"' => emit!(self.read_string_literal()),
                '\'' => {
                    for tok in self.read_char_literal() {
                        emit!(tok);
                    }
                }

                c if c.is_alphabetic() || c == '_' => emit!(self.read_identifier_or_keyword()),
                c if c.is_ascii_digit() => emit!(self.read_number()),

                _ => emit_adv!(Token::Unknown(c)),
            }
        }
        tokens
    }

    fn read_string_literal(&mut self) -> Token {
        self.next_char(); // consume "
        let mut content = String::new();
        while let Some(c) = self.peek() {
            match c {
                '"' => {
                    self.next_char();
                    break;
                }
                '\\' => {
                    self.next_char();
                    if let Some(nc) = self.next_char() {
                        content.push(match nc {
                            'n' => '\n',
                            't' => '\t',
                            '\\' => '\\',
                            '"' => '"',
                            _ => nc,
                        });
                    }
                }
                _ => {
                    content.push(c);
                    self.next_char();
                }
            }
        }
        Token::StringLiteral(content)
    }

    fn read_char_literal(&mut self) -> Vec<Token> {
        self.next_char(); // consume '
        let mut content = String::new();
        while let Some(c) = self.peek() {
            match c {
                '\'' => {
                    self.next_char();
                    break;
                }
                '\\' => {
                    self.next_char();
                    if let Some(nc) = self.next_char() {
                        content.push(match nc {
                            'n' => '\n',
                            't' => '\t',
                            '\\' => '\\',
                            '\'' => '\'',
                            _ => nc,
                        });
                    }
                }
                _ => {
                    content.push(c);
                    self.next_char();
                }
            }
        }
        let val = content.chars().next().unwrap_or('\0') as u8;
        vec![Token::IntLiteral(val.to_string()), Token::TypeU8]
    }

    fn read_number(&mut self) -> Token {
        Token::IntLiteral(self.consume_while(|c| c.is_ascii_digit()))
    }

    fn read_identifier_or_keyword(&mut self) -> Token {
        // Includes ':' to handle std:: intrinsics as single atomic identifiers
        let ident = self.consume_while(|c| c.is_alphanumeric() || c == '_' || c == ':');
        match ident.as_str() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "u8" => Token::TypeU8,
            "i8" => Token::TypeI8,
            "u16" => Token::TypeU16,
            "i16" => Token::TypeI16,
            "u32" => Token::TypeU32,
            "i32" => Token::TypeI32,
            "u64" => Token::TypeU64,
            "i64" => Token::TypeI64,
            "str" => Token::TypeStr,
            _ => Token::Identifier(ident),
        }
    }
}
