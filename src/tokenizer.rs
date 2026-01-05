use std::{default, fmt::Display};
use std::iter::Peekable;
use std::str::Chars;
use std::sync::Arc;

use logos::Logos;
use miette::{Diagnostic, NamedSource, SourceSpan, Result};
use thiserror::Error;

// --- Data Structures ---

#[derive(Debug, PartialEq, Clone)]
pub struct SourceLocation {
    pub span: SourceSpan,
    pub origin: SourceCodeOrigin,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SourceCodeOrigin {
    File(String), //String contains the file path
    Anon(Arc<String>), //String contains the code itself
}

#[derive(Debug, PartialEq, Clone, Default, Error, Diagnostic)]
pub enum LexingErrorKind {
    #[error("Invalid escape sequence: \\{0}")]
    InvalidEscapeSequence(char),
    #[error("Invalid character literal: {0}")]
    InvalidCharLiteral(char),
    #[error("Unexpected character encountered")]
    UnexpectedCharacter,
    #[default]
    #[error("Unknown lexing error")]
    Other,
}

#[derive(Debug, Diagnostic, PartialEq, Clone)]
pub struct LexingError {
    #[source_code]
    pub src: NamedSource<String>,
    
    #[label("error occurred here")]
    pub span: SourceSpan,
    
    // The specific error variant
    #[diagnostic(transparent)]
    pub kind: LexingErrorKind,
}

impl Default for LexingError {
    fn default() -> Self {
        LexingError {
            src: NamedSource::new("input", String::new()),
            span: SourceSpan::new(0.into(), 0),
            kind: LexingErrorKind::Other,
        }
    }
}

impl Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl std::error::Error for LexingError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.kind.source()
    }
}


#[derive(Debug, PartialEq, Clone, Logos)]
#[logos(skip r"[ \t\n\r]+")]
#[logos(error = LexingError)]
pub enum Token {
    // Keywords
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    // Types
    #[token("u8")]
    TypeU8,
    #[token("i8")]
    TypeI8,
    #[token("u16")]
    TypeU16,
    #[token("i16")]
    TypeI16,
    #[token("u32")]
    TypeU32,
    #[token("i32")]
    TypeI32,
    #[token("u64")]
    TypeU64,
    #[token("i64")]
    TypeI64,
    #[token("str")]
    TypeStr,
    // Symbols
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(";")]
    Semicolon,
    #[token("=")]
    Equals,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token(",")]
    Comma,
    // Literals & Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r"[0-9]+", |lex| lex.slice().to_string())]
    IntLiteral(String),
    #[regex(r#""(?:\\.|[^"\\])*""#, |lex| parse_string(lex))]
    StringLiteral(String),
    #[regex(r#"'(?:\\.|[^'\\])'"#, |lex| parse_char(lex))]
    CharLiteral(String),
}

fn parse_string(lex: &mut logos::Lexer<Token>) -> Result<String, LexingError> {
    let s = lex.slice();
    let span = lex.span();
    let source = lex.source();
    
    let mut result = String::new();
    let mut chars = s.chars();
    chars.next(); // skip opening quote
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(escaped) = chars.next() {
                match escaped {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    '\'' => result.push('\''),
                    _ => return Err(LexingError {
                        src: NamedSource::new("input", source.to_string()),
                        span: (span.start, span.end - span.start).into(),
                        kind: LexingErrorKind::InvalidEscapeSequence(escaped),
                    }),
                }
            }
        } else if c == '"' {
            break;
        } else {
            result.push(c);
        }
    }
    assert!(chars.next().is_none(), "String literal not properly closed");
    Ok(result)
}

fn parse_char(lex: &mut logos::Lexer<Token>) -> Result<String, LexingError> {
    let s = lex.slice();
    let span = lex.span();
    let source = lex.source();
    
    let mut chars = s.chars();
    chars.next(); // skip opening quote
    let c = if let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(escaped) = chars.next() {
                match escaped {
                    'n' => '\n',
                    't' => '\t',
                    '\\' => '\\',
                    '\'' => '\'',
                    _ => return Err(LexingError {
                        src: NamedSource::new("input", source.to_string()),
                        span: (span.start, span.end - span.start).into(),
                        kind: LexingErrorKind::InvalidEscapeSequence(escaped),
                    }),
                }
            } else {
                return Err(LexingError {
                    src: NamedSource::new("input", source.to_string()),
                    span: (span.start, span.end - span.start).into(),
                    kind: LexingErrorKind::UnexpectedCharacter,
                });
            }
        } else {
            c
        }
    } else {
        return Err(LexingError {
            src: NamedSource::new("input", source.to_string()),
            span: (span.start, span.end - span.start).into(),
            kind: LexingErrorKind::UnexpectedCharacter,
        });
    };
    assert!(chars.next() == Some('\''), "Char literal not properly closed");
    Ok(c.to_string())
}


/// Implementation 2: Full Source Info
#[derive(Debug, PartialEq, Clone)]
pub struct LocatableToken {
    pub token: Token,
    pub loc: SourceLocation,
}



// --- The Lexer ---

pub struct Lexer<'a> {
    input: &'a str,
    origin: SourceCodeOrigin,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, filename: Option<String>) -> Self {
        Lexer {
            input,
            origin: match filename {
                Some(name) => SourceCodeOrigin::File(name),
                None => SourceCodeOrigin::Anon(Arc::new(input.to_string())),
            },
        }
    }

    // --- Core Logic ---

    pub fn finalize_error(&self, lex: &logos::Lexer<Token>, err: LexingError) -> LexingError {
        LexingError {
            src: NamedSource::new(
                match &self.origin {
                    SourceCodeOrigin::File(name) => name.clone(),
                    SourceCodeOrigin::Anon(code) => code.as_ref().clone(),
                },
                self.input.to_string(),
            ),
            span: (lex.span().start, lex.span().end - lex.span().start).into(),
            kind: err.kind,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut lexer = Token::lexer(self.input);
        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => tokens.push(t),
                Err(e) => {
                    if e.kind == LexingErrorKind::Other {
                        Err(self.finalize_error(&lexer, e))?;
                    } else {
                        Err(e)?;
                    } 
                }
            }
        }
        Ok(tokens)
    }

    pub fn tokenize_with_locations(&mut self) -> Result<Vec<LocatableToken>> {
        let mut tokens = Vec::new();
        let mut lexer = Token::lexer(self.input);
        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    let loc = SourceLocation {
                        span: (lexer.span().start, lexer.span().end - lexer.span().start).into(),
                        origin: self.origin.clone(),
                    };
                    tokens.push(LocatableToken { token: t, loc });
                }
                Err(e) => {
                    if e.kind == LexingErrorKind::Other {
                        Err(self.finalize_error(&lexer, e))?;
                    } else {
                        Err(e)?;
                    } 
                }
            }
        }
        Ok(tokens)
    }

}
