use std::fmt::Display;
use std::sync::Arc;

use logos::Logos;
use miette::{Diagnostic, NamedSource, SourceSpan};
use serde::Serialize;
use serde::ser::SerializeStruct;
use thiserror::Error;

use crate::sources::{SourceCodeOrigin, SourceLocation};

// --- Data Structures ---

#[derive(Debug, PartialEq, Eq, Clone, Default, Error, Diagnostic, Serialize)]
pub enum LexingErrorKind {
    #[error("Invalid escape sequence: \\{0}")]
    InvalidEscapeSequence(char),
    #[error("Invalid character literal: {0}")]
    #[diagnostic(help(
        "Character literals must be a single ASCII character enclosed in single quotes"
    ))]
    InvalidCharLiteral(char),
    #[error("Unexpected character encountered")]
    UnexpectedCharacter,
    #[default]
    #[error("Unknown lexing error")]
    Other,
}

#[derive(Debug, Diagnostic, PartialEq, Eq, Clone)]
pub struct LexingError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("error occurred here")]
    pub span: SourceSpan,

    // The specific error variant
    #[diagnostic(transparent)]
    pub kind: LexingErrorKind,
}

impl Serialize for LexingError {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("LexingError", 4)?;
        state.serialize_field("src_name", &self.src.name())?;
        state.serialize_field("src_code", &self.src.inner())?;
        state.serialize_field("span", &self.span)?;
        state.serialize_field("kind", &self.kind)?;
        state.end()
    }
}

#[derive(Debug, Diagnostic, PartialEq, Eq, Clone, Error, Serialize)]
#[error("Lexing pass failed with {} error(s)", errors.len())]
pub struct LexingErrorCollection {
    #[related]
    pub errors: Vec<LexingError>,
}

pub type LexResult<T> = std::result::Result<T, LexingErrorCollection>;

impl From<LexingError> for LexingErrorCollection {
    fn from(value: LexingError) -> Self {
        Self {
            errors: vec![value],
        }
    }
}

impl Default for LexingError {
    fn default() -> Self {
        Self {
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

#[derive(Debug, PartialEq, Eq, Clone, Logos, Serialize)]
#[logos(skip r"[ \t\n\r]+")]
#[logos(error(LexingError, callback = |lex| LexingError {
    src: NamedSource::new("input", lex.source().to_string()),
    span: (lex.span().start, lex.span().end - lex.span().start).into(),
    kind: LexingErrorKind::UnexpectedCharacter,
}))]
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
    CharLiteral(char),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::Fn => write!(f, "`fn`"),
            Self::Let => write!(f, "`let`"),
            Self::TypeU8 => write!(f, "`u8`"),
            Self::TypeI8 => write!(f, "`i8`"),
            Self::TypeU16 => write!(f, "`u16`"),
            Self::TypeI16 => write!(f, "`i16`"),
            Self::TypeU32 => write!(f, "`u32`"),
            Self::TypeI32 => write!(f, "`i32`"),
            Self::TypeU64 => write!(f, "`u64`"),
            Self::TypeI64 => write!(f, "`i64`"),
            Self::TypeStr => write!(f, "`str`"),
            Self::LBrace => write!(f, "`{{`"),
            Self::RBrace => write!(f, "`}}`"),
            Self::LParen => write!(f, "`(`"),
            Self::RParen => write!(f, "`)`"),
            Self::LSquare => write!(f, "`[`"),
            Self::RSquare => write!(f, "`]`"),
            Self::Colon => write!(f, "`:`"),
            Self::DoubleColon => write!(f, "`::`"),
            Self::Semicolon => write!(f, "`;`"),
            Self::Equals => write!(f, "`=`"),
            Self::LAngle => write!(f, "`<`"),
            Self::RAngle => write!(f, "`>`"),
            Self::Comma => write!(f, "`,`"),
            Self::Identifier(s) => write!(f, "`{s}`"),
            Self::IntLiteral(s) => write!(f, "`{s}`"),
            Self::StringLiteral(s) => write!(f, "`\"{s}\"`"),
            Self::CharLiteral(c) => write!(f, "`'{c}'`"),
        }
    }
}

fn parse_string(lex: &logos::Lexer<Token>) -> std::result::Result<String, LexingError> {
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
                    'r' => result.push('\r'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    '\'' => result.push('\''),
                    _ => {
                        return Err(LexingError {
                            src: NamedSource::new("input", source.to_string()),
                            span: (span.start, span.end - span.start).into(),
                            kind: LexingErrorKind::InvalidEscapeSequence(escaped),
                        });
                    }
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

fn parse_char(lex: &logos::Lexer<Token>) -> std::result::Result<char, LexingError> {
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
                    'r' => '\r',
                    '\\' => '\\',
                    '\'' => '\'',
                    _ => {
                        return Err(LexingError {
                            src: NamedSource::new("input", source.to_string()),
                            span: (span.start, span.end - span.start).into(),
                            kind: LexingErrorKind::InvalidEscapeSequence(escaped),
                        });
                    }
                }
            } else {
                return Err(LexingError {
                    src: NamedSource::new("input", source.to_string()),
                    span: (span.start, span.end - span.start).into(),
                    kind: LexingErrorKind::UnexpectedCharacter,
                });
            }
        } else if !c.is_ascii() {
            return Err(LexingError {
                src: NamedSource::new("input", source.to_string()),
                span: (span.start, span.end - span.start).into(),
                kind: LexingErrorKind::InvalidCharLiteral(c),
            });
        } else {
            return Ok(c);
        }
    } else {
        return Err(LexingError {
            src: NamedSource::new("input", source.to_string()),
            span: (span.start, span.end - span.start).into(),
            kind: LexingErrorKind::UnexpectedCharacter,
        });
    };
    assert!(
        chars.next() == Some('\''),
        "Char literal not properly closed"
    );
    Ok(c)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Locatable<T> {
    pub value: T,
    pub loc: SourceLocation,
}

// --- The Lexer ---

pub struct Lexer<'a> {
    input: &'a str,
    origin: SourceCodeOrigin,
}

impl<'a> Lexer<'a> {
    #[must_use]
    pub fn new(input: &'a str, filename: Option<String>) -> Self {
        Lexer {
            input,
            origin: filename.map_or_else(
                || SourceCodeOrigin::Anon(Arc::new(input.to_string())),
                |name| SourceCodeOrigin::File(name.into()),
            ),
        }
    }

    // --- Core Logic ---

    #[must_use]
    pub fn finalize_error(&self, lex: &logos::Lexer<Token>, err: LexingError) -> LexingError {
        LexingError {
            src: NamedSource::new(
                match &self.origin {
                    SourceCodeOrigin::File(name) => name.as_ref(),
                    SourceCodeOrigin::Anon(code) => code.as_ref(),
                },
                self.input.to_string(),
            ),
            span: (lex.span().start, lex.span().end - lex.span().start).into(),
            kind: err.kind,
        }
    }

    pub fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut lexer = Token::lexer(self.input);
        let mut errors = Vec::new();
        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => tokens.push(t),
                Err(e) => {
                    errors.push(if e.kind == LexingErrorKind::Other {
                        self.finalize_error(&lexer, e)
                    } else {
                        e
                    });
                }
            }
        }
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(LexingErrorCollection { errors })
        }
    }

    pub fn tokenize_with_locations(&mut self) -> LexResult<Vec<Locatable<Token>>> {
        let mut tokens = Vec::new();
        let mut lexer = Token::lexer(self.input);
        let mut errors = Vec::new();
        while let Some(token) = lexer.next() {
            match token {
                Ok(t) => {
                    let loc = SourceLocation {
                        span: (lexer.span().start, lexer.span().end - lexer.span().start).into(),
                        origin: Some(self.origin.clone()),
                    };
                    tokens.push(Locatable { value: t, loc });
                }
                Err(e) => {
                    errors.push(if e.kind == LexingErrorKind::Other {
                        self.finalize_error(&lexer, e)
                    } else {
                        e
                    });
                }
            }
        }
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(LexingErrorCollection { errors })
        }
    }
}
