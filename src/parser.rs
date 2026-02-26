use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::num::IntErrorKind;

use clap::error;
use miette::{Diagnostic, NamedSource, SourceSpan};
use serde::Serialize;
use serde::ser::SerializeStruct;
use thiserror::Error;

use crate::sources::SourceCodeOrigin;
use crate::tokenizer::{Locatable, Token};

// --- AST Definitions ---

pub trait ASTAnnotation: Sized + Clone + Debug {
    type ProgramAnnotation: Debug + Clone + Serialize;
    fn construct_program_annotation(
        &mut self,
        functions: &[Function<Self>],
        function_name_mapping: &HashMap<FunctionId, String>,
    ) -> Self::ProgramAnnotation;
    type FunctionParamAnnotation: Debug + Clone + Serialize;
    fn construct_function_param_annotation(
        &mut self,
        identifier_token: &Locatable<Token>,
        colon_token: &Locatable<Token>,
        type_node: &ASTTypeNode<Self>,
        variable_index: &VariableId,
    ) -> Self::FunctionParamAnnotation;
    type FunctionAnnotation: Debug + Clone + Serialize;
    fn construct_function_annotation(
        &mut self,
        fn_token: &Locatable<Token>,
        name_token: &Locatable<Token>,
        left_paren_token: &Locatable<Token>,
        params: &[FunctionParam<Self>],
        comma_tokens: &[Locatable<Token>],
        right_paren_token: &Locatable<Token>,
        body: &Block<Self>,
        function_id: &FunctionId,
    ) -> Self::FunctionAnnotation;
    type BlockAnnotation: Debug + Clone + Serialize;
    fn construct_block_annotation(
        &mut self,
        left_brace_token: &Locatable<Token>,
        statements: &[BlockItem<Self>],
        right_brace_token: &Locatable<Token>,
    ) -> Self::BlockAnnotation;
    type StatementAnnotation: Debug + Clone + Serialize;
    fn construct_expression_statement_annotation(
        &mut self,
        expr: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation;
    fn construct_assignment_annotation(
        &mut self,
        identifier_token: &Locatable<Token>,
        equals_token: &Locatable<Token>,
        value: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation;
    fn construct_var_decl_annotation(
        &mut self,
        let_token: &Locatable<Token>,
        identifier_token: &Locatable<Token>,
        colon_token: &Locatable<Token>,
        type_node: &ASTTypeNode<Self>,
        equals_token: &Locatable<Token>,
        value: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
        variable_index: &VariableId,
    ) -> Self::StatementAnnotation;
    type ExpressionAnnotation: Debug + Clone + Serialize;
    fn construct_string_literal_annotation(
        &mut self,
        string_token: &Locatable<Token>,
        value: &str,
    ) -> Self::ExpressionAnnotation;
    fn construct_int_literal_annotation(
        &mut self,
        int_token: &Locatable<Token>,
        int_value: &str,
        type_node: &ASTTypeNode<Self>,
        parsed_value: &IntLiteral,
    ) -> Self::ExpressionAnnotation;
    fn construct_array_access_annotation(
        &mut self,
        array: &VariableAccess<Self>,
        left_bracket_token: &Locatable<Token>,
        index_expr: &Expression<Self>,
        right_bracket_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation;
    fn construct_fn_call_annotation(
        &mut self,
        qualified_name: &QualifiedName<Self>,
        left_paren_token: &Locatable<Token>,
        arguments: &[Expression<Self>],
        comma_tokens: &[Locatable<Token>],
        right_paren_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation;
    fn construct_variable_access_expression_annotation(
        &mut self,
        variable_access: &VariableAccess<Self>,
    ) -> Self::ExpressionAnnotation;
    type VariableAccessAnnotation: Debug + Clone + Serialize;
    fn construct_variable_access_annotation(
        &mut self,
        name_token: &Locatable<Token>,
        name: &str,
        variable_id: &VariableId,
    ) -> Self::VariableAccessAnnotation;
    type QualifiedIdentifierAnnotation: Debug + Clone + Serialize;
    fn construct_qualified_identifier_annotation(
        &mut self,
        identifier_tokens: &[Locatable<Token>],
        double_colon_tokens: &[Locatable<Token>],
        parts: &[String],
        name: &str,
    ) -> Self::QualifiedIdentifierAnnotation;
    type ASTTypeNodeAnnotation: Debug + Clone + Serialize;
    fn construct_simple_type_node_annotation(
        &mut self,
        type_token: &Locatable<Token>,
        kind: &ASTTypeKind,
    ) -> Self::ASTTypeNodeAnnotation;
    fn construct_str_type_node_annotation(
        &mut self,
        type_token: &Locatable<Token>,
        kind: &ASTTypeKind,
        left_angle_token: &Locatable<Token>,
        size_token: &Locatable<Token>,
        right_angle_token: &Locatable<Token>,
    ) -> Self::ASTTypeNodeAnnotation;
}

#[derive(Debug, PartialEq, Clone, Default, Error, Diagnostic, Serialize)]
pub enum ParsingErrorKind {
    // #[error("Invalid escape sequence: \\{0}")]
    // InvalidEscapeSequence(char),
    // #[error("Invalid character literal: {0}")]
    // #[diagnostic(help(
    //     "Character literals must be a single ASCII character enclosed in single quotes"
    // ))]
    // InvalidCharLiteral(char),
    // #[error("Unexpected character encountered")]
    // UnexpectedCharacter,
    #[error("Unexpected token: expected {expected:?}, found {found:?}")]
    WrongToken { expected: Token, found: Token },
    #[error("Unexpected end of input")]
    UnexpectedEOF,
    #[error("Unexpected token, expected one of {expected:?}, found {found:?}")]
    WrongTokenOneOf { expected: Vec<Token>, found: Token },
    #[error("Expected identifier, found {found:?}")]
    ExpectedIdentifier { found: Token },
    #[error("Variable '{name}' already declared in this scope")]
    VariableAlreadyDeclared { name: String },
    #[error("Expected token type {expected:?}, found {found:?}")]
    ExpectedTokenType { expected: String, found: Token },
    #[error("Variable '{name}' not declared before assignment")]
    VariableNotDeclared { name: String },
    #[error("Expected expression, found {found:?}")]
    ExpectedExpression { found: Token },
    #[error("Expected integer literal, found {found:?}")]
    ExpectedIntLiteral { found: Token },
    #[error("Cannot parse integer literal '{literal}' as non-integer type {type_name}")]
    InvalidIntLiteralTypeMismatch { literal: String, type_name: String },
    #[error("Integer literal parsing error: {message}")]
    InvalidIntLiteral { message: String },
    #[error("Expected string literal, found {found:?}")]
    ExpectedStringLiteral { found: Token },
    #[error("Expected type token, found {found:?}")]
    ExpectedType { found: Token },
    #[default]
    #[error("Unknown Parsing error")]
    Other,
}

#[derive(Debug, Diagnostic, PartialEq, Clone)]
pub struct ParsingError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("error occurred here")]
    pub span: SourceSpan,

    // The specific error variant
    #[diagnostic(transparent)]
    pub kind: ParsingErrorKind,
}

impl Serialize for ParsingError {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("ParsingError", 4)?;
        state.serialize_field("src_name", &self.src.name())?;
        state.serialize_field("src_code", &self.src.inner())?;
        state.serialize_field("span", &self.span)?;
        state.serialize_field("kind", &self.kind)?;
        state.end()
    }
}

#[derive(Debug, Diagnostic, PartialEq, Clone, Error, Serialize)]
#[error("Parsing pass failed with {} error(s)", errors.len())]
pub struct ParsingErrorCollection {
    #[related]
    pub errors: Vec<ParsingError>,
}

pub type ParseResult<T> = std::result::Result<T, ParsingErrorCollection>;

impl From<ParsingError> for ParsingErrorCollection {
    fn from(value: ParsingError) -> Self {
        Self {
            errors: vec![value],
        }
    }
}

impl Default for ParsingError {
    fn default() -> Self {
        ParsingError {
            src: NamedSource::new("input", String::new()),
            span: SourceSpan::new(0.into(), 0),
            kind: ParsingErrorKind::Other,
        }
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

impl std::error::Error for ParsingError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.kind.source()
    }
}

impl ASTAnnotation for () {
    type ProgramAnnotation = ();

    fn construct_program_annotation(
        &mut self,
        _functions: &[Function<Self>],
        _function_name_mapping: &HashMap<FunctionId, String>,
    ) -> Self::ProgramAnnotation {
        ()
    }

    type FunctionParamAnnotation = ();

    fn construct_function_param_annotation(
        &mut self,
        _identifier_token: &Locatable<Token>,
        _colon_token: &Locatable<Token>,
        _type_node: &ASTTypeNode<Self>,
        _variable_index: &VariableId,
    ) -> Self::FunctionParamAnnotation {
        ()
    }

    type FunctionAnnotation = ();

    fn construct_function_annotation(
        &mut self,
        _fn_token: &Locatable<Token>,
        _name_token: &Locatable<Token>,
        _left_paren_token: &Locatable<Token>,
        _params: &[FunctionParam<Self>],
        _comma_tokens: &[Locatable<Token>],
        _right_paren_token: &Locatable<Token>,
        _body: &Block<Self>,
        _function_id: &FunctionId,
    ) -> Self::FunctionAnnotation {
        ()
    }

    type BlockAnnotation = ();

    fn construct_block_annotation(
        &mut self,
        _left_brace_token: &Locatable<Token>,
        _statements: &[BlockItem<Self>],
        _right_brace_token: &Locatable<Token>,
    ) -> Self::BlockAnnotation {
        ()
    }

    type StatementAnnotation = ();

    fn construct_expression_statement_annotation(
        &mut self,
        _expr: &Expression<Self>,
        _semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation {
        ()
    }

    fn construct_assignment_annotation(
        &mut self,
        _identifier_token: &Locatable<Token>,
        _equals_token: &Locatable<Token>,
        _value: &Expression<Self>,
        _semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation {
        ()
    }

    fn construct_var_decl_annotation(
        &mut self,
        _let_token: &Locatable<Token>,
        _identifier_token: &Locatable<Token>,
        _colon_token: &Locatable<Token>,
        _type_node: &ASTTypeNode<Self>,
        _equals_token: &Locatable<Token>,
        _value: &Expression<Self>,
        _semicolon_token: &Locatable<Token>,
        _variable_index: &VariableId,
    ) -> Self::StatementAnnotation {
        ()
    }

    type ExpressionAnnotation = ();

    fn construct_string_literal_annotation(
        &mut self,
        _string_token: &Locatable<Token>,
        _value: &str,
    ) -> Self::ExpressionAnnotation {
        ()
    }

    fn construct_int_literal_annotation(
        &mut self,
        _int_token: &Locatable<Token>,
        _int_value: &str,
        _type_node: &ASTTypeNode<Self>,
        _parsed_value: &IntLiteral,
    ) -> Self::ExpressionAnnotation {
        ()
    }

    fn construct_array_access_annotation(
        &mut self,
        _array: &VariableAccess<Self>,
        _left_bracket_token: &Locatable<Token>,
        _index_expr: &Expression<Self>,
        _right_bracket_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation {
        ()
    }

    fn construct_fn_call_annotation(
        &mut self,
        _qualified_name: &QualifiedName<Self>,
        _left_paren_token: &Locatable<Token>,
        _arguments: &[Expression<Self>],
        _comma_tokens: &[Locatable<Token>],
        _right_paren_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation {
        ()
    }

    type VariableAccessAnnotation = ();

    fn construct_variable_access_annotation(
        &mut self,
        _name_token: &Locatable<Token>,
        _name: &str,
        _variable_id: &VariableId,
    ) -> Self::VariableAccessAnnotation {
        ()
    }

    type QualifiedIdentifierAnnotation = ();

    fn construct_qualified_identifier_annotation(
        &mut self,
        _identifier_tokens: &[Locatable<Token>],
        _double_colon_tokens: &[Locatable<Token>],
        _parts: &[String],
        _name: &str,
    ) -> Self::QualifiedIdentifierAnnotation {
        ()
    }

    fn construct_variable_access_expression_annotation(
        &mut self,
        _variable_access: &VariableAccess<Self>,
    ) -> Self::ExpressionAnnotation {
        ()
    }

    type ASTTypeNodeAnnotation = ();

    fn construct_simple_type_node_annotation(
        &mut self,
        _type_token: &Locatable<Token>,
        _kind: &ASTTypeKind,
    ) -> Self::ASTTypeNodeAnnotation {
        ()
    }

    fn construct_str_type_node_annotation(
        &mut self,
        _type_token: &Locatable<Token>,
        _kind: &ASTTypeKind,
        _left_angle_token: &Locatable<Token>,
        _size_token: &Locatable<Token>,
        _right_angle_token: &Locatable<Token>,
    ) -> Self::ASTTypeNodeAnnotation {
        ()
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Program<Annotation: ASTAnnotation> {
    pub functions: Vec<Function<Annotation>>,
    pub function_name_mapping: HashMap<FunctionId, String>,
    pub annotation: Annotation::ProgramAnnotation,
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionParam<Annotation: ASTAnnotation> {
    pub type_: ASTTypeNode<Annotation>,
    pub variable_index: VariableId,
    pub annotation: Annotation::FunctionParamAnnotation,
}

#[derive(Debug, Clone, Serialize)]
pub struct Function<Annotation: ASTAnnotation> {
    pub name: String,
    pub params: Vec<FunctionParam<Annotation>>,
    pub body: Block<Annotation>,
    pub id: FunctionId,
    pub variable_name_mapping: HashMap<VariableId, String>,
    pub annotation: Annotation::FunctionAnnotation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct FunctionId(pub(crate) usize);

impl Display for FunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FUNCTION_ID({})", self.0)
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum BlockItem<Annotation: ASTAnnotation> {
    Statement(Statement<Annotation>),
    Block(Block<Annotation>),
}

#[derive(Debug, Clone, Serialize)]
pub struct Block<Annotation: ASTAnnotation> {
    pub statements: Vec<BlockItem<Annotation>>,
    pub annotation: Annotation::BlockAnnotation,
}

#[derive(Debug, Clone, Serialize)]
pub enum Statement<Annotation: ASTAnnotation> {
    // let x: type = expr;
    VarDecl {
        name: String,
        type_: ASTTypeNode<Annotation>,
        value: Expression<Annotation>,
        variable_index: VariableId,
        annotation: Annotation::StatementAnnotation,
    },
    // x = expr;
    Assignment {
        var: VariableId,
        value: Expression<Annotation>,
        annotation: Annotation::StatementAnnotation,
    },

    // expr;
    Expression {
        expr: Expression<Annotation>,
        annotation: Annotation::StatementAnnotation,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum IntLiteral {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
}

#[derive(Debug, Clone, Serialize)]
pub struct VariableAccess<Annotation: ASTAnnotation> {
    pub id: VariableId,
    pub annotation: Annotation::VariableAccessAnnotation,
}

#[derive(Debug, Clone, Serialize)]
pub enum Expression<Annotation: ASTAnnotation> {
    IntLiteral {
        value: IntLiteral,
        annotation: Annotation::ExpressionAnnotation,
    },
    StringLiteral {
        value: String,
        annotation: Annotation::ExpressionAnnotation,
    },
    VariableAccess {
        value: VariableAccess<Annotation>,
        annotation: Annotation::ExpressionAnnotation,
    },
    ArrayAccess {
        array: VariableAccess<Annotation>,
        index_expr: Box<Expression<Annotation>>,
        annotation: Annotation::ExpressionAnnotation,
    },
    FnCall {
        qualified_name: QualifiedName<Annotation>,
        arguments: Vec<Expression<Annotation>>,
        annotation: Annotation::ExpressionAnnotation,
    },
}

impl<Annotation: ASTAnnotation> Expression<Annotation> {
    pub fn annotation(&self) -> &Annotation::ExpressionAnnotation {
        match self {
            Expression::IntLiteral { annotation, .. } => annotation,
            Expression::StringLiteral { annotation, .. } => annotation,
            Expression::VariableAccess { annotation, .. } => annotation,
            Expression::ArrayAccess { annotation, .. } => annotation,
            Expression::FnCall { annotation, .. } => annotation,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct VariableId(usize);

#[derive(Debug, Clone, Serialize)]
pub struct QualifiedName<Annotation: ASTAnnotation> {
    pub parts: Vec<String>,
    pub annotation: Annotation::QualifiedIdentifierAnnotation,
}

impl<Annotation: ASTAnnotation> QualifiedName<Annotation> {
    pub fn full_name(&self) -> String {
        self.parts.join("::")
    }
}

pub struct Parser<'a, Annotation: ASTAnnotation> {
    tokens: Vec<Locatable<Token>>,
    pos: usize,
    variable_index: VariableId,
    variable_tracker: Vec<HashMap<String, VariableId>>,
    function_name_mapping: HashMap<FunctionId, String>,
    variable_name_mapping: HashMap<VariableId, String>,
    annotation_processor: &'a mut Annotation,
}

impl<'a, Annotation: ASTAnnotation> Parser<'a, Annotation> {
    pub fn new(tokens: Vec<Locatable<Token>>, annotation_processor: &'a mut Annotation) -> Self {
        Parser {
            tokens,
            pos: 0,
            variable_index: VariableId(0),
            variable_tracker: vec![],
            function_name_mapping: HashMap::new(),
            variable_name_mapping: HashMap::new(),
            annotation_processor,
        }
    }

    // --- Helpers ---

    fn peek(&self) -> Option<&Locatable<Token>> {
        self.tokens.get(self.pos)
    }

    fn peek2(&self) -> Option<&Locatable<Token>> {
        self.tokens.get(self.pos + 1)
    }

    fn last(&self) -> Option<&Locatable<Token>> {
        if self.pos == 0 {
            None
        } else {
            self.tokens.get(self.pos - 1)
        }
    }

    fn advance(&mut self) -> Option<Locatable<Token>> {
        let t = self.tokens.get(self.pos).cloned();
        self.pos += 1;
        t
    }

    fn source_for_token(token: &Locatable<Token>) -> NamedSource<String> {
        match &token.loc.origin {
            Some(SourceCodeOrigin::File(filename)) => NamedSource::new(
                filename.to_string(),
                std::fs::read_to_string(&**filename)
                    .unwrap_or_else(|_| format!("Could not read source file: {}", filename)),
            ),
            Some(SourceCodeOrigin::Anon(code)) => NamedSource::new("anonymous", code.to_string()),
            None => NamedSource::new("input", String::new()),
        }
    }

    fn expect(&mut self, expected: Token) -> ParseResult<Locatable<Token>> {
        if self.peek().map(|lt| &lt.value) == Some(&expected) {
            Ok(self.advance().unwrap())
        } else {
            let found = self.peek().expect("Expected a token to be available");

            return Err(ParsingError {
                src: Self::source_for_token(found),
                span: found.loc.span,
                kind: ParsingErrorKind::WrongToken {
                    expected,
                    found: found.value.clone(),
                },
            }
            .into());
        }
    }

    // --- Parsing Logic ---

    pub fn parse_program(&mut self) -> ParseResult<Program<Annotation>> {
        let mut functions = Vec::new();
        let mut current_function_id = FunctionId(0);
        while self.peek().is_some() {
            functions.push(self.parse_function(current_function_id)?);
            current_function_id = FunctionId(current_function_id.0 + 1);
        }
        let annotation = Annotation::construct_program_annotation(
            self.annotation_processor,
            &functions,
            &self.function_name_mapping,
        );
        Ok(Program {
            functions,
            function_name_mapping: std::mem::take(&mut self.function_name_mapping),
            annotation,
        })
    }

    fn parse_function(&mut self, id: FunctionId) -> ParseResult<Function<Annotation>> {
        self.variable_index = VariableId(0);
        self.variable_tracker.clear();
        self.variable_tracker.push(HashMap::new());
        self.variable_name_mapping.clear();
        // Grammar: "fn" ID "(" ")" Block
        let fn_token = self.expect(Token::Fn)?;

        let name_token = self.advance().expect("should have a name token");

        let name = match &name_token.value {
            Token::Identifier(n) => n.clone(),
            t => {
                return Err(ParsingError {
                    src: Self::source_for_token(&name_token),
                    span: name_token.loc.span,
                    kind: ParsingErrorKind::WrongToken {
                        expected: Token::Identifier("function_name".to_string()),
                        found: t.clone(),
                    },
                }
                .into());
            }
        };

        self.function_name_mapping.insert(id, name.clone());

        let left_paren_token = self.expect(Token::LParen)?;

        let mut params = Vec::new();
        let mut param_comma_tokens = Vec::new();
        //parse parameters
        //syntax: fn function_name(param: Type)
        while self.peek().map(|t| &t.value) != Some(&Token::RParen) {
            let param_identifier_token = self.advance().expect("should have a name token");

            let param_name = match &param_identifier_token.value {
                Token::Identifier(n) => n.clone(),
                t => {
                    return Err(ParsingError {
                        src: Self::source_for_token(&param_identifier_token),
                        span: param_identifier_token.loc.span,
                        kind: ParsingErrorKind::WrongToken {
                            expected: Token::Identifier("parameter_name".to_string()),
                            found: t.clone(),
                        },
                    }
                    .into());
                }
            };

            let param_colon_token = self.expect(Token::Colon)?;

            let param_type = self.parse_type()?;

            // assign variable index
            let var_index = self.variable_index;
            self.variable_index = VariableId(self.variable_index.0 + 1);
            self.variable_tracker
                .last_mut()
                .unwrap()
                .insert(param_name.clone(), var_index);

            self.variable_name_mapping
                .insert(var_index, param_name.clone());

            let param_annotation = Annotation::construct_function_param_annotation(
                self.annotation_processor,
                &param_identifier_token,
                &param_colon_token,
                &param_type,
                &var_index,
            );
            params.push(FunctionParam {
                variable_index: var_index,
                type_: param_type,
                annotation: param_annotation,
            });

            if self.peek().map(|t| &t.value) == Some(&Token::Comma) {
                let comma_token = self.advance().unwrap();
                param_comma_tokens.push(comma_token);
            } else {
                break; // no more parameters
            }
        }

        let right_paren_token = self.expect(Token::RParen)?;

        let body = self.parse_block()?;

        let annotation = Annotation::construct_function_annotation(
            self.annotation_processor,
            &fn_token,
            &name_token,
            &left_paren_token,
            &params,
            &param_comma_tokens,
            &right_paren_token,
            &body,
            &id,
        );
        Ok(Function {
            name,
            params,
            id,
            variable_name_mapping: std::mem::take(&mut self.variable_name_mapping),
            body,
            annotation,
        })
    }

    fn parse_block(&mut self) -> ParseResult<Block<Annotation>> {
        self.variable_tracker.push(HashMap::new());
        let left_brace_token = self.expect(Token::LBrace)?;
        let mut statements = Vec::new();

        while self.peek().is_some() && self.peek().map(|t| &t.value) != Some(&Token::RBrace) {
            statements.push(self.parse_statement_or_block()?);
        }

        let right_brace_token = self.expect(Token::RBrace)?;
        self.variable_tracker.pop();
        let annotation = Annotation::construct_block_annotation(
            self.annotation_processor,
            &left_brace_token,
            &statements,
            &right_brace_token,
        );
        Ok(Block {
            statements,
            annotation,
        })
    }

    fn parse_statement_or_block(&mut self) -> ParseResult<BlockItem<Annotation>> {
        if self.peek().map(|t| &t.value) == Some(&Token::LBrace) {
            let block = self.parse_block()?;
            Ok(BlockItem::Block(block))
        } else {
            let stmt = self.parse_statement()?;
            Ok(BlockItem::Statement(stmt))
        }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement<Annotation>> {
        let token = self.peek().ok_or(ParsingError {
            src: NamedSource::new("input", String::new()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;
        match &token.value {
            Token::Let => self.parse_var_decl(),
            Token::Identifier(_) => {
                // Could be Assignment or expression
                // We need to look ahead 1 token
                match self.peek2().map(|t| &t.value) {
                    Some(Token::Equals) => self.parse_assignment(),
                    _ => {
                        let parsed_expr = self.parse_expression()?;
                        let semicolon_token = self.expect(Token::Semicolon)?;
                        let annotation = Annotation::construct_expression_statement_annotation(
                            self.annotation_processor,
                            &parsed_expr,
                            &semicolon_token,
                        );
                        Ok(Statement::Expression {
                            expr: parsed_expr,
                            annotation,
                        })
                    }
                }
            }
            _ => {
                let parsed_expr = self.parse_expression()?;
                let semicolon_token = self.expect(Token::Semicolon)?;
                let annotation = Annotation::construct_expression_statement_annotation(
                    self.annotation_processor,
                    &parsed_expr,
                    &semicolon_token,
                );
                Ok(Statement::Expression {
                    expr: parsed_expr,
                    annotation,
                })
            }
        }
    }

    fn parse_var_decl(&mut self) -> ParseResult<Statement<Annotation>> {
        // Grammar: "let" ID ":" Type "=" Expression ";"
        let let_token = self.advance().ok_or(ParsingError {
            src: Self::source_for_token(self.last().unwrap()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;

        let identifier_token = self.advance().ok_or(ParsingError {
            src: Self::source_for_token(self.last().unwrap()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;
        let name = match identifier_token.value {
            Token::Identifier(ref n) => n.clone(),
            _ => {
                return Err(ParsingError {
                    src: Self::source_for_token(&identifier_token),
                    span: identifier_token.loc.span,
                    kind: ParsingErrorKind::ExpectedIdentifier {
                        found: identifier_token.value.clone(),
                    },
                }
                .into());
            }
        };

        let colon_token = self.expect(Token::Colon)?;

        // Parse Type
        let type_ = self.parse_type()?;

        let equals_token = self.expect(Token::Equals)?;

        // Parse Expression
        let expr_ast = self.parse_expression()?;

        let semicolon_token = self.expect(Token::Semicolon)?;

        // make sure that a variable of this name does not already exist in the current scope
        for scope in self.variable_tracker.iter().rev() {
            if scope.contains_key(&name) {
                return Err(ParsingError {
                    src: Self::source_for_token(&identifier_token),
                    span: identifier_token.loc.span,
                    kind: ParsingErrorKind::VariableAlreadyDeclared { name: name.clone() },
                }
                .into());
            }
        }

        // assign variable index
        let var_index = self.variable_index;
        self.variable_index = VariableId(self.variable_index.0 + 1);
        self.variable_tracker
            .last_mut()
            .unwrap()
            .insert(name.clone(), var_index);

        self.variable_name_mapping.insert(var_index, name.clone());

        let annotation = Annotation::construct_var_decl_annotation(
            self.annotation_processor,
            &let_token,
            &identifier_token,
            &colon_token,
            &type_,
            &equals_token,
            &expr_ast,
            &semicolon_token,
            &var_index,
        );
        Ok(Statement::VarDecl {
            name,
            type_: type_,
            value: expr_ast,
            variable_index: var_index,
            annotation,
        })
    }

    fn parse_assignment(&mut self) -> ParseResult<Statement<Annotation>> {
        // Grammar: ID "=" Expression ";"
        let identifier_token = self.advance().ok_or(ParsingError {
            src: Self::source_for_token(self.last().unwrap()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;
        let name = match identifier_token.value {
            Token::Identifier(ref n) => n.clone(),
            _ => {
                return Err(ParsingError {
                    src: Self::source_for_token(&identifier_token),
                    span: identifier_token.loc.span,
                    kind: ParsingErrorKind::ExpectedIdentifier {
                        found: identifier_token.value.clone(),
                    },
                }
                .into());
            }
        };

        let equals_token = self.expect(Token::Equals)?;

        let expr_ast = self.parse_expression()?;

        let semicolon_token = self.expect(Token::Semicolon)?;

        // grab variable index
        let mut found_index = None;
        for scope in self.variable_tracker.iter().rev() {
            if let Some(idx) = scope.get(&name) {
                found_index = Some(*idx);
                break;
            }
        }
        let var_index = found_index.ok_or(ParsingError {
            src: Self::source_for_token(&identifier_token),
            span: identifier_token.loc.span,
            kind: ParsingErrorKind::VariableNotDeclared { name: name.clone() },
        })?;
        let annotation = Annotation::construct_assignment_annotation(
            self.annotation_processor,
            &identifier_token,
            &equals_token,
            &expr_ast,
            &semicolon_token,
        );
        Ok(Statement::Assignment {
            var: var_index,
            value: expr_ast,
            annotation,
        })
    }

    fn parse_function_call(&mut self) -> ParseResult<Expression<Annotation>> {
        // Grammar: ID "(" ")" or ID ("::" ID)* "(" Arguments? ")"
        let qualified_name = self.parse_qualified_identifier()?;
        let name = qualified_name.parts.join("::");

        let left_paren = self.expect(Token::LParen)?;

        let mut args = Vec::new();
        let mut comma_tokens = Vec::new();

        while self.peek().map(|t| &t.value) != Some(&Token::RParen) {
            let arg_expr = self.parse_expression()?;
            args.push(arg_expr);

            if self.peek().map(|t| &t.value) == Some(&Token::Comma) {
                let comma_token = self.advance().unwrap();
                comma_tokens.push(comma_token);
            } else {
                break; // no more arguments
            }
        }

        let right_paren = self.expect(Token::RParen)?;

        // function type checking will happen in a later phase

        let annotation = Annotation::construct_fn_call_annotation(
            self.annotation_processor,
            &qualified_name,
            &left_paren,
            &args,
            &comma_tokens,
            &right_paren,
        );
        Ok(Expression::FnCall {
            qualified_name,
            arguments: args,
            annotation,
        })
    }

    fn parse_qualified_identifier(&mut self) -> ParseResult<QualifiedName<Annotation>> {
        // Grammar: ID ("::" ID)*
        let mut parts = Vec::new();
        let mut identifier_tokens = Vec::new();
        let mut double_colon_tokens = Vec::new();

        let first_token = self.advance().ok_or(ParsingError {
            src: NamedSource::new("input", String::new()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;
        let first_name = match &first_token.value {
            Token::Identifier(n) => n.clone(),
            _ => {
                return Err(ParsingError {
                    src: Self::source_for_token(&first_token),
                    span: first_token.loc.span,
                    kind: ParsingErrorKind::ExpectedIdentifier {
                        found: first_token.value.clone(),
                    },
                }
                .into());
            }
        };
        parts.push(first_name);
        identifier_tokens.push(first_token);

        while self.peek().map(|t| &t.value) == Some(&Token::DoubleColon) {
            let double_colon_token = self.advance().unwrap();

            let next_token = self.advance().ok_or(ParsingError {
                src: Self::source_for_token(&double_colon_token),
                span: double_colon_token.loc.span,
                kind: ParsingErrorKind::UnexpectedEOF,
            })?;

            double_colon_tokens.push(double_colon_token);

            let next_name = match &next_token.value {
                Token::Identifier(n) => n.clone(),
                _ => {
                    return Err(ParsingError {
                        src: Self::source_for_token(&next_token),
                        span: next_token.loc.span,
                        kind: ParsingErrorKind::ExpectedIdentifier {
                            found: next_token.value.clone(),
                        },
                    }
                    .into());
                }
            };
            parts.push(next_name);
            identifier_tokens.push(next_token);
        }

        let name = parts.join("::");
        let annotation = Annotation::construct_qualified_identifier_annotation(
            self.annotation_processor,
            &identifier_tokens,
            &double_colon_tokens,
            &parts,
            &name,
        );

        Ok(QualifiedName { parts, annotation })
    }

    // Returns the AST node AND its resolved type
    fn parse_expression(&mut self) -> ParseResult<Expression<Annotation>> {
        let token = self.peek().ok_or(ParsingError {
            src: NamedSource::new("input", String::new()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;

        match token.value {
            Token::IntLiteral(_) => self.parse_int_literal(),
            Token::StringLiteral(_) => self.parse_string_literal(),
            Token::Identifier(_) => {
                // could be a several different things
                match self.peek2().map(|t| &t.value) {
                    Some(Token::LParen) | Some(Token::DoubleColon) => self.parse_function_call(),
                    Some(Token::LSquare) => self.parse_array_access(),
                    _ => {
                        let var = self.parse_variable_access()?;
                        let annotation =
                            Annotation::construct_variable_access_expression_annotation(
                                self.annotation_processor,
                                &var,
                            );
                        Ok(Expression::VariableAccess {
                            value: var,
                            annotation,
                        })
                    }
                }
            }
            _ => Err(ParsingError {
                src: Self::source_for_token(token),
                span: token.loc.span,
                kind: ParsingErrorKind::ExpectedExpression {
                    found: token.value.clone(),
                },
            }
            .into()),
        }
    }

    fn build_int_parse_error(
        &self,
        int_token: &Locatable<Token>,
        type_: &ASTTypeNode<Annotation>,
        parse_error: &std::num::ParseIntError,
    ) -> String {
        let int_str = match type_ {
            ASTTypeNode {
                kind: ASTTypeKind::U8,
                ..
            } => "u8",
            ASTTypeNode {
                kind: ASTTypeKind::I8,
                ..
            } => "i8",
            ASTTypeNode {
                kind: ASTTypeKind::U16,
                ..
            } => "u16",
            ASTTypeNode {
                kind: ASTTypeKind::I16,
                ..
            } => "i16",
            ASTTypeNode {
                kind: ASTTypeKind::U32,
                ..
            } => "u32",
            ASTTypeNode {
                kind: ASTTypeKind::I32,
                ..
            } => "i32",
            ASTTypeNode {
                kind: ASTTypeKind::U64,
                ..
            } => "u64",
            ASTTypeNode {
                kind: ASTTypeKind::I64,
                ..
            } => "i64",
            _ => "unknown",
        };

        return match parse_error.kind() {
            IntErrorKind::Empty => format!(
                "Cannot parse integer literal: empty string cannot be parsed as {}",
                int_str
            ),
            IntErrorKind::InvalidDigit => format!(
                "Cannot parse integer literal: invalid digit for type {}",
                int_str
            ),
            IntErrorKind::PosOverflow => format!(
                "Cannot parse integer literal: value too large for type {}",
                int_str
            ),
            IntErrorKind::NegOverflow => format!(
                "Cannot parse integer literal: value too small for type {}",
                int_str
            ),
            IntErrorKind::Zero => {
                unreachable!("There is no non-zero types parsed for integer literals")
            }
            _ => unreachable!("Unknown IntErrorKind"),
        };
    }

    fn parse_int_literal(&mut self) -> ParseResult<Expression<Annotation>> {
        let int_token = self.advance().ok_or(ParsingError {
            src: NamedSource::new("input", String::new()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;

        let int_value = match &int_token.value {
            Token::IntLiteral(s) => s.clone(),
            _ => {
                return Err(ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::ExpectedIntLiteral {
                        found: int_token.value.clone(),
                    },
                }
                .into());
            }
        };

        let type_token = self.parse_type()?;

        let parsed_value = match &type_token.kind {
            ASTTypeKind::U8 => IntLiteral::U8((&int_value).parse::<u8>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            ASTTypeKind::I8 => IntLiteral::I8((&int_value).parse::<i8>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            ASTTypeKind::U16 => IntLiteral::U16((&int_value).parse::<u16>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            ASTTypeKind::I16 => IntLiteral::I16((&int_value).parse::<i16>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            ASTTypeKind::I16 => IntLiteral::I16((&int_value).parse::<i16>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            ASTTypeKind::U32 => IntLiteral::U32((&int_value).parse::<u32>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            ASTTypeKind::I32 => IntLiteral::I32((&int_value).parse::<i32>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            ASTTypeKind::U64 => IntLiteral::U64((&int_value).parse::<u64>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            ASTTypeKind::I64 => IntLiteral::I64((&int_value).parse::<i64>().map_err(|e| {
                let error_message = self.build_int_parse_error(&int_token, &type_token, &e);
                ParsingError {
                    src: Self::source_for_token(&int_token),
                    span: int_token.loc.span,
                    kind: ParsingErrorKind::InvalidIntLiteral {
                        message: error_message,
                    },
                }
            })?),
            _ => Err(ParsingError {
                src: Self::source_for_token(&int_token),
                span: int_token.loc.span,
                kind: ParsingErrorKind::InvalidIntLiteralTypeMismatch {
                    literal: int_value.clone(),
                    type_name: format!("{:?}", type_token.kind),
                },
            })?,
        };

        let annotation = Annotation::construct_int_literal_annotation(
            self.annotation_processor,
            &int_token,
            &int_value,
            &type_token,
            &parsed_value,
        );
        Ok(Expression::IntLiteral {
            value: parsed_value,
            annotation,
        })
    }

    fn parse_string_literal(&mut self) -> ParseResult<Expression<Annotation>> {
        let str_token = self.advance().ok_or(ParsingError {
            src: Self::source_for_token(self.last().unwrap()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;

        let str_value = match &str_token.value {
            Token::StringLiteral(s) => s.clone(),
            _ => {
                return Err(ParsingError {
                    src: Self::source_for_token(&str_token),
                    span: str_token.loc.span,
                    kind: ParsingErrorKind::ExpectedStringLiteral {
                        found: str_token.value.clone(),
                    },
                }
                .into());
            }
        };

        let annotation = Annotation::construct_string_literal_annotation(
            self.annotation_processor,
            &str_token,
            &str_value,
        );
        Ok(Expression::StringLiteral {
            value: str_value,
            annotation,
        })
    }

    fn parse_array_access(&mut self) -> ParseResult<Expression<Annotation>> {
        // Grammar: ID "[" Expression "]"
        let variable_access = self.parse_variable_access()?;

        let left_bracket = self.expect(Token::LSquare)?;

        let index_expr = self.parse_expression()?;

        let right_bracket = self.expect(Token::RSquare)?;

        let annotation = Annotation::construct_array_access_annotation(
            self.annotation_processor,
            &variable_access,
            &left_bracket,
            &index_expr,
            &right_bracket,
        );

        Ok(Expression::ArrayAccess {
            index_expr: Box::new(index_expr),
            array: variable_access,
            annotation,
        })
    }

    fn parse_variable_access(&mut self) -> ParseResult<VariableAccess<Annotation>> {
        // Grammar: ID
        let name_token = self.advance().ok_or(ParsingError {
            src: Self::source_for_token(self.last().unwrap()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;

        let name = match &name_token.value {
            Token::Identifier(n) => n.clone(),
            _ => {
                return Err(ParsingError {
                    src: Self::source_for_token(&name_token),
                    span: name_token.loc.span,
                    kind: ParsingErrorKind::ExpectedIdentifier {
                        found: name_token.value.clone(),
                    },
                }
                .into());
            }
        };

        // look for the existing variable index
        let mut found_index = None;
        for scope in self.variable_tracker.iter().rev() {
            if let Some(idx) = scope.get(&name) {
                found_index = Some(*idx);
                break;
            }
        }

        let var_index = found_index.ok_or(ParsingError {
            src: Self::source_for_token(&name_token),
            span: name_token.loc.span,
            kind: ParsingErrorKind::VariableNotDeclared { name: name.clone() },
        })?;

        Ok(VariableAccess {
            id: var_index,
            annotation: Annotation::construct_variable_access_annotation(
                self.annotation_processor,
                &name_token,
                &name,
                &var_index,
            ),
        })
    }

    fn parse_type(&mut self) -> ParseResult<ASTTypeNode<Annotation>> {
        let t = self.advance().ok_or(ParsingError {
            src: Self::source_for_token(self.last().unwrap()),
            span: self.last().unwrap().loc.span,
            kind: ParsingErrorKind::UnexpectedEOF,
        })?;
        match t.value {
            Token::TypeU8 => {
                let kind = ASTTypeKind::U8;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                    ),
                })
            }
            Token::TypeI8 => {
                let kind = ASTTypeKind::I8;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                    ),
                })
            }
            Token::TypeU16 => {
                let kind = ASTTypeKind::U16;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                    ),
                })
            }
            Token::TypeI16 => {
                let kind = ASTTypeKind::I16;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                    ),
                })
            }
            Token::TypeU32 => {
                let kind = ASTTypeKind::U32;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                    ),
                })
            }
            Token::TypeI32 => {
                let kind = ASTTypeKind::I32;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                    ),
                })
            }
            Token::TypeU64 => {
                let kind = ASTTypeKind::U64;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                    ),
                })
            }
            Token::TypeI64 => {
                let kind = ASTTypeKind::I64;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                    ),
                })
            }
            Token::TypeStr => {
                // Format: str<INT>
                let left_angle = self.expect(Token::LAngle)?;
                let size_token = self.advance().ok_or(ParsingError {
                    src: Self::source_for_token(self.last().unwrap()),
                    span: self.last().unwrap().loc.span,
                    kind: ParsingErrorKind::UnexpectedEOF,
                })?;
                let size = match size_token.value {
                    Token::IntLiteral(ref n) => n.parse::<usize>().unwrap(),
                    _ => {
                        return Err(ParsingError {
                            src: Self::source_for_token(&size_token),
                            span: size_token.loc.span,
                            kind: ParsingErrorKind::ExpectedIntLiteral {
                                found: size_token.value.clone(),
                            },
                        }
                        .into());
                    }
                };
                let right_angle = self.expect(Token::RAngle)?;
                let kind = ASTTypeKind::Str(size);
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_str_type_node_annotation(
                        self.annotation_processor,
                        &t,
                        &kind,
                        &left_angle,
                        &size_token,
                        &right_angle,
                    ),
                })
            }
            _ => Err(ParsingError {
                src: Self::source_for_token(&t),
                span: t.loc.span,
                kind: ParsingErrorKind::ExpectedType {
                    found: t.value.clone(),
                },
            }
            .into()),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ASTTypeNode<Annotation: ASTAnnotation> {
    pub kind: ASTTypeKind,
    pub annotation: Annotation::ASTTypeNodeAnnotation,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Serialize)]
pub enum ASTTypeKind {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    Str(usize), // str<LEN>
}
