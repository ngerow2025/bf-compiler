use miette::{Diagnostic, NamedSource};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::tokenizer::{LocatableToken, SourceLocation, Token};

// --- AST Definitions ---

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub function_name_mapping: HashMap<FunctionId, String>,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub type_: ASTTypeNode,
    pub variable_index: VariableId,
    pub source: Option<SourceLocation>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub body: Block,
    pub id: FunctionId,
    pub source: Option<SourceLocation>,
    pub variable_name_mapping: HashMap<VariableId, String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub(crate) usize);

impl Display for FunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FUNCTION_ID({})", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Statement(Statement),
    Block(Block),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<BlockItem>,
    pub source: Option<SourceLocation>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    // let x: type = expr;
    VarDecl {
        name: String,
        type_: ASTTypeNode,
        value: Expression,
        variable_index: VariableId,
        source: Option<SourceLocation>,
    },
    // x = expr;
    Assignment {
        var: VariableId,
        value: Expression,
        source: Option<SourceLocation>,
    },

    // expr;
    Expression {
        expr: Expression,
        source: Option<SourceLocation>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub struct VariableAccess {
    pub id: VariableId,
    pub source: Option<SourceLocation>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntLiteral {
        value: IntLiteral,
        source: Option<SourceLocation>,
    },
    StringLiteral {
        value: String,
        source: Option<SourceLocation>,
    },
    Variable(VariableAccess),
    ArrayAccess {
        array: VariableAccess,
        index_expr: Box<Expression>,
        source: Option<SourceLocation>,
    },
    FnCall {
        name: String,
        arguments: Vec<Expression>,
        source: Option<SourceLocation>,
    },
}

impl Expression {
    pub fn get_source(&self) -> Option<SourceLocation> {
        match self {
            Expression::IntLiteral { source, .. } => source.clone(),
            Expression::StringLiteral { source, .. } => source.clone(),
            Expression::Variable(VariableAccess { source, .. }) => source.clone(),
            Expression::ArrayAccess { source, .. } => source.clone(),
            Expression::FnCall { source, .. } => source.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

#[derive(Debug)]
struct ExpectedTokenError {
    expected: Token,
    found: Option<LocatableToken>,
    source: NamedSource<String>,
}

impl Diagnostic for ExpectedTokenError {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(format!("ExpectedToken::{:?}", self.expected)))
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(format!("{}", self)))
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Error)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        if let Some(found) = &self.found {
            Some(Box::new(std::iter::once(
                miette::LabeledSpan::new_with_span(Some("here".to_string()), found.loc.span),
            )))
        } else {
            None
        }
    }
}

impl Display for ExpectedTokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.found {
            Some(found) => write!(f, "Expected token {:?}, found {:?}", self.expected, found),
            None => write!(f, "Expected token {:?}, found end of file", self.expected),
        }
    }
}

impl Error for ExpectedTokenError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }

    fn description(&self) -> &str {
        "Expected Token Error"
    }
}

struct QualifiedName {
    parts: Vec<String>,
    part_sources: Option<Vec<SourceLocation>>,
    source: Option<SourceLocation>,
}

pub struct Parser {
    tokens: Vec<LocatableToken>,
    pos: usize,
    variable_index: VariableId,
    variable_tracker: Vec<HashMap<String, VariableId>>,
    function_name_mapping: HashMap<FunctionId, String>,
    variable_name_mapping: HashMap<VariableId, String>,
}

impl Parser {
    pub fn new(tokens: Vec<LocatableToken>) -> Self {
        Parser {
            tokens,
            pos: 0,
            variable_index: VariableId(0),
            variable_tracker: vec![],
            function_name_mapping: HashMap::new(),
            variable_name_mapping: HashMap::new(),
        }
    }

    // --- Helpers ---

    fn peek(&self) -> Option<&LocatableToken> {
        self.tokens.get(self.pos)
    }

    fn peek2(&self) -> Option<&LocatableToken> {
        self.tokens.get(self.pos + 1)
    }

    fn advance(&mut self) -> Option<LocatableToken> {
        let t = self.tokens.get(self.pos).cloned();
        self.pos += 1;
        t
    }

    fn expect(&mut self, expected: Token) -> Result<LocatableToken, String> {
        if self.peek().map(|lt| &lt.token) == Some(&expected) {
            Ok(self.advance().unwrap())
        } else {
            panic!("expected {:?}, got {:?}", expected, self.peek());
            Err(format!("Expected {:?}, found {:?}", expected, self.peek()))
        }
    }

    // --- Parsing Logic ---

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut functions = Vec::new();
        let mut current_function_id = FunctionId(0);
        while self.peek().is_some() {
            functions.push(self.parse_function(current_function_id)?);
            current_function_id = FunctionId(current_function_id.0 + 1);
        }
        Ok(Program {
            functions,
            function_name_mapping: self.function_name_mapping.clone(),
        })
    }

    fn parse_function(&mut self, id: FunctionId) -> Result<Function, String> {
        self.variable_index = VariableId(0);
        self.variable_tracker.clear();
        self.variable_tracker.push(HashMap::new());
        self.variable_name_mapping.clear();
        // Grammar: "fn" ID "(" ")" Block
        self.expect(Token::Fn)?;

        let name_token = self.advance().expect("should have a name token");

        let name = match name_token.token {
            Token::Identifier(n) => n.clone(),
            t => return Err(format!("Expected function name, found {:?}", t)),
        };

        self.function_name_mapping.insert(id, name.clone());

        self.expect(Token::LParen)?;

        let mut params = Vec::new();
        //parse parameters
        //syntax: fn function_name(param: Type)
        while self.peek().map(|t| &t.token) != Some(&Token::RParen) {
            let name_token = self.advance().expect("should have a name token");

            let param_name = match name_token.token {
                Token::Identifier(n) => n.clone(),
                t => return Err(format!("Expected parameter name, found {:?}", t)),
            };

            self.expect(Token::Colon)?;

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

            params.push(FunctionParam {
                variable_index: var_index,
                source: Some(SourceLocation::superset(&[
                    &name_token.loc,
                    param_type.source.as_ref().unwrap(),
                ])),
                type_: param_type,
            });

            if self.peek().map(|t| &t.token) == Some(&Token::Comma) {
                self.advance(); // consume comma
            } else {
                break; // no more parameters
            }
        }

        self.expect(Token::RParen)?;

        let body = self.parse_block()?;

        Ok(Function {
            name,
            params,
            id,
            variable_name_mapping: self.variable_name_mapping.clone(),
            source: Some(SourceLocation::superset(&[
                &name_token.loc,
                body.source.as_ref().unwrap(),
            ])),
            body,
        })
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        self.variable_tracker.push(HashMap::new());
        let start_token = self.expect(Token::LBrace)?;
        let mut statements = Vec::new();

        while self.peek().is_some() && self.peek().map(|t| &t.token) != Some(&Token::RBrace) {
            statements.push(self.parse_statement_or_block()?);
        }

        let end_token = self.expect(Token::RBrace)?;
        self.variable_tracker.pop();
        Ok(Block {
            statements,
            source: Some(SourceLocation::superset(&[
                &start_token.loc,
                &end_token.loc,
            ])),
        })
    }

    fn parse_statement_or_block(&mut self) -> Result<BlockItem, String> {
        if self.peek().map(|t| &t.token) == Some(&Token::LBrace) {
            let block = self.parse_block()?;
            Ok(BlockItem::Block(block))
        } else {
            let stmt = self.parse_statement()?;
            Ok(BlockItem::Statement(stmt))
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        let token = self.peek().ok_or("Unexpected EOF")?;
        match &token.token {
            Token::Let => self.parse_var_decl(),
            Token::Identifier(_) => {
                // Could be Assignment or expression
                // We need to look ahead 1 token
                match self.peek2().map(|t| &t.token) {
                    Some(Token::Equals) => self.parse_assignment(),
                    _ => {
                        let parsed_expr = self.parse_expression()?;
                        let semicolon_token = self.expect(Token::Semicolon)?;
                        Ok(Statement::Expression {
                            source: Some(SourceLocation::superset(&[
                                &parsed_expr.get_source().unwrap(),
                                &semicolon_token.loc,
                            ])),
                            expr: parsed_expr,
                        })
                    }
                }
            }
            _ => {
                let parsed_expr = self.parse_expression()?;
                let semicolon_token = self.expect(Token::Semicolon)?;
                Ok(Statement::Expression {
                    source: Some(SourceLocation::superset(&[
                        &parsed_expr.get_source().unwrap(),
                        &semicolon_token.loc,
                    ])),
                    expr: parsed_expr,
                })
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<Statement, String> {
        // Grammar: "let" ID ":" Type "=" Expression ";"
        let let_token = self.advance().expect("need the let token"); // consume 'let'

        let name = match self.advance().map(|t| t.token) {
            Some(Token::Identifier(n)) => n.clone(),
            t => return Err(format!("Expected variable name, found {:?}", t)),
        };

        self.expect(Token::Colon)?;

        // Parse Type
        let type_ = self.parse_type()?;

        self.expect(Token::Equals)?;

        // Parse Expression
        let expr_ast = self.parse_expression()?;

        let semicolon_token = self.expect(Token::Semicolon)?;

        // make sure that a variable of this name does not already exist in the current scope
        for scope in self.variable_tracker.iter().rev() {
            if scope.contains_key(&name) {
                return Err(format!(
                    "Variable '{}' already declared in this scope",
                    name
                ));
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

        Ok(Statement::VarDecl {
            name,
            type_,
            value: expr_ast,
            variable_index: var_index,
            source: Some(SourceLocation::superset(&[
                &let_token.loc,
                &semicolon_token.loc,
            ])),
        })
    }

    fn parse_assignment(&mut self) -> Result<Statement, String> {
        // Grammar: ID "=" Expression ";"
        let name_token = self.peek().cloned().ok_or("Unexpected EOF")?;

        let name = match self.advance().map(|t| t.token) {
            Some(Token::Identifier(n)) => n.clone(),
            _ => return Err("Expected identifier".into()),
        };

        self.expect(Token::Equals)?;

        let expr_ast = self.parse_expression()?;

        self.expect(Token::Semicolon)?;

        // grab variable index
        let mut found_index = None;
        for scope in self.variable_tracker.iter().rev() {
            if let Some(idx) = scope.get(&name) {
                found_index = Some(*idx);
                break;
            }
        }
        let var_index = found_index.ok_or(format!(
            "Variable '{}' not declared before assignment",
            name
        ))?;
        Ok(Statement::Assignment {
            var: var_index,
            source: Some(SourceLocation::superset(&[
                &name_token.loc,
                &expr_ast.get_source().unwrap(),
            ])),
            value: expr_ast,
        })
    }

    fn parse_function_call(&mut self) -> Result<Expression, String> {
        // Grammar: ID "(" ")"
        let name_token = self.parse_qualified_identifier()?;
        let name = name_token.parts.join("::");

        self.expect(Token::LParen)?;

        let mut args = Vec::new();
        while self.peek().map(|t| &t.token) != Some(&Token::RParen) {
            let arg_expr = self.parse_expression()?;
            args.push(arg_expr);

            if self.peek().map(|t| &t.token) == Some(&Token::Comma) {
                self.advance(); // consume comma
            } else {
                break; // no more arguments
            }
        }

        let r_paren_token = self.expect(Token::RParen)?;

        // function type checking will happen in a later phase

        Ok(Expression::FnCall {
            name,
            arguments: args,
            source: Some(SourceLocation::superset(&[
                &name_token.source.unwrap(),
                &r_paren_token.loc,
            ])),
        })
    }

    fn parse_qualified_identifier(&mut self) -> Result<QualifiedName, String> {
        // Grammar: ID ("::" ID)*
        let mut parts = Vec::new();
        let mut source_locations = Vec::new();

        let first_token = self.advance().ok_or("Expected identifier")?;
        let first_name = match first_token.token {
            Token::Identifier(n) => n.clone(),
            _ => return Err("Expected identifier".into()),
        };
        parts.push(first_name);
        source_locations.push(first_token.loc);
        while self.peek().map(|t| &t.token) == Some(&Token::DoubleColon) {
            self.advance(); // consume '::'

            let next_token = self.advance().ok_or("Expected identifier after '::'")?;
            let next_name = match next_token.token {
                Token::Identifier(n) => n.clone(),
                _ => return Err("Expected identifier after '::'".into()),
            };
            parts.push(next_name);
            source_locations.push(next_token.loc);
        }

        Ok(QualifiedName {
            parts,
            source: Some(SourceLocation::superset(&[
                source_locations.first().unwrap(),
                source_locations.last().unwrap(),
            ])),
            part_sources: Some(source_locations),
        })
    }

    // Returns the AST node AND its resolved type
    fn parse_expression(&mut self) -> Result<Expression, String> {
        let token = self.peek().ok_or("Unexpected EOF in expression")?;

        match token.token {
            Token::IntLiteral(_) => self.parse_int_literal(),
            Token::StringLiteral(_) => self.parse_string_literal(),
            Token::Identifier(_) => {
                // could be a several different things
                match self.peek2().map(|t| &t.token) {
                    Some(Token::LParen) | Some(Token::DoubleColon) => self.parse_function_call(),
                    Some(Token::LSquare) => self.parse_array_access(),
                    _ => self
                        .parse_variable_access()
                        .map(|var| Expression::Variable(var)),
                }
            }
            _ => Err(format!("Invalid token in expression: {:?}", token)),
        }
    }

    fn parse_int_literal(&mut self) -> Result<Expression, String> {
        let int_token = self.advance().ok_or("Expected integer literal")?;

        let int_value = match int_token.token {
            Token::IntLiteral(s) => s.clone(),
            _ => return Err("Expected integer literal".into()),
        };

        let type_token = self.parse_type()?;

        let parsed_value = match type_token.kind {
            ASTTypeKind::U8 => IntLiteral::U8(int_value.parse::<u8>().map_err(|e| {
                format!(
                    "Failed to parse integer literal '{}' as u8: {}",
                    int_value, e
                )
            })?),
            ASTTypeKind::I8 => IntLiteral::I8(int_value.parse::<i8>().map_err(|e| {
                format!(
                    "Failed to parse integer literal '{}' as i8: {}",
                    int_value, e
                )
            })?),
            ASTTypeKind::U16 => IntLiteral::U16(int_value.parse::<u16>().map_err(|e| {
                format!(
                    "Failed to parse integer literal '{}' as u16: {}",
                    int_value, e
                )
            })?),
            ASTTypeKind::I16 => IntLiteral::I16(int_value.parse::<i16>().map_err(|e| {
                format!(
                    "Failed to parse integer literal '{}' as i16: {}",
                    int_value, e
                )
            })?),
            ASTTypeKind::U32 => IntLiteral::U32(int_value.parse::<u32>().map_err(|e| {
                format!(
                    "Failed to parse integer literal '{}' as u32: {}",
                    int_value, e
                )
            })?),
            ASTTypeKind::I32 => IntLiteral::I32(int_value.parse::<i32>().map_err(|e| {
                format!(
                    "Failed to parse integer literal '{}' as i32: {}",
                    int_value, e
                )
            })?),
            ASTTypeKind::U64 => IntLiteral::U64(int_value.parse::<u64>().map_err(|e| {
                format!(
                    "Failed to parse integer literal '{}' as u64: {}",
                    int_value, e
                )
            })?),
            ASTTypeKind::I64 => IntLiteral::I64(int_value.parse::<i64>().map_err(|e| {
                format!(
                    "Failed to parse integer literal '{}' as i64: {}",
                    int_value, e
                )
            })?),
            _ => {
                return Err(format!(
                    "Invalid type for integer literal: {:?}",
                    type_token.kind
                ));
            }
        };

        Ok(Expression::IntLiteral {
            value: parsed_value,
            source: Some(SourceLocation::superset(&[
                &int_token.loc,
                type_token.source.as_ref().unwrap(),
            ])),
        })
    }

    fn parse_string_literal(&mut self) -> Result<Expression, String> {
        let str_token = self.advance().ok_or("Expected string literal")?;

        let str_value = match str_token.token {
            Token::StringLiteral(s) => s.clone(),
            _ => return Err("Expected string literal".into()),
        };

        Ok(Expression::StringLiteral {
            value: str_value,
            source: Some(str_token.loc),
        })
    }

    fn parse_array_access(&mut self) -> Result<Expression, String> {
        // Grammar: ID "[" Expression "]"
        let variable_access = self.parse_variable_access()?;

        self.expect(Token::LSquare)?;

        let index_expr = self.parse_expression()?;

        let r_square_token = self.expect(Token::RSquare)?;

        Ok(Expression::ArrayAccess {
            index_expr: Box::new(index_expr),
            source: Some(SourceLocation::superset(&[
                &variable_access.source.clone().unwrap(),
                &r_square_token.loc,
            ])),
            array: variable_access,
        })
    }

    fn parse_variable_access(&mut self) -> Result<VariableAccess, String> {
        // Grammar: ID
        let name_token = self.advance().ok_or("Expected identifier for variable")?;

        let name = match name_token.token {
            Token::Identifier(n) => n.clone(),
            _ => return Err("Expected identifier for variable".into()),
        };

        // look for the existing variable index
        let mut found_index = None;
        for scope in self.variable_tracker.iter().rev() {
            if let Some(idx) = scope.get(&name) {
                found_index = Some(*idx);
                break;
            }
        }

        let var_index = found_index.ok_or(format!("Variable '{}' not declared", name))?;

        Ok(VariableAccess {
            id: var_index,
            source: Some(name_token.loc),
        })
    }

    //TODO: use parse_variable everywhere and add Varible struct for tracking source info like for the ArrayAccess node

    fn parse_type(&mut self) -> Result<ASTTypeNode, String> {
        let t = self.advance().ok_or("Expected type")?;
        match t.token {
            Token::TypeU8 => Ok(ASTTypeNode {
                kind: ASTTypeKind::U8,
                source: Some(t.loc),
            }),
            Token::TypeI8 => Ok(ASTTypeNode {
                kind: ASTTypeKind::I8,
                source: Some(t.loc),
            }),
            Token::TypeU16 => Ok(ASTTypeNode {
                kind: ASTTypeKind::U16,
                source: Some(t.loc),
            }),
            Token::TypeI16 => Ok(ASTTypeNode {
                kind: ASTTypeKind::I16,
                source: Some(t.loc),
            }),
            Token::TypeU32 => Ok(ASTTypeNode {
                kind: ASTTypeKind::U32,
                source: Some(t.loc),
            }),
            Token::TypeI32 => Ok(ASTTypeNode {
                kind: ASTTypeKind::I32,
                source: Some(t.loc),
            }),
            Token::TypeU64 => Ok(ASTTypeNode {
                kind: ASTTypeKind::U64,
                source: Some(t.loc),
            }),
            Token::TypeI64 => Ok(ASTTypeNode {
                kind: ASTTypeKind::I64,
                source: Some(t.loc),
            }),
            Token::TypeStr => {
                // Format: str<INT>
                self.expect(Token::LAngle)?;
                let size = match self.advance().map(|t| t.token) {
                    Some(Token::IntLiteral(n)) => n.parse::<usize>().unwrap(),
                    _ => return Err("Expected integer literal for string size".into()),
                };
                let r_angle_token = self.expect(Token::RAngle)?;
                Ok(ASTTypeNode {
                    kind: ASTTypeKind::Str(size),
                    source: Some(SourceLocation::superset(&[&t.loc, &r_angle_token.loc])),
                })
            }
            _ => Err(format!("Unknown type token: {:?}", t)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTTypeNode {
    pub kind: ASTTypeKind,
    pub source: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
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
