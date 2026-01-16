use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

use crate::tokenizer::{Locatable, Token};

// --- AST Definitions ---

pub trait ASTAnnotation: Sized + Clone + Debug {
    type ProgramAnnotation: Debug + Clone;
    fn construct_program_annotation(
        functions: &[Function<Self>],
        function_name_mapping: &HashMap<FunctionId, String>,
    ) -> Self::ProgramAnnotation;
    type FunctionParamAnnotation: Debug + Clone;
    fn construct_function_param_annotation(
        identifier_token: &Locatable<Token>,
        colon_token: &Locatable<Token>,
        type_node: &ASTTypeNode<Self>,
        variable_index: &VariableId,
    ) -> Self::FunctionParamAnnotation;
    type FunctionAnnotation: Debug + Clone;
    fn construct_function_annotation(
        fn_token: &Locatable<Token>,
        name_token: &Locatable<Token>,
        left_paren_token: &Locatable<Token>,
        params: &[FunctionParam<Self>],
        comma_tokens: &[Locatable<Token>],
        right_paren_token: &Locatable<Token>,
        body: &Block<Self>,
        function_id: &FunctionId,
    ) -> Self::FunctionAnnotation;
    type BlockAnnotation: Debug + Clone;
    fn construct_block_annotation(
        left_brace_token: &Locatable<Token>,
        statements: &[BlockItem<Self>],
        right_brace_token: &Locatable<Token>,
    ) -> Self::BlockAnnotation;
    type StatementAnnotation: Debug + Clone;
    fn construct_expression_statement_annotation(
        expr: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation;
    fn construct_assignment_annotation(
        identifier_token: &Locatable<Token>,
        equals_token: &Locatable<Token>,
        value: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation;
    fn construct_var_decl_annotation(
        let_token: &Locatable<Token>,
        identifier_token: &Locatable<Token>,
        colon_token: &Locatable<Token>,
        type_node: &ASTTypeNode<Self>,
        equals_token: &Locatable<Token>,
        value: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
        variable_index: &VariableId,
    ) -> Self::StatementAnnotation;
    type ExpressionAnnotation: Debug + Clone;
    fn construct_string_literal_annotation(
        string_token: &Locatable<Token>,
        value: &str,
    ) -> Self::ExpressionAnnotation;
    fn construct_int_literal_annotation(
        int_token: &Locatable<Token>,
        int_value: &str,
        type_node: &ASTTypeNode<Self>,
        parsed_value: &IntLiteral,
    ) -> Self::ExpressionAnnotation;
    fn construct_array_access_annotation(
        array: &VariableAccess<Self>,
        left_bracket_token: &Locatable<Token>,
        index_expr: &Expression<Self>,
        right_bracket_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation;
    fn construct_fn_call_annotation(
        qualified_name: &QualifiedName<Self>,
        left_paren_token: &Locatable<Token>,
        arguments: &[Expression<Self>],
        comma_tokens: &[Locatable<Token>],
        right_paren_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation;
    type VariableAccessAnnotation: Debug + Clone;
    fn construct_variable_access_annotation(
        name_token: &Locatable<Token>,
        name: &str,
        variable_id: &VariableId,
    ) -> Self::VariableAccessAnnotation;
    type QualifiedIdentifierAnnotation: Debug + Clone;
    fn construct_qualified_identifier_annotation(
        identifier_tokens: &[Locatable<Token>],
        double_colon_tokens: &[Locatable<Token>],
        parts: &[String],
        name: &str,
    ) -> Self::QualifiedIdentifierAnnotation;
    type ASTTypeNodeAnnotation: Debug + Clone;
    fn construct_simple_type_node_annotation(
        type_token: &Locatable<Token>,
        kind: &ASTTypeKind,
    ) -> Self::ASTTypeNodeAnnotation;
    fn construct_str_type_node_annotation(
        type_token: &Locatable<Token>,
        kind: &ASTTypeKind,
        left_angle_token: &Locatable<Token>,
        size_token: &Locatable<Token>,
        right_angle_token: &Locatable<Token>,
    ) -> Self::ASTTypeNodeAnnotation;
}

impl ASTAnnotation for () {
    type ProgramAnnotation = ();

    fn construct_program_annotation(
        _functions: &[Function<Self>],
        _function_name_mapping: &HashMap<FunctionId, String>,
    ) -> Self::ProgramAnnotation {
        ()
    }

    type FunctionParamAnnotation = ();

    fn construct_function_param_annotation(
        _identifier_token: &Locatable<Token>,
        _colon_token: &Locatable<Token>,
        _type_node: &ASTTypeNode<Self>,
        _variable_index: &VariableId,
    ) -> Self::FunctionParamAnnotation {
        ()
    }

    type FunctionAnnotation = ();

    fn construct_function_annotation(
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
        _left_brace_token: &Locatable<Token>,
        _statements: &[BlockItem<Self>],
        _right_brace_token: &Locatable<Token>,
    ) -> Self::BlockAnnotation {
        ()
    }

    type StatementAnnotation = ();

    fn construct_expression_statement_annotation(
        _expr: &Expression<Self>,
        _semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation {
        ()
    }

    fn construct_assignment_annotation(
        _identifier_token: &Locatable<Token>,
        _equals_token: &Locatable<Token>,
        _value: &Expression<Self>,
        _semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation {
        ()
    }

    fn construct_var_decl_annotation(
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
        _string_token: &Locatable<Token>,
        _value: &str,
    ) -> Self::ExpressionAnnotation {
        ()
    }

    fn construct_int_literal_annotation(
        _int_token: &Locatable<Token>,
        _int_value: &str,
        _type_node: &ASTTypeNode<Self>,
        _parsed_value: &IntLiteral,
    ) -> Self::ExpressionAnnotation {
        ()
    }

    fn construct_array_access_annotation(
        _array: &VariableAccess<Self>,
        _left_bracket_token: &Locatable<Token>,
        _index_expr: &Expression<Self>,
        _right_bracket_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation {
        ()
    }

    fn construct_fn_call_annotation(
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
        _name_token: &Locatable<Token>,
        _name: &str,
        _variable_id: &VariableId,
    ) -> Self::VariableAccessAnnotation {
        ()
    }

    type QualifiedIdentifierAnnotation = ();

    fn construct_qualified_identifier_annotation(
        _identifier_tokens: &[Locatable<Token>],
        _double_colon_tokens: &[Locatable<Token>],
        _parts: &[String],
        _name: &str,
    ) -> Self::QualifiedIdentifierAnnotation {
        ()
    }

    type ASTTypeNodeAnnotation = ();

    fn construct_simple_type_node_annotation(
        _type_token: &Locatable<Token>,
        _kind: &ASTTypeKind,
    ) -> Self::ASTTypeNodeAnnotation {
        ()
    }

    fn construct_str_type_node_annotation(
        _type_token: &Locatable<Token>,
        _kind: &ASTTypeKind,
        _left_angle_token: &Locatable<Token>,
        _size_token: &Locatable<Token>,
        _right_angle_token: &Locatable<Token>,
    ) -> Self::ASTTypeNodeAnnotation {
        ()
    }
}

#[derive(Debug, Clone)]
pub struct Program<Annotation: ASTAnnotation> {
    pub functions: Vec<Function<Annotation>>,
    pub function_name_mapping: HashMap<FunctionId, String>,
    pub annotation: Annotation::ProgramAnnotation,
}

#[derive(Debug, Clone)]
pub struct FunctionParam<Annotation: ASTAnnotation> {
    pub type_: ASTTypeNode<Annotation>,
    pub variable_index: VariableId,
    pub annotation: Annotation::FunctionParamAnnotation,
}

#[derive(Debug, Clone)]
pub struct Function<Annotation: ASTAnnotation> {
    pub name: String,
    pub params: Vec<FunctionParam<Annotation>>,
    pub body: Block<Annotation>,
    pub id: FunctionId,
    pub variable_name_mapping: HashMap<VariableId, String>,
    pub annotation: Annotation::FunctionAnnotation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub(crate) usize);

impl Display for FunctionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FUNCTION_ID({})", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum BlockItem<Annotation: ASTAnnotation> {
    Statement(Statement<Annotation>),
    Block(Block<Annotation>),
}

#[derive(Debug, Clone)]
pub struct Block<Annotation: ASTAnnotation> {
    pub statements: Vec<BlockItem<Annotation>>,
    pub annotation: Annotation::BlockAnnotation,
}

#[derive(Debug, Clone)]
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
pub struct VariableAccess<Annotation: ASTAnnotation> {
    pub id: VariableId,
    pub annotation: Annotation::VariableAccessAnnotation,
}

#[derive(Debug, Clone)]
pub enum Expression<Annotation: ASTAnnotation> {
    IntLiteral {
        value: IntLiteral,
        annotation: Annotation::ExpressionAnnotation,
    },
    StringLiteral {
        value: String,
        annotation: Annotation::ExpressionAnnotation,
    },
    Variable(VariableAccess<Annotation>),
    ArrayAccess {
        array: VariableAccess<Annotation>,
        index_expr: Box<Expression<Annotation>>,
        annotation: Annotation::ExpressionAnnotation,
    },
    FnCall {
        name: String,
        arguments: Vec<Expression<Annotation>>,
        annotation: Annotation::ExpressionAnnotation,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

pub struct QualifiedName<Annotation: ASTAnnotation> {
    parts: Vec<String>,
    annotation: Annotation::QualifiedIdentifierAnnotation,
}

pub struct Parser {
    tokens: Vec<Locatable<Token>>,
    pos: usize,
    variable_index: VariableId,
    variable_tracker: Vec<HashMap<String, VariableId>>,
    function_name_mapping: HashMap<FunctionId, String>,
    variable_name_mapping: HashMap<VariableId, String>,
}

impl Parser {
    pub fn new(tokens: Vec<Locatable<Token>>) -> Self {
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

    fn peek(&self) -> Option<&Locatable<Token>> {
        self.tokens.get(self.pos)
    }

    fn peek2(&self) -> Option<&Locatable<Token>> {
        self.tokens.get(self.pos + 1)
    }

    fn advance(&mut self) -> Option<Locatable<Token>> {
        let t = self.tokens.get(self.pos).cloned();
        self.pos += 1;
        t
    }

    fn expect(&mut self, expected: Token) -> Result<Locatable<Token>, String> {
        if self.peek().map(|lt| &lt.value) == Some(&expected) {
            Ok(self.advance().unwrap())
        } else {
            Err(format!("Expected {:?}, found {:?}", expected, self.peek()))
        }
    }

    // --- Parsing Logic ---

    pub fn parse_program<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Program<Annotation>, String> {
        let mut functions = Vec::new();
        let mut current_function_id = FunctionId(0);
        while self.peek().is_some() {
            functions.push(self.parse_function::<Annotation>(current_function_id)?);
            current_function_id = FunctionId(current_function_id.0 + 1);
        }
        let annotation =
            Annotation::construct_program_annotation(&functions, &self.function_name_mapping);
        Ok(Program {
            functions,
            function_name_mapping: std::mem::take(&mut self.function_name_mapping),
            annotation,
        })
    }

    fn parse_function<Annotation: ASTAnnotation>(
        &mut self,
        id: FunctionId,
    ) -> Result<Function<Annotation>, String> {
        self.variable_index = VariableId(0);
        self.variable_tracker.clear();
        self.variable_tracker.push(HashMap::new());
        self.variable_name_mapping.clear();
        // Grammar: "fn" ID "(" ")" Block
        let fn_token = self.expect(Token::Fn)?;

        let name_token = self.advance().expect("should have a name token");

        let name = match name_token.value {
            Token::Identifier(ref n) => n.clone(),
            t => return Err(format!("Expected function name, found {:?}", t)),
        };

        self.function_name_mapping.insert(id, name.clone());

        let left_paren_token = self.expect(Token::LParen)?;

        let mut params = Vec::new();
        let mut param_comma_tokens = Vec::new();
        //parse parameters
        //syntax: fn function_name(param: Type)
        while self.peek().map(|t| &t.value) != Some(&Token::RParen) {
            let param_identifier_token = self.advance().expect("should have a name token");

            let param_name = match param_identifier_token.value {
                Token::Identifier(ref n) => n.clone(),
                t => return Err(format!("Expected parameter name, found {:?}", t)),
            };

            let param_colon_token = self.expect(Token::Colon)?;

            let param_type = self.parse_type::<Annotation>()?;

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

        let body = self.parse_block::<Annotation>()?;

        let annotation = Annotation::construct_function_annotation(
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

    fn parse_block<Annotation: ASTAnnotation>(&mut self) -> Result<Block<Annotation>, String> {
        self.variable_tracker.push(HashMap::new());
        let left_brace_token = self.expect(Token::LBrace)?;
        let mut statements = Vec::new();

        while self.peek().is_some() && self.peek().map(|t| &t.value) != Some(&Token::RBrace) {
            statements.push(self.parse_statement_or_block::<Annotation>()?);
        }

        let right_brace_token = self.expect(Token::RBrace)?;
        self.variable_tracker.pop();
        let annotation = Annotation::construct_block_annotation(
            &left_brace_token,
            &statements,
            &right_brace_token,
        );
        Ok(Block {
            statements,
            annotation,
        })
    }

    fn parse_statement_or_block<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<BlockItem<Annotation>, String> {
        if self.peek().map(|t| &t.value) == Some(&Token::LBrace) {
            let block = self.parse_block()?;
            Ok(BlockItem::Block(block))
        } else {
            let stmt = self.parse_statement()?;
            Ok(BlockItem::Statement(stmt))
        }
    }

    fn parse_statement<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Statement<Annotation>, String> {
        let token = self.peek().ok_or("Unexpected EOF")?;
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

    fn parse_var_decl<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Statement<Annotation>, String> {
        // Grammar: "let" ID ":" Type "=" Expression ";"
        let let_token = self.advance().ok_or("Expected 'let'")?;

        let identifier_token = self.advance().ok_or("Expected identifier")?;
        let name = match identifier_token.value {
            Token::Identifier(ref n) => n.clone(),
            _ => return Err(format!("Expected variable name")),
        };

        let colon_token = self.expect(Token::Colon)?;

        // Parse Type
        let type_ = self.parse_type::<Annotation>()?;

        let equals_token = self.expect(Token::Equals)?;

        // Parse Expression
        let expr_ast = self.parse_expression::<Annotation>()?;

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

        let annotation = Annotation::construct_var_decl_annotation(
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

    fn parse_assignment<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Statement<Annotation>, String> {
        // Grammar: ID "=" Expression ";"
        let identifier_token = self.advance().ok_or("Expected identifier")?;
        let name = match identifier_token.value {
            Token::Identifier(ref n) => n.clone(),
            _ => return Err("Expected identifier".into()),
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
        let var_index = found_index.ok_or(format!(
            "Variable '{}' not declared before assignment",
            name
        ))?;
        let annotation = Annotation::construct_assignment_annotation(
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

    fn parse_function_call<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Expression<Annotation>, String> {
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
            &qualified_name,
            &left_paren,
            &args,
            &comma_tokens,
            &right_paren,
        );
        Ok(Expression::FnCall {
            name,
            arguments: args,
            annotation,
        })
    }

    fn parse_qualified_identifier<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<QualifiedName<Annotation>, String> {
        // Grammar: ID ("::" ID)*
        let mut parts = Vec::new();
        let mut identifier_tokens = Vec::new();
        let mut double_colon_tokens = Vec::new();

        let first_token = self.advance().ok_or("Expected identifier")?;
        let first_name = match &first_token.value {
            Token::Identifier(n) => n.clone(),
            _ => return Err("Expected identifier".into()),
        };
        parts.push(first_name);
        identifier_tokens.push(first_token);

        while self.peek().map(|t| &t.value) == Some(&Token::DoubleColon) {
            let double_colon_token = self.advance().unwrap();
            double_colon_tokens.push(double_colon_token);

            let next_token = self.advance().ok_or("Expected identifier after '::'")?;
            let next_name = match &next_token.value {
                Token::Identifier(n) => n.clone(),
                _ => return Err("Expected identifier after '::'".into()),
            };
            parts.push(next_name);
            identifier_tokens.push(next_token);
        }

        let name = parts.join("::");
        let annotation = Annotation::construct_qualified_identifier_annotation(
            &identifier_tokens,
            &double_colon_tokens,
            &parts,
            &name,
        );

        Ok(QualifiedName { parts, annotation })
    }

    // Returns the AST node AND its resolved type
    fn parse_expression<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Expression<Annotation>, String> {
        let token = self.peek().ok_or("Unexpected EOF in expression")?;

        match token.value {
            Token::IntLiteral(_) => self.parse_int_literal(),
            Token::StringLiteral(_) => self.parse_string_literal(),
            Token::Identifier(_) => {
                // could be a several different things
                match self.peek2().map(|t| &t.value) {
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

    fn parse_int_literal<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Expression<Annotation>, String> {
        let int_token = self.advance().ok_or("Expected integer literal")?;

        let int_value = match &int_token.value {
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

        let annotation = Annotation::construct_int_literal_annotation(
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

    fn parse_string_literal<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Expression<Annotation>, String> {
        let str_token = self.advance().ok_or("Expected string literal")?;

        let str_value = match &str_token.value {
            Token::StringLiteral(s) => s.clone(),
            _ => return Err("Expected string literal".into()),
        };

        let annotation = Annotation::construct_string_literal_annotation(&str_token, &str_value);
        Ok(Expression::StringLiteral {
            value: str_value,
            annotation,
        })
    }

    fn parse_array_access<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<Expression<Annotation>, String> {
        // Grammar: ID "[" Expression "]"
        let variable_access = self.parse_variable_access()?;

        let left_bracket = self.expect(Token::LSquare)?;

        let index_expr = self.parse_expression()?;

        let right_bracket = self.expect(Token::RSquare)?;

        let annotation = Annotation::construct_array_access_annotation(
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

    fn parse_variable_access<Annotation: ASTAnnotation>(
        &mut self,
    ) -> Result<VariableAccess<Annotation>, String> {
        // Grammar: ID
        let name_token = self.advance().ok_or("Expected identifier for variable")?;

        let name = match &name_token.value {
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
            annotation: Annotation::construct_variable_access_annotation(
                &name_token,
                &name,
                &var_index,
            ),
        })
    }

    fn parse_type<Annotation: ASTAnnotation>(&mut self) -> Result<ASTTypeNode<Annotation>, String> {
        let t = self.advance().ok_or("Expected type")?;
        match t.value {
            Token::TypeU8 => {
                let kind = ASTTypeKind::U8;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(&t, &kind),
                })
            }
            Token::TypeI8 => {
                let kind = ASTTypeKind::I8;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(&t, &kind),
                })
            }
            Token::TypeU16 => {
                let kind = ASTTypeKind::U16;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(&t, &kind),
                })
            }
            Token::TypeI16 => {
                let kind = ASTTypeKind::I16;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(&t, &kind),
                })
            }
            Token::TypeU32 => {
                let kind = ASTTypeKind::U32;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(&t, &kind),
                })
            }
            Token::TypeI32 => {
                let kind = ASTTypeKind::I32;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(&t, &kind),
                })
            }
            Token::TypeU64 => {
                let kind = ASTTypeKind::U64;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(&t, &kind),
                })
            }
            Token::TypeI64 => {
                let kind = ASTTypeKind::I64;
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_simple_type_node_annotation(&t, &kind),
                })
            }
            Token::TypeStr => {
                // Format: str<INT>
                let left_angle = self.expect(Token::LAngle)?;
                let size_token = self
                    .advance()
                    .ok_or("Expected integer literal for string size")?;
                let size = match size_token.value {
                    Token::IntLiteral(ref n) => n.parse::<usize>().unwrap(),
                    _ => return Err("Expected integer literal for string size".into()),
                };
                let right_angle = self.expect(Token::RAngle)?;
                let kind = ASTTypeKind::Str(size);
                Ok(ASTTypeNode {
                    kind,
                    annotation: Annotation::construct_str_type_node_annotation(
                        &t,
                        &kind,
                        &left_angle,
                        &size_token,
                        &right_angle,
                    ),
                })
            }
            _ => Err(format!("Unknown type token: {:?}", t)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTTypeNode<Annotation: ASTAnnotation> {
    pub kind: ASTTypeKind,
    pub annotation: Annotation::ASTTypeNodeAnnotation,
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
