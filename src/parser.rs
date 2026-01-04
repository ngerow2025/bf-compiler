use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::tokenizer::Token;

// --- AST Definitions ---


#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub type_: ASTType,
    pub variable_index: VariableId,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub body: Block,
    pub id: FunctionId,
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
}

#[derive(Debug, Clone)]
pub enum Statement {
    // let x: type = expr;
    VarDecl { name: String, type_: ASTType, value: Expression, variable_index: VariableId }, 
    
    // x = expr;
    Assignment { var: VariableId, value: Expression },
    
    // expr;
    Expression(Expression),
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
pub enum Expression {
    IntLiteral(IntLiteral),
    StringLiteral(String),
    Variable(VariableId),
    ArrayAccess { array: VariableId, index_expr: Box<Expression> },
    FnCall { name: String, arguments: Vec<Expression> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    variable_index: VariableId,
    variable_tracker: Vec<HashMap<String, VariableId>>, 
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            variable_index: VariableId(0),
            variable_tracker: vec![],
        }
    }

    // --- Helpers ---

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<Token> {
        let t = self.tokens.get(self.pos).cloned();
        self.pos += 1;
        t
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if self.peek() == Some(&expected) {
            self.advance();
            Ok(())
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
        Ok(Program { functions })
    }

    fn parse_function(&mut self, id: FunctionId) -> Result<Function, String> {
        self.variable_index = VariableId(0);
        self.variable_tracker.clear();
        self.variable_tracker.push(HashMap::new());
        // Grammar: "fn" ID "(" ")" Block
        self.expect(Token::Fn)?;

        let name = match self.advance() {
            Some(Token::Identifier(n)) => n.clone(),
            t => return Err(format!("Expected function name, found {:?}", t)),
        };

        self.expect(Token::LParen)?;


        let mut params = Vec::new();
        //parse parameters
        //syntax: fn function_name(param: Type)
        while self.peek() != Some(&Token::RParen) {
            let param_name = match self.advance() {
                Some(Token::Identifier(n)) => n.clone(),
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

            params.push(FunctionParam { type_: param_type, variable_index: var_index });

            if self.peek() == Some(&Token::Comma) {
                self.advance(); // consume comma
            } else {
                break; // no more parameters
            }
        }

        self.expect(Token::RParen)?;


        let body = self.parse_block()?;

        Ok(Function { name, body, params, id })
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        self.variable_tracker.push(HashMap::new());
        self.expect(Token::LBrace)?;
        let mut statements = Vec::new();

        while self.peek().is_some() && self.peek() != Some(&Token::RBrace) {
            statements.push(self.parse_statement_or_block()?);
        }

        self.expect(Token::RBrace)?;
        self.variable_tracker.pop();
        Ok(Block { statements })
    }

    fn parse_statement_or_block(&mut self) -> Result<BlockItem, String> {
        if self.peek() == Some(&Token::LBrace) {
            let block = self.parse_block()?;
            Ok(BlockItem::Block(block))
        } else {
            let stmt = self.parse_statement()?;
            Ok(BlockItem::Statement(stmt))
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        let token = self.peek().ok_or("Unexpected EOF")?;
        match token {
            Token::Let => self.parse_var_decl(),
            Token::Identifier(_) => {
                // Could be Assignment or expression
                // We need to look ahead 1 token
                let next_token = self.tokens.get(self.pos + 1);
                match next_token {
                    Some(Token::Equals) => self.parse_assignment(),
                    _ => Ok(Statement::Expression(self.parse_expression()?)),
                }
            }
            _ => Ok(Statement::Expression(self.parse_expression()?)),
        }
    }

    fn parse_var_decl(&mut self) -> Result<Statement, String> {
        // Grammar: "let" ID ":" Type "=" Expression ";"
        self.advance(); // consume 'let'

        let name = match self.advance() {
            Some(Token::Identifier(n)) => n.clone(),
            t => return Err(format!("Expected variable name, found {:?}", t)),
        };

        self.expect(Token::Colon)?;
        
        // Parse Type
        let type_ = self.parse_type()?;

        self.expect(Token::Equals)?;

        // Parse Expression
        let expr_ast = self.parse_expression()?;

        self.expect(Token::Semicolon)?;

        // make sure that a variable of this name does not already exist in the current scope
        for scope in self.variable_tracker.iter().rev() {
            if scope.contains_key(&name) {
                return Err(format!("Variable '{}' already declared in this scope", name));
            }
        }

        // assign variable index
        let var_index = self.variable_index;
        self.variable_index = VariableId(self.variable_index.0 + 1);
        self.variable_tracker
            .last_mut()
            .unwrap()
            .insert(name.clone(), var_index);

        
        Ok(Statement::VarDecl { name, type_, value: expr_ast, variable_index: var_index })
    }

    fn parse_assignment(&mut self) -> Result<Statement, String> {
        // Grammar: ID "=" Expression ";"
        let name = match self.advance() {
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
        let var_index = found_index.ok_or(format!("Variable '{}' not declared before assignment", name))?;
        Ok(Statement::Assignment { var: var_index, value: expr_ast })
    }

    fn parse_fn_call(&mut self, identifier: String) -> Result<Expression, String> {
        // Grammar: ID "(" ")" ";"
        let name = identifier;

        self.expect(Token::LParen)?;

        let mut args = Vec::new();
        while self.peek() != Some(&Token::RParen) {
            let arg_expr = self.parse_expression()?;
            args.push(arg_expr);

            if self.peek() == Some(&Token::Comma) {
                self.advance(); // consume comma
            } else {
                break; // no more arguments
            }
        }

        self.expect(Token::RParen)?;
        self.expect(Token::Semicolon)?;

        // function type checking will happen in a later phase
        
        Ok(Expression::FnCall {
            name,
            arguments: args,
        })
    }

    // Returns the AST node AND its resolved type
    fn parse_expression(&mut self) -> Result<Expression, String> {
        let token = self.advance().ok_or("Unexpected EOF in expression")?;

        match token {
            Token::IntLiteral(s) => {
                let int_lit_type = self.advance().ok_or("Expected type after integer literal")?;
                Ok(match int_lit_type {
                    Token::TypeU8 => {
                        let val = s.parse().map_err(|_| "Invalid u8 literal")?;
                        Expression::IntLiteral(IntLiteral::U8(val))
                    }
                    Token::TypeI8 => {
                        let val = s.parse().map_err(|_| "Invalid i8 literal")?;
                        Expression::IntLiteral(IntLiteral::I8(val))
                    }
                    Token::TypeU16 => {
                        let val = s.parse().map_err(|_| "Invalid u16 literal")?;
                        Expression::IntLiteral(IntLiteral::U16(val))
                    }
                    Token::TypeI16 => {
                        let val = s.parse().map_err(|_| "Invalid i16 literal")?;
                        Expression::IntLiteral(IntLiteral::I16(val))
                    }
                    Token::TypeU32 => {
                        let val = s.parse().map_err(|_| "Invalid u32 literal")?;
                        Expression::IntLiteral(IntLiteral::U32(val))
                    }
                    Token::TypeI32 => {
                        let val = s.parse().map_err(|_| "Invalid i32 literal")?;
                        Expression::IntLiteral(IntLiteral::I32(val))
                    }
                    Token::TypeU64 => {
                        let val = s.parse().map_err(|_| "Invalid u64 literal")?;
                        Expression::IntLiteral(IntLiteral::U64(val))
                    }
                    Token::TypeI64 => {
                        let val = s.parse().map_err(|_| "Invalid i64 literal")?;
                        Expression::IntLiteral(IntLiteral::I64(val))
                    }
                    _ => return Err("Expected type after integer literal".into()),
                })
            }
            Token::StringLiteral(s) => {
                // String literals are inherently str<LEN>
                Ok(Expression::StringLiteral(s.clone()))
            }
            Token::Identifier(name) => {
                // could be a function call
                if let Some(Token::LParen) = self.peek() {
                    self.parse_fn_call(name.clone())
                } else if let Some(Token::DoubleColon) = self.peek() {
                    // this is the start of a qualified name, must be a function call
                    // parse the full qualified name
                    let mut full_name = name.clone();
                    while let Some(Token::DoubleColon) = self.peek() {
                        full_name += "::";
                        self.advance(); // consume '::'
                        let next_part = match self.advance() {
                            Some(Token::Identifier(n)) => n,
                            t => return Err(format!("Expected identifier in qualified name, found {:?}", t)),
                        };
                        full_name += &next_part;
                    }
                    self.parse_fn_call(full_name)
                } else {

                    // look for the existing variable index
                    let mut found_index = None;
                    for scope in self.variable_tracker.iter().rev() {
                        if let Some(idx) = scope.get(&name) {
                            found_index = Some(*idx);
                            break;
                        }
                    }

                    let base_index = found_index.ok_or(format!("Variable '{}' not declared", name))?;

                    // if we peek an opening square bracket, this is an array access
                    if let Some(Token::LSquare) = self.peek() {
                        self.expect(Token::LSquare)?;

                        let expr = self.parse_expression()?;

                        self.expect(Token::RSquare)?;

                        return Ok(Expression::ArrayAccess {
                            array: base_index,
                            index_expr: Box::new(expr),
                        })
                    }
                    Ok(Expression::Variable(found_index.ok_or(format!("Variable '{}' not declared", name))?))
                }
            
            }
            _ => Err(format!("Invalid token in expression: {:?}", token)),
        }
    }

    fn parse_type(&mut self) -> Result<ASTType, String> {
        let t = self.advance().ok_or("Expected type")?;
        match t {
            Token::TypeU8 => Ok(ASTType::U8),
            Token::TypeI8 => Ok(ASTType::I8),
            Token::TypeU16 => Ok(ASTType::U16),
            Token::TypeI16 => Ok(ASTType::I16),
            Token::TypeU32 => Ok(ASTType::U32),
            Token::TypeI32 => Ok(ASTType::I32),
            Token::TypeU64 => Ok(ASTType::U64),
            Token::TypeI64 => Ok(ASTType::I64),
            Token::TypeStr => {
                // Format: str<INT>
                self.expect(Token::LAngle)?;
                let size = match self.advance() {
                    Some(Token::IntLiteral(n)) => n.parse::<usize>().unwrap(),
                    _ => return Err("Expected integer literal for string size".into()),
                };
                self.expect(Token::RAngle)?;
                Ok(ASTType::Str(size))
            }
            _ => Err(format!("Unknown type token: {:?}", t)),
        }
    }

}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ASTType {
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

#[cfg(test)]
mod parser_tests {
    use crate::tokenizer::{Lexer, TokenEmitter};

    use super::*;

    // Helper to tokenize then parse
    fn parse(input: &str) -> Result<Program, String> {
        let mut lexer = Lexer::new(input, TokenEmitter{});
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    #[test]
    fn test_valid_program() {
        let src = r#"
            fn main() {
                let a: u8 = 10;
                let b: i32 = 1000;
                std::out(a);
            }
        "#;
        let res = parse(src);
        assert!(res.is_ok(), "Program should parse: {:?}", res.err());
    }

    #[test]
    fn test_type_error_bounds() {
        // 300 does not fit in u8
        let src = r#"
            fn main() {
                let a: u8 = 300; 
            }
        "#;
        let res = parse(src);
        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), "Literal 300 is out of bounds for type U8");
    }

    #[test]
    fn test_type_error_mismatch() {
        // Assigning u8 variable to i32 variable
        let src = r#"
            fn main() {
                let a: u8 = 10;
                let b: i32 = a;
            }
        "#;
        let res = parse(src);
        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), "Type Mismatch: Expected I32, got U8");
    }

    #[test]
    fn test_undeclared_variable() {
        let src = r#"
            fn main() {
                a = 10;
            }
        "#;
        let res = parse(src);
        assert!(res.is_err());
        assert!(res.unwrap_err().contains("not declared before assignment")); 
        // Or "Variable 'a' not declared" depending on parsing path
    }

    #[test]
    fn test_string_overflow() {
        let src = r#"
            fn main() {
                let s: str<3> = "Hello";
            }
        "#;
        let res = parse(src);
        assert!(res.is_err());
        assert!(res.unwrap_err().contains("String overflow"));
    }
    
    #[test]
    fn test_stdin_type() {
        // std::in returns u8. Trying to put it in u16 should fail.
        let src = r#"
            fn main() {
                let x: u16 = std::in();
            }
        "#;
        let res = parse(src);
        assert!(res.is_err());
        assert!(res.unwrap_err().contains("Type Mismatch"));
    }
}