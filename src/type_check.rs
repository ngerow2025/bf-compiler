use core::panic;
use std::collections::HashMap;

use crate::parser::{ASTType, Block, BlockItem, Expression, Function, FunctionId, IntLiteral, Program, Statement, VariableId};


#[derive(Debug)]
pub struct TypedProgram {
    pub functions: Vec<TypedFunction>,
    pub function_name_mapping: HashMap<FunctionId, String>,
}

#[derive(Debug)]
pub struct TypedFunction {
    pub params: Vec<TypedFunctionParam>,
    pub body: TypedBlock,
    pub variable_map: HashMap<VariableId, Type>, // this maps variable ids to their types
    pub id: FunctionId,
}

#[derive(Debug)]
pub struct TypedFunctionParam {
    pub type_: Type,
    pub variable_index: VariableId,
}

#[derive(Debug)]
pub struct TypedBlock {
    pub statements: Vec<TypedBlockItem>,
}

#[derive(Debug)]
pub enum TypedBlockItem {
    Statement(TypedStatement),
    Block(TypedBlock),
}


#[derive(Debug)]
pub enum TypedStatement {
    // let x: type = expr;
    VarDecl { var: VariableId, type_: Type, value: TypedExpression }, 
    
    // x = expr;
    Assignment { var: VariableId, value: TypedExpression },
    
    // expr;
    Expression(TypedExpression),
}

#[derive(Debug, Clone)]
pub enum TypedExpression {
    IntLiteral{
        int_literal: IntLiteral,
        type_: Type,
    },
    StringLiteral {
        string_literal: String,
        type_: Type,
    },
    Variable {
        variable: VariableId,
        type_: Type,
    },
    FnCall {
        function: FunctionId,
        arguments: Vec<TypedExpression>,
        type_: Type
    },
    ArrayAccess {
        array: VariableId,
        index: Box<TypedExpression>,
        type_: Type,
    },
}

impl TypedExpression {
    pub fn get_type(&self) -> Type {
        match self {
            TypedExpression::IntLiteral { type_, .. } => *type_,
            TypedExpression::StringLiteral { type_, .. } => *type_,
            TypedExpression::Variable { type_, .. } => *type_,
            TypedExpression::FnCall { type_, .. } => *type_,
            TypedExpression::ArrayAccess { type_, .. } => *type_,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub enum Type {
    U8, I8, U16, I16, U32, I32, U64, I64,
    Str(usize), // str<N>
    Void,       // For functions
    #[default] Unknown,   // For untyped variables (to be inferred)
}


impl Type {
    pub fn size_in_bytes(&self) -> usize {
        match self {
            Type::U8 | Type::I8 => 1,
            Type::U16 | Type::I16 => 2,
            Type::U32 | Type::I32 => 4,
            Type::U64 | Type::I64 => 8,
            Type::Str(len) => *len,
            Type::Void => 0,
            Type::Unknown => panic!("Cannot get size of unknown type"),
        }
    }
}

impl From<ASTType> for Type {
    fn from(ast_type: ASTType) -> Self {
        match ast_type {
            ASTType::U8 => Type::U8,
            ASTType::I8 => Type::I8,
            ASTType::U16 => Type::U16,
            ASTType::I16 => Type::I16,
            ASTType::U32 => Type::U32,
            ASTType::I32 => Type::I32,
            ASTType::U64 => Type::U64,
            ASTType::I64 => Type::I64,
            ASTType::Str(len) => Type::Str(len),
        }
    }
}

pub struct FunctionSignature {
    param_types: Vec<Type>,
    return_type: Type,
}

pub fn type_annotate_program(ast_program: Program) -> TypedProgram {
    // first collect all the function signatures and create mapping from function name to id
    let mut function_name_map = HashMap::new();
    let mut function_signature_map: HashMap<FunctionId, FunctionSignature> = HashMap::new();
    for function in &ast_program.functions {
        function_name_map.insert(function.name.clone(), function.id);
        function_signature_map.insert(function.id, FunctionSignature {
            param_types: function.params.iter().map(|p| Type::from(p.type_)).collect(),
            return_type: Type::Void, // Placeholder, assuming void return type for now
        });
    }
    let mut next_function_id = FunctionId(ast_program.functions.len());
    
    while function_signature_map.contains_key(&next_function_id) {
        next_function_id = FunctionId(next_function_id.0 + 1);
    }
    //add in the built-in functions
    function_name_map.insert("std::in".to_string(), next_function_id);
    function_signature_map.insert(next_function_id, FunctionSignature {
        param_types: vec![],
        return_type: Type::U8,
    });

    while function_signature_map.contains_key(&next_function_id) {
        next_function_id = FunctionId(next_function_id.0 + 1);
    }

    function_name_map.insert("std::out".to_string(), next_function_id);
    function_signature_map.insert(next_function_id, FunctionSignature {
        param_types: vec![Type::U8],
        return_type: Type::Void,
    });

    let mut reversed_function_name_map = HashMap::new();
    for (function_name, function_id) in function_name_map.clone() {
        reversed_function_name_map.insert(function_id, function_name);
    }


    TypedProgram { 
        functions: ast_program.functions.into_iter().map(|f| type_annotate_function(f, &function_name_map, &function_signature_map)).collect(),
        function_name_mapping: reversed_function_name_map
    }
}

fn type_annotate_function(ast_function: Function, function_name_map: &HashMap<String, FunctionId>, function_signature_map: &HashMap<FunctionId, FunctionSignature>) -> TypedFunction {
    let mut variable_type_map = HashMap::new();
    
    for param in &ast_function.params {
        variable_type_map.insert(param.variable_index, Type::from(param.type_));
    }

    let typed_function_params = ast_function.params.iter().map(|p| TypedFunctionParam {
        type_: Type::from(p.type_),
        variable_index: p.variable_index,
    }).collect::<Vec<TypedFunctionParam>>();
    
    let typed_body = type_annotate_block(ast_function.body, &mut variable_type_map, function_name_map, function_signature_map);

    
    // Placeholder implementation
    TypedFunction {
        params: typed_function_params,
        body: typed_body,
        variable_map: variable_type_map,
        id: ast_function.id,
    }
}

fn type_annotate_block(ast_block: Block, variable_type_map: &mut HashMap<VariableId, Type>, function_name_map: &HashMap<String, FunctionId>, function_signature_map: &HashMap<FunctionId, FunctionSignature>) -> TypedBlock {
    let mut typed_statements = Vec::new();

    for item in ast_block.statements {
        match item {
            BlockItem::Statement(stmt) => {
                let typed_stmt = type_annotate_statement(stmt, variable_type_map, function_name_map, function_signature_map);
                typed_statements.push(TypedBlockItem::Statement(typed_stmt));
            },
            BlockItem::Block(block) => {
                let typed_block = type_annotate_block(block, variable_type_map, function_name_map, function_signature_map);
                typed_statements.push(TypedBlockItem::Block(typed_block));
            },
        }
    }

    TypedBlock { statements: typed_statements }
}

fn type_annotate_statement(ast_stmt: Statement, variable_type_map: &mut HashMap<VariableId, Type>, function_name_map: &HashMap<String, FunctionId>, function_signature_map: &HashMap<FunctionId, FunctionSignature>) -> TypedStatement {
    match ast_stmt {
        Statement::VarDecl { variable_index, type_, value, name } => {
            let typed_value = type_annotate_expression(value, variable_type_map, function_name_map, function_signature_map);
            let var_type = Type::from(type_);
            variable_type_map.insert(variable_index, var_type);

            // Type check time
            assert_eq!(typed_value.get_type(), var_type, "Type mismatch in variable declaration for '{}'", name);

            TypedStatement::VarDecl { var: variable_index, type_: var_type, value: typed_value }
        },
        Statement::Assignment { var, value } => {
            let typed_value = type_annotate_expression(value, variable_type_map, function_name_map, function_signature_map);

            // Type check time
            let var_type = *variable_type_map.get(&var).expect("Variable not declared");
            assert_eq!(typed_value.get_type(), var_type, "Type mismatch in assignment to variable {:?}", var);

            TypedStatement::Assignment { var, value: typed_value }
        },
        Statement::Expression(expr) => {
            let typed_expr = type_annotate_expression(expr, variable_type_map, function_name_map, function_signature_map);
            TypedStatement::Expression(typed_expr)
        },
    }
}

fn type_annotate_expression(ast_expr: Expression, variable_type_map: &HashMap<VariableId, Type>, function_name_map: &HashMap<String, FunctionId>, function_signature_map: &HashMap<FunctionId, FunctionSignature>) -> TypedExpression {
    match ast_expr {
        Expression::IntLiteral(int_literal) => {
            TypedExpression::IntLiteral {
                int_literal,
                type_: match int_literal {
                    IntLiteral::I8(_) => Type::I8,
                    IntLiteral::U8(_) => Type::U8,
                    IntLiteral::I16(_) => Type::I16,
                    IntLiteral::U16(_) => Type::U16,
                    IntLiteral::I32(_) => Type::I32,
                    IntLiteral::U32(_) => Type::U32,
                    IntLiteral::I64(_) => Type::I64,
                    IntLiteral::U64(_) => Type::U64,
                }
            }
        }
        Expression::StringLiteral(str) => TypedExpression::StringLiteral {
            type_: Type::Str(str.len()),
            string_literal: str,
        },
        Expression::Variable(var) => TypedExpression::Variable {
            type_: *variable_type_map.get(&var).expect("Variable not declared"),
            variable: var
        },
        Expression::FnCall {name, arguments} => {
            let function_id = *function_name_map.get(&name).unwrap_or_else(|| panic!("unknown function {name}"));
            let function_signature = function_signature_map.get(&function_id).expect("Function signature not found");
            let typed_arguments = arguments.into_iter().map(|arg| type_annotate_expression(arg, variable_type_map, function_name_map, function_signature_map)).collect::<Vec<_>>();

            // Argument type checking
            for (i, typed_arg) in typed_arguments.iter().enumerate() {
                let expected_type = *function_signature.param_types.get(i).expect("Argument index out of bounds");
                assert_eq!(typed_arg.get_type(), expected_type, "Type mismatch in argument {} of function call to '{}'", i, name);
            }

            TypedExpression::FnCall {
                function: function_id,
                arguments: typed_arguments,
                type_: function_signature.return_type,
            }
        },
        Expression::ArrayAccess { array, index_expr } => {
            TypedExpression::ArrayAccess {
                array,
                type_: Type::U8,
                index: Box::new(type_annotate_expression(*index_expr, variable_type_map, function_name_map, function_signature_map)),
            }
        }
    }

}

