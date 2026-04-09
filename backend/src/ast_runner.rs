use std::collections::HashMap;

use crate::{
    ast_annotation::ASTAnnotation,
    input_grabbing::{RawModeGuard, read_single_byte},
    parser::{
        ASTTypeKind, Block, BlockItem, Expression, Function, IntLiteral, Program, Statement,
        VariableId,
    },
};

struct GlobalContext<'a, T: ASTAnnotation> {
    //holds all functions
    pub functions: &'a Vec<Function<T>>,
}

pub fn run_program<T: ASTAnnotation>(program: &Program<T>) {
    let _raw_mode_guard = RawModeGuard::new();

    let global_context = GlobalContext {
        functions: &program.functions,
    };

    let main_function = global_context
        .functions
        .iter()
        .find(|f| f.name == "main")
        .expect("No main function found");

    run_function(main_function, &global_context, vec![]);
}

#[allow(dead_code)]
struct VariableInfo {
    value: Value,
    id: VariableId,
    type_: ASTTypeKind,
    name: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum Value {
    I64(i64),
    U64(u64),
    I32(i32),
    U32(u32),
    I16(i16),
    U16(u16),
    I8(i8),
    U8(u8),
    String(String),
    None,
}

impl Value {
    pub fn get_type(&self) -> Option<ASTTypeKind> {
        match self {
            Value::I64(_) => Some(ASTTypeKind::I64),
            Value::U64(_) => Some(ASTTypeKind::U64),
            Value::I32(_) => Some(ASTTypeKind::I32),
            Value::U32(_) => Some(ASTTypeKind::U32),
            Value::I16(_) => Some(ASTTypeKind::I16),
            Value::U16(_) => Some(ASTTypeKind::U16),
            Value::I8(_) => Some(ASTTypeKind::I8),
            Value::U8(_) => Some(ASTTypeKind::U8),
            Value::String(s) => Some(ASTTypeKind::Str(s.len())),
            Value::None => None,
        }
    }
}

struct FunctionContext {
    variables: HashMap<VariableId, VariableInfo>,
}

impl FunctionContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
}

fn run_function<T: ASTAnnotation>(
    function: &Function<T>,
    global_context: &GlobalContext<T>,
    arguments: Vec<Value>,
) {
    let mut function_context = FunctionContext::new();
    for (i, param) in function.params.iter().enumerate() {
        function_context.variables.insert(
            param.variable_index,
            VariableInfo {
                value: arguments[i].clone(),
                id: param.variable_index,
                type_: param.type_.kind,
                name: param.name.clone(),
            },
        );
    }

    run_block(&function.body, global_context, &mut function_context);
}

fn run_block<T: ASTAnnotation>(
    block: &Block<T>,
    global_context: &GlobalContext<T>,
    function_context: &mut FunctionContext,
) {
    for block_item in &block.statements {
        match block_item {
            BlockItem::Block(inner_block) => {
                run_block(inner_block, global_context, function_context)
            }
            BlockItem::Statement(inner_statement) => {
                run_statement(inner_statement, global_context, function_context)
            }
        }
    }
}

fn run_statement<T: ASTAnnotation>(
    statement: &Statement<T>,
    global_context: &GlobalContext<T>,
    function_context: &mut FunctionContext,
) {
    match statement {
        Statement::VarDecl {
            name,
            type_,
            value,
            variable_index,
            annotation: _,
        } => {
            let value = run_expression(value, global_context, function_context);
            function_context.variables.insert(
                *variable_index,
                VariableInfo {
                    value,
                    id: *variable_index,
                    type_: type_.kind,
                    name: name.to_string(),
                },
            );
        }
        Statement::Assignment {
            var,
            value,
            annotation: _,
        } => {
            let value = run_expression(value, global_context, function_context);
            let var = function_context
                .variables
                .get_mut(var)
                .expect("expected to be valid variable");
            //ensure that the types match
            assert_eq!(
                value.get_type(),
                Some(var.type_),
                "Type mismatch in assignment"
            );
            var.value = value;
        }
        Statement::Expression {
            expr,
            annotation: _,
        } => {
            run_expression(expr, global_context, function_context);
        }
    }
}

fn run_expression<T: ASTAnnotation>(
    expr: &Expression<T>,
    global_context: &GlobalContext<T>,
    function_context: &mut FunctionContext,
) -> Value {
    match expr {
        Expression::IntLiteral {
            value,
            annotation: _,
        } => match value {
            IntLiteral::I64(x) => Value::I64(*x),
            IntLiteral::U64(x) => Value::U64(*x),
            IntLiteral::I32(x) => Value::I32(*x),
            IntLiteral::U32(x) => Value::U32(*x),
            IntLiteral::I16(x) => Value::I16(*x),
            IntLiteral::U16(x) => Value::U16(*x),
            IntLiteral::I8(x) => Value::I8(*x),
            IntLiteral::U8(x) => Value::U8(*x),
        },
        Expression::CharLiteral {
            value,
            annotation: _,
        } => Value::U8(*value as u8),
        Expression::StringLiteral {
            value,
            annotation: _,
        } => Value::String(value.clone()),
        Expression::VariableAccess {
            value,
            annotation: _,
        } => {
            let var_info = function_context
                .variables
                .get(&value.id)
                .expect("expected to be valid variable");
            var_info.value.clone()
        }
        Expression::ArrayAccess {
            array,
            index_expr,
            annotation: _,
        } => {
            let index_value = run_expression(index_expr, global_context, function_context);
            let array_value = function_context
                .variables
                .get(&array.id)
                .expect("expected to be valid array variable");

            assert_eq!(
                index_value.get_type(),
                Some(ASTTypeKind::U8),
                "Array index must be of type u8"
            );
            assert!(
                matches!(array_value.value.get_type(), Some(ASTTypeKind::Str(_))),
                "Only string literals can be indexed"
            );

            match &array_value.value {
                Value::String(s) => {
                    let index = match index_value {
                        Value::U8(x) => x as usize,
                        _ => unreachable!(),
                    };
                    let char = s.chars().nth(index).expect("Index out of bounds");
                    Value::U8(char as u8)
                }
                _ => unreachable!(),
            }
        }
        Expression::FnCall {
            qualified_name,
            arguments,
            annotation: _,
        } => {
            if qualified_name.full_name() == "std::out" {
                assert_eq!(arguments.len(), 1, "std::out expects exactly one argument");
                let arg_value = run_expression(&arguments[0], global_context, function_context);
                assert_eq!(
                    arg_value.get_type(),
                    Some(ASTTypeKind::U8),
                    "std::out only accepts u8 arguments"
                );
                match arg_value {
                    Value::U8(x) => print!("{}", x as char),
                    _ => unreachable!(),
                }
                Value::None
            } else if qualified_name.full_name() == "std::in" {
                assert!(arguments.is_empty(), "std::in expects no arguments");
                Value::U8(read_single_byte())
            } else {
                let function = global_context
                    .functions
                    .iter()
                    .find(|f| f.name == qualified_name.full_name())
                    .unwrap_or_else(|| panic!("Function {} not found", qualified_name.full_name()));

                run_function(
                    function,
                    global_context,
                    arguments
                        .iter()
                        .map(|arg| run_expression(arg, global_context, function_context))
                        .collect(),
                );
                Value::None
            }
        }
    }
}
