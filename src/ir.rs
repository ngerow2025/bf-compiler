use std::{collections::HashMap, vec};

use crate::parser::{Block, BlockItem, Expression, Function, IntLiteral, Program, Statement, VariableId};
use crate::type_check::*;


#[derive(Debug)]
enum IrInstruction {
    Move { target: IrRegisterId, source: IrMoveOperand },
    IndirectRead { base: IrRegisterId, offset: IrRegisterId, output: IrRegisterId },
    IndirectWrite { base: IrRegisterId, offset: IrRegisterId, value: IrRegisterId },
    Call { function_id: IrFunctionId, parameters: Vec<IrRegisterId>, output: IrRegisterId },
    Input { target: IrRegisterId },
    Output { element: IrRegisterId },

}



#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct IrFunctionId(usize);

#[derive(Debug)]
pub struct IrFunction {
    id: IrFunctionId,
    code: Vec<IrInstruction>,
    registers: HashMap<IrRegisterId, IrRegister>,
}


#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct IrRegisterId(usize);

#[derive(Debug, Clone)]
struct IrRegister {
    size: usize,
    id: IrRegisterId,
}

#[derive(Debug)]
enum IrMoveOperand {
    Register(IrRegisterId),
    IntLiteral(IntLiteral),
    StringLiteral(String),
}


pub fn generate_ir(ast: &TypedProgram) -> Vec<IrFunction> {
    ast.functions.iter().map(|func| {
        
        let mut function_lowerer = FunctionLowerer::new(&ast.functions);
        
        IrFunction {
            code: function_lowerer.lower_block(&func.body),
            id: IrFunctionId(func.id.0),
            registers: function_lowerer.registers.registers,
        }
    }).collect()
}

struct RegisterTracker {
    registers: HashMap<IrRegisterId, IrRegister>,
    variable_mapping: HashMap<VariableId, IrRegisterId>,
}

impl RegisterTracker {
    fn new() -> Self {
        RegisterTracker {
            registers: HashMap::new(),
            variable_mapping: HashMap::new(),
        }
    }

    fn create_register(&mut self, var_id: VariableId, size: usize) -> IrRegisterId {
        let new_reg_id = IrRegisterId(self.registers.len());
        self.registers.insert(new_reg_id, IrRegister {
            size,
            id: new_reg_id,
        });
        self.variable_mapping.insert(var_id, new_reg_id);
        new_reg_id
    }

    fn lookup_register(&self, var_id: &VariableId) -> Option<IrRegisterId> {
        self.variable_mapping.get(var_id).cloned()
    }

    fn lookup_register_from_id(&self, reg_id: IrRegisterId) -> Option<&IrRegister> {
        self.registers.get(&reg_id)
    }

    fn create_intermediate_register(&mut self, size: usize) -> IrRegisterId {
        let new_reg_id = IrRegisterId(self.registers.len());
        self.registers.insert(new_reg_id, IrRegister {
            size,
            id: new_reg_id,
        });
        new_reg_id
    }
}


struct FunctionLowerer<'a> {
    registers: RegisterTracker,
    function_list: &'a Vec<TypedFunction>,
}

impl<'a> FunctionLowerer<'a> {
    fn new(function_list: &'a Vec<TypedFunction>) -> Self {
        FunctionLowerer {
            registers: RegisterTracker::new(),
            function_list,
        }
    }

    fn lower_block(&mut self, block: &TypedBlock) -> Vec<IrInstruction> {
        let mut instructions = Vec::new();
        for item in &block.statements {
            match item {
                TypedBlockItem::Statement(stmt) => {
                    instructions.extend(
                        self.lower_statement(stmt)
                    );
                }
                TypedBlockItem::Block(sub_block) => {
                    instructions.extend(self.lower_block(sub_block));
                }
            }
        }
        instructions
    }

    fn lower_statement(&mut self, stmt: &TypedStatement) -> Vec<IrInstruction> {
        match stmt {
            TypedStatement::VarDecl { var, type_, value } => {
                let var_register = self.registers.create_register(*var, type_.size_in_bytes());
                
                let (expr_reg, mut instructions) = self.lower_expression(value);
                
                instructions.push(IrInstruction::Move {
                    target: var_register,
                    source: IrMoveOperand::Register(expr_reg),
                });
                instructions
            }

            TypedStatement::Assignment { var, value } => {
                let target_reg = self.registers.lookup_register(var).expect("Undefined variable");
                let (expr_reg, mut instructions) = self.lower_expression(value);
                
                instructions.push(IrInstruction::Move {
                    target: target_reg,
                    source: IrMoveOperand::Register(expr_reg),
                });
                instructions
            }
            TypedStatement::Expression(expr) => {
                self.lower_expression(expr).1
            }
        }
    }

    fn lower_expression(&mut self, expr: &TypedExpression) -> (IrRegisterId, Vec<IrInstruction>) {
        match expr {
            TypedExpression::IntLiteral {type_, int_literal} => {
                let new_reg = self.registers.create_intermediate_register(type_.size_in_bytes());
                let instructions = vec![IrInstruction::Move {
                    target: new_reg,
                    source: IrMoveOperand::IntLiteral(*int_literal),
                }];
                (new_reg, instructions)
            }
            TypedExpression::StringLiteral{type_, string_literal} => {
                let new_reg = self.registers.create_intermediate_register(type_.size_in_bytes());
                let instructions = vec![IrInstruction::Move {
                    target: new_reg,
                    source: IrMoveOperand::StringLiteral(string_literal.clone()),
                }];
                (new_reg, instructions)
            }
            TypedExpression::Variable { variable, type_ } => {
                let reg = self.registers.lookup_register(variable).expect("Undefined variable in expression");
                (reg, vec![])
            }
            TypedExpression::FnCall { function, arguments, type_ } => {
                let mut instructions = Vec::new();
                let mut arg_registers = Vec::new();
                for arg in arguments {
                    let (arg_reg, mut arg_instructions) = self.lower_expression(arg);
                    instructions.append(&mut arg_instructions);
                    arg_registers.push(arg_reg);
                }
                let output_reg = self.registers.create_intermediate_register(type_.size_in_bytes());
                instructions.push(IrInstruction::Call {
                    function_id: IrFunctionId(function.0),
                    parameters: arg_registers,
                    output: output_reg,
                });
                (output_reg, instructions)
            }
            TypedExpression::ArrayAccess { array, index, type_ } => {

            }
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::parser::{Block, BlockItem, Expression, Function, Statement, Type, Program};

//     #[test]
//     fn test_ir_generation() {
//         // fn main() {
//         //     let a: u8 = 10;
//         //     std::out(a);
//         // }
//         let ast = Program {
//             functions: vec![Function {
//                 name: "main".to_string(),
//                 params: vec![],
//                 body: Block {
//                     statements: vec![
//                         BlockItem::Statement(Statement::VarDecl {
//                             name: "a".to_string(),
//                             type_: Type::U8,
//                             value: Expression::IntLiteral(IntLiteral::U8(10)),
//                         }),
//                         BlockItem::Statement(Statement::Output(Expression::Variable("a".to_string()))),
//                     ],
//                 },
//             }],
//         };

//         let ir = generate_ir(&ast);
//         assert_eq!(ir.len(), 1);
//         let main_func = &ir[0];
//         assert_eq!(main_func.name, "main");
        
//         let block = &main_func.code;
        
//         // Count instructions
//         // 1. Move lit
//         // 2. Move var
//         // 3. Output
//         assert_eq!(block.instructions.len(), 3);
        
//         if let IrBlockItem::Instruction(IrInstruction::Move { target: _, source }) = &block.instructions[0] {
//              if let IrMoveOperand::IntLiteral(val) = source {
//                  assert_eq!(*val, IntLiteral::U8(10));
//              } else {
//                  panic!("Expected Immediate");
//              }
//         } else {
//             panic!("Expected Move instruction");
//         }
//     }
// }
