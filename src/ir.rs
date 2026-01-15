use std::fmt::Display;
use std::{collections::HashMap, vec};

use crate::parser::{
    Block, BlockItem, Expression, Function, FunctionId, IntLiteral, Program, Statement, VariableId,
};
use crate::type_check::*;

#[derive(Debug)]
pub enum IrInstruction {
    Move {
        target: IrRegisterId,
        source: IrMoveOperand,
    },
    IndirectRead {
        base: IrRegisterId,
        offset: IrRegisterId,
        output: IrRegisterId,
    },
    IndirectWrite {
        base: IrRegisterId,
        offset: IrRegisterId,
        value: IrRegisterId,
    },
    Call {
        function_id: FunctionId,
        parameters: Vec<IrRegisterId>,
        output: IrRegisterId,
    },
    Input {
        target: IrRegisterId,
    },
    Output {
        element: IrRegisterId,
    },
}

#[derive(Debug)]
pub struct IrFunction {
    pub id: FunctionId,
    pub code: Vec<IrInstruction>,
    pub registers: HashMap<IrRegisterId, IrRegister>,
    pub parameters: Vec<IrRegisterId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IrRegisterId(usize);

#[derive(Debug, Clone)]
pub struct IrRegister {
    pub size: usize,
    id: IrRegisterId,
}

#[derive(Debug)]
pub enum IrMoveOperand {
    Register(IrRegisterId),
    IntLiteral(IntLiteral),
    StringLiteral(String),
}

pub fn generate_ir(ast: &TypedProgram) -> Vec<IrFunction> {
    ast.functions
        .iter()
        .map(|func| {
            let mut function_lowerer = FunctionLowerer::new(&ast.functions);
            let code = function_lowerer.lower_function(func);
            let parameter_registers = func
                .params
                .iter()
                .map(|param| {
                    function_lowerer
                        .registers
                        .lookup_register(&param.variable_index)
                        .unwrap()
                })
                .collect();
            IrFunction {
                code,
                id: func.id,
                registers: function_lowerer.registers.registers,
                parameters: parameter_registers,
            }
        })
        .map(
            |IrFunction {
                 id,
                 parameters,
                 code: old_code,
                 registers: old_registers,
             }| {
                let mut deleted_registers = vec![];
                let new_code = old_code
                    .into_iter()
                    .map(|i| match i {
                        IrInstruction::Call {
                            function_id,
                            parameters,
                            output,
                        } => {
                            if ast.function_name_mapping.get(&function_id)
                                == Some(&"std::out".to_string())
                            {
                                assert_eq!(parameters.len(), 1);
                                deleted_registers.push(output);
                                IrInstruction::Output {
                                    element: parameters[0],
                                }
                            } else if ast.function_name_mapping.get(&function_id)
                                == Some(&"std::in".to_string())
                            {
                                assert_eq!(parameters.len(), 0);
                                IrInstruction::Input { target: output }
                            } else {
                                IrInstruction::Call {
                                    function_id,
                                    parameters,
                                    output,
                                }
                            }
                        }
                        a => a,
                    })
                    .collect();
                let mut new_registers = old_registers.clone();
                new_registers.retain(|k, v| !deleted_registers.contains(k));
                IrFunction {
                    id,
                    parameters,
                    code: new_code,
                    registers: new_registers,
                }
            },
        )
        .collect()
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
        self.registers.insert(
            new_reg_id,
            IrRegister {
                size,
                id: new_reg_id,
            },
        );
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
        self.registers.insert(
            new_reg_id,
            IrRegister {
                size,
                id: new_reg_id,
            },
        );
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

    fn lower_function(&mut self, function: &TypedFunction) -> Vec<IrInstruction> {
        for param in &function.params {
            self.registers
                .create_register(param.variable_index, param.type_.size_in_bytes());
        }
        self.lower_block(&function.body)
    }

    fn lower_block(&mut self, block: &TypedBlock) -> Vec<IrInstruction> {
        let mut instructions = Vec::new();
        for item in &block.statements {
            match item {
                TypedBlockItem::Statement(stmt) => {
                    instructions.extend(self.lower_statement(stmt));
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
                let target_reg = self
                    .registers
                    .lookup_register(var)
                    .expect("Undefined variable");
                let (expr_reg, mut instructions) = self.lower_expression(value);

                instructions.push(IrInstruction::Move {
                    target: target_reg,
                    source: IrMoveOperand::Register(expr_reg),
                });
                instructions
            }
            TypedStatement::Expression { expr } => self.lower_expression(expr).1,
        }
    }

    fn lower_expression(&mut self, expr: &TypedExpression) -> (IrRegisterId, Vec<IrInstruction>) {
        match expr {
            TypedExpression::IntLiteral { type_, int_literal } => {
                let new_reg = self
                    .registers
                    .create_intermediate_register(type_.size_in_bytes());
                let instructions = vec![IrInstruction::Move {
                    target: new_reg,
                    source: IrMoveOperand::IntLiteral(*int_literal),
                }];
                (new_reg, instructions)
            }
            TypedExpression::StringLiteral {
                type_,
                string_literal,
            } => {
                let new_reg = self
                    .registers
                    .create_intermediate_register(type_.size_in_bytes());
                let instructions = vec![IrInstruction::Move {
                    target: new_reg,
                    source: IrMoveOperand::StringLiteral(string_literal.clone()),
                }];
                (new_reg, instructions)
            }
            TypedExpression::Variable { variable, type_ } => {
                let reg = self
                    .registers
                    .lookup_register(variable)
                    .expect("Undefined variable in expression");
                (reg, vec![])
            }
            TypedExpression::FnCall {
                function,
                arguments,
                type_,
            } => {
                let mut instructions = Vec::new();
                let mut arg_registers = Vec::new();
                for arg in arguments {
                    let (arg_reg, mut arg_instructions) = self.lower_expression(arg);
                    instructions.append(&mut arg_instructions);
                    arg_registers.push(arg_reg);
                }
                let output_reg = self
                    .registers
                    .create_intermediate_register(type_.size_in_bytes());
                instructions.push(IrInstruction::Call {
                    function_id: *function,
                    parameters: arg_registers,
                    output: output_reg,
                });
                (output_reg, instructions)
            }
            TypedExpression::ArrayAccess {
                array,
                index,
                type_,
            } => {
                let mut instructions = Vec::new();
                let (index_reg, mut index_instructions) = self.lower_expression(index);
                instructions.append(&mut index_instructions);
                let output_reg = self
                    .registers
                    .create_intermediate_register(type_.size_in_bytes());
                let array_reg = self
                    .registers
                    .lookup_register(array)
                    .expect("register was not allocated for array var");
                instructions.push(IrInstruction::IndirectRead {
                    base: array_reg,
                    offset: index_reg,
                    output: output_reg,
                });
                (output_reg, instructions)
            }
        }
    }
}

impl Display for IrFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Function {:?}:", self.id)?;
        for instr in &self.code {
            writeln!(f, "  {}", instr)?;
        }
        Ok(())
    }
}

impl Display for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrInstruction::Move { target, source } => {
                write!(f, "MOVE {:?} -> {:?}", source, target)
            }
            IrInstruction::IndirectRead {
                base,
                offset,
                output,
            } => {
                write!(
                    f,
                    "INDIRECT_READ base: {:?}, offset: {:?} -> {:?}",
                    base, offset, output
                )
            }
            IrInstruction::IndirectWrite {
                base,
                offset,
                value,
            } => {
                write!(
                    f,
                    "INDIRECT_WRITE base: {:?}, offset: {:?} <- {:?}",
                    base, offset, value
                )
            }
            IrInstruction::Call {
                function_id,
                parameters,
                output,
            } => {
                write!(
                    f,
                    "CALL {:?}({:?}) -> {:?}",
                    function_id, parameters, output
                )
            }
            IrInstruction::Input { target } => {
                write!(f, "INPUT -> {:?}", target)
            }
            IrInstruction::Output { element } => {
                write!(f, "OUTPUT <- {:?}", element)
            }
        }
    }
}
