use std::fmt::Display;
use std::{collections::HashMap, vec};

use crate::parser::{FunctionId, IntLiteral, VariableId};
use crate::type_check::{
    TypedBlock, TypedBlockItem, TypedExpression, TypedFunction, TypedProgram, TypedStatement,
};

#[derive(Debug)]
pub enum IrInstruction {
    Copy {
        target: IrRegisterId,
        source: IrCopyOperand,
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
    pub parameters: Vec<IrParameter>,
    pub name: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IrRegisterId(pub usize);

#[derive(Debug, Clone)]
pub struct IrRegister {
    pub size: usize,
    pub description: String,
}

#[derive(Debug)]
pub enum IrCopyOperand {
    Register(IrRegisterId),
    IntLiteral(IntLiteral),
    StringLiteral(String),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct IrParameter {
    pub register: IrRegisterId,
    pub name: String,
}

#[must_use]
pub fn generate_ir(ast: &TypedProgram) -> Vec<IrFunction> {
    ast.functions
        .iter()
        .map(|func| {
            let (code, register_tracker) = lower_function(func);
            let parameter_registers = func
                .params
                .iter()
                .map(|param| IrParameter {
                    register: register_tracker
                        .lookup_register(&param.variable_index)
                        .unwrap(),
                    name: param.name.clone(),
                })
                .collect();
            IrFunction {
                code,
                id: func.id,
                registers: register_tracker.registers,
                parameters: parameter_registers,
                name: func.name.clone(),
            }
        })
        .map(
            |IrFunction {
                 id,
                 parameters,
                 code: old_code,
                 registers: old_registers,
                 name,
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
                                    element: *parameters
                                        .first()
                                        .expect("there should only be a single param for std::out"),
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
                let mut new_registers = old_registers;
                new_registers.retain(|k, _| !deleted_registers.contains(k));
                IrFunction {
                    id,
                    parameters,
                    code: new_code,
                    registers: new_registers,
                    name,
                }
            },
        )
        .collect()
}

#[derive(Debug, Clone)]
pub struct RegisterTracker {
    registers: HashMap<IrRegisterId, IrRegister>,
    variable_mapping: HashMap<VariableId, IrRegisterId>,
}

#[must_use]
pub fn lower_function(function: &TypedFunction) -> (Vec<IrInstruction>, RegisterTracker) {
    let mut function_lowerer = FunctionLowerer::new(
        function
            .variable_info
            .iter()
            .map(|(k, v)| (*k, v.name.clone()))
            .collect(),
    );
    let code = function_lowerer.lower_function(function);
    (code, function_lowerer.register_tracker())
}

impl Default for RegisterTracker {
    fn default() -> Self {
        Self::new()
    }
}

impl RegisterTracker {
    #[must_use]
    pub fn new() -> Self {
        Self {
            registers: HashMap::new(),
            variable_mapping: HashMap::new(),
        }
    }

    pub fn create_register(
        &mut self,
        var_id: VariableId,
        size: usize,
        description: &str,
    ) -> IrRegisterId {
        let new_reg_id = IrRegisterId(self.registers.len());
        self.registers.insert(
            new_reg_id,
            IrRegister {
                size,
                description: format!("{}#{}", description, new_reg_id.0),
            },
        );
        self.variable_mapping.insert(var_id, new_reg_id);
        new_reg_id
    }

    #[must_use]
    pub fn lookup_register(&self, var_id: &VariableId) -> Option<IrRegisterId> {
        self.variable_mapping.get(var_id).copied()
    }

    #[must_use]
    pub fn lookup_register_from_id(&self, reg_id: IrRegisterId) -> Option<&IrRegister> {
        self.registers.get(&reg_id)
    }

    pub fn create_intermediate_register(&mut self, size: usize, description: &str) -> IrRegisterId {
        let new_reg_id = IrRegisterId(self.registers.len());
        self.registers.insert(
            new_reg_id,
            IrRegister {
                size,
                description: format!("{}#{}", description, new_reg_id.0),
            },
        );
        new_reg_id
    }
}

struct FunctionLowerer {
    registers: RegisterTracker,
    variable_names: HashMap<VariableId, String>,
}

impl FunctionLowerer {
    fn new(variable_names: HashMap<VariableId, String>) -> Self {
        Self {
            registers: RegisterTracker::new(),
            variable_names,
        }
    }

    fn lower_function(&mut self, function: &TypedFunction) -> Vec<IrInstruction> {
        for param in &function.params {
            self.registers.create_register(
                param.variable_index,
                param.type_.size_in_bytes(),
                &format!("param%{}%", param.name),
            );
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
                let var_register = self.registers.create_register(
                    *var,
                    type_.size_in_bytes(),
                    &format!(
                        "var%{}%",
                        self.variable_names
                            .get(var)
                            .cloned()
                            .unwrap_or_else(|| format!("unnamed_var_{}", var.0))
                    ),
                );

                let (expr_reg, mut instructions) = self.lower_expression(value);

                instructions.push(IrInstruction::Copy {
                    target: var_register,
                    source: IrCopyOperand::Register(expr_reg),
                });
                instructions
            }

            TypedStatement::Assignment { var, value } => {
                let target_reg = self
                    .registers
                    .lookup_register(var)
                    .expect("Undefined variable");
                let (expr_reg, mut instructions) = self.lower_expression(value);

                instructions.push(IrInstruction::Copy {
                    target: target_reg,
                    source: IrCopyOperand::Register(expr_reg),
                });
                instructions
            }
            TypedStatement::Expression { expr } => self.lower_expression(expr).1,
        }
    }

    fn lower_expression(&mut self, expr: &TypedExpression) -> (IrRegisterId, Vec<IrInstruction>) {
        match expr {
            TypedExpression::IntLiteral { type_, int_literal } => {
                let new_reg = self.registers.create_intermediate_register(
                    type_.size_in_bytes(),
                    &format!("int_literal_tmp({int_literal})"),
                );
                let instructions = vec![IrInstruction::Copy {
                    target: new_reg,
                    source: IrCopyOperand::IntLiteral(*int_literal),
                }];
                (new_reg, instructions)
            }
            TypedExpression::StringLiteral {
                type_,
                string_literal,
            } => {
                let new_reg = self.registers.create_intermediate_register(
                    type_.size_in_bytes(),
                    &format!(
                        "string_literal_tmp({})",
                        sanitize_string_literal(string_literal)
                    ),
                );
                let instructions = vec![IrInstruction::Copy {
                    target: new_reg,
                    source: IrCopyOperand::StringLiteral(string_literal.clone()),
                }];
                (new_reg, instructions)
            }
            TypedExpression::Variable { variable, type_: _ } => {
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
                let output_reg = self.registers.create_intermediate_register(
                    type_.size_in_bytes(),
                    &format!("call_output_tmp({function})"),
                );
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
                let output_reg = self.registers.create_intermediate_register(
                    type_.size_in_bytes(),
                    &format!(
                        "array_access_output_tmp({})",
                        self.variable_names
                            .get(array)
                            .cloned()
                            .unwrap_or_else(|| format!("unnamed_array_{}", array.0))
                    ),
                );
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

    fn register_tracker(&self) -> RegisterTracker {
        self.registers.clone()
    }
}

impl Display for IrFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Function {:?}:", self.id)?;
        for instr in &self.code {
            writeln!(f, "  {instr}")?;
        }
        Ok(())
    }
}

impl Display for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Copy { target, source } => {
                write!(f, "MOVE {source:?} -> {target:?}")
            }
            Self::IndirectRead {
                base,
                offset,
                output,
            } => {
                write!(
                    f,
                    "INDIRECT_READ base: {base:?}, offset: {offset:?} -> {output:?}"
                )
            }
            Self::IndirectWrite {
                base,
                offset,
                value,
            } => {
                write!(
                    f,
                    "INDIRECT_WRITE base: {base:?}, offset: {offset:?} <- {value:?}"
                )
            }
            Self::Call {
                function_id,
                parameters,
                output,
            } => {
                write!(f, "CALL {function_id:?}({parameters:?}) -> {output:?}")
            }
            Self::Input { target } => {
                write!(f, "INPUT -> {target:?}")
            }
            Self::Output { element } => {
                write!(f, "OUTPUT <- {element:?}")
            }
        }
    }
}

fn sanitize_string_literal(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '\n' => "\\n".to_string(),
            '\r' => "\\r".to_string(),
            '\t' => "\\t".to_string(),
            '\"' => "\\\"".to_string(),
            '\\' => "\\\\".to_string(),
            c if c.is_control() || !c.is_ascii() => format!("\\x{:02x}", c as u8),
            c => c.to_string(),
        })
        .collect()
}

impl IrInstruction {
    #[must_use]
    pub fn display_with_reg_desc(
        &self,
        registers: &HashMap<IrRegisterId, IrRegister>,
        function_name_map: &HashMap<FunctionId, String>,
    ) -> String {
        match self {
            Self::Copy { target, source } => {
                let target_desc = registers
                    .get(target)
                    .map_or_else(|| format!("{target:?}"), |r| r.description.clone());
                let source_desc = match source {
                    IrCopyOperand::Register(reg_id) => registers
                        .get(reg_id)
                        .map_or_else(|| format!("{reg_id:?}"), |r| r.description.clone()),
                    IrCopyOperand::IntLiteral(int) => format!("IntLiteral({int})"),
                    IrCopyOperand::StringLiteral(s) => {
                        format!("StringLiteral(\"{}\")", sanitize_string_literal(s))
                    }
                };
                format!("MOVE {source_desc} -> {target_desc}")
            }
            Self::IndirectRead {
                base,
                offset,
                output,
            } => {
                let base_desc = registers
                    .get(base)
                    .map_or_else(|| format!("{base:?}"), |r| r.description.clone());
                let offset_desc = registers
                    .get(offset)
                    .map_or_else(|| format!("{offset:?}"), |r| r.description.clone());
                let output_desc = registers
                    .get(output)
                    .map_or_else(|| format!("{output:?}"), |r| r.description.clone());
                format!("INDIRECT_READ base: {base_desc}, offset: {offset_desc} -> {output_desc}")
            }
            Self::IndirectWrite {
                base,
                offset,
                value,
            } => {
                let base_desc = registers
                    .get(base)
                    .map_or_else(|| format!("{base:?}"), |r| r.description.clone());
                let offset_desc = registers
                    .get(offset)
                    .map_or_else(|| format!("{offset:?}"), |r| r.description.clone());
                let value_desc = registers
                    .get(value)
                    .map_or_else(|| format!("{value:?}"), |r| r.description.clone());
                format!("INDIRECT_WRITE base: {base_desc}, offset: {offset_desc} <- {value_desc}")
            }
            Self::Call {
                function_id,
                parameters,
                output,
            } => {
                let param_descs: Vec<String> = parameters
                    .iter()
                    .map(|reg_id| {
                        registers
                            .get(reg_id)
                            .map_or_else(|| format!("{reg_id:?}"), |r| r.description.clone())
                    })
                    .collect();
                let output_desc = registers
                    .get(output)
                    .map_or_else(|| format!("{output:?}"), |r| r.description.clone());
                format!(
                    "CALL {:?}({}) -> {}",
                    function_name_map
                        .get(function_id)
                        .cloned()
                        .unwrap_or_else(|| format!("Function {function_id}")),
                    param_descs.join(", "),
                    output_desc
                )
            }
            Self::Input { target } => {
                let target_desc = registers
                    .get(target)
                    .map_or_else(|| format!("{target:?}"), |r| r.description.clone());
                format!("INPUT -> {target_desc}")
            }
            Self::Output { element } => {
                let element_desc = registers
                    .get(element)
                    .map_or_else(|| format!("{element:?}"), |r| r.description.clone());
                format!("OUTPUT <- {element_desc}")
            }
        }
    }
}
