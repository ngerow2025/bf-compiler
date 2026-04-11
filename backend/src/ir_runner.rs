use std::{
    collections::HashMap,
    io::{self, Write},
    vec,
};

use crate::{
    bump_allocator::BumpAllocator,
    input_grabbing::{RawModeGuard, read_single_byte},
    ir::{IrFunction, IrInstruction, IrMoveOperand},
    parser::{FunctionId, IntLiteral},
};

struct IrGlobalContext<'a> {
    //holds all functions
    pub functions: &'a Vec<IrFunction>,
    pub function_name_mapping: HashMap<FunctionId, String>,
}

pub fn run_ir(ir_functions: &Vec<IrFunction>, function_name_mapping: HashMap<FunctionId, String>) {
    let global_context = IrGlobalContext {
        functions: ir_functions,
        function_name_mapping,
    };

    let mut main_function_id = None;
    for (id, name) in &global_context.function_name_mapping {
        if name == "main" {
            main_function_id = Some(*id);
            break;
        }
    }

    if let Some(main_id) = main_function_id {
        let _raw_input_guard = RawModeGuard::new();
        run_function(main_id, &global_context, vec![]);
    } else {
        panic!("No main function found in function name mapping");
    }
}

fn run_function(
    function_id: FunctionId,
    global_context: &IrGlobalContext,
    parameters: Vec<Vec<u8>>,
) {
    let function = global_context
        .functions
        .iter()
        .find(|f| f.id == function_id)
        .expect("Function ID not found in global context");

    let total_size = function.registers.values().map(|reg| reg.size).sum();
    let mut local_variables = BumpAllocator::new(total_size);
    for (var_id, reg) in &function.registers {
        local_variables
            .allocate(reg.size, var_id)
            .expect("exact amount of capacity should have been alocated but allocate call failed");
    }

    for (i, param) in function.parameters.iter().enumerate() {
        local_variables
            .set_memory_contents(param, &parameters[i])
            .expect("failed to set parameter memory contents");
    }

    for instruction in &function.code {
        match instruction {
            IrInstruction::Move { target, source } => {
                match source {
                    IrMoveOperand::Register(source_reg) => {
                        //this is basically a memcpy from source_reg to target
                        local_variables
                            .copy(source_reg, target)
                            .expect("Source register not found in local variables");
                    }
                    IrMoveOperand::IntLiteral(int_literal) => {
                        let raw_information = match int_literal {
                            IntLiteral::I64(i) => i.to_le_bytes().to_vec(),
                            IntLiteral::U64(u) => u.to_le_bytes().to_vec(),
                            IntLiteral::I32(i) => i.to_le_bytes().to_vec(),
                            IntLiteral::U32(u) => u.to_le_bytes().to_vec(),
                            IntLiteral::I16(i) => i.to_le_bytes().to_vec(),
                            IntLiteral::U16(u) => u.to_le_bytes().to_vec(),
                            IntLiteral::I8(i) => i.to_le_bytes().to_vec(),
                            IntLiteral::U8(u) => u.to_le_bytes().to_vec(),
                        };
                        local_variables
                            .set_memory_contents(target, &raw_information)
                            .expect("Failed to set memory contents for integer literal");
                    }
                    IrMoveOperand::StringLiteral(string_literal) => {
                        local_variables
                            .set_memory_contents(target, string_literal.as_bytes())
                            .expect("Failed to set memory contents for string literal");
                    }
                }
            }
            IrInstruction::Input { target } => {
                let input_byte = read_single_byte();
                local_variables
                    .set_memory_contents(target, &[input_byte])
                    .expect("Failed to set memory contents for input instruction");
            }
            IrInstruction::Output { element } => {
                let element_mem = local_variables
                    .get_memory_contents(element)
                    .expect("Failed to get memory contents for output instruction");
                print!("{}", String::from_utf8_lossy(element_mem.as_slice()));
                io::stdout().flush().expect("failed to flush stdout");
            }
            IrInstruction::IndirectRead {
                base,
                offset,
                output,
            } => {
                let offset_value = local_variables
                    .get_memory_contents(offset)
                    .expect("Failed to get memory contents for offset");
                assert!(
                    offset_value.len() == 1,
                    "Offset register must be 1 byte in size for indirect read"
                );
                let read_value = local_variables
                    .read_offset(base, offset_value[0] as usize)
                    .expect("Failed to read from memory in indirect read instruction");
                local_variables.set_memory_contents(output, &[read_value]).expect("Failed to set memory contents for output register in indirect read instruction");
            }
            IrInstruction::IndirectWrite {
                base,
                offset,
                value,
            } => {
                let offset_value = local_variables
                    .get_memory_contents(offset)
                    .expect("Failed to get memory contents for offset");
                assert!(
                    offset_value.len() == 1,
                    "Offset register must be 1 byte in size for indirect write"
                );
                let value_to_write = local_variables
                    .get_memory_contents(value)
                    .expect("Failed to get memory contents for value");
                assert!(
                    value_to_write.len() == 1,
                    "Value register must be 1 byte in size for indirect write"
                );
                local_variables
                    .write_offset(base, offset_value[0] as usize, value_to_write[0])
                    .expect("Failed to write to memory in indirect write instruction");
            }
            IrInstruction::Call {
                function_id,
                parameters,
                output: _,
            } => {
                let parameter_values: Vec<Vec<u8>> = parameters
                    .iter()
                    .map(|param| {
                        local_variables
                            .get_memory_contents(param)
                            .expect("Failed to get memory contents for function call parameter")
                    })
                    .collect();
                run_function(*function_id, global_context, parameter_values);
            }
        }
    }
}
