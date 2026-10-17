use std::{collections::HashMap, hash::BuildHasher};

#[cfg(feature = "advanced-output")]
use std::{
    io::{self, Write},
    vec,
};

use crate::{
    bump_allocator::BumpAllocator,
    ir::{IrCopyOperand, IrFunction, IrInstruction},
    parser::{FunctionId, IntLiteral},
};

#[cfg(feature = "advanced-output")]
use crate::input_grabbing::{RawModeGuard, read_single_byte};

struct IrGlobalContext<'a, H: BuildHasher> {
    //holds all functions
    pub functions: &'a Vec<IrFunction>,
    pub function_name_mapping: HashMap<FunctionId, String, H>,
}

trait InputProvider: Fn() -> Option<u8> {}
impl<T: Fn() -> Option<u8>> InputProvider for T {}

trait OutputConsumer: Fn(u8) {}
impl<T: Fn(u8)> OutputConsumer for T {}

struct IrRuntimeContext<'a, I: InputProvider, O: OutputConsumer, H: BuildHasher> {
    global_context: &'a IrGlobalContext<'a, H>,
    input_provider: I,
    output_consumer: O,
}

pub fn run_ir<InputProvider: Fn() -> Option<u8>, OutputConsumer: Fn(u8), H: BuildHasher>(
    ir_functions: &Vec<IrFunction>,
    function_name_mapping: HashMap<FunctionId, String, H>,
    input_provider: InputProvider,
    output_consumer: OutputConsumer,
) {
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
        let runtime_context = IrRuntimeContext {
            global_context: &global_context,
            input_provider,
            output_consumer,
        };
        run_function(main_id, &runtime_context, &[]);
    } else {
        panic!("No main function found in function name mapping");
    }
}

#[cfg(feature = "advanced-output")]
pub fn run_ir_console(
    ir_functions: &Vec<IrFunction>,
    function_name_mapping: HashMap<FunctionId, String>,
) {
    let _raw_input_guard = RawModeGuard::new();
    run_ir(
        ir_functions,
        function_name_mapping,
        || Some(read_single_byte()),
        |b: u8| {
            print!("{}", b as char);
            io::stdout().flush().expect("failed to flush stdout");
        },
    );
}

fn run_function<I: InputProvider, O: OutputConsumer, H: BuildHasher>(
    function_id: FunctionId,
    runtime_context: &IrRuntimeContext<I, O, H>,
    parameters: &[Vec<u8>],
) {
    let global_context = runtime_context.global_context;
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

    for (param, arg) in function.parameters.iter().zip(parameters.iter()) {
        local_variables
            .set_memory_contents(&param.register, arg)
            .expect("failed to set parameter memory contents");
    }

    for instruction in &function.code {
        match instruction {
            IrInstruction::Copy { target, source } => {
                match source {
                    IrCopyOperand::Register(source_reg) => {
                        //this is basically a memcpy from source_reg to target
                        local_variables
                            .copy(source_reg, target)
                            .expect("Source register not found in local variables");
                    }
                    IrCopyOperand::IntLiteral(int_literal) => {
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
                    IrCopyOperand::StringLiteral(string_literal) => {
                        local_variables
                            .set_memory_contents(target, string_literal.as_bytes())
                            .expect("Failed to set memory contents for string literal");
                    }
                }
            }
            IrInstruction::Input { target } => {
                let input_byte = (runtime_context.input_provider)().unwrap_or(0);
                local_variables
                    .set_memory_contents(target, &[input_byte])
                    .expect("Failed to set memory contents for input instruction");
            }
            IrInstruction::Output { element } => {
                let element_mem = local_variables
                    .get_memory_contents(element)
                    .expect("Failed to get memory contents for output instruction");
                for &byte in &element_mem {
                    (runtime_context.output_consumer)(byte);
                }
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
                    .read_offset(
                        base,
                        *offset_value
                            .first()
                            .expect("Offset register must contain one byte")
                            as usize,
                    )
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
                    .write_offset(
                        base,
                        *offset_value
                            .first()
                            .expect("Offset register must contain one byte")
                            as usize,
                        *value_to_write
                            .first()
                            .expect("Value register must contain one byte"),
                    )
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
                run_function(*function_id, runtime_context, &parameter_values);
            }
        }
    }
}
