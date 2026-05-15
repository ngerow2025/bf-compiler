//this will work in terms of bytes and distinguish copy/move
//this will also do register allocation for actual tape layout

use std::{collections::HashMap, fmt::Display, vec};

use crate::ir2_memory_representation::{PhysicalLocation, PhysicalLocationAllocator, PhysicalSlot};
use crate::parser::FunctionId;
use crate::{
    ir::{IrCopyOperand, IrFunction, IrInstruction, IrRegisterId},
    parser::IntLiteral,
};
//do instruction level dependency tracking here so that we can reorder instructions to reduce memory usage

fn get_instruction_dependencies(instr: &IrInstruction) -> Vec<IrRegisterId> {
    match instr {
        IrInstruction::Copy { target: _, source } => match source {
            IrCopyOperand::Register(src_reg) => vec![*src_reg],
            _ => vec![],
        },
        IrInstruction::IndirectRead {
            base,
            offset,
            output: _,
        } => {
            vec![*base, *offset]
        }
        IrInstruction::IndirectWrite {
            base: _,
            offset,
            value,
        } => {
            vec![*offset, *value]
        }
        IrInstruction::Call {
            function_id: _,
            parameters,
            output: _,
        } => parameters.clone(),
        IrInstruction::Input { .. } => {
            vec![]
        }
        IrInstruction::Output { element } => {
            vec![*element]
        }
    }
}

fn get_instruction_dependents(instr: &IrInstruction) -> Vec<IrRegisterId> {
    match instr {
        IrInstruction::Copy { target, source: _ } => vec![*target],
        IrInstruction::IndirectRead {
            base: _,
            offset: _,
            output,
        } => {
            vec![*output]
        }
        IrInstruction::IndirectWrite {
            base,
            offset: _,
            value: _,
        } => {
            vec![*base]
        }
        IrInstruction::Call {
            function_id: _,
            parameters: _,
            output,
        } => {
            vec![*output]
        }
        IrInstruction::Input { target } => {
            vec![*target]
        }
        IrInstruction::Output { element: _ } => {
            vec![]
        }
    }
}

enum RegisterLifetimeEventKind {
    Read,
    Write,
}

struct RegisterLifetimeEvent {
    kind: RegisterLifetimeEventKind,
    instruction_index: usize,
}

fn get_function_register_lifetimes(
    function: &IrFunction,
) -> HashMap<IrRegisterId, Vec<RegisterLifetimeEvent>> {
    //this will track the usages of every single register used in the function
    let mut register_useages = HashMap::new();
    for (index, instruction) in function.code.iter().enumerate() {
        let dependencies = get_instruction_dependencies(instruction);
        let dependents = get_instruction_dependents(instruction);
        for dep in dependencies {
            //mark a read event for this register
            let events = register_useages.entry(dep).or_insert(vec![]);
            events.push(RegisterLifetimeEvent {
                kind: RegisterLifetimeEventKind::Read,
                instruction_index: index,
            });
        }
        for dep in dependents {
            //mark a write event for this register
            let events = register_useages.entry(dep).or_insert(vec![]);
            events.push(RegisterLifetimeEvent {
                kind: RegisterLifetimeEventKind::Write,
                instruction_index: index,
            });
        }
    }
    register_useages
}

pub fn print_instruction_with_lifetime_annotations(function: &IrFunction) {
    let register_usages = get_function_register_lifetimes(function);
    let max_instruction_length = function
        .code
        .iter()
        .map(|instr| format!("{}", instr).len())
        .max()
        .unwrap_or(0);
    let general_lifetimes: HashMap<IrRegisterId, (usize, usize)> = register_usages
        .iter()
        .map(|(reg_id, events)| {
            let first_use = events.iter().map(|e| e.instruction_index).min().unwrap();
            let last_use = events.iter().map(|e| e.instruction_index).max().unwrap();
            (*reg_id, (first_use, last_use))
        })
        .collect();

    //start the main print loop
    for (index, instruction) in function.code.iter().enumerate() {
        //print out the actual instruction
        let instr_str = format!("{}", instruction);
        print!("{:width$}", instr_str, width = max_instruction_length);

        //now add additional padding
        print!("   ");

        //now print out a | for each register that is alive at this point
        //print a W for each register that is written to at this point
        //write a R for each register that is read at this point
        for reg_id in function.registers.keys() {
            let (first_use, last_use) = general_lifetimes.get(reg_id).unwrap();

            if index < *first_use || index > *last_use {
                //not alive at this point
                print!(" ");
            } else {
                //alive at this point
                let events = &register_usages[reg_id];
                let mut event_str = String::new();
                for event in events {
                    if event.instruction_index == index {
                        match event.kind {
                            RegisterLifetimeEventKind::Read => event_str.push('R'),
                            RegisterLifetimeEventKind::Write => event_str.push('W'),
                        }
                    }
                }
                if event_str.len() == 2 {
                    event_str = "X".to_string();
                }
                if event_str.is_empty() {
                    event_str.push('|');
                }
                print!("{:}", event_str);
            }
        }
        println!();
    }
}

//new instructions for IR2:
//clear: clears a register to zero
//move: sets the source register to 0, sets the target to the value that was held in source
//Nmove: sets the source register to 0, sets N different targets to the value that was held in source
//indirect read: reads from memory at base + offset into target register, also annotated with the maxiumum offset that can be read
//indirect write: writes to memory at base + offset from source register, also annotated with the maximum offset that can be written
//call: calls a function with a list of parameter registers, writes output to output register
//    the inputs will be set to zero by the callee
//    the output register must be set to zero before the call
//input: reads a byte from input into the target register, the target register is allowed to be non-zero
//output: writes a byte from the element register to output

struct Ir2Register {
    location: PhysicalSlot,
    description: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ir2Instruction {
    Clear {
        target: PhysicalSlot,
    },
    Init {
        target: PhysicalSlot,
        values: Vec<u8>,
    },
    Move {
        target: PhysicalSlot,
        source: PhysicalSlot,
    },
    NMove {
        targets: Vec<PhysicalSlot>,
        source: PhysicalSlot,
    },
    MoveFromIndirect {
        base: PhysicalLocation,
        offset: PhysicalSlot,
        output: PhysicalSlot,
    },
    MoveFromIndirectConstant {
        base: PhysicalLocation,
        offset: usize,
        output: PhysicalSlot,
    },
    MoveToIndirect {
        base: PhysicalLocation,
        offset: PhysicalSlot,
        value: PhysicalSlot,
    },
    MoveToIndirectConstant {
        base: PhysicalLocation,
        offset: usize,
        value: PhysicalSlot,
    },
    ClearIndirect {
        base: PhysicalLocation,
        offset: PhysicalSlot,
    },
    ClearIndirectConstant {
        base: PhysicalLocation,
        offset: usize,
    },
    ClearIndirectArray {
        base: PhysicalLocation,
        offset: PhysicalSlot,
        element_size: usize,
    },
    ClearIndirectArrayConstant {
        base: PhysicalLocation,
        offset: usize,
        element_size: usize,
    },
    Call {
        function_id: FunctionId,
        new_stack_frame_base: PhysicalLocation,
    },
    Input {
        target: PhysicalSlot,
    },
    Output {
        element: PhysicalSlot,
    },
}

#[derive(Clone, Debug)]
pub struct Ir2Function {
    pub code: Vec<Ir2Instruction>,
    pub metadata: Ir2FunctionMetadata,
    pub id: FunctionId,
    pub physical_register_mapping: HashMap<IrRegisterId, PhysicalSlot>,
}

#[derive(Debug, Clone)]
pub struct Ir2FunctionMetadata {
    pub parameter_layout: Vec<PhysicalSlot>,
    // output_location: PhysicalLocation,
}

//TODO: consider if we should return the original registers and the mapping to physical locations
fn lower_ir_function(
    function: &IrFunction,
    global_function_metadata: &HashMap<FunctionId, Ir2FunctionMetadata>,
) -> Ir2Function {
    //first determine the stack layout/mapping from IrRegisters to PhysicalLocations
    let mut physical_space_allocator = PhysicalLocationAllocator::new();
    let mut register_mapping = HashMap::new();

    let this_metadata = global_function_metadata
        .get(&function.id)
        .expect("expected valid function metadata");

    for (physical_location, param) in this_metadata
        .parameter_layout
        .iter()
        .zip(function.parameters.iter())
    {
        register_mapping.insert(param.register, *physical_location);
    }

    //TODO: set the output register location
    // register_mapping.insert(&function.output, this_metadata.output_location);

    for (idx, slot) in this_metadata.parameter_layout.iter().enumerate() {
        physical_space_allocator.register_allocation(
            slot.clone(),
            format!("param%{}%", function.parameters[idx].name),
        );
    }

    for (register_id, register) in &function.registers {
        if !register_mapping.contains_key(register_id) {
            register_mapping.insert(
                *register_id,
                physical_space_allocator.allocate(
                    register.size,
                    format!("from_ir_reg({})", register.description),
                ),
            );
        }
    }

    let mut new_code = function
        .code
        .iter()
        .flat_map(|old_instruction| {
            convert_instruction(
                function,
                global_function_metadata,
                &mut physical_space_allocator,
                &register_mapping,
                old_instruction,
            )
        })
        .collect::<Vec<_>>();

    //emit all the instructions needed to clear all of the used memory locations at the end of the function
    //TODO: be smarter about this
    //TODO: this is where return value handling will go later

    for (allocation, _) in physical_space_allocator.get_all_allocations() {
        new_code.push(Ir2Instruction::Clear { target: allocation });
    }

    Ir2Function {
        metadata: this_metadata.clone(),
        physical_register_mapping: register_mapping,
        code: new_code,
        id: function.id,
    }
}

fn convert_instruction(
    function: &IrFunction,
    global_function_metadata: &HashMap<FunctionId, Ir2FunctionMetadata>,
    physical_space_allocator: &mut PhysicalLocationAllocator<String>,
    register_mapping: &HashMap<IrRegisterId, PhysicalSlot>,
    old_instruction: &IrInstruction,
) -> Vec<Ir2Instruction> {
    match old_instruction {
        IrInstruction::Copy { target, source } => {
            let target_location = register_mapping
                .get(target)
                .expect("expected valid mapping");
            match source {
                IrCopyOperand::IntLiteral(x) => match x {
                    IntLiteral::U8(val) => {
                        vec![Ir2Instruction::Init {
                            target: *target_location,
                            values: vec![*val],
                        }]
                    }
                    IntLiteral::I8(val) => {
                        vec![Ir2Instruction::Init {
                            target: *target_location,
                            values: (*val).to_be_bytes().to_vec(),
                        }]
                    }
                    IntLiteral::U16(val) => {
                        let raw_bytes = val.to_be_bytes();
                        vec![Ir2Instruction::Init {
                            target: *target_location,
                            values: raw_bytes.to_vec(),
                        }]
                    }
                    IntLiteral::I16(val) => {
                        let raw_bytes = val.to_be_bytes();
                        vec![Ir2Instruction::Init {
                            target: *target_location,
                            values: raw_bytes.to_vec(),
                        }]
                    }
                    IntLiteral::U32(val) => {
                        let raw_bytes = val.to_be_bytes();
                        vec![Ir2Instruction::Init {
                            target: *target_location,
                            values: raw_bytes.to_vec(),
                        }]
                    }
                    IntLiteral::I32(val) => {
                        let raw_bytes = val.to_be_bytes();
                        vec![Ir2Instruction::Init {
                            target: *target_location,
                            values: raw_bytes.to_vec(),
                        }]
                    }
                    IntLiteral::U64(val) => {
                        let raw_bytes = val.to_be_bytes();
                        vec![Ir2Instruction::Init {
                            target: *target_location,
                            values: raw_bytes.to_vec(),
                        }]
                    }
                    IntLiteral::I64(val) => {
                        let raw_bytes = val.to_be_bytes();
                        vec![Ir2Instruction::Init {
                            target: *target_location,
                            values: raw_bytes.to_vec(),
                        }]
                    }
                },
                IrCopyOperand::StringLiteral(val) => {
                    let bytes = val.as_bytes();
                    vec![Ir2Instruction::Init {
                        target: *target_location,
                        values: bytes.to_vec(),
                    }]
                }
                IrCopyOperand::Register(reg_id) => {
                    //this is actually considered a copy to be safe, further analysis needs to be done to determine if this can be a move
                    //create a temporary register for the copy operation
                    let reg = register_mapping
                        .get(reg_id)
                        .expect("expected valid register mapping");
                    let reg_size = function
                        .registers
                        .get(reg_id)
                        .expect("expected valid register")
                        .size;

                    let copy_tmp = physical_space_allocator.allocate(
                        reg_size,
                        format!(
                            "copy_tmp({} -> {})",
                            function.registers.get(reg_id).unwrap().description,
                            function.registers.get(target).unwrap().description
                        ),
                    );

                    vec![
                        Ir2Instruction::Move {
                            target: copy_tmp,
                            source: *reg,
                        },
                        Ir2Instruction::NMove {
                            targets: vec![*target_location, *reg],
                            source: copy_tmp,
                        },
                    ]
                }
            }
        }
        IrInstruction::Input { target } => {
            vec![Ir2Instruction::Input {
                target: *register_mapping
                    .get(target)
                    .expect("should have allocated physical space for every register"),
            }]
        }
        IrInstruction::Output { element } => {
            vec![Ir2Instruction::Output {
                element: *register_mapping
                    .get(element)
                    .expect("should have allocated physical space for every register"),
            }]
        }
        IrInstruction::Call {
            function_id,
            parameters,
            output: _,
        } => {
            let function_metadata = global_function_metadata
                .get(function_id)
                .expect("expected valid function metadata");

            let tmp_param_locations: Vec<PhysicalSlot> = parameters
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    let reg = function
                        .registers
                        .get(param)
                        .expect("expected that param had proper register allocated");
                    let size = reg.size;
                    physical_space_allocator
                        .allocate(size, format!("call_param_copy_tmp(param {})", i))
                })
                .collect();

            let new_stack_frame_base = physical_space_allocator.free_space_start() + 3;

            let param_locations: Vec<PhysicalSlot> = function_metadata
                .parameter_layout
                .iter()
                .map(|param_slot| param_slot.clone() + new_stack_frame_base)
                .collect();

            let mut call_setup_code = vec![];

            for (i, param) in parameters.iter().enumerate() {

                let param_physical_location = register_mapping
                    .get(param)
                    .expect("expected valid register mapping");

                //we need to copy the values into the places specified by the function input layout starting at the new stack frame base
                call_setup_code.extend(vec![
                    Ir2Instruction::Move {
                        target: tmp_param_locations[i],
                        source: *param_physical_location,
                    },
                    Ir2Instruction::NMove {
                        targets: vec![param_locations[i], *param_physical_location],
                        source: tmp_param_locations[i],
                    },
                ])
            }

            let call_instruction = Ir2Instruction::Call {
                function_id: *function_id,
                new_stack_frame_base,
            };

            let call_cleanup_code = vec![]; //TODO: this will handle the output in the future

            let mut full_call_code = vec![];
            full_call_code.extend(call_setup_code);
            full_call_code.push(call_instruction);
            full_call_code.extend(call_cleanup_code);
            full_call_code
        }
        IrInstruction::IndirectRead {
            base,
            offset,
            output,
        } => {
            let base_location = register_mapping
                .get(base)
                .expect("expected valid register mapping");
            let offset_location = register_mapping
                .get(offset)
                .expect("expected valid register mapping");
            let output_location = register_mapping
                .get(output)
                .expect("expected valid register mapping");
            let output_size = function
                .registers
                .get(output)
                .expect("expected valid register")
                .size;

            let tmp_offset_location = physical_space_allocator.allocate(
                function
                    .registers
                    .get(offset)
                    .expect("expected valid register")
                    .size,
                format!(
                    "indirect_read_offset_tmp({})",
                    function.registers.get(offset).unwrap().description
                ),
            );
            let tmp_output_location_1 = physical_space_allocator.allocate(
                output_size,
                format!(
                    "indirect_read_output_tmp1({})",
                    function.registers.get(output).unwrap().description
                ),
            );
            let tmp_output_location_2 = physical_space_allocator.allocate(
                output_size,
                format!(
                    "indirect_read_output_tmp2({})",
                    function.registers.get(output).unwrap().description
                ),
            );

            let usage_offset_location = physical_space_allocator.allocate(
                function
                    .registers
                    .get(offset)
                    .expect("expected valid register")
                    .size,
                format!(
                    "indirect_read_usage_offset_tmp({})",
                    function.registers.get(offset).unwrap().description
                ),
            );

            //move offset -> tmp_offset, usage_offset
            //indirect read base, usage_offset -> tmp_output
            //move tmp_output -> output, tmp_output_2
            //move tmp_offset -> offset, usage_offset
            //indirect write base, usage_offset, tmp_output_2

            vec![
                Ir2Instruction::NMove {
                    source: *offset_location,
                    targets: vec![tmp_offset_location, usage_offset_location],
                },
                Ir2Instruction::MoveFromIndirect {
                    base: base_location.get_start(),
                    offset: usage_offset_location,
                    output: tmp_output_location_1,
                },
                Ir2Instruction::NMove {
                    source: tmp_output_location_1,
                    targets: vec![*output_location, tmp_output_location_2],
                },
                Ir2Instruction::NMove {
                    source: tmp_offset_location,
                    targets: vec![*offset_location, usage_offset_location],
                },
                Ir2Instruction::MoveToIndirect {
                    base: base_location.get_start(),
                    offset: usage_offset_location,
                    value: tmp_output_location_2,
                },
            ]
        }
        IrInstruction::IndirectWrite {
            base,
            offset,
            value,
        } => {
            //first do an indirect clear to zero out the target location
            //then do a similar thing to the indirect read to write the value back
            //make sure to copy all of the parameters used in the indirect clear and indirect write

            let mut code_sequence = vec![];

            let base_location = register_mapping
                .get(base)
                .expect("expected valid register mapping");
            let offset_location = register_mapping
                .get(offset)
                .expect("expected valid register mapping");
            let value_location = register_mapping
                .get(value)
                .expect("expected valid register mapping");

            let tmp_offset_location = physical_space_allocator.allocate(
                function
                    .registers
                    .get(offset)
                    .expect("expected valid register")
                    .size,
                format!(
                    "indirect_write_offset_tmp({})",
                    function.registers.get(offset).unwrap().description
                ),
            );
            let usage_offset_location = physical_space_allocator.allocate(
                function
                    .registers
                    .get(offset)
                    .expect("expected valid register")
                    .size,
                format!(
                    "indirect_write_usage_offset_tmp({})",
                    function.registers.get(offset).unwrap().description
                ),
            );
            let tmp_value_location = physical_space_allocator.allocate(
                value_location.get_size(),
                format!(
                    "indirect_write_value_tmp({})",
                    function.registers.get(value).unwrap().description
                ),
            );
            let usage_value_location = physical_space_allocator.allocate(
                value_location.get_size(),
                format!(
                    "indirect_write_usage_value_tmp({})",
                    function.registers.get(value).unwrap().description
                ),
            );

            //move offset -> tmp_offset, usage_offset
            //indirect clear base, usage_offset
            //move value -> tmp_value, usage_value
            //move tmp_offset -> offset, usage_offset
            //indirect write base, usage_offset, usage_value
            //move tmp_value -> value

            code_sequence.push(Ir2Instruction::NMove {
                source: *offset_location,
                targets: vec![tmp_offset_location, usage_offset_location],
            });

            code_sequence.push(Ir2Instruction::ClearIndirect {
                base: base_location.get_start(),
                offset: usage_offset_location,
            });

            code_sequence.push(Ir2Instruction::NMove {
                source: *value_location,
                targets: vec![tmp_value_location, usage_value_location],
            });

            code_sequence.push(Ir2Instruction::NMove {
                source: tmp_offset_location,
                targets: vec![*offset_location, usage_offset_location],
            });

            code_sequence.push(Ir2Instruction::MoveToIndirect {
                base: base_location.get_start(),
                offset: usage_offset_location,
                value: usage_value_location,
            });

            code_sequence.push(Ir2Instruction::Move {
                target: *value_location,
                source: tmp_value_location,
            });

            code_sequence
        }
    }
}

pub fn generate_ir2(ir_functions: &Vec<IrFunction>) -> HashMap<FunctionId, Ir2Function> {
    //first generate function metadata for all functions
    let mut global_function_metadata = HashMap::new();
    for function in ir_functions {
        let mut physical_space_allocator = PhysicalLocationAllocator::new();

        let parameter_layout: Vec<PhysicalSlot> = function
            .parameters
            .iter()
            .map(|param| {
                let reg = function
                    .registers
                    .get(&param.register)
                    .expect("expected that param had proper register allocated");
                physical_space_allocator.allocate(reg.size, ())
            })
            .collect();
        // let output_reg = function
        //     .registers
        //     .get(&function.output)
        //     .expect("expected that output had proper register allocated");
        // let output_location = PhysicalLocation(current_offset);
        // current_offset += output_reg.size;

        let metadata = Ir2FunctionMetadata {
            parameter_layout,
            // output_location,
        };
        global_function_metadata.insert(function.id, metadata);
    }

    //then lower each function
    let mut lowered_functions = HashMap::new();
    for function in ir_functions {
        let lowered_function = lower_ir_function(function, &global_function_metadata);
        lowered_functions.insert(function.id, lowered_function);
    }

    lowered_functions
}

impl Display for Ir2Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ir2Instruction::Clear { target } => {
                write!(f, "CLEAR [SP + {}]", target)
            }
            Ir2Instruction::Init {
                target,
                values: value,
            } => {
                write!(f, "INIT [SP + {}] = {:?}", target, value)
            }
            Ir2Instruction::Move { target, source } => {
                write!(f, "MOVE [SP + {}] <- [SP + {}]", target, source)
            }
            Ir2Instruction::NMove { targets, source } => {
                let target_strs: Vec<String> =
                    targets.iter().map(|t| format!("[SP + {}]", t)).collect();
                write!(f, "NMOVE {} <- [SP + {}]", target_strs.join(", "), source)
            }
            Ir2Instruction::MoveFromIndirect {
                base,
                offset,
                output,
            } => {
                write!(
                    f,
                    "MOVE_FROM_INDIRECT [SP + {} + [SP + {}]] -> [SP + {}]",
                    base, offset, output
                )
            }
            Ir2Instruction::MoveFromIndirectConstant {
                base,
                offset,
                output,
            } => {
                write!(
                    f,
                    "MOVE_FROM_INDIRECT_CONST [SP + {} + {}] -> [SP + {}]",
                    base, offset, output
                )
            }
            Ir2Instruction::MoveToIndirect {
                base,
                offset,
                value,
            } => {
                write!(
                    f,
                    "MOVE_TO_INDIRECT [SP + {} + [SP + {}]] <- [SP + {}]",
                    base, offset, value
                )
            }
            Ir2Instruction::MoveToIndirectConstant {
                base,
                offset,
                value,
            } => {
                write!(
                    f,
                    "MOVE_TO_INDIRECT_CONST [SP + {} + {}] <- [SP + {}]",
                    base, offset, value
                )
            }
            Ir2Instruction::ClearIndirect { base, offset } => {
                write!(f, "CLEAR_INDIRECT [SP + {} + [SP + {}]]", base, offset)
            }
            Ir2Instruction::ClearIndirectConstant { base, offset } => {
                write!(f, "CLEAR_INDIRECT_CONST [SP + {} + {}]", base, offset)
            }
            Ir2Instruction::ClearIndirectArray {
                base,
                offset,
                element_size,
            } => {
                write!(
                    f,
                    "CLEAR_INDIRECT_CONST [SP + {} + {} * {}]",
                    base, offset, element_size
                )
            }
            Ir2Instruction::ClearIndirectArrayConstant {
                base,
                offset,
                element_size,
            } => {
                write!(
                    f,
                    "CLEAR_INDIRECT_ARRAY_CONST [SP + {} + {} * {}]",
                    base, offset, element_size
                )
            }
            Ir2Instruction::Input { target } => {
                write!(f, "INPUT [SP + {}]", target)
            }
            Ir2Instruction::Output { element } => {
                write!(f, "OUTPUT [SP + {}]", element)
            }
            Ir2Instruction::Call {
                function_id,
                new_stack_frame_base,
            } => {
                write!(
                    f,
                    "CALL FUNC_{} NEW_STACK_BASE [SP + {}]",
                    function_id, new_stack_frame_base
                )
            }
        }
    }
}

//TODO: write negitive tests that ensure that asserts trigger when they are supposed to
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ir::IrRegister, util::cartesian_product_iter};

    fn test_convert_ir(
        original_ir: Vec<IrInstruction>,
        register_mapping: HashMap<IrRegisterId, PhysicalSlot>,
        registers: HashMap<IrRegisterId, IrRegister>,
    ) -> Vec<Ir2Instruction> {
        let mut physical_space_allocator = PhysicalLocationAllocator::new();

        for (reg_id, physical_location) in &register_mapping {
            let reg = registers
                .get(reg_id)
                .expect("expected valid register for register mapping");
            assert_eq!(
                physical_location.get_size(),
                reg.size,
                "Invalid register mapping: physical location size does not match register size for register {}",
                reg.description
            );
            println!(
                "registering allocation for register {} at physical location {} with size {} and description '{}'",
                reg_id.0,
                physical_location.get_start(),
                reg.size,
                reg.description
            );
            physical_space_allocator
                .register_allocation(*physical_location, reg.description.clone());
        }

        //sanity check that all the allocated registers do not overlap in physical space
        for ((reg_id1, loc1), (reg_id2, loc2)) in
            cartesian_product_iter(&register_mapping.iter().collect())
        {
            let reg1 = registers
                .get(reg_id1)
                .expect("expected valid register for register mapping");
            let reg2 = registers
                .get(reg_id2)
                .expect("expected valid register for register mapping");

            assert!(
                !loc1.intersects(loc2),
                "physical locations for registers {} and {} overlap",
                reg1.description,
                reg2.description
            );
        }

        original_ir
            .iter()
            .flat_map(|old_instruction| {
                convert_instruction(
                    &IrFunction {
                        id: FunctionId(0),
                        code: vec![],
                        registers: registers.clone(),
                        parameters: vec![],
                        name: "test_function".to_string(),
                    },
                    &HashMap::new(),
                    &mut physical_space_allocator,
                    &register_mapping,
                    old_instruction,
                )
            })
            .collect::<Vec<_>>()
    }

    fn test_tape_output_size(tape: &Vec<u8>, expected_size: usize) {
        assert!(
            tape.len() >= expected_size,
            "Expected tape output size of at least {}, but got {}",
            expected_size,
            tape.len()
        );
        //if the tape is too long, make sure that the extra is just zeros
        if tape.len() > expected_size {
            let extra = &tape[expected_size..];
            let zero_slice = vec![0; extra.len()];
            assert_eq!(
                extra,
                zero_slice.as_slice(),
                "Expected extra tape output to be zeros"
            );
        }
    }

    mod init_int_literal {
        use crate::{
            ir::{IrCopyOperand, IrInstruction, IrRegister, IrRegisterId},
            ir2::{
                Ir2Instruction, PhysicalLocation,
                tests::{test_convert_ir, test_tape_output_size},
            },
            ir2_memory_representation::PhysicalSlot,
            ir2_runner::test_instructions,
            parser::IntLiteral,
        };
        use maplit::hashmap;

        fn test_ir2_init_int_literal(literal: IntLiteral, bytes: Vec<u8>) {
            println!("Testing literal {:?} with bytes {:?}", literal, bytes);

            let ir_code = vec![IrInstruction::Copy {
                target: IrRegisterId(0),
                source: IrCopyOperand::IntLiteral(literal),
            }];

            let register_mapping = hashmap! {
                IrRegisterId(0) => PhysicalSlot::from_start_size(PhysicalLocation::new(0), bytes.len()),
            };
            let registers = hashmap! {
                IrRegisterId(0) => IrRegister {
                    size: bytes.len(),
                    description: "target_register".to_string(),
                },
            };

            let ir2_code = test_convert_ir(ir_code, register_mapping, registers);

            let expected_ir2_code = vec![Ir2Instruction::Init {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), bytes.len()),
                values: bytes.clone(),
            }];

            assert_eq!(ir2_code, expected_ir2_code);

            let start_tape = vec![];
            let result_tape = test_instructions(ir2_code, start_tape);
            test_tape_output_size(&result_tape, bytes.len());
            assert_eq!(result_tape[0..bytes.len()], bytes[..]);
        }

        #[test]
        fn u8() {
            test_ir2_init_int_literal(IntLiteral::U8(42), (42u8).to_be_bytes().to_vec());
        }

        #[test]
        fn i8() {
            test_ir2_init_int_literal(IntLiteral::I8(-73), (-73i8).to_be_bytes().to_vec());
        }

        #[test]
        fn u16() {
            test_ir2_init_int_literal(IntLiteral::U16(0x1122), (0x1122u16).to_be_bytes().to_vec());
        }

        #[test]
        fn i16() {
            test_ir2_init_int_literal(
                IntLiteral::I16(-0x1234),
                (-0x1234i16).to_be_bytes().to_vec(),
            );
        }

        #[test]
        fn u32() {
            test_ir2_init_int_literal(
                IntLiteral::U32(0x11223344),
                (0x11223344u32).to_be_bytes().to_vec(),
            );
        }

        #[test]
        fn i32() {
            test_ir2_init_int_literal(
                IntLiteral::I32(-0x12345678),
                (-0x12345678i32).to_be_bytes().to_vec(),
            );
        }

        #[test]
        fn u64() {
            test_ir2_init_int_literal(
                IntLiteral::U64(0x1122334455667788),
                (0x1122334455667788u64).to_be_bytes().to_vec(),
            );
        }

        #[test]
        fn i64() {
            test_ir2_init_int_literal(
                IntLiteral::I64(-0x1234567890ABCDEF),
                (-0x1234567890ABCDEFi64).to_be_bytes().to_vec(),
            );
        }
    }

    mod init_string_literal {
        use crate::{
            ir::{IrCopyOperand, IrInstruction, IrRegister, IrRegisterId},
            ir2::{
                Ir2Instruction, PhysicalLocation,
                tests::{test_convert_ir, test_tape_output_size},
            },
            ir2_memory_representation::PhysicalSlot,
            ir2_runner::test_instructions,
        };
        use maplit::hashmap;

        fn test_ir2_init_string_literal(literal: &str, bytes: Vec<u8>) {
            let ir_code = vec![IrInstruction::Copy {
                target: IrRegisterId(0),
                source: IrCopyOperand::StringLiteral(literal.to_string()),
            }];

            let register_mapping = hashmap! {
                IrRegisterId(0) => PhysicalSlot::from_start_size(PhysicalLocation::new(0), bytes.len()),
            };
            let registers = hashmap! {
                IrRegisterId(0) => IrRegister {
                    size: bytes.len(),
                    description: "target_register".to_string(),
                },
            };

            let ir2_code = test_convert_ir(ir_code, register_mapping, registers);

            let expected_ir2_code = vec![Ir2Instruction::Init {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), bytes.len()),
                values: bytes.clone(),
            }];

            assert_eq!(ir2_code, expected_ir2_code);

            let start_tape = vec![];
            let result_tape = test_instructions(ir2_code, start_tape);
            test_tape_output_size(&result_tape, bytes.len());
            assert_eq!(result_tape[0..bytes.len()], bytes[..]);
        }

        #[test]
        fn ascii() {
            test_ir2_init_string_literal("hello", b"hello".to_vec());
        }

        #[test]
        fn zero_length() {
            test_ir2_init_string_literal("", vec![]);
        }

        #[test]
        fn special_characters() {
            let literal = "line1\nline2\t\\\"'";
            test_ir2_init_string_literal(literal, literal.as_bytes().to_vec());
        }

        #[test]
        fn null_character_only() {
            let literal = "\0";
            test_ir2_init_string_literal(literal, vec![0]);
        }

        #[test]
        fn embedded_null_character() {
            let literal = "ab\0cd";
            test_ir2_init_string_literal(literal, vec![b'a', b'b', 0, b'c', b'd']);
        }
    }

    mod reg_reg_move {
        use maplit::hashmap;

        use crate::{
            ir::{IrCopyOperand, IrInstruction, IrRegister, IrRegisterId},
            ir2::{
                PhysicalLocation,
                tests::{test_convert_ir, test_tape_output_size},
            },
            ir2_memory_representation::PhysicalSlot,
            ir2_runner::test_instructions,
        };

        fn test_ir2_reg_reg_move(reg_size: usize, data: Vec<u8>) {
            assert_eq!(
                reg_size,
                data.len(),
                "test data length must match register size"
            );

            // IrInstruction::Move { target: IrRegisterId(0), source: IrMoveOperand::Register(IrRegisterId(1)) };
            let ir_code = vec![IrInstruction::Copy {
                target: IrRegisterId(1),
                source: IrCopyOperand::Register(IrRegisterId(0)),
            }];

            let register_mapping = hashmap! {
                IrRegisterId(0) => PhysicalSlot::from_start_size(PhysicalLocation::new(0), reg_size),
                IrRegisterId(1) => PhysicalSlot::from_start_size(PhysicalLocation::new(reg_size), reg_size),
            };

            let registers = hashmap! {
                IrRegisterId(0) => IrRegister {
                    size: reg_size,
                    description: "source".to_string(),
                },
                IrRegisterId(1) => IrRegister {
                    size: reg_size,
                    description: "target".to_string(),
                },
            };

            let ir2_code = test_convert_ir(ir_code, register_mapping, registers);

            println!("Generated IR2 code:");
            for instr in &ir2_code {
                println!("{}", instr);
            }
            println!();

            let mut start_tape = vec![];
            //set the beggining of the tape to data
            start_tape.extend(data.clone());

            let result_tape = test_instructions(ir2_code, start_tape);

            test_tape_output_size(&result_tape, reg_size * 2);

            println!("Result tape: {:?}", result_tape);

            //the source should be zeroed out
            assert_eq!(
                result_tape[0..reg_size],
                data[..],
                "source register should be unchanged"
            );

            //the target should now contain the data
            assert_eq!(
                result_tape[reg_size..reg_size * 2],
                data[..],
                "target register should contain the data"
            );
        }

        #[test]
        fn single_byte() {
            test_ir2_reg_reg_move(1, vec![42]);
        }

        #[test]
        fn word() {
            test_ir2_reg_reg_move(2, vec![0x12, 0x34]);
        }
    }
}
