//this will work in terms of bytes and distinguish copy/move
//this will also do register allocation for actual tape layout

use std::{collections::HashMap, fmt::Display, vec};

use crate::{
    ir::{IrFunction, IrInstruction, IrMoveOperand, IrRegisterId},
    parser::IntLiteral,
};
use crate::parser::FunctionId;
//do instruction level dependency tracking here so that we can reorder instructions to reduce memory usage

fn get_instruction_dependencies(instr: &IrInstruction) -> Vec<IrRegisterId> {
    match instr {
        IrInstruction::Move { target, source } => match source {
            IrMoveOperand::Register(src_reg) => vec![*src_reg],
            _ => vec![],
        },
        IrInstruction::IndirectRead {
            base,
            offset,
            output,
        } => {
            vec![*base, *offset]
        }
        IrInstruction::IndirectWrite {
            base,
            offset,
            value,
        } => {
            vec![*offset, *value]
        }
        IrInstruction::Call {
            function_id,
            parameters,
            output,
        } => parameters.clone(),
        IrInstruction::Input { target } => {
            vec![]
        }
        IrInstruction::Output { element } => {
            vec![*element]
        }
    }
}

fn get_instruction_dependents(instr: &IrInstruction) -> Vec<IrRegisterId> {
    match instr {
        IrInstruction::Move { target, source: _ } => vec![*target],
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
    let mut registerUseages = HashMap::new();
    for (index, instruction) in function.code.iter().enumerate() {
        let dependencies = get_instruction_dependencies(instruction);
        let dependents = get_instruction_dependents(instruction);
        for dep in dependencies {
            //mark a read event for this register
            let events = registerUseages.entry(dep).or_insert(vec![]);
            events.push(RegisterLifetimeEvent {
                kind: RegisterLifetimeEventKind::Read,
                instruction_index: index,
            });
        }
        for dep in dependents {
            //mark a write event for this register
            let events = registerUseages.entry(dep).or_insert(vec![]);
            events.push(RegisterLifetimeEvent {
                kind: RegisterLifetimeEventKind::Write,
                instruction_index: index,
            });
        }
    }
    registerUseages
}

pub fn print_instruction_with_lifetime_annotations(function: &IrFunction) {
    let registerUsages = get_function_register_lifetimes(function);
    let max_instruction_length = function
        .code
        .iter()
        .map(|instr| format!("{}", instr).len())
        .max()
        .unwrap_or(0);
    let general_lifetimes: HashMap<IrRegisterId, (usize, usize)> = registerUsages
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
                let events = &registerUsages[reg_id];
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

// this represents an actual location in the stack
// size is implicitly 1 byte
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
struct PhysicalLocation(usize); // offset from stack frame adress

struct PhysicalLocationAllocator {
    next_avalible_space: PhysicalLocation,
    registers_in_use: Vec<(PhysicalLocation, usize)>, //location and size
}

impl PhysicalLocationAllocator {
    fn new() -> Self {
        PhysicalLocationAllocator {
            next_avalible_space: PhysicalLocation(0),
            registers_in_use: Vec::new(),
        }
    }

    fn allocate(&mut self, size: usize) -> PhysicalLocation {
        let allocated = self.next_avalible_space;
        self.next_avalible_space = PhysicalLocation(self.next_avalible_space.0 + size);
        self.registers_in_use.push((allocated, size));
        allocated
    }

    fn emit_clear_instructions(&self) -> Vec<Ir2Instruction> {
        let mut clear_instructions = vec![];
        for (location, size) in &self.registers_in_use {
            for offset in 0..*size {
                clear_instructions.push(Ir2Instruction::Clear {
                    target: PhysicalLocation(location.0 + offset),
                });
            }
        }
        clear_instructions
    }
}

pub enum Ir2Instruction {
    Clear {
        target: PhysicalLocation,
    },
    Init {
        target: PhysicalLocation,
        value: u8,
    },
    Move {
        target: PhysicalLocation,
        source: PhysicalLocation,
    },
    NMove {
        targets: Vec<PhysicalLocation>,
        source: PhysicalLocation,
    },
    BulkMove {
        target: PhysicalLocation,
        source: PhysicalLocation,
        size: usize,
    },
    BulkNMove {
        targets: Vec<PhysicalLocation>,
        source: PhysicalLocation,
        size: usize,
    },
    MoveFromIndirect {
        base: PhysicalLocation,
        offset: PhysicalLocation,
        output: PhysicalLocation,
    },
    MoveFromIndirectConstant {
        base: PhysicalLocation,
        offset: usize,
        output: PhysicalLocation,
    },
    MoveToIndirect {
        base: PhysicalLocation,
        offset: PhysicalLocation,
        value: PhysicalLocation,
    },
    MoveToIndirectConstant {
        base: PhysicalLocation,
        offset: usize,
        value: PhysicalLocation,
    },
    MoveFromIndirectArray {
        base: PhysicalLocation,
        offset: PhysicalLocation,
        element_size: usize,
        output: PhysicalLocation,
    },
    MoveToIndirectArray {
        base: PhysicalLocation,
        offset: PhysicalLocation,
        element_size: usize,
        value: PhysicalLocation,
    },
    MoveToIndirectArrayConstant {
        base: PhysicalLocation,
        offset: usize,
        element_size: usize,
        value: PhysicalLocation,
    },
    MoveFromIndirectArrayConstant {
        base: PhysicalLocation,
        offset: usize,
        element_size: usize,
        output: PhysicalLocation,
    },
    ClearBulk {
        target: PhysicalLocation,
        size: usize,
    },
    ClearIndirect {
        base: PhysicalLocation,
        offset: PhysicalLocation,
    },
    ClearIndirectConstant {
        base: PhysicalLocation,
        offset: usize,
    },
    ClearIndirectArray {
        base: PhysicalLocation,
        offset: PhysicalLocation,
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
        target: PhysicalLocation,
    },
    Output {
        element: PhysicalLocation,
    },
}

pub struct Ir2Function {
    pub code: Vec<Ir2Instruction>,
    pub metadata: Ir2FunctionMetadata,
}

#[derive(Debug, Clone)]
struct Ir2FunctionMetadata {
    parameter_layout: Vec<PhysicalLocation>,
    // output_location: PhysicalLocation,
    id: FunctionId,
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

    for (physical_location, param_register) in this_metadata
        .parameter_layout
        .iter()
        .zip(function.parameters.iter())
    {
        register_mapping.insert(param_register, *physical_location);
    }

    //TODO: set the output register location
    // register_mapping.insert(&function.output, this_metadata.output_location);

    //set the next available space to after the parameters
    if let Some((last_param_index, last_param)) = this_metadata
        .parameter_layout
        .iter()
        .enumerate()
        .max_by_key(|(i, p)| p.0)
    {
        let last_param_size = function
            .registers
            .get(&function.parameters[last_param_index])
            .expect("expected that param had proper register allocated")
            .size;
        physical_space_allocator.allocate(last_param.0 + last_param_size);
    }

    for register in &function.registers {
        if !register_mapping.contains_key(register.0) {
            register_mapping.insert(
                register.0,
                physical_space_allocator.allocate(register.1.size),
            );
        }
    }

    let mut new_code = vec![];
    for old_instruction in &function.code {
        new_code.extend(match old_instruction {
            IrInstruction::Move { target, source } => {
                let target_location = register_mapping
                    .get(target)
                    .expect("expected valid mapping");
                match source {
                    IrMoveOperand::IntLiteral(x) => match x {
                        IntLiteral::U8(val) => {
                            vec![Ir2Instruction::Init {
                                target: *target_location,
                                value: *val,
                            }]
                        }
                        IntLiteral::I8(val) => {
                            vec![Ir2Instruction::Init {
                                target: *target_location,
                                value: (*val).to_be_bytes()[0],
                            }]
                        }
                        IntLiteral::U16(val) => {
                            let raw_bytes = val.to_be_bytes();
                            vec![
                                Ir2Instruction::Init {
                                    target: *target_location,
                                    value: raw_bytes[0],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 1),
                                    value: raw_bytes[1],
                                },
                            ]
                        }
                        IntLiteral::I16(val) => {
                            let raw_bytes = val.to_be_bytes();
                            vec![
                                Ir2Instruction::Init {
                                    target: *target_location,
                                    value: raw_bytes[0],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 1),
                                    value: raw_bytes[1],
                                },
                            ]
                        }
                        IntLiteral::U32(val) => {
                            let raw_bytes = val.to_be_bytes();
                            vec![
                                Ir2Instruction::Init {
                                    target: *target_location,
                                    value: raw_bytes[0],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 1),
                                    value: raw_bytes[1],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 2),
                                    value: raw_bytes[2],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 3),
                                    value: raw_bytes[3],
                                },
                            ]
                        }
                        IntLiteral::I32(val) => {
                            let raw_bytes = val.to_be_bytes();
                            vec![
                                Ir2Instruction::Init {
                                    target: *target_location,
                                    value: raw_bytes[0],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 1),
                                    value: raw_bytes[1],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 2),
                                    value: raw_bytes[2],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 3),
                                    value: raw_bytes[3],
                                },
                            ]
                        }
                        IntLiteral::U64(val) => {
                            let raw_bytes = val.to_be_bytes();
                            vec![
                                Ir2Instruction::Init {
                                    target: *target_location,
                                    value: raw_bytes[0],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 1),
                                    value: raw_bytes[1],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 2),
                                    value: raw_bytes[2],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 3),
                                    value: raw_bytes[3],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 4),
                                    value: raw_bytes[4],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 5),
                                    value: raw_bytes[5],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 6),
                                    value: raw_bytes[6],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 7),
                                    value: raw_bytes[7],
                                },
                            ]
                        }
                        IntLiteral::I64(val) => {
                            let raw_bytes = val.to_be_bytes();
                            vec![
                                Ir2Instruction::Init {
                                    target: *target_location,
                                    value: raw_bytes[0],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 1),
                                    value: raw_bytes[1],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 2),
                                    value: raw_bytes[2],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 3),
                                    value: raw_bytes[3],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 4),
                                    value: raw_bytes[4],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 5),
                                    value: raw_bytes[5],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 6),
                                    value: raw_bytes[6],
                                },
                                Ir2Instruction::Init {
                                    target: PhysicalLocation(target_location.0 + 7),
                                    value: raw_bytes[7],
                                },
                            ]
                        }
                    },
                    IrMoveOperand::StringLiteral(val) => {
                        let bytes = val.as_bytes();
                        let mut inits = vec![];
                        for (i, byte) in bytes.iter().enumerate() {
                            inits.push(Ir2Instruction::Init {
                                target: PhysicalLocation(target_location.0 + i),
                                value: *byte,
                            });
                        }
                        inits
                    }
                    IrMoveOperand::Register(reg_id) => {
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

                        let copy_tmp = physical_space_allocator.allocate(reg_size);

                        if reg.0 == 1 {
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
                        } else {
                            vec![
                                Ir2Instruction::Move {
                                    target: copy_tmp,
                                    source: *reg,
                                },
                                Ir2Instruction::BulkNMove {
                                    targets: vec![*target_location, *reg],
                                    source: copy_tmp,
                                    size: function
                                        .registers
                                        .get(reg_id)
                                        .expect("expected valid register")
                                        .size,
                                },
                            ]
                        }
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
                output,
            } => {
                let function_metadata = global_function_metadata
                    .get(function_id)
                    .expect("expected valid function metadata");
                let total_param_size: usize = parameters
                    .iter()
                    .map(|param| {
                        let reg = function
                            .registers
                            .get(param)
                            .expect("expected that param had proper register allocated");
                        reg.size
                    })
                    .sum();
                let new_stack_frame_base = PhysicalLocation(
                    physical_space_allocator.next_avalible_space.0 + total_param_size,
                );

                let mut call_setup_code = vec![];

                for (i, param) in parameters.iter().enumerate() {
                    let param_reg = function
                        .registers
                        .get(param)
                        .expect("expected that param had proper register allocated");
                    let param_size = param_reg.size;
                    let param_physical_location = register_mapping
                        .get(param)
                        .expect("expected valid register mapping");
                    let tmp_move_location = physical_space_allocator.allocate(param_size);
                    //we need to copy the values into the places specified by the function input layout starting at the new stack frame base
                    call_setup_code.extend(match param_size {
                        1 => {
                            vec![
                                Ir2Instruction::Move {
                                    target: tmp_move_location,
                                    source: *param_physical_location,
                                },
                                Ir2Instruction::NMove {
                                    targets: vec![
                                        PhysicalLocation(
                                            new_stack_frame_base.0
                                                + function_metadata.parameter_layout[i].0,
                                        ),
                                        *param_physical_location,
                                    ],
                                    source: tmp_move_location,
                                },
                            ]
                        }
                        _ => {
                            vec![
                                Ir2Instruction::BulkMove {
                                    target: tmp_move_location,
                                    source: *param_physical_location,
                                    size: param_size,
                                },
                                Ir2Instruction::BulkNMove {
                                    targets: vec![
                                        PhysicalLocation(
                                            new_stack_frame_base.0
                                                + function_metadata.parameter_layout[i].0,
                                        ),
                                        *param_physical_location,
                                    ],
                                    source: tmp_move_location,
                                    size: param_size,
                                },
                            ]
                        }
                    })
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
                );
                let tmp_output_location_1 = physical_space_allocator.allocate(output_size);
                let tmp_output_location_2 = physical_space_allocator.allocate(output_size);

                let usage_offset_location = physical_space_allocator.allocate(
                    function
                        .registers
                        .get(offset)
                        .expect("expected valid register")
                        .size,
                );

                let output_register = function
                    .registers
                    .get(output)
                    .expect("expected valid register");
                let offset_register = function
                    .registers
                    .get(offset)
                    .expect("expected valid register");

                let mut code_sequence = vec![];

                //move offset -> tmp_offset, usage_offset
                //indirect read base, usage_offset -> tmp_output
                //move tmp_output -> output, tmp_output_2
                //move tmp_offset -> offset, usage_offset
                //indirect write base, usage_offset, tmp_output_2

                code_sequence.push(match offset_register.size {
                    1 => Ir2Instruction::NMove {
                        source: *offset_location,
                        targets: vec![tmp_offset_location, usage_offset_location],
                    },
                    _ => Ir2Instruction::BulkNMove {
                        source: *offset_location,
                        targets: vec![tmp_offset_location, usage_offset_location],
                        size: offset_register.size,
                    },
                });

                code_sequence.push(Ir2Instruction::MoveFromIndirect {
                    base: *base_location,
                    offset: usage_offset_location,
                    output: tmp_output_location_1,
                });

                code_sequence.push(match output_register.size {
                    1 => Ir2Instruction::NMove {
                        source: tmp_output_location_1,
                        targets: vec![*output_location, tmp_output_location_2],
                    },
                    _ => Ir2Instruction::BulkNMove {
                        source: tmp_output_location_1,
                        targets: vec![*output_location, tmp_output_location_2],
                        size: output_register.size,
                    },
                });

                code_sequence.push(match offset_register.size {
                    1 => Ir2Instruction::NMove {
                        source: tmp_offset_location,
                        targets: vec![*offset_location, usage_offset_location],
                    },
                    _ => Ir2Instruction::BulkNMove {
                        source: tmp_offset_location,
                        targets: vec![*offset_location, usage_offset_location],
                        size: offset_register.size,
                    },
                });

                code_sequence.push(Ir2Instruction::MoveToIndirect {
                    base: *base_location,
                    offset: usage_offset_location,
                    value: tmp_output_location_2,
                });

                code_sequence
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

                let value_register = function
                    .registers
                    .get(value)
                    .expect("expected valid register");
                let offset_register = function
                    .registers
                    .get(offset)
                    .expect("expected valid register");

                let tmp_offset_location = physical_space_allocator.allocate(
                    function
                        .registers
                        .get(offset)
                        .expect("expected valid register")
                        .size,
                );
                let usage_offset_location = physical_space_allocator.allocate(
                    function
                        .registers
                        .get(offset)
                        .expect("expected valid register")
                        .size,
                );
                let tmp_value_location = physical_space_allocator.allocate(value_register.size);
                let usage_value_location = physical_space_allocator.allocate(value_register.size);

                //move offset -> tmp_offset, usage_offset
                //indirect clear base, usage_offset
                //move value -> tmp_value, usage_value
                //move tmp_offset -> offset, usage_offset
                //indirect write base, usage_offset, usage_value
                //move tmp_value -> value

                code_sequence.push(match offset_register.size {
                    1 => Ir2Instruction::NMove {
                        source: *offset_location,
                        targets: vec![tmp_offset_location, usage_offset_location],
                    },
                    _ => Ir2Instruction::BulkNMove {
                        source: *offset_location,
                        targets: vec![tmp_offset_location, usage_offset_location],
                        size: offset_register.size,
                    },
                });

                code_sequence.push(Ir2Instruction::ClearIndirect {
                    base: *base_location,
                    offset: usage_offset_location,
                });

                code_sequence.push(match value_register.size {
                    1 => Ir2Instruction::NMove {
                        source: *value_location,
                        targets: vec![tmp_value_location, usage_value_location],
                    },
                    _ => Ir2Instruction::BulkNMove {
                        source: *value_location,
                        targets: vec![tmp_value_location, usage_value_location],
                        size: value_register.size,
                    },
                });

                code_sequence.push(match offset_register.size {
                    1 => Ir2Instruction::NMove {
                        source: tmp_offset_location,
                        targets: vec![*offset_location, usage_offset_location],
                    },
                    _ => Ir2Instruction::BulkNMove {
                        source: tmp_offset_location,
                        targets: vec![*offset_location, usage_offset_location],
                        size: offset_register.size,
                    },
                });

                code_sequence.push(Ir2Instruction::MoveToIndirect {
                    base: *base_location,
                    offset: usage_offset_location,
                    value: usage_value_location,
                });

                code_sequence.push(match value_register.size {
                    1 => Ir2Instruction::Move {
                        target: *value_location,
                        source: tmp_value_location,
                    },
                    _ => Ir2Instruction::BulkMove {
                        target: *value_location,
                        source: tmp_value_location,
                        size: value_register.size,
                    },
                });

                code_sequence
            }
        });
    }

    //emit all the instructions needed to clear all of the used memory locations at the end of the function
    //TODO: be smarter about this
    //TODO: this is where return value handling will go later
    new_code.extend(physical_space_allocator.emit_clear_instructions());

    Ir2Function {
        metadata: this_metadata.clone(),
        code: new_code,
    }
}

pub fn generate_ir2(ir_functions: &Vec<IrFunction>) -> HashMap<FunctionId, Ir2Function> {
    //first generate function metadata for all functions
    let mut global_function_metadata = HashMap::new();
    for function in ir_functions {
        let mut parameter_layout = vec![];
        let mut current_offset = 0;
        for param in &function.parameters {
            let reg = function
                .registers
                .get(param)
                .expect("expected that param had proper register allocated");
            parameter_layout.push(PhysicalLocation(current_offset));
            current_offset += reg.size;
        }
        // let output_reg = function
        //     .registers
        //     .get(&function.output)
        //     .expect("expected that output had proper register allocated");
        // let output_location = PhysicalLocation(current_offset);
        // current_offset += output_reg.size;

        let metadata = Ir2FunctionMetadata {
            id: function.id,
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


impl Display for PhysicalLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}%", self.0)
    }
}

impl Display for Ir2Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ir2Instruction::Clear { target } => {
                write!(f, "CLEAR [SP + {}]", target)
            }
            Ir2Instruction::Init { target, value } => {
                write!(f, "INIT [SP + {}] = {}", target, value)
            }
            Ir2Instruction::Move { target, source } => {
                write!(f, "MOVE [SP + {}] <- [SP + {}]", target, source)
            }
            Ir2Instruction::NMove { targets, source } => {
                let target_strs: Vec<String> = targets
                    .iter()
                    .map(|t| format!("[SP + {}]", t))
                    .collect();
                write!(f, "NMOVE {} <- [SP + {}]", target_strs.join(", "), source)
            }
            Ir2Instruction::BulkMove { target, source, size } => {
                write!(
                    f,
                    "BULK_MOVE [SP + {}] <- [SP + {}] SIZE {}",
                    target, source, size
                )
            }
            Ir2Instruction::BulkNMove { targets, source, size } => {
                let target_strs: Vec<String> = targets
                    .iter()
                    .map(|t| format!("[SP + {}]", t))
                    .collect();
                write!(
                    f,
                    "BULK_NMOVE {} <- [SP + {}] SIZE {}",
                    target_strs.join(", "),
                    source,
                    size
                )
            }
            Ir2Instruction::MoveFromIndirect { base, offset, output } => {
                write!(
                    f,
                    "MOVE_FROM_INDIRECT [SP + {} + [SP + {}]] -> [SP + {}]",
                    base, offset, output
                )
            }
            Ir2Instruction::MoveFromIndirectConstant { base, offset, output } => {
                write!(
                    f,
                    "MOVE_FROM_INDIRECT_CONST [SP + {} + {}] -> [SP + {}]",
                    base, offset, output
                )
            }
            Ir2Instruction::MoveFromIndirectArray { base, offset, output, element_size } => {
                write!(
                    f,
                    "MOVE_FROM_INDIRECT_CONST [SP + {} + {} * {}] -> [SP + {}] size {}",
                    base, offset, element_size, output, element_size
                )
            }
            Ir2Instruction::MoveFromIndirectArrayConstant { base, offset, output, element_size } => {
                write!(
                    f,
                    "MOVE_FROM_INDIRECT_ARRAY_CONST [SP + {} + {} * {}] -> [SP + {}] size {}",
                    base, offset, element_size, output, element_size
                )
            }
            Ir2Instruction::MoveToIndirectArray { base, offset, element_size, value } => {
                write!(
                    f,
                    "MOVE_TO_INDIRECT_ARRAY [SP + {} + {} * {}] <- [SP + {}] size {}",
                    base, offset, element_size, value, element_size
                )
            }
            Ir2Instruction::MoveToIndirectArrayConstant { base, offset, element_size, value } => {
                write!(
                    f,
                    "MOVE_TO_INDIRECT_ARRAY_CONST [SP + {} + {} * {}] <- [SP + {}] size {}",
                    base, offset, element_size, value, element_size
                )
            }
            Ir2Instruction::MoveToIndirect { base, offset, value } => {
                write!(
                    f,
                    "MOVE_TO_INDIRECT [SP + {} + [SP + {}]] <- [SP + {}]",
                    base, offset, value
                )
            }
            Ir2Instruction::MoveToIndirectConstant { base, offset, value } => {
                write!(
                    f,
                    "MOVE_TO_INDIRECT_CONST [SP + {} + {}] <- [SP + {}]",
                    base, offset, value
                )
            }
            Ir2Instruction::ClearIndirect { base, offset } => {
                write!(
                    f,
                    "CLEAR_INDIRECT [SP + {} + [SP + {}]]",
                    base, offset
                )
            }
            Ir2Instruction::ClearIndirectConstant { base, offset } => {
                write!(
                    f,
                    "CLEAR_INDIRECT_CONST [SP + {} + {}]",
                    base, offset
                )
            }
            Ir2Instruction::ClearIndirectArray { base, offset , element_size} => {
                write!(
                    f,
                    "CLEAR_INDIRECT_CONST [SP + {} + {} * {}]",
                    base, offset, element_size
                )
            }
            Ir2Instruction::ClearIndirectArrayConstant { base, offset , element_size} => {
                write!(
                    f,
                    "CLEAR_INDIRECT_ARRAY_CONST [SP + {} + {} * {}]",
                    base, offset, element_size
                )
            }
            Ir2Instruction::ClearBulk { target, size } => {
                write!(f, "CLEAR_BULK [SP + {}] SIZE {}", target, size)
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
