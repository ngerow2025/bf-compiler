use crate::ir2::{Ir2Function, Ir2Instruction};
use crate::ir2_memory_representation::PhysicalLocation;
use crate::parser::FunctionId;
use crate::util::transpose;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter;

pub struct BfGenerator {
    code: Vec<BfUcodeInstruction>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocationId(pub usize);

#[derive(Debug, Clone, Copy)]
pub enum Location {
    Absolute(PhysicalLocation),
    Relative(isize),
    OffsetAbsolute {
        base: PhysicalLocation,
        offset: isize,
    },
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Absolute(x) => write!(f, "Abs({})", x),
            Location::Relative(x) => write!(f, "Rel({})", x),
            Location::OffsetAbsolute { base, offset } => {
                write!(f, "OffsetAbs(base: {}, offset: {})", base, offset)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum BfUcodeInstruction {
    Input,
    Output,
    MovePtr(Location),
    MoveData {
        from: Location,
        to: Location,
    },
    Inc(u8),
    Dec(u8),
    Loop(Vec<BfUcodeInstruction>),
    JumpLocation {
        this_location: LocationId,
        target_location: LocationId,
    },
    Clear,
}

impl Display for BfUcodeInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            match self {
                BfUcodeInstruction::Input => "Input".to_string(),
                BfUcodeInstruction::Output => "Output".to_string(),
                BfUcodeInstruction::MovePtr(x) => format!("MovePtr({})", x),
                BfUcodeInstruction::MoveData { from, to } =>
                    format!("RelMove(from: {}, to: {})", from, to),
                BfUcodeInstruction::Inc(x) => format!("Inc({})", x),
                BfUcodeInstruction::Dec(x) => format!("Dec({})", x),
                BfUcodeInstruction::Loop(body) => {
                    let mut body_str = String::new();
                    for instr in body {
                        body_str.push_str(&format!("  {}\n", instr));
                    }
                    format!("Loop {{\n{}}}", body_str)
                }
                BfUcodeInstruction::JumpLocation {
                    this_location,
                    target_location,
                } => format!(
                    "JumpLocation(this: {}, target: {})",
                    this_location.0, target_location.0
                ),
                BfUcodeInstruction::Clear => "Clear".to_string(),
            }
        )
    }
}

impl Default for BfGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl BfGenerator {
    pub fn new() -> BfGenerator {
        BfGenerator { code: vec![] }
    }

    fn code_gen_ir_instruction(
        &mut self,
        instruction: &Ir2Instruction,
        jump_points: &mut usize,
        entry_points: &HashMap<&FunctionId, LocationId>,
    ) {
        self.code.extend(match instruction {
            Ir2Instruction::Clear { target } => target
                .get_all_locations()
                .iter()
                .flat_map(|target| {
                    [
                        BfUcodeInstruction::MovePtr(Location::Absolute(*target)),
                        BfUcodeInstruction::Clear,
                    ]
                })
                .collect(),
            Ir2Instruction::Init { target, values } => {
                assert_eq!(
                    target.get_size(),
                    values.len(),
                    "Init instruction target size must match number of values"
                );

                target
                    .get_all_locations()
                    .iter()
                    .zip(values)
                    .flat_map(|(target_loc, value)| {
                        [
                            BfUcodeInstruction::MovePtr(Location::Absolute(*target_loc)),
                            BfUcodeInstruction::Inc(*value),
                        ]
                    })
                    .collect()
            }
            Ir2Instruction::Move { target, source } => {
                assert_eq!(
                    target.get_size(),
                    source.get_size(),
                    "Move instruction source and target sizes must match"
                );

                source
                    .get_all_locations()
                    .iter()
                    .zip(target.get_all_locations().iter())
                    .map(|(source_loc, target_loc)| BfUcodeInstruction::MoveData {
                        from: Location::Absolute(*source_loc),
                        to: Location::Absolute(*target_loc),
                    })
                    .collect()
            }
            Ir2Instruction::NMove { targets, source } => {
                for target in targets.iter() {
                    assert_eq!(
                        target.get_size(),
                        source.get_size(),
                        "NMove instruction source and target sizes must match"
                    );
                }

                let targets_loc_groups = transpose(
                    targets
                        .iter()
                        .map(|target| target.get_all_locations())
                        .collect(),
                );

                source
                    .get_all_locations()
                    .iter()
                    .zip(targets_loc_groups)
                    .flat_map(|(source_loc, target_locs)| {
                        let inc_targets = target_locs.iter().flat_map(|target_loc| {
                            [
                                BfUcodeInstruction::MovePtr(Location::Absolute(*target_loc)),
                                BfUcodeInstruction::Inc(1),
                            ]
                        });

                        vec![
                            BfUcodeInstruction::MovePtr(Location::Absolute(*source_loc)),
                            BfUcodeInstruction::Loop(
                                iter::once(BfUcodeInstruction::Dec(1))
                                    .chain(inc_targets)
                                    .chain(iter::once(BfUcodeInstruction::MovePtr(
                                        Location::Absolute(*source_loc),
                                    )))
                                    .collect(),
                            ),
                        ]
                    })
                    .collect()
            }

            Ir2Instruction::MoveFromIndirect {
                output,
                base: actual_base,
                offset,
            } => {
                assert_eq!(
                    offset.get_size(),
                    1,
                    "MoveFromIndirect offset must be a single cell"
                );
                assert_eq!(
                    output.get_size(),
                    1,
                    "Array MoveFromIndirect is not supported yet"
                );

                let base = *actual_base + 1; //we only need 3 extra bytes before the actual array start, act like base is shifted by 1
                vec![
                    BfUcodeInstruction::MoveData {
                        from: Location::Absolute(offset.get_start()),
                        to: Location::Absolute(base),
                    },
                    BfUcodeInstruction::MovePtr(Location::Absolute(base)),
                    BfUcodeInstruction::Loop(vec![
                        BfUcodeInstruction::Dec(1),
                        BfUcodeInstruction::MovePtr(Location::Relative(1)),
                        BfUcodeInstruction::Inc(1),
                        BfUcodeInstruction::MovePtr(Location::Relative(-1)),
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(1),
                            to: Location::Relative(2),
                        },
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(0),
                            to: Location::Relative(1),
                        },
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(3),
                            to: Location::Relative(0),
                        },
                        BfUcodeInstruction::MovePtr(Location::Relative(1)),
                    ]),
                    BfUcodeInstruction::MoveData {
                        from: Location::Relative(3),
                        to: Location::Relative(2),
                    },
                    BfUcodeInstruction::MovePtr(Location::Relative(1)),
                    BfUcodeInstruction::Loop(vec![
                        BfUcodeInstruction::Dec(1),
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(0),
                            to: Location::Relative(-1),
                        },
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(1),
                            to: Location::Relative(0),
                        },
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(-2),
                            to: Location::Relative(1),
                        },
                        BfUcodeInstruction::MovePtr(Location::Relative(-1)),
                    ]),
                    BfUcodeInstruction::MoveData {
                        from: Location::OffsetAbsolute { base, offset: 2 },
                        to: Location::Absolute(output.get_start()),
                    },
                ]
            }

            Ir2Instruction::MoveFromIndirectConstant {
                base,
                offset,
                output,
            } => {
                assert!(
                    output.get_size() == 1,
                    "Array MoveFromIndirectConstant is not supported yet"
                );

                vec![BfUcodeInstruction::MoveData {
                    from: Location::OffsetAbsolute {
                        base: *base,
                        offset: *offset as isize + 4,
                    },
                    to: Location::Absolute(output.get_start()),
                }]
            }
            //this does assume that there is 4 scratch cells before the actual array
            Ir2Instruction::MoveToIndirect {
                base,
                offset,
                value,
            } => {
                assert_eq!(
                    offset.get_size(),
                    1,
                    "MoveToIndirect offset must be a single cell"
                );
                assert_eq!(
                    value.get_size(),
                    1,
                    "Array MoveToIndirect is not supported yet"
                );

                //setup the following cells:
                //ind -ind val 0 arr...
                vec![
                    BfUcodeInstruction::MoveData {
                        from: Location::Absolute(offset.get_start()),
                        to: Location::Absolute(*base),
                    },
                    BfUcodeInstruction::MoveData {
                        from: Location::Absolute(value.get_start()),
                        to: Location::OffsetAbsolute {
                            base: *base,
                            offset: 2,
                        },
                    },
                    BfUcodeInstruction::MovePtr(Location::Absolute(*base)),
                    BfUcodeInstruction::Loop(vec![
                        BfUcodeInstruction::Dec(1),
                        BfUcodeInstruction::MovePtr(Location::Relative(1)),
                        BfUcodeInstruction::Inc(1),
                        BfUcodeInstruction::MovePtr(Location::Relative(-1)),
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(2),
                            to: Location::Relative(3),
                        },
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(1),
                            to: Location::Relative(2),
                        },
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(0),
                            to: Location::Relative(1),
                        },
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(4),
                            to: Location::Relative(0),
                        },
                        BfUcodeInstruction::MovePtr(Location::Relative(1)),
                    ]),
                    BfUcodeInstruction::MoveData {
                        from: Location::Relative(2),
                        to: Location::Relative(4),
                    },
                    //layout: 0 ind 0 0
                    BfUcodeInstruction::MovePtr(Location::Relative(1)),
                    BfUcodeInstruction::Loop(vec![
                        BfUcodeInstruction::Dec(1),
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(-2),
                            to: Location::Relative(2),
                        },
                        BfUcodeInstruction::MoveData {
                            from: Location::Relative(0),
                            to: Location::Relative(-1),
                        },
                        BfUcodeInstruction::MovePtr(Location::Relative(-1)),
                    ]),
                    BfUcodeInstruction::MovePtr(Location::Relative(-1)),
                ]
            }
            Ir2Instruction::MoveToIndirectConstant { .. } => {
                todo!("Need to actually do MoveToIndirectConstant")
            }
            Ir2Instruction::ClearIndirect { .. } => {
                todo!("Need to actually do ClearIndirect")
            }
            Ir2Instruction::ClearIndirectConstant { .. } => {
                todo!("Need to actually do ClearIndirectConstant")
            }
            Ir2Instruction::ClearIndirectArray { .. } => {
                todo!("Need to actually do ClearIndirectArray")
            }
            Ir2Instruction::ClearIndirectArrayConstant { .. } => {
                todo!("Need to actually do ClearIndirectArrayConstant")
            }
            Ir2Instruction::Call {
                function_id,
                new_stack_frame_base,
            } => {
                let return_address = LocationId(*jump_points);
                *jump_points += 1;
                let function_entry = entry_points.get(function_id).unwrap();
                vec![
                    //codegen will set up all of the return addressing and jump handling with its 3 reserved cells right before the new stack frame base
                    //move to the actual new stack frame base
                    BfUcodeInstruction::MovePtr(Location::Absolute(*new_stack_frame_base)),
                    //jump to function
                    BfUcodeInstruction::JumpLocation {
                        this_location: return_address,
                        target_location: *function_entry,
                    },
                ]
            }
            Ir2Instruction::Input { target } => {
                assert_eq!(
                    target.get_size(),
                    1,
                    "Input instruction target must be a single cell"
                );
                vec![
                    BfUcodeInstruction::MovePtr(Location::Absolute(target.get_start())),
                    BfUcodeInstruction::Input,
                ]
            }
            Ir2Instruction::Output { element } => {
                assert_eq!(
                    element.get_size(),
                    1,
                    "Output instruction element must be a single cell"
                );
                vec![
                    BfUcodeInstruction::MovePtr(Location::Absolute(element.get_start())),
                    BfUcodeInstruction::Output,
                ]
            }
        });
    }

    pub fn ucodegen_program(program: HashMap<FunctionId, Ir2Function>) -> UCodeProgram {
        let mut total_results = HashMap::new();
        let mut entry_points = HashMap::new();
        for function_id in program.keys() {
            entry_points.insert(function_id, LocationId(entry_points.len() + 1));
        }
        let mut jump_points = entry_points.len() + 1;
        for (function_id, function) in &program {
            let mut bf_generator = BfGenerator::new();

            for instruction in &function.code {
                bf_generator.code_gen_ir_instruction(instruction, &mut jump_points, &entry_points);
            }

            let generated_bf = bf_generator.code;
            total_results.insert(*function_id, generated_bf);
        }

        UCodeProgram {
            functions: total_results,
            entry_points: entry_points.into_iter().map(|(k, v)| (*k, v)).collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct UCodeProgram {
    pub functions: HashMap<FunctionId, Vec<BfUcodeInstruction>>,
    pub entry_points: HashMap<FunctionId, LocationId>,
}
