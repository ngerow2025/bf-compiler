use crate::ir2::{Ir2Function, Ir2Instruction};
use crate::parser::FunctionId;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub struct BfGenerator {
    code: Vec<BfUcodeInstruction>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocationId(pub usize);

#[derive(Debug, Clone, Copy)]
pub enum Location {
    Absolute(usize),
    Relative(i64),
    OffsetAbsolute { base: usize, offset: i64 },
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
            Ir2Instruction::Clear { target } => {
                vec![
                    BfUcodeInstruction::MovePtr(Location::Absolute(target.0)),
                    BfUcodeInstruction::Clear,
                ]
            }
            Ir2Instruction::Init { target, value } => {
                vec![
                    BfUcodeInstruction::MovePtr(Location::Absolute(target.0)),
                    BfUcodeInstruction::Inc(*value as u8),
                ]
            }
            Ir2Instruction::Move { target, source } => {
                vec![BfUcodeInstruction::MoveData {
                    from: Location::Absolute(source.0),
                    to: Location::Absolute(target.0),
                }]
            }
            Ir2Instruction::NMove { targets, source } => {
                vec![
                    BfUcodeInstruction::MovePtr(Location::Absolute(source.0)),
                    BfUcodeInstruction::Loop(
                        vec![BfUcodeInstruction::Dec(1)]
                            .into_iter()
                            .chain(targets.into_iter().flat_map(|target| {
                                vec![
                                    BfUcodeInstruction::MovePtr(Location::Absolute(target.0)),
                                    BfUcodeInstruction::Inc(1),
                                ]
                            }))
                            .collect(),
                    ),
                ]
            }
            Ir2Instruction::BulkMove {
                target,
                source,
                size,
            } => {
                let distance = target.0 as i64 - source.0 as i64;
                vec![BfUcodeInstruction::MovePtr(Location::Absolute(source.0))]
                    .into_iter()
                    .chain((0..*size).into_iter().flat_map(|_| {
                        vec![
                            BfUcodeInstruction::MoveData {
                                from: Location::Relative(0),
                                to: Location::Relative(distance),
                            },
                            BfUcodeInstruction::MovePtr(Location::Relative(1)),
                        ]
                    }))
                    .collect()
            }
            Ir2Instruction::BulkNMove {
                size,
                source,
                targets,
            } => {
                let mut distances = vec![targets[0].0 as i64 - source.0 as i64];
                distances.extend(
                    targets
                        .windows(2)
                        .map(|window| window[1].0 as i64 - window[0].0 as i64),
                );
                let return_distance = source.0 as i64 - targets.last().unwrap().0 as i64;

                vec![BfUcodeInstruction::MovePtr(Location::Absolute(source.0))]
                    .into_iter()
                    .chain((0..*size).into_iter().flat_map(|_| {
                        vec![
                            BfUcodeInstruction::Loop(
                                vec![BfUcodeInstruction::Dec(1)]
                                    .into_iter()
                                    .chain(distances.iter().flat_map(|d| {
                                        vec![
                                            BfUcodeInstruction::MovePtr(Location::Relative(*d)),
                                            BfUcodeInstruction::Inc(1),
                                        ]
                                    }))
                                    .chain(vec![BfUcodeInstruction::MovePtr(Location::Relative(
                                        return_distance,
                                    ))])
                                    .collect(),
                            ),
                            BfUcodeInstruction::MovePtr(Location::Relative(1)),
                        ]
                    }))
                    .collect()
            }

            Ir2Instruction::MoveFromIndirect {
                output,
                base: actual_base,
                offset,
            } => {
                let base = actual_base.0 + 1; //we only need 3 extra bytes before the actual array start, act like base is shifted by 1
                vec![
                    BfUcodeInstruction::MoveData {
                        from: Location::Absolute(offset.0),
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
                        to: Location::Absolute(output.0),
                    },
                ]
            }

            Ir2Instruction::MoveFromIndirectConstant {
                base,
                offset,
                output,
            } => {
                vec![BfUcodeInstruction::MoveData {
                    from: Location::OffsetAbsolute {
                        base: base.0,
                        offset: *offset as i64 + 4,
                    },
                    to: Location::Absolute(output.0),
                }]
            }
            //this does assume that there is 4 scratch cells before the actual array
            Ir2Instruction::MoveToIndirect {
                base,
                offset,
                value,
            } => {
                //setup the following cells:
                //ind -ind val 0 arr...
                vec![
                    BfUcodeInstruction::MoveData {
                        from: Location::Absolute(offset.0),
                        to: Location::Absolute(base.0),
                    },
                    BfUcodeInstruction::MoveData {
                        from: Location::Absolute(value.0),
                        to: Location::OffsetAbsolute {
                            base: base.0,
                            offset: 2,
                        },
                    },
                    BfUcodeInstruction::MovePtr(Location::Absolute(base.0)),
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
            Ir2Instruction::MoveFromIndirectArray { .. } => {
                todo!("Need to actually do MoveFromIndirectArray")
            }
            Ir2Instruction::MoveToIndirectArray { .. } => {
                todo!("Need to actually do MoveToIndirectArray")
            }
            Ir2Instruction::MoveToIndirectArrayConstant { .. } => {
                todo!("Need to actually do MoveToIndirectArrayConstant")
            }
            Ir2Instruction::MoveFromIndirectArrayConstant { .. } => {
                todo!("Need to actually do MoveFromIndirectArrayConstant")
            }
            Ir2Instruction::ClearBulk { target, size } => {
                vec![BfUcodeInstruction::MovePtr(Location::Absolute(target.0))]
                    .into_iter()
                    .chain((0..*size).flat_map(|_| {
                        vec![
                            BfUcodeInstruction::Loop(vec![BfUcodeInstruction::Dec(1)]),
                            BfUcodeInstruction::MovePtr(Location::Relative(1)),
                        ]
                    }))
                    .collect()
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
                    BfUcodeInstruction::MovePtr(Location::Absolute(new_stack_frame_base.0)),
                    //jump to function
                    BfUcodeInstruction::JumpLocation {
                        this_location: return_address,
                        target_location: *function_entry,
                    },
                ]
            }
            Ir2Instruction::Input { target } => {
                vec![
                    BfUcodeInstruction::MovePtr(Location::Absolute(target.0)),
                    BfUcodeInstruction::Input,
                ]
            }
            Ir2Instruction::Output { element } => {
                vec![
                    BfUcodeInstruction::MovePtr(Location::Absolute(element.0)),
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
