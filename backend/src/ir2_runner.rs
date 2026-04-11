use std::{
    collections::HashMap,
    io::{self, Write},
    vec,
};

use crate::{
    input_grabbing::read_single_byte,
    ir2::{Ir2Function, Ir2Instruction},
    parser::FunctionId,
};

struct Ir2GlobalContext<'a> {
    pub functions: Vec<&'a Ir2Function>,
    pub function_name_mapping: HashMap<FunctionId, String>,
}

pub fn run_ir2(
    ir_functions: Vec<&Ir2Function>,
    function_name_mapping: HashMap<FunctionId, String>,
) {
    let global_context = Ir2GlobalContext {
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
        let _raw_input_guard = crate::input_grabbing::RawModeGuard::new();
        let mut main_tape = Tape::new();
        run_function(main_id, &global_context, TapeView::new(&mut main_tape));
    } else {
        panic!("No main function found in function name mapping");
    }
}

struct Tape {
    data: Vec<u8>,
}

impl Tape {
    fn new() -> Self {
        Tape { data: vec![] }
    }
}

struct TapeView<'a> {
    view: &'a mut Tape,
    offset: usize,
}

impl<'a> TapeView<'a> {
    fn new(tape: &'a mut Tape) -> Self {
        TapeView {
            view: tape,
            offset: 0,
        }
    }

    fn read(&mut self, index: usize) -> u8 {
        self.ensure_capacity(index);
        self.view.data[self.offset + index]
    }

    fn write(&mut self, index: usize, value: u8) {
        self.ensure_capacity(index);
        self.view.data[self.offset + index] = value;
    }

    fn ensure_capacity(&mut self, index: usize) {
        let required_length = self.offset + index + 1;
        if self.view.data.len() < required_length {
            self.view.data.resize(required_length, 0);
        }
    }

    fn get_offset_view<'b>(&'b mut self, offset: usize) -> TapeView<'b> {
        TapeView {
            view: self.view,
            offset: self.offset + offset,
        }
    }
}

fn run_function(function_id: FunctionId, global_context: &Ir2GlobalContext, mut memory: TapeView) {
    let function = global_context
        .functions
        .iter()
        .find(|f| f.id == function_id)
        .expect("Function ID not found in global context");

    print!(
        "Running function {}\r\n",
        global_context.function_name_mapping[&function_id]
    );

    run_instructions(&function.code, global_context, &mut memory);
}

fn run_instructions(
    instructions: &[Ir2Instruction],
    global_context: &Ir2GlobalContext,
    memory: &mut TapeView,
) {
    for instruction in instructions {
        print!("Memory state before: ");
        for i in 0..10 {
            let value = memory.read(i);
            print!("[{:>3}]", value);
        }
        print!("\r\n");
        print!("Executing instruction: {:?}\r\n", instruction);
        match instruction {
            Ir2Instruction::BulkMove {
                target,
                source,
                size,
            } => {
                for i in 0..*size {
                    let value = memory.read(source.0 + i);
                    let prev_value = memory.read(target.0 + i);
                    memory.write(target.0 + i, value.wrapping_add(prev_value));
                    memory.write(source.0 + i, 0);
                }
            }
            Ir2Instruction::BulkNMove {
                targets,
                source,
                size,
            } => {
                for i in 0..*size {
                    for target in targets {
                        let read_value = memory.read(source.0 + i);
                        let prev_value = memory.read(target.0 + i);
                        memory.write(target.0 + i, read_value.wrapping_add(prev_value));
                    }
                    memory.write(source.0 + i, 0);
                }
            }
            Ir2Instruction::Call {
                function_id,
                new_stack_frame_base,
            } => {
                print!("Params: ");
                for i in 0..10 {
                    let value = memory.read(new_stack_frame_base.0 + i);
                    print!("[{:>3}]", value);
                }
                run_function(
                    *function_id,
                    global_context,
                    memory.get_offset_view(new_stack_frame_base.0),
                );
            }
            Ir2Instruction::Clear { target } => {
                memory.write(target.0, 0);
            }
            Ir2Instruction::ClearBulk { target, size } => {
                for i in 0..*size {
                    memory.write(target.0 + i, 0);
                }
            }
            Ir2Instruction::ClearIndirect { base, offset } => {
                let resoved_address = base.0 as usize + memory.read(offset.0) as usize;
                memory.write(resoved_address, 0);
            }
            Ir2Instruction::ClearIndirectArray {
                base,
                offset,
                element_size,
            } => {
                let start_address =
                    base.0 as usize + memory.read(offset.0) as usize * (*element_size);
                for i in 0..*element_size {
                    memory.write(start_address + i, 0);
                }
            }
            Ir2Instruction::ClearIndirectArrayConstant {
                base,
                offset,
                element_size,
            } => {
                let start_address = base.0 as usize + (*offset as usize) * (*element_size);
                for i in 0..*element_size {
                    memory.write(start_address + i, 0);
                }
            }
            Ir2Instruction::ClearIndirectConstant { base, offset } => {
                let address = base.0 as usize + (*offset as usize);
                memory.write(address, 0);
            }
            Ir2Instruction::Init { target, value } => {
                let prev_value = memory.read(target.0);
                memory.write(target.0, value.wrapping_add(prev_value));
            }
            Ir2Instruction::Move { target, source } => {
                let value = memory.read(source.0);
                let prev_value = memory.read(target.0);
                memory.write(target.0, value.wrapping_add(prev_value));
                memory.write(source.0, 0);
            }
            Ir2Instruction::NMove { targets, source } => {
                let value = memory.read(source.0);
                for target in targets {
                    let prev_value = memory.read(target.0);
                    memory.write(target.0, value.wrapping_add(prev_value));
                }
                memory.write(source.0, 0);
            }
            Ir2Instruction::MoveFromIndirect {
                base,
                offset,
                output,
            } => {
                let resolved_address = base.0 as usize + memory.read(offset.0) as usize;
                let value = memory.read(resolved_address);
                let prev_value = memory.read(output.0);
                memory.write(output.0, value.wrapping_add(prev_value));
                memory.write(resolved_address, 0);
            }
            Ir2Instruction::MoveFromIndirectConstant {
                base,
                offset,
                output,
            } => {
                let resolved_address = base.0 as usize + (*offset as usize);
                let value = memory.read(resolved_address);
                let prev_value = memory.read(output.0);
                memory.write(output.0, value.wrapping_add(prev_value));
                memory.write(resolved_address, 0);
            }
            Ir2Instruction::MoveToIndirect {
                base,
                offset,
                value,
            } => {
                let resolved_address = base.0 as usize + memory.read(offset.0) as usize;
                let read_value = memory.read(value.0);
                let prev_value = memory.read(resolved_address);
                memory.write(resolved_address, read_value.wrapping_add(prev_value));
                memory.write(value.0, 0);
            }
            Ir2Instruction::MoveToIndirectConstant {
                base,
                offset,
                value,
            } => {
                let address = base.0 as usize + (*offset as usize);
                let read_value = memory.read(value.0);
                let prev_value = memory.read(address);
                memory.write(address, read_value.wrapping_add(prev_value));
                memory.write(value.0, 0);
            }
            Ir2Instruction::MoveFromIndirectArray {
                base,
                offset,
                element_size,
                output,
            } => {
                let start_address =
                    base.0 as usize + memory.read(offset.0) as usize * (*element_size);
                for i in 0..*element_size {
                    let value = memory.read(start_address + i);
                    let prev_value = memory.read(output.0 + i);
                    memory.write(output.0 + i, value.wrapping_add(prev_value));
                    memory.write(start_address + i, 0);
                }
            }
            Ir2Instruction::MoveToIndirectArray {
                base,
                offset,
                element_size,
                value,
            } => {
                let start_address =
                    base.0 as usize + memory.read(offset.0) as usize * (*element_size);
                for i in 0..*element_size {
                    let read_value = memory.read(value.0 + i);
                    let prev_value = memory.read(start_address + i);
                    memory.write(start_address + i, read_value.wrapping_add(prev_value));
                    memory.write(value.0 + i, 0);
                }
            }
            Ir2Instruction::MoveToIndirectArrayConstant {
                base,
                offset,
                element_size,
                value,
            } => {
                let start_address = base.0 as usize + (*offset as usize) * (*element_size);
                for i in 0..*element_size {
                    let read_value = memory.read(value.0 + i);
                    let prev_value = memory.read(start_address + i);
                    memory.write(start_address + i, read_value.wrapping_add(prev_value));
                    memory.write(value.0 + i, 0);
                }
            }
            Ir2Instruction::MoveFromIndirectArrayConstant {
                base,
                offset,
                element_size,
                output,
            } => {
                let start_address = base.0 as usize + (*offset as usize) * (*element_size);
                for i in 0..*element_size {
                    let value = memory.read(start_address + i);
                    memory.write(output.0 + i, value);
                    memory.write(start_address + i, 0);
                }
            }
            Ir2Instruction::Input { target } => {
                let input_byte = read_single_byte();
                memory.write(target.0, input_byte);
            }
            Ir2Instruction::Output { element } => {
                let output_byte = memory.read(element.0);
                print!("{}", output_byte as char);
                io::stdout().flush().expect("failed to flush stdout");
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{ir2::PhysicalLocation, ir2_runner::*};

    fn test_instructions(instructions: Vec<Ir2Instruction>, start_tape: Vec<u8>) -> Vec<u8> {
        let global_context = Ir2GlobalContext {
            functions: vec![],
            function_name_mapping: HashMap::new(),
        };
        let mut tape = Tape { data: start_tape };
        let mut tape_view = TapeView::new(&mut tape);

        run_instructions(&instructions, &global_context, &mut tape_view);
        tape.data
    }

    // Clear {
    //     target: PhysicalLocation,
    // },
    #[test]
    fn test_clear_instruction() {
        let instructions = vec![Ir2Instruction::Clear {
            target: PhysicalLocation(0),
        }];
        let start_tape = vec![5, 10, 15];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 10, 15]);
    }
    // Init {
    //     target: PhysicalLocation,
    //     value: u8,
    // },
    #[test]
    fn test_init_instruction1() {
        let instructions = vec![Ir2Instruction::Init {
            target: PhysicalLocation(1),
            value: 42,
        }];
        let start_tape = vec![5, 10, 15];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![5, 52, 15]);
    }

    #[test]
    fn test_init_instruction2() {
        let instructions = vec![
            Ir2Instruction::Init {
                target: PhysicalLocation(0),
                value: 100,
            },
            Ir2Instruction::Init {
                target: PhysicalLocation(1),
                value: 200,
            },
        ];
        let start_tape = vec![0, 5, 10];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![100, 205, 10]);
    }

    // Move {
    //     target: PhysicalLocation,
    //     source: PhysicalLocation,
    // },

    #[test]
    fn test_move_instruction1() {
        let instructions = vec![Ir2Instruction::Move {
            target: PhysicalLocation(1),
            source: PhysicalLocation(0),
        }];
        let start_tape = vec![5, 0, 0];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 5, 0]);
    }

    #[test]
    fn test_move_instruction2() {
        let instructions = vec![Ir2Instruction::Move {
            target: PhysicalLocation(2),
            source: PhysicalLocation(1),
        }];
        let start_tape = vec![0, 10, 1];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 11]);
    }

    // NMove {
    //     targets: Vec<PhysicalLocation>,
    //     source: PhysicalLocation,
    // },

    #[test]
    fn test_nmove_instruction1() {
        let instructions = vec![Ir2Instruction::NMove {
            targets: vec![PhysicalLocation(0), PhysicalLocation(2)],
            source: PhysicalLocation(1),
        }];
        let start_tape = vec![0, 10, 0];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![10, 0, 10]);
    }

    #[test]
    fn test_nmove_instruction2() {
        let instructions = vec![Ir2Instruction::NMove {
            targets: vec![PhysicalLocation(1), PhysicalLocation(2)],
            source: PhysicalLocation(0),
        }];
        let start_tape = vec![5, 2, 3];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 7, 8]);
    }

    // BulkMove {
    //     target: PhysicalLocation,
    //     source: PhysicalLocation,
    //     size: usize,
    // },

    #[test]
    fn test_bulk_move_instruction1() {
        let instructions = vec![Ir2Instruction::BulkMove {
            target: PhysicalLocation(3),
            source: PhysicalLocation(0),
            size: 3,
        }];
        let start_tape = vec![1, 2, 3, 0, 0, 0];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 1, 2, 3]);
    }

    #[test]
    fn test_bulk_move_instruction2() {
        let instructions = vec![Ir2Instruction::BulkMove {
            target: PhysicalLocation(3),
            source: PhysicalLocation(0),
            size: 3,
        }];
        let start_tape = vec![1, 2, 3, 10, 10, 10];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 11, 12, 13]);
    }

    // BulkNMove {
    //     targets: Vec<PhysicalLocation>,
    //     source: PhysicalLocation,
    //     size: usize,
    // },

    #[test]
    fn test_bulk_nmove_instruction1() {
        let instructions = vec![Ir2Instruction::BulkNMove {
            targets: vec![PhysicalLocation(3), PhysicalLocation(6)],
            source: PhysicalLocation(0),
            size: 3,
        }];

        let start_tape = vec![1, 2, 3, 0, 0, 0, 0, 0, 0];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 1, 2, 3, 1, 2, 3]);
    }

    #[test]
    fn test_bulk_nmove_instruction2() {
        let instructions = vec![Ir2Instruction::BulkNMove {
            targets: vec![PhysicalLocation(3), PhysicalLocation(6)],
            source: PhysicalLocation(0),
            size: 3,
        }];
        let start_tape = vec![1, 2, 3, 10, 10, 10, 20, 20, 20];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 11, 12, 13, 21, 22, 23]);
    }

    // MoveFromIndirect {
    //     base: PhysicalLocation,
    //     offset: PhysicalLocation,
    //     output: PhysicalLocation,
    // },

    #[test]
    fn test_move_from_indirect_instruction1() {
        let instructions = vec![Ir2Instruction::MoveFromIndirect {
            base: PhysicalLocation(1),
            offset: PhysicalLocation(1),
            output: PhysicalLocation(0),
        }];
        // output, offset, array[0], array[1], array[2]
        let start_tape = vec![0, 1, 10, 20, 30];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![10, 1, 0, 20, 30]);
    }

    #[test]
    fn test_move_from_indirect_instruction2() {
        let instructions = vec![Ir2Instruction::MoveFromIndirect {
            base: PhysicalLocation(1),
            offset: PhysicalLocation(1),
            output: PhysicalLocation(0),
        }];
        // output, offset, array[0], array[1], array[2]
        let start_tape = vec![6, 1, 10, 20, 30];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![16, 1, 0, 20, 30]);
    }

    // MoveFromIndirectConstant {
    //     base: PhysicalLocation,
    //     offset: usize,
    //     output: PhysicalLocation,
    // },

    #[test]
    fn test_move_from_indirect_constant_instruction1() {
        let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
            base: PhysicalLocation(1),
            offset: 1,
            output: PhysicalLocation(0),
        }];
        // output, array[0], array[1], array[2]
        let start_tape = vec![0, 10, 20, 30];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![20, 10, 0, 30]);
    }

    #[test]
    fn test_move_from_indirect_constant_instruction2() {
        let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
            base: PhysicalLocation(1),
            offset: 1,
            output: PhysicalLocation(0),
        }];
        // output, array[0], array[1], array[2]
        let start_tape = vec![6, 10, 20, 30];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![26, 10, 0, 30]);
    }

    // MoveToIndirect {
    //     base: PhysicalLocation,
    //     offset: PhysicalLocation,
    //     value: PhysicalLocation,
    // },

    #[test]
    fn test_move_to_indirect_instruction1() {
        let instructions = vec![Ir2Instruction::MoveToIndirect {
            base: PhysicalLocation(2),
            offset: PhysicalLocation(1),
            value: PhysicalLocation(0),
        }];

        // value, offset, array[0], array[1], array[2]
        let start_tape = vec![42, 1, 10, 0, 30];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 1, 10, 42, 30]);
    }

    #[test]
    fn test_move_to_indirect_instruction2() {
        let instructions = vec![Ir2Instruction::MoveToIndirect {
            base: PhysicalLocation(2),
            offset: PhysicalLocation(1),
            value: PhysicalLocation(0),
        }];

        // value, offset, array[0], array[1], array[2]
        let start_tape = vec![42, 1, 10, 4, 30];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 1, 10, 46, 30]);
    }

    // MoveToIndirectConstant {
    //     base: PhysicalLocation,
    //     offset: usize,
    //     value: PhysicalLocation,
    // },

    #[test]
    fn test_move_to_indirect_constant_instruction1() {
        let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
            base: PhysicalLocation(2),
            offset: 1,
            value: PhysicalLocation(0),
        }];

        // value, offset, array[0], array[1], array[2]
        let start_tape = vec![42, 1, 10, 0, 30];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 1, 10, 42, 30]);
    }

    #[test]
    fn test_move_to_indirect_constant_instruction2() {
        let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
            base: PhysicalLocation(2),
            offset: 1,
            value: PhysicalLocation(0),
        }];

        // value, offset, array[0], array[1], array[2]
        let start_tape = vec![42, 1, 10, 4, 30];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 1, 10, 46, 30]);
    }

    // MoveFromIndirectArray {
    //     base: PhysicalLocation,
    //     offset: PhysicalLocation,
    //     element_size: usize,
    //     output: PhysicalLocation,
    // },
    #[test]
    fn test_move_from_indirect_array_instruction1() {
        let instructions = vec![Ir2Instruction::MoveFromIndirectArray {
            base: PhysicalLocation(4),
            offset: PhysicalLocation(0),
            element_size: 3,
            output: PhysicalLocation(1),
        }];

        let start_tape = vec![1, 0, 0, 0, 10, 11, 12, 20, 21, 22];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![1, 20, 21, 22, 10, 11, 12, 0, 0, 0]);
    }

    #[test]
    fn test_move_from_indirect_array_instruction2() {
        let instructions = vec![Ir2Instruction::MoveFromIndirectArray {
            base: PhysicalLocation(4),
            offset: PhysicalLocation(0),
            element_size: 3,
            output: PhysicalLocation(1),
        }];

        let start_tape = vec![0, 0, 0, 0, 10, 11, 12, 20, 21, 22];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 10, 11, 12, 0, 0, 0, 20, 21, 22]);
    }

    // MoveFromIndirectArrayConstant {
    //     base: PhysicalLocation,
    //     offset: usize,
    //     element_size: usize,
    //     output: PhysicalLocation,
    // },
    #[test]
    fn test_move_from_indirect_array_constant_instruction1() {
        let instructions = vec![Ir2Instruction::MoveFromIndirectArrayConstant {
            base: PhysicalLocation(1),
            offset: 1,
            element_size: 3,
            output: PhysicalLocation(7),
        }];

        let start_tape = vec![99, 10, 11, 12, 20, 21, 22, 0, 0, 0];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![99, 10, 11, 12, 0, 0, 0, 20, 21, 22]);
    }

    #[test]
    fn test_move_from_indirect_array_constant_instruction2() {
        let instructions = vec![Ir2Instruction::MoveFromIndirectArrayConstant {
            base: PhysicalLocation(1),
            offset: 0,
            element_size: 3,
            output: PhysicalLocation(7),
        }];
        // source = base + 0*3 = PhysicalLocation(1)
        let start_tape = vec![99, 10, 11, 12, 20, 21, 22, 0, 0, 0];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![99, 0, 0, 0, 20, 21, 22, 10, 11, 12]);
    }

    // MoveToIndirectArray {
    //     base: PhysicalLocation,
    //     offset: PhysicalLocation,
    //     element_size: usize,
    //     value: PhysicalLocation,
    // },
    #[test]
    fn test_move_to_indirect_array_instruction1() {
        let instructions = vec![Ir2Instruction::MoveToIndirectArray {
            base: PhysicalLocation(4),
            offset: PhysicalLocation(0),
            element_size: 3,
            value: PhysicalLocation(1),
        }];
        // tape[offset]=tape[0]=1 → dest = base + 1*3 = PhysicalLocation(7)
        // layout: [offset, value(x3), padding(x3), array[0](x3), array[1](x3)]
        let start_tape = vec![1, 10, 11, 12, 0, 0, 0, 0, 0, 0];
        //                     ^off ^---val---  ^--arr[0]-- ^--arr[1]--
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![1, 0, 0, 0, 0, 0, 0, 10, 11, 12]);
    }

    #[test]
    fn test_move_to_indirect_array_instruction2() {
        let instructions = vec![Ir2Instruction::MoveToIndirectArray {
            base: PhysicalLocation(4),
            offset: PhysicalLocation(0),
            element_size: 3,
            value: PhysicalLocation(1),
        }];
        // tape[offset]=tape[0]=0 → dest = base + 0*3 = PhysicalLocation(4), accumulates
        let start_tape = vec![0, 5, 6, 7, 10, 11, 12, 20, 21, 22];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 0, 15, 17, 19, 20, 21, 22]);
    }

    // MoveToIndirectArrayConstant {
    //     base: PhysicalLocation,
    //     offset: usize,
    //     element_size: usize,
    //     value: PhysicalLocation,
    // },
    #[test]
    fn test_move_to_indirect_array_constant_instruction1() {
        let instructions = vec![Ir2Instruction::MoveToIndirectArrayConstant {
            base: PhysicalLocation(3),
            offset: 1,
            element_size: 3,
            value: PhysicalLocation(0),
        }];
        // dest = base + 1*3 = PhysicalLocation(6)
        let start_tape = vec![10, 11, 12, 0, 0, 0, 0, 0, 0];
        //                     ^---val---  ^--arr[0]-- ^--arr[1]--
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 0, 0, 0, 10, 11, 12]);
    }

    #[test]
    fn test_move_to_indirect_array_constant_instruction2() {
        let instructions = vec![Ir2Instruction::MoveToIndirectArrayConstant {
            base: PhysicalLocation(3),
            offset: 0,
            element_size: 3,
            value: PhysicalLocation(0),
        }];
        // dest = base + 0*3 = PhysicalLocation(3), accumulates
        let start_tape = vec![5, 6, 7, 10, 11, 12, 20, 21, 22];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 15, 17, 19, 20, 21, 22]);
    }

    // ClearBulk {
    //     target: PhysicalLocation,
    //     size: usize,
    // },
    #[test]
    fn test_clear_bulk_instruction1() {
        let instructions = vec![Ir2Instruction::ClearBulk {
            target: PhysicalLocation(1),
            size: 3,
        }];
        let start_tape = vec![5, 10, 20, 30, 40];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![5, 0, 0, 0, 40]);
    }

    #[test]
    fn test_clear_bulk_instruction2() {
        let instructions = vec![Ir2Instruction::ClearBulk {
            target: PhysicalLocation(0),
            size: 5,
        }];
        let start_tape = vec![1, 2, 3, 4, 5];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 0, 0]);
    }

    // ClearIndirect {
    //     base: PhysicalLocation,
    //     offset: PhysicalLocation,
    // },
    #[test]
    fn test_clear_indirect_instruction1() {
        let instructions = vec![Ir2Instruction::ClearIndirect {
            base: PhysicalLocation(1),
            offset: PhysicalLocation(0),
        }];
        // tape[offset]=tape[0]=2 → clear tape[base + 2] = tape[3]
        let start_tape = vec![2, 10, 20, 30, 40];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![2, 10, 20, 0, 40]);
    }

    #[test]
    fn test_clear_indirect_instruction2() {
        let instructions = vec![Ir2Instruction::ClearIndirect {
            base: PhysicalLocation(1),
            offset: PhysicalLocation(0),
        }];
        // tape[offset]=tape[0]=0 → clear tape[base + 0] = tape[1]
        let start_tape = vec![0, 99, 20, 30, 40];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 20, 30, 40]);
    }

    // ClearIndirectConstant {
    //     base: PhysicalLocation,
    //     offset: usize,
    // },
    #[test]
    fn test_clear_indirect_constant_instruction1() {
        let instructions = vec![Ir2Instruction::ClearIndirectConstant {
            base: PhysicalLocation(1),
            offset: 2,
        }];
        // clear tape[base + offset] = tape[3]
        let start_tape = vec![5, 10, 20, 30, 40];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![5, 10, 20, 0, 40]);
    }

    #[test]
    fn test_clear_indirect_constant_instruction2() {
        let instructions = vec![Ir2Instruction::ClearIndirectConstant {
            base: PhysicalLocation(1),
            offset: 0,
        }];
        // clear tape[base + offset] = tape[1]
        let start_tape = vec![5, 99, 20, 30, 40];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![5, 0, 20, 30, 40]);
    }

    // ClearIndirectArray {
    //     base: PhysicalLocation,
    //     offset: PhysicalLocation,
    //     element_size: usize,
    // },
    #[test]
    fn test_clear_indirect_array_instruction1() {
        let instructions = vec![Ir2Instruction::ClearIndirectArray {
            base: PhysicalLocation(1),
            offset: PhysicalLocation(0),
            element_size: 3,
        }];
        // tape[offset]=tape[0]=1 → clear tape[base + 1*3 .. +3] = tape[4..7]
        let start_tape = vec![1, 10, 11, 12, 20, 21, 22, 30, 31, 32];
        //                     ^   ^---arr[0]-- ^---arr[1]-- ^---arr[2]--
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![1, 10, 11, 12, 0, 0, 0, 30, 31, 32]);
    }

    #[test]
    fn test_clear_indirect_array_instruction2() {
        let instructions = vec![Ir2Instruction::ClearIndirectArray {
            base: PhysicalLocation(1),
            offset: PhysicalLocation(0),
            element_size: 3,
        }];
        // tape[offset]=tape[0]=0 → clear tape[base + 0*3 .. +3] = tape[1..4]
        let start_tape = vec![0, 10, 11, 12, 20, 21, 22, 30, 31, 32];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![0, 0, 0, 0, 20, 21, 22, 30, 31, 32]);
    }

    // ClearIndirectArrayConstant {
    //     base: PhysicalLocation,
    //     offset: usize,
    //     element_size: usize,
    // },
    #[test]
    fn test_clear_indirect_array_constant_instruction1() {
        let instructions = vec![Ir2Instruction::ClearIndirectArrayConstant {
            base: PhysicalLocation(0),
            offset: 2,
            element_size: 3,
        }];
        // clear tape[base + 2*3 .. +3] = tape[6..9]
        let start_tape = vec![10, 11, 12, 20, 21, 22, 30, 31, 32];
        //                     ^---arr[0]-- ^---arr[1]-- ^---arr[2]--
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![10, 11, 12, 20, 21, 22, 0, 0, 0]);
    }

    #[test]
    fn test_clear_indirect_array_constant_instruction2() {
        let instructions = vec![Ir2Instruction::ClearIndirectArrayConstant {
            base: PhysicalLocation(0),
            offset: 1,
            element_size: 3,
        }];
        // clear tape[base + 1*3 .. +3] = tape[3..6]
        let start_tape = vec![10, 11, 12, 20, 21, 22, 30, 31, 32];
        let result_tape = test_instructions(instructions, start_tape);
        assert_eq!(result_tape, vec![10, 11, 12, 0, 0, 0, 30, 31, 32]);
    }
}
