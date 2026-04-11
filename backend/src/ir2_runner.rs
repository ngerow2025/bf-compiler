use std::{collections::HashMap, vec};

use crate::{
    input_grabbing::read_single_byte, ir2::{Ir2Function, Ir2Instruction}, parser::FunctionId
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

    for instruction in &function.code {
        match instruction {
            Ir2Instruction::BulkMove {
                target,
                source,
                size,
            } => {
                for i in 0..*size {
                    let value = memory.read(source.0 + i);
                    memory.write(target.0 + i, value);
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
                        memory.write(target.0 + i, read_value);
                    }
                    memory.write(source.0 + i, 0);
                }
            }
            Ir2Instruction::Call {
                function_id,
                new_stack_frame_base,
            } => {
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
                memory.write(target.0, *value);
            }
            Ir2Instruction::Move { target, source } => {
                let value = memory.read(source.0);
                memory.write(target.0, value);
                memory.write(source.0, 0);
            }
            Ir2Instruction::NMove { targets, source } => {
                let value = memory.read(source.0);
                for target in targets {
                    memory.write(target.0, value);
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
                memory.write(output.0, value);
                memory.write(resolved_address, 0);
            }
            Ir2Instruction::MoveFromIndirectConstant {
                base,
                offset,
                output,
            } => {
                let resolved_address = base.0 as usize + (*offset as usize);
                let value = memory.read(resolved_address);
                memory.write(output.0, value);
                memory.write(resolved_address, 0);
            }
            Ir2Instruction::MoveToIndirect {
                base,
                offset,
                value,
            } => {
                let resolved_address = base.0 as usize + memory.read(offset.0) as usize;
                let read_value = memory.read(value.0);
                memory.write(resolved_address, read_value);
                memory.write(value.0, 0);
            }
            Ir2Instruction::MoveToIndirectConstant {
                base,
                offset,
                value,
            } => {
                let address = base.0 as usize + (*offset as usize);
                let read_value = memory.read(value.0);
                memory.write(address, read_value);
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
                    memory.write(output.0 + i, value);
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
                    memory.write(start_address + i, read_value);
                    memory.write(value.0 + i, 0);
                }
            },
            Ir2Instruction::MoveToIndirectArrayConstant {
                base,
                offset,
                element_size,
                value,
            } => {
                let start_address = base.0 as usize + (*offset as usize) * (*element_size);
                for i in 0..*element_size {
                    let read_value = memory.read(value.0 + i);
                    memory.write(start_address + i, read_value);
                    memory.write(value.0 + i, 0);
                }
            },
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
            },
            Ir2Instruction::Output { element } => {
                let output_byte = memory.read(element.0);
                print!("{}", output_byte as char);
            }
        }
    }
}
