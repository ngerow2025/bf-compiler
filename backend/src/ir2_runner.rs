use std::{collections::HashMap, vec};

use crate::{
    ir2::{Ir2Function, Ir2Instruction},
    parser::FunctionId,
};

struct Ir2GlobalContext<'a> {
    pub functions: &'a Vec<Ir2Function>,
    pub function_name_mapping: HashMap<FunctionId, String>,
}

pub fn run_ir2(
    ir_functions: &Vec<Ir2Function>,
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

    fn read(&self, index: usize) -> u8 {
        self.view.data[self.offset + index]
    }

    fn write(&mut self, index: usize, value: u8) {
        self.view.data[self.offset + index] = value;
    }

    fn get_offset_view(&mut self, offset: usize) -> TapeView {
        TapeView {
            view: self.view,
            offset: self.offset + offset,
        }
    }
}

fn run_function(function_id: FunctionId, global_context: &Ir2GlobalContext, memory: TapeView) {
    let function = global_context
        .functions
        .iter()
        .find(|f| f.id == function_id)
        .expect("Function ID not found in global context");

    // for instruction in &function.code {
    //     match instruction {
    //         Ir2Instruction::BulkMove { target, source, size } => {
    //             for i in 0..*size {
    //                 local_memory[target.0 + i] = local_memory[source.0 + i];
    //                 local_memory[source.0 + i] = 0;
    //             }
    //         }
    //         Ir2Instruction::BulkNMove { targets, source, size } => {
    //             for i in 0..*size {
    //                 for target in targets {
    //                     local_memory[target.0 + i] = local_memory[source.0 + i];
    //                 }
    //                 local_memory[source.0 + i] = 0;
    //             }
    //         }
    //         Ir2Instruction::Call { function_id, new_stack_frame_base } => {
    //             let new_parameters = local_memory[new_stack_frame_base.0..].to_vec();
    //             let end_state = run_function(*function_id, global_context, new_parameters);
    //             local_memory[new_stack_frame_base.0..] = end_state;
    //         }
    //         Ir2Instruction::Clear { target } => {
    //             local_memory[target.0] = 0;
    //         }
    //         Ir2Instruction::ClearBulk { target, size } => {
    //             for i in 0..*size {
    //                 local_memory[target.0 + i] = 0;
    //             }
    //         }
    //         Ir2Instruction::ClearIndirect { base, offset } => {
    //             let address = base.0 as usize + local_memory[offset.0] as usize;
    //             local_memory[address] = 0;
    //         }
    //         Ir2Instruction::ClearIndirectArray { base, offset, element_size } => {
    //             let start_address = base.0 as usize + local_memory[offset.0] as usize * (*element_size);
    //             for i in 0..*element_size {
    //                 local_memory[start_address + i] = 0;
    //             }
    //         }
    //         Ir2Instruction::ClearIndirectArrayConstant { base, offset, element_size } => {
    //             let start_address = base.0 as usize + (*offset as usize) * (*element_size);
    //             for i in 0..*element_size {
    //                 local_memory[start_address + i] = 0;
    //             }
    //         }
    //         Ir2Instruction::ClearIndirectConstant { base, offset } => {
    //             let address = base.0 as usize + (*offset as usize);
    //             local_memory[address] = 0;
    //         }
    //         Ir2Instruction::
    //         Ir2Instruction::Init { target, value } => {
    //             local_memory[target.0] = *value;
    //         }

    //     }
    // }
}
