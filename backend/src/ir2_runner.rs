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
        run_function(main_id, &global_context, vec![]);
    } else {
        panic!("No main function found in function name mapping");
    }
}

fn run_function(function_id: FunctionId, global_context: &Ir2GlobalContext, parameters: Vec<u8>) {
    let function = global_context
        .functions
        .iter()
        .find(|f| f.id == function_id)
        .expect("Function ID not found in global context");

    let mut local_memory = vec![0u8; function.stack_size];

    assert!(
        parameters.len() <= local_memory.len(),
        "Not enough local memory to hold parameters"
    );

    // Set the first section of local memory to the parameters
    for (i, byte) in parameters.iter().enumerate() {
        local_memory[i] = *byte;
    }

    for instruction in &function.code {
        // match instruction {
        //     Ir2Instruction::Clear { target } => {
        //         local_memory[*target] = 0;
        //     }
        //     Ir2Instruction::Init { target, value } => {
        //         local_memory[*target] = *value;
        //     }

        // }
    }
}
