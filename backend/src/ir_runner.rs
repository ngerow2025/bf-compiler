use std::{cell::UnsafeCell, collections::HashMap, vec};

use crate::{
    input_grabbing::{RawModeGuard, read_single_byte},
    ir::{IrFunction, IrInstruction, IrMoveOperand, IrRegisterId},
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

struct BumpAllocator<'alloc> {
    memory: UnsafeCell<Box<[u8]>>,
    offset: usize,
    capacity: usize,
    _self_lifetime: &'alloc (),
}

impl<'alloc> BumpAllocator<'alloc> {
    pub fn new(size: usize) -> Self {
        Self {
            memory: UnsafeCell::new(vec![0; size].into_boxed_slice()),
            offset: 0,
            capacity: size,
            _self_lifetime: &(),
        }
    }

    pub fn allocate(&mut self, size: usize) -> &'alloc mut [u8] {
        if self.offset + size > self.capacity {
            panic!("Out of memory in bump allocator");
        }
        let start = self.offset;
        self.offset += size;
        unsafe {
            let ptr = (&raw mut **self.memory.get()).cast::<u8>().add(start);
            std::slice::from_raw_parts_mut(ptr, size)
        }
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
    let mut local_allocator = BumpAllocator::new(total_size);
    let mut local_variables: HashMap<IrRegisterId, &mut [u8]> = HashMap::new();
    for (var_id, reg) in &function.registers {
        let var_memory = local_allocator.allocate(reg.size);
        local_variables.insert(*var_id, var_memory);
    }

    for (i, param) in function.parameters.iter().enumerate() {
        let param_memory = local_variables
            .get_mut(param)
            .expect("Parameter register not found in local variables");

        param_memory.copy_from_slice(&parameters[i]);
    }

    for instruction in &function.code {
        match instruction {
            IrInstruction::Move { target, source } => {
                match source {
                    IrMoveOperand::Register(source_reg) => {
                        //this is basically a memcpy from source_reg to target
                        let (source_ptr, source_len) = {
                            let source_mem = local_variables
                                .get(source_reg)
                                .expect("Source register not found in local variables");
                            (source_mem.as_ptr(), source_mem.len())
                        }; // borrow dropped here
                        let target_mem = local_variables
                            .get_mut(target)
                            .expect("Target register not found in local variables");
                        assert_eq!(
                            source_len,
                            target_mem.len(),
                            "Source and target registers must be the same size for move operation"
                        );
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                source_ptr,
                                target_mem.as_mut_ptr(),
                                source_len,
                            );
                        }
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
                        let target_mem = local_variables
                            .get_mut(target)
                            .expect("Target register not found in local variables");
                        assert_eq!(
                            raw_information.len(),
                            target_mem.len(),
                            "Integer literal length must match target register size"
                        );
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                raw_information.as_ptr(),
                                target_mem.as_mut_ptr(),
                                raw_information.len(),
                            );
                        }
                    }
                    IrMoveOperand::StringLiteral(string_literal) => {
                        let target_mem = local_variables
                            .get_mut(target)
                            .expect("Target register not found in local variables");
                        let string_bytes = string_literal.as_bytes();
                        assert_eq!(
                            string_bytes.len(),
                            target_mem.len(),
                            "String literal length must match target register size"
                        );
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                string_bytes.as_ptr(),
                                target_mem.as_mut_ptr(),
                                string_bytes.len(),
                            );
                        }
                    }
                }
            }
            IrInstruction::Input { target } => {
                let input_byte = read_single_byte();
                let target_mem = local_variables
                    .get_mut(target)
                    .expect("Target register not found in local variables");
                assert_eq!(
                    target_mem.len(),
                    1,
                    "Input instruction target register must be 1 byte in size"
                );
                target_mem[0] = input_byte;
            }
            IrInstruction::Output { element } => {
                let source_mem = local_variables
                    .get(element)
                    .expect("Source register not found in local variables");
                let output_str = String::from_utf8_lossy(source_mem);
                print!("{}", output_str);
            }
            IrInstruction::IndirectRead {
                base,
                offset,
                output,
            } => {
                let offset_value = {
                    let offset_mem = local_variables
                        .get(offset)
                        .expect("Offset register not found in local variables");
                    assert!(
                        offset_mem.len() == 1,
                        "Offset register must be 1 byte in size for indirect read"
                    );
                    offset_mem[0] as usize
                };
                let element_value = {
                    let base_mem = local_variables
                        .get(base)
                        .expect("Base register not found in local variables");
                    *base_mem
                        .get(offset_value)
                        .expect("Out of Range access in indirect read instruction")
                };
                let output_mem = local_variables
                    .get_mut(output)
                    .expect("Output register not found in local variables");
                assert!(
                    output_mem.len() == 1,
                    "Output register must be 1 byte in size for indirect read"
                );
                output_mem[0] = element_value;
            }
            IrInstruction::IndirectWrite {
                base,
                offset,
                value,
            } => {
                let (offset_value, element_value) = {
                    let offset_mem = local_variables
                        .get(offset)
                        .expect("Offset register not found in local variables");
                    assert!(
                        offset_mem.len() == 1,
                        "Offset register must be 1 byte in size for indirect write"
                    );
                    let value_mem = local_variables
                        .get(value)
                        .expect("Value register not found in local variables");
                    assert!(
                        value_mem.len() == 1,
                        "Value register must be 1 byte in size for indirect write"
                    );
                    (offset_mem[0] as usize, value_mem[0])
                };
                let base_mem = local_variables
                    .get_mut(base)
                    .expect("Base register not found in local variables");
                if offset_value >= base_mem.len() {
                    panic!("Out of Range access in indirect write instruction");
                }
                base_mem[offset_value] = element_value;
            }
            IrInstruction::Call {
                function_id,
                parameters,
                output: _,
            } => {
                let parameter_values: Vec<Vec<u8>> = parameters
                    .iter()
                    .map(|param| {
                        let param_mem = local_variables
                            .get(param)
                            .expect("Parameter register not found in local variables");
                        param_mem.to_vec()
                    })
                    .collect();
                run_function(*function_id, global_context, parameter_values);
            }
        }
    }
}
