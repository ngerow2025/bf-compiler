use std::{
    collections::HashMap,
    io::{self, Write},
};

use crate::{
    input_grabbing::read_single_byte,
    ir2::{Ir2Function, Ir2Instruction},
    ir2_memory_representation::{PhysicalLocation, PhysicalSlot, Tape, TapeView},
    parser::FunctionId,
    util::{cartesian_product_iter, transpose},
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

fn run_function(function_id: FunctionId, global_context: &Ir2GlobalContext, mut memory: TapeView) {
    let function = global_context
        .functions
        .iter()
        .find(|f| f.id == function_id)
        .expect("Function ID not found in global context");

    run_instructions(&function.code, global_context, &mut memory);
}

fn assert_non_overlapping_ranges(a: PhysicalSlot, b: PhysicalSlot) {
    let a_end = a.get_start() + a.get_size();
    let b_end = b.get_start() + b.get_size();
    assert!(a_end <= b.get_start() || b_end <= a.get_start());
}

fn assert_offset_not_in_array_min_span(
    offset: PhysicalSlot,
    base: PhysicalLocation,
    resolved_address: PhysicalSlot,
) {
    let array_range = PhysicalSlot::from_start_end_inclusive(base, resolved_address.get_end());
    assert!(
        !offset.intersects(&array_range),
        "Offset location {:?} intersects with array range {:?} which is likely a bug",
        offset,
        array_range
    );
}

fn run_instructions(
    instructions: &[Ir2Instruction],
    global_context: &Ir2GlobalContext,
    memory: &mut TapeView,
) {
    for instruction in instructions {
        match instruction {
            Ir2Instruction::Call {
                function_id,
                new_stack_frame_base,
            } => {
                run_function(
                    *function_id,
                    global_context,
                    memory.get_offset_view(*new_stack_frame_base),
                );
            }
            Ir2Instruction::Clear { target } => {
                for location in target.get_all_locations() {
                    memory.write(location, 0);
                }
            }
            Ir2Instruction::ClearIndirect { base, offset } => {
                assert_eq!(
                    offset.get_size(),
                    1,
                    "Offset for ClearIndirect must be a single byte"
                );
                let resoved_address =
                    *base + memory.read(*offset.get_all_locations().first().unwrap()) as usize;

                //it is most likely a bug if the offset location is inside of the array
                assert_offset_not_in_array_min_span(
                    *offset,
                    *base,
                    PhysicalSlot::from_start_size(resoved_address, 1),
                );

                memory.write(resoved_address, 0);
            }
            Ir2Instruction::ClearIndirectArray {
                base,
                offset,
                element_size,
            } => {
                assert_eq!(
                    offset.get_size(),
                    1,
                    "Offset for ClearIndirectArray must be a single byte"
                );
                let resolved_address = *base
                    + memory.read(*offset.get_all_locations().first().unwrap()) as usize
                        * *element_size;
                let resolved_slot = PhysicalSlot::from_start_size(resolved_address, *element_size);

                //it is most likely a bug if the offset location is inside of the array
                //minimum possible array span is from base.0 to start_address (inclusive)
                assert_offset_not_in_array_min_span(*offset, *base, resolved_slot);

                for location in resolved_slot.get_all_locations() {
                    memory.write(location, 0);
                }
            }
            Ir2Instruction::ClearIndirectArrayConstant {
                base,
                offset,
                element_size,
            } => {
                let resolved_address = *base + *offset * (*element_size);
                let resolved_slot = PhysicalSlot::from_start_size(resolved_address, *element_size);
                for location in resolved_slot.get_all_locations() {
                    memory.write(location, 0);
                }
            }
            Ir2Instruction::ClearIndirectConstant { base, offset } => {
                let address = *base + *offset;
                memory.write(address, 0);
            }
            Ir2Instruction::Init { target, values } => {
                for (location, value) in target.get_all_locations().iter().zip(values) {
                    let prev_value = memory.read(*location);
                    memory.write(*location, value.wrapping_add(prev_value));
                }
            }
            Ir2Instruction::Move { target, source } => {
                assert_non_overlapping_ranges(*target, *source);

                for (source_loc, target_loc) in source
                    .get_all_locations()
                    .iter()
                    .zip(target.get_all_locations())
                {
                    let value = memory.read(*source_loc);
                    let prev_value = memory.read(target_loc);
                    memory.write(target_loc, value.wrapping_add(prev_value));
                    memory.write(*source_loc, 0);
                }
            }
            Ir2Instruction::NMove { targets, source } => {
                for target in targets {
                    assert_non_overlapping_ranges(*target, *source);
                }

                for (a, b) in cartesian_product_iter(targets) {
                    assert_non_overlapping_ranges(*a, *b);
                }

                let targets = transpose(
                    targets
                        .iter()
                        .map(|t| t.get_all_locations().to_vec())
                        .collect(),
                );

                for (source_loc, target_locs) in
                    source.get_all_locations().iter().zip(targets)
                {
                    let value = memory.read(*source_loc);
                    for target_loc in target_locs {
                        let prev_value = memory.read(target_loc);
                        memory.write(target_loc, value.wrapping_add(prev_value));
                        memory.write(*source_loc, 0);
                    }
                }
            }
            Ir2Instruction::MoveFromIndirect {
                base,
                offset,
                output,
            } => {
                assert_eq!(
                    offset.get_size(),
                    1,
                    "Offset for MoveFromIndirect must be a single byte"
                );
                let resolved_address =
                    *base + memory.read(*offset.get_all_locations().first().unwrap()) as usize;
                let resolved_slot =
                    PhysicalSlot::from_start_size(resolved_address, output.get_size());

                assert_offset_not_in_array_min_span(*offset, *base, resolved_slot);

                assert_non_overlapping_ranges(*output, resolved_slot);

                assert_eq!(
                    output.get_size(),
                    resolved_slot.get_size(),
                    "Output slot size must match the size of the data being moved"
                );

                for (source_loc, target_loc) in resolved_slot
                    .get_all_locations()
                    .iter()
                    .zip(output.get_all_locations())
                {
                    let value = memory.read(*source_loc);
                    let prev_value = memory.read(target_loc);
                    memory.write(target_loc, value.wrapping_add(prev_value));
                    memory.write(*source_loc, 0);
                }
            }
            Ir2Instruction::MoveFromIndirectConstant {
                base,
                offset,
                output,
            } => {
                let resolved_address = *base + *offset;
                let resolved_slot =
                    PhysicalSlot::from_start_size(resolved_address, output.get_size());

                assert_non_overlapping_ranges(*output, resolved_slot);

                assert_eq!(
                    output.get_size(),
                    resolved_slot.get_size(),
                    "Output slot size must match the size of the data being moved"
                );

                for (source_loc, target_loc) in resolved_slot
                    .get_all_locations()
                    .iter()
                    .zip(output.get_all_locations())
                {
                    let value = memory.read(*source_loc);
                    let prev_value = memory.read(target_loc);
                    memory.write(target_loc, value.wrapping_add(prev_value));
                    memory.write(*source_loc, 0);
                }
            }
            Ir2Instruction::MoveToIndirect {
                base,
                offset,
                value,
            } => {
                let resolved_address =
                    *base + memory.read(*offset.get_all_locations().first().unwrap()) as usize;
                let resolved_slot =
                    PhysicalSlot::from_start_size(resolved_address, value.get_size());

                assert_offset_not_in_array_min_span(*offset, *base, resolved_slot);

                for (source_loc, target_loc) in value
                    .get_all_locations()
                    .iter()
                    .zip(resolved_slot.get_all_locations())
                {
                    let read_value = memory.read(*source_loc);
                    let prev_value = memory.read(target_loc);
                    memory.write(target_loc, read_value.wrapping_add(prev_value));
                    memory.write(*source_loc, 0);
                }
            }
            Ir2Instruction::MoveToIndirectConstant {
                base,
                offset,
                value,
            } => {
                let resolved_address = *base + *offset;
                let resolved_slot =
                    PhysicalSlot::from_start_size(resolved_address, value.get_size());

                assert_offset_not_in_array_min_span(*value, *base, resolved_slot);

                for (source_loc, target_loc) in value
                    .get_all_locations()
                    .iter()
                    .zip(resolved_slot.get_all_locations())
                {
                    let read_value = memory.read(*source_loc);
                    let prev_value = memory.read(target_loc);
                    memory.write(target_loc, read_value.wrapping_add(prev_value));
                    memory.write(*source_loc, 0);
                }
            }
            Ir2Instruction::Input { target } => {
                assert_eq!(
                    target.get_size(),
                    1,
                    "Input instruction target must be a single byte"
                );
                let input_byte = read_single_byte();
                memory.write(*target.get_all_locations().first().unwrap(), input_byte);
            }
            Ir2Instruction::Output { element } => {
                let output_byte = memory.read(*element.get_all_locations().first().unwrap());
                print!("{}", output_byte as char);
                io::stdout().flush().expect("failed to flush stdout");
            }
        }
    }
}

#[cfg(test)]
pub fn test_instructions(instructions: Vec<Ir2Instruction>, start_tape: Vec<u8>) -> Vec<u8> {
    let global_context = Ir2GlobalContext {
        functions: vec![],
        function_name_mapping: HashMap::new(),
    };
    let mut tape = Tape { data: start_tape };
    let mut tape_view = TapeView::new(&mut tape);

    run_instructions(&instructions, &global_context, &mut tape_view);
    tape.data
}

#[cfg(test)]
mod test {

    mod clear {
        // Clear {
        //     target: PhysicalSlot,
        // },
        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn single_cell() {
            let instructions = vec![Ir2Instruction::Clear {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
            }];
            let start_tape = vec![5, 10, 15];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 10, 15]);
        }

        #[test]
        fn multi_cell() {
            let instructions = vec![Ir2Instruction::Clear {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 2),
            }];
            let start_tape = vec![5, 10, 15];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![5, 0, 0]);
        }

        #[test]
        fn single_no_change() {
            let instructions = vec![Ir2Instruction::Clear {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            let start_tape = vec![5, 0, 15];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![5, 0, 15]);
        }

        #[test]
        fn multi_partial_change() {
            let instructions = vec![Ir2Instruction::Clear {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 2),
            }];
            let start_tape = vec![5, 0, 15];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 0, 15]);
        }

        #[test]
        fn multi_no_change() {
            let instructions = vec![Ir2Instruction::Clear {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
            }];
            let start_tape = vec![0, 0, 0, 2];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 0, 0, 2]);
        }
    }

    mod init {
        // Init {
        //     target: PhysicalSlot,
        //     values: Vec<u8>,
        // }
        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn single_cell_from_zero() {
            let instructions = vec![Ir2Instruction::Init {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                values: vec![42],
            }];
            let start_tape = vec![0, 10, 15];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![42, 10, 15]);
        }

        #[test]
        fn single_cell_from_non_zero() {
            let instructions = vec![Ir2Instruction::Init {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                values: vec![42],
            }];
            let start_tape = vec![5, 10, 15];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![47, 10, 15]);
        }

        #[test]
        fn multi_cell_from_zero() {
            let instructions = vec![Ir2Instruction::Init {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
                values: vec![1, 2, 3],
            }];
            let start_tape = vec![0, 0, 0, 98];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![1, 2, 3, 98]);
        }

        #[test]
        fn multi_cell_from_non_zero() {
            let instructions = vec![Ir2Instruction::Init {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
                values: vec![1, 2, 3],
            }];
            let start_tape = vec![5, 10, 15, 98];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![6, 12, 18, 98]);
        }

        #[test]
        fn multi_cell_partial_from_non_zero() {
            let instructions = vec![Ir2Instruction::Init {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
                values: vec![1, 2, 3],
            }];
            let start_tape = vec![5, 0, 15, 98];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![6, 2, 18, 98]);
        }
    }

    mod r#move {
        // Move {
        //     target: PhysicalSlot,
        //     source: PhysicalSlot,
        // },

        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn single_zero_target() {
            let instructions = vec![Ir2Instruction::Move {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
                source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
            }];
            let start_tape = vec![5, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 5, 0]);
        }

        #[test]
        fn single_zero_target_source() {
            let instructions = vec![Ir2Instruction::Move {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
                source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
            }];
            let start_tape = vec![0, 0, 3];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 0, 3]);
        }

        #[test]
        fn single_zero_source() {
            let instructions = vec![Ir2Instruction::Move {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
                source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
            }];
            let start_tape = vec![0, 98, 3];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 98, 3]);
        }

        #[test]
        fn multi_zero_target() {
            let instructions = vec![Ir2Instruction::Move {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
                source: PhysicalSlot::from_start_size(PhysicalLocation::new(4), 3),
            }];
            let start_tape = vec![0, 0, 0, 0, 1, 2, 3];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 1, 2, 3, 0, 0, 0]);
        }

        #[test]
        fn multi_partial_zero_target() {
            let instructions = vec![Ir2Instruction::Move {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
                source: PhysicalSlot::from_start_size(PhysicalLocation::new(4), 3),
            }];
            let start_tape = vec![0, 0, 0, 0, 1, 0, 3];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 1, 0, 3, 0, 0, 0]);
        }

        #[test]
        fn multi_zero_source() {
            let instructions = vec![Ir2Instruction::Move {
                target: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
                source: PhysicalSlot::from_start_size(PhysicalLocation::new(4), 3),
            }];
            let start_tape = vec![0, 8, 7, 6, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 8, 7, 6, 0, 0, 0]);
        }
    }

    mod nmove {
        // NMove {
        //     targets: Vec<PhysicalSlot>,
        //     source: PhysicalSlot,
        // },

        mod single_target {
            use crate::{
                ir2::Ir2Instruction,
                ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
                ir2_runner::test_instructions,
            };

            #[test]
            fn single_zero_target() {
                let instructions = vec![Ir2Instruction::NMove {
                    targets: vec![PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1)],
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                }];
                let start_tape = vec![5, 0, 0];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 5, 0]);
            }

            #[test]
            fn single_zero_target_source() {
                let instructions = vec![Ir2Instruction::NMove {
                    targets: vec![PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1)],
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                }];
                let start_tape = vec![0, 0, 3];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 0, 3]);
            }

            #[test]
            fn single_zero_source() {
                let instructions = vec![Ir2Instruction::NMove {
                    targets: vec![PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1)],
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                }];
                let start_tape = vec![0, 98, 3];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 98, 3]);
            }

            #[test]
            fn multi_zero_target() {
                let instructions = vec![Ir2Instruction::NMove {
                    targets: vec![PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3)],
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(4), 3),
                }];
                let start_tape = vec![0, 0, 0, 0, 1, 2, 3];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 1, 2, 3, 0, 0, 0]);
            }

            #[test]
            fn multi_partial_zero_target() {
                let instructions = vec![Ir2Instruction::NMove {
                    targets: vec![PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3)],
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(4), 3),
                }];
                let start_tape = vec![0, 0, 0, 0, 1, 0, 3];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 1, 0, 3, 0, 0, 0]);
            }

            #[test]
            fn multi_zero_source() {
                let instructions = vec![Ir2Instruction::NMove {
                    targets: vec![PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3)],
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(4), 3),
                }];
                let start_tape = vec![0, 8, 7, 6, 0, 0, 0];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 8, 7, 6, 0, 0, 0]);
            }
        }

        mod multiple_targets {
            use crate::{
                ir2::Ir2Instruction,
                ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
                ir2_runner::test_instructions,
            };

            #[test]
            fn single_multi_target() {
                let instructions = vec![Ir2Instruction::NMove {
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                    targets: vec![
                        PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
                        PhysicalSlot::from_start_size(PhysicalLocation::new(2), 1),
                    ],
                }];
                let start_tape = vec![1, 2, 3, 4, 5, 6];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 3, 4, 4, 5, 6]);
            }

            #[test]
            fn single_multi_target_zero_source() {
                let instructions = vec![Ir2Instruction::NMove {
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                    targets: vec![
                        PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
                        PhysicalSlot::from_start_size(PhysicalLocation::new(2), 1),
                    ],
                }];
                let start_tape = vec![0, 2, 3, 4, 5, 6];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 2, 3, 4, 5, 6]);
            }

            #[test]
            fn single_multi_target_zero_target() {
                let instructions = vec![Ir2Instruction::NMove {
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                    targets: vec![
                        PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
                        PhysicalSlot::from_start_size(PhysicalLocation::new(2), 1),
                    ],
                }];
                let start_tape = vec![1, 0, 0, 4, 5, 6];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 1, 1, 4, 5, 6]);
            }

            #[test]
            fn multi_multi_target() {
                let instructions = vec![Ir2Instruction::NMove {
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
                    targets: vec![
                        PhysicalSlot::from_start_size(PhysicalLocation::new(3), 3),
                        PhysicalSlot::from_start_size(PhysicalLocation::new(6), 3),
                    ],
                }];
                let start_tape = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 0, 0, 5, 7, 9, 8, 10, 12]);
            }

            #[test]
            fn multi_multi_target_zero_source() {
                let instructions = vec![Ir2Instruction::NMove {
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
                    targets: vec![
                        PhysicalSlot::from_start_size(PhysicalLocation::new(3), 3),
                        PhysicalSlot::from_start_size(PhysicalLocation::new(6), 3),
                    ],
                }];
                let start_tape = vec![0, 0, 0, 4, 5, 6, 7, 8, 9];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 0, 0, 4, 5, 6, 7, 8, 9]);
            }

            #[test]
            fn multi_multi_target_zero_target() {
                let instructions = vec![Ir2Instruction::NMove {
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
                    targets: vec![
                        PhysicalSlot::from_start_size(PhysicalLocation::new(3), 3),
                        PhysicalSlot::from_start_size(PhysicalLocation::new(6), 3),
                    ],
                }];
                let start_tape = vec![1, 2, 3, 0, 0, 0, 0, 0, 0];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 0, 0, 1, 2, 3, 1, 2, 3]);
            }

            #[test]
            fn multi_multi_target_partial_zero() {
                let instructions = vec![Ir2Instruction::NMove {
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
                    targets: vec![
                        PhysicalSlot::from_start_size(PhysicalLocation::new(3), 3),
                        PhysicalSlot::from_start_size(PhysicalLocation::new(6), 3),
                    ],
                }];
                let start_tape = vec![1, 0, 3, 4, 5, 6, 7, 8, 9];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 0, 0, 5, 5, 9, 8, 8, 12]);
            }

            #[test]
            fn multi_multi_source_zero_target_zero() {
                let instructions = vec![Ir2Instruction::NMove {
                    source: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 3),
                    targets: vec![
                        PhysicalSlot::from_start_size(PhysicalLocation::new(3), 3),
                        PhysicalSlot::from_start_size(PhysicalLocation::new(6), 3),
                    ],
                }];
                let start_tape = vec![0, 0, 0, 0, 0, 0, 0, 0, 0];
                let result_tape = test_instructions(instructions, start_tape);
                assert_eq!(result_tape, vec![0, 0, 0, 0, 0, 0, 0, 0, 0]);
            }
        }
    }

    mod move_from_indirect {
        // MoveFromIndirect {
        //     base: PhysicalLocation,
        //     offset: PhysicalSlot, // Restricted to 1 byte in length
        //     output: PhysicalSlot,
        // },

        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn single_zero_target() {
            let instructions = vec![Ir2Instruction::MoveFromIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // offset at 0 is 2. base is 4. source = 4 + 2 = 6.
            // target at 1 is 0. source at 6 is 5.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 5];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 6 is cleared, target at 1 gets 5 added to it.
            assert_eq!(result_tape, vec![2, 5, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn single_non_zero_target() {
            let instructions = vec![Ir2Instruction::MoveFromIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // offset at 0 is 2. base is 4. source = 4 + 2 = 6.
            // target at 1 is 3. source at 6 is 5.
            let start_tape = vec![2, 3, 0, 0, 0, 0, 5];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 6 is cleared, target at 1 gets 5 added to it (3 + 5 = 8).
            assert_eq!(result_tape, vec![2, 8, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn single_zero_target_source() {
            let instructions = vec![Ir2Instruction::MoveFromIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // target at 1 is 0. source at 6 is 0.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn single_zero_source() {
            let instructions = vec![Ir2Instruction::MoveFromIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // target at 1 is 3. source at 6 is 0.
            let start_tape = vec![2, 3, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 3, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn multi_zero_target() {
            let instructions = vec![Ir2Instruction::MoveFromIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // offset at 0 is 2. base is 4. source starts at 6.
            // target at 1 is size 3 (values 0, 0, 0).
            // source at 6 is size 3 (values 10, 20, 30).
            let start_tape = vec![2, 0, 0, 0, 0, 0, 10, 20, 30];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 6, 7, 8 is cleared, targets at 1, 2, 3 receive the values.
            assert_eq!(result_tape, vec![2, 10, 20, 30, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn multi_partial_zero_target() {
            let instructions = vec![Ir2Instruction::MoveFromIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // target at 1 is size 3 (values 5, 0, 5).
            // source at 6 is size 3 (values 10, 20, 30).
            let start_tape = vec![2, 5, 0, 5, 0, 0, 10, 20, 30];
            let result_tape = test_instructions(instructions, start_tape);
            // Adds values to target and clears source
            assert_eq!(result_tape, vec![2, 15, 20, 35, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn multi_zero_source() {
            let instructions = vec![Ir2Instruction::MoveFromIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // target at 1 is size 3 (values 5, 6, 7).
            // source at 6 is size 3 (values 0, 0, 0).
            let start_tape = vec![2, 5, 6, 7, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 5, 6, 7, 0, 0, 0, 0, 0]);
        }
    }

    mod move_from_indirect_constant {
        // MoveFromIndirectConstant {
        //     base: PhysicalLocation,
        //     offset: usize,
        //     output: PhysicalSlot,
        // },

        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn single_zero_target() {
            let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // base is 4. offset is 2. source = 4 + 2 = 6.
            // target at 1 is 0. source at 6 is 5.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 5];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 6 is cleared, target at 1 gets 5 added to it.
            assert_eq!(result_tape, vec![2, 5, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn single_non_zero_target() {
            let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // base is 4. offset is 2. source = 4 + 2 = 6.
            // target at 1 is 3. source at 6 is 5.
            let start_tape = vec![2, 3, 0, 0, 0, 0, 5];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 6 is cleared, target at 1 gets 5 added to it (3 + 5 = 8).
            assert_eq!(result_tape, vec![2, 8, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn single_zero_target_source() {
            let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // target at 1 is 0. source at 6 is 0.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn single_zero_source() {
            let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // target at 1 is 3. source at 6 is 0.
            let start_tape = vec![2, 3, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 3, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn multi_zero_target() {
            let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // base is 4. offset is 2. source starts at 6.
            // target at 1 is size 3 (values 0, 0, 0).
            // source at 6 is size 3 (values 10, 20, 30).
            let start_tape = vec![2, 0, 0, 0, 0, 0, 10, 20, 30];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 6, 7, 8 is cleared, targets at 1, 2, 3 receive the values.
            assert_eq!(result_tape, vec![2, 10, 20, 30, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn multi_partial_zero_target() {
            let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // target at 1 is size 3 (values 5, 0, 5).
            // source at 6 is size 3 (values 10, 20, 30).
            let start_tape = vec![2, 5, 0, 5, 0, 0, 10, 20, 30];
            let result_tape = test_instructions(instructions, start_tape);
            // Adds values to target and clears source
            assert_eq!(result_tape, vec![2, 15, 20, 35, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn multi_zero_source() {
            let instructions = vec![Ir2Instruction::MoveFromIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                output: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // target at 1 is size 3 (values 5, 6, 7).
            // source at 6 is size 3 (values 0, 0, 0).
            let start_tape = vec![2, 5, 6, 7, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 5, 6, 7, 0, 0, 0, 0, 0]);
        }
    }

    mod move_to_indirect {
        // MoveToIndirect {
        //     base: PhysicalLocation,
        //     offset: PhysicalSlot, // Restricted to 1 byte in length
        //     value: PhysicalSlot,
        // },

        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn single_zero_target() {
            let instructions = vec![Ir2Instruction::MoveToIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // offset at 0 is 2. base is 4. target = 4 + 2 = 6.
            // source at 1 is 5. target at 6 is 0.
            let start_tape = vec![2, 5, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 1 is cleared, target at 6 gets 5 added to it.
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 5]);
        }

        #[test]
        fn single_non_zero_target() {
            let instructions = vec![Ir2Instruction::MoveToIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // offset at 0 is 2. base is 4. target = 4 + 2 = 6.
            // source at 1 is 5. target at 6 is 3.
            let start_tape = vec![2, 5, 0, 0, 0, 0, 3];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 1 is cleared, target at 6 gets 5 added to it (3 + 5 = 8).
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 8]);
        }

        #[test]
        fn single_zero_target_source() {
            let instructions = vec![Ir2Instruction::MoveToIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // source at 1 is 0. target at 6 is 0.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn single_zero_source() {
            let instructions = vec![Ir2Instruction::MoveToIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // source at 1 is 0. target at 6 is 3.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 3];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 3]);
        }

        #[test]
        fn multi_zero_target() {
            let instructions = vec![Ir2Instruction::MoveToIndirect {
                base: PhysicalLocation::new(6),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // offset at 0 is 2. base is 6. target starts at 8.
            // source at 1 is size 3 (values 10, 20, 30).
            // target at 8 is size 3 (values 0, 0, 0).
            let start_tape = vec![2, 10, 20, 30, 0, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 1, 2, 3 is cleared, targets at 8, 9, 10 receive the values.
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0, 0, 10, 20, 30]);
        }

        #[test]
        fn multi_partial_zero_target() {
            let instructions = vec![Ir2Instruction::MoveToIndirect {
                base: PhysicalLocation::new(6),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // target at 8 is size 3 (values 5, 0, 5).
            // source at 1 is size 3 (values 10, 20, 30).
            let start_tape = vec![2, 10, 20, 30, 0, 0, 0, 0, 5, 0, 5];
            let result_tape = test_instructions(instructions, start_tape);
            // Adds values to target and clears source
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0, 0, 15, 20, 35]);
        }

        #[test]
        fn multi_zero_source() {
            let instructions = vec![Ir2Instruction::MoveToIndirect {
                base: PhysicalLocation::new(6),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // target at 8 is size 3 (values 5, 6, 7).
            // source at 1 is size 3 (values 0, 0, 0).
            let start_tape = vec![2, 0, 0, 0, 0, 0, 0, 0, 5, 6, 7];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0, 0, 5, 6, 7]);
        }
    }

    mod move_to_indirect_constant {
        // MoveToIndirectConstant {
        //     base: PhysicalLocation,
        //     offset: usize,
        //     value: PhysicalSlot,
        // },

        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn single_zero_target() {
            let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // base is 4. offset is 2. target = 4 + 2 = 6.
            // source at 1 is 5. target at 6 is 0.
            let start_tape = vec![2, 5, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 1 is cleared, target at 6 gets 5 added to it.
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 5]);
        }

        #[test]
        fn single_non_zero_target() {
            let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // base is 4. offset is 2. target = 4 + 2 = 6.
            // source at 1 is 5. target at 6 is 3.
            let start_tape = vec![2, 5, 0, 0, 0, 0, 3];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 1 is cleared, target at 6 gets 5 added to it (3 + 5 = 8).
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 8]);
        }

        #[test]
        fn single_zero_target_source() {
            let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // source at 1 is 0. target at 6 is 0.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn single_zero_source() {
            let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 1),
            }];
            // source at 1 is 0. target at 6 is 3.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 3];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 3]);
        }

        #[test]
        fn multi_zero_target() {
            let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
                base: PhysicalLocation::new(6),
                offset: 2,
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // base is 6. offset is 2. target starts at 8.
            // source at 1 is size 3 (values 10, 20, 30).
            // target at 8 is size 3 (values 0, 0, 0).
            let start_tape = vec![2, 10, 20, 30, 0, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            // Source at 1, 2, 3 is cleared, targets at 8, 9, 10 receive the values.
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0, 0, 10, 20, 30]);
        }

        #[test]
        fn multi_partial_zero_target() {
            let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
                base: PhysicalLocation::new(6),
                offset: 2,
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // target at 8 is size 3 (values 5, 0, 5).
            // source at 1 is size 3 (values 10, 20, 30).
            let start_tape = vec![2, 10, 20, 30, 0, 0, 0, 0, 5, 0, 5];
            let result_tape = test_instructions(instructions, start_tape);
            // Adds values to target and clears source
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0, 0, 15, 20, 35]);
        }

        #[test]
        fn multi_zero_source() {
            let instructions = vec![Ir2Instruction::MoveToIndirectConstant {
                base: PhysicalLocation::new(6),
                offset: 2,
                value: PhysicalSlot::from_start_size(PhysicalLocation::new(1), 3),
            }];
            // target at 8 is size 3 (values 5, 6, 7).
            // source at 1 is size 3 (values 0, 0, 0).
            let start_tape = vec![2, 0, 0, 0, 0, 0, 0, 0, 5, 6, 7];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0, 0, 5, 6, 7]);
        }
    }

    mod clear_indirect {
        // ClearIndirect {
        //     base: PhysicalLocation,
        //     offset: PhysicalSlot, // Restricted to 1 byte in length
        // },

        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn clear_non_zero_target() {
            let instructions = vec![Ir2Instruction::ClearIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
            }];
            // offset at 0 is 2. base is 4. target = 4 + 2 = 6.
            // target at 6 is 5, should be cleared to 0.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 5, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn clear_zero_target() {
            let instructions = vec![Ir2Instruction::ClearIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
            }];
            // offset at 0 is 2. base is 4. target = 4 + 2 = 6.
            // target at 6 is already 0.
            let start_tape = vec![2, 0, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![2, 0, 0, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn clear_with_zero_offset() {
            let instructions = vec![Ir2Instruction::ClearIndirect {
                base: PhysicalLocation::new(4),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
            }];
            // offset at 0 is 0. base is 4. target = 4 + 0 = 4.
            // target at 4 is 9.
            let start_tape = vec![0, 0, 0, 0, 9, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 0, 0, 0, 0, 0, 0, 0]);
        }
    }

    mod clear_indirect_constant {
        // ClearIndirectConstant {
        //     base: PhysicalLocation,
        //     offset: usize,
        // },

        use crate::{
            ir2::Ir2Instruction, ir2_memory_representation::PhysicalLocation,
            ir2_runner::test_instructions,
        };

        #[test]
        fn clear_non_zero_target() {
            let instructions = vec![Ir2Instruction::ClearIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
            }];
            // base is 4. offset is 2. target = 4 + 2 = 6.
            // target at 6 is 5, should be cleared to 0.
            let start_tape = vec![0, 0, 0, 0, 0, 0, 5, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 0, 0, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn clear_zero_target() {
            let instructions = vec![Ir2Instruction::ClearIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 2,
            }];
            // target at 6 is already 0.
            let start_tape = vec![0, 0, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 0, 0, 0, 0, 0, 0, 0]);
        }

        #[test]
        fn clear_with_zero_offset() {
            let instructions = vec![Ir2Instruction::ClearIndirectConstant {
                base: PhysicalLocation::new(4),
                offset: 0,
            }];
            // target = 4 + 0 = 4. target at 4 is 9.
            let start_tape = vec![0, 0, 0, 0, 9, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(result_tape, vec![0, 0, 0, 0, 0, 0, 0, 0]);
        }
    }

    mod clear_indirect_array {
        // ClearIndirectArray {
        //     base: PhysicalLocation,
        //     offset: PhysicalSlot, // Restricted to 1 byte in length
        //     element_size: usize,
        // },

        use crate::{
            ir2::Ir2Instruction,
            ir2_memory_representation::{PhysicalLocation, PhysicalSlot},
            ir2_runner::test_instructions,
        };

        #[test]
        fn clear_array_elements() {
            let instructions = vec![Ir2Instruction::ClearIndirectArray {
                base: PhysicalLocation::new(5),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                element_size: 3,
            }];
            // offset at 0 is 2. base is 5. element_size is 3.
            // target array starts at 5 + (2 * 3) = 11.
            // values at 11, 12, 13 are [7, 8, 9].
            let start_tape = vec![2, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 7, 8, 9, 0];
            let result_tape = test_instructions(instructions, start_tape);
            // target array should be cleared.
            assert_eq!(
                result_tape,
                vec![2, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            );
        }

        #[test]
        fn clear_array_already_zero() {
            let instructions = vec![Ir2Instruction::ClearIndirectArray {
                base: PhysicalLocation::new(5),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                element_size: 3,
            }];
            // target array starts at 11. already 0.
            let start_tape = vec![2, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(
                result_tape,
                vec![2, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            );
        }

        #[test]
        fn clear_array_zero_offset() {
            let instructions = vec![Ir2Instruction::ClearIndirectArray {
                base: PhysicalLocation::new(5),
                offset: PhysicalSlot::from_start_size(PhysicalLocation::new(0), 1),
                element_size: 3,
            }];
            // offset at 0 is 0. target array starts at 5 + (0 * 3) = 5.
            // values at 5, 6, 7 are [10, 20, 30].
            let start_tape = vec![0, 0, 0, 0, 0, 10, 20, 30, 0, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(
                result_tape,
                vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            );
        }
    }

    mod clear_indirect_array_constant {
        // ClearIndirectArrayConstant {
        //     base: PhysicalLocation,
        //     offset: usize,
        //     element_size: usize,
        // },

        use crate::{
            ir2::Ir2Instruction, ir2_memory_representation::PhysicalLocation,
            ir2_runner::test_instructions,
        };

        #[test]
        fn clear_array_elements() {
            let instructions = vec![Ir2Instruction::ClearIndirectArrayConstant {
                base: PhysicalLocation::new(5),
                offset: 2,
                element_size: 3,
            }];
            // offset is 2. base is 5. element_size is 3.
            // target array starts at 5 + (2 * 3) = 11.
            // values at 11, 12, 13 are [7, 8, 9].
            let start_tape = vec![0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 7, 8, 9, 0];
            let result_tape = test_instructions(instructions, start_tape);
            // target array should be cleared.
            assert_eq!(
                result_tape,
                vec![0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            );
        }

        #[test]
        fn clear_array_already_zero() {
            let instructions = vec![Ir2Instruction::ClearIndirectArrayConstant {
                base: PhysicalLocation::new(5),
                offset: 2,
                element_size: 3,
            }];
            // target array starts at 11. already 0.
            let start_tape = vec![0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(
                result_tape,
                vec![0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            );
        }

        #[test]
        fn clear_array_zero_offset() {
            let instructions = vec![Ir2Instruction::ClearIndirectArrayConstant {
                base: PhysicalLocation::new(5),
                offset: 0,
                element_size: 3,
            }];
            // offset is 0. target array starts at 5 + (0 * 3) = 5.
            // values at 5, 6, 7 are [10, 20, 30].
            let start_tape = vec![0, 0, 0, 0, 0, 10, 20, 30, 0, 0, 0, 0, 0, 0, 0];
            let result_tape = test_instructions(instructions, start_tape);
            assert_eq!(
                result_tape,
                vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            );
        }
    }
}
