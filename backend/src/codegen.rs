use crate::parser::FunctionId;
use crate::ucodegen::{BfUcodeInstruction, Location, LocationId, UCodeProgram};
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum BfInstruction {
    Right,
    Left,
    Inc,
    Dec,
    Output,
    Input,
    LoopStart,
    LoopEnd,
}

pub fn codegen_program(ucode: UCodeProgram, entry_point: FunctionId) -> Vec<BfInstruction> {
    let mut code_blocks = HashMap::new();

    for (function_id, instructions) in ucode.functions {
        let mut bf_instructions = Vec::new();
        let mut block_id = *ucode.entry_points.get(&function_id).unwrap();
        assert_ne!(
            block_id,
            LocationId(0),
            "Function entry point can not be at location 0"
        );
        let mut current_point_position = 0;
        for instruction in instructions {
            if let Some(direct_code) =
                direct_codegen_ucode_instruction(instruction.clone(), &mut current_point_position)
            {
                bf_instructions.extend(direct_code);
            } else if let BfUcodeInstruction::JumpLocation {
                this_location,
                target_location,
            } = instruction
            {
                bf_instructions.push(BfInstruction::Left);
                bf_instructions.push(BfInstruction::Left);
                bf_instructions.push(BfInstruction::Left);
                bf_instructions.extend(vec![BfInstruction::Inc; this_location.0]);
                bf_instructions.push(BfInstruction::Right);
                bf_instructions.extend(vec![BfInstruction::Inc; target_location.0]);
                code_blocks.insert(block_id, bf_instructions);
                bf_instructions = Vec::new();
                block_id = this_location;
            } else {
                panic!("Unhandled instruction: {:?} ?", instruction);
            }
        }
        bf_instructions.push(BfInstruction::Left);
        bf_instructions.push(BfInstruction::Left);
        //move to the return address location for the jump
    }

    let mut bf_code = Vec::new();

    //first generate the code to jump to the entry point
    bf_code.push(BfInstruction::Right);
    bf_code.extend(vec![BfInstruction::Inc; entry_point.0]); //set the entry jump target

    //the main loop start
    bf_code.push(BfInstruction::LoopStart);
    bf_code.push(BfInstruction::Right);
    bf_code.push(BfInstruction::Inc); //set the flag to 1
    bf_code.push(BfInstruction::Left);

    //itterate through the code blocks in the order of their location ids
    let mut sorted_code_blocks = code_blocks.into_iter().collect::<Vec<_>>();
    sorted_code_blocks.sort_by_key(|(location_id, _)| *location_id);

    bf_code.extend(codegen_switch_statement(
        sorted_code_blocks,
        0,
        vec![], //default case is empty for now
    ));

    bf_code.push(BfInstruction::LoopEnd);

    bf_code
}

macro_rules! bf_vec {
    // 1. Match "list <ident>"
    (list $extra:ident $(, $($tail:tt)*)?) => {
        {
            let mut v = Vec::new();
            v.extend($extra);
            $( v.extend(bf_vec!($($tail)*)); )?
            v
        }
    };

    // 2. Match "<ident>::<ident>" (Path)
    ($instr:path $(, $($tail:tt)*)?) => {
        #[allow(clippy::vec_init_then_push)]
        {
            let mut v = Vec::new();
            v.push($instr);
            $( v.extend(bf_vec!($($tail)*)); )?
            v
        }
    };

    // 3. Base case: end of list
    () => {
        Vec::<BfInstruction>::new()
    };
}

fn codegen_switch_statement(
    remaining_cases: Vec<(LocationId, Vec<BfInstruction>)>,
    current_subtracted_val: usize,
    default_case: Vec<BfInstruction>,
) -> Vec<BfInstruction> {
    match remaining_cases.split_first() {
        Some(((location_id, case_instructions), rest_cases)) => {
            let diff = location_id.0 - current_subtracted_val;
            let difference_code = vec![BfInstruction::Dec; diff];
            let switch_statement_code =
                codegen_switch_statement(Vec::from(rest_cases), location_id.0, default_case);

            bf_vec![
                list difference_code,
                BfInstruction::LoopStart,
                list switch_statement_code,

                BfInstruction::LoopEnd,
                BfInstruction::Right, //move to the flag
                BfInstruction::LoopStart, //check the flag
                BfInstruction::Dec, //turn off the flag
                BfInstruction::Left, //move to address, this is now zero
                BfInstruction::Left, //move to the return address, start setting up for the return jump
                BfInstruction::LoopStart, //move the return address to the jump location
                BfInstruction::Dec,
                BfInstruction::Right,
                BfInstruction::Inc,
                BfInstruction::Left,
                BfInstruction::LoopEnd,

                BfInstruction::Right, //move to the new base
                BfInstruction::Right,
                BfInstruction::Right,

                list case_instructions,

                BfInstruction::Left, //move back to the target address
                BfInstruction::Left,

                BfInstruction::LoopEnd
            ]
        }
        None => {
            bf_vec![
                BfInstruction::Right, //move to the flag
                BfInstruction::Dec, //turn off the flag
                BfInstruction::Left, //move to the target address
                BfInstruction::Left, //move to the return address, start setting up for the return jump
                BfInstruction::LoopStart, //move the return address to the jump location
                BfInstruction::Dec,
                BfInstruction::Right,
                BfInstruction::Inc,
                BfInstruction::Left,
                BfInstruction::LoopEnd,
                BfInstruction::Right, //move to the new base
                BfInstruction::Right,
                BfInstruction::Right,

                list default_case,

                BfInstruction::Left, //move back to the target address
                BfInstruction::Left,
            ]
        }
    }
}

fn direct_codegen_ucode_instruction(
    instruction: BfUcodeInstruction,
    current_point_position: &mut isize,
) -> Option<Vec<BfInstruction>> {
    match instruction {
        BfUcodeInstruction::Dec(x) => Some(vec![BfInstruction::Dec; x as usize]),
        BfUcodeInstruction::Input => Some(vec![BfInstruction::Input]),
        BfUcodeInstruction::Output => Some(vec![BfInstruction::Output]),
        BfUcodeInstruction::MovePtr(location) => {
            Some(codegen_move_ptr(current_point_position, location))
        }
        BfUcodeInstruction::MoveData { from, to } => Some(
            codegen_move_ptr(current_point_position, from)
                .into_iter()
                .chain([BfInstruction::LoopStart, BfInstruction::Dec])
                .chain(codegen_move_ptr(current_point_position, to))
                .chain([BfInstruction::Inc])
                .chain(codegen_move_ptr(current_point_position, from))
                .chain([BfInstruction::LoopEnd])
                .collect::<Vec<_>>(),
        ),
        BfUcodeInstruction::Inc(x) => Some(vec![BfInstruction::Inc; x as usize]),
        BfUcodeInstruction::Loop(inner) => vec![BfInstruction::LoopStart]
            .into_iter()
            .chain(inner.into_iter().flat_map(|instr| {
                direct_codegen_ucode_instruction(instr, current_point_position)
                    .expect("can not handle function calls inside loops yet")
            }))
            .chain([BfInstruction::LoopEnd])
            .collect::<Vec<_>>()
            .into(),
        BfUcodeInstruction::JumpLocation { .. } => None,
        BfUcodeInstruction::Clear => Some(vec![
            BfInstruction::LoopStart,
            BfInstruction::Dec,
            BfInstruction::LoopEnd,
        ]),
    }
}

fn codegen_move_ptr(current_point_position: &mut isize, location: Location) -> Vec<BfInstruction> {
    match location {
        Location::Absolute(x) => move_to_absolute(current_point_position, x as isize),
        Location::Relative(x) => {
            if x >= 0 {
                *current_point_position += x as isize;
                vec![BfInstruction::Right; x as usize]
            } else {
                *current_point_position -= (-x) as isize;
                vec![BfInstruction::Left; (-x) as usize]
            }
        }
        Location::OffsetAbsolute { offset, base } => {
            let x = base as isize + offset as isize;
            move_to_absolute(current_point_position, x)
        }
    }
}

fn move_to_absolute(current_position: &mut isize, target_position: isize) -> Vec<BfInstruction> {
    if *current_position < target_position {
        let move_amount = (target_position - *current_position) as usize;
        *current_position = target_position;
        vec![BfInstruction::Right; move_amount]
    } else {
        let move_amount = (*current_position - target_position) as usize;
        *current_position = target_position;
        vec![BfInstruction::Left; move_amount]
    }
}

impl Display for BfInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            BfInstruction::Right => '>',
            BfInstruction::Left => '<',
            BfInstruction::Inc => '+',
            BfInstruction::Dec => '-',
            BfInstruction::Output => '.',
            BfInstruction::Input => ',',
            BfInstruction::LoopStart => '[',
            BfInstruction::LoopEnd => ']',
        };
        write!(f, "{}", c)
    }
}
