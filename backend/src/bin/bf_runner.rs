#[cfg(feature = "advanced-output")]
use std::io::{self, Write};

#[cfg(feature = "advanced-output")]
use compiler_bf_target::input_grabbing::{RawModeGuard, read_single_byte};

fn main() {
    #[cfg(feature = "advanced-output")]
    {
        //take in bf code from an input
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let mut bf_runner = BfInterpreter::new();
        bf_runner.run(&input);
    }
    #[cfg(not(feature = "advanced-output"))]
    println!("Advanced output feature is not enabled");
}

#[cfg(feature = "advanced-output")]
struct BfInterpreter {
    tape: Box<[u8]>,
    pointer: usize,
}

#[cfg(feature = "advanced-output")]
impl BfInterpreter {
    fn new() -> Self {
        Self {
            tape: vec![0; 30000].into_boxed_slice(),
            pointer: 0,
        }
    }

    fn get_tape_value(&self) -> u8 {
        *self
            .tape
            .get(self.pointer)
            .expect("invalid tape pointer access")
    }

    fn set_tape_value(&mut self, value: u8) {
        *self
            .tape
            .get_mut(self.pointer)
            .expect("invalid tape pointer access") = value;
    }

    fn run(&mut self, code: &str) {
        let code_chars: Vec<char> = code.chars().collect();
        let mut pc: usize = 0;
        let mut loop_stack: Vec<usize> = Vec::new();

        //loop through all instructions and ensure that brackets are balanced
        let mut bracket_balance = 0;
        for &ch in &code_chars {
            match ch {
                '[' => bracket_balance += 1,
                ']' => bracket_balance -= 1,
                _ => {}
            }
            assert!(bracket_balance >= 0, "Unbalanced brackets in BF code");
        }
        assert_eq!(bracket_balance, 0, "Unbalanced brackets in BF code");

        let _raw_mode_guard = RawModeGuard::new();

        while let Some(instruction) = code_chars.get(pc) {
            match instruction {
                '>' => self.pointer += 1,
                '<' => self.pointer -= 1,
                '+' => self.set_tape_value(self.get_tape_value().wrapping_add(1)),
                '-' => self.set_tape_value(self.get_tape_value().wrapping_sub(1)),
                '.' => {
                    print!("{}", self.get_tape_value() as char);
                    io::stdout().flush().expect("failed to flush stdout");
                }
                ',' => {
                    self.set_tape_value(read_single_byte());
                }
                '[' => {
                    if self.get_tape_value() == 0 {
                        let mut open_brackets = 1;
                        while open_brackets > 0 {
                            pc += 1;
                            if *code_chars
                                .get(pc)
                                .expect("brackets should have been ensured to be balanced")
                                == '['
                            {
                                open_brackets += 1;
                            } else if *code_chars
                                .get(pc)
                                .expect("brackets should have been ensured to be balanced")
                                == ']'
                            {
                                open_brackets -= 1;
                            }
                        }
                    } else {
                        loop_stack.push(pc);
                    }
                }
                ']' => {
                    if self.get_tape_value() != 0 {
                        if let Some(&loop_start) = loop_stack.last() {
                            pc = loop_start;
                        }
                    } else {
                        loop_stack.pop();
                    }
                }
                _ => {}
            }
            pc += 1;
        }
    }
}
