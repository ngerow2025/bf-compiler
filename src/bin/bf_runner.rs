use std::io;

fn main() {
    //take in bf code from an input
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();

    let mut bf_runner = BfInterpreter::new();
    bf_runner.run(&input);
}

struct BfInterpreter {
    tape: [u8; 30000],
    pointer: usize,
}

impl BfInterpreter {
    fn new() -> Self {
        BfInterpreter {
            tape: [0; 30000],
            pointer: 0,
        }
    }

    fn run(&mut self, code: &str) {
        let mut code_chars: Vec<char> = code.chars().collect();
        let mut pc: usize = 0;
        let mut loop_stack: Vec<usize> = Vec::new();

        while pc < code_chars.len() {
            match code_chars[pc] {
                '>' => self.pointer += 1,
                '<' => self.pointer -= 1,
                '+' => self.tape[self.pointer] = self.tape[self.pointer].wrapping_add(1),
                '-' => self.tape[self.pointer] = self.tape[self.pointer].wrapping_sub(1),
                '.' => print!("{}", self.tape[self.pointer] as char),
                ',' => {
                    // For simplicity, we won't implement input in this example
                }
                '[' => {
                    if self.tape[self.pointer] == 0 {
                        let mut open_brackets = 1;
                        while open_brackets > 0 {
                            pc += 1;
                            if code_chars[pc] == '[' {
                                open_brackets += 1;
                            } else if code_chars[pc] == ']' {
                                open_brackets -= 1;
                            }
                        }
                    } else {
                        loop_stack.push(pc);
                    }
                }
                ']' => {
                    if self.tape[self.pointer] != 0 {
                        if let Some(loop_start) = loop_stack.last() {
                            pc = *loop_start;
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
