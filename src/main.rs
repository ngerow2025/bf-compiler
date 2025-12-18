use crate::{parser::Parser, tokenizer::Lexer};

mod parser;
mod tokenizer;
mod ir;
mod type_check;

fn main(){
    //tokenize and parse a file spesified by the command line input
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }
    let input_file = &args[1];
    let source_code = std::fs::read_to_string(input_file)
        .expect("Failed to read input file");
    let mut lexer = Lexer::new(&source_code);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program();
    let type_checked = type_check::type_annotate_program(ast.unwrap());
    // let ir = ir::generate_ir(&ast.unwrap());
    println!("{:#?}", type_checked);
}
