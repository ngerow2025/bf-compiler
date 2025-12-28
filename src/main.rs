use crate::{ir2::print_instruction_with_lifetime_annotations, parser::Parser, tokenizer::Lexer};

mod parser;
mod tokenizer;
mod ir;
mod type_check;
mod ir2;

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
    let type_checked = type_check::type_annotate_program(ast.clone().unwrap());
    let generated_ir = ir::generate_ir(&type_checked);

    // println!("raw AST: {:#?}", ast);
    println!("typed checked AST: {:#?}", type_checked);
    
    for function in &generated_ir {
        print_instruction_with_lifetime_annotations(function);
        print!("\n\n\n");
    }

    let generated_ir2 = ir2::generate_ir2(&generated_ir);
    for (function_id, function) in &generated_ir2 {
        println!("IR2 for function {}:", function_id);
        for instruction in &function.code {
            println!("  {}", instruction);
        }
        println!();
    }

}
