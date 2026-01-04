use compiler_bf_target::tokenizer::TokenEmitter;
use compiler_bf_target::{ir2::print_instruction_with_lifetime_annotations, parser::Parser, tokenizer::Lexer};
use compiler_bf_target::ir2::generate_ir2;
use compiler_bf_target::ir::generate_ir;
use compiler_bf_target::type_check::type_annotate_program;
use compiler_bf_target::ucodegen::BfGenerator;
use compiler_bf_target::codegen::codegen_program;

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
    let mut lexer = Lexer::new(&source_code, TokenEmitter{});
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program();
    let type_checked = type_annotate_program(ast.clone().unwrap());

    let entry_point = type_checked.function_name_mapping.iter().filter(|(_, name)| *name == "main").map(|(id, _)| *id).next().expect("No main function found");

    let generated_ir = generate_ir(&type_checked);

    // println!("raw AST: {:#?}", ast);
    println!("typed checked AST: {:#?}", type_checked);
    
    for function in &generated_ir {
        print_instruction_with_lifetime_annotations(function);
        print!("\n\n\n");
    }

    let generated_ir2 = generate_ir2(&generated_ir);
    for (function_id, function) in &generated_ir2 {
        println!("IR2 for function {}:", function_id);
        for instruction in &function.code {
            println!("  {}", instruction);
        }
        println!();
    }

    let generated_bf = BfGenerator::ucodegen_program(generated_ir2);

    for function in &generated_bf.functions {
        println!("BF for function {}:", function.0);
        for instruction in function.1 {
            println!("  {}", instruction);
        }
    }

    let final_bf_code = codegen_program(generated_bf, entry_point);

    println!("Final Brainfuck Code:");
    for instruction in &final_bf_code {
        print!("{}", instruction);
    }

    //write the generated bf code to a file
    let output_file = format!("{}.bf", input_file);
    let bf_code_string: String = final_bf_code.iter().map(|instr| instr.to_string()).collect::<String>();
    std::fs::write(&output_file, bf_code_string).expect("Failed to write output file");
    println!("Generated Brainfuck code written to {}", output_file);

}
