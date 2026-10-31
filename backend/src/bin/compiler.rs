#[cfg(not(feature = "advanced-output"))]
use std::io::{self, Read, Write};

#[cfg(not(feature = "advanced-output"))]
use compiler_bf_target::ast_runner::run_program;
#[cfg(feature = "advanced-output")]
use compiler_bf_target::ast_runner::run_program_console;
use compiler_bf_target::codegen::codegen_program;
use compiler_bf_target::ir::generate_ir;
#[cfg(not(feature = "advanced-output"))]
use compiler_bf_target::ir_runner::run_ir;
#[cfg(feature = "advanced-output")]
use compiler_bf_target::ir_runner::run_ir_console;
use compiler_bf_target::ir2::generate_ir2;
#[cfg(not(feature = "advanced-output"))]
use compiler_bf_target::ir2_runner::run_ir2;
#[cfg(feature = "advanced-output")]
use compiler_bf_target::ir2_runner::run_ir2_console;
use compiler_bf_target::source_annotation::SourceAnnotation;
use compiler_bf_target::sources::SourceCodeOrigin;
use compiler_bf_target::type_check::type_annotate_program;
use compiler_bf_target::ucodegen::BfGenerator;
use compiler_bf_target::{parser, tokenizer::Lexer};

use clap::Parser;
use miette::Result;

#[derive(Parser)]
#[command(name = "MyApp")]
#[command(version = "1.0")]
#[command(about = "Compiles a custom language to Brainfuck", long_about = None)]
struct Cli {
    source_file: String,
    #[arg(short, long, default_value_t = String::from("output.bf"))]
    output_file: String,
    #[arg(short, long, help = "Run the code at each compilation stage")]
    run: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    #[cfg(not(feature = "advanced-output"))]
    let console_input = || {
        let mut byte = [0u8; 1];
        io::stdin().read_exact(&mut byte).ok()?;
        Some(byte[0])
    };
    #[cfg(not(feature = "advanced-output"))]
    let console_output = |b: u8| {
        print!("{}", b as char);
        io::stdout().flush().expect("failed to flush stdout");
    };

    if cli.run {
        println!("Running code in all stages of compilation...");
    }

    let source_code =
        std::fs::read_to_string(cli.source_file.clone()).expect("Failed to read input file");
    let mut lexer = Lexer::new(&source_code, Some(cli.source_file.clone()));
    let tokens = lexer.tokenize_with_locations()?;

    let mut source_annotator = SourceAnnotation {
        source_code: SourceCodeOrigin::File(cli.source_file.into()),
    };

    let mut parser = parser::Parser::new(tokens, &mut source_annotator);
    let ast = parser.parse_program()?;

    if cli.run {
        println!("Running AST... -----------------------");
        #[cfg(feature = "advanced-output")]
        run_program_console(&ast);
        #[cfg(not(feature = "advanced-output"))]
        run_program(&ast, console_input, console_output);
        println!("Finished running AST. -----------------------\n");
    }

    let type_checked = type_annotate_program(&ast);
    let function_name_mapping = ast.function_name_mapping;

    let entry_point = type_checked
        .function_name_mapping
        .iter()
        .filter(|(_, name)| *name == "main")
        .map(|(id, _)| *id)
        .next()
        .expect("No main function found");

    let generated_ir = generate_ir(&type_checked);

    println!("Generated IR:");
    for ir_function in &generated_ir {
        println!(
            "IR for function {}:",
            function_name_mapping
                .get(&ir_function.id)
                .cloned()
                .unwrap_or_else(|| format!("Function {}", ir_function.id))
        );
        for instruction in &ir_function.code {
            println!(
                "  {}",
                instruction.display_with_reg_desc(&ir_function.registers, &function_name_mapping)
            );
        }
        println!();
    }

    #[cfg(feature = "advanced-output")]
    if cli.run {
        println!("Running IR... -----------------------");
        run_ir_console(&generated_ir, function_name_mapping.clone());
        println!("Finished running IR. -----------------------\n");
    }
    #[cfg(not(feature = "advanced-output"))]
    if cli.run {
        println!("Running IR... -----------------------");
        run_ir(
            &generated_ir,
            function_name_mapping.clone(),
            console_input,
            console_output,
        );
        println!("Finished running IR. -----------------------\n");
    }

    // for ir_function in &generated_ir {
    //     println!("IR for function {}:", ir_function.id);
    //     for instruction in &ir_function.code {
    //         println!("  {}", instruction);
    //     }
    //     println!();
    // }

    let generated_ir2 = generate_ir2(&generated_ir);

    for ir2_function in generated_ir2.values() {
        println!("IR2 for function {}:", ir2_function.id);
        for instruction in &ir2_function.code {
            println!("  {}", instruction);
        }
        println!();
    }

    #[cfg(feature = "advanced-output")]
    if cli.run {
        println!("Running IR2... -----------------------");
        run_ir2_console(
            generated_ir2.values().collect(),
            function_name_mapping.clone(),
        );
        println!("Finished running IR2. -----------------------\n");
    }
    #[cfg(not(feature = "advanced-output"))]
    if cli.run {
        println!("Running IR2... -----------------------");

        run_ir2(
            generated_ir2.values().collect(),
            function_name_mapping,
            console_input,
            console_output,
        );
        println!("Finished running IR2. -----------------------\n");
    }

    let generated_bf = BfGenerator::ucodegen_program(&generated_ir2);

    let final_bf_code = codegen_program(generated_bf, entry_point);

    // println!("Final Brainfuck Code:");
    // for instruction in &final_bf_code {
    //     print!("{}", instruction);
    // }

    //write the generated bf code to a file
    let output_file = format!("{}.bf", cli.output_file);
    let bf_code_string: String = final_bf_code
        .iter()
        .map(ToString::to_string)
        .collect::<String>();
    std::fs::write(&output_file, bf_code_string).expect("Failed to write output file");
    // println!("Generated Brainfuck code written to {}", output_file);

    Ok(())
}
