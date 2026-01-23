use std::sync::Arc;

use wasm_bindgen::prelude::*;

use crate::codegen::codegen_program;
use crate::ir::generate_ir;
use crate::ir2::generate_ir2;
use crate::source_annotation::SourceAnnotation;
use crate::sources::SourceCodeOrigin;
use crate::tokenizer::{Lexer, Locatable, Token};
use crate::type_check::type_annotate_program;
use crate::ucodegen::UCodeProgram;

#[derive(serde::Serialize)]
struct TokenInfo {
    kind: String,
    value: Option<String>,
    span_start: usize,
    span_len: usize,
}

#[derive(serde::Serialize)]
struct FunctionDump {
    function_id: usize,
    lines: Vec<String>,
}

#[derive(serde::Serialize)]
struct CompilationSteps {
    tokens: Vec<TokenInfo>,
    ast_debug: String,
    // typed_debug: String,
    // ir_by_function: Vec<FunctionDump>,
    // ir2_by_function: Vec<FunctionDump>,
    // ucode_by_function: Vec<FunctionDump>,
    // bf_program: String,
}

fn token_to_info(loc_token: &Locatable<Token>) -> TokenInfo {
    let (kind, value) = match &loc_token.value {
        Token::Fn => ("Fn".to_string(), None),
        Token::Let => ("Let".to_string(), None),
        Token::TypeU8 => ("TypeU8".to_string(), None),
        Token::TypeI8 => ("TypeI8".to_string(), None),
        Token::TypeU16 => ("TypeU16".to_string(), None),
        Token::TypeI16 => ("TypeI16".to_string(), None),
        Token::TypeU32 => ("TypeU32".to_string(), None),
        Token::TypeI32 => ("TypeI32".to_string(), None),
        Token::TypeU64 => ("TypeU64".to_string(), None),
        Token::TypeI64 => ("TypeI64".to_string(), None),
        Token::TypeStr => ("TypeStr".to_string(), None),
        Token::LBrace => ("LBrace".to_string(), None),
        Token::RBrace => ("RBrace".to_string(), None),
        Token::LParen => ("LParen".to_string(), None),
        Token::RParen => ("RParen".to_string(), None),
        Token::LSquare => ("LSquare".to_string(), None),
        Token::RSquare => ("RSquare".to_string(), None),
        Token::Colon => ("Colon".to_string(), None),
        Token::DoubleColon => ("DoubleColon".to_string(), None),
        Token::Semicolon => ("Semicolon".to_string(), None),
        Token::Equals => ("Equals".to_string(), None),
        Token::LAngle => ("LAngle".to_string(), None),
        Token::RAngle => ("RAngle".to_string(), None),
        Token::Comma => ("Comma".to_string(), None),
        Token::Identifier(s) => ("Identifier".to_string(), Some(s.clone())),
        Token::IntLiteral(s) => ("IntLiteral".to_string(), Some(s.clone())),
        Token::StringLiteral(s) => ("StringLiteral".to_string(), Some(s.clone())),
        Token::CharLiteral(c) => ("CharLiteral".to_string(), Some(c.to_string())),
    };
    let span_start = loc_token.loc.span.offset();
    let span_len = loc_token.loc.span.len();
    TokenInfo {
        kind,
        value,
        span_start,
        span_len,
    }
}

fn ucode_dump(ucode: &UCodeProgram) -> Vec<FunctionDump> {
    let mut v = Vec::new();
    for (func_id, instrs) in &ucode.functions {
        let lines = instrs
            .iter()
            .map(|i| format!("{}", i))
            .collect::<Vec<String>>();
        v.push(FunctionDump {
            function_id: func_id.0,
            lines,
        });
    }
    v
}

fn bf_program_string(program: &[crate::codegen::BfInstruction]) -> String {
    program.iter().map(|i| i.to_string()).collect::<String>()
}

#[wasm_bindgen]
pub fn tokenize_json(source: &str) -> Result<String, JsValue> {
    let mut lexer = Lexer::new(source, None);
    let tokens = lexer
        .tokenize_with_locations()
        .map_err(|e| JsValue::from_str(&format!("{}", e)))?;
    let infos = tokens.iter().map(token_to_info).collect::<Vec<_>>();
    serde_json::to_string(&infos)
        .map_err(|e| JsValue::from_str(&format!("serialization error: {}", e)))
}

#[wasm_bindgen]
pub fn compile_steps_json(source: &str) -> Result<String, JsValue> {
    let mut lexer = Lexer::new(source, None);
    let tokens = lexer
        .tokenize_with_locations()
        .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

    let mut source_annotator = SourceAnnotation {
        source_code: SourceCodeOrigin::Anon(Arc::new(source.to_string())),
    };

    let mut parser = crate::parser::Parser::new(tokens.clone(), &mut source_annotator);
    let ast = parser
        .parse_program()
        .map_err(|e| JsValue::from_str(&format!("parse error: {}", e)))?;

    // let typed = type_annotate_program(ast.clone());

    // let entry_point = typed
    //     .function_name_mapping
    //     .iter()
    //     .find_map(|(id, name)| if name == "main" { Some(*id) } else { None })
    //     .ok_or_else(|| JsValue::from_str("No main function found"))?;

    // let ir = generate_ir(&typed);
    // let ir_dump = ir
    //     .iter()
    //     .map(|f| FunctionDump {
    //         function_id: f.id.0,
    //         lines: f
    //             .code
    //             .iter()
    //             .map(|i| format!("{}", i))
    //             .collect::<Vec<String>>(),
    //     })
    //     .collect::<Vec<_>>();

    // let ir2 = generate_ir2(&ir);
    // let ir2_dump = ir2
    //     .iter()
    //     .map(|(fid, func)| FunctionDump {
    //         function_id: fid.0,
    //         lines: func
    //             .code
    //             .iter()
    //             .map(|i| format!("{}", i))
    //             .collect::<Vec<String>>(),
    //     })
    //     .collect::<Vec<_>>();

    // let ucode = crate::ucodegen::BfGenerator::ucodegen_program(ir2);
    // let ucode_dump = ucode_dump(&ucode);

    // let bf = codegen_program(ucode, entry_point);

    let steps = CompilationSteps {
        tokens: tokens.iter().map(token_to_info).collect::<Vec<_>>(),
        ast_debug: format!("{:#?}", ast),
        // typed_debug: format!("{:#?}", typed),
        // ir_by_function: ir_dump,
        // ir2_by_function: ir2_dump,
        // ucode_by_function: ucode_dump,
        // bf_program: bf_program_string(&bf),
    };

    serde_json::to_string(&steps)
        .map_err(|e| JsValue::from_str(&format!("serialization error: {}", e)))
}

#[wasm_bindgen]
pub fn compile_to_bf(source: &str) -> Result<String, JsValue> {
    let mut lexer = Lexer::new(source, None);
    let tokens = lexer
        .tokenize_with_locations()
        .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

    let mut source_annotator = SourceAnnotation {
        source_code: SourceCodeOrigin::Anon(Arc::new(source.to_string())),
    };

    let mut parser = crate::parser::Parser::new(tokens, &mut source_annotator);
    let ast = parser
        .parse_program()
        .map_err(|e| JsValue::from_str(&format!("parse error: {}", e)))?;
    let typed = type_annotate_program(ast);

    let entry_point = typed
        .function_name_mapping
        .iter()
        .find_map(|(id, name)| if name == "main" { Some(*id) } else { None })
        .ok_or_else(|| JsValue::from_str("No main function found"))?;

    let ir = generate_ir(&typed);
    let ir2 = generate_ir2(&ir);
    let ucode = crate::ucodegen::BfGenerator::ucodegen_program(ir2);
    let bf = codegen_program(ucode, entry_point);
    Ok(bf_program_string(&bf))
}

#[wasm_bindgen]
pub fn init() {
    console_error_panic_hook::set_once();
}
