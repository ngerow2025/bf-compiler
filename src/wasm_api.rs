use std::collections::HashMap;
use std::sync::Arc;

use miette::Report;
use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::codegen::{BfInstruction, codegen_program};
use crate::ir::{IrFunction, generate_ir};
use crate::ir2::{Ir2Function, generate_ir2};
use crate::parser::{Function, FunctionId};
use crate::source_annotation::SourceAnnotation;
use crate::sources::{SourceCodeOrigin, SourceLocation};
use crate::tokenizer::{Lexer, Locatable, Token};
use crate::type_check::{TypedProgram, type_annotate_program};
use crate::ucodegen::UCodeProgram;

// ============================================================================
// Exported Wrapper Types
// ============================================================================

/// A single token with location information
#[wasm_bindgen]
pub struct WasmToken {
    kind: String,
    value: Option<String>,
    span_start: usize,
    span_len: usize,
}

#[wasm_bindgen]
impl WasmToken {
    #[wasm_bindgen(getter)]
    pub fn kind(&self) -> String {
        self.kind.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn value(&self) -> Option<String> {
        self.value.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn span_start(&self) -> usize {
        self.span_start
    }

    #[wasm_bindgen(getter)]
    pub fn span_len(&self) -> usize {
        self.span_len
    }
}

/// Collection of tokens from lexing
#[wasm_bindgen]
pub struct WasmTokens {
    tokens: Vec<Locatable<Token>>,
}

#[wasm_bindgen]
impl WasmTokens {
    #[wasm_bindgen(getter)]
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn get(&self, index: usize) -> Option<WasmToken> {
        self.tokens.get(index).map(token_to_wasm)
    }

    pub fn to_vec(&self) -> Vec<WasmToken> {
        self.tokens.iter().map(token_to_wasm).collect()
    }

    pub fn debug_string(&self) -> String {
        format!("{:#?}", self.tokens)
    }
}

/// The AST (Abstract Syntax Tree) from parsing
#[wasm_bindgen]
pub struct WasmAst {
    ast: crate::parser::Program<SourceAnnotation>,
    source: Arc<String>,
}

#[wasm_bindgen]
impl WasmAst {
    #[wasm_bindgen(getter)]
    pub fn function_count(&self) -> usize {
        self.ast.functions.len()
    }

    pub fn get_function_name(&self, index: usize) -> Option<String> {
        self.ast.functions.get(index).map(|f| f.name.clone())
    }

    pub fn get_function_names(&self) -> Vec<String> {
        self.ast.functions.iter().map(|f| f.name.clone()).collect()
    }

    pub fn get_raw_func(&self) -> Result<JsValue, JsValue> {
        serde_wasm_bindgen::to_value(&self.ast.functions[0])
            .map_err(|e| JsValue::from_str(&e.to_string()))
    }

    pub fn get_all_functions(&self) -> Result<JsValue, JsValue> {
        serde_wasm_bindgen::to_value(&self.ast.functions)
            .map_err(|e| JsValue::from_str(&e.to_string()))
    }

    pub fn debug_string(&self) -> String {
        format!("{:#?}", self.ast)
    }
}

/// The type-checked AST
#[wasm_bindgen]
pub struct WasmTypedAst {
    typed: TypedProgram,
}

#[wasm_bindgen]
impl WasmTypedAst {
    #[wasm_bindgen(getter)]
    pub fn function_count(&self) -> usize {
        self.typed.functions.len()
    }

    pub fn get_function_names(&self) -> Vec<String> {
        self.typed.function_name_mapping.values().cloned().collect()
    }

    pub fn has_main(&self) -> bool {
        self.typed
            .function_name_mapping
            .values()
            .any(|n| n == "main")
    }

    pub fn debug_string(&self) -> String {
        format!("{:#?}", self.typed)
    }
}

/// A single IR function
#[wasm_bindgen]
pub struct WasmIrFunction {
    function_id: usize,
    instructions: Vec<String>,
}

#[wasm_bindgen]
impl WasmIrFunction {
    #[wasm_bindgen(getter)]
    pub fn function_id(&self) -> usize {
        self.function_id
    }

    #[wasm_bindgen(getter)]
    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    pub fn get_instruction(&self, index: usize) -> Option<String> {
        self.instructions.get(index).cloned()
    }

    pub fn get_instructions(&self) -> Vec<String> {
        self.instructions.clone()
    }
}

/// The IR (Intermediate Representation)
#[wasm_bindgen]
pub struct WasmIr {
    ir: Vec<IrFunction>,
}

#[wasm_bindgen]
impl WasmIr {
    #[wasm_bindgen(getter)]
    pub fn function_count(&self) -> usize {
        self.ir.len()
    }

    pub fn get_function(&self, index: usize) -> Option<WasmIrFunction> {
        self.ir.get(index).map(|f| WasmIrFunction {
            function_id: f.id.0,
            instructions: f.code.iter().map(|i| format!("{:?}", i)).collect(),
        })
    }

    pub fn debug_string(&self) -> String {
        format!("{:#?}", self.ir)
    }
}

/// A single IR2 function
#[wasm_bindgen]
pub struct WasmIr2Function {
    function_id: usize,
    instructions: Vec<String>,
}

#[wasm_bindgen]
impl WasmIr2Function {
    #[wasm_bindgen(getter)]
    pub fn function_id(&self) -> usize {
        self.function_id
    }

    #[wasm_bindgen(getter)]
    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    pub fn get_instruction(&self, index: usize) -> Option<String> {
        self.instructions.get(index).cloned()
    }

    pub fn get_instructions(&self) -> Vec<String> {
        self.instructions.clone()
    }
}

/// The IR2 (second-stage Intermediate Representation)
#[wasm_bindgen]
pub struct WasmIr2 {
    ir2: HashMap<FunctionId, Ir2Function>,
}

#[wasm_bindgen]
impl WasmIr2 {
    #[wasm_bindgen(getter)]
    pub fn function_count(&self) -> usize {
        self.ir2.len()
    }

    pub fn get_function_ids(&self) -> Vec<usize> {
        self.ir2.keys().map(|id| id.0).collect()
    }

    pub fn get_function(&self, function_id: usize) -> Option<WasmIr2Function> {
        self.ir2
            .get(&FunctionId(function_id))
            .map(|f| WasmIr2Function {
                function_id,
                instructions: f.code.iter().map(|i| format!("{}", i)).collect(),
            })
    }

    pub fn debug_string(&self) -> String {
        format!("{:#?}", self.ir2)
    }
}

/// A single UCode function
#[wasm_bindgen]
pub struct WasmUCodeFunction {
    function_id: usize,
    instructions: Vec<String>,
}

#[wasm_bindgen]
impl WasmUCodeFunction {
    #[wasm_bindgen(getter)]
    pub fn function_id(&self) -> usize {
        self.function_id
    }

    #[wasm_bindgen(getter)]
    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    pub fn get_instruction(&self, index: usize) -> Option<String> {
        self.instructions.get(index).cloned()
    }

    pub fn get_instructions(&self) -> Vec<String> {
        self.instructions.clone()
    }
}

/// The UCode (micro-code before final BF generation)
#[wasm_bindgen]
pub struct WasmUCode {
    ucode: UCodeProgram,
}

#[wasm_bindgen]
impl WasmUCode {
    #[wasm_bindgen(getter)]
    pub fn function_count(&self) -> usize {
        self.ucode.functions.len()
    }

    pub fn get_function_ids(&self) -> Vec<usize> {
        self.ucode.functions.keys().map(|id| id.0).collect()
    }

    pub fn get_function(&self, function_id: usize) -> Option<WasmUCodeFunction> {
        self.ucode
            .functions
            .get(&FunctionId(function_id))
            .map(|instrs| WasmUCodeFunction {
                function_id,
                instructions: instrs.iter().map(|i| format!("{}", i)).collect(),
            })
    }

    pub fn get_entry_point(&self, function_id: usize) -> Option<usize> {
        self.ucode
            .entry_points
            .get(&FunctionId(function_id))
            .map(|loc| loc.0)
    }

    pub fn debug_string(&self) -> String {
        format!("{:#?}", self.ucode)
    }
}

/// The final BF program
#[wasm_bindgen]
pub struct WasmBfProgram {
    instructions: Vec<BfInstruction>,
}

#[wasm_bindgen]
impl WasmBfProgram {
    #[wasm_bindgen(getter)]
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn to_string(&self) -> String {
        bf_program_string(&self.instructions)
    }

    pub fn get_instruction(&self, index: usize) -> Option<String> {
        self.instructions.get(index).map(|i| i.to_string())
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn token_to_wasm(loc_token: &Locatable<Token>) -> WasmToken {
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
    WasmToken {
        kind,
        value,
        span_start: loc_token.loc.span.offset(),
        span_len: loc_token.loc.span.len(),
    }
}

fn bf_program_string(program: &[BfInstruction]) -> String {
    program.iter().map(|i| i.to_string()).collect::<String>()
}

// ============================================================================
// Compilation Step Functions - Each returns an exported type
// ============================================================================

#[derive(Debug, Serialize)]
struct TokenizationError {
    error: Report,
}

/// Tokenize source code and return a WasmTokens object
#[wasm_bindgen]
pub fn tokenize(source: &str) -> Result<WasmTokens, JsValue> {
    let mut lexer = Lexer::new(source, None);
    let tokens = lexer.tokenize_with_locations();
    let tokens = match tokens {
        Ok(toks) => toks,
        Err(e) => {
            let tokenization_error = TokenizationError { error: e };
            return Err(serde_wasm_bindgen::to_value(&tokenization_error)?);
        }
    };
    let tokens = lexer
        .tokenize_with_locations()
        .map_err(|e| JsValue::from_str(&format!("tokenize error: {}", e)))?;
    Ok(WasmTokens { tokens })
}

/// Parse tokens into an AST
#[wasm_bindgen]
pub fn parse(tokens: &WasmTokens, source: &str) -> Result<WasmAst, JsValue> {
    let source_arc = Arc::new(source.to_string());
    let mut source_annotator = SourceAnnotation {
        source_code: SourceCodeOrigin::Anon(source_arc.clone()),
    };

    let mut parser = crate::parser::Parser::new(tokens.tokens.clone(), &mut source_annotator);
    let ast = parser
        .parse_program()
        .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

    Ok(WasmAst {
        ast,
        source: source_arc,
    })
}

/// Type check the AST
#[wasm_bindgen]
pub fn type_check(ast: &WasmAst) -> WasmTypedAst {
    let typed = type_annotate_program(ast.ast.clone());
    WasmTypedAst { typed }
}

/// Generate IR from typed AST
#[wasm_bindgen]
pub fn gen_ir(typed: &WasmTypedAst) -> WasmIr {
    let ir = generate_ir(&typed.typed);
    WasmIr { ir }
}

/// Generate IR2 from IR
#[wasm_bindgen]
pub fn gen_ir2(ir: &WasmIr) -> WasmIr2 {
    let ir2 = generate_ir2(&ir.ir);
    WasmIr2 { ir2 }
}

/// Generate UCode from IR2
#[wasm_bindgen]
pub fn gen_ucode(ir2: &WasmIr2) -> WasmUCode {
    let ucode = crate::ucodegen::BfGenerator::ucodegen_program(ir2.ir2.clone());
    WasmUCode { ucode }
}

/// Generate final BF code from UCode
#[wasm_bindgen]
pub fn gen_bf(ucode: &WasmUCode, typed: &WasmTypedAst) -> Result<WasmBfProgram, JsValue> {
    let entry_point = typed
        .typed
        .function_name_mapping
        .iter()
        .find_map(|(id, name)| if name == "main" { Some(*id) } else { None })
        .ok_or_else(|| JsValue::from_str("No main function found"))?;

    let bf = codegen_program(ucode.ucode.clone(), entry_point);
    Ok(WasmBfProgram { instructions: bf })
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Compile source code directly to BF string (convenience function)
#[wasm_bindgen]
pub fn compile_to_bf(source: &str) -> Result<String, JsValue> {
    let tokens = tokenize(source)?;
    let ast = parse(&tokens, source)?;
    let typed = type_check(&ast);
    let ir = gen_ir(&typed);
    let ir2 = gen_ir2(&ir);
    let ucode = gen_ucode(&ir2);
    let bf = gen_bf(&ucode, &typed)?;
    Ok(bf.to_string())
}

#[wasm_bindgen]
pub fn init() {
    console_error_panic_hook::set_once();
}
