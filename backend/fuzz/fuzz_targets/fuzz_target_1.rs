#![no_main]

use compiler_bf_target::parser;
use compiler_bf_target::sources::SourceCodeOrigin;
use compiler_bf_target::source_annotation::SourceAnnotation;
use compiler_bf_target::tokenizer::Lexer;
use libfuzzer_sys::fuzz_target;
use std::sync::Arc;

fuzz_target!(|data: &str| {
    let mut lexed = Lexer::new(data, Some("<fuzz generated program>".to_string()));
    if let Ok(tokens) = lexed.tokenize_with_locations() {
        let mut source_annotator = SourceAnnotation {
            source_code: SourceCodeOrigin::Anon(Arc::new(data.to_string())),
        };

        let mut parser = parser::Parser::new(tokens, &mut source_annotator);
        let _ast = parser.parse_program();
    }
});
