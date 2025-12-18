use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Fn,
    Let,
    
    // Types
    TypeU8, TypeI8,
    TypeU16, TypeI16,
    TypeU32, TypeI32,
    TypeU64, TypeI64,
    TypeStr,

    // Symbols
    LBrace,     // {
    RBrace,     // }
    LParen,     // (
    RParen,     // )
    LSquare,    // [
    RSquare,    // ]
    Colon,      // :
    DoubleColon,// ::
    Semicolon,  // ;
    Equals,     // =
    LAngle,     // <
    RAngle,     // >
    Comma,      // ,


    // Literals & Identifiers
    Identifier(String),
    IntLiteral(String), // Stored as string to avoid overflow before parsing
    StringLiteral(String),

    // Error / Unknown
    Unknown(char),
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(&c) = self.input.peek() {
            match c {
                // 1. Skip Whitespace
                c if c.is_whitespace() => {
                    self.input.next();
                }

                // 2. Handle Comments (Starts with //)
                '/' => {
                    self.input.next(); // consume first /
                    if let Some(&'/') = self.input.peek() {
                        // It is a comment, consume until newline
                        self.consume_comment();
                    } else {
                        // SR-0 doesn't support division, so a single / is invalid
                        tokens.push(Token::Unknown('/'));
                    }
                }

                // 3. Symbols
                '{' => { tokens.push(Token::LBrace); self.input.next(); }
                '}' => { tokens.push(Token::RBrace); self.input.next(); }
                '(' => { tokens.push(Token::LParen); self.input.next(); }
                ')' => { tokens.push(Token::RParen); self.input.next(); }
                '[' => { tokens.push(Token::LSquare); self.input.next(); }
                ']' => { tokens.push(Token::RSquare); self.input.next(); }
                ':' => {
                    self.input.next();
                    if self.input.peek() == Some(&':') {
                        self.input.next();
                        tokens.push(Token::DoubleColon);
                    } else {
                        tokens.push(Token::Colon);
                    }
                }
                ';' => { tokens.push(Token::Semicolon); self.input.next(); }
                '=' => { tokens.push(Token::Equals); self.input.next(); }
                '<' => { tokens.push(Token::LAngle); self.input.next(); }
                '>' => { tokens.push(Token::RAngle); self.input.next(); }
                ',' => { tokens.push(Token::Comma); self.input.next(); }

                // 4. String Literals
                '"' => {
                    tokens.push(self.read_string());
                }

                // char literal, this is in the format 'c' where c is a single character
                // parse it just like a string then check length == 1 and then convert to int literal
                '\'' => {
                    tokens.extend(self.read_char());
                }

                // 5. Identifiers, Keywords, Types, and Intrinsics
                c if c.is_alphabetic() || c == '_' => {
                    tokens.push(self.read_identifier_or_keyword());
                }

                // 6. Integer Literals
                c if c.is_ascii_digit() => {
                    tokens.push(self.read_number());
                }

                // 7. Unknown/Invalid
                _ => {
                    tokens.push(Token::Unknown(c));
                    self.input.next();
                }
            }
        }

        tokens
    }

    fn consume_comment(&mut self) {
        // consume characters until we hit a newline or EOF
        while let Some(&c) = self.input.peek() {
            if c == '\n' {
                break;
            }
            self.input.next();
        }
    }

    fn read_string(&mut self) -> Token {
        self.input.next(); // consume opening quote
        let mut result = String::new();
        
        while let Some(&c) = self.input.peek() {
            if c == '"' {
                self.input.next(); // consume closing quote
                return Token::StringLiteral(result);
            }
            // Handle escape characters (basic support per spec)
            if c == '\\' {
                self.input.next();
                if let Some(next_char) = self.input.next() {
                   match next_char {
                       'n' => result.push('\n'),
                       't' => result.push('\t'),
                       '\\' => result.push('\\'),
                       '"' => result.push('"'),
                       _ => result.push(next_char),
                   }
                }
            } else {
                result.push(c);
                self.input.next();
            }
        }
        // If we reach here, strings wasn't closed. In a real compiler, error here.
        Token::StringLiteral(result)
    }

    fn read_char(&mut self) -> Vec<Token> {
        self.input.next(); // consume opening quote
        let mut result = String::new();

        while let Some(&c) = self.input.peek() {
            if c == '\'' {
                self.input.next(); // consume closing quote
                assert_eq!(result.len(), 1, "Char literal must be a single character");
                return vec![Token::IntLiteral((result.chars().next().unwrap() as u8).to_string()), Token::TypeU8];
            }
            // Handle escape characters (basic support per spec)
            if c == '\\' {
                self.input.next();
                if let Some(next_char) = self.input.next() {
                    match next_char {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        '\\' => result.push('\\'),
                        '\'' => result.push('\''),
                        _ => result.push(next_char),
                    }
                }
            } else {
                result.push(c);
                self.input.next();
            }
        }
        // If we reach here, strings wasn't closed. In a real compiler, error here.
        assert!(result.len() == 1, "Char literal must be a single character");
        vec![Token::IntLiteral((result.chars().next().unwrap() as u8).to_string()), Token::TypeU8]
    }


    fn read_number(&mut self) -> Token {
        let mut number = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_ascii_digit() {
                number.push(c);
                self.input.next();
            } else {
                break;
            }
        }
        Token::IntLiteral(number)
    }

    fn read_identifier_or_keyword(&mut self) -> Token {
        let mut ident = String::new();
        
        // 1. Read the word
        while let Some(&c) = self.input.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.input.next();
            } else {
                break;
            }
        }


        // 3. Match against Keywords and Types
        match ident.as_str() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "u8" => Token::TypeU8,
            "i8" => Token::TypeI8,
            "u16" => Token::TypeU16,
            "i16" => Token::TypeI16,
            "u32" => Token::TypeU32,
            "i32" => Token::TypeI32,
            "u64" => Token::TypeU64,
            "i64" => Token::TypeI64,
            "str" => Token::TypeStr,
            _ => Token::Identifier(ident),
        }
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to simplify test assertions
    fn expect_tokens(input: &str, expected: Vec<Token>) {
        let mut lexer = Lexer::new(input);
        let output = lexer.tokenize();
        assert_eq!(output, expected, "\nInput: {:?}\nGot: {:?}\nExpected: {:?}", input, output, expected);
    }

    #[test]
    fn test_keywords_and_symbols() {
        let input = "fn let { } ( ) : ; = < >";
        let expected = vec![
            Token::Fn,
            Token::Let,
            Token::LBrace,
            Token::RBrace,
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::Semicolon,
            Token::Equals,
            Token::LAngle,
            Token::RAngle,
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_all_types() {
        let input = "u8 i8 u16 i16 u32 i32 u64 i64 str";
        let expected = vec![
            Token::TypeU8,
            Token::TypeI8,
            Token::TypeU16,
            Token::TypeI16,
            Token::TypeU32,
            Token::TypeI32,
            Token::TypeU64,
            Token::TypeI64,
            Token::TypeStr,
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_intrinsics() {
        // SR-0 treats std::in and std::out as atomic tokens
        let input = "std::in std::out";
        let expected = vec![
            Token::Identifier("std::in".to_string()),
            Token::Identifier("std::out".to_string()), 
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_identifiers_vs_keywords() {
        // Ensures that words starting with keywords but continuing are treated as identifiers
        let input = "fn function let letter std std::unknown";
        let expected = vec![
            Token::Fn,
            Token::Identifier("function".to_string()),
            Token::Let,
            Token::Identifier("letter".to_string()),
            Token::Identifier("std".to_string()),
            Token::Identifier("std::unknown".to_string()),
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_literals() {
        let input = r#"123 0 9999 "hello" "escape\n""#;
        let expected = vec![
            Token::IntLiteral("123".to_string()),
            Token::IntLiteral("0".to_string()),
            Token::IntLiteral("9999".to_string()),
            Token::StringLiteral("hello".to_string()),
            Token::StringLiteral("escape\n".to_string()),
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_comments_and_whitespace() {
        let input = r#"
            let a = 10; // This is a comment
            // This is a full line comment
            let b = 20;
        "#;
        
        // Should ignore whitespace and comments entirely
        let expected = vec![
            Token::Let,
            Token::Identifier("a".to_string()),
            Token::Equals,
            Token::IntLiteral("10".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("b".to_string()),
            Token::Equals,
            Token::IntLiteral("20".to_string()),
            Token::Semicolon,
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_variable_declaration_syntax() {
        // Simulates a real declaration: let my_var: u8 = 255;
        let input = "let my_var: u8 = 255;";
        let expected = vec![
            Token::Let,
            Token::Identifier("my_var".to_string()),
            Token::Colon,
            Token::TypeU8,
            Token::Equals,
            Token::IntLiteral("255".to_string()),
            Token::Semicolon,
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_string_type_declaration() {
        // Simulates: let s: str<10> = "test";
        let input = r#"let s: str<10> = "test";"#;
        let expected = vec![
            Token::Let,
            Token::Identifier("s".to_string()),
            Token::Colon,
            Token::TypeStr,
            Token::LAngle,
            Token::IntLiteral("10".to_string()),
            Token::RAngle,
            Token::Equals,
            Token::StringLiteral("test".to_string()),
            Token::Semicolon,
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_unknown_characters() {
        // SR-0 spec doesn't include $, @, or `
        let input = "let $x = 5;";
        let expected = vec![
            Token::Let,
            Token::Unknown('$'),
            Token::Identifier("x".to_string()),
            Token::Equals,
            Token::IntLiteral("5".to_string()),
            Token::Semicolon,
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_example_compliance_program() {
        // This tests the full "Example Compliance" program provided in the spec
        let input = r#"
        fn main() {
            let a: u8 = 65;
            std::out(a);
        }
        "#;

        let expected = vec![
            Token::Fn,
            Token::Identifier("main".to_string()),
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            
            // let a: u8 = 65;
            Token::Let,
            Token::Identifier("a".to_string()),
            Token::Colon,
            Token::TypeU8,
            Token::Equals,
            Token::IntLiteral("65".to_string()),
            Token::Semicolon,

            // std::out(a);
            Token::Identifier("std::out".to_string()),
            Token::LParen,
            Token::Identifier("a".to_string()),
            Token::RParen,
            Token::Semicolon,

            Token::RBrace,
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_negative_illegal_characters() {
        // SR-0 does not support symbols like @, #, $, or %.
        // The lexer should not crash; it should produce Token::Unknown.
        let input = "let price$ = 100;";
        
        let expected = vec![
            Token::Let,
            Token::Identifier("price".to_string()),
            Token::Unknown('$'), // The illegal character is isolated
            Token::Equals,
            Token::IntLiteral("100".to_string()),
            Token::Semicolon,
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_negative_malformed_intrinsic() {
        // 'std::in' and 'std::out' are valid tokens.
        // 'std::fake' is not. 
        // Based on the lexer logic, this falls through to Identifier("std::fake").
        // This allows the *parser* to later say "Error: 'std::fake' is not a valid function."
        let input = "std::fake";
        
        let expected = vec![
            Token::Identifier("std::fake".to_string()),
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_negative_incomplete_intrinsic() {
        // Input stops abruptly at the colons
        let input = "std::";
        
        // The lexer logic looks for a suffix. If it finds EOF immediately, 
        // suffix is empty. logic: "std::" + "" -> Identifier("std::")
        let expected = vec![
            Token::Identifier("std::".to_string()),
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_negative_identifiers_starting_with_numbers() {
        // SR-0 (and most languages) do not allow identifiers to start with numbers.
        // The lexer usually splits this into an IntLiteral followed by an Identifier.
        // The *parser* will then error because it expects an operator after an int, not an identifier.
        let input = "123var";
        
        let expected = vec![
            Token::IntLiteral("123".to_string()),
            Token::Identifier("var".to_string()),
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_negative_unclosed_string() {
        // The string starts but never ends.
        // The lexer should consume until EOF and return what it has.
        let input = "\"This string never ends";
        
        let expected = vec![
            Token::StringLiteral("This string never ends".to_string()),
        ];
        expect_tokens(input, expected);
    }

    #[test]
    fn test_negative_invalid_escapes() {
        // The lexer allows basic escapes. If an escape is invalid (like \x), 
        // the current implementation simply treats it as the literal character 'x'.
        let input = r#""bad \x escape""#;
        
        let expected = vec![
            Token::StringLiteral("bad x escape".to_string()),
        ];
        expect_tokens(input, expected);
    }
}