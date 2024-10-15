pub mod parser {
    use std::fmt::{Display, Formatter};
    use std::str::Chars;

    macro_rules! create_token {
        ($token_type:expr, $lexem:expr) => {
            Token { token_type: $token_type, lexem: $lexem }
        };
    }

    #[derive(Debug)]
    pub struct Token {
        pub token_type: TokenType,
        pub lexem: String,
    }

    #[derive(Debug)]
    pub enum TokenType {
        // Single-character
        LeftParenthesis,
        RightParenthesis,

        // Special
        EOF,
    }

    impl Display for TokenType {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self {
                TokenType::LeftParenthesis => write!(f, "LEFT_PAREN"),
                TokenType::RightParenthesis => write!(f, "RIGHT_PAREN"),
                TokenType::EOF => write!(f, "EOF")
            }
        }
    }

    pub struct Parser<'a> {
        source_code: Chars<'a>,
    }

    impl<'a> Parser<'a> {
        pub fn new(s: &str) -> Parser {
            Parser { source_code: s.chars() }
        }

        pub fn tokenize(&mut self) -> Vec<Token> {
            let mut tokens: Vec<Token> = Vec::new();

            let mut char_iter = &mut self.source_code;

            loop {
                if let Some(token) = Self::try_parse_single_character_token(&mut char_iter) {
                    tokens.push(token)
                } else if char_iter.next() == None {
                    // Exit when the whole source code was parsed
                    break;
                } else {
                    panic!("Cannot parse tokens: {}", &self.source_code.as_str())
                }
            }

            tokens.push(create_token!(TokenType::EOF, String::new()));

            tokens
        }

        fn try_parse_single_character_token(char_iter: &mut Chars) -> Option<Token> {
            if let Some(char) = char_iter.next() {
                // FIXME: Try to avoid memory allocations from to_string
                return match char {
                    '(' => Some(create_token!(TokenType::LeftParenthesis, char.to_string())),
                    ')' => Some(create_token!(TokenType::RightParenthesis, char.to_string())),
                    _ => None
                };
            }

            None
        }
    }
}