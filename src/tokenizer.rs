pub mod tokenizer {
    use std::fmt::{Display, Formatter};
    use std::iter::Peekable;
    use std::str::Chars;
    use convert_case::{Case, Casing};
    use crate::parser::parser::TokenLength::{Eof, Single, SingleOrDouble};

    macro_rules! create_token {
        ($token_type:expr) => {
            Token {
                token_type: $token_type,
                lexem: $token_type.token_lexem().to_string(),
            }
        };
    }

    #[derive(Debug)]
    enum TokenLength {
        Single,
        SingleOrDouble,
        Literal,
        Keyword,
        Eof
    }

    #[derive(Debug, Eq, PartialEq)]
    pub enum TokenType {
        // Single-character
        LeftParenthesis,
        RightParenthesis,
        LeftBrace,
        RightBrace,
        Comma,
        Dot,
        Minus,
        Plus,
        Semicolon,
        Slash,
        Star,

        // Single or double character,
        Bang,
        BangEqual,
        Equal,
        EqualEqual,
        Greater,
        GreaterEqual,
        Less,
        LessEqual,

        // Special
        EOF,
    }

    impl TokenType {
        fn token_length(&self) -> TokenLength {
            match &self {
                TokenType::LeftParenthesis => Single,
                TokenType::RightParenthesis => Single,
                TokenType::LeftBrace => Single,
                TokenType::RightBrace => Single,
                TokenType::Comma => Single,
                TokenType::Dot => Single,
                TokenType::Minus => Single,
                TokenType::Plus => Single,
                TokenType::Semicolon => Single,
                TokenType::Slash => Single,
                TokenType::Star => Single,
                TokenType::Bang => SingleOrDouble,
                TokenType::BangEqual => SingleOrDouble,
                TokenType::Equal => SingleOrDouble,
                TokenType::EqualEqual => SingleOrDouble,
                TokenType::Greater => SingleOrDouble,
                TokenType::GreaterEqual => SingleOrDouble,
                TokenType::Less => SingleOrDouble,
                TokenType::LessEqual => SingleOrDouble,
                TokenType::EOF => Eof
            }
        }

        fn token_lexem(&self) -> &str {
            match &self {
                TokenType::LeftParenthesis => "(",
                TokenType::RightParenthesis => ")",
                TokenType::LeftBrace => "{",
                TokenType::RightBrace => "}",
                TokenType::Comma => ",",
                TokenType::Dot => ".",
                TokenType::Minus => "-",
                TokenType::Plus => "+",
                TokenType::Semicolon => ";",
                TokenType::Slash => "/",
                TokenType::Star => "*",
                TokenType::Bang => "!",
                TokenType::BangEqual => "!=",
                TokenType::Equal => "=",
                TokenType::EqualEqual => "==",
                TokenType::Greater => ">",
                TokenType::GreaterEqual => ">=",
                TokenType::Less => "<",
                TokenType::LessEqual => "<=",
                TokenType::EOF => ""
            }
        }
    }

    impl Display for TokenType {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self {
                // Non-default cases
                TokenType::LeftParenthesis => write!(f, "LEFT_PAREN"),
                TokenType::RightParenthesis => write!(f, "RIGHT_PAREN"),
                _ => write!(f, "{}", format!("{:?}", self).to_case(Case::UpperSnake)),
            }
        }
    }

    #[derive(Debug, Eq, PartialEq)]
    pub struct Token {
        pub token_type: TokenType,
        pub lexem: String,
    }

    pub enum TokenizerParseError {
        UnexpectedCharacter { line: u64, character: String },
    }

    impl Display for TokenizerParseError {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self {
                TokenizerParseError::UnexpectedCharacter { line, character } => {
                    write!(
                        f,
                        "[line {}] Error: Unexpected character: {}",
                        line, character
                    )
                }
            }
        }
    }

    pub struct TokenizerParseResult {
        pub tokens: Vec<Token>,
        pub errors: Vec<TokenizerParseError>,
    }

    pub struct Tokenizer<'a> {
        source_code: Peekable<Chars<'a>>,
    }

    impl<'a> Tokenizer<'a> {
        pub fn new(s: &str) -> Tokenizer {
            Tokenizer {
                source_code: s.chars().peekable(),
            }
        }

        pub fn tokenize(&mut self) -> TokenizerParseResult {
            let mut tokens: Vec<Token> = Vec::new();
            let mut errors: Vec<TokenizerParseError> = Vec::new();

            let mut char_iter = &mut self.source_code;

            let mut line: u64 = 1;

            loop {
                if let Some(token) = Self::try_parse_single_or_double_character_token(&mut char_iter) {
                    tokens.push(token);
                } else if let Some(token) = Self::try_parse_single_character_token(&mut char_iter) {
                    tokens.push(token);
                } else {
                    // Handle unexpected characters
                    if let Some(char) = char_iter.next() {
                        if char == '\n' {
                            line += 1;
                            continue;
                        }

                        errors.push(TokenizerParseError::UnexpectedCharacter {
                            character: char.to_string(),
                            line,
                        })
                    } else {
                        // Exit the loop if no chars are left
                        break;
                    }
                }
            }

            tokens.push(create_token!(TokenType::EOF));

            TokenizerParseResult { tokens, errors }
        }

        // FIXME: Try to avoid memory allocations from to_string
        fn try_parse_single_or_double_character_token(char_iter: &mut Peekable<Chars>) -> Option<Token> {
            None
        }

        // FIXME: Try to avoid memory allocations from to_string
        fn try_parse_single_character_token(char_iter: &mut Peekable<Chars>) -> Option<Token> {
            if let Some(char) = char_iter.peek() {
                let token = match char {
                    '(' => Some(create_token!(TokenType::LeftParenthesis)),
                    ')' => Some(create_token!(TokenType::RightParenthesis)),
                    '{' => Some(create_token!(TokenType::LeftBrace)),
                    '}' => Some(create_token!(TokenType::RightBrace)),
                    ',' => Some(create_token!(TokenType::Comma)),
                    '.' => Some(create_token!(TokenType::Dot)),
                    '-' => Some(create_token!(TokenType::Minus)),
                    '+' => Some(create_token!(TokenType::Plus)),
                    ';' => Some(create_token!(TokenType::Semicolon)),
                    '/' => Some(create_token!(TokenType::Slash)),
                    '*' => Some(create_token!(TokenType::Star)),
                    _ => None,
                };

                if let Some(_) = token {
                    char_iter.next();
                }

                return token;
            }

            None
        }
    }

    #[cfg(test)]
    #[allow(unused_imports)]
    mod tests {
        use super::*;
        use crate::tokenizer::tokenizer::TokenType::EOF;
        use pretty_assertions::{assert_eq, assert_ne};

        fn assert_tokenizes_without_error(s: &str) -> Vec<Token> {
            let mut tokenizer = Tokenizer::new(s);
            let result = tokenizer.tokenize();

            assert_eq!(result.errors.is_empty(), true);

            result.tokens
        }

        #[test]
        fn empty_source_returns_eof() {
            let tokens = assert_tokenizes_without_error("");
            let mut token_iter = tokens.iter();
            assert_eq!(token_iter.next(), Some(create_token!(TokenType::EOF)));
            assert_eq!(token_iter.next(), None);
        }

        #[test]
        fn parenthesis_parses() {
            let tokens = assert_tokenizes_without_error("");
            let mut token_iter = tokens.iter();
            assert_eq!(
                token_iter.next(),
                Some(&Token {
                    token_type: EOF,
                    lexem: String::new()
                })
            );
            assert_eq!(token_iter.next(), None);
        }
    }
}
