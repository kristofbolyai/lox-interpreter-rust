pub mod tokenizer {
    use convert_case::{Case, Casing};
    use std::fmt::{Display, Formatter};
    use std::iter::Peekable;
    use std::str::Chars;
    use strum::IntoEnumIterator;
    use strum_macros::EnumIter;

    macro_rules! create_token {
        ($token_type:expr) => {
            Token {
                token_type: $token_type,
                lexem: $token_type.token_lexem().to_string(),
            }
        };
    }

    #[derive(Debug, Eq, PartialEq)]
    enum TokenLength {
        Single,
        SingleOrDouble,
        Literal,
        Keyword,
        EOF,
    }

    #[derive(Debug, Eq, PartialEq, EnumIter, Copy, Clone)]
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
                TokenType::LeftParenthesis => TokenLength::Single,
                TokenType::RightParenthesis => TokenLength::Single,
                TokenType::LeftBrace => TokenLength::Single,
                TokenType::RightBrace => TokenLength::Single,
                TokenType::Comma => TokenLength::Single,
                TokenType::Dot => TokenLength::Single,
                TokenType::Minus => TokenLength::Single,
                TokenType::Plus => TokenLength::Single,
                TokenType::Semicolon => TokenLength::Single,
                TokenType::Slash => TokenLength::Single,
                TokenType::Star => TokenLength::Single,
                TokenType::Bang => TokenLength::SingleOrDouble,
                TokenType::BangEqual => TokenLength::SingleOrDouble,
                TokenType::Equal => TokenLength::SingleOrDouble,
                TokenType::EqualEqual => TokenLength::SingleOrDouble,
                TokenType::Greater => TokenLength::SingleOrDouble,
                TokenType::GreaterEqual => TokenLength::SingleOrDouble,
                TokenType::Less => TokenLength::SingleOrDouble,
                TokenType::LessEqual => TokenLength::SingleOrDouble,
                TokenType::EOF => TokenLength::EOF,
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
                TokenType::EOF => "",
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
                if let Some(token) =
                    Self::try_parse_single_or_double_character_token(&mut char_iter)
                {
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

        fn try_parse_single_or_double_character_token(
            char_iter: &mut Peekable<Chars>,
        ) -> Option<Token> {
            let maybe_first_char = char_iter.peek();

            if maybe_first_char == None {
                return None;
            }

            let token_types_matching_first_char: Vec<TokenType> = TokenType::iter()
                .filter(|token_type| token_type.token_length() == TokenLength::SingleOrDouble)
                .filter(|token_type| {
                    let mut token_type_chars = token_type.token_lexem().chars();
                    if let Some(first_token_char) = token_type_chars.next() {
                        return first_token_char.eq(maybe_first_char.unwrap());
                    }

                    false
                })
                .collect();

            // We have no matches for the first char, we can't find any matching tokens
            if token_types_matching_first_char.is_empty() {
                return None;
            }

            // Iterate the character and check whether the next character matches
            let first_char = char_iter.next().unwrap();

            let token_types_matching_full_lexem: Vec<&TokenType> =
                token_types_matching_first_char
                    .iter()
                    .filter(|token_type| {
                        let mut token_type_chars = token_type.token_lexem().chars();
                        // We've already checked the first char
                        token_type_chars.next();

                        if let Some(second_token_char) = token_type_chars.next() {
                            if let Some(second_char) = char_iter.peek() {
                                return second_token_char.eq(second_char);
                            }

                            // Token type has two characters, but char_iter only has
                            // a single value left
                            return false;
                        }

                        // The token type is a single-character type, so it's valid
                        true
                    })
                    .collect();

            if token_types_matching_full_lexem.is_empty() {
                return None;
            }

            let token_type : &TokenType;

            if token_types_matching_full_lexem.len() > 1 {
                let maybe_token_type = token_types_matching_full_lexem.iter().filter(|token_type| token_type.token_lexem().len() == 2).next();

                if maybe_token_type == None {
                    panic!(
                        "Token '{}{}' matched multiple single tokens while parsing single-or-double tokens.",
                        first_char,
                        char_iter.peek().unwrap_or(&'\0')
                    );
                }

                token_type = maybe_token_type.unwrap();
            } else {
                token_type = token_types_matching_full_lexem[0];
            }

            // Check if we used two chars for the lexem, if so, move the char iterator
            if token_type.token_lexem().len() == 2 {
                char_iter.next();
            }

            Some(create_token!(*token_type))
        }

        fn try_parse_single_character_token(char_iter: &mut Peekable<Chars>) -> Option<Token> {
            if let Some(char) = char_iter.peek() {
                let maybe_token_type = TokenType::iter()
                    .filter(|token_type| token_type.token_length() == TokenLength::Single)
                    .filter(|token_type| {
                        if let Some(single_char_token) = token_type.token_lexem().chars().next() {
                            return single_char_token.eq(char);
                        }

                        false
                    })
                    .next();

                if let Some(token_type) = maybe_token_type {
                    // Iterate the character as we have found a token
                    char_iter.next();

                    return Some(create_token!(token_type.clone()));
                }
            }

            None
        }
    }

    #[cfg(test)]
    #[allow(unused_imports)]
    mod tests {
        use super::*;
        use crate::tokenizer::tokenizer::TokenType::*;
        use pretty_assertions::{assert_eq, assert_ne};

        fn assert_tokenizes_without_error(s: &str) -> Vec<Token> {
            let mut tokenizer = Tokenizer::new(s);
            let result = tokenizer.tokenize();

            assert_eq!(result.errors.is_empty(), true);

            result.tokens
        }

        fn assert_tokenizes_with_error(s: &str) {
            let mut tokenizer = Tokenizer::new(s);
            let result = tokenizer.tokenize();

            assert_eq!(result.errors.is_empty(), false);
        }

        fn assert_token_list_matches(result_tokens: Vec<Token>, expected: Vec<TokenType>) {
            let expected_tokens: Vec<Token> = expected
                .into_iter()
                .map(|token_type| Token {
                    lexem: token_type.token_lexem().to_string(),
                    token_type,
                })
                .collect();

            assert_eq!(result_tokens, expected_tokens);
        }

        #[test]
        fn empty_source_returns_eof() {
            let tokens = assert_tokenizes_without_error("");
            assert_token_list_matches(tokens, vec![EOF]);
        }

        #[test]
        fn parenthesis_parses() {
            let tokens = assert_tokenizes_without_error("(()");
            assert_token_list_matches(
                tokens,
                vec![LeftParenthesis, LeftParenthesis, RightParenthesis, EOF],
            );
        }

        #[test]
        fn braces_parses() {
            let tokens = assert_tokenizes_without_error("{{}}");
            assert_token_list_matches(
                tokens,
                vec![LeftBrace, LeftBrace, RightBrace, RightBrace, EOF],
            );
        }

        #[test]
        fn single_tokens_parses() {
            let tokens = assert_tokenizes_without_error("({*.,+*})");
            assert_token_list_matches(
                tokens,
                vec![
                    LeftParenthesis,
                    LeftBrace,
                    Star,
                    Dot,
                    Comma,
                    Plus,
                    Star,
                    RightBrace,
                    RightParenthesis,
                    EOF,
                ],
            )
        }

        #[test]
        fn unexpected_tokens_error() {
            assert_tokenizes_with_error("@,.$(#");
        }

        #[test]
        fn assignment_and_equality_parses() {
            let tokens = assert_tokenizes_without_error("={===}");
            assert_token_list_matches(
                tokens,
                vec![Equal, LeftBrace, EqualEqual, Equal, RightBrace, EOF],
            );
        }

        #[test]
        fn negation_and_inequality_parses() {
            let tokens = assert_tokenizes_without_error("!!===");
            assert_token_list_matches(
                tokens,
                vec![Bang, BangEqual, EqualEqual, EOF],
            );
        }

        #[test]
        fn relational_operations_parses() {
            let tokens = assert_tokenizes_without_error("<<=>>=");
            assert_token_list_matches(
                tokens,
                vec![Less, LessEqual, Greater, GreaterEqual, EOF],
            );
        }
    }
}
