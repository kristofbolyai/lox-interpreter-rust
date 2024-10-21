pub mod tokenizer {
    use convert_case::{Case, Casing};
    use std::fmt::{Debug, Display, Formatter};
    use std::iter::Peekable;
    use std::str::Chars;
    use once_cell::sync::Lazy;
    use regex::Regex;
    use strum::IntoEnumIterator;
    use strum_macros::EnumIter;

    macro_rules! create_token {
        ($token_type:expr) => {
            Token {
                token_type: $token_type,
                literal: TokenLiteral::Empty,
            }
        };
    }

    macro_rules! create_string_token {
        ($literal:expr) => {
            Token {
                token_type: TokenType::StringLiteral,
                literal: TokenLiteral::StringLiteral { string_lexeme: $literal },
            }
        };
    }

    macro_rules! create_number_token {
        ($literal:expr) => {
            Token {
                token_type: TokenType::NumberLiteral,
                literal: TokenLiteral::NumberLiteral { number_lexeme: $literal.to_string() },
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

        // Literals
        StringLiteral,
        NumberLiteral,

        // Special
        EOF,
    }

    impl TokenType {
        pub fn token_lexeme(&self) -> String {
            match &self {
                TokenType::LeftParenthesis => "(".to_string(),
                TokenType::RightParenthesis => ")".to_string(),
                TokenType::LeftBrace => "{".to_string(),
                TokenType::RightBrace => "}".to_string(),
                TokenType::Comma => ",".to_string(),
                TokenType::Dot => ".".to_string(),
                TokenType::Minus => "-".to_string(),
                TokenType::Plus => "+".to_string(),
                TokenType::Semicolon => ";".to_string(),
                TokenType::Slash => "/".to_string(),
                TokenType::Star => "*".to_string(),
                TokenType::Bang => "!".to_string(),
                TokenType::BangEqual => "!=".to_string(),
                TokenType::Equal => "=".to_string(),
                TokenType::EqualEqual => "==".to_string(),
                TokenType::Greater => ">".to_string(),
                TokenType::GreaterEqual => ">=".to_string(),
                TokenType::Less => "<".to_string(),
                TokenType::LessEqual => "<=".to_string(),
                TokenType::StringLiteral => panic!("Literal types should not call TokenType#token_lexeme."),
                TokenType::NumberLiteral => panic!("Literal types should not call TokenType#token_lexeme."),
                TokenType::EOF => "".to_string(),
            }
        }

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
                TokenType::StringLiteral => TokenLength::Literal,
                TokenType::NumberLiteral => TokenLength::Literal,
                TokenType::EOF => TokenLength::EOF,
            }
        }
    }

    impl Display for TokenType {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self {
                // Non-default cases
                TokenType::LeftParenthesis => write!(f, "LEFT_PAREN"),
                TokenType::RightParenthesis => write!(f, "RIGHT_PAREN"),
                TokenType::StringLiteral => write!(f, "STRING"),
                TokenType::NumberLiteral => write!(f, "NUMBER"),
                _ => write!(f, "{}", format!("{:?}", self).to_case(Case::UpperSnake)),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum TokenLiteral {
        Empty,
        StringLiteral { string_lexeme: String },
        NumberLiteral { number_lexeme: String },
    }

    impl TokenLiteral {
        fn lexeme(&self) -> String {
            match &self {
                TokenLiteral::Empty => "".to_string(),
                TokenLiteral::StringLiteral { string_lexeme } => string_lexeme.to_string(),
                TokenLiteral::NumberLiteral { number_lexeme } => number_lexeme.to_string()
            }
        }
    }

    impl Display for TokenLiteral {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            static NUMBER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\.(0+[1-9]+|0)0+$").unwrap());

            match &self {
                TokenLiteral::Empty => write!(f, "null"),
                TokenLiteral::StringLiteral { string_lexeme } => write!(f, "{}", string_lexeme.replace("\"", "")),
                TokenLiteral::NumberLiteral { number_lexeme } => {
                    if number_lexeme.contains(".") {
                        write!(f, "{}", NUMBER_RE.replace(number_lexeme, ".$1"))
                    } else {
                        write!(f, "{}.0", number_lexeme)
                    }
                },
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Token {
        pub token_type: TokenType,
        pub literal: TokenLiteral,
    }

    impl Display for Token {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            if &self.token_type.token_length() == &TokenLength::Literal {
                write!(f, "{} {} {}", &self.token_type, &self.literal.lexeme(), &self.literal)
            } else {
                write!(f, "{} {} {}", &self.token_type, &self.token_type.token_lexeme(), &self.literal)
            }
        }
    }

    #[derive(Eq, PartialEq)]
    pub enum TokenizerParseError {
        UnexpectedCharacter { line: u64, character: String },
        UnterminatedStringLiteral { line: u64 },
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
                TokenizerParseError::UnterminatedStringLiteral { line } => {
                    write!(f, "[line {}] Error: Unterminated string.", line)
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
                Self::try_skip_comments(&mut char_iter);

                let string_parse_result = Self::try_parse_string_literals(&mut char_iter, line);

                if let Ok(Some(token)) = string_parse_result {
                    tokens.push(token);
                } else if let Err(error) = string_parse_result {
                    errors.push(error);
                }

                if let Some(token) = Self::try_parse_number_literals(&mut char_iter) {
                    tokens.push(token)
                } else if let Some(token) =
                    Self::try_parse_single_or_double_character_token(&mut char_iter)
                {
                    tokens.push(token);
                } else if let Some(token) = Self::try_parse_single_character_token(&mut char_iter) {
                    tokens.push(token);
                } else {
                    // Handle unexpected characters
                    if let Some(char) = char_iter.next() {
                        match char {
                            '\n' => line += 1,
                            '\t' => (),
                            '\r' => (),
                            ' ' => (),
                            _ => errors.push(TokenizerParseError::UnexpectedCharacter {
                                character: char.to_string(),
                                line,
                            }),
                        }
                    } else {
                        // Exit the loop if no chars are left
                        break;
                    }
                }
            }

            tokens.push(create_token!(TokenType::EOF));

            TokenizerParseResult { tokens, errors }
        }

        fn try_skip_comments(char_iter: &mut Peekable<Chars>) {
            let mut cloned_char_iter = char_iter.clone();
            let maybe_first_char = cloned_char_iter.peek();

            if maybe_first_char == None {
                return;
            }

            let first_char = maybe_first_char.unwrap();

            if !first_char.eq(&'/') {
                return;
            }

            // Skip the first char
            cloned_char_iter.next();

            let maybe_second_char = cloned_char_iter.peek();

            if maybe_second_char == None {
                return;
            }

            let second_char = maybe_second_char.unwrap();

            if !second_char.eq(&'/') {
                return;
            }

            // We have a comment, go until the next line
            while let Some(char) = char_iter.peek() {
                // Break if we've found a new line
                if char.eq(&'\n') {
                    break;
                }

                char_iter.next();
            }
        }

        fn try_parse_number_literals(char_iter: &mut Peekable<Chars>) -> Option<Token> {
            let mut number_string = String::new();

            loop {
                let next_char = char_iter.peek();
                if let None = next_char {
                    break;
                }

                if next_char?.is_numeric() || (!number_string.is_empty() && next_char? == &'.') {
                    number_string.push(char_iter.next().unwrap());
                    continue;
                }

                break;
            }

            if !number_string.is_empty() {
                Some(create_number_token!(number_string))
            } else {
                None
            }
        }

        fn try_parse_string_literals(
            char_iter: &mut Peekable<Chars>,
            line: u64,
        ) -> Result<Option<Token>, TokenizerParseError> {
            let maybe_first_char = char_iter.peek();

            if maybe_first_char == None {
                return Ok(None);
            }

            let first_char = maybe_first_char.unwrap();

            if first_char != &'"' {
                return Ok(None);
            }

            // Iterate to the next character
            char_iter.next();

            let mut literal_terminated = false;
            let mut literal_lexeme = String::new();
            literal_lexeme.push('"');

            while let Some(next_char) = char_iter.next() {
                match next_char {
                    '\n' => return Err(TokenizerParseError::UnterminatedStringLiteral { line }),
                    '"' => literal_terminated = true,
                    _ => literal_lexeme.push(next_char),
                }

                if literal_terminated {
                    break;
                }
            }

            literal_lexeme.push('"');

            if !literal_terminated {
                Err(TokenizerParseError::UnterminatedStringLiteral { line })
            } else {
                Ok(Some(create_string_token!(literal_lexeme)))
            }
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
                    let token_lexeme = token_type.token_lexeme();
                    let mut token_type_chars = token_lexeme.chars();
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

            let token_types_matching_full_lexem: Vec<&TokenType> = token_types_matching_first_char
                .iter()
                .filter(|token_type| {
                    let token_lexeme = token_type.token_lexeme();
                    let mut token_type_chars = token_lexeme.chars();
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

            let token_type: &TokenType;

            if token_types_matching_full_lexem.len() > 1 {
                let maybe_token_type = token_types_matching_full_lexem
                    .iter()
                    .filter(|token_type| token_type.token_lexeme().len() == 2)
                    .next();

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
            if token_type.token_lexeme().len() == 2 {
                char_iter.next();
            }

            Some(create_token!(*token_type))
        }

        fn try_parse_single_character_token(char_iter: &mut Peekable<Chars>) -> Option<Token> {
            if let Some(char) = char_iter.peek() {
                let maybe_token_type = TokenType::iter()
                    .filter(|token_type| token_type.token_length() == TokenLength::Single)
                    .filter(|token_type| {
                        if let Some(single_char_token) =
                            token_type.token_lexeme().chars().next()
                        {
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
        use std::f32::consts::E;
        use std::ptr::null;

        fn assert_tokenizes_without_error(s: &str) -> Vec<Token> {
            let mut tokenizer = Tokenizer::new(s);
            let result = tokenizer.tokenize();

            assert_eq!(result.errors.is_empty(), true);

            result.tokens
        }

        fn assert_tokenizes_with_error(s: &str) -> (Vec<Token>, Vec<TokenizerParseError>) {
            let mut tokenizer = Tokenizer::new(s);
            let result = tokenizer.tokenize();

            assert_eq!(result.errors.is_empty(), false);

            (result.tokens, result.errors)
        }

        fn assert_token_types_matches(result_tokens: Vec<Token>, expected: Vec<TokenType>) {
            let expected_tokens: Vec<Token> = expected
                .into_iter()
                .map(|token_type| Token {
                    token_type,
                    literal: TokenLiteral::Empty,
                })
                .collect();

            assert_eq!(result_tokens, expected_tokens);
        }

        fn assert_token_list_matches(result_tokens: Vec<Token>, expected: Vec<Token>) {
            assert_eq!(result_tokens, expected);
        }

        #[test]
        fn empty_source_returns_eof() {
            let tokens = assert_tokenizes_without_error("");
            assert_token_types_matches(tokens, vec![EOF]);
        }

        #[test]
        fn parenthesis_parses() {
            let tokens = assert_tokenizes_without_error("(()");
            assert_token_types_matches(
                tokens,
                vec![LeftParenthesis, LeftParenthesis, RightParenthesis, EOF],
            );
        }

        #[test]
        fn braces_parses() {
            let tokens = assert_tokenizes_without_error("{{}}");
            assert_token_types_matches(
                tokens,
                vec![LeftBrace, LeftBrace, RightBrace, RightBrace, EOF],
            );
        }

        #[test]
        fn single_tokens_parses() {
            let tokens = assert_tokenizes_without_error("({*.,+*})");
            assert_token_types_matches(
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
        fn unexpected_token_errors() {
            let (_, errors) = assert_tokenizes_with_error("@,.$(#");

            let expected_error_0 = TokenizerParseError::UnexpectedCharacter {
                line: 1,
                character: "@".to_string(),
            };
            let expected_error_1 = TokenizerParseError::UnexpectedCharacter {
                line: 1,
                character: "$".to_string(),
            };
            let expected_error_2 = TokenizerParseError::UnexpectedCharacter {
                line: 1,
                character: "#".to_string(),
            };
            assert!(errors.iter().nth(0).unwrap() == &expected_error_0);
            assert!(errors.iter().nth(1).unwrap() == &expected_error_1);
            assert!(errors.iter().nth(2).unwrap() == &expected_error_2);
        }

        #[test]
        fn assignment_and_equality_parses() {
            let tokens = assert_tokenizes_without_error("={===}");
            assert_token_types_matches(
                tokens,
                vec![Equal, LeftBrace, EqualEqual, Equal, RightBrace, EOF],
            );
        }

        #[test]
        fn negation_and_inequality_parses() {
            let tokens = assert_tokenizes_without_error("!!===");
            assert_token_types_matches(tokens, vec![Bang, BangEqual, EqualEqual, EOF]);
        }

        #[test]
        fn relational_operations_parses() {
            let tokens = assert_tokenizes_without_error("<<=>>=");
            assert_token_types_matches(tokens, vec![Less, LessEqual, Greater, GreaterEqual, EOF]);
        }

        #[test]
        fn comments_get_ignored_parses() {
            let tokens = assert_tokenizes_without_error("()// Comment");
            assert_token_types_matches(tokens, vec![LeftParenthesis, RightParenthesis, EOF]);
        }

        #[test]
        fn comments_get_ignored_only_until_newline_parses() {
            let tokens = assert_tokenizes_without_error("()// Comment\n*(");
            assert_token_types_matches(
                tokens,
                vec![
                    LeftParenthesis,
                    RightParenthesis,
                    Star,
                    LeftParenthesis,
                    EOF,
                ],
            );
        }

        #[test]
        fn whitespace_gets_ignored_parses() {
            let tokens = assert_tokenizes_without_error("(    \t ) \r *");
            assert_token_types_matches(tokens, vec![LeftParenthesis, RightParenthesis, Star, EOF]);
        }

        #[test]
        fn multi_line_with_unexpected_characters_errors() {
            let (tokens, errors) = assert_tokenizes_with_error("# ( \n)\t@");
            assert_token_types_matches(tokens, vec![LeftParenthesis, RightParenthesis, EOF]);

            let expected_error_0 = TokenizerParseError::UnexpectedCharacter {
                line: 1,
                character: "#".to_string(),
            };
            let expected_error_1 = TokenizerParseError::UnexpectedCharacter {
                line: 2,
                character: "@".to_string(),
            };
            assert!(errors.iter().nth(0).unwrap() == &expected_error_0);
            assert!(errors.iter().nth(1).unwrap() == &expected_error_1);
        }

        #[test]
        fn simple_string_literal_parses() {
            let tokens = assert_tokenizes_without_error("\"foo baz\"");
            assert_token_list_matches(
                tokens,
                vec![
                    create_string_token!("foo baz".to_string()),
                    create_token!(EOF),
                ],
            )
        }

        #[test]
        fn multiple_string_literals_parses() {
            let tokens = assert_tokenizes_without_error("\"foo\" \"baz\"");
            assert_token_list_matches(
                tokens,
                vec![
                    create_string_token!("foo".to_string()),
                    create_string_token!("baz".to_string()),
                    create_token!(EOF),
                ],
            )
        }

        #[test]
        fn unterminated_string_errors() {
            let (_, errors) = assert_tokenizes_with_error("\"foo");

            let expected_error = TokenizerParseError::UnterminatedStringLiteral { line: 1 };
            assert!(errors.iter().nth(0).unwrap() == &expected_error);
        }

        #[test]
        fn decimal_numbers_parses() {
            let tokens = assert_tokenizes_without_error("42 124");
            assert_token_list_matches(
                tokens,
                vec![
                    create_number_token!("42"),
                    create_number_token!("124"),
                    create_token!(EOF),
                ],
            )
        }

        #[test]
        fn float_numbers_parses() {
            let tokens = assert_tokenizes_without_error("42.124 56 75.0000 1");
            assert_token_list_matches(
                tokens,
                vec![
                    create_number_token!("42.124"),
                    create_number_token!("56"),
                    create_number_token!("75.0000"),
                    create_number_token!("1"),
                    create_token!(EOF),
                ],
            )
        }
    }
}
