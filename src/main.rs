pub mod tokenizer;

use crate::tokenizer::tokenizer::Tokenizer;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut tokenizer = Tokenizer::new(file_contents.as_str());
            let tokenizer_result = tokenizer.tokenize();

            for error in &tokenizer_result.errors {
                eprintln!("{}", error)
            }

            for token in &tokenizer_result.tokens {
                println!("{} {} {}", token.token_type, token.lexem, token.literal.as_ref().unwrap_or(&"null".to_string()))
            }

            // Exit with error ode 65 if any errors are present
            if !&tokenizer_result.errors.is_empty() {
                exit(65);
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
