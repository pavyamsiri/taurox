use color_eyre::eyre::{Context, Result};
use proptest::prelude::*;
use std::{
    fs::{read_dir, read_to_string},
    path::Path,
};

use taurox::lexer::{
    formatter::{BasicFormatter, ToFormatter, TokenFormatter},
    Lexer, Token, TokenKind,
};

fn check(input: &str, expected: &str, test_name: &str) {
    let mut scanner = Lexer::new(input);
    let formatter: BasicFormatter = scanner.create_formatter();
    let mut buffer = String::new();
    loop {
        match scanner.next_token() {
            Ok(token) => {
                buffer.push_str(&formatter.format(&token));
                if matches!(token.kind, TokenKind::Eof) {
                    break;
                }
            }
            Err(error) => {
                buffer.push_str(&formatter.format_lexical_error(&error));
            }
        }
        buffer.push('\n');
    }

    assert_eq!(buffer, expected, "Failed the test {test_name}");
}

#[test]
fn smoke_test() {
    check("", "EOF  null", "smoke");
}

#[test]
fn test_all() -> Result<()> {
    let input_dir = Path::new("./test_data/lexer/in");
    let output_dir = Path::new("./test_data/lexer/out");

    let mut succeeded = true;
    for entry in read_dir(input_dir).context("Failed to open input test data folder")? {
        let entry = entry?;
        let path = entry.path();

        let Some(extension) = path.extension() else {
            continue;
        };

        if extension != "lox" {
            continue;
        }

        let test_name = AsRef::<Path>::as_ref(
            path.file_name()
                .expect("File name can't be none as the path is to a real file."),
        );

        let input = read_to_string(&path).context("Failed to open input test data file")?;

        let expected = {
            let output_file_name = test_name.with_extension("txt");
            let output_path = output_dir.join(output_file_name);
            read_to_string(output_path).context("Failed to open output test data file")?
        };

        let res = std::panic::catch_unwind(|| {
            check(&input, &expected, &test_name.to_string_lossy());
        });
        if res.is_err() {
            succeeded = false;
        }
    }

    if !succeeded {
        assert!(false);
    }

    Ok(())
}

// Property-based tests

fn symbol_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("(".to_string()),
        Just("}".to_string()),
        Just("{".to_string()),
        Just("}".to_string()),
        Just(",".to_string()),
        Just(".".to_string()),
        Just("-".to_string()),
        Just("+".to_string()),
        Just(";".to_string()),
        Just("*".to_string()),
        Just("!".to_string()),
        Just("!=".to_string()),
        Just("=".to_string()),
        Just("==".to_string()),
        Just("<".to_string()),
        Just("<=".to_string()),
        Just(">".to_string()),
        Just(">=".to_string()),
        Just("/".to_string()),
    ]
}

fn numeric_literal_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        "[0-9]+".prop_map(|s| s),          // Integer literals
        "[0-9]+\\.[0-9]+".prop_map(|s| s)  // Decimal literals
    ]
}

fn string_literal_strategy() -> impl Strategy<Value = String> {
    "[^\"]*".prop_map(|s: String| format!("\"{}\"", s))
}

fn identifier_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z][a-zA-Z0-9_]*".prop_map(|s: String| s)
}

fn keyword_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("and".to_string()),
        Just("class".to_string()),
        Just("else".to_string()),
        Just("false".to_string()),
        Just("for".to_string()),
        Just("fun".to_string()),
        Just("if".to_string()),
        Just("nil".to_string()),
        Just("or".to_string()),
        Just("print".to_string()),
        Just("return".to_string()),
        Just("super".to_string()),
        Just("this".to_string()),
        Just("true".to_string()),
        Just("var".to_string()),
        Just("while".to_string()),
    ]
}

fn comment_strategy() -> impl Strategy<Value = String> {
    "[^\n]*".prop_map(|s: String| format!("//{}\n", s))
}

fn token_sequence_with_comments_strategy() -> impl Strategy<Value = String> {
    const MIN_TOKEN_COUNT: usize = 1;
    const MAX_TOKEN_COUNT: usize = 100;
    prop::collection::vec(
        prop_oneof![
            symbol_strategy(),
            numeric_literal_strategy(),
            string_literal_strategy(),
            identifier_strategy(),
            keyword_strategy(),
            comment_strategy(),
        ],
        MIN_TOKEN_COUNT..MAX_TOKEN_COUNT,
    )
    .prop_map(|tokens| tokens.join(" "))
}

fn token_sequence_without_comments_strategy() -> impl Strategy<Value = Vec<String>> {
    const MIN_TOKEN_COUNT: usize = 1;
    const MAX_TOKEN_COUNT: usize = 100;
    prop::collection::vec(
        prop_oneof![
            symbol_strategy(),
            numeric_literal_strategy(),
            string_literal_strategy(),
            identifier_strategy(),
            keyword_strategy(),
        ],
        MIN_TOKEN_COUNT..MAX_TOKEN_COUNT,
    )
}

proptest! {
    #[test]
    fn lexer_handles_valid_tokens_without_comments(input in token_sequence_without_comments_strategy()) {
        // Add 1 to include EOF token
        let expected_num_tokens = input.len() + 1;
        let input = input.join(" ");
        let mut scanner = Lexer::new(&input);
        let mut num_tokens = 0;
        loop {
            num_tokens += 1;
            match scanner.next_token() {
                Ok(Token {kind: TokenKind::Eof, ..}) => {
                    break;
                },
                token => {
                    prop_assert!(token.is_ok());
                }
            }
        }
        prop_assert_eq!(num_tokens, expected_num_tokens);
    }

    #[test]
    fn lexer_handles_valid_tokens_with_comments(input in token_sequence_with_comments_strategy()) {
        let mut scanner = Lexer::new(&input);
        loop {
            match scanner.next_token() {
                Ok(Token {kind: TokenKind::Eof, ..}) => {
                    break;
                },
                token => {
                    prop_assert!(token.is_ok());
                }
            }
        }
    }
}
