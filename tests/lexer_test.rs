use color_eyre::eyre::{Context, Result};
use std::{
    fs::{read_dir, read_to_string},
    path::Path,
};

use taurox::{
    lexer::Lexer,
    token::{
        formatter::{BasicFormatter, ToFormatter, TokenFormatter},
        TokenKind,
    },
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
        eprintln!("Testing {test_name:?}");
        check(&input, &expected, &test_name.to_string_lossy());
    }
    Ok(())
}
