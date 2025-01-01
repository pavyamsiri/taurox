use color_eyre::eyre::{Context, Result};
use std::{
    fs::{read_dir, read_to_string},
    path::Path,
};

use taurox::parser::{
    formatter::{ExpressionFormatter, SExpressionFormatter},
    Parser,
};

fn check(input: &str, expected: &str, test_name: &str) {
    let parser = Parser::new(input, test_name.as_ref());
    let formatter = SExpressionFormatter::new(input);

    let expr = match parser.consume_expression() {
        Ok(expression) => expression,
        Err(errors) => {
            let mut buffer = String::new();
            for err in errors {
                formatter.format_error_in_place(&mut buffer, &err);
            }
            assert_eq!(expected, buffer, "Failed the test {test_name} [Error]");
            return;
        }
    };
    let actual = formatter.format(&expr);

    assert_eq!(
        actual, expected,
        "Failed the test {test_name} [SExpression]"
    );
}

#[test]
fn smoke_test() {
    check("", "(1) Unexpected EOF", "smoke");
}

#[test]
fn test_all() -> Result<()> {
    let input_dir = Path::new("./test_data/parser/in");
    let output_dir = Path::new("./test_data/parser/out");

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
            read_to_string(&output_path).context("Failed to open output test data file")?
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
