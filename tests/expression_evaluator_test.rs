use color_eyre::eyre::{Context, Result};
use std::{
    fs::{read_dir, read_to_string},
    path::Path,
};

use taurox::{
    evaluator::{
        formatter::{BasicFormatter, ValueFormatter},
        ExpressionEvaluator,
    },
    expression::formatter::{ExpressionFormatter, SExpressionFormatter},
    parser::Parser,
};

fn check(input: &str, expected: &str, test_name: &str) {
    let mut parser = Parser::new(input);
    let result = parser.parse_expression();
    let expression_formatter = SExpressionFormatter;
    let value_formatter = BasicFormatter;
    let environment = HashMap::new();
    let actual = match result {
        Ok(ref t) => {
            let v = ExpressionEvaluator::evaluate_expression(t);
            match v {
                Ok(ref v) => format!("{}", value_formatter.format(v)),
                Err(ref e) => format!("{}", value_formatter.format_error(e)),
            }
        }
        Err(ref e) => format!("{}", expression_formatter.format_error(e)),
    };

    assert_eq!(actual, expected, "Failed the test {test_name}");
}

#[test]
fn smoke_test() {
    check("true", "true", "smoke");
}

#[test]
fn test_all() -> Result<()> {
    let input_dir = Path::new("./test_data/evaluator/in");
    let output_dir = Path::new("./test_data/evaluator/out");

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
