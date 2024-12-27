use color_eyre::eyre::{Context, Result};
use std::{
    fs::{read_dir, read_to_string},
    path::Path,
};
use taurox::{
    interpreter::{
        context::BufferedContext,
        environment::SharedEnvironment,
        formatter::{BasicFormatter, ValueFormatter},
        resolver::Resolver,
        StatementInterpreter, TreeWalkStatementInterpreter,
    },
    parser::{
        formatter::{ExpressionFormatter, SExpressionFormatter},
        Parser,
    },
};

fn check(input: &str, expected: &str, test_name: &str) {
    let mut parser = Parser::new(input, test_name.as_ref());
    // Create formatters
    let expression_formatter = SExpressionFormatter::new(input);
    let value_formatter = BasicFormatter::new(input);

    let expr = match parser.parse_expression() {
        Ok(expr) => expr,
        Err(e) => {
            let actual = format!("{}", expression_formatter.format_error(&e));
            assert_eq!(actual, expected, "Failed the test {test_name} [value]");
            return;
        }
    };

    let resolver = Resolver::new();
    let resolution = match resolver.resolve_expression_and_consume(&expr) {
        Ok(r) => r,
        Err(e) => {
            let actual = format!("{}", e);
            assert_eq!(actual, expected, "Failed the test {test_name} [value]");
            return;
        }
    };

    let mut environment = SharedEnvironment::new();
    let interpreter = TreeWalkStatementInterpreter;
    let mut context = BufferedContext::new();
    let actual = match interpreter.evaluate(&expr, &mut environment, &mut context, &resolution) {
        Ok(v) => {
            format!("{}", value_formatter.format(&v))
        }
        Err(e) => {
            format!("{}", value_formatter.format_error(&e))
        }
    };

    let actual_io = context.into_data();

    assert_eq!(actual, expected, "Failed the test {test_name} [value]");
    assert_eq!(actual_io, "", "Failed the test {test_name} [IO]");
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
