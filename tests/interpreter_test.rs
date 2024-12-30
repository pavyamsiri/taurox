use std::{
    fs::{read_dir, read_to_string},
    path::Path,
    str::FromStr,
};

use color_eyre::{eyre::Context, Result};
use taurox::{
    interpreter::{
        context::BufferedContext, Interpreter, ProgramState, TreeWalkInterpreter,
        TreeWalkStatementInterpreter,
    },
    parser::{
        formatter::{NystromParserFormatter, ParserFormatter},
        Parser,
    },
    resolver::{
        formatter::{BasicResolverFormatter, ResolverFormatter},
        Resolver,
    },
    value::formatter::{NystromValueFormatter, ValueFormatter},
};

struct TestCase {
    name: String,
    source: String,
    output: String,
    compiler_errors: String,
    runtime_errors: String,
}

impl TestCase {
    pub fn check(&self) {
        let mut parser = Parser::new(&self.source, self.name.as_ref());
        let resolver = Resolver::new();

        let parser_formatter = NystromParserFormatter::new(&self.source);
        let resolver_formatter = BasicResolverFormatter::new(&self.source);
        let value_formatter = NystromValueFormatter::new(&self.source);

        let program = match parser.parse() {
            Ok(p) => p,
            Err(e) => {
                let msg = parser_formatter.format_error(&e);
                assert_eq!(
                    self.compiler_errors, msg,
                    "Failed test {} at compilation stage.",
                    self.name,
                );
                return;
            }
        };

        let resolution = match resolver.resolve_program(&program) {
            Ok(r) => r,
            Err(e) => {
                let msg = resolver_formatter.format_error(&e);
                assert_eq!(
                    self.compiler_errors, msg,
                    "Failed test {} at resolution stage.",
                    self.name,
                );
                return;
            }
        };

        let mut interpreter =
            TreeWalkInterpreter::<TreeWalkStatementInterpreter, BufferedContext>::new(
                program, resolution,
            );
        let mut context = BufferedContext::new();
        loop {
            match interpreter.step(&mut context) {
                Ok(state) => match state {
                    ProgramState::Run => {}
                    ProgramState::Terminate | ProgramState::Return(_) => break,
                },
                Err(e) => {
                    let msg = value_formatter.format_error(&e);
                    assert_eq!(
                        self.runtime_errors, msg,
                        "Failed test {} at runtime.",
                        self.name,
                    );
                    return;
                }
            }
        }
        let msg = context.into_data();
        assert_eq!(self.output, msg, "Failed test {} at print.", self.name);
    }
}

#[test]
fn test_assignment() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/assignment");
    test_engine(input_dir)
}

fn test_engine(input_dir: &Path) -> Result<()> {
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

        let test_case = parse_test_case(&path)?;

        let res = std::panic::catch_unwind(|| {
            test_case.check();
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

fn parse_test_case(input_path: &Path) -> Result<TestCase> {
    let test_name = AsRef::<Path>::as_ref(
        input_path
            .file_name()
            .expect("File name can't be none as the path is to a real file."),
    )
    .to_string_lossy();

    let input = read_to_string(&input_path).context("Failed to open input test data file")?;

    let mut source_lines = Vec::new();
    let mut expected_outputs = Vec::new();
    let mut expected_compiler_errors = Vec::new();
    let mut expected_runtime_errors = Vec::new();

    for (line_index, line) in input.lines().enumerate() {
        if let Some(comment_index) = line.find("// expect:") {
            let expected = line[comment_index..]
                .strip_prefix("// expect:")
                .unwrap()
                .trim()
                .to_string();
            expected_outputs.push(expected);
        } else if let Some(comment_index) = line.find("// Error at") {
            let expected = &line[comment_index..].strip_prefix("// ").unwrap().trim();
            let expected = format!("({}) [Compiler] {expected}", line_index + 1);
            expected_compiler_errors.push(expected);
        } else if let Some(comment_index) = line.find("// expect runtime error:") {
            let expected = &line[comment_index..]
                .strip_prefix("// expect runtime error:")
                .unwrap()
                .trim();
            let expected = format!("({}) [Runtime] {expected}", line_index + 1);
            expected_runtime_errors.push(expected);
        }
        source_lines.push(line.to_string());
    }

    expected_outputs.push(String::from_str("")?);
    Ok(TestCase {
        name: test_name.into_owned(),
        source: source_lines.join("\n"),
        output: expected_outputs.join("\n"),
        compiler_errors: expected_compiler_errors.join("\n"),
        runtime_errors: expected_runtime_errors.join("\n"),
    })
}