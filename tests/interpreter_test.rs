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
        formatter::{NystromResolverFormatter, ResolverFormatter},
        Resolver,
    },
    value::formatter::{NystromValueFormatter, ValueFormatter},
};

#[test]
fn test_assignment() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/assignment");
    test_engine(input_dir)
}

#[test]
fn test_operator() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/operator");
    test_engine(input_dir)
}

#[test]
fn test_block() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/block");
    test_engine(input_dir)
}

#[test]
fn test_call() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/call");
    test_engine(input_dir)
}

#[test]
fn test_bool() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/bool");
    test_engine(input_dir)
}

#[test]
fn test_class() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/class");
    test_engine(input_dir)
}

#[test]
fn test_closure() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/closure");
    test_engine(input_dir)
}

#[test]
fn test_comments() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/comments");
    test_engine(input_dir)
}

#[test]
fn test_constructor() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/constructor");
    test_engine(input_dir)
}

#[test]
fn test_field() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/field");
    test_engine(input_dir)
}

#[test]
fn test_for() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/for");
    test_engine(input_dir)
}

#[test]
fn test_function() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/function");
    test_engine(input_dir)
}

#[test]
fn test_if() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/if");
    test_engine(input_dir)
}

#[test]
fn test_inheritance() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/inheritance");
    test_engine(input_dir)
}

#[test]
fn test_logical_operator() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/logical_operator");
    test_engine(input_dir)
}

#[test]
fn test_method() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/method");
    test_engine(input_dir)
}

#[test]
fn test_miscellaneous() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/miscellaneous");
    test_engine(input_dir)
}

#[test]
fn test_nil() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/nil");
    test_engine(input_dir)
}

#[test]
fn test_number() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/number");
    test_engine(input_dir)
}

#[test]
fn test_print() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/print");
    test_engine(input_dir)
}

#[test]
fn test_return() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/return");
    test_engine(input_dir)
}

#[test]
fn test_string() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/string");
    test_engine(input_dir)
}

#[test]
fn test_super() -> Result<()> {
    let input_dir = Path::new("./test_data/interpreter/super");
    test_engine(input_dir)
}

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
        let resolver_formatter = NystromResolverFormatter::new(&self.source);
        let value_formatter = NystromValueFormatter::new(&self.source);

        let mut parser_buffer = String::new();
        let program = 'program: loop {
            match parser.parse() {
                Ok(Some(program)) => break 'program program,
                Ok(None) => {
                    let actual = parser_buffer.trim_end();
                    assert_eq!(
                        self.compiler_errors, actual,
                        "Failed test {} at compilation stage.",
                        self.name,
                    );
                    return;
                }
                Err(e) => {
                    parser_formatter.format_error_in_place(&mut parser_buffer, &e);
                    parser_buffer.push('\n');
                }
            }
        };

        let resolution = match resolver.resolve_program_report_errors(&program) {
            Ok(r) => r,
            Err(errors) => {
                let mut buffer = String::new();
                for (index, e) in errors.iter().enumerate() {
                    resolver_formatter.format_error_in_place(&mut buffer, &e);
                    if index < errors.len() - 1 {
                        buffer.push('\n');
                    }
                }
                assert_eq!(
                    self.compiler_errors, buffer,
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

        println!("Parsing {path:?}");
        let test_case = parse_test_case(&path)?;
        println!("Checking {}", test_case.name);

        let res = std::panic::catch_unwind(|| {
            test_case.check();
        });
        if res.is_err() {
            println!("\tFails test case {}", test_case.name);
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
        } else if let Some(comment_index) = line.find("// [line ") {
            let body = &line[comment_index..]
                .strip_prefix("// [line ")
                .unwrap()
                .trim();
            let (left, right) = body.split_once("] ").unwrap();
            let line_number: usize = left.parse().unwrap();
            let expected = format!("({}) [Compiler] {right}", line_number);
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
