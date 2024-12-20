use clap::{Parser, Subcommand, ValueEnum};
use color_eyre::eyre::Result;
use std::path::{Path, PathBuf};
use std::{fs::read_to_string, process::ExitCode};
use taurox::interpreter::error::RuntimeError;
use taurox::parser::ParserError;

#[derive(Debug, Parser)]
#[clap(name = "taurox", version)]
pub struct CLArgs {
    #[clap(subcommand)]
    pub routine: TauroxCommand,
}

#[derive(Debug, Subcommand)]
pub enum TauroxCommand {
    Tokenize {
        path: PathBuf,
        #[clap(long = "format", value_enum, default_value = "basic")]
        format: TokenFormat,
    },
    Parse {
        path: PathBuf,
        #[clap(long = "format", value_enum, default_value = "sexpr")]
        format: ExpressionFormat,
    },
    Evaluate {
        path: PathBuf,
        #[clap(long = "format", value_enum, default_value = "basic")]
        format: ValueFormat,
    },
    Run {
        path: PathBuf,
    },
}

#[derive(Debug, Clone, ValueEnum)]
pub enum TokenFormat {
    Debug,
    Basic,
    Line,
    Pretty,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum ExpressionFormat {
    Debug,
    #[clap(name = "sexpr")]
    SExpr,
    Pretty,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum ValueFormat {
    Debug,
    Basic,
    Pretty,
}

fn main() -> ExitCode {
    taurox_main().expect("Encountered an error!")
}

fn taurox_main() -> Result<ExitCode> {
    color_eyre::install().expect("Can't fail at first call!");
    let args = CLArgs::parse();
    match args.routine {
        TauroxCommand::Tokenize { path, format } => {
            eprintln!("Tokenizing {:?}...", path);
            let src = read_to_string(&path)?;
            if !tokenize(&src, &path, &format) {
                return Ok(ExitCode::from(65));
            }
        }
        TauroxCommand::Parse { path, format } => {
            eprintln!("Parsing {:?}...", path);
            let src = read_to_string(&path)?;
            if !parse(&src, &path, &format) {
                return Ok(ExitCode::from(65));
            }
        }
        TauroxCommand::Evaluate { path, format } => {
            eprintln!("Evaluating {:?}...", path);
            let src = read_to_string(&path)?;
            let res = evaluate(&src, &path, &format);
            match res {
                Ok(_) => {}
                Err(ProgramError::CompileError(e)) => {
                    eprintln!("{e}");
                    return Ok(ExitCode::from(65));
                }
                Err(ProgramError::RuntimeError(e)) => {
                    eprintln!("{e}");
                    return Ok(ExitCode::from(70));
                }
            }
        }
        TauroxCommand::Run { path } => {
            eprintln!("Running {:?}...", path);
            let src = read_to_string(&path)?;
            let res = run(&src, &path);
            match res {
                Ok(_) => {}
                Err(ProgramError::CompileError(e)) => {
                    eprintln!("{e}");
                    return Ok(ExitCode::from(65));
                }
                Err(ProgramError::RuntimeError(e)) => {
                    eprintln!("{e}");
                    return Ok(ExitCode::from(70));
                }
            }
        }
    }
    Ok(ExitCode::SUCCESS)
}

fn tokenize(src: &str, file_path: &Path, format: &TokenFormat) -> bool {
    use taurox::lexer::formatter::{
        BasicFormatter, DebugFormatter, LineFormatter, PrettyFormatter, ToFormatter, TokenFormatter,
    };
    use taurox::lexer::{Lexer, TokenKind};

    let mut scanner = Lexer::new(src, file_path);
    let formatter: Box<dyn TokenFormatter> = match format {
        TokenFormat::Debug => Box::new(ToFormatter::<DebugFormatter>::create_formatter(&scanner)),
        TokenFormat::Basic => Box::new(ToFormatter::<BasicFormatter>::create_formatter(&scanner)),
        TokenFormat::Line => Box::new(ToFormatter::<LineFormatter>::create_formatter(&scanner)),
        TokenFormat::Pretty => Box::new(ToFormatter::<PrettyFormatter>::create_formatter(&scanner)),
    };
    let mut succeeded = true;
    loop {
        match scanner.next_token() {
            Ok(token) => {
                // println!("{}", formatter.format(&token));
                if matches!(token.kind, TokenKind::Eof) {
                    return succeeded;
                }
            }
            Err(error) => {
                eprintln!("{}", formatter.format_lexical_error(&error));
                succeeded = false;
            }
        };
    }
}

fn parse(src: &str, path: &Path, format: &ExpressionFormat) -> bool {
    use taurox::parser::formatter::{
        DebugFormatter, ExpressionFormatter, PrettyFormatter, SExpressionFormatter, ToFormatter,
    };
    use taurox::parser::Parser;

    let mut parser = Parser::new(src, path);
    let formatter: Box<dyn ExpressionFormatter> = match format {
        ExpressionFormat::Debug => {
            Box::new(ToFormatter::<DebugFormatter>::create_formatter(&parser))
        }
        ExpressionFormat::SExpr => Box::new(ToFormatter::<SExpressionFormatter>::create_formatter(
            &parser,
        )),
        ExpressionFormat::Pretty => {
            Box::new(ToFormatter::<PrettyFormatter>::create_formatter(&parser))
        }
    };
    match parser.parse_expression() {
        Ok(expression) => {
            println!("{}", formatter.format(&expression));
            true
        }
        Err(err) => {
            println!("{}", formatter.format_error(&err));
            false
        }
    }
}

enum ProgramError {
    CompileError(ParserError),
    RuntimeError(RuntimeError),
}

fn evaluate(src: &str, path: &Path, format: &ValueFormat) -> std::result::Result<(), ProgramError> {
    use taurox::interpreter::environment::SharedEnvironment;
    use taurox::interpreter::formatter::{
        BasicFormatter as BasicValueFormatter, DebugFormatter as DebugValueFormatter,
        PrettyFormatter as PrettyValueFormatter, ToFormatter as ToValueFormatter, ValueFormatter,
    };
    use taurox::interpreter::{StatementInterpreter, TreeWalkStatementInterpreter};
    use taurox::parser::{
        formatter::{
            DebugFormatter as DebugExpressionFormatter, ExpressionFormatter,
            PrettyFormatter as PrettyExpressionFormatter, SExpressionFormatter,
            ToFormatter as ToExpressionFormatter,
        },
        Parser,
    };

    let mut parser = Parser::new(src, path);
    let expression_formatter: Box<dyn ExpressionFormatter> = match format {
        ValueFormat::Debug => {
            Box::new(ToExpressionFormatter::<DebugExpressionFormatter>::create_formatter(&parser))
        }
        ValueFormat::Basic => {
            Box::new(ToExpressionFormatter::<SExpressionFormatter>::create_formatter(&parser))
        }
        ValueFormat::Pretty => {
            Box::new(ToExpressionFormatter::<PrettyExpressionFormatter>::create_formatter(&parser))
        }
    };
    let expression = match parser.parse_expression() {
        Ok(expr) => expr,
        Err(e) => {
            eprintln!("{}", expression_formatter.format_error(&e));
            return Err(ProgramError::CompileError(e.into()));
        }
    };

    let value_formatter: Box<dyn ValueFormatter> = match format {
        ValueFormat::Debug => Box::new(ToValueFormatter::<DebugValueFormatter>::create_formatter(
            &parser,
        )),
        ValueFormat::Basic => Box::new(ToValueFormatter::<BasicValueFormatter>::create_formatter(
            &parser,
        )),
        ValueFormat::Pretty => Box::new(
            ToValueFormatter::<PrettyValueFormatter>::create_formatter(&parser),
        ),
    };

    let mut environment = SharedEnvironment::new();
    let interpreter = TreeWalkStatementInterpreter;
    let result = match interpreter.evaluate(&expression, &mut environment) {
        Ok(result) => result,
        Err(e) => {
            eprintln!("{}", value_formatter.format_error(&e));
            return Err(ProgramError::RuntimeError(e.into()));
        }
    };

    println!("{}", result);

    Ok(())
}

fn run(src: &str, path: &Path) -> std::result::Result<(), ProgramError> {
    use taurox::interpreter::ProgramState;
    use taurox::interpreter::{Interpreter, TreeWalkInterpreter, TreeWalkStatementInterpreter};
    use taurox::parser::Parser;

    let mut parser = Parser::new(src, path);
    let program = parser.parse().map_err(|e| ProgramError::CompileError(e))?;
    let mut interpreter = TreeWalkInterpreter::<TreeWalkStatementInterpreter>::new(program);
    loop {
        let state = interpreter
            .step()
            .map_err(|e| ProgramError::RuntimeError(e))?;
        match state {
            ProgramState::Run => {}
            ProgramState::Terminate | ProgramState::Return(_) => break,
        }
    }

    Ok(())
}
