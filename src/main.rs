use clap::{Parser, Subcommand, ValueEnum};
use color_eyre::eyre::Result;
use std::path::PathBuf;
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
    },
    Run {
        path: PathBuf,
    },
}

#[derive(Debug, Clone, ValueEnum)]
pub enum TokenFormat {
    Debug,
    Basic,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum ExpressionFormat {
    Debug,
    #[clap(name = "sexpr")]
    SExpr,
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
            let src = read_to_string(path)?;
            if !tokenize(&src, &format) {
                return Ok(ExitCode::from(65));
            }
        }
        TauroxCommand::Parse { path, format } => {
            eprintln!("Parsing {:?}...", path);
            let src = read_to_string(path)?;
            let res = parse(&src, &format);
            match res {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{e}");
                    return Ok(ExitCode::from(65));
                }
            }
        }
        TauroxCommand::Evaluate { path } => {
            eprintln!("Evaluating {:?}...", path);
            let src = read_to_string(path)?;
            let res = evaluate(&src);
            match res {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{e}");
                    return Ok(ExitCode::from(70));
                }
            }
        }
        TauroxCommand::Run { path } => {
            eprintln!("Running {:?}...", path);
            let src = read_to_string(path)?;
            let res = run(&src);
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

fn tokenize(src: &str, format: &TokenFormat) -> bool {
    use taurox::lexer::formatter::{BasicFormatter, DebugFormatter, ToFormatter, TokenFormatter};
    use taurox::lexer::{Lexer, TokenKind};

    let mut scanner = Lexer::new(src);
    let formatter: Box<dyn TokenFormatter> = match format {
        TokenFormat::Debug => Box::new(ToFormatter::<DebugFormatter>::create_formatter(&scanner)),
        TokenFormat::Basic => Box::new(ToFormatter::<BasicFormatter>::create_formatter(&scanner)),
    };
    let mut succeeded = true;
    loop {
        match scanner.next_token() {
            Ok(token) => {
                println!("{}", formatter.format(&token));
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

fn parse(src: &str, format: &ExpressionFormat) -> Result<()> {
    use taurox::parser::formatter::{DebugFormatter, ExpressionFormatter, SExpressionFormatter};
    use taurox::parser::Parser;

    let mut parser = Parser::new(src);
    let formatter: Box<dyn ExpressionFormatter> = match format {
        ExpressionFormat::Debug => Box::new(DebugFormatter {}),
        ExpressionFormat::SExpr => Box::new(SExpressionFormatter {}),
    };
    let expression = parser.parse_expression()?;
    println!("{}", formatter.format(&expression));
    Ok(())
}

fn evaluate(src: &str) -> Result<()> {
    use taurox::interpreter::environment::Environment;
    use taurox::parser::Parser;

    use taurox::interpreter::{StatementInterpreter, TreeWalkStatementInterpreter};

    let mut parser = Parser::new(src);
    let expression = parser.parse_expression()?;

    let mut environment = Environment::new();
    let interpreter = TreeWalkStatementInterpreter;
    let result = interpreter.evaluate(&expression, &mut environment)?;

    println!("{}", result);

    Ok(())
}

enum ProgramError {
    CompileError(ParserError),
    RuntimeError(RuntimeError),
}

fn run(src: &str) -> std::result::Result<(), ProgramError> {
    use taurox::interpreter::ProgramState;
    use taurox::interpreter::{Interpreter, TreeWalkInterpreter, TreeWalkStatementInterpreter};
    use taurox::parser::Parser;

    let mut parser = Parser::new(src);
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
