use clap::{Parser, Subcommand, ValueEnum};
use color_eyre::eyre::Result;
use std::path::{Path, PathBuf};
use std::{fs::read_to_string, process::ExitCode};
use taurox::resolver::ResolutionError;
use taurox::value::error::RuntimeError;

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
        #[clap(long = "format", value_enum, default_value = "basic")]
        format: ProgramFormat,
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

#[derive(Debug, Clone, ValueEnum)]
pub enum ProgramFormat {
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
                Err(ProgramError::CompileError) => {
                    return Ok(ExitCode::from(65));
                }
                Err(ProgramError::AnalysisError(_)) => {
                    return Ok(ExitCode::from(67));
                }
                Err(ProgramError::RuntimeError(_)) => {
                    return Ok(ExitCode::from(70));
                }
            }
        }
        TauroxCommand::Run { path, format } => {
            eprintln!("Running {:?}...", path);
            let src = read_to_string(&path)?;
            let res = run(&src, &path, &format);
            match res {
                Ok(_) => {}
                Err(ProgramError::CompileError) => {
                    return Ok(ExitCode::from(65));
                }
                Err(ProgramError::AnalysisError(e)) => {
                    eprintln!("{e}");
                    return Ok(ExitCode::from(67));
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
        BasicFormatter, DebugFormatter, LineFormatter, PrettyFormatter, TokenFormatter,
    };
    use taurox::lexer::{Lexer, TokenKind};

    let mut scanner = Lexer::new(src, file_path);
    let formatter: Box<dyn TokenFormatter> = match format {
        TokenFormat::Debug => Box::new(DebugFormatter {}),
        TokenFormat::Basic => Box::new(BasicFormatter::new(src)),
        TokenFormat::Line => Box::new(LineFormatter::new(src)),
        TokenFormat::Pretty => Box::new(PrettyFormatter::new(src, file_path)),
    };
    let mut succeeded = true;
    loop {
        match scanner.next_token() {
            Ok(token) => {
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
        DebugExpressionFormatter, ExpressionFormatter, PrettyExpressionFormatter,
        SExpressionFormatter,
    };
    use taurox::parser::Parser;

    let mut parser = Parser::new(src, path);
    let formatter: Box<dyn ExpressionFormatter> = match format {
        ExpressionFormat::Debug => Box::new(DebugExpressionFormatter {}),
        ExpressionFormat::SExpr => Box::new(SExpressionFormatter::new(src)),
        ExpressionFormat::Pretty => Box::new(PrettyExpressionFormatter::new(src, path)),
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
    CompileError,
    AnalysisError(ResolutionError),
    RuntimeError(RuntimeError),
}

fn evaluate(src: &str, path: &Path, format: &ValueFormat) -> std::result::Result<(), ProgramError> {
    use taurox::environment::SharedEnvironment;
    use taurox::interpreter::context::StdioContext;
    use taurox::interpreter::{StatementInterpreter, TreeWalkStatementInterpreter};
    use taurox::parser::{
        formatter::{
            DebugExpressionFormatter, ExpressionFormatter, PrettyExpressionFormatter,
            SExpressionFormatter,
        },
        Parser,
    };
    use taurox::resolver::Resolver;
    use taurox::value::formatter::{
        BasicFormatter as BasicValueFormatter, DebugFormatter as DebugValueFormatter,
        PrettyFormatter as PrettyValueFormatter, ValueFormatter,
    };

    let mut parser = Parser::new(src, path);
    let expression_formatter: Box<dyn ExpressionFormatter> = match format {
        ValueFormat::Debug => Box::new(DebugExpressionFormatter {}),
        ValueFormat::Basic => Box::new(SExpressionFormatter::new(src)),
        ValueFormat::Pretty => Box::new(PrettyExpressionFormatter::new(src, path)),
    };
    let expression = match parser.parse_expression() {
        Ok(expr) => expr,
        Err(e) => {
            eprintln!("{}", expression_formatter.format_error(&e));
            return Err(ProgramError::CompileError);
        }
    };

    let value_formatter: Box<dyn ValueFormatter> = match format {
        ValueFormat::Debug => Box::new(DebugValueFormatter {}),
        ValueFormat::Basic => Box::new(BasicValueFormatter::new(src)),
        ValueFormat::Pretty => Box::new(PrettyValueFormatter::new(src, path)),
    };

    let mut environment = SharedEnvironment::new();
    let resolver = Resolver::new();
    let resolution = match resolver.resolve_expression_and_consume(&expression) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("{e}");
            return Err(ProgramError::AnalysisError(e.into()));
        }
    };
    let interpreter = TreeWalkStatementInterpreter;
    let mut context = StdioContext;
    let result =
        match interpreter.evaluate(&expression, &mut environment, &mut context, &resolution) {
            Ok(result) => result,
            Err(e) => {
                eprintln!("{}", value_formatter.format_error(&e));
                return Err(ProgramError::RuntimeError(e.into()));
            }
        };

    println!("{}", result);

    Ok(())
}

fn run(src: &str, path: &Path, format: &ProgramFormat) -> std::result::Result<(), ProgramError> {
    use taurox::interpreter::ProgramState;
    use taurox::interpreter::{
        context::StdioContext, Interpreter, TreeWalkInterpreter, TreeWalkStatementInterpreter,
    };
    use taurox::parser::{
        formatter::{BasicParserFormatter, DebugParserFormatter, ParserFormatter},
        Parser,
    };
    use taurox::resolver::Resolver;

    let program_formatter: Box<dyn ParserFormatter> = match format {
        ProgramFormat::Debug => Box::new(DebugParserFormatter {}),
        ProgramFormat::Basic => Box::new(BasicParserFormatter::new(src)),
        ProgramFormat::Pretty => todo!(),
    };

    let mut parser = Parser::new(src, path);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(e) => {
            eprintln!("{}", program_formatter.format_error(&e));
            return Err(ProgramError::CompileError);
        }
    };

    // Static analysis
    let resolver = Resolver::new();
    let resolution = match resolver.resolve_program(&program) {
        Ok(r) => r,
        Err(e) => {
            return Err(ProgramError::AnalysisError(e.into()));
        }
    };

    let mut interpreter =
        TreeWalkInterpreter::<TreeWalkStatementInterpreter, StdioContext>::new(program, resolution);
    let mut context = StdioContext;
    loop {
        let state = interpreter
            .step(&mut context)
            .map_err(|e| ProgramError::RuntimeError(e))?;
        match state {
            ProgramState::Run => {}
            ProgramState::Terminate | ProgramState::Return(_) => break,
        }
    }

    Ok(())
}
