use clap::{Parser, Subcommand, ValueEnum};
use color_eyre::eyre::Result;
use std::path::{Path, PathBuf};
use std::{fs::read_to_string, process::ExitCode};

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
    Compile {
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
    Nystrom,
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
                Err(ProgramError::AnalysisError) => {
                    return Ok(ExitCode::from(67));
                }
                Err(ProgramError::RuntimeError) => {
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
                Err(ProgramError::AnalysisError) => {
                    return Ok(ExitCode::from(67));
                }
                Err(ProgramError::RuntimeError) => {
                    return Ok(ExitCode::from(70));
                }
            }
        }

        TauroxCommand::Compile { path, format } => {
            eprintln!("Compiling {:?}...", path);
            let src = read_to_string(&path)?;
            let res = compile(&src, &path, &format);
            match res {
                Ok(_) => {}
                Err(ProgramError::CompileError) => {
                    return Ok(ExitCode::from(65));
                }
                Err(ProgramError::AnalysisError) => {
                    return Ok(ExitCode::from(67));
                }
                Err(ProgramError::RuntimeError) => {
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
                println!("{}", formatter.format(&token));
                if matches!(token.kind, TokenKind::Eof) {
                    return succeeded;
                }
            }
            Err(error) => {
                eprintln!("{}", formatter.format_error(&error));
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

    let formatter: Box<dyn ExpressionFormatter> = match format {
        ExpressionFormat::Debug => Box::new(DebugExpressionFormatter {}),
        ExpressionFormat::SExpr => Box::new(SExpressionFormatter::new(src)),
        ExpressionFormat::Pretty => Box::new(PrettyExpressionFormatter::new(src, path)),
    };

    let parser = Parser::new(src, path);
    match parser.consume_expression() {
        Ok(expr) => {
            println!("{}", formatter.format(&expr));
            true
        }
        Err(errors) => {
            for e in errors {
                eprintln!("{}", formatter.format_error(&e));
            }
            false
        }
    }
}

enum ProgramError {
    CompileError,
    AnalysisError,
    RuntimeError,
}

fn evaluate(src: &str, path: &Path, format: &ValueFormat) -> std::result::Result<(), ProgramError> {
    use taurox::environment::SharedEnvironment;
    use taurox::interpreter::context::StdioContext;
    use taurox::interpreter::TreeWalkStatementInterpreter;
    use taurox::parser::{
        formatter::{
            DebugExpressionFormatter, ExpressionFormatter, PrettyExpressionFormatter,
            SExpressionFormatter,
        },
        Parser,
    };
    use taurox::resolver::Resolver;
    use taurox::value::formatter::{
        BasicValueFormatter, DebugValueFormatter, PrettyValueFormatter, ValueFormatter,
    };

    let expression_formatter: Box<dyn ExpressionFormatter> = match format {
        ValueFormat::Debug => Box::new(DebugExpressionFormatter {}),
        ValueFormat::Basic => Box::new(SExpressionFormatter::new(src)),
        ValueFormat::Pretty => Box::new(PrettyExpressionFormatter::new(src, path)),
    };

    let parser = Parser::new(src, path);
    let expression = match parser.consume_expression() {
        Ok(expr) => expr,
        Err(errors) => {
            for e in errors {
                eprintln!("{}", expression_formatter.format_error(&e));
            }
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
    let program = match resolver.resolve_expression_and_consume(&expression) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("{e}");
            return Err(ProgramError::AnalysisError);
        }
    };
    let interpreter = TreeWalkStatementInterpreter;
    let mut context = StdioContext;
    let result = match interpreter.evaluate(&program, &mut environment, &mut context, &expression) {
        Ok(result) => result,
        Err(e) => {
            eprintln!("{}", value_formatter.format_error(&e));
            return Err(ProgramError::RuntimeError);
        }
    };

    println!("{}", result);

    Ok(())
}

fn run(src: &str, path: &Path, format: &ProgramFormat) -> std::result::Result<(), ProgramError> {
    use taurox::interpreter::{context::StdioContext, TreeWalkInterpreter};
    use taurox::parser::{
        formatter::{
            BasicParserFormatter, DebugParserFormatter, NystromParserFormatter, ParserFormatter,
            PrettyParserFormatter,
        },
        Parser,
    };
    use taurox::resolver::{
        formatter::{
            BasicResolverFormatter, DebugResolverFormatter, NystromResolverFormatter,
            PrettyResolverFormatter, ResolverFormatter,
        },
        Resolver,
    };
    use taurox::value::formatter::{
        BasicValueFormatter, DebugValueFormatter, NystromValueFormatter, PrettyValueFormatter,
        ValueFormatter,
    };

    let parser_formatter: Box<dyn ParserFormatter> = match format {
        ProgramFormat::Debug => Box::new(DebugParserFormatter {}),
        ProgramFormat::Basic => Box::new(BasicParserFormatter::new(src)),
        ProgramFormat::Pretty => Box::new(PrettyParserFormatter::new(src, path)),
        ProgramFormat::Nystrom => Box::new(NystromParserFormatter::new(src)),
    };
    let resolver_formatter: Box<dyn ResolverFormatter> = match format {
        ProgramFormat::Debug => Box::new(DebugResolverFormatter {}),
        ProgramFormat::Basic => Box::new(BasicResolverFormatter::new(src)),
        ProgramFormat::Pretty => Box::new(PrettyResolverFormatter::new(src, path)),
        ProgramFormat::Nystrom => Box::new(NystromResolverFormatter::new(src)),
    };
    let value_formatter: Box<dyn ValueFormatter> = match format {
        ProgramFormat::Debug => Box::new(DebugValueFormatter {}),
        ProgramFormat::Basic => Box::new(BasicValueFormatter::new(src)),
        ProgramFormat::Pretty => Box::new(PrettyValueFormatter::new(src, path)),
        ProgramFormat::Nystrom => Box::new(NystromValueFormatter::new(src)),
    };

    let parser = Parser::new(src, path);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(errors) => {
            for e in errors {
                eprintln!("{}", parser_formatter.format_error(&e));
            }
            return Err(ProgramError::CompileError);
        }
    };

    // Static analysis
    let resolver = Resolver::new();
    let program = match resolver.resolve(program) {
        Ok(r) => r,
        Err(errors) => {
            for e in errors {
                eprintln!("{}", resolver_formatter.format_error(&e));
            }
            return Err(ProgramError::AnalysisError);
        }
    };

    let context = StdioContext;
    let interpreter = TreeWalkInterpreter::<StdioContext>::new(context);
    match interpreter.run(&program) {
        Ok(_) => Ok(()),
        Err(e) => {
            eprintln!("{}", value_formatter.format_error(&e));
            Err(ProgramError::RuntimeError)
        }
    }
}

fn compile(
    src: &str,
    path: &Path,
    format: &ProgramFormat,
) -> std::result::Result<(), ProgramError> {
    use taurox::interpreter::{context::StdioContext, TreeWalkInterpreter};
    use taurox::machine::VirtualMachine;
    use taurox::parser::{
        formatter::{
            BasicParserFormatter, DebugParserFormatter, NystromParserFormatter, ParserFormatter,
            PrettyParserFormatter,
        },
        Parser,
    };
    use taurox::resolver::{
        formatter::{
            BasicResolverFormatter, DebugResolverFormatter, NystromResolverFormatter,
            PrettyResolverFormatter, ResolverFormatter,
        },
        Resolver,
    };
    use taurox::value::formatter::{
        BasicValueFormatter, DebugValueFormatter, NystromValueFormatter, PrettyValueFormatter,
        ValueFormatter,
    };

    let parser_formatter: Box<dyn ParserFormatter> = match format {
        ProgramFormat::Debug => Box::new(DebugParserFormatter {}),
        ProgramFormat::Basic => Box::new(BasicParserFormatter::new(src)),
        ProgramFormat::Pretty => Box::new(PrettyParserFormatter::new(src, path)),
        ProgramFormat::Nystrom => Box::new(NystromParserFormatter::new(src)),
    };
    let resolver_formatter: Box<dyn ResolverFormatter> = match format {
        ProgramFormat::Debug => Box::new(DebugResolverFormatter {}),
        ProgramFormat::Basic => Box::new(BasicResolverFormatter::new(src)),
        ProgramFormat::Pretty => Box::new(PrettyResolverFormatter::new(src, path)),
        ProgramFormat::Nystrom => Box::new(NystromResolverFormatter::new(src)),
    };
    let value_formatter: Box<dyn ValueFormatter> = match format {
        ProgramFormat::Debug => Box::new(DebugValueFormatter {}),
        ProgramFormat::Basic => Box::new(BasicValueFormatter::new(src)),
        ProgramFormat::Pretty => Box::new(PrettyValueFormatter::new(src, path)),
        ProgramFormat::Nystrom => Box::new(NystromValueFormatter::new(src)),
    };

    let parser = Parser::new(src, path);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(errors) => {
            for e in errors {
                eprintln!("{}", parser_formatter.format_error(&e));
            }
            return Err(ProgramError::CompileError);
        }
    };

    // Static analysis
    let resolver = Resolver::new();
    let program = match resolver.resolve(program) {
        Ok(r) => r,
        Err(errors) => {
            for e in errors {
                eprintln!("{}", resolver_formatter.format_error(&e));
            }
            return Err(ProgramError::AnalysisError);
        }
    };

    let vm = VirtualMachine::new(StdioContext);
    match vm.run(&program) {
        Ok(_) => Ok(()),
        Err(e) => {
            eprintln!("{}", value_formatter.format_error(&e));
            Err(ProgramError::RuntimeError)
        }
    }
}
