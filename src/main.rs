use clap::{Parser, Subcommand, ValueEnum};
use color_eyre::eyre::Result;
use std::path::PathBuf;
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
            let res = tokenize(&src, &format);
            match res {
                Ok(true) => {}
                Ok(false) | Err(_) => {
                    return Ok(ExitCode::from(65));
                }
            }
        }
        TauroxCommand::Parse { path, format } => {
            eprintln!("Parsing {:?}...", path);
            let src = read_to_string(path)?;
            let res = parse(&src, &format);
            match res {
                Ok(true) => {}
                Ok(false) | Err(_) => {
                    return Ok(ExitCode::from(65));
                }
            }
        }
        TauroxCommand::Evaluate { path } => {
            eprintln!("Evaluating {:?}...", path);
            todo!();
        }
        TauroxCommand::Run { path } => {
            eprintln!("Running {:?}...", path);
            todo!();
        }
    }
    Ok(ExitCode::SUCCESS)
}

fn tokenize(src: &str, format: &TokenFormat) -> Result<bool> {
    use taurox::lexer::Lexer;
    use taurox::token::formatter::{BasicFormatter, DebugFormatter, ToFormatter, TokenFormatter};
    use taurox::token::TokenKind;

    let mut scanner = Lexer::new(src);
    let formatter: Box<dyn TokenFormatter> = match format {
        TokenFormat::Debug => Box::new(ToFormatter::<DebugFormatter>::create_formatter(&scanner)),
        TokenFormat::Basic => Box::new(ToFormatter::<BasicFormatter>::create_formatter(&scanner)),
    };
    let mut succeeded = true;
    loop {
        match scanner.next_token() {
            Ok(token) => {
                eprintln!("{}", formatter.format(&token));
                if matches!(token.kind, TokenKind::Eof) {
                    return Ok(succeeded);
                }
            }
            Err(error) => {
                eprintln!("{}", formatter.format_lexical_error(&error));
                succeeded = false;
            }
        };
    }
}

fn parse(src: &str, format: &ExpressionFormat) -> Result<bool> {
    use taurox::expression::formatter::{
        DebugFormatter, ExpressionFormatter, SExpressionFormatter,
    };
    use taurox::parser::Parser;

    let mut parser = Parser::new(src);
    let formatter: Box<dyn ExpressionFormatter> = match format {
        ExpressionFormat::Debug => Box::new(DebugFormatter {}),
        ExpressionFormat::SExpr => Box::new(SExpressionFormatter {}),
    };
    let expression = parser.parse_expression();
    eprintln!("{}", formatter.format(&expression));
    Ok(true)
}
