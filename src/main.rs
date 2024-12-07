mod lexer;
mod token;

use clap::{Parser, Subcommand};
use color_eyre::eyre::{Context, Result};
use lexer::Lexer;
use std::path::PathBuf;
use std::{fs::read_to_string, process::ExitCode};
use token::TokenKind;

#[derive(Debug, Parser)]
#[clap(name = "taurox", version)]
pub struct CLArgs {
    #[clap(subcommand)]
    pub routine: TauroxCommand,
}

#[derive(Debug, Subcommand)]
pub enum TauroxCommand {
    Tokenize { path: PathBuf },
    Parse { path: PathBuf },
    Evaluate { path: PathBuf },
    Run { path: PathBuf },
}

fn main() -> ExitCode {
    taurox_main().expect("Encountered an error!")
}

fn taurox_main() -> Result<ExitCode> {
    color_eyre::install().expect("Can't fail at first call!");
    let args = CLArgs::parse();
    match args.routine {
        TauroxCommand::Tokenize { path } => {
            eprintln!("Tokenizing {:?}...", path);
            let src = read_to_string(path)?;
            let res = tokenize(&src);
            match res {
                Ok(_) => {}
                Err(_) => {
                    return Ok(ExitCode::from(65));
                }
            }
        }
        TauroxCommand::Parse { path } => {
            eprintln!("Parsing {:?}...", path);
            todo!();
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

fn tokenize(src: &str) -> Result<()> {
    let mut scanner = Lexer::new(src);
    loop {
        let token = match scanner.next_token() {
            Ok(t) => t,
            Err(e) => {
                eprintln!("Lexical {e}");
                return Err(e).wrap_err("Lexical error");
            }
        };
        if matches!(token.kind, TokenKind::Eof) {
            eprintln!("Hit EOF!");
            return Ok(());
        }
        eprintln!("Token = {token:?}");
    }
}
