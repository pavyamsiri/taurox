mod lexer;

use clap::{Parser, Subcommand};
use color_eyre::eyre::Result;
use lexer::Lexer;
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
    let scanner = Lexer::new(src);
    dbg!(scanner);

    Ok(())
}
