//! CLI for the zOS-clone mainframe emulator.

use clap::Parser;
use miette::Result;

mod error;

pub use error::CliError;

#[derive(Parser, Debug)]
#[command(name = "zos-clone")]
#[command(author, version, about = "zOS-clone mainframe emulator", long_about = None)]
struct Cli {
    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,
}

fn main() -> Result<()> {
    let _cli = Cli::parse();

    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    tracing::info!("zOS-clone starting");

    Ok(())
}
