//! IDCAMS CLI command.

use std::io::{self, BufRead, Read};
use std::path::PathBuf;

use miette::Result;
use open_mainframe_dataset::Idcams;

/// IDCAMS subcommands.
#[derive(clap::Subcommand, Debug)]
pub enum IdcamsAction {
    /// Execute IDCAMS commands from stdin
    Run {
        /// Base directory for datasets
        #[arg(long)]
        dataset_dir: Option<PathBuf>,
    },

    /// Execute a single IDCAMS command
    Exec {
        /// The IDCAMS command to execute
        command: String,

        /// Base directory for datasets
        #[arg(long)]
        dataset_dir: Option<PathBuf>,
    },

    /// Execute IDCAMS commands from a file
    File {
        /// Input file containing IDCAMS commands
        input: PathBuf,

        /// Base directory for datasets
        #[arg(long)]
        dataset_dir: Option<PathBuf>,
    },
}

/// Get the dataset directory.
fn get_dataset_dir(dataset_dir: Option<PathBuf>) -> PathBuf {
    dataset_dir.unwrap_or_else(|| {
        std::env::var("OPEN_MAINFRAME_DATASET_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                std::env::current_dir()
                    .unwrap_or_else(|_| PathBuf::from("."))
                    .join("datasets")
            })
    })
}

/// Run IDCAMS commands.
pub fn run(action: IdcamsAction) -> Result<()> {
    match action {
        IdcamsAction::Run { dataset_dir } => {
            let base_dir = get_dataset_dir(dataset_dir);
            let mut idcams = Idcams::new(&base_dir);

            // Read from stdin
            let stdin = io::stdin();
            let mut input = String::new();

            if atty::is(atty::Stream::Stdin) {
                // Interactive mode - read line by line
                println!("IDCAMS - Enter commands (Ctrl+D to execute):");
                for line in stdin.lock().lines() {
                    let line = line.map_err(|e| miette::miette!("Error reading input: {}", e))?;
                    input.push_str(&line);
                    input.push('\n');
                }
            } else {
                // Pipe mode - read all at once
                stdin.lock().read_to_string(&mut input)
                    .map_err(|e| miette::miette!("Error reading input: {}", e))?;
            }

            let result = idcams
                .execute(&input)
                .map_err(|e| miette::miette!("IDCAMS error: {}", e))?;

            print!("{}", result.output);

            if result.has_errors() {
                std::process::exit(result.return_code as i32);
            }

            Ok(())
        }

        IdcamsAction::Exec { command, dataset_dir } => {
            let base_dir = get_dataset_dir(dataset_dir);
            let mut idcams = Idcams::new(&base_dir);

            let result = idcams
                .execute(&command)
                .map_err(|e| miette::miette!("IDCAMS error: {}", e))?;

            print!("{}", result.output);

            if result.has_errors() {
                std::process::exit(result.return_code as i32);
            }

            Ok(())
        }

        IdcamsAction::File { input, dataset_dir } => {
            let base_dir = get_dataset_dir(dataset_dir);
            let mut idcams = Idcams::new(&base_dir);

            let commands = std::fs::read_to_string(&input)
                .map_err(|e| miette::miette!("Error reading file: {}", e))?;

            let result = idcams
                .execute(&commands)
                .map_err(|e| miette::miette!("IDCAMS error: {}", e))?;

            print!("{}", result.output);

            if result.has_errors() {
                std::process::exit(result.return_code as i32);
            }

            Ok(())
        }
    }
}
