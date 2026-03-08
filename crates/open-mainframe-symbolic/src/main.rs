//! CLI entry point for the OpenMainframe Symbolic Execution Engine.

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match open_mainframe_symbolic::cli::parse_args(&args) {
        Ok(command) => match open_mainframe_symbolic::cli::run_command(&command) {
            Ok(output) => print!("{output}"),
            Err(e) => {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        },
        Err(e) => {
            eprintln!("Error: {e}");
            eprintln!("Run with 'help' for usage information.");
            std::process::exit(1);
        }
    }
}
