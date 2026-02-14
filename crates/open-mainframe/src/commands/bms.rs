//! BMS map compilation command.
//!
//! Parses BMS macro source files and generates COBOL copybooks
//! for use with CICS programs.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result};

/// Run the BMS compile command.
pub fn run(input: PathBuf, output_dir: Option<PathBuf>) -> Result<()> {
    let source = std::fs::read_to_string(&input)
        .into_diagnostic()
        .map_err(|e| miette::miette!("Failed to read BMS source: {}", e))?;

    tracing::info!("Compiling BMS: {}", input.display());

    // Parse BMS source
    let mut parser = open_mainframe_cics::bms::BmsParser::new();
    let mapset = parser.parse(&source).map_err(|e| {
        miette::miette!("BMS parse error: {}", e)
    })?;

    println!("✓ Parsed mapset: {}", mapset.name);
    println!("  Maps: {}", mapset.maps.len());
    for map in &mapset.maps {
        let named_fields = map.fields.iter().filter(|f| !f.name.is_empty()).count();
        println!("  - {} ({} fields, {}x{})", map.name, named_fields, map.size.0, map.size.1);
    }

    // Generate COBOL copybook
    let generator = open_mainframe_cics::bms::SymbolicMapGenerator::new();
    let copybook = generator.generate(&mapset);

    // Determine output path
    let output_dir = output_dir.unwrap_or_else(|| {
        input.parent().unwrap_or(&PathBuf::from(".")).to_path_buf()
    });

    let copybook_name = mapset.name.to_uppercase();
    let output_path = output_dir.join(format!("{}.cpy", copybook_name));

    std::fs::create_dir_all(&output_dir)
        .into_diagnostic()
        .map_err(|e| miette::miette!("Failed to create output directory: {}", e))?;

    std::fs::write(&output_path, &copybook)
        .into_diagnostic()
        .map_err(|e| miette::miette!("Failed to write copybook: {}", e))?;

    println!("✓ Generated copybook: {}", output_path.display());

    Ok(())
}

/// Compile all BMS files in a directory.
pub fn run_all(input_dir: PathBuf, output_dir: Option<PathBuf>) -> Result<()> {
    let output_dir = output_dir.unwrap_or_else(|| input_dir.clone());
    let mut success = 0;
    let mut failure = 0;

    let entries: Vec<_> = std::fs::read_dir(&input_dir)
        .into_diagnostic()
        .map_err(|e| miette::miette!("Failed to read directory: {}", e))?
        .filter_map(|e| e.ok())
        .filter(|e| {
            let name = e.file_name().to_string_lossy().to_lowercase();
            name.ends_with(".bms")
        })
        .collect();

    if entries.is_empty() {
        return Err(miette::miette!("No .bms files found in {}", input_dir.display()));
    }

    println!("Compiling {} BMS files from {}", entries.len(), input_dir.display());

    for entry in entries {
        let path = entry.path();
        let name = path.file_name().unwrap_or_default().to_string_lossy();

        match run(path.clone(), Some(output_dir.clone())) {
            Ok(()) => {
                success += 1;
            }
            Err(e) => {
                println!("✗ Failed: {} - {}", name, e);
                failure += 1;
            }
        }
    }

    println!();
    println!("Results: {} succeeded, {} failed", success, failure);

    if failure > 0 {
        Err(miette::miette!("{} BMS compilations failed", failure))
    } else {
        Ok(())
    }
}
