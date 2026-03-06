//! Mainframe Code Wiki — auto-generated documentation for the z/OS ecosystem.
//!
//! Generates a comprehensive Markdown + Mermaid wiki covering all 9 languages,
//! 5 data systems, 12+ subsystems, system services, and the z/OSMF REST API.

pub mod api;
pub mod callgraph;
pub mod crossref;
pub mod data;
pub mod datadict;
pub mod index;
pub mod languages;
pub mod programs;
pub mod screens;
pub mod subsystems;
pub mod system;

use std::fs;
use std::path::PathBuf;

use thiserror::Error;
use tracing::info;

/// Errors from wiki generation.
#[derive(Error, Debug)]
pub enum WikiError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Source directory not found: {0}")]
    SourceNotFound(PathBuf),

    #[error("Generation error: {0}")]
    Generation(String),
}

pub type WikiResult<T> = Result<T, WikiError>;

/// Output format options.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WikiFormat {
    Markdown,
    MarkdownNoMermaid,
}

/// Configuration for wiki generation.
#[derive(Debug, Clone)]
pub struct WikiConfig {
    /// Root directory containing source files to analyze.
    pub source_dir: PathBuf,
    /// Output directory for generated wiki.
    pub output_dir: PathBuf,
    /// Copybook search paths.
    pub include_paths: Vec<PathBuf>,
    /// BMS map directory.
    pub bms_dir: Option<PathBuf>,
    /// JCL directory.
    pub jcl_dir: Option<PathBuf>,
    /// REXX exec directory.
    pub rexx_dir: Option<PathBuf>,
    /// HLASM source directory.
    pub hlasm_dir: Option<PathBuf>,
    /// PL/I source directory.
    pub pli_dir: Option<PathBuf>,
    /// CLIST directory.
    pub clist_dir: Option<PathBuf>,
    /// PARMLIB member directory.
    pub parmlib_dir: Option<PathBuf>,
    /// Wiki title.
    pub title: String,
    /// Output format.
    pub format: WikiFormat,
    /// Generate z/OS system reference pages.
    pub system_ref: bool,
    /// Verbose output.
    pub verbose: bool,
}

impl Default for WikiConfig {
    fn default() -> Self {
        Self {
            source_dir: PathBuf::from("."),
            output_dir: PathBuf::from("./wiki"),
            include_paths: Vec::new(),
            bms_dir: None,
            jcl_dir: None,
            rexx_dir: None,
            hlasm_dir: None,
            pli_dir: None,
            clist_dir: None,
            parmlib_dir: None,
            title: "Mainframe System Wiki".to_string(),
            format: WikiFormat::Markdown,
            system_ref: false,
            verbose: false,
        }
    }
}

/// Collected analysis results passed between generators.
pub struct WikiOutput {
    /// Programs discovered during scanning.
    pub programs: Vec<programs::ProgramDoc>,
    /// Call graph edges.
    pub call_edges: Vec<open_mainframe_assess::CallEdge>,
    /// Data dictionary entries.
    pub data_dict: Vec<datadict::DataDictEntry>,
    /// BMS screen docs.
    pub screen_docs: Vec<screens::ScreenDoc>,
    /// Cross-reference entries.
    pub xrefs: Vec<crossref::CrossRefEntry>,
}

/// Main wiki generator.
pub struct WikiGenerator {
    config: WikiConfig,
}

impl WikiGenerator {
    pub fn new(config: WikiConfig) -> Self {
        Self { config }
    }

    /// Generate the full wiki.
    pub fn generate(&self) -> WikiResult<()> {
        let src = &self.config.source_dir;
        if !src.exists() {
            return Err(WikiError::SourceNotFound(src.clone()));
        }

        info!(output = %self.config.output_dir.display(), "Generating wiki");

        self.create_dirs()?;

        let mermaid = self.config.format == WikiFormat::Markdown;

        // Phase 1: Scan source files and build program docs
        let (program_docs, call_edges, data_dict) = self.analyze_sources()?;

        // Phase 2: Parse BMS screens
        let screen_docs = if let Some(ref bms_dir) = self.config.bms_dir {
            screens::generate_screens(bms_dir)?
        } else {
            Vec::new()
        };

        // Phase 3: Build cross-references
        let xrefs = crossref::build_crossrefs(&program_docs, &call_edges);

        let output = WikiOutput {
            programs: program_docs,
            call_edges,
            data_dict,
            screen_docs,
            xrefs,
        };

        // Phase 4: Generate all pages
        languages::generate_language_pages(&self.config)?;
        programs::generate_program_pages(&self.config, &output.programs)?;
        data::generate_data_pages(&self.config)?;
        datadict::generate_datadict_page(&self.config, &output.data_dict)?;
        subsystems::generate_subsystem_pages(&self.config)?;
        screens::write_screen_pages(&self.config, &output.screen_docs)?;

        if self.config.system_ref {
            system::generate_system_pages(&self.config)?;
            api::generate_api_pages(&self.config)?;
        }

        callgraph::generate_callgraph_page(&self.config, &output.call_edges, mermaid)?;
        crossref::generate_crossref_page(&self.config, &output.xrefs)?;

        // Phase 5: Master index + runtime
        index::generate_index_page(&self.config, &output, mermaid)?;
        self.generate_runtime_page()?;

        info!("Wiki generation complete");
        Ok(())
    }

    fn create_dirs(&self) -> WikiResult<()> {
        let out = &self.config.output_dir;
        let dirs = [
            out.to_path_buf(),
            out.join("languages"),
            out.join("languages/cobol/programs"),
            out.join("languages/jcl/jobs"),
            out.join("languages/rexx/execs"),
            out.join("languages/hlasm/modules"),
            out.join("languages/pli/programs"),
            out.join("languages/clist/scripts"),
            out.join("languages/easytrieve/programs"),
            out.join("languages/natural/programs"),
            out.join("languages/focus/requests"),
            out.join("data"),
            out.join("subsystems/cics/screens"),
            out.join("system"),
            out.join("api"),
        ];
        for d in &dirs {
            fs::create_dir_all(d)?;
        }
        Ok(())
    }

    fn analyze_sources(
        &self,
    ) -> WikiResult<(
        Vec<programs::ProgramDoc>,
        Vec<open_mainframe_assess::CallEdge>,
        Vec<datadict::DataDictEntry>,
    )> {
        let mut scan_config =
            open_mainframe_assess::ScanConfig::new(&self.config.source_dir);
        for inc in &self.config.include_paths {
            scan_config = scan_config.with_copybook_path(inc);
        }

        let scanner = open_mainframe_assess::Scanner::new(scan_config);
        let scan_result = scanner
            .scan()
            .map_err(|e| WikiError::Generation(e.to_string()))?;

        let mut program_docs = Vec::new();
        let mut data_dict_entries = Vec::new();

        for result in &scan_result.report.results {
            let doc = programs::ProgramDoc::from_analysis(result);
            // Extract data dictionary entries from features
            for feature in &result.features {
                if feature.category == open_mainframe_assess::FeatureCategory::CoreLanguage {
                    data_dict_entries.push(datadict::DataDictEntry {
                        field_name: feature.name.clone(),
                        program: result
                            .program_id
                            .clone()
                            .unwrap_or_else(|| result.file_name.clone()),
                        occurrences: feature.count,
                    });
                }
            }
            program_docs.push(doc);
        }

        // Build call graph from features (CALL, CICS LINK, CICS XCTL markers)
        let mut cg = open_mainframe_assess::CallGraph::new();
        for result in &scan_result.report.results {
            let caller = result
                .program_id
                .clone()
                .unwrap_or_else(|| result.file_name.clone());
            cg.add_program(&caller);
            for feature in &result.features {
                if feature.category == open_mainframe_assess::FeatureCategory::Interoperability
                    || feature.category == open_mainframe_assess::FeatureCategory::Transaction
                {
                    // Feature names like "CALL PROGRAM-X" or "CICS LINK PROGRAM-Y"
                    let callee = feature.name.clone();
                    let call_type = if feature.name.contains("XCTL") {
                        open_mainframe_assess::CallType::CicsXctl
                    } else if feature.name.contains("LINK") {
                        open_mainframe_assess::CallType::CicsLink
                    } else {
                        open_mainframe_assess::CallType::StaticCall
                    };
                    cg.add_edge(&caller, &callee, call_type);
                }
            }
        }
        let call_edges = cg.edges().to_vec();

        // Also scan JCL if configured
        if let Some(ref jcl_dir) = self.config.jcl_dir {
            if jcl_dir.exists() {
                if let Ok(entries) = fs::read_dir(jcl_dir) {
                    for entry in entries.flatten() {
                        let path = entry.path();
                        if let Some(ext) = path.extension() {
                            let ext_lower = ext.to_string_lossy().to_lowercase();
                            if ext_lower == "jcl" || ext_lower == "job" {
                                if let Ok(source) = fs::read_to_string(&path) {
                                    let name = path
                                        .file_stem()
                                        .map(|s| s.to_string_lossy().to_uppercase())
                                        .unwrap_or_default();
                                    program_docs.push(programs::ProgramDoc {
                                        name,
                                        language: programs::SourceLanguage::Jcl,
                                        source_path: path.clone(),
                                        lines: source.lines().count(),
                                        complexity: None,
                                        features: Vec::new(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok((program_docs, call_edges, data_dict_entries))
    }

    fn generate_runtime_page(&self) -> WikiResult<()> {
        let content = generate_runtime_content();
        let path = self.config.output_dir.join("runtime.md");
        fs::write(&path, content)?;
        info!(path = %path.display(), "Generated runtime.md");
        Ok(())
    }
}

fn generate_runtime_content() -> String {
    let mut md = String::new();
    md.push_str("# Language Environment Reference\n\n");
    md.push_str("The Language Environment (LE) provides common runtime services for all z/OS language compilers.\n\n");

    md.push_str("## Runtime Components\n\n");
    md.push_str("| Component | Description |\n");
    md.push_str("|-----------|-------------|\n");
    md.push_str("| CEERUN | LE runtime library |\n");
    md.push_str("| CEELKED | LE link-edit utility |\n");
    md.push_str("| CEEBINIT | Batch initialization |\n");
    md.push_str("| CEEPIPI | Pre-init program interface |\n\n");

    md.push_str("## ABEND Codes\n\n");
    md.push_str("| Code | Description |\n");
    md.push_str("|------|-------------|\n");
    md.push_str("| U0001 | Language-specific condition |\n");
    md.push_str("| U0002 | Out of storage |\n");
    md.push_str("| U0004 | Program check |\n");
    md.push_str("| S0C1 | Operation exception |\n");
    md.push_str("| S0C4 | Protection exception |\n");
    md.push_str("| S0C7 | Data exception |\n");
    md.push_str("| S0CB | Decimal divide exception |\n");
    md.push_str("| S013 | Open error (dataset) |\n");
    md.push_str("| S0B37 | End of volume |\n");
    md.push_str("| S0D37 | No space in directory |\n");
    md.push_str("| S0E37 | No space on volume |\n");
    md.push_str("| S222 | Operator cancel |\n");
    md.push_str("| S322 | Job time exceeded |\n");
    md.push_str("| S806 | Module not found |\n");
    md.push_str("| S837 | Region size exceeded |\n\n");

    md.push_str("## LE Runtime Options\n\n");
    md.push_str("| Option | Default | Description |\n");
    md.push_str("|--------|---------|-------------|\n");
    md.push_str("| ABTERMENC | ABEND | Enclave termination action |\n");
    md.push_str("| ALL31 | ON | All modules AMODE 31 |\n");
    md.push_str("| CBLPSHPOP | ON | COBOL PUSH/POP handle |\n");
    md.push_str("| DEBUG | OFF | Debug mode |\n");
    md.push_str("| ERRCOUNT | 20 | Error count limit |\n");
    md.push_str("| HEAP | 32768 | Initial heap size |\n");
    md.push_str("| POSIX | OFF | POSIX semantics |\n");
    md.push_str("| STACK | 131072 | Stack size |\n");
    md.push_str("| STORAGE | (NONE,NONE,NONE) | Storage initialization |\n");
    md.push_str("| TERMTHDACT | TRACE | Thread termination action |\n");
    md.push_str("| TRAP | ON | Trap conditions |\n");
    md.push_str("| XPLINK | OFF | Extra performance linkage |\n\n");

    md.push_str("## COBOL Intrinsic Functions (77+)\n\n");
    md.push_str("### Numeric Functions\n\n");
    md.push_str("| Function | Description |\n");
    md.push_str("|----------|-------------|\n");
    md.push_str("| ABS | Absolute value |\n");
    md.push_str("| ACOS | Arc cosine |\n");
    md.push_str("| ASIN | Arc sine |\n");
    md.push_str("| ATAN | Arc tangent |\n");
    md.push_str("| COS | Cosine |\n");
    md.push_str("| FACTORIAL | Factorial |\n");
    md.push_str("| INTEGER | Integer part |\n");
    md.push_str("| INTEGER-OF-DATE | Days from date |\n");
    md.push_str("| LOG | Natural logarithm |\n");
    md.push_str("| LOG10 | Base-10 logarithm |\n");
    md.push_str("| MAX | Maximum value |\n");
    md.push_str("| MEAN | Arithmetic mean |\n");
    md.push_str("| MEDIAN | Median value |\n");
    md.push_str("| MIN | Minimum value |\n");
    md.push_str("| MOD | Modulus |\n");
    md.push_str("| NUMVAL | Numeric value |\n");
    md.push_str("| NUMVAL-C | Numeric value with currency |\n");
    md.push_str("| RANDOM | Random number |\n");
    md.push_str("| RANGE | Range (max - min) |\n");
    md.push_str("| REM | Remainder |\n");
    md.push_str("| SIGN | Sign of number |\n");
    md.push_str("| SIN | Sine |\n");
    md.push_str("| SQRT | Square root |\n");
    md.push_str("| SUM | Summation |\n");
    md.push_str("| TAN | Tangent |\n");
    md.push_str("| VARIANCE | Variance |\n\n");

    md.push_str("### String Functions\n\n");
    md.push_str("| Function | Description |\n");
    md.push_str("|----------|-------------|\n");
    md.push_str("| CONCATENATE | Concatenate strings |\n");
    md.push_str("| LENGTH | Length of string |\n");
    md.push_str("| LOWER-CASE | Convert to lowercase |\n");
    md.push_str("| REVERSE | Reverse string |\n");
    md.push_str("| TRIM | Remove leading/trailing spaces |\n");
    md.push_str("| UPPER-CASE | Convert to uppercase |\n\n");

    md.push_str("### Date/Time Functions\n\n");
    md.push_str("| Function | Description |\n");
    md.push_str("|----------|-------------|\n");
    md.push_str("| CURRENT-DATE | Current date and time |\n");
    md.push_str("| DATE-OF-INTEGER | Date from integer |\n");
    md.push_str("| DAY-OF-INTEGER | Julian date from integer |\n");
    md.push_str("| WHEN-COMPILED | Compilation date/time |\n\n");

    md
}

/// CLI argument struct for integration with the open-mainframe binary.
#[derive(clap::Parser, Debug)]
pub struct WikiArgs {
    /// Source directory containing mainframe programs.
    pub source_dir: PathBuf,

    /// Output directory for the generated wiki.
    #[arg(short = 'o', long = "output", default_value = "./wiki")]
    pub output: PathBuf,

    /// Copybook search paths.
    #[arg(short = 'I', long = "include")]
    pub include: Vec<PathBuf>,

    /// BMS map directory.
    #[arg(long = "bms-dir")]
    pub bms_dir: Option<PathBuf>,

    /// JCL directory.
    #[arg(long = "jcl-dir")]
    pub jcl_dir: Option<PathBuf>,

    /// REXX exec directory.
    #[arg(long = "rexx-dir")]
    pub rexx_dir: Option<PathBuf>,

    /// HLASM source directory.
    #[arg(long = "hlasm-dir")]
    pub hlasm_dir: Option<PathBuf>,

    /// PL/I source directory.
    #[arg(long = "pli-dir")]
    pub pli_dir: Option<PathBuf>,

    /// CLIST directory.
    #[arg(long = "clist-dir")]
    pub clist_dir: Option<PathBuf>,

    /// PARMLIB member directory.
    #[arg(long = "parmlib-dir")]
    pub parmlib_dir: Option<PathBuf>,

    /// Wiki title.
    #[arg(long = "title", default_value = "Mainframe System Wiki")]
    pub title: String,

    /// Disable Mermaid diagrams.
    #[arg(long = "no-mermaid")]
    pub no_mermaid: bool,

    /// Generate z/OS system reference pages.
    #[arg(long = "system-ref")]
    pub system_ref: bool,

    /// Verbose output.
    #[arg(short = 'v', long)]
    pub verbose: bool,
}

impl From<WikiArgs> for WikiConfig {
    fn from(args: WikiArgs) -> Self {
        WikiConfig {
            source_dir: args.source_dir,
            output_dir: args.output,
            include_paths: args.include,
            bms_dir: args.bms_dir,
            jcl_dir: args.jcl_dir,
            rexx_dir: args.rexx_dir,
            hlasm_dir: args.hlasm_dir,
            pli_dir: args.pli_dir,
            clist_dir: args.clist_dir,
            parmlib_dir: args.parmlib_dir,
            title: args.title,
            format: if args.no_mermaid {
                WikiFormat::MarkdownNoMermaid
            } else {
                WikiFormat::Markdown
            },
            system_ref: args.system_ref,
            verbose: args.verbose,
        }
    }
}

/// Run the wiki generator from CLI args.
pub fn run_wiki(args: WikiArgs) -> WikiResult<()> {
    let config: WikiConfig = args.into();
    let generator = WikiGenerator::new(config);
    generator.generate()
}
