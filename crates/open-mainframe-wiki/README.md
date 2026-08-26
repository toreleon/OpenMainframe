# open-mainframe-wiki

`open-mainframe-wiki` is an automated documentation generator for mainframe application portfolios and the z/OS ecosystem. It analyzes source trees to produce an interconnected Markdown wiki complete with Mermaid call graphs, 3270 screen mockups, data dictionaries, cross-reference matrices, and z/OS system reference manuals.

## Purpose

Understanding legacy mainframe applications requires navigating cross-language calls, screen maps, copybook definitions, and subsystem dependencies. This crate performs static analysis across repository assets and generates a clean, navigable documentation hub suitable for developer onboarding, architecture assessment, and migration planning.

## Capabilities

- **Multi-Language Discovery**: Scans and documents programs across 9 mainframe languages: COBOL, JCL, REXX, HLASM, PL/I, CLIST, Easytrieve Plus, Software AG Natural, and Information Builders FOCUS.
- **Program Dossiers**: Generates per-program documentation under `languages/<lang>/programs/` containing program IDs, lines of code, cyclomatic complexity metrics, detected language features, and paragraph execution sequences.
- **Visual Call Graph Generation**: Builds system-wide Mermaid call graphs in `callgraph.md` showing relationships across static `CALL`, `EXEC CICS LINK`, and `EXEC CICS XCTL` invocations.
- **Centralized Data Dictionary**: Aggregates variables, copybook fields, and working-storage definitions across programs into `datadict.md`.
- **Cross-Reference Indexes**: Correlates programs, datasets, copybooks, transactions, and screens into a queryable cross-reference matrix in `crossref.md`.
- **BMS 3270 Screen Mockups**: Parses BMS macro definition sources (DFHMSD/DFHMDI/DFHMDF) and generates visual screen previews and field attribute tables under `subsystems/cics/screens/`.
- **Data & Storage Reference**: Generates reference pages under `data/` for VSAM datasets (KSDS, ESDS, RRDS), DB2 SQL, IMS DL/I, IDMS CODASYL, and ADABAS.
- **Subsystem & System Reference**: Documents runtime subsystems (CICS, JES2, RACF, TSO, ISPF, MQ, WLM, SMF, MVS, USS, Networking, Crypto) and Language Environment runtime options, ABEND codes, and 77+ COBOL intrinsic functions.
- **z/OSMF API Catalog**: Generates endpoint documentation and routing tables for z/OSMF REST services under `api/`.

## Architecture

```text
               Source Files (.cbl, .bms, .jcl, .rexx, etc.)
                                   │
                                   ▼
┌────────────────────────────────────────────────────────────────────────┐
│                        WikiGenerator (lib.rs)                          │
│                                                                        │
│  Phase 1: Source Analysis (open-mainframe-assess Scanner)              │
│           ├── Program metadata & complexity                            │
│           ├── Call graph edges (CALL, LINK, XCTL)                      │
│           └── Data dictionary elements                                 │
│                                                                        │
│  Phase 2: BMS Screen Parsing (screens.rs)                              │
│           └── DFHMSD/DFHMDI/DFHMDF macros → ScreenDoc                  │
│                                                                        │
│  Phase 3: Cross-Reference Correlation (crossref.rs)                    │
│           └── Build xref maps (Program ↔ Dataset ↔ Screen ↔ Copybook)  │
│                                                                        │
│  Phase 4: Page Generation                                              │
│           ├── languages.rs & programs.rs  (Language & Program pages)   │
│           ├── callgraph.rs                (Mermaid call graphs)        │
│           ├── datadict.rs                 (Data dictionary table)      │
│           ├── screens.rs                  (3270 screen mockups)        │
│           ├── data.rs & subsystems.rs     (Data & Subsystem pages)     │
│           └── system.rs & api.rs          (System & REST API pages)    │
│                                                                        │
│  Phase 5: Master Index & Runtime                                       │
│           ├── index.rs                    (index.md navigation hub)    │
│           └── generate_runtime_page()     (runtime.md reference)       │
└──────────────────────────────────┬─────────────────────────────────────┘
                                   │
                                   ▼
                  Generated Markdown Wiki Output Directory
```

### Module Structure

| Module | Description |
|---|---|
| `lib` | Core orchestrator (`WikiGenerator`, `WikiConfig`, `WikiArgs`, `WikiFormat`, `run_wiki`) and `runtime.md` Language Environment documentation generator. |
| `programs` | Per-program dossier generator (`ProgramDoc`, `SourceLanguage`) analyzing program complexity, features, and source lines. |
| `callgraph` | Call graph generation (`generate_callgraph_page`) rendering Mermaid flowcharts of program invocations. |
| `datadict` | Data dictionary generation (`generate_datadict_page`) aggregating variable declarations across programs. |
| `crossref` | Cross-reference index generator (`generate_crossref_page`) mapping relationships between programs, files, and screens. |
| `screens` | BMS macro parser and 3270 screen mockup renderer (`generate_screens`, `write_screen_pages`). |
| `languages` | Language family overview pages (`generate_language_pages`) for the 9 supported mainframe languages. |
| `data` | Database and dataset storage reference pages (`generate_data_pages`) covering VSAM, DB2, IMS, IDMS, and ADABAS. |
| `subsystems` | Subsystem reference documentation generator (`generate_subsystem_pages`). |
| `system` | z/OS system service reference generator (`generate_system_pages`) covering PARMLIB, SVCs, and console commands. |
| `api` | z/OSMF REST API endpoint catalog generator (`generate_api_pages`). |
| `index` | Master index page generator (`generate_index_page`) providing navigation, portfolio statistics, and Mermaid topology diagrams. |

## Public API and CLI Integration

### CLI Invocations

The generator is exposed as a subcommand through the [`open-mainframe`](../open-mainframe/README.md) CLI:

```text
open-mainframe wiki [OPTIONS] <SOURCE_DIR>

Arguments:
  <SOURCE_DIR>  Source directory containing mainframe programs

Options:
  -o, --output <dir>        Output directory for the generated wiki (default: ./wiki)
  -I, --include <dir>       Copybook search paths
      --bms-dir <dir>       BMS map source directory
      --jcl-dir <dir>       JCL job source directory
      --rexx-dir <dir>      REXX exec directory
      --hlasm-dir <dir>     HLASM source directory
      --pli-dir <dir>       PL/I source directory
      --clist-dir <dir>     CLIST script directory
      --parmlib-dir <dir>   PARMLIB member directory
      --title <TITLE>       Wiki title (default: "Mainframe System Wiki")
      --no-mermaid          Disable Mermaid diagram rendering
      --system-ref          Include z/OS system and REST API reference pages
  -v, --verbose             Enable verbose logging
```

### Library API

```rust
use std::path::PathBuf;
use open_mainframe_wiki::{WikiConfig, WikiFormat, WikiGenerator};

let config = WikiConfig {
    source_dir: PathBuf::from("app/cbl"),
    output_dir: PathBuf::from("docs/generated-wiki"),
    include_paths: vec![PathBuf::from("app/cpy")],
    bms_dir: Some(PathBuf::from("app/bms")),
    jcl_dir: Some(PathBuf::from("app/jcl")),
    title: "CardDemo Core Wiki".to_string(),
    format: WikiFormat::Markdown,
    system_ref: true,
    verbose: true,
    ..Default::default()
};

let generator = WikiGenerator::new(config);
generator.generate().expect("Failed to generate wiki");
```

## Integration and Consumers

### Workspace Dependencies

- [`open-mainframe-assess`](../open-mainframe-assess/README.md) — Source scanner, complexity analysis, call-graph edge extraction, and feature detection.
- [`open-mainframe-cobol`](../open-mainframe-cobol/README.md) — COBOL AST and preprocessor facilities.
- [`open-mainframe-cics`](../open-mainframe-cics/README.md) — BMS map definitions and CICS command references.
- [`open-mainframe-jcl`](../open-mainframe-jcl/README.md) — JCL parsing and job stream structures.
- [`open-mainframe-dataset`](../open-mainframe-dataset/README.md) — Dataset types and ICF catalog definitions.
- [`open-mainframe-encoding`](../open-mainframe-encoding/README.md) — EBCDIC character set definitions.

### Known Consumers

- [`open-mainframe`](../open-mainframe/README.md) (`src/main.rs`) — Directly invokes `open_mainframe_wiki::run_wiki(args)` via the `open-mainframe wiki` CLI command.

## Examples

### Generating a Full Wiki from a CardDemo Checkout

```bash
cargo run --release -p open-mainframe -- wiki app/cbl \
    -o ./carddemo-wiki \
    -I app/cpy \
    -I app/cpy-bms \
    --bms-dir app/bms \
    --jcl-dir app/jcl \
    --title "CardDemo Application Wiki" \
    --system-ref
```

### Programmatic Invocation via `WikiArgs`

```rust
use std::path::PathBuf;
use open_mainframe_wiki::{run_wiki, WikiArgs};

let args = WikiArgs {
    source_dir: PathBuf::from("src/programs"),
    output: PathBuf::from("target/wiki"),
    include: vec![PathBuf::from("src/copybooks")],
    bms_dir: None,
    jcl_dir: None,
    rexx_dir: None,
    hlasm_dir: None,
    pli_dir: None,
    clist_dir: None,
    parmlib_dir: None,
    title: "Application Architecture".to_string(),
    no_mermaid: false,
    system_ref: false,
    verbose: false,
};

run_wiki(args).expect("Wiki generation failed");
```

## Testing

Run tests within the crate:

```bash
cargo test -p open-mainframe-wiki
```

Wiki generation is validated by executing the generator against reference test directories and verifying the output structure, cross-reference links, and Mermaid diagram formatting.

## Limitations

- **Dynamic Call Resolution**: Programs using dynamic `CALL identifier` statements where the target name is determined at runtime cannot be fully resolved statically; they are documented with identifier placeholders.
- **Language Coverage**: In-depth parsing and AST decomposition are provided for COBOL, BMS, and JCL; auxiliary languages (Natural, FOCUS, PL/I, etc.) rely on regex/scanner heuristics and reference templates when specialized parsers are inactive.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [OpenMainframe CLI (`open-mainframe`)](../open-mainframe/README.md)
- [Code Assessment Subsystem (`open-mainframe-assess`)](../open-mainframe-assess/README.md)
- [CICS Subsystem (`open-mainframe-cics`)](../open-mainframe-cics/README.md)
- [COBOL Compiler (`open-mainframe-cobol`)](../open-mainframe-cobol/README.md)
- [z/OSMF Server (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
