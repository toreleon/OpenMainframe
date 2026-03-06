//! Master index page generator.

use std::fs;

use crate::programs::SourceLanguage;
use crate::{WikiConfig, WikiFormat, WikiOutput, WikiResult};

/// Generate the master wiki index page.
pub fn generate_index_page(
    config: &WikiConfig,
    output: &WikiOutput,
    mermaid: bool,
) -> WikiResult<()> {
    let mut md = String::new();

    md.push_str(&format!("# {}\n\n", config.title));
    md.push_str("Auto-generated documentation for the IBM Mainframe ecosystem.\n\n");

    // Quick stats
    let total_programs = output.programs.len();
    let total_screens = output.screen_docs.len();
    let total_xrefs = output.xrefs.len();
    let total_edges = output.call_edges.len();

    let cobol_count = output
        .programs
        .iter()
        .filter(|p| p.language == SourceLanguage::Cobol)
        .count();
    let jcl_count = output
        .programs
        .iter()
        .filter(|p| p.language == SourceLanguage::Jcl)
        .count();
    let total_lines: usize = output.programs.iter().map(|p| p.lines).sum();

    md.push_str("## Quick Stats\n\n");
    md.push_str("| Metric | Value |\n");
    md.push_str("|--------|-------|\n");
    md.push_str(&format!("| Programs Analyzed | {} |\n", total_programs));
    md.push_str(&format!("| COBOL Programs | {} |\n", cobol_count));
    md.push_str(&format!("| JCL Jobs | {} |\n", jcl_count));
    md.push_str(&format!("| Total Source Lines | {} |\n", total_lines));
    md.push_str(&format!("| BMS Screens | {} |\n", total_screens));
    md.push_str(&format!("| Call Graph Edges | {} |\n", total_edges));
    md.push_str(&format!("| Cross-References | {} |\n", total_xrefs));
    md.push_str(&format!(
        "| Data Dictionary Entries | {} |\n",
        output.data_dict.len()
    ));
    md.push('\n');

    // Architecture diagram
    if mermaid {
        md.push_str("## System Architecture\n\n");
        md.push_str("```mermaid\ngraph TB\n");
        md.push_str("    subgraph \"Languages\"\n");
        md.push_str("        COBOL[COBOL Programs]\n");
        md.push_str("        JCL[JCL Jobs]\n");
        md.push_str("        REXX[REXX Execs]\n");
        md.push_str("        HLASM[HLASM Modules]\n");
        md.push_str("        PLI[PL/I Programs]\n");
        md.push_str("    end\n");
        md.push_str("    subgraph \"Subsystems\"\n");
        md.push_str("        CICS[CICS Transaction Server]\n");
        md.push_str("        JES2[JES2 Job Entry]\n");
        md.push_str("        TSO[TSO/ISPF]\n");
        md.push_str("        MQ[IBM MQ]\n");
        md.push_str("    end\n");
        md.push_str("    subgraph \"Data\"\n");
        md.push_str("        VSAM[VSAM Datasets]\n");
        md.push_str("        DB2[DB2 Database]\n");
        md.push_str("        IMS[IMS Database]\n");
        md.push_str("    end\n");
        md.push_str("    subgraph \"System Services\"\n");
        md.push_str("        MVS[MVS Kernel]\n");
        md.push_str("        RACF[RACF Security]\n");
        md.push_str("        WLM[Workload Manager]\n");
        md.push_str("        SMF[SMF Records]\n");
        md.push_str("    end\n");
        md.push_str("    COBOL --> CICS\n");
        md.push_str("    COBOL --> DB2\n");
        md.push_str("    COBOL --> VSAM\n");
        md.push_str("    JCL --> JES2\n");
        md.push_str("    JCL --> COBOL\n");
        md.push_str("    CICS --> VSAM\n");
        md.push_str("    CICS --> DB2\n");
        md.push_str("    CICS --> MQ\n");
        md.push_str("    MVS --> RACF\n");
        md.push_str("    MVS --> WLM\n");
        md.push_str("    MVS --> SMF\n");
        md.push_str("```\n\n");
    }

    // Navigation hub
    md.push_str("## Wiki Sections\n\n");

    md.push_str("### Languages\n\n");
    md.push_str("Comprehensive reference for all 9 mainframe languages.\n\n");
    md.push_str("- [Language Overview](languages/index.md)\n");
    md.push_str("- [COBOL Reference](languages/cobol/index.md)\n");
    md.push_str("- [JCL Reference](languages/jcl/index.md)\n");
    md.push_str("- [REXX Reference](languages/rexx/index.md)\n");
    md.push_str("- [HLASM Reference](languages/hlasm/index.md)\n");
    md.push_str("- [PL/I Reference](languages/pli/index.md)\n");
    md.push_str("- [CLIST Reference](languages/clist/index.md)\n");
    md.push_str("- [Easytrieve Reference](languages/easytrieve/index.md)\n");
    md.push_str("- [Natural Reference](languages/natural/index.md)\n");
    md.push_str("- [FOCUS Reference](languages/focus/index.md)\n\n");

    // Per-program links
    if !output.programs.is_empty() {
        md.push_str("### Analyzed Programs\n\n");
        md.push_str("| Program | Language | Lines | Source |\n");
        md.push_str("|---------|----------|-------|--------|\n");
        for prog in &output.programs {
            let link = format!(
                "languages/{}/{}.md",
                prog.language.dir_name(),
                prog.name
            );
            md.push_str(&format!(
                "| [{}]({}) | {} | {} | `{}` |\n",
                prog.name,
                link,
                prog.language.label(),
                prog.lines,
                prog.source_path.display()
            ));
        }
        md.push('\n');
    }

    md.push_str("### Data Systems\n\n");
    md.push_str("- [Data Systems Overview](data/index.md)\n");
    md.push_str("- [Dataset Types (VSAM, QSAM, PDS, GDG)](data/datasets.md)\n");
    md.push_str("- [DB2 Reference](data/db2.md)\n");
    md.push_str("- [IMS Reference](data/ims.md)\n");
    md.push_str("- [IDMS Reference](data/idms.md)\n");
    md.push_str("- [ADABAS Reference](data/adabas.md)\n");
    md.push_str("- [Encoding Reference](data/encoding.md)\n");
    md.push_str("- [Data Dictionary](data/datadict.md)\n\n");

    md.push_str("### Subsystems\n\n");
    md.push_str("- [Subsystems Overview](subsystems/index.md)\n");
    md.push_str("- [CICS Transaction Server](subsystems/cics/index.md)\n");
    md.push_str("  - [CICS Commands](subsystems/cics/commands.md)\n");
    md.push_str("  - [CICS Response Codes](subsystems/cics/response-codes.md)\n");
    if !output.screen_docs.is_empty() {
        md.push_str("  - BMS Screens:\n");
        for screen in &output.screen_docs {
            md.push_str(&format!(
                "    - [{}/{}](subsystems/cics/screens/{}_{}.md)\n",
                screen.mapset_name, screen.map_name, screen.mapset_name, screen.map_name
            ));
        }
    }
    md.push_str("- [JES2 Reference](subsystems/jes2.md)\n");
    md.push_str("- [RACF Security](subsystems/racf.md)\n");
    md.push_str("- [TSO Reference](subsystems/tso.md)\n");
    md.push_str("- [ISPF Reference](subsystems/ispf.md)\n");
    md.push_str("- [IBM MQ Reference](subsystems/mq.md)\n");
    md.push_str("- [MVS Services](subsystems/mvs.md)\n");
    md.push_str("- [USS Reference](subsystems/uss.md)\n");
    md.push_str("- [WLM Reference](subsystems/wlm.md)\n");
    md.push_str("- [SMF Reference](subsystems/smf.md)\n");
    md.push_str("- [Networking Reference](subsystems/networking.md)\n\n");

    if config.system_ref {
        md.push_str("### System Services\n\n");
        md.push_str("- [System Services Overview](system/index.md)\n");
        md.push_str("- [PARMLIB Reference](system/parmlib.md)\n");
        md.push_str("- [Utilities Reference](system/utilities.md)\n");
        md.push_str("- [DFSORT Reference](system/sort.md)\n");
        md.push_str("- [Program Management](system/pgmmgmt.md)\n");
        md.push_str("- [Cryptographic Services](system/crypto.md)\n");
        md.push_str("- [System Commands](system/syscmd.md)\n");
        md.push_str("- [DRDA Wire Protocol](system/drda.md)\n\n");

        md.push_str("### z/OSMF REST API\n\n");
        md.push_str("- [API Overview](api/index.md)\n");
        md.push_str("- [Authentication](api/auth.md)\n");
        md.push_str("- [Dataset Endpoints](api/datasets.md)\n");
        md.push_str("- [Job Endpoints](api/jobs.md)\n");
        md.push_str("- [TSO Endpoints](api/tso.md)\n");
        md.push_str("- [Console Endpoints](api/console.md)\n");
        md.push_str("- [USS File Endpoints](api/files.md)\n");
        md.push_str("- [CICS Endpoints](api/cics.md)\n\n");
    }

    md.push_str("### Analysis\n\n");
    md.push_str("- [Call Graph](callgraph.md)\n");
    md.push_str("- [Cross-Reference Index](crossref.md)\n");
    md.push_str("- [Language Environment Reference](runtime.md)\n\n");

    // Generation metadata
    md.push_str("---\n\n");
    md.push_str(&format!(
        "*Generated by Mainframe Code Wiki | Format: {} | Source: `{}`*\n",
        match config.format {
            WikiFormat::Markdown => "Markdown + Mermaid",
            WikiFormat::MarkdownNoMermaid => "Markdown (no Mermaid)",
        },
        config.source_dir.display()
    ));

    fs::write(config.output_dir.join("index.md"), md)?;
    Ok(())
}
