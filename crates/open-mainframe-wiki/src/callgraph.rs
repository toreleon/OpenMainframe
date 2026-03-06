//! Cross-language call graph generator (Mermaid).

use std::collections::HashMap;
use std::fs;

use open_mainframe_assess::{CallEdge, CallGraph, CallType};

use crate::{WikiConfig, WikiResult};

/// Generate the call graph page.
pub fn generate_callgraph_page(
    config: &WikiConfig,
    edges: &[CallEdge],
    mermaid: bool,
) -> WikiResult<()> {
    // Build a CallGraph from the edges for analysis
    let mut graph = CallGraph::new();
    for edge in edges {
        graph.add_edge(&edge.caller, &edge.callee, edge.call_type.clone());
    }

    let cycles = graph.find_cycles();
    let topo_order = graph.topological_sort();

    let total_programs = graph.programs().len();
    let total_edges = edges.len();

    let mut md = String::from("# Call Graph\n\n");
    md.push_str("Cross-language program dependency graph.\n\n");

    // Statistics
    md.push_str("## Statistics\n\n");
    md.push_str("| Metric | Value |\n");
    md.push_str("|----|----|\n");
    md.push_str(&format!("| Total Programs | {} |\n", total_programs));
    md.push_str(&format!("| Total Edges | {} |\n", total_edges));
    md.push_str(&format!("| Cycles Found | {} |\n", cycles.len()));
    md.push('\n');

    if edges.is_empty() {
        md.push_str("*No call relationships detected.*\n");
    } else if mermaid {
        // Group edges by call type for subgraphs
        let mut by_type: HashMap<&str, Vec<&CallEdge>> = HashMap::new();
        for edge in edges {
            by_type.entry(edge.call_type.label()).or_default().push(edge);
        }

        md.push_str("## Dependency Diagram\n\n");
        md.push_str("```mermaid\ngraph LR\n");

        // Node styles: rectangles for programs, rounded for transaction-related
        for prog in graph.programs() {
            let is_transactional = edges.iter().any(|e| {
                (e.caller == *prog || e.callee == *prog)
                    && matches!(e.call_type, CallType::CicsLink | CallType::CicsXctl)
            });
            if is_transactional {
                md.push_str(&format!("    {}({})\n", prog, prog));
            } else {
                md.push_str(&format!("    {}[{}]\n", prog, prog));
            }
        }
        md.push('\n');

        // Subgraphs by call type
        for (label, type_edges) in &by_type {
            md.push_str(&format!("    subgraph \"{}\"\n", label));
            for edge in type_edges {
                let style = match edge.call_type {
                    CallType::CicsLink => "-.->",
                    CallType::CicsXctl => "==>",
                    CallType::DynamicCall => "-. dynamic .->",
                    CallType::StaticCall => "-->",
                };
                md.push_str(&format!(
                    "        {} {} {}\n",
                    edge.caller, style, edge.callee
                ));
            }
            md.push_str("    end\n");
        }

        // Style classes
        md.push_str("\n    classDef transactional fill:#f9f,stroke:#333,stroke-width:2px\n");
        md.push_str("    classDef standard fill:#bbf,stroke:#333,stroke-width:1px\n");

        md.push_str("```\n\n");

        // Legend
        md.push_str("### Legend\n\n");
        md.push_str("| Arrow Style | Meaning |\n");
        md.push_str("|---|---|\n");
        md.push_str("| `-->` | Static CALL |\n");
        md.push_str("| `-. dynamic .->` | Dynamic CALL (variable target) |\n");
        md.push_str("| `-.->` | CICS LINK |\n");
        md.push_str("| `==>` | CICS XCTL (transfer control) |\n");
        md.push_str("| `[Name]` | Standard program |\n");
        md.push_str("| `(Name)` | CICS transactional program |\n");
        md.push('\n');
    }

    // Edge list table
    md.push_str("## Edge List\n\n");
    md.push_str("| Caller | Callee | Type |\n");
    md.push_str("|--------|--------|------|\n");
    for edge in edges {
        md.push_str(&format!(
            "| {} | {} | {} |\n",
            edge.caller,
            edge.callee,
            edge.call_type.label()
        ));
    }
    md.push('\n');

    // Cycle detection results
    md.push_str("## Cycle Analysis\n\n");
    if cycles.is_empty() {
        md.push_str("No circular dependencies detected.\n\n");
    } else {
        md.push_str(&format!(
            "**Warning:** {} circular dependency chain(s) detected:\n\n",
            cycles.len()
        ));
        for (i, cycle) in cycles.iter().enumerate() {
            let chain = cycle.join(" -> ");
            md.push_str(&format!(
                "{}. `{} -> {}`\n",
                i + 1,
                chain,
                cycle.first().unwrap_or(&String::new())
            ));
        }
        md.push('\n');
    }

    // Topological order
    md.push_str("## Dependency Order (Topological Sort)\n\n");
    md.push_str("Programs listed from leaf (no outgoing calls) to entry point.\n\n");
    match topo_order {
        Some(order) => {
            for (i, prog) in order.iter().enumerate() {
                md.push_str(&format!("{}. {}\n", i + 1, prog));
            }
        }
        None => {
            md.push_str(
                "*Topological ordering not available due to circular dependencies.*\n",
            );
        }
    }

    md.push_str("\n---\n*Generated by Mainframe Code Wiki*\n");
    fs::write(config.output_dir.join("callgraph.md"), md)?;
    Ok(())
}
