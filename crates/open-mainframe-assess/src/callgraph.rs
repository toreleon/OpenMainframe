//! Call graph analysis for COBOL program dependency mapping.
//!
//! Builds a directed graph of program-to-program calls (CALL, CICS LINK, etc.)
//! and provides topological ordering for migration planning and cycle detection.

use std::collections::{HashMap, HashSet, VecDeque};

/// Type of call edge in the graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize)]
pub enum CallType {
    /// Static CALL 'PROGRAM-NAME'
    StaticCall,
    /// Dynamic CALL variable
    DynamicCall,
    /// EXEC CICS LINK PROGRAM('NAME')
    CicsLink,
    /// EXEC CICS XCTL PROGRAM('NAME')
    CicsXctl,
}

impl CallType {
    /// Human-readable label.
    pub fn label(&self) -> &'static str {
        match self {
            CallType::StaticCall => "CALL",
            CallType::DynamicCall => "CALL (dynamic)",
            CallType::CicsLink => "CICS LINK",
            CallType::CicsXctl => "CICS XCTL",
        }
    }
}

/// An edge in the call graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize)]
pub struct CallEdge {
    /// Caller program name.
    pub caller: String,
    /// Callee program name.
    pub callee: String,
    /// Type of call.
    pub call_type: CallType,
    /// Whether the target is uncertain (e.g., dynamic dispatch).
    pub uncertain: bool,
}

/// Call graph for a set of programs.
#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct CallGraph {
    /// All program nodes.
    programs: HashSet<String>,
    /// Adjacency list: caller -> set of (callee, call_type, uncertain).
    edges: Vec<CallEdge>,
    /// Adjacency list for fast lookup: caller -> list of edge indices.
    adj: HashMap<String, Vec<usize>>,
}

impl CallGraph {
    /// Create an empty call graph.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a program node.
    pub fn add_program(&mut self, name: &str) {
        self.programs.insert(name.to_uppercase());
    }

    /// Add a call edge.
    pub fn add_edge(&mut self, caller: &str, callee: &str, call_type: CallType) {
        let caller_upper = caller.to_uppercase();
        let callee_upper = callee.to_uppercase();
        let uncertain = matches!(call_type, CallType::DynamicCall);

        self.programs.insert(caller_upper.clone());
        self.programs.insert(callee_upper.clone());

        let idx = self.edges.len();
        self.edges.push(CallEdge {
            caller: caller_upper.clone(),
            callee: callee_upper,
            call_type,
            uncertain,
        });
        self.adj.entry(caller_upper).or_default().push(idx);
    }

    /// Get all programs in the graph.
    pub fn programs(&self) -> &HashSet<String> {
        &self.programs
    }

    /// Get all edges.
    pub fn edges(&self) -> &[CallEdge] {
        &self.edges
    }

    /// Get outgoing edges for a program.
    pub fn callees(&self, program: &str) -> Vec<&CallEdge> {
        let key = program.to_uppercase();
        match self.adj.get(&key) {
            Some(indices) => indices.iter().map(|&i| &self.edges[i]).collect(),
            None => Vec::new(),
        }
    }

    /// Detect circular dependencies (cycles) via DFS.
    /// Returns a list of cycles, each cycle is a list of program names.
    pub fn find_cycles(&self) -> Vec<Vec<String>> {
        let mut visited = HashSet::new();
        let mut on_stack = HashSet::new();
        let mut stack = Vec::new();
        let mut cycles = Vec::new();

        for prog in &self.programs {
            if !visited.contains(prog) {
                self.dfs_cycles(
                    prog,
                    &mut visited,
                    &mut on_stack,
                    &mut stack,
                    &mut cycles,
                );
            }
        }
        cycles
    }

    fn dfs_cycles(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        on_stack: &mut HashSet<String>,
        stack: &mut Vec<String>,
        cycles: &mut Vec<Vec<String>>,
    ) {
        visited.insert(node.to_string());
        on_stack.insert(node.to_string());
        stack.push(node.to_string());

        for edge in self.callees(node) {
            if !visited.contains(&edge.callee) {
                self.dfs_cycles(&edge.callee, visited, on_stack, stack, cycles);
            } else if on_stack.contains(&edge.callee) {
                // Found a cycle — extract it from the stack
                let start = stack.iter().position(|n| n == &edge.callee).unwrap();
                let cycle: Vec<String> = stack[start..].to_vec();
                cycles.push(cycle);
            }
        }

        stack.pop();
        on_stack.remove(node);
    }

    /// Topological sort (Kahn's algorithm) for migration ordering.
    /// Leaf programs (no outgoing calls) appear first, entry points last.
    /// Returns `None` if the graph has cycles.
    pub fn topological_sort(&self) -> Option<Vec<String>> {
        // Compute in-degree for each program
        let mut in_degree: HashMap<String, usize> = HashMap::new();
        for prog in &self.programs {
            in_degree.entry(prog.clone()).or_insert(0);
        }
        for edge in &self.edges {
            *in_degree.entry(edge.callee.clone()).or_insert(0) += 1;
        }

        // Start with nodes that have no incoming edges (leaves / called-by-nobody)
        let mut queue: VecDeque<String> = VecDeque::new();
        for (prog, &deg) in &in_degree {
            if deg == 0 {
                queue.push_back(prog.clone());
            }
        }

        let mut order = Vec::new();
        while let Some(node) = queue.pop_front() {
            order.push(node.clone());
            for edge in self.callees(&node) {
                if let Some(deg) = in_degree.get_mut(&edge.callee) {
                    *deg -= 1;
                    if *deg == 0 {
                        queue.push_back(edge.callee.clone());
                    }
                }
            }
        }

        if order.len() == self.programs.len() {
            // Reverse so leaves come first, callers come last
            // Actually, Kahn's already gives us sources first. We want leaves first.
            // "Leaves" = programs that call nobody and are only called.
            // We want migration order: migrate callees before callers.
            // Kahn's gives: programs with no in-edges first (the callers that nobody calls).
            // We need the reverse: programs with no out-edges first.
            // Let's reverse the edge direction for topological sort.
            // Actually, let's just reverse the result.
            order.reverse();
            Some(order)
        } else {
            None // cycle detected
        }
    }

    /// Extract call information from COBOL source text.
    /// This is a simple regex-free text scanner for CALL and EXEC CICS patterns.
    pub fn extract_from_source(program_name: &str, source: &str) -> Vec<CallEdge> {
        let mut edges = Vec::new();
        let upper = source.to_uppercase();

        for line in upper.lines() {
            let trimmed = line.trim();

            // Skip comments (column 7 = '*' in fixed-format COBOL)
            if line.len() > 6 && line.as_bytes().get(6) == Some(&b'*') {
                continue;
            }

            // CALL 'PROGRAM-NAME'
            if let Some(pos) = trimmed.find("CALL ") {
                let after = &trimmed[pos + 5..];
                let after_trimmed = after.trim_start();
                if after_trimmed.starts_with('\'') || after_trimmed.starts_with('"') {
                    let quote = after_trimmed.as_bytes()[0];
                    if let Some(end) = after_trimmed[1..].find(quote as char) {
                        let target = &after_trimmed[1..1 + end];
                        edges.push(CallEdge {
                            caller: program_name.to_uppercase(),
                            callee: target.to_string(),
                            call_type: CallType::StaticCall,
                            uncertain: false,
                        });
                    }
                } else {
                    // Dynamic call — variable name
                    let target = after_trimmed
                        .split_whitespace()
                        .next()
                        .unwrap_or("UNKNOWN");
                    edges.push(CallEdge {
                        caller: program_name.to_uppercase(),
                        callee: target.to_string(),
                        call_type: CallType::DynamicCall,
                        uncertain: true,
                    });
                }
            }

            // EXEC CICS LINK PROGRAM('NAME')
            if trimmed.contains("EXEC CICS") && trimmed.contains("LINK") {
                if let Some(target) = extract_program_option(trimmed) {
                    edges.push(CallEdge {
                        caller: program_name.to_uppercase(),
                        callee: target,
                        call_type: CallType::CicsLink,
                        uncertain: false,
                    });
                }
            }

            // EXEC CICS XCTL PROGRAM('NAME')
            if trimmed.contains("EXEC CICS") && trimmed.contains("XCTL") {
                if let Some(target) = extract_program_option(trimmed) {
                    edges.push(CallEdge {
                        caller: program_name.to_uppercase(),
                        callee: target,
                        call_type: CallType::CicsXctl,
                        uncertain: false,
                    });
                }
            }
        }

        edges
    }
}

/// Extract PROGRAM('NAME') from an EXEC CICS statement.
fn extract_program_option(line: &str) -> Option<String> {
    let upper = line.to_uppercase();
    let pos = upper.find("PROGRAM(")?;
    let after = &upper[pos + 8..];
    let after_trimmed = after.trim_start();
    if after_trimmed.starts_with('\'') || after_trimmed.starts_with('"') {
        let quote = after_trimmed.as_bytes()[0];
        let end = after_trimmed[1..].find(quote as char)?;
        Some(after_trimmed[1..1 + end].to_string())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_call_graph_edges() {
        let mut graph = CallGraph::new();
        graph.add_edge("A", "B", CallType::StaticCall);
        graph.add_edge("A", "C", CallType::CicsLink);
        graph.add_edge("B", "D", CallType::StaticCall);

        assert_eq!(graph.programs().len(), 4);
        assert_eq!(graph.edges().len(), 3);
        assert_eq!(graph.callees("A").len(), 2);
        assert_eq!(graph.callees("B").len(), 1);
        assert_eq!(graph.callees("D").len(), 0);
    }

    #[test]
    fn test_topological_sort_no_cycles() {
        let mut graph = CallGraph::new();
        graph.add_edge("A", "B", CallType::StaticCall);
        graph.add_edge("A", "C", CallType::StaticCall);
        graph.add_edge("B", "D", CallType::StaticCall);

        let order = graph.topological_sort().unwrap();
        // Leaves first: D, then B and C (either order), then A
        let pos_a = order.iter().position(|n| n == "A").unwrap();
        let pos_b = order.iter().position(|n| n == "B").unwrap();
        let pos_d = order.iter().position(|n| n == "D").unwrap();
        assert!(pos_d < pos_b, "D should come before B");
        assert!(pos_b < pos_a, "B should come before A");
    }

    #[test]
    fn test_cycle_detection() {
        let mut graph = CallGraph::new();
        graph.add_edge("A", "B", CallType::StaticCall);
        graph.add_edge("B", "C", CallType::StaticCall);
        graph.add_edge("C", "A", CallType::StaticCall);

        let cycles = graph.find_cycles();
        assert!(!cycles.is_empty(), "Should detect at least one cycle");
        // The cycle should contain A, B, C
        let cycle = &cycles[0];
        assert!(cycle.contains(&"A".to_string()));
        assert!(cycle.contains(&"B".to_string()));
        assert!(cycle.contains(&"C".to_string()));
    }

    #[test]
    fn test_topological_sort_with_cycle_returns_none() {
        let mut graph = CallGraph::new();
        graph.add_edge("A", "B", CallType::StaticCall);
        graph.add_edge("B", "A", CallType::StaticCall);

        assert!(graph.topological_sort().is_none());
    }

    #[test]
    fn test_extract_static_call() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       PROCEDURE DIVISION.
           CALL 'SUBPROG' USING WS-DATA.
           CALL 'UTILITY'.
           STOP RUN.
"#;
        let edges = CallGraph::extract_from_source("MAINPROG", source);
        assert_eq!(edges.len(), 2);
        assert_eq!(edges[0].callee, "SUBPROG");
        assert_eq!(edges[0].call_type, CallType::StaticCall);
        assert!(!edges[0].uncertain);
        assert_eq!(edges[1].callee, "UTILITY");
    }

    #[test]
    fn test_extract_dynamic_call() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       PROCEDURE DIVISION.
           CALL WS-PROG-NAME USING WS-DATA.
"#;
        let edges = CallGraph::extract_from_source("MAINPROG", source);
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].call_type, CallType::DynamicCall);
        assert!(edges[0].uncertain);
    }

    #[test]
    fn test_extract_cics_link() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLER.
       PROCEDURE DIVISION.
           EXEC CICS LINK PROGRAM('SUBPROG') END-EXEC.
"#;
        let edges = CallGraph::extract_from_source("CALLER", source);
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].callee, "SUBPROG");
        assert_eq!(edges[0].call_type, CallType::CicsLink);
    }

    #[test]
    fn test_extract_cics_xctl() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLER.
       PROCEDURE DIVISION.
           EXEC CICS XCTL PROGRAM('TARGET') END-EXEC.
"#;
        let edges = CallGraph::extract_from_source("CALLER", source);
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].callee, "TARGET");
        assert_eq!(edges[0].call_type, CallType::CicsXctl);
    }

    #[test]
    fn test_call_type_labels() {
        assert_eq!(CallType::StaticCall.label(), "CALL");
        assert_eq!(CallType::DynamicCall.label(), "CALL (dynamic)");
        assert_eq!(CallType::CicsLink.label(), "CICS LINK");
        assert_eq!(CallType::CicsXctl.label(), "CICS XCTL");
    }

    #[test]
    fn test_graph_single_node() {
        let mut graph = CallGraph::new();
        graph.add_program("LONER");
        let order = graph.topological_sort().unwrap();
        assert_eq!(order, vec!["LONER"]);
        assert!(graph.find_cycles().is_empty());
    }
}
