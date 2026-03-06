//! BMS screen map documentation generator.

use std::fs;
use std::path::Path;

use crate::{WikiConfig, WikiResult};

/// Documentation for a single BMS screen.
#[derive(Debug, Clone)]
pub struct ScreenDoc {
    pub map_name: String,
    pub mapset_name: String,
    pub source_file: String,
    pub fields: Vec<ScreenField>,
}

/// A field within a BMS screen.
#[derive(Debug, Clone)]
pub struct ScreenField {
    pub name: String,
    pub row: usize,
    pub col: usize,
    pub length: usize,
    pub attr: String,
}

/// Parse BMS source files and generate screen documentation.
pub fn generate_screens(bms_dir: &Path) -> WikiResult<Vec<ScreenDoc>> {
    let mut docs = Vec::new();
    if !bms_dir.exists() {
        return Ok(docs);
    }

    if let Ok(entries) = fs::read_dir(bms_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(ext) = path.extension() {
                if ext.to_string_lossy().to_lowercase() == "bms" {
                    if let Ok(source) = fs::read_to_string(&path) {
                        let file_name = path
                            .file_stem()
                            .map(|s| s.to_string_lossy().to_string())
                            .unwrap_or_default();
                        docs.extend(parse_bms_source(&source, &file_name));
                    }
                }
            }
        }
    }
    Ok(docs)
}

/// Join BMS continuation lines. BMS macros use a comma at the end of a line
/// (before the continuation column) followed by a continuation line that starts
/// in column 16+ (or any non-label column). We join them so that POS, LENGTH,
/// ATTRB etc. that span lines are visible on a single logical line.
fn join_continuation_lines(source: &str) -> Vec<String> {
    let mut logical_lines: Vec<String> = Vec::new();
    for raw in source.lines() {
        let line = raw.to_string();
        // A continuation line typically has leading whitespace (no label in col 1)
        // and the previous logical line ended with a comma.
        let is_continuation = line.starts_with(' ') && !line.trim().is_empty();
        let prev_continues = logical_lines
            .last()
            .map(|l| {
                let t = l.trim_end();
                t.ends_with(',') || t.ends_with('X')
            })
            .unwrap_or(false);

        if is_continuation && prev_continues && !logical_lines.is_empty() {
            let prev = logical_lines.last_mut().unwrap();
            // Strip trailing continuation marker if 'X'
            let trimmed_prev = prev.trim_end();
            if trimmed_prev.ends_with('X') {
                let len = trimmed_prev.len();
                prev.truncate(len - 1);
            }
            prev.push(' ');
            prev.push_str(line.trim());
        } else {
            logical_lines.push(line);
        }
    }
    logical_lines
}

fn parse_bms_source(source: &str, file_name: &str) -> Vec<ScreenDoc> {
    let mut docs = Vec::new();
    let mut current_mapset = String::new();
    let mut current_map = String::new();
    let mut current_fields: Vec<ScreenField> = Vec::new();

    let logical_lines = join_continuation_lines(source);

    for line in &logical_lines {
        let trimmed = line.trim();
        if trimmed.contains("DFHMSD") && trimmed.contains("TYPE=MAP") {
            if let Some(name) = trimmed.split_whitespace().next() {
                current_mapset = name.to_string();
            }
        } else if trimmed.contains("DFHMDI") {
            // Save previous map if any
            if !current_map.is_empty() {
                docs.push(ScreenDoc {
                    map_name: current_map.clone(),
                    mapset_name: current_mapset.clone(),
                    source_file: file_name.to_string(),
                    fields: current_fields.clone(),
                });
                current_fields.clear();
            }
            if let Some(name) = trimmed.split_whitespace().next() {
                current_map = name.to_string();
            }
        } else if trimmed.contains("DFHMDF") {
            if let Some(name) = trimmed.split_whitespace().next() {
                if name != "DFHMDF" {
                    let (row, col) = parse_pos(trimmed);
                    let length = parse_length(trimmed);
                    let attr = parse_attrb(trimmed);
                    current_fields.push(ScreenField {
                        name: name.to_string(),
                        row,
                        col,
                        length,
                        attr,
                    });
                }
            }
        }
    }

    // Save last map
    if !current_map.is_empty() {
        docs.push(ScreenDoc {
            map_name: current_map,
            mapset_name: current_mapset,
            source_file: file_name.to_string(),
            fields: current_fields,
        });
    }

    docs
}

/// Parse POS=(row,col) from a DFHMDF line.
fn parse_pos(line: &str) -> (usize, usize) {
    let upper = line.to_uppercase();
    if let Some(pos_start) = upper.find("POS=(") {
        let after = &upper[pos_start + 5..];
        if let Some(end) = after.find(')') {
            let inner = &after[..end];
            let parts: Vec<&str> = inner.split(',').collect();
            if parts.len() == 2 {
                let row = parts[0].trim().parse::<usize>().unwrap_or(0);
                let col = parts[1].trim().parse::<usize>().unwrap_or(0);
                return (row, col);
            }
        }
    }
    (0, 0)
}

/// Parse LENGTH=n from a DFHMDF line.
fn parse_length(line: &str) -> usize {
    let upper = line.to_uppercase();
    if let Some(pos) = upper.find("LENGTH=") {
        let after = &upper[pos + 7..];
        let num_str: String = after.chars().take_while(|c| c.is_ascii_digit()).collect();
        return num_str.parse::<usize>().unwrap_or(0);
    }
    0
}

/// Parse ATTRB=(options) from a DFHMDF line.
fn parse_attrb(line: &str) -> String {
    let upper = line.to_uppercase();
    if let Some(pos) = upper.find("ATTRB=(") {
        let after = &upper[pos + 7..];
        if let Some(end) = after.find(')') {
            return after[..end].to_string();
        }
    }
    // Also handle ATTRB=SINGLE_VALUE (no parens)
    if let Some(pos) = upper.find("ATTRB=") {
        let after = &upper[pos + 6..];
        if !after.starts_with('(') {
            let val: String = after
                .chars()
                .take_while(|c| *c != ',' && *c != ' ')
                .collect();
            if !val.is_empty() {
                return val;
            }
        }
    }
    String::new()
}

/// Build an ASCII grid (24x80) representation of a BMS screen.
fn build_ascii_grid(screen: &ScreenDoc) -> String {
    let rows = 24;
    let cols = 80;
    let mut grid = vec![vec![b'.'; cols]; rows];

    for field in &screen.fields {
        if field.row == 0 || field.row > rows || field.col == 0 || field.col > cols {
            continue;
        }
        let r = field.row - 1;
        let c = field.col - 1;
        let len = if field.length > 0 {
            field.length
        } else {
            field.name.len().min(8)
        };
        // Place attribute byte marker
        if c < cols {
            grid[r][c] = b'%';
        }
        // Place field characters
        for i in 0..len {
            let fc = c + 1 + i;
            if fc < cols {
                grid[r][fc] = b'_';
            }
        }
    }

    let mut output = String::new();
    output.push_str("```\n");
    // Column ruler
    output.push_str("     ");
    for c in 0..cols {
        if c % 10 == 0 {
            output.push_str(&format!("{:<10}", c + 1));
        }
    }
    output.push('\n');
    output.push_str("     ");
    for _ in 0..cols {
        output.push('-');
    }
    output.push('\n');

    for (r, row) in grid.iter().enumerate() {
        output.push_str(&format!("{:>3}: ", r + 1));
        for &ch in row {
            output.push(ch as char);
        }
        output.push('\n');
    }
    output.push_str("```\n");
    output.push_str("\n**Key:** `%` = attribute byte, `_` = input/output field, `.` = empty\n");
    output
}

/// Write screen documentation pages.
pub fn write_screen_pages(config: &WikiConfig, screens: &[ScreenDoc]) -> WikiResult<()> {
    let dir = config.output_dir.join("subsystems/cics/screens");
    fs::create_dir_all(&dir)?;

    // Build a list of all screen filenames for navigation links
    let screen_refs: Vec<(String, String)> = screens
        .iter()
        .map(|s| {
            (
                format!("{}_{}", s.mapset_name, s.map_name),
                format!("{}_{}.md", s.mapset_name, s.map_name),
            )
        })
        .collect();

    for (i, screen) in screens.iter().enumerate() {
        let filename = format!("{}_{}.md", screen.mapset_name, screen.map_name);
        let path = dir.join(&filename);
        let content = render_screen_page(screen, i, &screen_refs);
        fs::write(&path, content)?;
    }
    Ok(())
}

fn render_screen_page(
    screen: &ScreenDoc,
    index: usize,
    all_screens: &[(String, String)],
) -> String {
    let mut md = String::new();

    // Navigation links
    let prev = if index > 0 {
        Some(&all_screens[index - 1])
    } else {
        None
    };
    let next = if index + 1 < all_screens.len() {
        Some(&all_screens[index + 1])
    } else {
        None
    };
    let mut nav = String::new();
    if let Some((label, file)) = prev {
        nav.push_str(&format!("[<< {}]({})", label, file));
    }
    nav.push_str(" | [Screen Index](../screens.md) | ");
    if let Some((label, file)) = next {
        nav.push_str(&format!("[{} >>]({})", label, file));
    }
    md.push_str(&nav);
    md.push_str("\n\n");

    md.push_str(&format!(
        "# BMS Screen: {} / {}\n\n",
        screen.mapset_name, screen.map_name
    ));
    md.push_str(&format!("**Source:** `{}`\n\n", screen.source_file));

    // ASCII grid layout
    md.push_str("## Screen Layout (24x80)\n\n");
    md.push_str(&build_ascii_grid(screen));
    md.push('\n');

    if screen.fields.is_empty() {
        md.push_str("*No named fields detected.*\n");
    } else {
        md.push_str("## Fields\n\n");
        md.push_str("| Name | Row | Col | Length | Attributes |\n");
        md.push_str("|------|-----|-----|--------|------------|\n");
        for f in &screen.fields {
            md.push_str(&format!(
                "| {} | {} | {} | {} | {} |\n",
                f.name, f.row, f.col, f.length, f.attr
            ));
        }

        // Attribute summary
        let prot_count = screen
            .fields
            .iter()
            .filter(|f| f.attr.contains("PROT") || f.attr.contains("ASKIP"))
            .count();
        let unprot_count = screen
            .fields
            .iter()
            .filter(|f| f.attr.contains("UNPROT"))
            .count();
        let num_count = screen
            .fields
            .iter()
            .filter(|f| f.attr.contains("NUM"))
            .count();
        let brt_count = screen
            .fields
            .iter()
            .filter(|f| f.attr.contains("BRT"))
            .count();

        md.push_str("\n### Field Attribute Summary\n\n");
        md.push_str(&format!("- **Protected/ASKIP fields:** {}\n", prot_count));
        md.push_str(&format!("- **Unprotected (input) fields:** {}\n", unprot_count));
        md.push_str(&format!("- **Numeric fields:** {}\n", num_count));
        md.push_str(&format!("- **Bright fields:** {}\n", brt_count));
        md.push_str(&format!("- **Total named fields:** {}\n", screen.fields.len()));
    }

    md.push_str("\n---\n*Generated by Mainframe Code Wiki*\n");
    md
}
