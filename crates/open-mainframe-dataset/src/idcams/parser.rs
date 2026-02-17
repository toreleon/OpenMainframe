//! IDCAMS command parser.
//!
//! Parses IDCAMS control statements with continuation support.

use crate::error::DatasetError;
use crate::idcams::commands::IdcamsCommand;
use crate::vsam::VsamType;

/// Parse IDCAMS commands from input text.
pub fn parse_commands(input: &str) -> Result<Vec<IdcamsCommand>, DatasetError> {
    let mut commands = Vec::new();
    let mut current_line = String::new();

    for line in input.lines() {
        let trimmed = line.trim();

        // Skip comments and empty lines
        if trimmed.is_empty() || trimmed.starts_with("/*") || trimmed.starts_with("*") {
            continue;
        }

        // Handle continuation (line ending with -)
        if let Some(without_dash) = trimmed.strip_suffix('-') {
            current_line.push_str(without_dash);
            current_line.push(' ');
            continue;
        }

        current_line.push_str(trimmed);

        // Parse the complete command
        if !current_line.is_empty() {
            if let Some(cmd) = parse_single_command(&current_line)? {
                commands.push(cmd);
            }
            current_line.clear();
        }
    }

    // Handle any remaining content
    if !current_line.is_empty() {
        if let Some(cmd) = parse_single_command(&current_line)? {
            commands.push(cmd);
        }
    }

    Ok(commands)
}

/// Parse a single IDCAMS command.
fn parse_single_command(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let input = input.trim().to_uppercase();

    if input.starts_with("DEFINE") {
        return parse_define(&input);
    }
    if input.starts_with("DELETE") {
        return parse_delete(&input);
    }
    if input.starts_with("ALTER") {
        return parse_alter(&input);
    }
    if input.starts_with("LISTCAT") {
        return parse_listcat(&input);
    }
    if input.starts_with("PRINT") {
        return parse_print(&input);
    }
    if input.starts_with("REPRO") {
        return parse_repro(&input);
    }
    if input.starts_with("VERIFY") {
        return parse_verify(&input);
    }

    // Unknown command - skip
    Ok(None)
}

/// Parse DEFINE command.
fn parse_define(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    // Check specific types first to avoid substring false matches.
    // "DEFINE ALTERNATEINDEX" must be checked before "CLUSTER" because
    // RELATE(MY.CLUSTER) contains "CLUSTER" as a substring.
    // "DEFINE PATH" must be checked before "AIX" because
    // PATHENTRY(MY.AIX) contains "AIX" as a substring.
    if input.contains("ALTERNATEINDEX") || input.starts_with("DEFINE AIX") {
        return parse_define_aix(input);
    }
    if input.starts_with("DEFINE PATH") {
        return parse_define_path(input);
    }
    if input.contains("CLUSTER") {
        return parse_define_cluster(input);
    }
    if input.contains("GDG") {
        return parse_define_gdg(input);
    }

    Err(DatasetError::InvalidParameter(format!(
        "Unknown DEFINE type: {}",
        input
    )))
}

/// Parse DEFINE CLUSTER command.
fn parse_define_cluster(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let name = extract_param(input, "NAME")
        .ok_or_else(|| DatasetError::InvalidParameter("DEFINE CLUSTER requires NAME".to_string()))?;

    let cluster_type = if input.contains("NUMBERED") {
        VsamType::Rrds
    } else if input.contains("NONINDEXED") {
        VsamType::Esds
    } else {
        VsamType::Ksds
    };

    let keys = extract_param(input, "KEYS").and_then(|s| parse_keys(&s));
    let recordsize = extract_param(input, "RECORDSIZE").and_then(|s| parse_recordsize(&s));
    let volumes = extract_param(input, "VOLUMES").map(|s| vec![s]);

    Ok(Some(IdcamsCommand::DefineCluster {
        name,
        cluster_type,
        keys,
        recordsize,
        volumes,
    }))
}

/// Parse DEFINE GDG command.
fn parse_define_gdg(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let name = extract_param(input, "NAME")
        .ok_or_else(|| DatasetError::InvalidParameter("DEFINE GDG requires NAME".to_string()))?;

    let limit = extract_param(input, "LIMIT")
        .and_then(|s| s.parse().ok())
        .unwrap_or(255);

    let scratch = !input.contains("NOSCRATCH");
    let empty = input.contains("EMPTY") || !input.contains("NOEMPTY");

    Ok(Some(IdcamsCommand::DefineGdg {
        name,
        limit,
        scratch,
        empty,
    }))
}

/// Parse DEFINE ALTERNATEINDEX command.
fn parse_define_aix(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let name = extract_param(input, "NAME")
        .ok_or_else(|| DatasetError::InvalidParameter("DEFINE ALTERNATEINDEX requires NAME".to_string()))?;

    let relate = extract_param(input, "RELATE")
        .ok_or_else(|| DatasetError::InvalidParameter("DEFINE ALTERNATEINDEX requires RELATE".to_string()))?;

    let keys = extract_param(input, "KEYS")
        .and_then(|s| parse_keys(&s))
        .ok_or_else(|| DatasetError::InvalidParameter("DEFINE ALTERNATEINDEX requires KEYS".to_string()))?;

    let unique_key = input.contains("UNIQUEKEY") || input.contains("UNIQUE");

    Ok(Some(IdcamsCommand::DefineAix {
        name,
        relate,
        keys,
        unique_key,
    }))
}

/// Parse DEFINE PATH command.
fn parse_define_path(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let name = extract_param(input, "NAME")
        .ok_or_else(|| DatasetError::InvalidParameter("DEFINE PATH requires NAME".to_string()))?;

    let pathentry = extract_param(input, "PATHENTRY")
        .ok_or_else(|| DatasetError::InvalidParameter("DEFINE PATH requires PATHENTRY".to_string()))?;

    Ok(Some(IdcamsCommand::DefinePath { name, pathentry }))
}

/// Parse DELETE command.
fn parse_delete(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    // Extract dataset name (first token after DELETE)
    let tokens: Vec<&str> = input.split_whitespace().collect();
    let name = if tokens.len() > 1 {
        // Check if it's in parentheses or plain
        let second = tokens[1];
        if let Some(paren_name) = extract_param(input, "DELETE") {
            paren_name
        } else {
            second.trim_matches(|c| c == '(' || c == ')').to_string()
        }
    } else {
        return Err(DatasetError::InvalidParameter(
            "DELETE requires dataset name".to_string(),
        ));
    };

    let purge = input.contains("PURGE");
    let force = input.contains("NOERASE") || input.contains("FORCE");

    Ok(Some(IdcamsCommand::Delete { name, purge, force }))
}

/// Parse ALTER command.
fn parse_alter(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let tokens: Vec<&str> = input.split_whitespace().collect();
    let name = if tokens.len() > 1 {
        tokens[1].trim_matches(|c| c == '(' || c == ')').to_string()
    } else {
        return Err(DatasetError::InvalidParameter(
            "ALTER requires dataset name".to_string(),
        ));
    };

    let newname = extract_param(input, "NEWNAME")
        .ok_or_else(|| DatasetError::InvalidParameter("ALTER requires NEWNAME".to_string()))?;

    Ok(Some(IdcamsCommand::Alter { name, newname }))
}

/// Parse LISTCAT command.
fn parse_listcat(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let entry = extract_param(input, "ENT")
        .or_else(|| extract_param(input, "ENTRIES"))
        .or_else(|| extract_param(input, "ENTRY"));

    let level = extract_param(input, "LVL")
        .or_else(|| extract_param(input, "LEVEL"));

    let all = input.contains(" ALL");

    Ok(Some(IdcamsCommand::Listcat { entry, level, all }))
}

/// Parse PRINT command.
fn parse_print(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let dataset = extract_param(input, "INDATASET")
        .or_else(|| extract_param(input, "IDS"))
        .ok_or_else(|| DatasetError::InvalidParameter("PRINT requires INDATASET".to_string()))?;

    let character = input.contains("CHARACTER") || input.contains("CHAR");
    let hex = input.contains(" HEX");

    let skip = extract_param(input, "SKIP")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    let count = extract_param(input, "COUNT")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    Ok(Some(IdcamsCommand::Print {
        dataset,
        character,
        hex,
        skip,
        count,
    }))
}

/// Parse REPRO command.
fn parse_repro(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let indataset = extract_param(input, "INDATASET")
        .or_else(|| extract_param(input, "IDS"))
        .ok_or_else(|| DatasetError::InvalidParameter("REPRO requires INDATASET".to_string()))?;

    let outdataset = extract_param(input, "OUTDATASET")
        .or_else(|| extract_param(input, "ODS"))
        .ok_or_else(|| DatasetError::InvalidParameter("REPRO requires OUTDATASET".to_string()))?;

    let fromkey = extract_param(input, "FROMKEY");
    let tokey = extract_param(input, "TOKEY");

    let skip = extract_param(input, "SKIP")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    let count = extract_param(input, "COUNT")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    Ok(Some(IdcamsCommand::Repro {
        indataset,
        outdataset,
        fromkey,
        tokey,
        skip,
        count,
    }))
}

/// Parse VERIFY command.
fn parse_verify(input: &str) -> Result<Option<IdcamsCommand>, DatasetError> {
    let dataset = extract_param(input, "DATASET")
        .or_else(|| extract_param(input, "DS"))
        .or_else(|| {
            // Try getting the first token after VERIFY
            let tokens: Vec<&str> = input.split_whitespace().collect();
            if tokens.len() > 1 {
                Some(tokens[1].trim_matches(|c| c == '(' || c == ')').to_string())
            } else {
                None
            }
        })
        .ok_or_else(|| DatasetError::InvalidParameter("VERIFY requires DATASET".to_string()))?;

    Ok(Some(IdcamsCommand::Verify { dataset }))
}

/// Extract a parameter value from IDCAMS syntax.
/// Handles: NAME(VALUE), NAME(VALUE1 VALUE2), etc.
fn extract_param(input: &str, param: &str) -> Option<String> {
    let search = format!("{}(", param);
    if let Some(start) = input.find(&search) {
        let after_paren = start + search.len();
        let rest = &input[after_paren..];

        // Find matching closing paren
        let mut depth = 1;
        let mut end_pos = 0;
        for (i, c) in rest.char_indices() {
            match c {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        end_pos = i;
                        break;
                    }
                }
                _ => {}
            }
        }

        if end_pos > 0 {
            return Some(rest[..end_pos].trim().to_string());
        }
    }
    None
}

/// Parse KEYS(length offset) parameter.
fn parse_keys(s: &str) -> Option<(u16, u16)> {
    let parts: Vec<&str> = s.split_whitespace().collect();
    if parts.len() >= 2 {
        let len: u16 = parts[0].parse().ok()?;
        let off: u16 = parts[1].parse().ok()?;
        Some((len, off))
    } else if parts.len() == 1 {
        let len: u16 = parts[0].parse().ok()?;
        Some((len, 0))
    } else {
        None
    }
}

/// Parse RECORDSIZE(avg max) parameter.
fn parse_recordsize(s: &str) -> Option<(u32, u32)> {
    let parts: Vec<&str> = s.split_whitespace().collect();
    if parts.len() >= 2 {
        let avg: u32 = parts[0].parse().ok()?;
        let max: u32 = parts[1].parse().ok()?;
        Some((avg, max))
    } else if parts.len() == 1 {
        let size: u32 = parts[0].parse().ok()?;
        Some((size, size))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_define_cluster() {
        let input = "DEFINE CLUSTER (NAME(MY.CLUSTER) KEYS(10 0) RECORDSIZE(100 200))";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::DefineCluster {
                name,
                cluster_type,
                keys,
                recordsize,
                ..
            } => {
                assert_eq!(name, "MY.CLUSTER");
                assert_eq!(*cluster_type, VsamType::Ksds);
                assert_eq!(*keys, Some((10, 0)));
                assert_eq!(*recordsize, Some((100, 200)));
            }
            _ => panic!("Expected DefineCluster"),
        }
    }

    #[test]
    fn test_parse_define_cluster_esds() {
        let input = "DEFINE CLUSTER (NAME(MY.ESDS) NONINDEXED)";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::DefineCluster { cluster_type, .. } => {
                assert_eq!(*cluster_type, VsamType::Esds);
            }
            _ => panic!("Expected DefineCluster"),
        }
    }

    #[test]
    fn test_parse_define_gdg() {
        let input = "DEFINE GDG (NAME(MY.GDG.BASE) LIMIT(10) SCRATCH)";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::DefineGdg {
                name,
                limit,
                scratch,
                ..
            } => {
                assert_eq!(name, "MY.GDG.BASE");
                assert_eq!(*limit, 10);
                assert!(*scratch);
            }
            _ => panic!("Expected DefineGdg"),
        }
    }

    #[test]
    fn test_parse_delete() {
        let input = "DELETE MY.DATASET PURGE";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::Delete { name, purge, .. } => {
                assert_eq!(name, "MY.DATASET");
                assert!(*purge);
            }
            _ => panic!("Expected Delete"),
        }
    }

    #[test]
    fn test_parse_listcat() {
        let input = "LISTCAT ENT(MY.DATASET) ALL";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::Listcat { entry, all, .. } => {
                assert_eq!(entry.as_deref(), Some("MY.DATASET"));
                assert!(*all);
            }
            _ => panic!("Expected Listcat"),
        }
    }

    #[test]
    fn test_parse_repro() {
        let input = "REPRO INDATASET(SOURCE.DATA) OUTDATASET(TARGET.DATA)";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::Repro {
                indataset,
                outdataset,
                ..
            } => {
                assert_eq!(indataset, "SOURCE.DATA");
                assert_eq!(outdataset, "TARGET.DATA");
            }
            _ => panic!("Expected Repro"),
        }
    }

    #[test]
    fn test_parse_continuation() {
        let input = "DEFINE CLUSTER (NAME(MY.CLUSTER) -\n  KEYS(10 0) -\n  RECORDSIZE(100 200))";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::DefineCluster { name, keys, .. } => {
                assert_eq!(name, "MY.CLUSTER");
                assert_eq!(*keys, Some((10, 0)));
            }
            _ => panic!("Expected DefineCluster"),
        }
    }

    #[test]
    fn test_parse_define_aix() {
        let input = "DEFINE ALTERNATEINDEX (NAME(MY.AIX) RELATE(MY.CLUSTER) KEYS(20 10) UNIQUEKEY)";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::DefineAix {
                name,
                relate,
                keys,
                unique_key,
            } => {
                assert_eq!(name, "MY.AIX");
                assert_eq!(relate, "MY.CLUSTER");
                assert_eq!(*keys, (20, 10));
                assert!(*unique_key);
            }
            _ => panic!("Expected DefineAix"),
        }
    }

    #[test]
    fn test_parse_define_aix_nonunique() {
        let input = "DEFINE AIX (NAME(MY.AIX2) RELATE(MY.BASE) KEYS(30 5))";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::DefineAix {
                name,
                relate,
                keys,
                unique_key,
            } => {
                assert_eq!(name, "MY.AIX2");
                assert_eq!(relate, "MY.BASE");
                assert_eq!(*keys, (30, 5));
                assert!(!*unique_key);
            }
            _ => panic!("Expected DefineAix"),
        }
    }

    #[test]
    fn test_parse_define_path() {
        let input = "DEFINE PATH (NAME(MY.PATH) PATHENTRY(MY.AIX))";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::DefinePath { name, pathentry } => {
                assert_eq!(name, "MY.PATH");
                assert_eq!(pathentry, "MY.AIX");
            }
            _ => panic!("Expected DefinePath"),
        }
    }

    #[test]
    fn test_parse_repro_with_skip_count() {
        let input = "REPRO INDATASET(SOURCE.DATA) OUTDATASET(TARGET.DATA) SKIP(5) COUNT(10)";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::Repro {
                indataset,
                outdataset,
                skip,
                count,
                ..
            } => {
                assert_eq!(indataset, "SOURCE.DATA");
                assert_eq!(outdataset, "TARGET.DATA");
                assert_eq!(*skip, 5);
                assert_eq!(*count, 10);
            }
            _ => panic!("Expected Repro"),
        }
    }

    #[test]
    fn test_parse_repro_with_fromkey_tokey() {
        let input = "REPRO INDATASET(IN.DS) OUTDATASET(OUT.DS) FROMKEY(AAA) TOKEY(ZZZ)";
        let cmds = parse_commands(input).unwrap();
        assert_eq!(cmds.len(), 1);

        match &cmds[0] {
            IdcamsCommand::Repro {
                fromkey,
                tokey,
                skip,
                count,
                ..
            } => {
                assert_eq!(fromkey.as_deref(), Some("AAA"));
                assert_eq!(tokey.as_deref(), Some("ZZZ"));
                assert_eq!(*skip, 0);
                assert_eq!(*count, 0);
            }
            _ => panic!("Expected Repro"),
        }
    }

    #[test]
    fn test_extract_param() {
        assert_eq!(
            extract_param("NAME(MY.TEST)", "NAME"),
            Some("MY.TEST".to_string())
        );
        assert_eq!(extract_param("KEYS(10 0)", "KEYS"), Some("10 0".to_string()));
        assert_eq!(extract_param("NONAME", "NAME"), None);
    }
}
