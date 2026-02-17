//! IDCAMS utility for dataset and catalog management.
//!
//! IDCAMS (Integrated Data Cluster Access Method Services) is the primary
//! utility for managing VSAM datasets, GDGs, and the catalog.
//!
//! # Supported Commands
//!
//! - DEFINE CLUSTER - Create VSAM clusters (KSDS, ESDS, RRDS)
//! - DEFINE GDG - Create Generation Data Groups
//! - DELETE - Delete datasets and catalog entries
//! - ALTER - Rename datasets
//! - LISTCAT - List catalog entries
//! - PRINT - Display dataset contents
//! - REPRO - Copy datasets
//! - DEFINE ALTERNATEINDEX - Create VSAM alternate indexes
//! - DEFINE PATH - Connect AIX to base cluster
//! - VERIFY - Verify VSAM integrity
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_dataset::idcams::{Idcams, IdcamsResult};
//!
//! let mut idcams = Idcams::new("/datasets");
//! let result = idcams.execute("DEFINE CLUSTER (NAME(MY.CLUSTER) KEYS(10 0))")?;
//! println!("{}", result.output);
//! ```

mod commands;
mod parser;

pub use commands::{IdcamsCommand, IdcamsResult};
pub use parser::parse_commands;

use std::path::{Path, PathBuf};

use crate::error::DatasetError;
use crate::gdg::{GdgBase, GdgOptions};
use crate::vsam::{ClusterParams, VsamCluster, VsamType};

/// IDCAMS utility executor.
pub struct Idcams {
    /// Base directory for datasets.
    base_dir: PathBuf,
    /// Output buffer.
    output: String,
    /// Return code (0 = success, 4 = warning, 8+ = error).
    return_code: u32,
}

impl Idcams {
    /// Create a new IDCAMS instance.
    pub fn new(base_dir: impl AsRef<Path>) -> Self {
        Self {
            base_dir: base_dir.as_ref().to_path_buf(),
            output: String::new(),
            return_code: 0,
        }
    }

    /// Execute IDCAMS commands from a string.
    pub fn execute(&mut self, input: &str) -> Result<IdcamsResult, DatasetError> {
        self.output.clear();
        self.return_code = 0;

        let commands = parse_commands(input)?;

        for cmd in commands {
            self.execute_command(&cmd)?;
        }

        Ok(IdcamsResult {
            output: self.output.clone(),
            return_code: self.return_code,
        })
    }

    /// Execute a single command.
    fn execute_command(&mut self, cmd: &IdcamsCommand) -> Result<(), DatasetError> {
        match cmd {
            IdcamsCommand::DefineCluster {
                name,
                cluster_type,
                keys,
                recordsize,
                ..
            } => self.define_cluster(name, cluster_type, keys, recordsize),

            IdcamsCommand::DefineGdg {
                name,
                limit,
                scratch,
                empty,
            } => self.define_gdg(name, *limit, *scratch, *empty),

            IdcamsCommand::Delete { name, purge, force } => self.delete(name, *purge, *force),

            IdcamsCommand::Alter { name, newname } => self.alter(name, newname),

            IdcamsCommand::Listcat { entry, level, all } => {
                self.listcat(entry.as_deref(), level.as_deref(), *all)
            }

            IdcamsCommand::Print {
                dataset,
                character,
                hex,
                skip,
                count,
            } => self.print(dataset, *character, *hex, *skip, *count),

            IdcamsCommand::Repro {
                indataset,
                outdataset,
                fromkey,
                tokey,
            } => self.repro(indataset, outdataset, fromkey.as_deref(), tokey.as_deref()),

            IdcamsCommand::Verify { dataset } => self.verify(dataset),

            IdcamsCommand::DefineAix {
                name,
                relate,
                keys,
                unique_key,
            } => self.define_aix(name, relate, keys, *unique_key),

            IdcamsCommand::DefinePath { name, pathentry } => self.define_path(name, pathentry),
        }
    }

    /// DEFINE CLUSTER command.
    fn define_cluster(
        &mut self,
        name: &str,
        cluster_type: &VsamType,
        keys: &Option<(u16, u16)>,
        recordsize: &Option<(u32, u32)>,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE CLUSTER - {}\n", name));

        let record_size = recordsize.map(|(_, max)| max as usize).unwrap_or(256);
        let path = self.name_to_path(name).with_extension("vsam");

        let params = match cluster_type {
            VsamType::Ksds => {
                let (key_len, key_off) = keys.unwrap_or((8, 0));
                ClusterParams::ksds(name, record_size, key_off as usize, key_len as usize)
                    .with_path(path)
            }
            VsamType::Esds => ClusterParams::esds(name, record_size).with_path(path),
            VsamType::Rrds => ClusterParams::rrds(name, record_size).with_path(path),
        };

        let mut cluster = VsamCluster::new(params)?;
        cluster.create()?;

        self.output
            .push_str(&format!("IDC0002I CLUSTER {} DEFINED\n", name));
        Ok(())
    }

    /// DEFINE GDG command.
    fn define_gdg(
        &mut self,
        name: &str,
        limit: u8,
        scratch: bool,
        empty: bool,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE GDG - {}\n", name));

        let options = GdgOptions {
            limit,
            scratch,
            empty,
            ..Default::default()
        };

        GdgBase::create(name, &self.base_dir, options)?;

        self.output
            .push_str(&format!("IDC0002I GDG {} DEFINED\n", name));
        Ok(())
    }

    /// DELETE command.
    fn delete(&mut self, name: &str, _purge: bool, force: bool) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DELETE - {}\n", name));

        let path = self.name_to_path(name);

        // Check if it's a GDG
        let gdg_path = path.join(".gdg");
        if gdg_path.exists() {
            let base = GdgBase::open(name, &self.base_dir)?;
            if !force && base.generation_count() > 0 {
                self.output.push_str(&format!(
                    "IDC3001E DELETE FAILED - GDG HAS {} GENERATIONS\n",
                    base.generation_count()
                ));
                self.return_code = 8;
                return Ok(());
            }
            base.delete()?;
        } else if path.exists() {
            // Regular file or VSAM cluster
            if path.is_dir() {
                std::fs::remove_dir_all(&path)?;
            } else {
                std::fs::remove_file(&path)?;
            }
        } else if !force {
            self.output
                .push_str(&format!("IDC3002E DELETE FAILED - {} NOT FOUND\n", name));
            self.return_code = 8;
            return Ok(());
        }

        self.output
            .push_str(&format!("IDC0002I {} DELETED\n", name));
        Ok(())
    }

    /// ALTER command.
    fn alter(&mut self, name: &str, newname: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I ALTER - {} TO {}\n", name, newname));

        let old_path = self.name_to_path(name);
        let new_path = self.name_to_path(newname);

        if !old_path.exists() {
            self.output
                .push_str(&format!("IDC3002E ALTER FAILED - {} NOT FOUND\n", name));
            self.return_code = 8;
            return Ok(());
        }

        std::fs::create_dir_all(new_path.parent().unwrap_or(Path::new(".")))?;
        std::fs::rename(&old_path, &new_path)?;

        self.output
            .push_str(&format!("IDC0002I {} RENAMED TO {}\n", name, newname));
        Ok(())
    }

    /// LISTCAT command.
    fn listcat(
        &mut self,
        entry: Option<&str>,
        level: Option<&str>,
        _all: bool,
    ) -> Result<(), DatasetError> {
        self.output.push_str("IDC0001I LISTCAT\n");

        let search_dir = if let Some(lvl) = level {
            self.name_to_path(lvl)
        } else if let Some(ent) = entry {
            self.name_to_path(ent)
        } else {
            self.base_dir.clone()
        };

        if entry.is_some() && search_dir.exists() {
            // Single entry
            self.list_entry(&search_dir, entry.unwrap())?;
        } else if search_dir.exists() && search_dir.is_dir() {
            // List directory
            self.list_directory(&search_dir, level.unwrap_or("*"))?;
        } else {
            self.output
                .push_str("IDC3002E NO ENTRIES FOUND\n");
            self.return_code = 4;
        }

        Ok(())
    }

    /// List a single catalog entry.
    fn list_entry(&mut self, path: &Path, name: &str) -> Result<(), DatasetError> {
        self.output.push_str(&format!("\n{}\n", name));

        // Check for GDG
        let gdg_path = path.join(".gdg");
        if gdg_path.exists() {
            let base = GdgBase::open(name, &self.base_dir)?;
            let info = base.list_info();
            self.output.push_str("  TYPE: GDG\n");
            self.output
                .push_str(&format!("  LIMIT: {}\n", info.limit));
            self.output
                .push_str(&format!("  SCRATCH: {}\n", info.scratch));
            self.output
                .push_str(&format!("  GENERATIONS: {}\n", info.generation_count));
            return Ok(());
        }

        // Check for VSAM cluster
        let vsam_path = path.with_extension("vsam");
        if vsam_path.exists() {
            if let Ok(cluster) = VsamCluster::open(&vsam_path) {
                self.output.push_str(&format!("  TYPE: VSAM {:?}\n", cluster.vsam_type()));
                self.output.push_str(&format!("  RECORD SIZE: {}\n", cluster.record_size()));
                return Ok(());
            }
        }

        // Regular file
        if path.exists() {
            let metadata = std::fs::metadata(path)?;
            self.output.push_str("  TYPE: PS\n");
            self.output
                .push_str(&format!("  SIZE: {} bytes\n", metadata.len()));
        }

        Ok(())
    }

    /// List directory contents.
    fn list_directory(&mut self, dir: &Path, _prefix: &str) -> Result<(), DatasetError> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let file_name = entry.file_name();
            let name_str = file_name.to_string_lossy();

            // Skip hidden files
            if name_str.starts_with('.') {
                continue;
            }

            self.output.push_str(&format!("  {}\n", name_str));
        }
        Ok(())
    }

    /// PRINT command.
    fn print(
        &mut self,
        dataset: &str,
        character: bool,
        hex: bool,
        skip: usize,
        count: usize,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I PRINT - {}\n", dataset));

        let path = self.name_to_path(dataset);
        if !path.exists() {
            self.output
                .push_str(&format!("IDC3002E PRINT FAILED - {} NOT FOUND\n", dataset));
            self.return_code = 8;
            return Ok(());
        }

        let data = std::fs::read(&path)?;

        let start = skip.min(data.len());
        let end = if count > 0 {
            (start + count).min(data.len())
        } else {
            data.len()
        };

        let slice = &data[start..end];

        // Print in chunks of 16 bytes
        for (i, chunk) in slice.chunks(16).enumerate() {
            let offset = start + i * 16;

            if hex || !character {
                // Hex format
                self.output.push_str(&format!("{:08X}  ", offset));
                for byte in chunk {
                    self.output.push_str(&format!("{:02X} ", byte));
                }
                self.output.push(' ');
            }

            if character || !hex {
                // Character format
                self.output.push_str(" |");
                for byte in chunk {
                    let c = if *byte >= 0x20 && *byte < 0x7F {
                        *byte as char
                    } else {
                        '.'
                    };
                    self.output.push(c);
                }
                self.output.push('|');
            }

            self.output.push('\n');
        }

        self.output
            .push_str(&format!("IDC0002I {} RECORDS PRINTED\n", slice.len() / 16 + 1));
        Ok(())
    }

    /// REPRO command.
    fn repro(
        &mut self,
        indataset: &str,
        outdataset: &str,
        _fromkey: Option<&str>,
        _tokey: Option<&str>,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I REPRO - {} TO {}\n", indataset, outdataset));

        let in_path = self.name_to_path(indataset);
        let out_path = self.name_to_path(outdataset);

        if !in_path.exists() {
            self.output
                .push_str(&format!("IDC3002E REPRO FAILED - {} NOT FOUND\n", indataset));
            self.return_code = 8;
            return Ok(());
        }

        std::fs::create_dir_all(out_path.parent().unwrap_or(Path::new(".")))?;
        std::fs::copy(&in_path, &out_path)?;

        let metadata = std::fs::metadata(&out_path)?;
        self.output
            .push_str(&format!("IDC0002I {} BYTES COPIED\n", metadata.len()));
        Ok(())
    }

    /// VERIFY command.
    fn verify(&mut self, dataset: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I VERIFY - {}\n", dataset));

        let path = self.name_to_path(dataset);
        let vsam_path = path.with_extension("vsam");

        if vsam_path.exists() {
            // Verify VSAM cluster
            if let Ok(_cluster) = VsamCluster::open(&vsam_path) {
                self.output
                    .push_str(&format!("IDC0002I {} VERIFIED OK\n", dataset));
            } else {
                self.output
                    .push_str(&format!("IDC3003E {} VERIFY FAILED\n", dataset));
                self.return_code = 8;
            }
        } else if path.exists() {
            self.output
                .push_str(&format!("IDC0002I {} VERIFIED OK\n", dataset));
        } else {
            self.output
                .push_str(&format!("IDC3002E VERIFY FAILED - {} NOT FOUND\n", dataset));
            self.return_code = 8;
        }

        Ok(())
    }

    /// DEFINE ALTERNATEINDEX command.
    fn define_aix(
        &mut self,
        name: &str,
        relate: &str,
        keys: &(u16, u16),
        unique_key: bool,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE ALTERNATEINDEX - {}\n", name));

        // Store AIX definition as a metadata file
        let path = self.name_to_path(name).with_extension("aix");
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let unique_str = if unique_key { "UNIQUEKEY" } else { "NONUNIQUEKEY" };
        let metadata = format!(
            "AIX={}\nRELATE={}\nKEYS={} {}\n{}\n",
            name, relate, keys.0, keys.1, unique_str
        );
        std::fs::write(&path, metadata)?;

        self.output
            .push_str(&format!("IDC0002I ALTERNATEINDEX {} DEFINED\n", name));
        Ok(())
    }

    /// DEFINE PATH command.
    fn define_path(&mut self, name: &str, pathentry: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE PATH - {}\n", name));

        // Store PATH definition as a metadata file
        let path = self.name_to_path(name).with_extension("path");
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let metadata = format!("PATH={}\nPATHENTRY={}\n", name, pathentry);
        std::fs::write(&path, metadata)?;

        self.output
            .push_str(&format!("IDC0002I PATH {} DEFINED\n", name));
        Ok(())
    }

    /// Convert dataset name to file path.
    fn name_to_path(&self, name: &str) -> PathBuf {
        let mut path = self.base_dir.clone();
        for component in name.split('.') {
            path.push(component);
        }
        path
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_dir() -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("idcams_test_{}", count))
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_dir_all(path);
    }

    #[test]
    fn test_define_gdg() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("DEFINE GDG (NAME(MY.TEST.GDG) LIMIT(5) SCRATCH)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("GDG MY.TEST.GDG DEFINED"));

        cleanup(&dir);
    }

    #[test]
    fn test_define_cluster() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("DEFINE CLUSTER (NAME(MY.VSAM.DATA) KEYS(10 0) RECORDSIZE(100 200))")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("CLUSTER MY.VSAM.DATA DEFINED"));

        // Verify file was created
        let vsam_path = dir.join("MY/VSAM/DATA.vsam");
        assert!(vsam_path.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_delete() {
        let dir = test_dir();
        cleanup(&dir);

        // Create a test file
        let test_path = dir.join("TEST/FILE");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "test data").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams.execute("DELETE TEST.FILE").unwrap();

        assert!(result.is_success());
        assert!(!test_path.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_repro() {
        let dir = test_dir();
        cleanup(&dir);

        // Create source file
        let source_path = dir.join("SOURCE/DATA");
        fs::create_dir_all(source_path.parent().unwrap()).unwrap();
        fs::write(&source_path, "source data content").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("REPRO INDATASET(SOURCE.DATA) OUTDATASET(TARGET.DATA)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("BYTES COPIED"));

        // Verify target was created
        let target_path = dir.join("TARGET/DATA");
        assert!(target_path.exists());
        assert_eq!(fs::read_to_string(&target_path).unwrap(), "source data content");

        cleanup(&dir);
    }

    #[test]
    fn test_print() {
        let dir = test_dir();
        cleanup(&dir);

        // Create test file
        let test_path = dir.join("TEST/DATA");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "Hello World!").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("PRINT INDATASET(TEST.DATA) CHARACTER")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("Hello World!"));

        cleanup(&dir);
    }

    #[test]
    fn test_listcat() {
        let dir = test_dir();
        cleanup(&dir);

        // Create a GDG
        let mut idcams = Idcams::new(&dir);
        idcams
            .execute("DEFINE GDG (NAME(MY.GDG) LIMIT(5))")
            .unwrap();

        let result = idcams.execute("LISTCAT ENT(MY.GDG) ALL").unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("MY.GDG"));
        assert!(result.output.contains("TYPE: GDG"));

        cleanup(&dir);
    }

    #[test]
    fn test_define_aix() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);

        // First create the base cluster
        idcams
            .execute("DEFINE CLUSTER (NAME(MY.BASE) KEYS(10 0) RECORDSIZE(100 200))")
            .unwrap();

        // Define an alternate index
        let result = idcams
            .execute("DEFINE ALTERNATEINDEX (NAME(MY.AIX) RELATE(MY.BASE) KEYS(20 10) UNIQUEKEY)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("ALTERNATEINDEX MY.AIX DEFINED"));

        // Verify AIX metadata file was created
        let aix_path = dir.join("MY/AIX.aix");
        assert!(aix_path.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_define_path() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);

        // Define a path
        let result = idcams
            .execute("DEFINE PATH (NAME(MY.PATH) PATHENTRY(MY.AIX))")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("PATH MY.PATH DEFINED"));

        // Verify PATH metadata file was created
        let path_file = dir.join("MY/PATH.path");
        assert!(path_file.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_multiple_commands() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute(
                "DEFINE GDG (NAME(MY.GDG) LIMIT(3))
                 LISTCAT ENT(MY.GDG)",
            )
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("GDG MY.GDG DEFINED"));
        assert!(result.output.contains("TYPE: GDG"));

        cleanup(&dir);
    }
}
