//! VSAM Cluster definition and management.
//!
//! A VSAM cluster is a logical grouping of data and index components
//! that together form a complete dataset.

use std::fs::{self, File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

use crate::error::DatasetError;

/// VSAM dataset organization types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VsamType {
    /// Key-Sequenced Data Set - records ordered by primary key.
    Ksds,
    /// Entry-Sequenced Data Set - records in arrival order.
    Esds,
    /// Relative Record Data Set - records by slot number.
    Rrds,
}

impl VsamType {
    /// Returns the magic number for this VSAM type.
    pub fn magic(&self) -> [u8; 4] {
        match self {
            VsamType::Ksds => *b"KSDS",
            VsamType::Esds => *b"ESDS",
            VsamType::Rrds => *b"RRDS",
        }
    }

    /// Parse VSAM type from magic bytes.
    pub fn from_magic(magic: &[u8; 4]) -> Option<Self> {
        match magic {
            b"KSDS" => Some(VsamType::Ksds),
            b"ESDS" => Some(VsamType::Esds),
            b"RRDS" => Some(VsamType::Rrds),
            _ => None,
        }
    }
}

/// Key specification for KSDS clusters.
#[derive(Debug, Clone)]
pub struct KeySpec {
    /// Offset of the key within the record (0-based).
    pub offset: usize,
    /// Length of the key in bytes.
    pub length: usize,
}

impl KeySpec {
    /// Creates a new key specification.
    pub fn new(offset: usize, length: usize) -> Self {
        Self { offset, length }
    }

    /// Validates the key spec against a record size.
    pub fn validate(&self, record_size: usize) -> Result<(), DatasetError> {
        if self.length == 0 {
            return Err(DatasetError::InvalidParameter(
                "Key length must be greater than 0".to_string(),
            ));
        }
        if self.offset + self.length > record_size {
            return Err(DatasetError::InvalidParameter(format!(
                "Key specification (offset {} + length {}) exceeds record size {}",
                self.offset, self.length, record_size
            )));
        }
        Ok(())
    }

    /// Extracts the key from a record.
    pub fn extract_key<'a>(&self, record: &'a [u8]) -> &'a [u8] {
        &record[self.offset..self.offset + self.length]
    }
}

/// Parameters for creating a VSAM cluster.
#[derive(Debug, Clone)]
pub struct ClusterParams {
    /// Dataset name (e.g., "MY.VSAM.DATA").
    pub name: String,
    /// VSAM organization type.
    pub vsam_type: VsamType,
    /// Maximum record size in bytes.
    pub record_size: usize,
    /// Key specification (required for KSDS).
    pub key_spec: Option<KeySpec>,
    /// Control interval size (default: 4096).
    pub ci_size: usize,
    /// Storage path for the cluster file.
    pub path: Option<PathBuf>,
}

impl ClusterParams {
    /// Creates parameters for a KSDS cluster.
    pub fn ksds(name: &str, record_size: usize, key_offset: usize, key_length: usize) -> Self {
        Self {
            name: name.to_string(),
            vsam_type: VsamType::Ksds,
            record_size,
            key_spec: Some(KeySpec::new(key_offset, key_length)),
            ci_size: 4096,
            path: None,
        }
    }

    /// Creates parameters for an ESDS cluster.
    pub fn esds(name: &str, record_size: usize) -> Self {
        Self {
            name: name.to_string(),
            vsam_type: VsamType::Esds,
            record_size,
            key_spec: None,
            ci_size: 4096,
            path: None,
        }
    }

    /// Creates parameters for an RRDS cluster.
    pub fn rrds(name: &str, record_size: usize) -> Self {
        Self {
            name: name.to_string(),
            vsam_type: VsamType::Rrds,
            record_size,
            key_spec: None,
            ci_size: 4096,
            path: None,
        }
    }

    /// Sets the storage path for the cluster.
    pub fn with_path<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.path = Some(path.as_ref().to_path_buf());
        self
    }

    /// Sets the control interval size.
    pub fn with_ci_size(mut self, ci_size: usize) -> Self {
        self.ci_size = ci_size;
        self
    }

    /// Validates the parameters.
    pub fn validate(&self) -> Result<(), DatasetError> {
        if self.name.is_empty() {
            return Err(DatasetError::InvalidParameter(
                "Cluster name cannot be empty".to_string(),
            ));
        }

        if self.record_size == 0 {
            return Err(DatasetError::InvalidParameter(
                "Record size must be greater than 0".to_string(),
            ));
        }

        if self.ci_size < self.record_size {
            return Err(DatasetError::InvalidParameter(format!(
                "Control interval size {} must be >= record size {}",
                self.ci_size, self.record_size
            )));
        }

        // KSDS requires key specification
        if self.vsam_type == VsamType::Ksds {
            match &self.key_spec {
                Some(key_spec) => key_spec.validate(self.record_size)?,
                None => {
                    return Err(DatasetError::InvalidParameter(
                        "KSDS cluster requires key specification".to_string(),
                    ))
                }
            }
        }

        Ok(())
    }
}

/// VSAM file header structure (128 bytes).
///
/// Layout:
/// - Bytes 0-3: Magic number (KSDS, ESDS, or RRDS)
/// - Bytes 4-5: Version (1)
/// - Bytes 6-7: Reserved
/// - Bytes 8-15: Record count
/// - Bytes 16-19: Record size
/// - Bytes 20-23: CI size
/// - Bytes 24-27: Key offset (KSDS only)
/// - Bytes 28-31: Key length (KSDS only)
/// - Bytes 32-127: Reserved
const HEADER_SIZE: usize = 128;
const HEADER_VERSION: u16 = 1;

/// A VSAM cluster representing a complete indexed dataset.
#[derive(Debug)]
pub struct VsamCluster {
    /// Cluster parameters.
    params: ClusterParams,
    /// Path to the cluster file.
    path: PathBuf,
    /// Current record count.
    record_count: u64,
    /// Whether the cluster is open.
    is_open: bool,
}

impl VsamCluster {
    /// Creates a new KSDS cluster with the given parameters.
    pub fn new_ksds(
        name: &str,
        record_size: usize,
        key_offset: usize,
        key_length: usize,
    ) -> Result<Self, DatasetError> {
        let params = ClusterParams::ksds(name, record_size, key_offset, key_length);
        Self::new(params)
    }

    /// Creates a new cluster with the given parameters.
    pub fn new(params: ClusterParams) -> Result<Self, DatasetError> {
        params.validate()?;

        let path = params
            .path
            .clone()
            .unwrap_or_else(|| Self::default_path(&params.name));

        Ok(Self {
            params,
            path,
            record_count: 0,
            is_open: false,
        })
    }

    /// Opens an existing cluster from disk.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, DatasetError> {
        let path = path.as_ref().to_path_buf();
        let mut file = File::open(&path).map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        // Read header
        let mut header = [0u8; HEADER_SIZE];
        file.read_exact(&mut header)
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        // Parse magic
        let magic: [u8; 4] = header[0..4].try_into().unwrap();
        let vsam_type = VsamType::from_magic(&magic).ok_or_else(|| {
            DatasetError::InvalidParameter("Invalid VSAM file format".to_string())
        })?;

        // Parse version
        let version = u16::from_le_bytes(header[4..6].try_into().unwrap());
        if version != HEADER_VERSION {
            return Err(DatasetError::InvalidParameter(format!(
                "Unsupported VSAM version {}",
                version
            )));
        }

        // Parse fields
        let record_count = u64::from_le_bytes(header[8..16].try_into().unwrap());
        let record_size = u32::from_le_bytes(header[16..20].try_into().unwrap()) as usize;
        let ci_size = u32::from_le_bytes(header[20..24].try_into().unwrap()) as usize;
        let key_offset = u32::from_le_bytes(header[24..28].try_into().unwrap()) as usize;
        let key_length = u32::from_le_bytes(header[28..32].try_into().unwrap()) as usize;

        let key_spec = if vsam_type == VsamType::Ksds {
            Some(KeySpec::new(key_offset, key_length))
        } else {
            None
        };

        // Extract name from path
        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("UNKNOWN")
            .to_string();

        let params = ClusterParams {
            name,
            vsam_type,
            record_size,
            key_spec,
            ci_size,
            path: Some(path.clone()),
        };

        Ok(Self {
            params,
            path,
            record_count,
            is_open: true,
        })
    }

    /// Creates the cluster on disk.
    pub fn create(&mut self) -> Result<(), DatasetError> {
        if self.is_open {
            return Err(DatasetError::InvalidParameter(
                "Cluster is already open".to_string(),
            ));
        }

        // Create parent directory if needed
        if let Some(parent) = self.path.parent() {
            fs::create_dir_all(parent).map_err(|e| DatasetError::IoError { message: e.to_string() })?;
        }

        // Create the file
        let mut file = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&self.path)
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        // Write header
        let header = self.build_header();
        file.write_all(&header)
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        self.is_open = true;
        Ok(())
    }

    /// Builds the file header.
    fn build_header(&self) -> [u8; HEADER_SIZE] {
        let mut header = [0u8; HEADER_SIZE];

        // Magic (4 bytes)
        header[0..4].copy_from_slice(&self.params.vsam_type.magic());

        // Version (2 bytes)
        header[4..6].copy_from_slice(&HEADER_VERSION.to_le_bytes());

        // Record count (8 bytes)
        header[8..16].copy_from_slice(&self.record_count.to_le_bytes());

        // Record size (4 bytes)
        header[16..20].copy_from_slice(&(self.params.record_size as u32).to_le_bytes());

        // CI size (4 bytes)
        header[20..24].copy_from_slice(&(self.params.ci_size as u32).to_le_bytes());

        // Key spec (8 bytes)
        if let Some(ref key_spec) = self.params.key_spec {
            header[24..28].copy_from_slice(&(key_spec.offset as u32).to_le_bytes());
            header[28..32].copy_from_slice(&(key_spec.length as u32).to_le_bytes());
        }

        header
    }

    /// Updates the header on disk.
    fn update_header(&self) -> Result<(), DatasetError> {
        let mut file = OpenOptions::new()
            .write(true)
            .open(&self.path)
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        file.seek(SeekFrom::Start(0))
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        let header = self.build_header();
        file.write_all(&header)
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        Ok(())
    }

    /// Returns the default path for a cluster name.
    fn default_path(name: &str) -> PathBuf {
        let sanitized = name.replace('.', "/");
        PathBuf::from(format!("{}.vsam", sanitized))
    }

    /// Returns the cluster name.
    pub fn name(&self) -> &str {
        &self.params.name
    }

    /// Returns the VSAM type.
    pub fn vsam_type(&self) -> VsamType {
        self.params.vsam_type
    }

    /// Returns the record size.
    pub fn record_size(&self) -> usize {
        self.params.record_size
    }

    /// Returns the control interval size.
    pub fn ci_size(&self) -> usize {
        self.params.ci_size
    }

    /// Returns the key specification (for KSDS).
    pub fn key_spec(&self) -> Option<&KeySpec> {
        self.params.key_spec.as_ref()
    }

    /// Returns the current record count.
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Returns the path to the cluster file.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Returns whether the cluster is open.
    pub fn is_open(&self) -> bool {
        self.is_open
    }

    /// Closes the cluster.
    pub fn close(&mut self) -> Result<(), DatasetError> {
        if self.is_open {
            self.update_header()?;
            self.is_open = false;
        }
        Ok(())
    }

    /// Deletes the cluster from disk.
    pub fn delete(&mut self) -> Result<(), DatasetError> {
        if self.is_open {
            self.is_open = false;
        }
        fs::remove_file(&self.path).map_err(|e| DatasetError::IoError { message: e.to_string() })?;
        Ok(())
    }

    /// Increments the record count.
    pub fn increment_record_count(&mut self) {
        self.record_count += 1;
    }

    /// Sets the record count.
    pub fn set_record_count(&mut self, count: u64) {
        self.record_count = count;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_path(name: &str) -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("vsam_test_{}_{}.vsam", name, count))
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_key_spec_validation() {
        let key_spec = KeySpec::new(0, 10);
        assert!(key_spec.validate(100).is_ok());
        assert!(key_spec.validate(10).is_ok());
        assert!(key_spec.validate(9).is_err());
    }

    #[test]
    fn test_key_spec_extract() {
        let key_spec = KeySpec::new(5, 3);
        let record = b"Hello World";
        assert_eq!(key_spec.extract_key(record), b" Wo");
    }

    #[test]
    fn test_cluster_params_ksds() {
        let params = ClusterParams::ksds("MY.TEST.DATA", 100, 0, 10);
        assert!(params.validate().is_ok());
    }

    #[test]
    fn test_cluster_params_invalid_key() {
        let params = ClusterParams::ksds("MY.TEST.DATA", 100, 95, 10);
        assert!(params.validate().is_err());
    }

    #[test]
    fn test_cluster_params_zero_key_length() {
        let params = ClusterParams::ksds("MY.TEST.DATA", 100, 0, 0);
        assert!(params.validate().is_err());
    }

    #[test]
    fn test_new_ksds() {
        let cluster = VsamCluster::new_ksds("MY.TEST.DATA", 100, 0, 10);
        assert!(cluster.is_ok());

        let cluster = cluster.unwrap();
        assert_eq!(cluster.name(), "MY.TEST.DATA");
        assert_eq!(cluster.vsam_type(), VsamType::Ksds);
        assert_eq!(cluster.record_size(), 100);
    }

    #[test]
    fn test_create_and_open_cluster() {
        let path = test_path("create_open");

        // Create cluster
        let params = ClusterParams::ksds("MY.TEST", 100, 0, 10).with_path(&path);
        let mut cluster = VsamCluster::new(params).unwrap();
        cluster.create().unwrap();

        assert!(path.exists());

        // Open cluster
        let opened = VsamCluster::open(&path).unwrap();
        assert_eq!(opened.vsam_type(), VsamType::Ksds);
        assert_eq!(opened.record_size(), 100);
        assert_eq!(opened.key_spec().unwrap().offset, 0);
        assert_eq!(opened.key_spec().unwrap().length, 10);

        cleanup(&path);
    }

    #[test]
    fn test_vsam_type_magic() {
        assert_eq!(VsamType::Ksds.magic(), *b"KSDS");
        assert_eq!(VsamType::Esds.magic(), *b"ESDS");
        assert_eq!(VsamType::Rrds.magic(), *b"RRDS");
    }

    #[test]
    fn test_default_path() {
        let path = VsamCluster::default_path("MY.TEST.DATA");
        assert_eq!(path, PathBuf::from("MY/TEST/DATA.vsam"));
    }

    #[test]
    fn test_delete_cluster() {
        let path = test_path("delete");

        let params = ClusterParams::ksds("TEST", 100, 0, 10).with_path(&path);
        let mut cluster = VsamCluster::new(params).unwrap();
        cluster.create().unwrap();

        assert!(path.exists());

        cluster.delete().unwrap();
        assert!(!path.exists());
    }
}
