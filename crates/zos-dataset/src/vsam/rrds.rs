//! RRDS (Relative Record Data Set) implementation.
//!
//! RRDS stores records in numbered slots. Each slot has a fixed size
//! and records are accessed by their relative record number (slot number).

use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::Path;

use super::cluster::{ClusterParams, VsamCluster, VsamType};
use super::ksds::FileStatus;
use crate::error::DatasetError;

/// Header size in bytes.
const HEADER_SIZE: u64 = 128;

/// Slot header size (1 byte for status).
const SLOT_HEADER_SIZE: usize = 1;

/// Slot status: empty.
const SLOT_EMPTY: u8 = 0x00;
/// Slot status: occupied.
const SLOT_OCCUPIED: u8 = 0x01;

/// Result of an RRDS operation, including file status.
#[derive(Debug)]
pub struct RrdsResult<T> {
    /// The result value (if any).
    pub value: Option<T>,
    /// The file status code.
    pub status: FileStatus,
}

impl<T> RrdsResult<T> {
    /// Creates a successful result.
    pub fn success(value: T) -> Self {
        Self {
            value: Some(value),
            status: FileStatus::Success,
        }
    }

    /// Creates a result with no value.
    pub fn empty(status: FileStatus) -> Self {
        Self {
            value: None,
            status,
        }
    }
}

/// RRDS (Relative Record Data Set) handler.
///
/// RRDS stores records in numbered slots. Each slot is identified by its
/// relative record number (RRN), starting from 1.
#[derive(Debug)]
pub struct Rrds {
    /// The underlying VSAM cluster.
    cluster: VsamCluster,
    /// Last operation status.
    last_status: FileStatus,
    /// Current slot for sequential reading.
    current_slot: Option<u64>,
    /// Total slot size (header + record).
    slot_size: usize,
    /// Highest allocated slot number.
    highest_slot: u64,
}

impl Rrds {
    /// Creates a new RRDS cluster.
    pub fn new(name: &str, record_size: usize) -> Result<Self, DatasetError> {
        let params = ClusterParams::rrds(name, record_size);
        Self::from_params(params)
    }

    /// Creates a new RRDS from cluster parameters.
    pub fn from_params(params: ClusterParams) -> Result<Self, DatasetError> {
        if params.vsam_type != VsamType::Rrds {
            return Err(DatasetError::InvalidParameter(
                "Parameters must specify RRDS type".to_string(),
            ));
        }

        let slot_size = SLOT_HEADER_SIZE + params.record_size;
        let mut cluster = VsamCluster::new(params)?;
        cluster.create()?;

        Ok(Self {
            cluster,
            last_status: FileStatus::Success,
            current_slot: None,
            slot_size,
            highest_slot: 0,
        })
    }

    /// Opens an existing RRDS cluster.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, DatasetError> {
        let cluster = VsamCluster::open(path.as_ref())?;

        if cluster.vsam_type() != VsamType::Rrds {
            return Err(DatasetError::InvalidParameter(
                "File is not an RRDS cluster".to_string(),
            ));
        }

        let slot_size = SLOT_HEADER_SIZE + cluster.record_size();

        // Calculate highest slot from file size
        let file = File::open(cluster.path())
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;
        let file_size = file
            .metadata()
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?
            .len();

        let data_size = file_size.saturating_sub(HEADER_SIZE);
        let highest_slot = data_size / slot_size as u64;

        Ok(Self {
            cluster,
            last_status: FileStatus::Success,
            current_slot: None,
            slot_size,
            highest_slot,
        })
    }

    /// Returns the last operation status.
    pub fn status(&self) -> FileStatus {
        self.last_status
    }

    /// Returns the record size.
    pub fn record_size(&self) -> usize {
        self.cluster.record_size()
    }

    /// Returns the current record count.
    pub fn record_count(&self) -> u64 {
        self.cluster.record_count()
    }

    /// Returns the cluster path.
    pub fn path(&self) -> &Path {
        self.cluster.path()
    }

    /// Returns the highest allocated slot number.
    pub fn highest_slot(&self) -> u64 {
        self.highest_slot
    }

    /// Calculates the file offset for a given slot number.
    fn slot_offset(&self, slot: u64) -> u64 {
        HEADER_SIZE + (slot - 1) * self.slot_size as u64
    }

    /// Writes a record to a specific slot.
    ///
    /// Slot numbers start from 1.
    pub fn write(&mut self, slot: u64, record: &[u8]) -> RrdsResult<()> {
        if slot == 0 {
            self.last_status = FileStatus::RecordNotFound;
            return RrdsResult::empty(FileStatus::RecordNotFound);
        }

        if record.len() > self.cluster.record_size() {
            self.last_status = FileStatus::RecordLengthError;
            return RrdsResult::empty(FileStatus::RecordLengthError);
        }

        // Check if slot is already occupied
        if slot <= self.highest_slot {
            if let Some(true) = self.is_slot_occupied(slot) {
                self.last_status = FileStatus::DuplicateKey;
                return RrdsResult::empty(FileStatus::DuplicateKey);
            }
        }

        // Open file for writing
        let mut file = match OpenOptions::new()
            .write(true)
            .open(self.cluster.path())
        {
            Ok(f) => f,
            Err(_) => {
                self.last_status = FileStatus::IoError;
                return RrdsResult::empty(FileStatus::IoError);
            }
        };

        // Ensure file is extended if needed
        if slot > self.highest_slot {
            // Extend file to accommodate new slot
            let new_size = self.slot_offset(slot) + self.slot_size as u64;
            if file.set_len(new_size).is_err() {
                self.last_status = FileStatus::IoError;
                return RrdsResult::empty(FileStatus::IoError);
            }
            self.highest_slot = slot;
        }

        // Seek to slot
        let offset = self.slot_offset(slot);
        if file.seek(SeekFrom::Start(offset)).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        // Write slot status (occupied)
        if file.write_all(&[SLOT_OCCUPIED]).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        // Write record data (padded to record size)
        let mut padded_record = vec![0u8; self.cluster.record_size()];
        padded_record[..record.len()].copy_from_slice(record);

        if file.write_all(&padded_record).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        // Update record count
        self.cluster.increment_record_count();

        self.last_status = FileStatus::Success;
        RrdsResult::success(())
    }

    /// Reads a record from a specific slot.
    ///
    /// Slot numbers start from 1.
    pub fn read(&mut self, slot: u64) -> RrdsResult<Vec<u8>> {
        if slot == 0 || slot > self.highest_slot {
            self.last_status = FileStatus::RecordNotFound;
            return RrdsResult::empty(FileStatus::RecordNotFound);
        }

        // Open file for reading
        let mut file = match File::open(self.cluster.path()) {
            Ok(f) => f,
            Err(_) => {
                self.last_status = FileStatus::IoError;
                return RrdsResult::empty(FileStatus::IoError);
            }
        };

        // Seek to slot
        let offset = self.slot_offset(slot);
        if file.seek(SeekFrom::Start(offset)).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        // Read slot status
        let mut status = [0u8; 1];
        if file.read_exact(&mut status).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        if status[0] != SLOT_OCCUPIED {
            self.last_status = FileStatus::RecordNotFound;
            return RrdsResult::empty(FileStatus::RecordNotFound);
        }

        // Read record data
        let mut record = vec![0u8; self.cluster.record_size()];
        if file.read_exact(&mut record).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        // Set current slot for sequential operations
        self.current_slot = Some(slot);

        self.last_status = FileStatus::Success;
        RrdsResult::success(record)
    }

    /// Updates a record in a specific slot.
    pub fn rewrite(&mut self, slot: u64, record: &[u8]) -> RrdsResult<()> {
        if slot == 0 || slot > self.highest_slot {
            self.last_status = FileStatus::RecordNotFound;
            return RrdsResult::empty(FileStatus::RecordNotFound);
        }

        if record.len() > self.cluster.record_size() {
            self.last_status = FileStatus::RecordLengthError;
            return RrdsResult::empty(FileStatus::RecordLengthError);
        }

        // Check if slot is occupied
        if let Some(false) = self.is_slot_occupied(slot) {
            self.last_status = FileStatus::RecordNotFound;
            return RrdsResult::empty(FileStatus::RecordNotFound);
        }

        // Open file for writing
        let mut file = match OpenOptions::new()
            .write(true)
            .open(self.cluster.path())
        {
            Ok(f) => f,
            Err(_) => {
                self.last_status = FileStatus::IoError;
                return RrdsResult::empty(FileStatus::IoError);
            }
        };

        // Seek to slot data (skip status byte)
        let offset = self.slot_offset(slot) + 1;
        if file.seek(SeekFrom::Start(offset)).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        // Write record data (padded)
        let mut padded_record = vec![0u8; self.cluster.record_size()];
        padded_record[..record.len()].copy_from_slice(record);

        if file.write_all(&padded_record).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        self.last_status = FileStatus::Success;
        RrdsResult::success(())
    }

    /// Deletes a record from a specific slot.
    pub fn delete(&mut self, slot: u64) -> RrdsResult<()> {
        if slot == 0 || slot > self.highest_slot {
            self.last_status = FileStatus::RecordNotFound;
            return RrdsResult::empty(FileStatus::RecordNotFound);
        }

        // Check if slot is occupied
        if let Some(false) = self.is_slot_occupied(slot) {
            self.last_status = FileStatus::RecordNotFound;
            return RrdsResult::empty(FileStatus::RecordNotFound);
        }

        // Open file for writing
        let mut file = match OpenOptions::new()
            .write(true)
            .open(self.cluster.path())
        {
            Ok(f) => f,
            Err(_) => {
                self.last_status = FileStatus::IoError;
                return RrdsResult::empty(FileStatus::IoError);
            }
        };

        // Seek to slot status
        let offset = self.slot_offset(slot);
        if file.seek(SeekFrom::Start(offset)).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        // Mark slot as empty
        if file.write_all(&[SLOT_EMPTY]).is_err() {
            self.last_status = FileStatus::IoError;
            return RrdsResult::empty(FileStatus::IoError);
        }

        self.last_status = FileStatus::Success;
        RrdsResult::success(())
    }

    /// Checks if a slot is occupied.
    fn is_slot_occupied(&self, slot: u64) -> Option<bool> {
        let mut file = File::open(self.cluster.path()).ok()?;
        let offset = self.slot_offset(slot);
        file.seek(SeekFrom::Start(offset)).ok()?;

        let mut status = [0u8; 1];
        file.read_exact(&mut status).ok()?;

        Some(status[0] == SLOT_OCCUPIED)
    }

    /// Positions for sequential reading at the first slot.
    pub fn start(&mut self) -> FileStatus {
        self.current_slot = Some(0); // Will be incremented on first read
        self.last_status = FileStatus::Success;
        FileStatus::Success
    }

    /// Positions for sequential reading at a specific slot.
    pub fn start_at(&mut self, slot: u64) -> FileStatus {
        if slot > self.highest_slot {
            self.last_status = FileStatus::RecordNotFound;
            return FileStatus::RecordNotFound;
        }
        self.current_slot = Some(slot.saturating_sub(1)); // Will be incremented on read
        self.last_status = FileStatus::Success;
        FileStatus::Success
    }

    /// Reads the next occupied slot in sequence.
    pub fn read_next(&mut self) -> RrdsResult<(u64, Vec<u8>)> {
        let mut slot = match self.current_slot {
            Some(s) => s + 1,
            None => {
                self.last_status = FileStatus::LogicError;
                return RrdsResult::empty(FileStatus::LogicError);
            }
        };

        // Find next occupied slot
        while slot <= self.highest_slot {
            if let Some(true) = self.is_slot_occupied(slot) {
                let result = self.read(slot);
                if result.status.is_success() {
                    self.current_slot = Some(slot);
                    return RrdsResult::success((slot, result.value.unwrap()));
                }
            }
            slot += 1;
        }

        self.last_status = FileStatus::EndOfFile;
        RrdsResult::empty(FileStatus::EndOfFile)
    }

    /// Closes the RRDS cluster.
    pub fn close(&mut self) -> Result<(), DatasetError> {
        self.cluster.close()
    }

    /// Deletes the RRDS cluster.
    pub fn delete_cluster(&mut self) -> Result<(), DatasetError> {
        self.cluster.delete()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_path(name: &str) -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("rrds_test_{}_{}.vsam", name, count))
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_create_rrds() {
        let path = test_path("create");
        let params = ClusterParams::rrds("TEST.RRDS", 100).with_path(&path);
        let rrds = Rrds::from_params(params);
        assert!(rrds.is_ok());

        let rrds = rrds.unwrap();
        assert_eq!(rrds.record_size(), 100);
        assert_eq!(rrds.record_count(), 0);

        cleanup(&path);
    }

    #[test]
    fn test_write_and_read() {
        let path = test_path("write_read");
        let params = ClusterParams::rrds("TEST.RRDS", 100).with_path(&path);
        let mut rrds = Rrds::from_params(params).unwrap();

        // Write to slot 1
        let record = b"Record in slot 1";
        let result = rrds.write(1, record);
        assert!(result.status.is_success());

        // Write to slot 5 (skipping slots 2-4)
        let record5 = b"Record in slot 5";
        let result5 = rrds.write(5, record5);
        assert!(result5.status.is_success());

        // Read slot 1
        let read_result = rrds.read(1);
        assert!(read_result.status.is_success());
        let data = read_result.value.unwrap();
        assert!(data.starts_with(record));

        // Read slot 5
        let read_result5 = rrds.read(5);
        assert!(read_result5.status.is_success());
        let data5 = read_result5.value.unwrap();
        assert!(data5.starts_with(record5));

        // Read empty slot 3
        let read_empty = rrds.read(3);
        assert_eq!(read_empty.status, FileStatus::RecordNotFound);

        cleanup(&path);
    }

    #[test]
    fn test_duplicate_slot() {
        let path = test_path("duplicate");
        let params = ClusterParams::rrds("TEST.RRDS", 100).with_path(&path);
        let mut rrds = Rrds::from_params(params).unwrap();

        // Write to slot 1
        rrds.write(1, b"First record");

        // Try to write to slot 1 again
        let result = rrds.write(1, b"Second record");
        assert_eq!(result.status, FileStatus::DuplicateKey);

        cleanup(&path);
    }

    #[test]
    fn test_rewrite() {
        let path = test_path("rewrite");
        let params = ClusterParams::rrds("TEST.RRDS", 100).with_path(&path);
        let mut rrds = Rrds::from_params(params).unwrap();

        // Write original record
        rrds.write(1, b"Original");

        // Rewrite
        let result = rrds.rewrite(1, b"Updated");
        assert!(result.status.is_success());

        // Read back
        let read_result = rrds.read(1);
        assert!(read_result.status.is_success());
        let data = read_result.value.unwrap();
        assert!(data.starts_with(b"Updated"));

        cleanup(&path);
    }

    #[test]
    fn test_delete() {
        let path = test_path("delete");
        let params = ClusterParams::rrds("TEST.RRDS", 100).with_path(&path);
        let mut rrds = Rrds::from_params(params).unwrap();

        // Write and delete
        rrds.write(1, b"To be deleted");
        let result = rrds.delete(1);
        assert!(result.status.is_success());

        // Read deleted slot
        let read_result = rrds.read(1);
        assert_eq!(read_result.status, FileStatus::RecordNotFound);

        // Can write to deleted slot again
        let write_result = rrds.write(1, b"New record");
        assert!(write_result.status.is_success());

        cleanup(&path);
    }

    #[test]
    fn test_sequential_read() {
        let path = test_path("sequential");
        let params = ClusterParams::rrds("TEST.RRDS", 100).with_path(&path);
        let mut rrds = Rrds::from_params(params).unwrap();

        // Write to slots 1, 3, 5 (skip 2, 4)
        rrds.write(1, b"Slot 1");
        rrds.write(3, b"Slot 3");
        rrds.write(5, b"Slot 5");

        // Sequential read
        rrds.start();

        let r1 = rrds.read_next();
        assert!(r1.status.is_success());
        let (slot1, data1) = r1.value.unwrap();
        assert_eq!(slot1, 1);
        assert!(data1.starts_with(b"Slot 1"));

        let r2 = rrds.read_next();
        assert!(r2.status.is_success());
        let (slot2, data2) = r2.value.unwrap();
        assert_eq!(slot2, 3);
        assert!(data2.starts_with(b"Slot 3"));

        let r3 = rrds.read_next();
        assert!(r3.status.is_success());
        let (slot3, data3) = r3.value.unwrap();
        assert_eq!(slot3, 5);
        assert!(data3.starts_with(b"Slot 5"));

        // EOF
        let r4 = rrds.read_next();
        assert_eq!(r4.status, FileStatus::EndOfFile);

        cleanup(&path);
    }

    #[test]
    fn test_invalid_slot_zero() {
        let path = test_path("slot_zero");
        let params = ClusterParams::rrds("TEST.RRDS", 100).with_path(&path);
        let mut rrds = Rrds::from_params(params).unwrap();

        // Slot 0 is invalid
        let result = rrds.write(0, b"Invalid");
        assert_eq!(result.status, FileStatus::RecordNotFound);

        let result = rrds.read(0);
        assert_eq!(result.status, FileStatus::RecordNotFound);

        cleanup(&path);
    }

    #[test]
    fn test_open_existing_rrds() {
        let path = test_path("open");
        let params = ClusterParams::rrds("TEST.RRDS", 100).with_path(&path);

        // Create and write
        {
            let mut rrds = Rrds::from_params(params).unwrap();
            rrds.write(1, b"Persistent");
            rrds.write(3, b"Data");
            rrds.close().unwrap();
        }

        // Reopen and read
        {
            let mut rrds = Rrds::open(&path).unwrap();
            assert_eq!(rrds.highest_slot(), 3);

            let result = rrds.read(1);
            assert!(result.status.is_success());
            assert!(result.value.unwrap().starts_with(b"Persistent"));

            let result = rrds.read(3);
            assert!(result.status.is_success());
            assert!(result.value.unwrap().starts_with(b"Data"));
        }

        cleanup(&path);
    }

    #[test]
    fn test_record_too_long() {
        let path = test_path("too_long");
        let params = ClusterParams::rrds("TEST.RRDS", 10).with_path(&path);
        let mut rrds = Rrds::from_params(params).unwrap();

        let long_record = vec![0u8; 100];
        let result = rrds.write(1, &long_record);
        assert_eq!(result.status, FileStatus::RecordLengthError);

        cleanup(&path);
    }
}
