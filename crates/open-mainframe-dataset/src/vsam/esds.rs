//! ESDS (Entry-Sequenced Data Set) implementation.
//!
//! ESDS stores records in arrival order. Records are accessed sequentially
//! or by their RBA (Relative Byte Address).

use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::Path;

use super::cluster::{ClusterParams, VsamCluster, VsamType};
use super::ksds::FileStatus;
use crate::error::DatasetError;

/// Header size in bytes.
const HEADER_SIZE: u64 = 128;

/// Result of an ESDS operation, including file status.
#[derive(Debug)]
pub struct EsdsResult<T> {
    /// The result value (if any).
    pub value: Option<T>,
    /// The file status code.
    pub status: FileStatus,
}

impl<T> EsdsResult<T> {
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

/// ESDS (Entry-Sequenced Data Set) handler.
///
/// ESDS stores records in arrival order. Each record has an RBA (Relative Byte Address)
/// that can be used to retrieve it directly.
#[derive(Debug)]
pub struct Esds {
    /// The underlying VSAM cluster.
    cluster: VsamCluster,
    /// Last operation status.
    last_status: FileStatus,
    /// Current RBA position for sequential reading.
    current_rba: Option<u64>,
    /// End of data offset (next write position).
    end_of_data: u64,
}

impl Esds {
    /// Creates a new ESDS cluster.
    pub fn new(name: &str, record_size: usize) -> Result<Self, DatasetError> {
        let params = ClusterParams::esds(name, record_size);
        Self::from_params(params)
    }

    /// Creates a new ESDS from cluster parameters.
    pub fn from_params(params: ClusterParams) -> Result<Self, DatasetError> {
        if params.vsam_type != VsamType::Esds {
            return Err(DatasetError::InvalidParameter(
                "Parameters must specify ESDS type".to_string(),
            ));
        }

        let mut cluster = VsamCluster::new(params)?;
        cluster.create()?;

        Ok(Self {
            cluster,
            last_status: FileStatus::Success,
            current_rba: None,
            end_of_data: HEADER_SIZE,
        })
    }

    /// Opens an existing ESDS cluster.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, DatasetError> {
        let cluster = VsamCluster::open(path.as_ref())?;

        if cluster.vsam_type() != VsamType::Esds {
            return Err(DatasetError::InvalidParameter(
                "File is not an ESDS cluster".to_string(),
            ));
        }

        // Calculate end of data from file size
        let file = File::open(cluster.path())
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;
        let end_of_data = file
            .metadata()
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?
            .len();

        Ok(Self {
            cluster,
            last_status: FileStatus::Success,
            current_rba: None,
            end_of_data,
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

    /// Writes a record to the ESDS, returning its RBA.
    ///
    /// Records are appended to the end of the dataset.
    pub fn write(&mut self, record: &[u8]) -> EsdsResult<u64> {
        if record.len() > self.cluster.record_size() {
            self.last_status = FileStatus::RecordLengthError;
            return EsdsResult::empty(FileStatus::RecordLengthError);
        }

        // Open file for writing
        let mut file = match OpenOptions::new()
            .write(true)
            .open(self.cluster.path())
        {
            Ok(f) => f,
            Err(_) => {
                self.last_status = FileStatus::IoError;
                return EsdsResult::empty(FileStatus::IoError);
            }
        };

        // Seek to end of data
        if file.seek(SeekFrom::Start(self.end_of_data)).is_err() {
            self.last_status = FileStatus::IoError;
            return EsdsResult::empty(FileStatus::IoError);
        }

        // Write record length (4 bytes) + record data
        let record_len = record.len() as u32;
        if file.write_all(&record_len.to_le_bytes()).is_err() {
            self.last_status = FileStatus::IoError;
            return EsdsResult::empty(FileStatus::IoError);
        }

        if file.write_all(record).is_err() {
            self.last_status = FileStatus::IoError;
            return EsdsResult::empty(FileStatus::IoError);
        }

        // Remember the RBA of this record
        let rba = self.end_of_data;

        // Update end of data position
        self.end_of_data += 4 + record.len() as u64;

        // Update record count
        self.cluster.increment_record_count();

        self.last_status = FileStatus::Success;
        EsdsResult::success(rba)
    }

    /// Reads a record by RBA (Relative Byte Address).
    pub fn read_rba(&mut self, rba: u64) -> EsdsResult<Vec<u8>> {
        if rba < HEADER_SIZE || rba >= self.end_of_data {
            self.last_status = FileStatus::RecordNotFound;
            return EsdsResult::empty(FileStatus::RecordNotFound);
        }

        // Open file for reading
        let mut file = match File::open(self.cluster.path()) {
            Ok(f) => f,
            Err(_) => {
                self.last_status = FileStatus::IoError;
                return EsdsResult::empty(FileStatus::IoError);
            }
        };

        // Seek to RBA
        if file.seek(SeekFrom::Start(rba)).is_err() {
            self.last_status = FileStatus::IoError;
            return EsdsResult::empty(FileStatus::IoError);
        }

        // Read record length
        let mut len_buf = [0u8; 4];
        if file.read_exact(&mut len_buf).is_err() {
            self.last_status = FileStatus::IoError;
            return EsdsResult::empty(FileStatus::IoError);
        }
        let record_len = u32::from_le_bytes(len_buf) as usize;

        // Validate record length
        if record_len > self.cluster.record_size() {
            self.last_status = FileStatus::RecordLengthError;
            return EsdsResult::empty(FileStatus::RecordLengthError);
        }

        // Read record data
        let mut record = vec![0u8; record_len];
        if file.read_exact(&mut record).is_err() {
            self.last_status = FileStatus::IoError;
            return EsdsResult::empty(FileStatus::IoError);
        }

        // Update current position for sequential read
        self.current_rba = Some(rba + 4 + record_len as u64);

        self.last_status = FileStatus::Success;
        EsdsResult::success(record)
    }

    /// Positions for sequential reading at the beginning of the dataset.
    pub fn start(&mut self) -> FileStatus {
        self.current_rba = Some(HEADER_SIZE);
        self.last_status = FileStatus::Success;
        FileStatus::Success
    }

    /// Positions for sequential reading at a specific RBA.
    pub fn start_at(&mut self, rba: u64) -> FileStatus {
        if rba < HEADER_SIZE {
            self.current_rba = Some(HEADER_SIZE);
        } else if rba >= self.end_of_data {
            self.last_status = FileStatus::RecordNotFound;
            return FileStatus::RecordNotFound;
        } else {
            self.current_rba = Some(rba);
        }
        self.last_status = FileStatus::Success;
        FileStatus::Success
    }

    /// Reads the next record in sequence.
    pub fn read_next(&mut self) -> EsdsResult<Vec<u8>> {
        let rba = match self.current_rba {
            Some(rba) => rba,
            None => {
                self.last_status = FileStatus::LogicError;
                return EsdsResult::empty(FileStatus::LogicError);
            }
        };

        if rba >= self.end_of_data {
            self.last_status = FileStatus::EndOfFile;
            return EsdsResult::empty(FileStatus::EndOfFile);
        }

        // read_rba updates current_rba to point to next record
        self.read_rba(rba)
    }

    /// Closes the ESDS cluster.
    pub fn close(&mut self) -> Result<(), DatasetError> {
        self.cluster.close()
    }

    /// Deletes the ESDS cluster.
    pub fn delete(&mut self) -> Result<(), DatasetError> {
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
        std::env::temp_dir().join(format!("esds_test_{}_{}.vsam", name, count))
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_create_esds() {
        let path = test_path("create");
        let params = ClusterParams::esds("TEST.ESDS", 100).with_path(&path);
        let esds = Esds::from_params(params);
        assert!(esds.is_ok());

        let esds = esds.unwrap();
        assert_eq!(esds.record_size(), 100);
        assert_eq!(esds.record_count(), 0);

        cleanup(&path);
    }

    #[test]
    fn test_write_and_read_rba() {
        let path = test_path("write_read");
        let params = ClusterParams::esds("TEST.ESDS", 100).with_path(&path);
        let mut esds = Esds::from_params(params).unwrap();

        // Write a record
        let record = b"Hello, ESDS!";
        let result = esds.write(record);
        assert!(result.status.is_success());
        let rba = result.value.unwrap();
        assert_eq!(rba, HEADER_SIZE); // First record at header boundary

        // Write another record
        let record2 = b"Second record";
        let result2 = esds.write(record2);
        assert!(result2.status.is_success());
        let rba2 = result2.value.unwrap();
        assert!(rba2 > rba);

        // Read first record by RBA
        let read_result = esds.read_rba(rba);
        assert!(read_result.status.is_success());
        assert_eq!(read_result.value.unwrap(), record.to_vec());

        // Read second record by RBA
        let read_result2 = esds.read_rba(rba2);
        assert!(read_result2.status.is_success());
        assert_eq!(read_result2.value.unwrap(), record2.to_vec());

        cleanup(&path);
    }

    #[test]
    fn test_sequential_read() {
        let path = test_path("sequential");
        let params = ClusterParams::esds("TEST.ESDS", 100).with_path(&path);
        let mut esds = Esds::from_params(params).unwrap();

        // Write some records
        esds.write(b"Record 1");
        esds.write(b"Record 2");
        esds.write(b"Record 3");

        // Sequential read
        esds.start();

        let r1 = esds.read_next();
        assert!(r1.status.is_success());
        assert_eq!(r1.value.unwrap(), b"Record 1".to_vec());

        let r2 = esds.read_next();
        assert!(r2.status.is_success());
        assert_eq!(r2.value.unwrap(), b"Record 2".to_vec());

        let r3 = esds.read_next();
        assert!(r3.status.is_success());
        assert_eq!(r3.value.unwrap(), b"Record 3".to_vec());

        // EOF
        let r4 = esds.read_next();
        assert_eq!(r4.status, FileStatus::EndOfFile);

        cleanup(&path);
    }

    #[test]
    fn test_open_existing_esds() {
        let path = test_path("open");
        let params = ClusterParams::esds("TEST.ESDS", 100).with_path(&path);

        // Create and write
        {
            let mut esds = Esds::from_params(params).unwrap();
            esds.write(b"Persistent data");
            esds.close().unwrap();
        }

        // Reopen and read
        {
            let mut esds = Esds::open(&path).unwrap();
            esds.start();
            let result = esds.read_next();
            assert!(result.status.is_success());
            assert_eq!(result.value.unwrap(), b"Persistent data".to_vec());
        }

        cleanup(&path);
    }

    #[test]
    fn test_read_invalid_rba() {
        let path = test_path("invalid_rba");
        let params = ClusterParams::esds("TEST.ESDS", 100).with_path(&path);
        let mut esds = Esds::from_params(params).unwrap();

        // Try to read from invalid RBA
        let result = esds.read_rba(0); // Before header
        assert_eq!(result.status, FileStatus::RecordNotFound);

        let result = esds.read_rba(9999); // Beyond data
        assert_eq!(result.status, FileStatus::RecordNotFound);

        cleanup(&path);
    }

    #[test]
    fn test_record_too_long() {
        let path = test_path("too_long");
        let params = ClusterParams::esds("TEST.ESDS", 10).with_path(&path);
        let mut esds = Esds::from_params(params).unwrap();

        // Try to write record larger than max size
        let long_record = vec![0u8; 100];
        let result = esds.write(&long_record);
        assert_eq!(result.status, FileStatus::RecordLengthError);

        cleanup(&path);
    }

    #[test]
    fn test_read_next_without_start() {
        let path = test_path("no_start");
        let params = ClusterParams::esds("TEST.ESDS", 100).with_path(&path);
        let mut esds = Esds::from_params(params).unwrap();

        // Try to read without positioning
        let result = esds.read_next();
        assert_eq!(result.status, FileStatus::LogicError);

        cleanup(&path);
    }

    #[test]
    fn test_start_at_rba() {
        let path = test_path("start_at");
        let params = ClusterParams::esds("TEST.ESDS", 100).with_path(&path);
        let mut esds = Esds::from_params(params).unwrap();

        // Write records
        let _rba1 = esds.write(b"Record 1").value.unwrap();
        let rba2 = esds.write(b"Record 2").value.unwrap();
        esds.write(b"Record 3");

        // Start at second record
        esds.start_at(rba2);
        let result = esds.read_next();
        assert!(result.status.is_success());
        assert_eq!(result.value.unwrap(), b"Record 2".to_vec());

        cleanup(&path);
    }
}
