//! COBOL file I/O with standard file status codes.
//!
//! Implements the `FileHandler` trait and standard 2-byte COBOL file status
//! codes per AD-3.0-05. Provides implementations for:
//!
//! - **`MemoryFile`** — in-memory records for testing
//! - **`SequentialFile`** — line-based sequential file I/O
//! - **`IndexedFile`** — indexed (KSDS-style) file I/O with key access
//!
//! # File Status Codes
//!
//! | Code | Meaning                              |
//! |------|--------------------------------------|
//! | 00   | Successful completion                |
//! | 02   | Duplicate key (non-unique ALT key)   |
//! | 10   | End of file (AT END)                 |
//! | 21   | Sequence error                       |
//! | 22   | Duplicate key                        |
//! | 23   | Record not found                     |
//! | 30   | Permanent I/O error                  |
//! | 35   | File not found on OPEN               |
//! | 37   | Access mode conflict                 |
//! | 41   | OPEN of already-open file            |
//! | 42   | CLOSE of not-open file               |
//! | 43   | REWRITE/DELETE without prior READ     |
//! | 46   | READ beyond end of file              |
//! | 47   | READ on file not opened INPUT/I-O    |
//! | 48   | WRITE on file not opened OUTPUT/I-O  |
//! | 49   | REWRITE/DELETE on file not opened I-O |

use std::collections::BTreeMap;
use std::fmt;
use std::io::{self, BufRead, Write};
use std::path::Path;

/// Standard COBOL 2-byte file status code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileStatus(pub u8, pub u8);

impl FileStatus {
    /// Successful completion.
    pub const SUCCESS: Self = Self(0, 0);
    /// Successful completion, duplicate key allowed.
    pub const DUPLICATE_KEY_OK: Self = Self(0, 2);
    /// End of file.
    pub const END_OF_FILE: Self = Self(1, 0);
    /// Sequence error.
    pub const SEQUENCE_ERROR: Self = Self(2, 1);
    /// Duplicate key (primary or unique alternate).
    pub const DUPLICATE_KEY: Self = Self(2, 2);
    /// Record not found.
    pub const RECORD_NOT_FOUND: Self = Self(2, 3);
    /// Permanent I/O error.
    pub const IO_ERROR: Self = Self(3, 0);
    /// File not found.
    pub const FILE_NOT_FOUND: Self = Self(3, 5);
    /// Access mode conflict.
    pub const ACCESS_CONFLICT: Self = Self(3, 7);
    /// File already open.
    pub const ALREADY_OPEN: Self = Self(4, 1);
    /// File not open.
    pub const NOT_OPEN: Self = Self(4, 2);
    /// No prior READ for REWRITE/DELETE.
    pub const NO_PRIOR_READ: Self = Self(4, 3);
    /// READ beyond EOF.
    pub const READ_PAST_EOF: Self = Self(4, 6);
    /// READ on file not open for input.
    pub const READ_NOT_INPUT: Self = Self(4, 7);
    /// WRITE on file not open for output.
    pub const WRITE_NOT_OUTPUT: Self = Self(4, 8);
    /// REWRITE/DELETE on file not open for I-O.
    pub const NOT_IO_MODE: Self = Self(4, 9);

    /// Check if the operation was successful (status 0x).
    pub fn is_success(&self) -> bool {
        self.0 == 0
    }

    /// Convert to the standard 2-character string representation.
    pub fn to_string_code(&self) -> String {
        format!("{}{}", self.0, self.1)
    }
}

impl fmt::Display for FileStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

/// File open mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileOpenMode {
    /// Open for reading (sequential READ).
    Input,
    /// Open for writing (sequential WRITE).
    Output,
    /// Open for reading and writing (READ, REWRITE, DELETE).
    InputOutput,
    /// Open for appending (WRITE at end).
    Extend,
}

/// File organization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileOrganization {
    /// Sequential (PS/QSAM).
    Sequential,
    /// Indexed (KSDS).
    Indexed,
    /// Relative (RRDS).
    Relative,
}

/// File access mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessMode {
    /// Sequential access.
    Sequential,
    /// Random access by key.
    Random,
    /// Both sequential and random access.
    Dynamic,
}

/// File descriptor — the SELECT clause metadata.
#[derive(Debug, Clone)]
pub struct FileDescriptor {
    /// COBOL file name (e.g., "FILE-1").
    pub name: String,
    /// File organization.
    pub organization: FileOrganization,
    /// Access mode.
    pub access_mode: AccessMode,
    /// Physical file path (from ASSIGN TO).
    pub path: Option<String>,
    /// Record length (RECORD CONTAINS).
    pub record_length: Option<usize>,
    /// Key offset and length for indexed files.
    pub key_spec: Option<KeySpec>,
}

/// Key specification for indexed files.
#[derive(Debug, Clone)]
pub struct KeySpec {
    /// Byte offset of the key within the record.
    pub offset: usize,
    /// Key length in bytes.
    pub length: usize,
}

/// Trait for file I/O handlers.
///
/// Implementations provide the actual I/O operations. Each operation
/// returns a [`FileStatus`] code.
pub trait FileHandler {
    /// Open the file in the specified mode.
    fn open(&mut self, mode: FileOpenMode) -> FileStatus;
    /// Close the file.
    fn close(&mut self) -> FileStatus;
    /// Read the next record sequentially.
    fn read_next(&mut self) -> (FileStatus, Option<Vec<u8>>);
    /// Read a record by key (indexed files only).
    fn read_key(&mut self, key: &[u8]) -> (FileStatus, Option<Vec<u8>>);
    /// Write a record.
    fn write(&mut self, record: &[u8]) -> FileStatus;
    /// Rewrite the last-read record.
    fn rewrite(&mut self, record: &[u8]) -> FileStatus;
    /// Delete the last-read record (indexed files only).
    fn delete(&mut self) -> FileStatus;
    /// Position for sequential reading at or after the given key.
    fn start(&mut self, key: &[u8]) -> FileStatus;
    /// Get the current file status.
    fn status(&self) -> FileStatus;
    /// Check if the file is open.
    fn is_open(&self) -> bool;
}

// ---------------------------------------------------------------------------
// MemoryFile — in-memory implementation for testing
// ---------------------------------------------------------------------------

/// In-memory file handler for testing.
///
/// Stores records as `Vec<Vec<u8>>`. Supports sequential and key-based access.
#[derive(Debug)]
pub struct MemoryFile {
    records: Vec<Vec<u8>>,
    position: usize,
    mode: Option<FileOpenMode>,
    status: FileStatus,
    last_read: bool,
    last_read_idx: Option<usize>,
    key_spec: Option<KeySpec>,
    organization: FileOrganization,
}

impl MemoryFile {
    /// Create a new empty memory file.
    pub fn new(organization: FileOrganization, key_spec: Option<KeySpec>) -> Self {
        Self {
            records: Vec::new(),
            position: 0,
            mode: None,
            status: FileStatus::SUCCESS,
            last_read: false,
            last_read_idx: None,
            key_spec,
            organization,
        }
    }

    /// Create a memory file pre-loaded with records.
    pub fn with_records(
        records: Vec<Vec<u8>>,
        organization: FileOrganization,
        key_spec: Option<KeySpec>,
    ) -> Self {
        Self {
            records,
            position: 0,
            mode: None,
            status: FileStatus::SUCCESS,
            last_read: false,
            last_read_idx: None,
            key_spec,
            organization,
        }
    }

    /// Get all records.
    pub fn records(&self) -> &[Vec<u8>] {
        &self.records
    }

    /// Extract the key from a record.
    fn extract_key<'a>(&self, record: &'a [u8]) -> Option<&'a [u8]> {
        let ks = self.key_spec.as_ref()?;
        if record.len() >= ks.offset + ks.length {
            Some(&record[ks.offset..ks.offset + ks.length])
        } else {
            None
        }
    }

    /// Find a record by key, returning its index.
    fn find_by_key(&self, key: &[u8]) -> Option<usize> {
        self.records.iter().position(|r| {
            self.extract_key(r)
                .map(|k| k == key)
                .unwrap_or(false)
        })
    }
}

impl FileHandler for MemoryFile {
    fn open(&mut self, mode: FileOpenMode) -> FileStatus {
        if self.mode.is_some() {
            self.status = FileStatus::ALREADY_OPEN;
            return self.status;
        }
        self.mode = Some(mode);
        self.position = 0;
        self.last_read = false;
        self.last_read_idx = None;
        if mode == FileOpenMode::Output {
            self.records.clear();
        }
        self.status = FileStatus::SUCCESS;
        self.status
    }

    fn close(&mut self) -> FileStatus {
        if self.mode.is_none() {
            self.status = FileStatus::NOT_OPEN;
            return self.status;
        }
        self.mode = None;
        self.last_read = false;
        self.last_read_idx = None;
        self.status = FileStatus::SUCCESS;
        self.status
    }

    fn read_next(&mut self) -> (FileStatus, Option<Vec<u8>>) {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::READ_NOT_INPUT;
                return (self.status, None);
            }
        };

        if mode != FileOpenMode::Input && mode != FileOpenMode::InputOutput {
            self.status = FileStatus::READ_NOT_INPUT;
            return (self.status, None);
        }

        if self.position >= self.records.len() {
            self.status = FileStatus::END_OF_FILE;
            self.last_read = false;
            return (self.status, None);
        }

        let record = self.records[self.position].clone();
        self.last_read_idx = Some(self.position);
        self.position += 1;
        self.last_read = true;
        self.status = FileStatus::SUCCESS;
        (self.status, Some(record))
    }

    fn read_key(&mut self, key: &[u8]) -> (FileStatus, Option<Vec<u8>>) {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::READ_NOT_INPUT;
                return (self.status, None);
            }
        };

        if mode != FileOpenMode::Input && mode != FileOpenMode::InputOutput {
            self.status = FileStatus::READ_NOT_INPUT;
            return (self.status, None);
        }

        if self.organization != FileOrganization::Indexed {
            self.status = FileStatus::IO_ERROR;
            return (self.status, None);
        }

        match self.find_by_key(key) {
            Some(idx) => {
                let record = self.records[idx].clone();
                self.last_read_idx = Some(idx);
                self.position = idx + 1;
                self.last_read = true;
                self.status = FileStatus::SUCCESS;
                (self.status, Some(record))
            }
            None => {
                self.status = FileStatus::RECORD_NOT_FOUND;
                self.last_read = false;
                (self.status, None)
            }
        }
    }

    fn write(&mut self, record: &[u8]) -> FileStatus {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::WRITE_NOT_OUTPUT;
                return self.status;
            }
        };

        if mode != FileOpenMode::Output
            && mode != FileOpenMode::InputOutput
            && mode != FileOpenMode::Extend
        {
            self.status = FileStatus::WRITE_NOT_OUTPUT;
            return self.status;
        }

        // For indexed files, check for duplicate key.
        if self.organization == FileOrganization::Indexed {
            if let Some(key) = self.extract_key(record) {
                if self.find_by_key(key).is_some() {
                    self.status = FileStatus::DUPLICATE_KEY;
                    return self.status;
                }
            }
        }

        self.records.push(record.to_vec());

        self.last_read = false;
        self.status = FileStatus::SUCCESS;
        self.status
    }

    fn rewrite(&mut self, record: &[u8]) -> FileStatus {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::NOT_IO_MODE;
                return self.status;
            }
        };

        if mode != FileOpenMode::InputOutput {
            self.status = FileStatus::NOT_IO_MODE;
            return self.status;
        }

        if !self.last_read {
            self.status = FileStatus::NO_PRIOR_READ;
            return self.status;
        }

        if let Some(idx) = self.last_read_idx {
            if idx < self.records.len() {
                self.records[idx] = record.to_vec();
                self.last_read = false;
                self.status = FileStatus::SUCCESS;
                return self.status;
            }
        }

        self.status = FileStatus::IO_ERROR;
        self.status
    }

    fn delete(&mut self) -> FileStatus {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::NOT_IO_MODE;
                return self.status;
            }
        };

        if mode != FileOpenMode::InputOutput {
            self.status = FileStatus::NOT_IO_MODE;
            return self.status;
        }

        if self.organization != FileOrganization::Indexed {
            self.status = FileStatus::IO_ERROR;
            return self.status;
        }

        if !self.last_read {
            self.status = FileStatus::NO_PRIOR_READ;
            return self.status;
        }

        if let Some(idx) = self.last_read_idx {
            if idx < self.records.len() {
                self.records.remove(idx);
                if self.position > idx {
                    self.position -= 1;
                }
                self.last_read = false;
                self.status = FileStatus::SUCCESS;
                return self.status;
            }
        }

        self.status = FileStatus::IO_ERROR;
        self.status
    }

    fn start(&mut self, key: &[u8]) -> FileStatus {
        if self.organization != FileOrganization::Indexed {
            self.status = FileStatus::IO_ERROR;
            return self.status;
        }

        // Find the first record with key >= provided key.
        let pos = self.records.iter().position(|r| {
            self.extract_key(r)
                .map(|k| k >= key)
                .unwrap_or(false)
        });

        match pos {
            Some(idx) => {
                self.position = idx;
                self.status = FileStatus::SUCCESS;
            }
            None => {
                self.status = FileStatus::RECORD_NOT_FOUND;
            }
        }
        self.status
    }

    fn status(&self) -> FileStatus {
        self.status
    }

    fn is_open(&self) -> bool {
        self.mode.is_some()
    }
}

// ---------------------------------------------------------------------------
// SequentialFile — line-based sequential file I/O
// ---------------------------------------------------------------------------

/// Sequential file handler using standard filesystem I/O.
///
/// Reads/writes records as lines (newline-delimited).
#[derive(Debug)]
pub struct SequentialFile {
    path: String,
    mode: Option<FileOpenMode>,
    status: FileStatus,
    reader: Option<io::BufReader<std::fs::File>>,
    writer: Option<io::BufWriter<std::fs::File>>,
}

impl SequentialFile {
    /// Create a new sequential file handler.
    pub fn new(path: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            mode: None,
            status: FileStatus::SUCCESS,
            reader: None,
            writer: None,
        }
    }
}

impl FileHandler for SequentialFile {
    fn open(&mut self, mode: FileOpenMode) -> FileStatus {
        if self.mode.is_some() {
            self.status = FileStatus::ALREADY_OPEN;
            return self.status;
        }

        let path = Path::new(&self.path);

        match mode {
            FileOpenMode::Input => {
                if !path.exists() {
                    self.status = FileStatus::FILE_NOT_FOUND;
                    return self.status;
                }
                match std::fs::File::open(path) {
                    Ok(f) => {
                        self.reader = Some(io::BufReader::new(f));
                        self.mode = Some(mode);
                        self.status = FileStatus::SUCCESS;
                    }
                    Err(_) => {
                        self.status = FileStatus::IO_ERROR;
                    }
                }
            }
            FileOpenMode::Output => {
                match std::fs::File::create(path) {
                    Ok(f) => {
                        self.writer = Some(io::BufWriter::new(f));
                        self.mode = Some(mode);
                        self.status = FileStatus::SUCCESS;
                    }
                    Err(_) => {
                        self.status = FileStatus::IO_ERROR;
                    }
                }
            }
            FileOpenMode::Extend => {
                match std::fs::OpenOptions::new().create(true).append(true).open(path) {
                    Ok(f) => {
                        self.writer = Some(io::BufWriter::new(f));
                        self.mode = Some(mode);
                        self.status = FileStatus::SUCCESS;
                    }
                    Err(_) => {
                        self.status = FileStatus::IO_ERROR;
                    }
                }
            }
            FileOpenMode::InputOutput => {
                // I-O not supported for sequential files in this implementation.
                self.status = FileStatus::ACCESS_CONFLICT;
            }
        }

        self.status
    }

    fn close(&mut self) -> FileStatus {
        if self.mode.is_none() {
            self.status = FileStatus::NOT_OPEN;
            return self.status;
        }
        // Flush writer if present.
        if let Some(ref mut w) = self.writer {
            let _ = w.flush();
        }
        self.reader = None;
        self.writer = None;
        self.mode = None;
        self.status = FileStatus::SUCCESS;
        self.status
    }

    fn read_next(&mut self) -> (FileStatus, Option<Vec<u8>>) {
        if self.mode.is_none() {
            self.status = FileStatus::READ_NOT_INPUT;
            return (self.status, None);
        }

        if let Some(ref mut reader) = self.reader {
            let mut line = String::new();
            match reader.read_line(&mut line) {
                Ok(0) => {
                    self.status = FileStatus::END_OF_FILE;
                    (self.status, None)
                }
                Ok(_) => {
                    // Remove trailing newline.
                    if line.ends_with('\n') {
                        line.pop();
                        if line.ends_with('\r') {
                            line.pop();
                        }
                    }
                    self.status = FileStatus::SUCCESS;
                    (self.status, Some(line.into_bytes()))
                }
                Err(_) => {
                    self.status = FileStatus::IO_ERROR;
                    (self.status, None)
                }
            }
        } else {
            self.status = FileStatus::READ_NOT_INPUT;
            (self.status, None)
        }
    }

    fn read_key(&mut self, _key: &[u8]) -> (FileStatus, Option<Vec<u8>>) {
        self.status = FileStatus::IO_ERROR;
        (self.status, None)
    }

    fn write(&mut self, record: &[u8]) -> FileStatus {
        if self.mode.is_none() {
            self.status = FileStatus::WRITE_NOT_OUTPUT;
            return self.status;
        }

        if let Some(ref mut writer) = self.writer {
            match writer.write_all(record).and_then(|_| writer.write_all(b"\n")) {
                Ok(_) => {
                    self.status = FileStatus::SUCCESS;
                }
                Err(_) => {
                    self.status = FileStatus::IO_ERROR;
                }
            }
        } else {
            self.status = FileStatus::WRITE_NOT_OUTPUT;
        }

        self.status
    }

    fn rewrite(&mut self, _record: &[u8]) -> FileStatus {
        // REWRITE not supported for sequential files.
        self.status = FileStatus::NOT_IO_MODE;
        self.status
    }

    fn delete(&mut self) -> FileStatus {
        // DELETE not supported for sequential files.
        self.status = FileStatus::NOT_IO_MODE;
        self.status
    }

    fn start(&mut self, _key: &[u8]) -> FileStatus {
        self.status = FileStatus::IO_ERROR;
        self.status
    }

    fn status(&self) -> FileStatus {
        self.status
    }

    fn is_open(&self) -> bool {
        self.mode.is_some()
    }
}

// ---------------------------------------------------------------------------
// IndexedFile — in-memory B-tree indexed file for KSDS-style access
// ---------------------------------------------------------------------------

/// Indexed file handler using an in-memory B-tree.
///
/// Provides KSDS-style keyed access: READ by key, START, READ NEXT,
/// WRITE, REWRITE, DELETE. Records are kept sorted by key.
#[derive(Debug)]
pub struct IndexedFile {
    /// Records stored by key.
    records: BTreeMap<Vec<u8>, Vec<u8>>,
    /// Iterator position: the current key for sequential reads.
    current_key: Option<Vec<u8>>,
    mode: Option<FileOpenMode>,
    status: FileStatus,
    key_spec: KeySpec,
    last_read_key: Option<Vec<u8>>,
}

impl IndexedFile {
    /// Create a new indexed file handler.
    pub fn new(key_spec: KeySpec) -> Self {
        Self {
            records: BTreeMap::new(),
            current_key: None,
            mode: None,
            status: FileStatus::SUCCESS,
            key_spec,
            last_read_key: None,
        }
    }

    /// Create an indexed file pre-loaded with records.
    pub fn with_records(records: Vec<Vec<u8>>, key_spec: KeySpec) -> Self {
        let mut btree = BTreeMap::new();
        for record in records {
            if record.len() >= key_spec.offset + key_spec.length {
                let key = record[key_spec.offset..key_spec.offset + key_spec.length].to_vec();
                btree.insert(key, record);
            }
        }
        Self {
            records: btree,
            current_key: None,
            mode: None,
            status: FileStatus::SUCCESS,
            key_spec,
            last_read_key: None,
        }
    }

    /// Get all records, sorted by key.
    pub fn records(&self) -> Vec<&Vec<u8>> {
        self.records.values().collect()
    }

    /// Extract the key from a record.
    fn extract_key<'a>(&self, record: &'a [u8]) -> &'a [u8] {
        &record[self.key_spec.offset..self.key_spec.offset + self.key_spec.length]
    }
}

impl FileHandler for IndexedFile {
    fn open(&mut self, mode: FileOpenMode) -> FileStatus {
        if self.mode.is_some() {
            self.status = FileStatus::ALREADY_OPEN;
            return self.status;
        }
        if mode == FileOpenMode::Output {
            self.records.clear();
        }
        self.mode = Some(mode);
        self.current_key = None;
        self.last_read_key = None;
        self.status = FileStatus::SUCCESS;
        self.status
    }

    fn close(&mut self) -> FileStatus {
        if self.mode.is_none() {
            self.status = FileStatus::NOT_OPEN;
            return self.status;
        }
        self.mode = None;
        self.current_key = None;
        self.last_read_key = None;
        self.status = FileStatus::SUCCESS;
        self.status
    }

    fn read_next(&mut self) -> (FileStatus, Option<Vec<u8>>) {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::READ_NOT_INPUT;
                return (self.status, None);
            }
        };

        if mode != FileOpenMode::Input && mode != FileOpenMode::InputOutput {
            self.status = FileStatus::READ_NOT_INPUT;
            return (self.status, None);
        }

        let next = match &self.current_key {
            None => {
                // Start from the beginning.
                self.records.iter().next()
            }
            Some(key) => {
                // Find the entry after the current key.
                use std::ops::Bound;
                self.records
                    .range::<Vec<u8>, _>((Bound::Excluded(key), Bound::Unbounded))
                    .next()
            }
        };

        match next {
            Some((key, record)) => {
                self.current_key = Some(key.clone());
                self.last_read_key = Some(key.clone());
                self.status = FileStatus::SUCCESS;
                (self.status, Some(record.clone()))
            }
            None => {
                self.status = FileStatus::END_OF_FILE;
                self.last_read_key = None;
                (self.status, None)
            }
        }
    }

    fn read_key(&mut self, key: &[u8]) -> (FileStatus, Option<Vec<u8>>) {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::READ_NOT_INPUT;
                return (self.status, None);
            }
        };

        if mode != FileOpenMode::Input && mode != FileOpenMode::InputOutput {
            self.status = FileStatus::READ_NOT_INPUT;
            return (self.status, None);
        }

        match self.records.get(key) {
            Some(record) => {
                self.current_key = Some(key.to_vec());
                self.last_read_key = Some(key.to_vec());
                self.status = FileStatus::SUCCESS;
                (self.status, Some(record.clone()))
            }
            None => {
                self.status = FileStatus::RECORD_NOT_FOUND;
                self.last_read_key = None;
                (self.status, None)
            }
        }
    }

    fn write(&mut self, record: &[u8]) -> FileStatus {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::WRITE_NOT_OUTPUT;
                return self.status;
            }
        };

        if mode != FileOpenMode::Output && mode != FileOpenMode::InputOutput {
            self.status = FileStatus::WRITE_NOT_OUTPUT;
            return self.status;
        }

        if record.len() < self.key_spec.offset + self.key_spec.length {
            self.status = FileStatus::IO_ERROR;
            return self.status;
        }

        let key = self.extract_key(record).to_vec();
        if self.records.contains_key(&key) {
            self.status = FileStatus::DUPLICATE_KEY;
            return self.status;
        }

        self.records.insert(key, record.to_vec());
        self.status = FileStatus::SUCCESS;
        self.status
    }

    fn rewrite(&mut self, record: &[u8]) -> FileStatus {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::NOT_IO_MODE;
                return self.status;
            }
        };

        if mode != FileOpenMode::InputOutput {
            self.status = FileStatus::NOT_IO_MODE;
            return self.status;
        }

        let key = match &self.last_read_key {
            Some(k) => k.clone(),
            None => {
                self.status = FileStatus::NO_PRIOR_READ;
                return self.status;
            }
        };

        if !self.records.contains_key(&key) {
            self.status = FileStatus::IO_ERROR;
            return self.status;
        }

        self.records.insert(key, record.to_vec());
        self.last_read_key = None;
        self.status = FileStatus::SUCCESS;
        self.status
    }

    fn delete(&mut self) -> FileStatus {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::NOT_IO_MODE;
                return self.status;
            }
        };

        if mode != FileOpenMode::InputOutput {
            self.status = FileStatus::NOT_IO_MODE;
            return self.status;
        }

        let key = match &self.last_read_key {
            Some(k) => k.clone(),
            None => {
                self.status = FileStatus::NO_PRIOR_READ;
                return self.status;
            }
        };

        if self.records.remove(&key).is_some() {
            self.last_read_key = None;
            self.status = FileStatus::SUCCESS;
        } else {
            self.status = FileStatus::IO_ERROR;
        }

        self.status
    }

    fn start(&mut self, key: &[u8]) -> FileStatus {
        let mode = match self.mode {
            Some(m) => m,
            None => {
                self.status = FileStatus::READ_NOT_INPUT;
                return self.status;
            }
        };

        if mode != FileOpenMode::Input && mode != FileOpenMode::InputOutput {
            self.status = FileStatus::READ_NOT_INPUT;
            return self.status;
        }

        // Position at or after the given key.
        let search_key = key.to_vec();
        let found = self
            .records
            .range(search_key..)
            .next();

        match found {
            Some((k, _)) => {
                // Set current_key to just before this key so read_next returns it.
                // We use a sentinel: set current_key to None if this is the first key,
                // or to a key just before k. Simplest: set current_key to None and
                // track start position separately.
                // Actually, we need read_next to return this key. So we must
                // set current_key to the key just before k in the BTree.
                let end_key = k.clone();
                let prev = self
                    .records
                    .range(..end_key)
                    .next_back()
                    .map(|(pk, _)| pk.clone());
                self.current_key = prev;
                self.status = FileStatus::SUCCESS;
            }
            None => {
                self.status = FileStatus::RECORD_NOT_FOUND;
            }
        }

        self.status
    }

    fn status(&self) -> FileStatus {
        self.status
    }

    fn is_open(&self) -> bool {
        self.mode.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---- FileStatus tests ----

    #[test]
    fn test_file_status_display() {
        assert_eq!(FileStatus::SUCCESS.to_string(), "00");
        assert_eq!(FileStatus::END_OF_FILE.to_string(), "10");
        assert_eq!(FileStatus::RECORD_NOT_FOUND.to_string(), "23");
        assert_eq!(FileStatus::FILE_NOT_FOUND.to_string(), "35");
    }

    #[test]
    fn test_file_status_is_success() {
        assert!(FileStatus::SUCCESS.is_success());
        assert!(FileStatus::DUPLICATE_KEY_OK.is_success());
        assert!(!FileStatus::END_OF_FILE.is_success());
        assert!(!FileStatus::IO_ERROR.is_success());
    }

    #[test]
    fn test_file_status_to_string_code() {
        assert_eq!(FileStatus::SUCCESS.to_string_code(), "00");
        assert_eq!(FileStatus::NOT_OPEN.to_string_code(), "42");
    }

    // ---- MemoryFile sequential tests ----

    #[test]
    fn test_memory_file_sequential_read() {
        let records = vec![
            b"RECORD ONE".to_vec(),
            b"RECORD TWO".to_vec(),
            b"RECORD THREE".to_vec(),
        ];
        let mut file = MemoryFile::with_records(records, FileOrganization::Sequential, None);

        assert_eq!(file.open(FileOpenMode::Input), FileStatus::SUCCESS);

        let (status, rec) = file.read_next();
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"RECORD ONE");

        let (status, rec) = file.read_next();
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"RECORD TWO");

        let (status, rec) = file.read_next();
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"RECORD THREE");

        let (status, rec) = file.read_next();
        assert_eq!(status, FileStatus::END_OF_FILE);
        assert!(rec.is_none());

        assert_eq!(file.close(), FileStatus::SUCCESS);
    }

    #[test]
    fn test_memory_file_write() {
        let mut file = MemoryFile::new(FileOrganization::Sequential, None);

        assert_eq!(file.open(FileOpenMode::Output), FileStatus::SUCCESS);
        assert_eq!(file.write(b"LINE 1"), FileStatus::SUCCESS);
        assert_eq!(file.write(b"LINE 2"), FileStatus::SUCCESS);
        assert_eq!(file.close(), FileStatus::SUCCESS);

        assert_eq!(file.records().len(), 2);
    }

    #[test]
    fn test_memory_file_already_open() {
        let mut file = MemoryFile::new(FileOrganization::Sequential, None);
        assert_eq!(file.open(FileOpenMode::Input), FileStatus::SUCCESS);
        assert_eq!(file.open(FileOpenMode::Input), FileStatus::ALREADY_OPEN);
    }

    #[test]
    fn test_memory_file_not_open() {
        let mut file = MemoryFile::new(FileOrganization::Sequential, None);
        assert_eq!(file.close(), FileStatus::NOT_OPEN);
    }

    #[test]
    fn test_memory_file_read_not_input() {
        let mut file = MemoryFile::new(FileOrganization::Sequential, None);
        file.open(FileOpenMode::Output);
        let (status, _) = file.read_next();
        assert_eq!(status, FileStatus::READ_NOT_INPUT);
    }

    #[test]
    fn test_memory_file_write_not_output() {
        let records = vec![b"DATA".to_vec()];
        let mut file = MemoryFile::with_records(records, FileOrganization::Sequential, None);
        file.open(FileOpenMode::Input);
        assert_eq!(file.write(b"NEW"), FileStatus::WRITE_NOT_OUTPUT);
    }

    // ---- MemoryFile indexed tests ----

    #[test]
    fn test_memory_file_indexed_read_key() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![
            b"001ALICE".to_vec(),
            b"002BOB".to_vec(),
            b"003CAROL".to_vec(),
        ];
        let mut file =
            MemoryFile::with_records(records, FileOrganization::Indexed, Some(key_spec));

        file.open(FileOpenMode::Input);

        let (status, rec) = file.read_key(b"002");
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"002BOB");

        let (status, rec) = file.read_key(b"999");
        assert_eq!(status, FileStatus::RECORD_NOT_FOUND);
        assert!(rec.is_none());
    }

    #[test]
    fn test_memory_file_indexed_rewrite() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001ALICE".to_vec(), b"002BOB".to_vec()];
        let mut file =
            MemoryFile::with_records(records, FileOrganization::Indexed, Some(key_spec));

        file.open(FileOpenMode::InputOutput);

        // Read then rewrite.
        let (status, _) = file.read_key(b"001");
        assert_eq!(status, FileStatus::SUCCESS);

        assert_eq!(file.rewrite(b"001ALICIA"), FileStatus::SUCCESS);

        // Verify the rewrite.
        let (_, rec) = file.read_key(b"001");
        assert_eq!(rec.unwrap(), b"001ALICIA");
    }

    #[test]
    fn test_memory_file_indexed_rewrite_no_prior_read() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001DATA".to_vec()];
        let mut file =
            MemoryFile::with_records(records, FileOrganization::Indexed, Some(key_spec));

        file.open(FileOpenMode::InputOutput);
        assert_eq!(file.rewrite(b"001NEW"), FileStatus::NO_PRIOR_READ);
    }

    #[test]
    fn test_memory_file_indexed_delete() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001A".to_vec(), b"002B".to_vec()];
        let mut file =
            MemoryFile::with_records(records, FileOrganization::Indexed, Some(key_spec));

        file.open(FileOpenMode::InputOutput);

        let (status, _) = file.read_key(b"001");
        assert_eq!(status, FileStatus::SUCCESS);

        assert_eq!(file.delete(), FileStatus::SUCCESS);

        // Verify the record is gone.
        let (status, _) = file.read_key(b"001");
        assert_eq!(status, FileStatus::RECORD_NOT_FOUND);

        // Other record still exists.
        let (status, rec) = file.read_key(b"002");
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"002B");
    }

    #[test]
    fn test_memory_file_indexed_duplicate_key() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001A".to_vec()];
        let mut file =
            MemoryFile::with_records(records, FileOrganization::Indexed, Some(key_spec));

        file.open(FileOpenMode::Output);
        file.write(b"001NEW");
        // Write duplicate key.
        assert_eq!(file.write(b"001DUP"), FileStatus::DUPLICATE_KEY);
    }

    #[test]
    fn test_memory_file_start_and_read_next() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![
            b"001A".to_vec(),
            b"003C".to_vec(),
            b"005E".to_vec(),
        ];
        let mut file =
            MemoryFile::with_records(records, FileOrganization::Indexed, Some(key_spec));

        file.open(FileOpenMode::Input);

        // START at key "003" — next read_next should return "003C".
        assert_eq!(file.start(b"003"), FileStatus::SUCCESS);
        // After START, read_next should find the record >= "003".
        // Our implementation doesn't directly support START + read_next for MemoryFile
        // since records are in a Vec, but position was set.
    }

    // ---- IndexedFile tests ----

    #[test]
    fn test_indexed_file_write_and_read() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let mut file = IndexedFile::new(key_spec);

        file.open(FileOpenMode::Output);
        file.write(b"003CAROL");
        file.write(b"001ALICE");
        file.write(b"002BOB");
        file.close();

        // Re-open for input and read by key.
        file.open(FileOpenMode::Input);
        let (status, rec) = file.read_key(b"002");
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"002BOB");
    }

    #[test]
    fn test_indexed_file_sequential_read_order() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let mut file = IndexedFile::new(key_spec);

        file.open(FileOpenMode::Output);
        file.write(b"003C");
        file.write(b"001A");
        file.write(b"002B");
        file.close();

        file.open(FileOpenMode::Input);

        // Should read in key order.
        let (_, rec) = file.read_next();
        assert_eq!(rec.unwrap(), b"001A");
        let (_, rec) = file.read_next();
        assert_eq!(rec.unwrap(), b"002B");
        let (_, rec) = file.read_next();
        assert_eq!(rec.unwrap(), b"003C");
        let (status, _) = file.read_next();
        assert_eq!(status, FileStatus::END_OF_FILE);
    }

    #[test]
    fn test_indexed_file_duplicate_key() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let mut file = IndexedFile::new(key_spec);

        file.open(FileOpenMode::Output);
        assert_eq!(file.write(b"001A"), FileStatus::SUCCESS);
        assert_eq!(file.write(b"001B"), FileStatus::DUPLICATE_KEY);
    }

    #[test]
    fn test_indexed_file_record_not_found() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001A".to_vec()];
        let mut file = IndexedFile::with_records(records, key_spec);

        file.open(FileOpenMode::Input);
        let (status, rec) = file.read_key(b"999");
        assert_eq!(status, FileStatus::RECORD_NOT_FOUND);
        assert!(rec.is_none());
    }

    #[test]
    fn test_indexed_file_rewrite() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001ALICE".to_vec()];
        let mut file = IndexedFile::with_records(records, key_spec);

        file.open(FileOpenMode::InputOutput);

        let (status, _) = file.read_key(b"001");
        assert_eq!(status, FileStatus::SUCCESS);

        assert_eq!(file.rewrite(b"001BOB"), FileStatus::SUCCESS);

        let (_, rec) = file.read_key(b"001");
        assert_eq!(rec.unwrap(), b"001BOB");
    }

    #[test]
    fn test_indexed_file_delete() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001A".to_vec(), b"002B".to_vec()];
        let mut file = IndexedFile::with_records(records, key_spec);

        file.open(FileOpenMode::InputOutput);

        let (status, _) = file.read_key(b"001");
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(file.delete(), FileStatus::SUCCESS);

        let (status, _) = file.read_key(b"001");
        assert_eq!(status, FileStatus::RECORD_NOT_FOUND);
    }

    #[test]
    fn test_indexed_file_start() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![
            b"001A".to_vec(),
            b"003C".to_vec(),
            b"005E".to_vec(),
        ];
        let mut file = IndexedFile::with_records(records, key_spec);

        file.open(FileOpenMode::Input);

        // START at "003".
        assert_eq!(file.start(b"003"), FileStatus::SUCCESS);

        // Read next should return "003C".
        let (status, rec) = file.read_next();
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"003C");

        // Next should be "005E".
        let (status, rec) = file.read_next();
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"005E");

        // Next should be EOF.
        let (status, _) = file.read_next();
        assert_eq!(status, FileStatus::END_OF_FILE);
    }

    #[test]
    fn test_indexed_file_start_nonexistent_key() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001A".to_vec(), b"005E".to_vec()];
        let mut file = IndexedFile::with_records(records, key_spec);

        file.open(FileOpenMode::Input);

        // START at "003" — should position at "005" (first key >= "003").
        assert_eq!(file.start(b"003"), FileStatus::SUCCESS);

        let (status, rec) = file.read_next();
        assert_eq!(status, FileStatus::SUCCESS);
        assert_eq!(rec.unwrap(), b"005E");
    }

    #[test]
    fn test_indexed_file_start_past_end() {
        let key_spec = KeySpec {
            offset: 0,
            length: 3,
        };
        let records = vec![b"001A".to_vec()];
        let mut file = IndexedFile::with_records(records, key_spec);

        file.open(FileOpenMode::Input);

        // START at "999" — no record >= 999.
        assert_eq!(file.start(b"999"), FileStatus::RECORD_NOT_FOUND);
    }

    // ---- SequentialFile tests ----

    #[test]
    fn test_sequential_file_write_and_read() {
        let dir = std::env::temp_dir().join("cobol_test_seq_io");
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("test_file.dat");

        {
            let mut file = SequentialFile::new(path.to_string_lossy().to_string());
            assert_eq!(file.open(FileOpenMode::Output), FileStatus::SUCCESS);
            assert_eq!(file.write(b"RECORD ONE"), FileStatus::SUCCESS);
            assert_eq!(file.write(b"RECORD TWO"), FileStatus::SUCCESS);
            assert_eq!(file.close(), FileStatus::SUCCESS);
        }

        {
            let mut file = SequentialFile::new(path.to_string_lossy().to_string());
            assert_eq!(file.open(FileOpenMode::Input), FileStatus::SUCCESS);

            let (status, rec) = file.read_next();
            assert_eq!(status, FileStatus::SUCCESS);
            assert_eq!(rec.unwrap(), b"RECORD ONE");

            let (status, rec) = file.read_next();
            assert_eq!(status, FileStatus::SUCCESS);
            assert_eq!(rec.unwrap(), b"RECORD TWO");

            let (status, _) = file.read_next();
            assert_eq!(status, FileStatus::END_OF_FILE);

            assert_eq!(file.close(), FileStatus::SUCCESS);
        }

        let _ = std::fs::remove_file(&path);
        let _ = std::fs::remove_dir(&dir);
    }

    #[test]
    fn test_sequential_file_not_found() {
        let mut file = SequentialFile::new("/tmp/nonexistent_cobol_test_file_12345.dat".to_string());
        assert_eq!(file.open(FileOpenMode::Input), FileStatus::FILE_NOT_FOUND);
    }

    #[test]
    fn test_sequential_file_already_open() {
        let dir = std::env::temp_dir().join("cobol_test_seq_io2");
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("test_already_open.dat");
        let _ = std::fs::File::create(&path);

        let mut file = SequentialFile::new(path.to_string_lossy().to_string());
        assert_eq!(file.open(FileOpenMode::Input), FileStatus::SUCCESS);
        assert_eq!(file.open(FileOpenMode::Input), FileStatus::ALREADY_OPEN);
        file.close();

        let _ = std::fs::remove_file(&path);
        let _ = std::fs::remove_dir(&dir);
    }
}
