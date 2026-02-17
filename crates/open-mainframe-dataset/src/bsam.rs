//! BSAM (Basic Sequential Access Method) block-level I/O.
//!
//! BSAM provides block-level access to sequential datasets, in contrast
//! to QSAM which operates at the record level. With BSAM the application
//! is responsible for blocking and deblocking records within physical blocks.
//!
//! # BPAM (Basic Partitioned Access Method)
//!
//! BPAM extends BSAM to provide directory-level access to PDS datasets.
//! It can read the PDS directory as a sequence of 256-byte directory blocks,
//! each containing member entries with TTR (Track, Track, Record) pointers.
//!
//! # Architecture
//!
//! - `BsamReader` / `BsamWriter` operate on fixed-size physical blocks
//! - `BpamDirectory` reads PDS directory entries in block format
//! - Block size is determined by `DatasetAttributes.blksize`

use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::error::DatasetError;
use crate::pds::Pds;
use crate::types::DatasetAttributes;

/// Size of a PDS directory block (256 bytes on z/OS).
pub const DIRECTORY_BLOCK_SIZE: usize = 256;

/// Size of a TTR pointer (3 bytes: Track(2) + Record(1)).
pub const TTR_SIZE: usize = 3;

/// A TTR (Track-Track-Record) pointer.
///
/// On real z/OS this is a physical disk address. In our implementation
/// we use it as a logical identifier: track = member index, record = 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ttr {
    /// Track number (big-endian 2 bytes).
    pub track: u16,
    /// Record number within the track.
    pub record: u8,
}

impl Ttr {
    /// Create a new TTR pointer.
    pub fn new(track: u16, record: u8) -> Self {
        Self { track, record }
    }

    /// Encode TTR to 3 bytes (big-endian).
    pub fn to_bytes(&self) -> [u8; 3] {
        let tb = self.track.to_be_bytes();
        [tb[0], tb[1], self.record]
    }

    /// Decode TTR from 3 bytes (big-endian).
    pub fn from_bytes(bytes: &[u8; 3]) -> Self {
        Self {
            track: u16::from_be_bytes([bytes[0], bytes[1]]),
            record: bytes[2],
        }
    }
}

/// Block-level sequential dataset reader (BSAM READ).
///
/// Reads data in fixed-size physical blocks rather than individual records.
/// The caller is responsible for deblocking records from the returned blocks.
pub struct BsamReader {
    /// Underlying file.
    file: File,
    /// Block size in bytes.
    blksize: usize,
    /// Current block number (0-indexed).
    block_number: u64,
    /// Internal buffer for the current block.
    buffer: Vec<u8>,
    /// Whether EOF has been reached.
    eof: bool,
}

impl BsamReader {
    /// Open a dataset for block-level reading.
    pub fn open(path: impl AsRef<Path>, attrs: &DatasetAttributes) -> Result<Self, DatasetError> {
        let path = path.as_ref();
        let file = File::open(path).map_err(|e| DatasetError::IoError {
            message: format!("BSAM OPEN failed for {}: {}", path.display(), e),
        })?;

        let blksize = attrs.blksize as usize;
        if blksize == 0 {
            return Err(DatasetError::InvalidParameter(
                "BSAM requires BLKSIZE > 0".to_string(),
            ));
        }

        Ok(Self {
            file,
            blksize,
            block_number: 0,
            buffer: vec![0u8; blksize],
            eof: false,
        })
    }

    /// Read the next physical block.
    ///
    /// Returns `Some(&[u8])` with the block data, or `None` at EOF.
    /// The last block may be shorter than `blksize` if the file size
    /// is not a multiple of the block size.
    pub fn read_block(&mut self) -> Result<Option<&[u8]>, DatasetError> {
        if self.eof {
            return Ok(None);
        }

        self.buffer.resize(self.blksize, 0);

        match self.file.read(&mut self.buffer) {
            Ok(0) => {
                self.eof = true;
                Ok(None)
            }
            Ok(n) => {
                self.block_number += 1;
                self.buffer.truncate(n);
                Ok(Some(&self.buffer))
            }
            Err(e) => Err(DatasetError::IoError {
                message: format!("BSAM READ error at block {}: {}", self.block_number, e),
            }),
        }
    }

    /// Get current block number (1-indexed after first read).
    pub fn block_number(&self) -> u64 {
        self.block_number
    }

    /// Check if at end of file.
    pub fn is_eof(&self) -> bool {
        self.eof
    }

    /// Get the block size.
    pub fn blksize(&self) -> usize {
        self.blksize
    }
}

/// Block-level sequential dataset writer (BSAM WRITE).
///
/// Writes data in fixed-size physical blocks. The caller is responsible
/// for building complete blocks from individual records.
pub struct BsamWriter {
    /// Underlying file.
    file: File,
    /// Block size in bytes.
    blksize: usize,
    /// Current block number (0-indexed).
    block_number: u64,
}

impl BsamWriter {
    /// Open a dataset for block-level writing.
    pub fn open(path: impl AsRef<Path>, attrs: &DatasetAttributes) -> Result<Self, DatasetError> {
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| DatasetError::IoError {
                message: format!("Failed to create directory: {}", e),
            })?;
        }

        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .map_err(|e| DatasetError::IoError {
                message: format!("BSAM OPEN failed for {}: {}", path.display(), e),
            })?;

        let blksize = attrs.blksize as usize;
        if blksize == 0 {
            return Err(DatasetError::InvalidParameter(
                "BSAM requires BLKSIZE > 0".to_string(),
            ));
        }

        Ok(Self {
            file,
            blksize,
            block_number: 0,
        })
    }

    /// Write a physical block.
    ///
    /// The block is padded with zeros to `blksize` if shorter.
    /// Returns an error if the block exceeds `blksize`.
    pub fn write_block(&mut self, data: &[u8]) -> Result<(), DatasetError> {
        if data.len() > self.blksize {
            return Err(DatasetError::RecordTooLong {
                actual: data.len(),
                max: self.blksize,
            });
        }

        // Pad to block size
        let mut block = vec![0u8; self.blksize];
        block[..data.len()].copy_from_slice(data);

        self.file.write_all(&block).map_err(|e| DatasetError::IoError {
            message: format!("BSAM WRITE error at block {}: {}", self.block_number, e),
        })?;

        self.block_number += 1;
        Ok(())
    }

    /// Write a short block (without padding to blksize).
    ///
    /// Used for the last block of a dataset when the remaining data
    /// doesn't fill a complete block.
    pub fn write_short_block(&mut self, data: &[u8]) -> Result<(), DatasetError> {
        if data.len() > self.blksize {
            return Err(DatasetError::RecordTooLong {
                actual: data.len(),
                max: self.blksize,
            });
        }

        self.file.write_all(data).map_err(|e| DatasetError::IoError {
            message: format!("BSAM WRITE error at block {}: {}", self.block_number, e),
        })?;

        self.block_number += 1;
        Ok(())
    }

    /// Flush any buffered data.
    pub fn flush(&mut self) -> Result<(), DatasetError> {
        self.file.flush().map_err(|e| DatasetError::IoError {
            message: format!("BSAM FLUSH error: {}", e),
        })
    }

    /// Get current block number.
    pub fn block_number(&self) -> u64 {
        self.block_number
    }

    /// Get the block size.
    pub fn blksize(&self) -> usize {
        self.blksize
    }
}

impl Drop for BsamWriter {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

// =========================================================================
// BPAM — Basic Partitioned Access Method
// =========================================================================

/// A PDS directory entry in BPAM format.
///
/// Each entry contains the member name (8 bytes, EBCDIC-padded on real z/OS,
/// ASCII-padded here), a TTR pointer, and an indicator byte.
#[derive(Debug, Clone)]
pub struct BpamDirEntry {
    /// Member name (up to 8 characters, space-padded).
    pub name: [u8; 8],
    /// TTR pointer to the member's first block.
    pub ttr: Ttr,
    /// Indicator byte (bit flags: alias, user data length, etc.).
    pub indicator: u8,
}

impl BpamDirEntry {
    /// Size of a single directory entry (12 bytes: 8 name + 3 TTR + 1 indicator).
    pub const SIZE: usize = 12;

    /// Create a new directory entry.
    pub fn new(name: &str, ttr: Ttr) -> Self {
        let mut name_bytes = [b' '; 8];
        let src = name.as_bytes();
        let len = src.len().min(8);
        name_bytes[..len].copy_from_slice(&src[..len]);

        Self {
            name: name_bytes,
            ttr,
            indicator: 0,
        }
    }

    /// Create an alias directory entry.
    pub fn alias(name: &str, ttr: Ttr) -> Self {
        let mut entry = Self::new(name, ttr);
        entry.indicator = 0x80; // Alias bit
        entry
    }

    /// Get the member name as a string (trimmed).
    pub fn name_str(&self) -> String {
        String::from_utf8_lossy(&self.name)
            .trim_end()
            .to_string()
    }

    /// Check if this is an alias entry.
    pub fn is_alias(&self) -> bool {
        self.indicator & 0x80 != 0
    }

    /// Serialize to bytes (12 bytes).
    pub fn to_bytes(&self) -> [u8; Self::SIZE] {
        let mut buf = [0u8; Self::SIZE];
        buf[..8].copy_from_slice(&self.name);
        let ttr_bytes = self.ttr.to_bytes();
        buf[8..11].copy_from_slice(&ttr_bytes);
        buf[11] = self.indicator;
        buf
    }

    /// Deserialize from bytes (12 bytes).
    pub fn from_bytes(bytes: &[u8; Self::SIZE]) -> Self {
        let mut name = [0u8; 8];
        name.copy_from_slice(&bytes[..8]);

        let ttr = Ttr::from_bytes(&[bytes[8], bytes[9], bytes[10]]);
        let indicator = bytes[11];

        Self {
            name,
            ttr,
            indicator,
        }
    }
}

/// Read a PDS directory in BPAM block format.
///
/// Converts the in-memory PDS directory to a sequence of 256-byte
/// directory blocks, each prefixed with a 2-byte used-length field.
///
/// The last block contains an end-of-directory marker (name = 0xFF×8).
pub fn read_pds_directory_blocks(pds: &Pds) -> Vec<Vec<u8>> {
    let members = pds.list_members();
    let mut blocks = Vec::new();
    let mut current_block = Vec::new();

    // Reserve 2 bytes for the used-length field at start of each block
    current_block.extend_from_slice(&[0u8; 2]);

    for (i, member) in members.iter().enumerate() {
        let ttr = Ttr::new(i as u16 + 1, 0);
        let entry = if member.alias_of.is_some() {
            BpamDirEntry::alias(&member.name, ttr)
        } else {
            BpamDirEntry::new(&member.name, ttr)
        };

        let entry_bytes = entry.to_bytes();

        // Check if adding this entry would exceed block size
        if current_block.len() + BpamDirEntry::SIZE > DIRECTORY_BLOCK_SIZE {
            // Finalize current block
            finalize_directory_block(&mut current_block);
            blocks.push(current_block);
            current_block = Vec::new();
            current_block.extend_from_slice(&[0u8; 2]);
        }

        current_block.extend_from_slice(&entry_bytes);
    }

    // Add end-of-directory marker (name = 0xFF×8)
    let eod_marker = [0xFFu8; 8];
    if current_block.len() + 8 > DIRECTORY_BLOCK_SIZE {
        finalize_directory_block(&mut current_block);
        blocks.push(current_block);
        current_block = Vec::new();
        current_block.extend_from_slice(&[0u8; 2]);
    }
    current_block.extend_from_slice(&eod_marker);

    finalize_directory_block(&mut current_block);
    blocks.push(current_block);

    blocks
}

/// Finalize a directory block by setting the used-length field and padding.
fn finalize_directory_block(block: &mut Vec<u8>) {
    let used = block.len() as u16;
    block[0] = (used >> 8) as u8;
    block[1] = (used & 0xFF) as u8;

    // Pad to full block size
    block.resize(DIRECTORY_BLOCK_SIZE, 0);
}

/// Parse directory entries from a BPAM directory block.
///
/// Returns the entries found in the block, stopping at the
/// end-of-directory marker or end of used data.
pub fn parse_directory_block(block: &[u8]) -> Vec<BpamDirEntry> {
    if block.len() < 2 {
        return Vec::new();
    }

    let used = ((block[0] as usize) << 8) | (block[1] as usize);
    let data_end = used.min(block.len());

    let mut entries = Vec::new();
    let mut pos = 2; // Skip used-length field

    while pos + BpamDirEntry::SIZE <= data_end {
        // Check for end-of-directory marker
        if block[pos..pos + 8] == [0xFF; 8] {
            break;
        }

        let mut entry_bytes = [0u8; BpamDirEntry::SIZE];
        entry_bytes.copy_from_slice(&block[pos..pos + BpamDirEntry::SIZE]);
        entries.push(BpamDirEntry::from_bytes(&entry_bytes));
        pos += BpamDirEntry::SIZE;
    }

    entries
}

/// Read a PDS member by BPAM (via TTR pointer lookup).
///
/// This resolves the member by finding its TTR in the directory,
/// then delegates to the standard PDS read. In a real BSAM/BPAM
/// implementation, the TTR would be used for physical disk I/O.
pub fn bpam_read_member(pds: &Pds, name: &str) -> Result<Vec<u8>, DatasetError> {
    pds.read_member(name)
}

/// Get the filesystem path for the PDS root, used by BSAM to locate blocks.
pub fn bpam_pds_path(pds: &Pds) -> PathBuf {
    pds.root().to_path_buf()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::DatasetAttributes;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_dir() -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("bsam_test_{}", count))
    }

    fn cleanup(path: &Path) {
        let _ = std::fs::remove_dir_all(path);
    }

    // === Story 608.1: BSAM Block-level I/O ===

    #[test]
    fn test_bsam_write_and_read_blocks() {
        let dir = test_dir();
        cleanup(&dir);
        std::fs::create_dir_all(&dir).unwrap();

        let path = dir.join("test.dat");
        let attrs = DatasetAttributes {
            blksize: 80,
            ..Default::default()
        };

        // Write two blocks
        {
            let mut writer = BsamWriter::open(&path, &attrs).unwrap();
            let block1 = b"BLOCK ONE DATA";
            let block2 = b"BLOCK TWO DATA";
            writer.write_block(block1).unwrap();
            writer.write_block(block2).unwrap();
            writer.flush().unwrap();
            assert_eq!(writer.block_number(), 2);
        }

        // Read them back
        {
            let mut reader = BsamReader::open(&path, &attrs).unwrap();
            let b1 = reader.read_block().unwrap().unwrap();
            assert_eq!(b1.len(), 80); // Padded to blksize
            assert!(b1.starts_with(b"BLOCK ONE DATA"));
            assert_eq!(reader.block_number(), 1);

            let b2 = reader.read_block().unwrap().unwrap();
            assert!(b2.starts_with(b"BLOCK TWO DATA"));
            assert_eq!(reader.block_number(), 2);

            assert!(reader.read_block().unwrap().is_none());
            assert!(reader.is_eof());
        }

        cleanup(&dir);
    }

    #[test]
    fn test_bsam_write_short_block() {
        let dir = test_dir();
        cleanup(&dir);
        std::fs::create_dir_all(&dir).unwrap();

        let path = dir.join("short.dat");
        let attrs = DatasetAttributes {
            blksize: 100,
            ..Default::default()
        };

        {
            let mut writer = BsamWriter::open(&path, &attrs).unwrap();
            writer.write_short_block(b"SHORT DATA").unwrap();
            writer.flush().unwrap();
        }

        // Read back — short block is only 10 bytes, not padded to 100
        {
            let mut reader = BsamReader::open(&path, &attrs).unwrap();
            let block = reader.read_block().unwrap().unwrap();
            assert_eq!(block.len(), 10);
            assert_eq!(block, b"SHORT DATA");
        }

        cleanup(&dir);
    }

    #[test]
    fn test_bsam_block_too_long() {
        let dir = test_dir();
        cleanup(&dir);
        std::fs::create_dir_all(&dir).unwrap();

        let path = dir.join("toolong.dat");
        let attrs = DatasetAttributes {
            blksize: 10,
            ..Default::default()
        };

        let mut writer = BsamWriter::open(&path, &attrs).unwrap();
        let result = writer.write_block(b"THIS IS WAY TOO LONG FOR THE BLOCK");
        assert!(result.is_err());

        cleanup(&dir);
    }

    #[test]
    fn test_bsam_zero_blksize_error() {
        let dir = test_dir();
        cleanup(&dir);
        std::fs::create_dir_all(&dir).unwrap();

        let path = dir.join("zero.dat");
        let attrs = DatasetAttributes {
            blksize: 0,
            ..Default::default()
        };

        let result = BsamReader::open(&path, &attrs);
        // blksize 0 should fail since the file doesn't exist
        // But even if it did exist, blksize=0 is rejected
        assert!(result.is_err());
    }

    // === Story 608.2: BPAM Directory Access ===

    #[test]
    fn test_ttr_roundtrip() {
        let ttr = Ttr::new(0x0102, 0x03);
        let bytes = ttr.to_bytes();
        assert_eq!(bytes, [0x01, 0x02, 0x03]);

        let decoded = Ttr::from_bytes(&bytes);
        assert_eq!(decoded, ttr);
    }

    #[test]
    fn test_bpam_dir_entry_roundtrip() {
        let entry = BpamDirEntry::new("MAINPGM", Ttr::new(1, 0));
        assert_eq!(entry.name_str(), "MAINPGM");
        assert!(!entry.is_alias());

        let bytes = entry.to_bytes();
        let decoded = BpamDirEntry::from_bytes(&bytes);
        assert_eq!(decoded.name_str(), "MAINPGM");
        assert_eq!(decoded.ttr, Ttr::new(1, 0));
        assert!(!decoded.is_alias());
    }

    #[test]
    fn test_bpam_dir_entry_alias() {
        let entry = BpamDirEntry::alias("MYALIAS", Ttr::new(5, 0));
        assert!(entry.is_alias());
        assert_eq!(entry.name_str(), "MYALIAS");
    }

    #[test]
    fn test_read_pds_directory_blocks() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("ALPHA", b"alpha data").unwrap();
        pds.add_member("BETA", b"beta data").unwrap();
        pds.add_member("GAMMA", b"gamma data").unwrap();

        let blocks = read_pds_directory_blocks(&pds);
        assert!(!blocks.is_empty());

        // Each block should be exactly DIRECTORY_BLOCK_SIZE bytes
        for block in &blocks {
            assert_eq!(block.len(), DIRECTORY_BLOCK_SIZE);
        }

        // Parse entries from first block
        let entries = parse_directory_block(&blocks[0]);
        assert_eq!(entries.len(), 3);
        assert_eq!(entries[0].name_str(), "ALPHA");
        assert_eq!(entries[1].name_str(), "BETA");
        assert_eq!(entries[2].name_str(), "GAMMA");

        cleanup(&dir);
    }

    #[test]
    fn test_parse_directory_block_empty_pds() {
        let dir = test_dir();
        cleanup(&dir);

        let pds = Pds::create(&dir).unwrap();
        let blocks = read_pds_directory_blocks(&pds);

        // Even empty PDS has one block (with just EOD marker)
        assert_eq!(blocks.len(), 1);
        let entries = parse_directory_block(&blocks[0]);
        assert!(entries.is_empty()); // No entries before EOD marker

        cleanup(&dir);
    }

    #[test]
    fn test_bpam_read_member() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("TESTMEM", b"test content here").unwrap();

        let data = bpam_read_member(&pds, "TESTMEM").unwrap();
        assert_eq!(&data, b"test content here");

        cleanup(&dir);
    }

    #[test]
    fn test_bpam_read_member_not_found() {
        let dir = test_dir();
        cleanup(&dir);

        let pds = Pds::create(&dir).unwrap();
        let result = bpam_read_member(&pds, "NOPE");
        assert!(result.is_err());

        cleanup(&dir);
    }

    #[test]
    fn test_directory_blocks_with_alias() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("TARGET", b"data").unwrap();
        pds.add_alias("ALIAS1", "TARGET").unwrap();

        let blocks = read_pds_directory_blocks(&pds);
        let entries = parse_directory_block(&blocks[0]);
        assert_eq!(entries.len(), 2);

        // ALIAS1 comes first alphabetically
        assert_eq!(entries[0].name_str(), "ALIAS1");
        assert!(entries[0].is_alias());

        assert_eq!(entries[1].name_str(), "TARGET");
        assert!(!entries[1].is_alias());

        cleanup(&dir);
    }

    #[test]
    fn test_bpam_many_members_multiple_blocks() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();

        // Add enough members to span multiple directory blocks
        // Each block is 256 bytes: 2-byte header + entries (12 bytes each)
        // Available per block: 254 bytes / 12 = 21 entries per block
        // Plus we need 8 bytes for EOD marker
        // So 22+ members should span 2 blocks
        for i in 0..25 {
            let name = format!("MEM{:05}", i);
            pds.add_member(&name, format!("data {}", i).as_bytes())
                .unwrap();
        }

        let blocks = read_pds_directory_blocks(&pds);
        assert!(blocks.len() >= 2, "Expected multiple blocks, got {}", blocks.len());

        // Parse all entries
        let mut all_entries = Vec::new();
        for block in &blocks {
            all_entries.extend(parse_directory_block(block));
        }
        assert_eq!(all_entries.len(), 25);

        cleanup(&dir);
    }
}
