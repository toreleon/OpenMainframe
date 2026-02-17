//! VSAM Linear Data Set (LDS) support.
//!
//! LDS provides page-level I/O for subsystem backing stores (e.g., DB2 tablespaces).
//! Pages are 4096 bytes, aligned on 4096-byte boundaries within the file.
//!
//! # Example
//!
//! ```no_run
//! use open_mainframe_dataset::vsam::lds::Lds;
//!
//! let mut lds = Lds::create("/tmp/my.lds", 100).unwrap();
//! let data = vec![0xABu8; 4096];
//! lds.write_page(0, &data).unwrap();
//! let page = lds.read_page(0).unwrap();
//! assert_eq!(page[0], 0xAB);
//! ```

use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

use crate::error::DatasetError;

/// LDS page size (4096 bytes, standard z/OS page).
pub const LDS_PAGE_SIZE: usize = 4096;

/// LDS file header size (one page reserved for metadata).
const LDS_HEADER_SIZE: usize = LDS_PAGE_SIZE;

/// Magic bytes identifying an LDS file.
const LDS_MAGIC: [u8; 4] = *b"LDS\0";

/// VSAM Linear Data Set.
///
/// Provides page-aligned I/O in 4096-byte pages. Used by subsystems
/// like DB2 for tablespace storage.
pub struct Lds {
    /// File path.
    path: PathBuf,
    /// Open file handle (when opened).
    file: Option<File>,
    /// Number of data pages currently allocated.
    page_count: u64,
}

impl Lds {
    /// Create a new LDS file with the given initial page capacity.
    ///
    /// The file is initialized with a header page followed by `initial_pages`
    /// zero-filled data pages.
    pub fn create(path: impl AsRef<Path>, initial_pages: u64) -> Result<Self, DatasetError> {
        let path = path.as_ref().to_path_buf();

        // Create parent directories if needed
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| DatasetError::IoError {
                message: format!("create LDS directory: {e}"),
            })?;
        }

        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&path)
            .map_err(|e| DatasetError::IoError {
                message: format!("create LDS file: {e}"),
            })?;

        // Write header page
        let mut header = vec![0u8; LDS_HEADER_SIZE];
        header[0..4].copy_from_slice(&LDS_MAGIC);
        header[4..12].copy_from_slice(&initial_pages.to_le_bytes());
        file.write_all(&header).map_err(|e| DatasetError::IoError {
            message: format!("write LDS header: {e}"),
        })?;

        // Write initial zero-filled pages
        let zero_page = vec![0u8; LDS_PAGE_SIZE];
        for _ in 0..initial_pages {
            file.write_all(&zero_page).map_err(|e| DatasetError::IoError {
                message: format!("write LDS initial page: {e}"),
            })?;
        }

        file.flush().map_err(|e| DatasetError::IoError {
            message: format!("flush LDS: {e}"),
        })?;

        Ok(Self {
            path,
            file: Some(file),
            page_count: initial_pages,
        })
    }

    /// Open an existing LDS file.
    pub fn open(path: impl AsRef<Path>) -> Result<Self, DatasetError> {
        let path = path.as_ref().to_path_buf();

        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(&path)
            .map_err(|e| DatasetError::IoError {
                message: format!("open LDS file: {e}"),
            })?;

        // Read and validate header
        let mut header = vec![0u8; LDS_HEADER_SIZE];
        file.read_exact(&mut header)
            .map_err(|e| DatasetError::IoError {
                message: format!("read LDS header: {e}"),
            })?;

        if header[0..4] != LDS_MAGIC {
            return Err(DatasetError::InvalidRecordFormat(
                "Not a valid LDS file (bad magic)".to_string(),
            ));
        }

        let mut page_count_bytes = [0u8; 8];
        page_count_bytes.copy_from_slice(&header[4..12]);
        let page_count = u64::from_le_bytes(page_count_bytes);

        Ok(Self {
            path,
            file: Some(file),
            page_count,
        })
    }

    /// Read a page from the LDS.
    ///
    /// Returns exactly `LDS_PAGE_SIZE` (4096) bytes for the given page number.
    pub fn read_page(&mut self, page_number: u64) -> Result<Vec<u8>, DatasetError> {
        if page_number >= self.page_count {
            return Err(DatasetError::InvalidParameter(format!(
                "Page {} out of range (LDS has {} pages)",
                page_number, self.page_count
            )));
        }

        let file = self.file.as_mut().ok_or_else(|| DatasetError::IoError {
            message: "LDS not open".to_string(),
        })?;

        let offset = LDS_HEADER_SIZE as u64 + page_number * LDS_PAGE_SIZE as u64;
        file.seek(SeekFrom::Start(offset))
            .map_err(|e| DatasetError::IoError {
                message: format!("seek for page read: {e}"),
            })?;

        let mut buf = vec![0u8; LDS_PAGE_SIZE];
        file.read_exact(&mut buf)
            .map_err(|e| DatasetError::IoError {
                message: format!("read page data: {e}"),
            })?;

        Ok(buf)
    }

    /// Write a page to the LDS.
    ///
    /// `data` must be exactly `LDS_PAGE_SIZE` (4096) bytes.
    /// If `page_number` is beyond the current extent, the LDS is extended.
    pub fn write_page(&mut self, page_number: u64, data: &[u8]) -> Result<(), DatasetError> {
        if data.len() != LDS_PAGE_SIZE {
            return Err(DatasetError::InvalidParameter(format!(
                "Page data must be exactly {} bytes, got {}",
                LDS_PAGE_SIZE,
                data.len()
            )));
        }

        // Extend if necessary
        if page_number >= self.page_count {
            self.extend_to(page_number + 1)?;
        }

        let file = self.file.as_mut().ok_or_else(|| DatasetError::IoError {
            message: "LDS not open".to_string(),
        })?;

        let offset = LDS_HEADER_SIZE as u64 + page_number * LDS_PAGE_SIZE as u64;
        file.seek(SeekFrom::Start(offset))
            .map_err(|e| DatasetError::IoError {
                message: format!("seek for page write: {e}"),
            })?;

        file.write_all(data).map_err(|e| DatasetError::IoError {
            message: format!("write page data: {e}"),
        })?;

        file.flush().map_err(|e| DatasetError::IoError {
            message: format!("flush after page write: {e}"),
        })?;

        Ok(())
    }

    /// Extend the LDS to hold at least `new_page_count` pages.
    ///
    /// Intermediate pages are zero-filled.
    fn extend_to(&mut self, new_page_count: u64) -> Result<(), DatasetError> {
        if new_page_count <= self.page_count {
            return Ok(());
        }

        let file = self.file.as_mut().ok_or_else(|| DatasetError::IoError {
            message: "LDS not open".to_string(),
        })?;

        // Seek to end of current data
        let current_end = LDS_HEADER_SIZE as u64 + self.page_count * LDS_PAGE_SIZE as u64;
        file.seek(SeekFrom::Start(current_end))
            .map_err(|e| DatasetError::IoError {
                message: format!("seek to end for extend: {e}"),
            })?;

        // Write zero-filled pages for the gap
        let zero_page = vec![0u8; LDS_PAGE_SIZE];
        let pages_to_add = new_page_count - self.page_count;
        for _ in 0..pages_to_add {
            file.write_all(&zero_page)
                .map_err(|e| DatasetError::IoError {
                    message: format!("write extension page: {e}"),
                })?;
        }

        // Update header with new page count
        file.seek(SeekFrom::Start(4))
            .map_err(|e| DatasetError::IoError {
                message: format!("seek to header for update: {e}"),
            })?;
        file.write_all(&new_page_count.to_le_bytes())
            .map_err(|e| DatasetError::IoError {
                message: format!("update header page count: {e}"),
            })?;

        file.flush().map_err(|e| DatasetError::IoError {
            message: format!("flush after extend: {e}"),
        })?;

        self.page_count = new_page_count;
        Ok(())
    }

    /// Return the number of data pages in this LDS.
    pub fn page_count(&self) -> u64 {
        self.page_count
    }

    /// Return the file path.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Close the LDS file.
    pub fn close(&mut self) {
        self.file = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static LDS_TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn temp_lds_path() -> PathBuf {
        let n = LDS_TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("open_mf_lds_test_{}.lds", n))
    }

    fn cleanup(path: &Path) {
        let _ = std::fs::remove_file(path);
    }

    /// Story 602.1: Create and page I/O.
    #[test]
    fn test_lds_create_and_read_write() {
        let path = temp_lds_path();
        let mut lds = Lds::create(&path, 10).unwrap();
        assert_eq!(lds.page_count(), 10);

        // Write a page
        let mut data = vec![0u8; LDS_PAGE_SIZE];
        data[0] = 0xAB;
        data[4095] = 0xCD;
        lds.write_page(0, &data).unwrap();

        // Read it back
        let page = lds.read_page(0).unwrap();
        assert_eq!(page[0], 0xAB);
        assert_eq!(page[4095], 0xCD);

        // Read an unwritten page (should be zero)
        let page5 = lds.read_page(5).unwrap();
        assert!(page5.iter().all(|&b| b == 0));

        cleanup(&path);
    }

    /// Story 602.1: Read/write at arbitrary page offsets.
    #[test]
    fn test_lds_read_page_50() {
        let path = temp_lds_path();
        let mut lds = Lds::create(&path, 100).unwrap();

        let mut data = vec![0x42u8; LDS_PAGE_SIZE];
        data[0] = 0xFF;
        lds.write_page(50, &data).unwrap();

        let page = lds.read_page(50).unwrap();
        assert_eq!(page[0], 0xFF);
        assert_eq!(page[1], 0x42);

        cleanup(&path);
    }

    /// Story 602.1: Open existing LDS and read.
    #[test]
    fn test_lds_open_existing() {
        let path = temp_lds_path();

        // Create and write
        {
            let mut lds = Lds::create(&path, 5).unwrap();
            let data = vec![0x11u8; LDS_PAGE_SIZE];
            lds.write_page(2, &data).unwrap();
        }

        // Reopen and read
        let mut lds = Lds::open(&path).unwrap();
        assert_eq!(lds.page_count(), 5);
        let page = lds.read_page(2).unwrap();
        assert_eq!(page[0], 0x11);

        cleanup(&path);
    }

    /// Story 602.1: Invalid page size produces error.
    #[test]
    fn test_lds_invalid_page_size() {
        let path = temp_lds_path();
        let mut lds = Lds::create(&path, 1).unwrap();

        let bad_data = vec![0u8; 100]; // Not 4096
        let result = lds.write_page(0, &bad_data);
        assert!(result.is_err());

        cleanup(&path);
    }

    /// Story 602.2: LDS extends dynamically when writing beyond current size.
    #[test]
    fn test_lds_dynamic_extend() {
        let path = temp_lds_path();
        let mut lds = Lds::create(&path, 10).unwrap();
        assert_eq!(lds.page_count(), 10);

        // Write to page 20 (beyond current size of 10)
        let data = vec![0xBBu8; LDS_PAGE_SIZE];
        lds.write_page(20, &data).unwrap();

        // LDS should have extended to at least 21 pages
        assert!(lds.page_count() >= 21, "page_count={}", lds.page_count());

        // Read back
        let page = lds.read_page(20).unwrap();
        assert_eq!(page[0], 0xBB);

        // Intermediate pages should be zero-filled
        let page15 = lds.read_page(15).unwrap();
        assert!(page15.iter().all(|&b| b == 0));

        cleanup(&path);
    }

    /// Story 602.2: Extend preserves existing data.
    #[test]
    fn test_lds_extend_preserves_data() {
        let path = temp_lds_path();
        let mut lds = Lds::create(&path, 5).unwrap();

        // Write to page 2
        let data = vec![0xAAu8; LDS_PAGE_SIZE];
        lds.write_page(2, &data).unwrap();

        // Extend by writing to page 10
        let data2 = vec![0xBBu8; LDS_PAGE_SIZE];
        lds.write_page(10, &data2).unwrap();

        // Original page 2 data should still be there
        let page2 = lds.read_page(2).unwrap();
        assert_eq!(page2[0], 0xAA);

        cleanup(&path);
    }

    /// Story 602.1: Out of range page read produces error.
    #[test]
    fn test_lds_read_out_of_range() {
        let path = temp_lds_path();
        let mut lds = Lds::create(&path, 5).unwrap();

        let result = lds.read_page(10);
        assert!(result.is_err());

        cleanup(&path);
    }
}
