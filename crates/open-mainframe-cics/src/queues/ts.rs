//! Temporary Storage (TS) Queue implementation.
//!
//! TS queues provide temporary storage that can be shared between
//! transactions. Each queue has a name and contains numbered items.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Type of temporary storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TsType {
    /// Main storage - in-memory, faster but limited
    #[default]
    Main,
    /// Auxiliary storage - disk-backed, larger capacity
    Auxiliary,
}

/// A single item in a TS queue.
#[derive(Debug, Clone)]
pub struct TsItem {
    /// Item number (1-based)
    pub item_number: u32,
    /// Item data
    pub data: Vec<u8>,
    /// Original length
    pub length: usize,
}

impl TsItem {
    /// Create a new TS item.
    pub fn new(item_number: u32, data: Vec<u8>) -> Self {
        let length = data.len();
        Self {
            item_number,
            data,
            length,
        }
    }
}

/// A Temporary Storage queue.
#[derive(Debug, Clone)]
pub struct TsQueue {
    /// Queue name (up to 8 characters in CICS, we allow 16)
    pub name: String,
    /// Storage type
    pub queue_type: TsType,
    /// Items in the queue
    items: Vec<TsItem>,
    /// Next item number to assign
    next_item: u32,
    /// Current read position for NEXT operations
    current_position: u32,
}

impl TsQueue {
    /// Create a new TS queue.
    pub fn new(name: &str, queue_type: TsType) -> Self {
        Self {
            name: name.to_string(),
            queue_type,
            items: Vec::new(),
            next_item: 1,
            current_position: 0,
        }
    }

    /// Get number of items in queue.
    pub fn num_items(&self) -> u32 {
        self.items.len() as u32
    }

    /// Write an item to the queue.
    ///
    /// If item_number is None, appends to end.
    /// If item_number is Some and rewrite is true, replaces existing item.
    pub fn write(&mut self, data: Vec<u8>, item_number: Option<u32>, rewrite: bool) -> TsResult<u32> {
        match item_number {
            Some(num) if rewrite => {
                // Rewrite existing item
                if let Some(item) = self.items.iter_mut().find(|i| i.item_number == num) {
                    item.data = data.clone();
                    item.length = data.len();
                    Ok(num)
                } else {
                    Err(TsError::ItemNotFound(num))
                }
            }
            Some(num) => {
                // Write at specific position (insert)
                let item = TsItem::new(num, data);
                // Find insertion point
                let pos = self.items.iter().position(|i| i.item_number >= num);
                match pos {
                    Some(idx) if self.items[idx].item_number == num => {
                        Err(TsError::ItemExists(num))
                    }
                    Some(idx) => {
                        self.items.insert(idx, item);
                        if num >= self.next_item {
                            self.next_item = num + 1;
                        }
                        Ok(num)
                    }
                    None => {
                        self.items.push(item);
                        if num >= self.next_item {
                            self.next_item = num + 1;
                        }
                        Ok(num)
                    }
                }
            }
            None => {
                // Append to end
                let num = self.next_item;
                self.items.push(TsItem::new(num, data));
                self.next_item += 1;
                Ok(num)
            }
        }
    }

    /// Read an item from the queue.
    pub fn read(&self, item_number: u32) -> TsResult<&TsItem> {
        self.items
            .iter()
            .find(|i| i.item_number == item_number)
            .ok_or(TsError::ItemNotFound(item_number))
    }

    /// Read the next item in sequence.
    pub fn read_next(&mut self) -> TsResult<&TsItem> {
        self.current_position += 1;
        self.read(self.current_position)
    }

    /// Reset read position.
    pub fn reset_position(&mut self) {
        self.current_position = 0;
    }

    /// Delete a specific item.
    pub fn delete_item(&mut self, item_number: u32) -> TsResult<()> {
        let pos = self.items.iter().position(|i| i.item_number == item_number);
        match pos {
            Some(idx) => {
                self.items.remove(idx);
                Ok(())
            }
            None => Err(TsError::ItemNotFound(item_number)),
        }
    }

    /// Check if this queue uses auxiliary (disk-backed) storage.
    pub fn is_auxiliary(&self) -> bool {
        self.queue_type == TsType::Auxiliary
    }

    /// Serialize the queue to bytes for disk persistence.
    ///
    /// Format: `[next_item:u32][item_count:u32]([item_number:u32][data_len:u32][data])*`
    pub fn serialize(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        buf.extend_from_slice(&self.next_item.to_le_bytes());
        buf.extend_from_slice(&(self.items.len() as u32).to_le_bytes());
        for item in &self.items {
            buf.extend_from_slice(&item.item_number.to_le_bytes());
            buf.extend_from_slice(&(item.data.len() as u32).to_le_bytes());
            buf.extend_from_slice(&item.data);
        }
        buf
    }

    /// Deserialize a queue from bytes loaded from disk.
    pub fn deserialize(name: &str, data: &[u8]) -> TsResult<Self> {
        if data.len() < 8 {
            return Err(TsError::StorageError("corrupt queue file: too short".to_string()));
        }
        let next_item = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        let item_count = u32::from_le_bytes([data[4], data[5], data[6], data[7]]) as usize;

        let mut offset = 8;
        let mut items = Vec::with_capacity(item_count);
        for _ in 0..item_count {
            if offset + 8 > data.len() {
                return Err(TsError::StorageError("corrupt queue file: truncated item header".to_string()));
            }
            let item_number = u32::from_le_bytes([
                data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
            ]);
            let data_len = u32::from_le_bytes([
                data[offset + 4], data[offset + 5], data[offset + 6], data[offset + 7],
            ]) as usize;
            offset += 8;
            if offset + data_len > data.len() {
                return Err(TsError::StorageError("corrupt queue file: truncated item data".to_string()));
            }
            let item_data = data[offset..offset + data_len].to_vec();
            offset += data_len;
            items.push(TsItem::new(item_number, item_data));
        }

        Ok(Self {
            name: name.to_string(),
            queue_type: TsType::Auxiliary,
            items,
            next_item,
            current_position: 0,
        })
    }

    /// Persist this queue to a file in the given directory.
    pub fn persist(&self, dir: &Path) -> TsResult<()> {
        let path = dir.join(self.storage_filename());
        let data = self.serialize();
        std::fs::write(&path, data)
            .map_err(|e| TsError::StorageError(format!("write {}: {}", path.display(), e)))
    }

    /// Load a queue from a file in the given directory.
    pub fn load(name: &str, dir: &Path) -> TsResult<Self> {
        let filename = sanitize_queue_name(name);
        let path = dir.join(format!("{filename}.tsq"));
        let data = std::fs::read(&path)
            .map_err(|e| TsError::StorageError(format!("read {}: {}", path.display(), e)))?;
        Self::deserialize(name, &data)
    }

    /// Remove the persisted file for this queue.
    pub fn remove_persisted(&self, dir: &Path) -> TsResult<()> {
        let path = dir.join(self.storage_filename());
        if path.exists() {
            std::fs::remove_file(&path)
                .map_err(|e| TsError::StorageError(format!("remove {}: {}", path.display(), e)))?;
        }
        Ok(())
    }

    /// Get the storage filename for this queue.
    fn storage_filename(&self) -> String {
        let sanitized = sanitize_queue_name(&self.name);
        format!("{sanitized}.tsq")
    }
}

/// Sanitize a queue name for use as a filename.
fn sanitize_queue_name(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_alphanumeric() || c == '-' || c == '_' { c } else { '_' })
        .collect()
}

/// Temporary Storage Queue Manager.
///
/// Manages all TS queues in the CICS region. When a storage directory is
/// configured, AUXILIARY queues are automatically persisted to disk so
/// they survive CICS restarts.
#[derive(Debug)]
pub struct TsQueueManager {
    /// Queues indexed by name
    queues: HashMap<String, TsQueue>,
    /// Optional directory for auxiliary queue persistence
    storage_dir: Option<PathBuf>,
}

impl Default for TsQueueManager {
    fn default() -> Self {
        Self::new()
    }
}

impl TsQueueManager {
    /// Create a new queue manager (in-memory only).
    pub fn new() -> Self {
        Self {
            queues: HashMap::new(),
            storage_dir: None,
        }
    }

    /// Create a queue manager with disk persistence for auxiliary queues.
    ///
    /// The directory is created if it does not exist. Any `.tsq` files
    /// found in the directory are loaded automatically.
    pub fn with_storage_dir(dir: &Path) -> TsResult<Self> {
        if !dir.exists() {
            std::fs::create_dir_all(dir)
                .map_err(|e| TsError::StorageError(format!("create dir {}: {}", dir.display(), e)))?;
        }
        let mut mgr = Self {
            queues: HashMap::new(),
            storage_dir: Some(dir.to_path_buf()),
        };
        mgr.load_auxiliary_queues()?;
        Ok(mgr)
    }

    /// Get the configured storage directory, if any.
    pub fn storage_dir(&self) -> Option<&Path> {
        self.storage_dir.as_deref()
    }

    /// Write to a TS queue.
    ///
    /// Creates the queue if it doesn't exist. AUXILIARY queues are
    /// automatically persisted to disk after each write.
    pub fn writeq(
        &mut self,
        queue_name: &str,
        data: Vec<u8>,
        item: Option<u32>,
        rewrite: bool,
        queue_type: TsType,
    ) -> TsResult<u32> {
        let queue = self
            .queues
            .entry(queue_name.to_string())
            .or_insert_with(|| TsQueue::new(queue_name, queue_type));

        let result = queue.write(data, item, rewrite)?;

        // Auto-persist auxiliary queues
        if queue.is_auxiliary() {
            if let Some(dir) = &self.storage_dir {
                queue.persist(dir)?;
            }
        }

        Ok(result)
    }

    /// Read from a TS queue by item number.
    pub fn readq(&self, queue_name: &str, item: u32) -> TsResult<Vec<u8>> {
        let queue = self.queues.get(queue_name).ok_or(TsError::QueueNotFound)?;
        let ts_item = queue.read(item)?;
        Ok(ts_item.data.clone())
    }

    /// Read next item from a TS queue.
    pub fn readq_next(&mut self, queue_name: &str) -> TsResult<(u32, Vec<u8>)> {
        let queue = self.queues.get_mut(queue_name).ok_or(TsError::QueueNotFound)?;
        let item = queue.read_next()?;
        Ok((item.item_number, item.data.clone()))
    }

    /// Delete a TS queue.
    ///
    /// For AUXILIARY queues, also removes the persisted file on disk.
    pub fn deleteq(&mut self, queue_name: &str) -> TsResult<()> {
        let queue = self.queues.remove(queue_name).ok_or(TsError::QueueNotFound)?;

        // Remove disk file for auxiliary queues
        if queue.is_auxiliary() {
            if let Some(dir) = &self.storage_dir {
                queue.remove_persisted(dir)?;
            }
        }

        Ok(())
    }

    /// Get number of items in a queue.
    pub fn num_items(&self, queue_name: &str) -> TsResult<u32> {
        let queue = self.queues.get(queue_name).ok_or(TsError::QueueNotFound)?;
        Ok(queue.num_items())
    }

    /// Check if a queue exists.
    pub fn queue_exists(&self, queue_name: &str) -> bool {
        self.queues.contains_key(queue_name)
    }

    /// Get all queue names.
    pub fn queue_names(&self) -> Vec<&str> {
        self.queues.keys().map(|s| s.as_str()).collect()
    }

    /// Load all auxiliary queues from the storage directory.
    fn load_auxiliary_queues(&mut self) -> TsResult<()> {
        let dir = match &self.storage_dir {
            Some(d) => d.clone(),
            None => return Ok(()),
        };

        let entries = std::fs::read_dir(&dir)
            .map_err(|e| TsError::StorageError(format!("read dir {}: {}", dir.display(), e)))?;

        for entry in entries {
            let entry = entry
                .map_err(|e| TsError::StorageError(format!("read entry: {e}")))?;
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("tsq") {
                let stem = path.file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("");
                if !stem.is_empty() {
                    let data = std::fs::read(&path)
                        .map_err(|e| TsError::StorageError(format!("read {}: {}", path.display(), e)))?;
                    let queue = TsQueue::deserialize(stem, &data)?;
                    self.queues.insert(stem.to_string(), queue);
                }
            }
        }

        Ok(())
    }
}

/// TS Queue errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TsError {
    /// Queue not found (QIDERR)
    QueueNotFound,
    /// Item not found (ITEMERR)
    ItemNotFound(u32),
    /// Item already exists
    ItemExists(u32),
    /// Queue is full
    QueueFull,
    /// Invalid item number
    InvalidItem,
    /// Length error
    LengthError,
    /// Storage I/O error (auxiliary queue persistence)
    StorageError(String),
}

impl std::fmt::Display for TsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TsError::QueueNotFound => write!(f, "Queue not found (QIDERR)"),
            TsError::ItemNotFound(n) => write!(f, "Item {} not found (ITEMERR)", n),
            TsError::ItemExists(n) => write!(f, "Item {} already exists", n),
            TsError::QueueFull => write!(f, "Queue is full"),
            TsError::InvalidItem => write!(f, "Invalid item number"),
            TsError::LengthError => write!(f, "Length error"),
            TsError::StorageError(msg) => write!(f, "Storage error: {msg}"),
        }
    }
}

impl std::error::Error for TsError {}

/// Result type for TS operations.
pub type TsResult<T> = Result<T, TsError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ts_queue_basic() {
        let mut queue = TsQueue::new("TESTQ", TsType::Main);

        // Write items
        let item1 = queue.write(b"Hello".to_vec(), None, false).unwrap();
        let item2 = queue.write(b"World".to_vec(), None, false).unwrap();

        assert_eq!(item1, 1);
        assert_eq!(item2, 2);
        assert_eq!(queue.num_items(), 2);
    }

    #[test]
    fn test_ts_queue_read() {
        let mut queue = TsQueue::new("TESTQ", TsType::Main);
        queue.write(b"Data1".to_vec(), None, false).unwrap();
        queue.write(b"Data2".to_vec(), None, false).unwrap();

        let item = queue.read(1).unwrap();
        assert_eq!(item.data, b"Data1");

        let item = queue.read(2).unwrap();
        assert_eq!(item.data, b"Data2");
    }

    #[test]
    fn test_ts_queue_rewrite() {
        let mut queue = TsQueue::new("TESTQ", TsType::Main);
        queue.write(b"Original".to_vec(), None, false).unwrap();

        // Rewrite item 1
        queue.write(b"Updated".to_vec(), Some(1), true).unwrap();

        let item = queue.read(1).unwrap();
        assert_eq!(item.data, b"Updated");
    }

    #[test]
    fn test_ts_queue_read_next() {
        let mut queue = TsQueue::new("TESTQ", TsType::Main);
        queue.write(b"First".to_vec(), None, false).unwrap();
        queue.write(b"Second".to_vec(), None, false).unwrap();
        queue.write(b"Third".to_vec(), None, false).unwrap();

        let item1 = queue.read_next().unwrap();
        assert_eq!(item1.data, b"First");

        let item2 = queue.read_next().unwrap();
        assert_eq!(item2.data, b"Second");

        let item3 = queue.read_next().unwrap();
        assert_eq!(item3.data, b"Third");

        // No more items
        assert!(queue.read_next().is_err());
    }

    #[test]
    fn test_ts_queue_manager() {
        let mut mgr = TsQueueManager::new();

        // Write to queue
        let item = mgr.writeq("MYQUEUE", b"Test data".to_vec(), None, false, TsType::Main).unwrap();
        assert_eq!(item, 1);

        // Read from queue
        let data = mgr.readq("MYQUEUE", 1).unwrap();
        assert_eq!(data, b"Test data");

        // Check count
        assert_eq!(mgr.num_items("MYQUEUE").unwrap(), 1);

        // Delete queue
        mgr.deleteq("MYQUEUE").unwrap();
        assert!(!mgr.queue_exists("MYQUEUE"));
    }

    #[test]
    fn test_ts_queue_not_found() {
        let mgr = TsQueueManager::new();
        let result = mgr.readq("NOQUEUE", 1);
        assert!(matches!(result, Err(TsError::QueueNotFound)));
    }

    #[test]
    fn test_ts_item_not_found() {
        let mut mgr = TsQueueManager::new();
        mgr.writeq("Q", b"data".to_vec(), None, false, TsType::Main).unwrap();

        let result = mgr.readq("Q", 99);
        assert!(matches!(result, Err(TsError::ItemNotFound(99))));
    }

    // --- Epic 208: Auxiliary TS Queue Storage ---

    #[test]
    fn test_auxiliary_queue_serialize_deserialize() {
        let mut queue = TsQueue::new("SCRTCH", TsType::Auxiliary);
        queue.write(b"First item".to_vec(), None, false).unwrap();
        queue.write(b"Second item".to_vec(), None, false).unwrap();
        queue.write(b"Third item".to_vec(), None, false).unwrap();

        let serialized = queue.serialize();
        let restored = TsQueue::deserialize("SCRTCH", &serialized).unwrap();

        assert_eq!(restored.name, "SCRTCH");
        assert_eq!(restored.queue_type, TsType::Auxiliary);
        assert_eq!(restored.num_items(), 3);
        assert_eq!(restored.read(1).unwrap().data, b"First item");
        assert_eq!(restored.read(2).unwrap().data, b"Second item");
        assert_eq!(restored.read(3).unwrap().data, b"Third item");
    }

    #[test]
    fn test_auxiliary_queue_persist_and_load() {
        let dir = tempdir();

        // Write and persist
        let mut queue = TsQueue::new("PERSIST", TsType::Auxiliary);
        queue.write(b"disk data".to_vec(), None, false).unwrap();
        queue.persist(&dir).unwrap();

        // Load from disk
        let loaded = TsQueue::load("PERSIST", &dir).unwrap();
        assert_eq!(loaded.num_items(), 1);
        assert_eq!(loaded.read(1).unwrap().data, b"disk data");

        cleanup_tempdir(&dir);
    }

    #[test]
    fn test_auxiliary_queue_survives_restart() {
        let dir = tempdir();

        // Simulate first CICS session — write auxiliary data
        {
            let mut mgr = TsQueueManager::with_storage_dir(&dir).unwrap();
            mgr.writeq("SCRTCH", b"session1 data".to_vec(), None, false, TsType::Auxiliary).unwrap();
            mgr.writeq("SCRTCH", b"more data".to_vec(), None, false, TsType::Auxiliary).unwrap();
            // mgr goes out of scope — "CICS shuts down"
        }

        // Simulate second CICS session — data should be restored
        {
            let mgr = TsQueueManager::with_storage_dir(&dir).unwrap();
            assert!(mgr.queue_exists("SCRTCH"));
            assert_eq!(mgr.num_items("SCRTCH").unwrap(), 2);
            let data1 = mgr.readq("SCRTCH", 1).unwrap();
            assert_eq!(data1, b"session1 data");
            let data2 = mgr.readq("SCRTCH", 2).unwrap();
            assert_eq!(data2, b"more data");
        }

        cleanup_tempdir(&dir);
    }

    #[test]
    fn test_main_queue_not_persisted() {
        let dir = tempdir();

        // Write a MAIN queue — should NOT be persisted
        {
            let mut mgr = TsQueueManager::with_storage_dir(&dir).unwrap();
            mgr.writeq("TEMP", b"volatile".to_vec(), None, false, TsType::Main).unwrap();
        }

        // After restart, MAIN queue should be gone
        {
            let mgr = TsQueueManager::with_storage_dir(&dir).unwrap();
            assert!(!mgr.queue_exists("TEMP"));
        }

        cleanup_tempdir(&dir);
    }

    #[test]
    fn test_auxiliary_deleteq_removes_file() {
        let dir = tempdir();

        let mut mgr = TsQueueManager::with_storage_dir(&dir).unwrap();
        mgr.writeq("DELME", b"data".to_vec(), None, false, TsType::Auxiliary).unwrap();

        // File should exist
        assert!(dir.join("DELME.tsq").exists());

        // Delete queue
        mgr.deleteq("DELME").unwrap();

        // File should be removed
        assert!(!dir.join("DELME.tsq").exists());
        assert!(!mgr.queue_exists("DELME"));

        cleanup_tempdir(&dir);
    }

    #[test]
    fn test_auxiliary_rewrite_persists() {
        let dir = tempdir();

        {
            let mut mgr = TsQueueManager::with_storage_dir(&dir).unwrap();
            mgr.writeq("RW", b"original".to_vec(), None, false, TsType::Auxiliary).unwrap();
            mgr.writeq("RW", b"updated".to_vec(), Some(1), true, TsType::Auxiliary).unwrap();
        }

        // Reload and verify
        {
            let mgr = TsQueueManager::with_storage_dir(&dir).unwrap();
            let data = mgr.readq("RW", 1).unwrap();
            assert_eq!(data, b"updated");
        }

        cleanup_tempdir(&dir);
    }

    #[test]
    fn test_deserialize_corrupt_data() {
        // Too short
        assert!(TsQueue::deserialize("X", &[0, 1, 2]).is_err());

        // Valid header but truncated item
        let mut buf = Vec::new();
        buf.extend_from_slice(&1u32.to_le_bytes()); // next_item
        buf.extend_from_slice(&1u32.to_le_bytes()); // 1 item
        // No item data follows
        assert!(TsQueue::deserialize("X", &buf).is_err());
    }

    #[test]
    fn test_is_auxiliary() {
        let main_q = TsQueue::new("M", TsType::Main);
        let aux_q = TsQueue::new("A", TsType::Auxiliary);
        assert!(!main_q.is_auxiliary());
        assert!(aux_q.is_auxiliary());
    }

    #[test]
    fn test_storage_dir_accessor() {
        let mgr = TsQueueManager::new();
        assert!(mgr.storage_dir().is_none());

        let dir = tempdir();
        let mgr2 = TsQueueManager::with_storage_dir(&dir).unwrap();
        assert_eq!(mgr2.storage_dir().unwrap(), dir);

        cleanup_tempdir(&dir);
    }

    /// Create a unique temporary directory for testing.
    fn tempdir() -> PathBuf {
        use std::sync::atomic::{AtomicU64, Ordering};
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let id = COUNTER.fetch_add(1, Ordering::SeqCst);
        let dir = std::env::temp_dir().join(format!(
            "cics_ts_test_{}_{id}",
            std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        dir
    }

    /// Clean up a temporary directory.
    fn cleanup_tempdir(dir: &Path) {
        let _ = std::fs::remove_dir_all(dir);
    }
}
