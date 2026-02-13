//! Temporary Storage (TS) Queue implementation.
//!
//! TS queues provide temporary storage that can be shared between
//! transactions. Each queue has a name and contains numbered items.

use std::collections::HashMap;

/// Type of temporary storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TsType {
    /// Main storage - in-memory, faster but limited
    Main,
    /// Auxiliary storage - disk-backed, larger capacity
    Auxiliary,
}

impl Default for TsType {
    fn default() -> Self {
        TsType::Main
    }
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
}

/// Temporary Storage Queue Manager.
///
/// Manages all TS queues in the CICS region.
#[derive(Debug, Default)]
pub struct TsQueueManager {
    /// Queues indexed by name
    queues: HashMap<String, TsQueue>,
}

impl TsQueueManager {
    /// Create a new queue manager.
    pub fn new() -> Self {
        Self {
            queues: HashMap::new(),
        }
    }

    /// Write to a TS queue.
    ///
    /// Creates the queue if it doesn't exist.
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

        queue.write(data, item, rewrite)
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
    pub fn deleteq(&mut self, queue_name: &str) -> TsResult<()> {
        self.queues
            .remove(queue_name)
            .map(|_| ())
            .ok_or(TsError::QueueNotFound)
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
}
