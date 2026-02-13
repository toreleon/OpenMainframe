//! Transient Data (TD) Queue implementation.
//!
//! TD queues are used for asynchronous processing. Records are written
//! to a queue and processed later by another transaction.

use std::collections::{HashMap, VecDeque};

/// Type of transient data destination.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TdDestType {
    /// Intrapartition - internal queue within CICS
    Intrapartition,
    /// Extrapartition - external file or device
    Extrapartition,
}

impl Default for TdDestType {
    fn default() -> Self {
        TdDestType::Intrapartition
    }
}

/// A Transient Data queue.
#[derive(Debug, Clone)]
pub struct TdQueue {
    /// Queue name (up to 4 characters in CICS)
    pub name: String,
    /// Destination type
    pub dest_type: TdDestType,
    /// Trigger level (number of records before triggering)
    pub trigger_level: Option<u32>,
    /// Transaction to trigger
    pub trigger_transid: Option<String>,
    /// Records in the queue (for intrapartition)
    records: VecDeque<Vec<u8>>,
    /// Record count (for trigger checking)
    record_count: u32,
    /// Whether trigger has fired
    trigger_fired: bool,
}

impl TdQueue {
    /// Create a new TD queue.
    pub fn new(name: &str, dest_type: TdDestType) -> Self {
        Self {
            name: name.to_string(),
            dest_type,
            trigger_level: None,
            trigger_transid: None,
            records: VecDeque::new(),
            record_count: 0,
            trigger_fired: false,
        }
    }

    /// Create with trigger.
    pub fn with_trigger(mut self, level: u32, transid: &str) -> Self {
        self.trigger_level = Some(level);
        self.trigger_transid = Some(transid.to_string());
        self
    }

    /// Write a record to the queue.
    pub fn write(&mut self, data: Vec<u8>) -> TdResult<()> {
        match self.dest_type {
            TdDestType::Intrapartition => {
                self.records.push_back(data);
                self.record_count += 1;
                Ok(())
            }
            TdDestType::Extrapartition => {
                // For extrapartition, we'd write to an external file
                // For now, just store in memory
                self.records.push_back(data);
                self.record_count += 1;
                Ok(())
            }
        }
    }

    /// Read a record from the queue (destructive read).
    pub fn read(&mut self) -> TdResult<Vec<u8>> {
        match self.dest_type {
            TdDestType::Intrapartition => {
                self.records.pop_front().ok_or(TdError::QueueEmpty)
            }
            TdDestType::Extrapartition => {
                self.records.pop_front().ok_or(TdError::QueueEmpty)
            }
        }
    }

    /// Check if trigger should fire.
    pub fn should_trigger(&self) -> bool {
        if self.trigger_fired {
            return false;
        }
        match self.trigger_level {
            Some(level) => self.record_count >= level,
            None => false,
        }
    }

    /// Get triggered transaction ID.
    pub fn get_trigger_transid(&self) -> Option<&str> {
        self.trigger_transid.as_deref()
    }

    /// Mark trigger as fired.
    pub fn mark_triggered(&mut self) {
        self.trigger_fired = true;
    }

    /// Reset trigger.
    pub fn reset_trigger(&mut self) {
        self.trigger_fired = false;
        self.record_count = 0;
    }

    /// Get number of records in queue.
    pub fn num_records(&self) -> usize {
        self.records.len()
    }

    /// Check if queue is empty.
    pub fn is_empty(&self) -> bool {
        self.records.is_empty()
    }
}

/// Transient Data Queue Manager.
#[derive(Debug, Default)]
pub struct TdQueueManager {
    /// Queues indexed by name
    queues: HashMap<String, TdQueue>,
    /// Triggered transactions waiting to run
    pending_triggers: Vec<String>,
}

impl TdQueueManager {
    /// Create a new TD queue manager.
    pub fn new() -> Self {
        Self {
            queues: HashMap::new(),
            pending_triggers: Vec::new(),
        }
    }

    /// Define a TD queue.
    pub fn define_queue(&mut self, queue: TdQueue) {
        self.queues.insert(queue.name.clone(), queue);
    }

    /// Write to a TD queue.
    pub fn writeq(&mut self, queue_name: &str, data: Vec<u8>) -> TdResult<()> {
        let queue = self.queues.get_mut(queue_name).ok_or(TdError::QueueNotFound)?;
        queue.write(data)?;

        // Check for trigger
        if queue.should_trigger() {
            if let Some(transid) = queue.get_trigger_transid() {
                self.pending_triggers.push(transid.to_string());
                queue.mark_triggered();
            }
        }

        Ok(())
    }

    /// Read from a TD queue.
    pub fn readq(&mut self, queue_name: &str) -> TdResult<Vec<u8>> {
        let queue = self.queues.get_mut(queue_name).ok_or(TdError::QueueNotFound)?;
        queue.read()
    }

    /// Get pending triggered transactions.
    pub fn get_pending_triggers(&mut self) -> Vec<String> {
        std::mem::take(&mut self.pending_triggers)
    }

    /// Check if queue exists.
    pub fn queue_exists(&self, queue_name: &str) -> bool {
        self.queues.contains_key(queue_name)
    }

    /// Get queue info.
    pub fn get_queue(&self, queue_name: &str) -> Option<&TdQueue> {
        self.queues.get(queue_name)
    }

    /// Delete a queue.
    pub fn delete_queue(&mut self, queue_name: &str) -> TdResult<()> {
        self.queues.remove(queue_name).map(|_| ()).ok_or(TdError::QueueNotFound)
    }
}

/// TD Queue errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TdError {
    /// Queue not found (QIDERR)
    QueueNotFound,
    /// Queue is empty (QZERO)
    QueueEmpty,
    /// Queue is disabled
    QueueDisabled,
    /// Write error
    WriteError,
    /// Read error
    ReadError,
    /// Queue is full
    QueueFull,
}

impl std::fmt::Display for TdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TdError::QueueNotFound => write!(f, "Queue not found (QIDERR)"),
            TdError::QueueEmpty => write!(f, "Queue is empty (QZERO)"),
            TdError::QueueDisabled => write!(f, "Queue is disabled"),
            TdError::WriteError => write!(f, "Write error"),
            TdError::ReadError => write!(f, "Read error"),
            TdError::QueueFull => write!(f, "Queue is full"),
        }
    }
}

impl std::error::Error for TdError {}

/// Result type for TD operations.
pub type TdResult<T> = Result<T, TdError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_td_queue_basic() {
        let mut queue = TdQueue::new("TDQ1", TdDestType::Intrapartition);

        // Write records
        queue.write(b"Record 1".to_vec()).unwrap();
        queue.write(b"Record 2".to_vec()).unwrap();

        assert_eq!(queue.num_records(), 2);

        // Read records (FIFO)
        let rec1 = queue.read().unwrap();
        assert_eq!(rec1, b"Record 1");

        let rec2 = queue.read().unwrap();
        assert_eq!(rec2, b"Record 2");

        assert!(queue.is_empty());
    }

    #[test]
    fn test_td_queue_trigger() {
        let queue = TdQueue::new("TDQ2", TdDestType::Intrapartition)
            .with_trigger(3, "TRIG");

        let mut mgr = TdQueueManager::new();
        mgr.define_queue(queue);

        // Write 2 records - no trigger yet
        mgr.writeq("TDQ2", b"Rec1".to_vec()).unwrap();
        mgr.writeq("TDQ2", b"Rec2".to_vec()).unwrap();
        assert!(mgr.get_pending_triggers().is_empty());

        // Write 3rd record - trigger fires
        mgr.writeq("TDQ2", b"Rec3".to_vec()).unwrap();
        let triggers = mgr.get_pending_triggers();
        assert_eq!(triggers.len(), 1);
        assert_eq!(triggers[0], "TRIG");
    }

    #[test]
    fn test_td_queue_manager() {
        let mut mgr = TdQueueManager::new();

        // Define queue
        let queue = TdQueue::new("MYQUEUE", TdDestType::Intrapartition);
        mgr.define_queue(queue);

        // Write and read
        mgr.writeq("MYQUEUE", b"Test data".to_vec()).unwrap();
        let data = mgr.readq("MYQUEUE").unwrap();
        assert_eq!(data, b"Test data");
    }

    #[test]
    fn test_td_queue_empty() {
        let mut mgr = TdQueueManager::new();
        mgr.define_queue(TdQueue::new("EMPTY", TdDestType::Intrapartition));

        let result = mgr.readq("EMPTY");
        assert!(matches!(result, Err(TdError::QueueEmpty)));
    }

    #[test]
    fn test_td_queue_not_found() {
        let mut mgr = TdQueueManager::new();
        let result = mgr.writeq("NOQUEUE", b"data".to_vec());
        assert!(matches!(result, Err(TdError::QueueNotFound)));
    }
}
