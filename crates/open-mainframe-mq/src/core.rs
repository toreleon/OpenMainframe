//! MQ Core Runtime & Queue Manager.
//!
//! Provides the local queue manager with queue creation, deletion,
//! and message storage. Queues hold messages as byte vectors with
//! associated message descriptors.

use std::collections::{HashMap, VecDeque};
use crate::structures::Mqmd;

// ---------------------------------------------------------------------------
//  Queue types
// ---------------------------------------------------------------------------

/// Type of an MQ queue.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum QueueType {
    /// Local queue (holds messages).
    Local,
    /// Alias queue (points to another queue).
    Alias,
    /// Remote queue (definition for remote queue manager).
    Remote,
    /// Model queue (template for dynamic queues).
    Model,
    /// Dead-letter queue.
    DeadLetter,
}

// ---------------------------------------------------------------------------
//  Message
// ---------------------------------------------------------------------------

/// A message stored in a queue.
#[derive(Debug, Clone)]
pub struct Message {
    /// Message descriptor.
    pub mqmd: Mqmd,
    /// Message body.
    pub data: Vec<u8>,
}

// ---------------------------------------------------------------------------
//  Queue
// ---------------------------------------------------------------------------

/// An MQ queue.
#[derive(Debug)]
pub struct Queue {
    /// Queue name.
    pub name: String,
    /// Queue type.
    pub queue_type: QueueType,
    /// Maximum depth (0 = unlimited).
    pub max_depth: u32,
    /// Current messages.
    messages: VecDeque<Message>,
    /// Whether the queue is inhibited for put.
    pub put_inhibited: bool,
    /// Whether the queue is inhibited for get.
    pub get_inhibited: bool,
    /// Queue description.
    pub description: String,
    /// Target queue name for alias queues.
    pub target_queue: Option<String>,
    /// Remote queue manager name.
    pub remote_qmgr: Option<String>,
    /// Remote queue name.
    pub remote_queue: Option<String>,
    /// Total messages put since creation.
    pub put_count: u64,
    /// Total messages got since creation.
    pub get_count: u64,
}

impl Queue {
    /// Create a new local queue.
    pub fn new(name: &str, queue_type: QueueType) -> Self {
        Self {
            name: name.to_uppercase(),
            queue_type,
            max_depth: 5000,
            messages: VecDeque::new(),
            put_inhibited: false,
            get_inhibited: false,
            description: String::new(),
            target_queue: None,
            remote_qmgr: None,
            remote_queue: None,
            put_count: 0,
            get_count: 0,
        }
    }

    /// Current queue depth.
    pub fn depth(&self) -> u32 {
        self.messages.len() as u32
    }

    /// Put a message onto the queue.
    pub fn put(&mut self, mqmd: Mqmd, data: Vec<u8>) -> Result<(), MqError> {
        if self.put_inhibited {
            return Err(MqError::PutInhibited(self.name.clone()));
        }
        if self.max_depth > 0 && self.depth() >= self.max_depth {
            return Err(MqError::QueueFull(self.name.clone()));
        }
        self.messages.push_back(Message { mqmd, data });
        self.put_count += 1;
        Ok(())
    }

    /// Get a message from the queue (destructive get, FIFO).
    pub fn get(&mut self) -> Result<Message, MqError> {
        if self.get_inhibited {
            return Err(MqError::GetInhibited(self.name.clone()));
        }
        self.messages
            .pop_front()
            .inspect(|_| {
                self.get_count += 1;
            })
            .ok_or_else(|| MqError::NoMessage(self.name.clone()))
    }

    /// Browse (non-destructive) the next message.
    pub fn browse(&self, cursor: usize) -> Option<&Message> {
        self.messages.get(cursor)
    }

    /// Get a message by correlation ID.
    pub fn get_by_correl_id(&mut self, correl_id: &[u8; 24]) -> Result<Message, MqError> {
        if self.get_inhibited {
            return Err(MqError::GetInhibited(self.name.clone()));
        }
        let pos = self
            .messages
            .iter()
            .position(|m| &m.mqmd.correl_id == correl_id);
        match pos {
            Some(idx) => {
                let msg = self.messages.remove(idx).unwrap();
                self.get_count += 1;
                Ok(msg)
            }
            None => Err(MqError::NoMessage(self.name.clone())),
        }
    }

    /// Clear all messages from the queue.
    pub fn clear(&mut self) {
        self.messages.clear();
    }
}

// ---------------------------------------------------------------------------
//  Queue Manager
// ---------------------------------------------------------------------------

/// The MQ Queue Manager.
#[derive(Debug)]
pub struct QueueManager {
    /// Queue manager name.
    pub name: String,
    /// Queues by name.
    queues: HashMap<String, Queue>,
    /// Dead-letter queue name.
    pub dead_letter_queue: Option<String>,
    /// Whether the queue manager is running.
    pub running: bool,
    /// Next connection handle.
    next_conn_handle: u32,
    /// Description.
    pub description: String,
}

impl Default for QueueManager {
    fn default() -> Self {
        Self::new("QMGR")
    }
}

impl QueueManager {
    /// Create a new queue manager.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            queues: HashMap::new(),
            dead_letter_queue: None,
            running: true,
            next_conn_handle: 1,
            description: String::new(),
        }
    }

    /// Define a queue.
    pub fn define_queue(&mut self, name: &str, queue_type: QueueType) -> Result<(), MqError> {
        let upper = name.to_uppercase();
        if self.queues.contains_key(&upper) {
            return Err(MqError::QueueExists(upper));
        }
        self.queues.insert(upper, Queue::new(name, queue_type));
        Ok(())
    }

    /// Delete a queue (must be empty).
    pub fn delete_queue(&mut self, name: &str) -> Result<(), MqError> {
        let upper = name.to_uppercase();
        let queue = self
            .queues
            .get(&upper)
            .ok_or_else(|| MqError::QueueNotFound(upper.clone()))?;
        if queue.depth() > 0 {
            return Err(MqError::QueueNotEmpty(upper));
        }
        self.queues.remove(&upper);
        Ok(())
    }

    /// Delete a queue, purging all messages.
    pub fn delete_queue_purge(&mut self, name: &str) -> Result<(), MqError> {
        let upper = name.to_uppercase();
        if !self.queues.contains_key(&upper) {
            return Err(MqError::QueueNotFound(upper));
        }
        self.queues.remove(&upper);
        Ok(())
    }

    /// Get a mutable reference to a queue.
    pub fn get_queue_mut(&mut self, name: &str) -> Result<&mut Queue, MqError> {
        let upper = name.to_uppercase();
        self.queues
            .get_mut(&upper)
            .ok_or(MqError::QueueNotFound(upper))
    }

    /// Get a reference to a queue.
    pub fn get_queue(&self, name: &str) -> Result<&Queue, MqError> {
        let upper = name.to_uppercase();
        self.queues
            .get(&upper)
            .ok_or(MqError::QueueNotFound(upper))
    }

    /// Check if a queue exists.
    pub fn queue_exists(&self, name: &str) -> bool {
        self.queues.contains_key(&name.to_uppercase())
    }

    /// List all queue names.
    pub fn list_queues(&self) -> Vec<String> {
        self.queues.keys().cloned().collect()
    }

    /// Get the number of queues.
    pub fn queue_count(&self) -> usize {
        self.queues.len()
    }

    /// Allocate the next connection handle.
    pub fn next_handle(&mut self) -> u32 {
        let h = self.next_conn_handle;
        self.next_conn_handle += 1;
        h
    }

    /// Resolve an alias queue to the target local queue name.
    pub fn resolve_alias(&self, name: &str) -> Result<String, MqError> {
        let upper = name.to_uppercase();
        let queue = self
            .queues
            .get(&upper)
            .ok_or_else(|| MqError::QueueNotFound(upper.clone()))?;
        if queue.queue_type == QueueType::Alias {
            queue
                .target_queue
                .clone()
                .ok_or_else(|| MqError::Other(format!("Alias {upper} has no target")))
        } else {
            Ok(upper)
        }
    }
}

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// MQ error codes (mapped to human-readable errors).
#[derive(Debug, Clone, thiserror::Error)]
pub enum MqError {
    #[error("Queue not found: {0}")]
    QueueNotFound(String),
    #[error("Queue already exists: {0}")]
    QueueExists(String),
    #[error("Queue full: {0}")]
    QueueFull(String),
    #[error("Queue not empty: {0}")]
    QueueNotEmpty(String),
    #[error("No message available on queue: {0}")]
    NoMessage(String),
    #[error("Put inhibited on queue: {0}")]
    PutInhibited(String),
    #[error("Get inhibited on queue: {0}")]
    GetInhibited(String),
    #[error("Not connected")]
    NotConnected,
    #[error("Queue not open")]
    QueueNotOpen,
    #[error("Invalid handle")]
    InvalidHandle,
    #[error("MQSC error: {0}")]
    MqscError(String),
    #[error("MQ error: {0}")]
    Other(String),
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_queue_manager_create() {
        let qm = QueueManager::new("TESTQM");
        assert_eq!(qm.name, "TESTQM");
        assert!(qm.running);
        assert_eq!(qm.queue_count(), 0);
    }

    #[test]
    fn test_define_queue() {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("MY.QUEUE", QueueType::Local).unwrap();
        assert!(qm.queue_exists("MY.QUEUE"));
        assert_eq!(qm.queue_count(), 1);
    }

    #[test]
    fn test_define_duplicate_queue() {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("MY.QUEUE", QueueType::Local).unwrap();
        assert!(qm.define_queue("MY.QUEUE", QueueType::Local).is_err());
    }

    #[test]
    fn test_delete_empty_queue() {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("MY.QUEUE", QueueType::Local).unwrap();
        qm.delete_queue("MY.QUEUE").unwrap();
        assert!(!qm.queue_exists("MY.QUEUE"));
    }

    #[test]
    fn test_delete_nonempty_queue_fails() {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("MY.QUEUE", QueueType::Local).unwrap();
        let queue = qm.get_queue_mut("MY.QUEUE").unwrap();
        queue.put(Mqmd::default(), b"hello".to_vec()).unwrap();
        assert!(qm.delete_queue("MY.QUEUE").is_err());
    }

    #[test]
    fn test_delete_purge() {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("MY.QUEUE", QueueType::Local).unwrap();
        let queue = qm.get_queue_mut("MY.QUEUE").unwrap();
        queue.put(Mqmd::default(), b"hello".to_vec()).unwrap();
        qm.delete_queue_purge("MY.QUEUE").unwrap();
        assert!(!qm.queue_exists("MY.QUEUE"));
    }

    #[test]
    fn test_put_and_get() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        queue.put(Mqmd::default(), b"msg1".to_vec()).unwrap();
        queue.put(Mqmd::default(), b"msg2".to_vec()).unwrap();
        assert_eq!(queue.depth(), 2);

        let msg1 = queue.get().unwrap();
        assert_eq!(msg1.data, b"msg1");
        assert_eq!(queue.depth(), 1);

        let msg2 = queue.get().unwrap();
        assert_eq!(msg2.data, b"msg2");
        assert_eq!(queue.depth(), 0);
    }

    #[test]
    fn test_get_empty_queue() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        assert!(queue.get().is_err());
    }

    #[test]
    fn test_queue_full() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        queue.max_depth = 2;
        queue.put(Mqmd::default(), b"1".to_vec()).unwrap();
        queue.put(Mqmd::default(), b"2".to_vec()).unwrap();
        assert!(queue.put(Mqmd::default(), b"3".to_vec()).is_err());
    }

    #[test]
    fn test_put_inhibited() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        queue.put_inhibited = true;
        assert!(queue.put(Mqmd::default(), b"msg".to_vec()).is_err());
    }

    #[test]
    fn test_get_inhibited() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        queue.put(Mqmd::default(), b"msg".to_vec()).unwrap();
        queue.get_inhibited = true;
        assert!(queue.get().is_err());
    }

    #[test]
    fn test_browse() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        queue.put(Mqmd::default(), b"msg1".to_vec()).unwrap();
        queue.put(Mqmd::default(), b"msg2".to_vec()).unwrap();

        let msg = queue.browse(0).unwrap();
        assert_eq!(msg.data, b"msg1");
        let msg = queue.browse(1).unwrap();
        assert_eq!(msg.data, b"msg2");
        assert!(queue.browse(2).is_none());
        // Queue unchanged.
        assert_eq!(queue.depth(), 2);
    }

    #[test]
    fn test_get_by_correl_id() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        let mut md1 = Mqmd::default();
        md1.correl_id[0] = 0x01;
        let mut md2 = Mqmd::default();
        md2.correl_id[0] = 0x02;

        queue.put(md1, b"msg1".to_vec()).unwrap();
        queue.put(md2, b"msg2".to_vec()).unwrap();

        let mut target = [0u8; 24];
        target[0] = 0x02;
        let msg = queue.get_by_correl_id(&target).unwrap();
        assert_eq!(msg.data, b"msg2");
        assert_eq!(queue.depth(), 1);
    }

    #[test]
    fn test_clear_queue() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        queue.put(Mqmd::default(), b"1".to_vec()).unwrap();
        queue.put(Mqmd::default(), b"2".to_vec()).unwrap();
        queue.clear();
        assert_eq!(queue.depth(), 0);
    }

    #[test]
    fn test_put_get_counts() {
        let mut queue = Queue::new("TEST", QueueType::Local);
        queue.put(Mqmd::default(), b"1".to_vec()).unwrap();
        queue.put(Mqmd::default(), b"2".to_vec()).unwrap();
        assert_eq!(queue.put_count, 2);
        queue.get().unwrap();
        assert_eq!(queue.get_count, 1);
    }

    #[test]
    fn test_list_queues() {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("Q1", QueueType::Local).unwrap();
        qm.define_queue("Q2", QueueType::Local).unwrap();
        let names = qm.list_queues();
        assert_eq!(names.len(), 2);
    }
}
