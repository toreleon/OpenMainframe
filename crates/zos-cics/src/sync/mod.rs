//! CICS Synchronization Services.
//!
//! Provides SYNCPOINT and ENQ/DEQ commands.

use std::collections::{HashMap, HashSet};

/// Resource lock type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LockType {
    /// Exclusive lock
    Exclusive,
    /// Shared lock
    Shared,
}

/// A resource lock.
#[derive(Debug, Clone)]
pub struct ResourceLock {
    /// Resource name
    pub resource: String,
    /// Lock type
    pub lock_type: LockType,
    /// Task that holds the lock
    pub task_id: u32,
}

/// Syncpoint manager for transaction control.
#[derive(Debug, Default)]
pub struct SyncpointManager {
    /// Current unit of work ID
    uow_id: u64,
    /// Resources modified in current UOW
    modified_resources: HashSet<String>,
    /// Syncpoint history for this task
    syncpoint_count: u32,
}

impl SyncpointManager {
    /// Create a new syncpoint manager.
    pub fn new() -> Self {
        Self {
            uow_id: 1,
            modified_resources: HashSet::new(),
            syncpoint_count: 0,
        }
    }

    /// Start a new unit of work.
    pub fn begin_uow(&mut self) {
        self.uow_id += 1;
        self.modified_resources.clear();
    }

    /// Record a resource modification.
    pub fn record_modification(&mut self, resource: &str) {
        self.modified_resources.insert(resource.to_string());
    }

    /// Execute SYNCPOINT (commit).
    pub fn syncpoint(&mut self) -> SyncResult<SyncpointInfo> {
        let info = SyncpointInfo {
            uow_id: self.uow_id,
            resources_committed: self.modified_resources.len(),
        };

        self.syncpoint_count += 1;
        self.modified_resources.clear();
        self.uow_id += 1;

        Ok(info)
    }

    /// Execute SYNCPOINT ROLLBACK.
    pub fn rollback(&mut self) -> SyncResult<SyncpointInfo> {
        let info = SyncpointInfo {
            uow_id: self.uow_id,
            resources_committed: 0, // Nothing committed
        };

        self.modified_resources.clear();
        self.uow_id += 1;

        Ok(info)
    }

    /// Get current unit of work ID.
    pub fn current_uow(&self) -> u64 {
        self.uow_id
    }

    /// Get number of syncpoints taken.
    pub fn syncpoint_count(&self) -> u32 {
        self.syncpoint_count
    }
}

/// Syncpoint result info.
#[derive(Debug, Clone)]
pub struct SyncpointInfo {
    /// Unit of work ID
    pub uow_id: u64,
    /// Number of resources committed
    pub resources_committed: usize,
}

/// Resource enqueue manager.
#[derive(Debug, Default)]
pub struct EnqueueManager {
    /// Active locks by resource name
    locks: HashMap<String, Vec<ResourceLock>>,
    /// Locks held by each task
    task_locks: HashMap<u32, Vec<String>>,
    /// Wait queue for blocked requests
    wait_queue: Vec<EnqueueRequest>,
}

/// A pending enqueue request.
#[derive(Debug, Clone)]
struct EnqueueRequest {
    resource: String,
    lock_type: LockType,
    task_id: u32,
}

impl EnqueueManager {
    /// Create a new enqueue manager.
    pub fn new() -> Self {
        Self {
            locks: HashMap::new(),
            task_locks: HashMap::new(),
            wait_queue: Vec::new(),
        }
    }

    /// Enqueue (lock) a resource.
    pub fn enq(
        &mut self,
        resource: &str,
        lock_type: LockType,
        task_id: u32,
        nowait: bool,
    ) -> SyncResult<()> {
        // Check for existing locks
        if let Some(existing) = self.locks.get(resource) {
            // Check compatibility
            let can_lock = match lock_type {
                LockType::Exclusive => existing.is_empty(),
                LockType::Shared => existing.iter().all(|l| l.lock_type == LockType::Shared),
            };

            if !can_lock {
                if nowait {
                    return Err(SyncError::ResourceBusy);
                } else {
                    // Add to wait queue
                    self.wait_queue.push(EnqueueRequest {
                        resource: resource.to_string(),
                        lock_type,
                        task_id,
                    });
                    return Err(SyncError::Waiting);
                }
            }
        }

        // Grant the lock
        let lock = ResourceLock {
            resource: resource.to_string(),
            lock_type,
            task_id,
        };

        self.locks
            .entry(resource.to_string())
            .or_insert_with(Vec::new)
            .push(lock);

        self.task_locks
            .entry(task_id)
            .or_insert_with(Vec::new)
            .push(resource.to_string());

        Ok(())
    }

    /// Dequeue (unlock) a resource.
    pub fn deq(&mut self, resource: &str, task_id: u32) -> SyncResult<()> {
        // Remove lock
        if let Some(locks) = self.locks.get_mut(resource) {
            let pos = locks.iter().position(|l| l.task_id == task_id);
            if let Some(idx) = pos {
                locks.remove(idx);

                // Remove from task's lock list
                if let Some(task_locks) = self.task_locks.get_mut(&task_id) {
                    task_locks.retain(|r| r != resource);
                }

                // Check wait queue for pending requests
                self.process_wait_queue(resource);

                return Ok(());
            }
        }

        Err(SyncError::NotLocked)
    }

    /// Release all locks held by a task.
    pub fn release_all(&mut self, task_id: u32) {
        if let Some(resources) = self.task_locks.remove(&task_id) {
            for resource in resources {
                if let Some(locks) = self.locks.get_mut(&resource) {
                    locks.retain(|l| l.task_id != task_id);
                    self.process_wait_queue(&resource);
                }
            }
        }
    }

    /// Process wait queue after a lock release.
    fn process_wait_queue(&mut self, resource: &str) {
        // Find first waiter for this resource that can be granted
        let mut granted = Vec::new();

        for (idx, req) in self.wait_queue.iter().enumerate() {
            if req.resource == resource {
                // Try to grant
                let existing = self.locks.get(resource);
                let can_grant = match req.lock_type {
                    LockType::Exclusive => existing.map_or(true, |l| l.is_empty()),
                    LockType::Shared => existing.map_or(true, |l| {
                        l.iter().all(|x| x.lock_type == LockType::Shared)
                    }),
                };

                if can_grant {
                    granted.push(idx);
                    // Only grant one exclusive, but can grant multiple shared
                    if req.lock_type == LockType::Exclusive {
                        break;
                    }
                }
            }
        }

        // Grant locks (in reverse order to maintain indices)
        for idx in granted.into_iter().rev() {
            let req = self.wait_queue.remove(idx);
            let _ = self.enq(&req.resource, req.lock_type, req.task_id, true);
        }
    }

    /// Check if a resource is locked.
    pub fn is_locked(&self, resource: &str) -> bool {
        self.locks.get(resource).map_or(false, |l| !l.is_empty())
    }

    /// Get locks held by a task.
    pub fn get_task_locks(&self, task_id: u32) -> Vec<&str> {
        self.task_locks
            .get(&task_id)
            .map_or(Vec::new(), |v| v.iter().map(|s| s.as_str()).collect())
    }
}

/// Synchronization errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyncError {
    /// Resource is busy (ENQBUSY)
    ResourceBusy,
    /// Request is waiting
    Waiting,
    /// Resource not locked
    NotLocked,
    /// Syncpoint error
    SyncpointError,
    /// Rollback error
    RollbackError,
}

impl std::fmt::Display for SyncError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyncError::ResourceBusy => write!(f, "Resource busy (ENQBUSY)"),
            SyncError::Waiting => write!(f, "Request is waiting"),
            SyncError::NotLocked => write!(f, "Resource not locked"),
            SyncError::SyncpointError => write!(f, "Syncpoint error"),
            SyncError::RollbackError => write!(f, "Rollback error"),
        }
    }
}

impl std::error::Error for SyncError {}

/// Result type for sync operations.
pub type SyncResult<T> = Result<T, SyncError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_syncpoint_basic() {
        let mut mgr = SyncpointManager::new();

        mgr.record_modification("FILE1");
        mgr.record_modification("FILE2");

        let info = mgr.syncpoint().unwrap();
        assert_eq!(info.resources_committed, 2);
        assert_eq!(mgr.syncpoint_count(), 1);
    }

    #[test]
    fn test_syncpoint_rollback() {
        let mut mgr = SyncpointManager::new();

        mgr.record_modification("FILE1");
        let info = mgr.rollback().unwrap();
        assert_eq!(info.resources_committed, 0);
    }

    #[test]
    fn test_enq_deq_basic() {
        let mut mgr = EnqueueManager::new();

        // Enqueue resource
        mgr.enq("RESOURCE1", LockType::Exclusive, 1, false).unwrap();
        assert!(mgr.is_locked("RESOURCE1"));

        // Dequeue resource
        mgr.deq("RESOURCE1", 1).unwrap();
        assert!(!mgr.is_locked("RESOURCE1"));
    }

    #[test]
    fn test_enq_exclusive_conflict() {
        let mut mgr = EnqueueManager::new();

        // First task gets lock
        mgr.enq("RESOURCE1", LockType::Exclusive, 1, false).unwrap();

        // Second task tries to get same lock with NOWAIT
        let result = mgr.enq("RESOURCE1", LockType::Exclusive, 2, true);
        assert!(matches!(result, Err(SyncError::ResourceBusy)));
    }

    #[test]
    fn test_enq_shared_compatible() {
        let mut mgr = EnqueueManager::new();

        // Multiple tasks can get shared locks
        mgr.enq("RESOURCE1", LockType::Shared, 1, false).unwrap();
        mgr.enq("RESOURCE1", LockType::Shared, 2, false).unwrap();
        mgr.enq("RESOURCE1", LockType::Shared, 3, false).unwrap();

        assert!(mgr.is_locked("RESOURCE1"));
    }

    #[test]
    fn test_enq_shared_exclusive_conflict() {
        let mut mgr = EnqueueManager::new();

        // Shared lock
        mgr.enq("RESOURCE1", LockType::Shared, 1, false).unwrap();

        // Exclusive lock should fail
        let result = mgr.enq("RESOURCE1", LockType::Exclusive, 2, true);
        assert!(matches!(result, Err(SyncError::ResourceBusy)));
    }

    #[test]
    fn test_release_all() {
        let mut mgr = EnqueueManager::new();

        // Task 1 gets multiple locks
        mgr.enq("RES1", LockType::Exclusive, 1, false).unwrap();
        mgr.enq("RES2", LockType::Exclusive, 1, false).unwrap();
        mgr.enq("RES3", LockType::Shared, 1, false).unwrap();

        // Release all
        mgr.release_all(1);

        assert!(!mgr.is_locked("RES1"));
        assert!(!mgr.is_locked("RES2"));
        assert!(!mgr.is_locked("RES3"));
    }

    #[test]
    fn test_get_task_locks() {
        let mut mgr = EnqueueManager::new();

        mgr.enq("RES1", LockType::Exclusive, 1, false).unwrap();
        mgr.enq("RES2", LockType::Shared, 1, false).unwrap();

        let locks = mgr.get_task_locks(1);
        assert_eq!(locks.len(), 2);
        assert!(locks.contains(&"RES1"));
        assert!(locks.contains(&"RES2"));
    }
}
