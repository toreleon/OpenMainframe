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

/// Resource enqueue manager with deadlock detection.
#[derive(Debug, Default)]
pub struct EnqueueManager {
    /// Active locks by resource name
    locks: HashMap<String, Vec<ResourceLock>>,
    /// Locks held by each task
    task_locks: HashMap<u32, Vec<String>>,
    /// Wait queue for blocked requests
    wait_queue: Vec<EnqueueRequest>,
    /// Lock timeout in milliseconds (0 = no timeout)
    lock_timeout_ms: u64,
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
            lock_timeout_ms: 0,
        }
    }

    /// Set lock timeout in milliseconds (0 = no timeout).
    pub fn set_lock_timeout(&mut self, ms: u64) {
        self.lock_timeout_ms = ms;
    }

    /// Get current lock timeout.
    pub fn lock_timeout(&self) -> u64 {
        self.lock_timeout_ms
    }

    /// Enqueue (lock) a resource.
    ///
    /// When `nowait` is true (NOSUSPEND), returns `ResourceBusy` immediately
    /// if the lock cannot be granted. When `nowait` is false and a deadlock
    /// is detected, returns `Deadlock`.
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
                }

                // Check for deadlock before queuing
                let holders: Vec<u32> = existing.iter().map(|l| l.task_id).collect();
                if self.would_deadlock(task_id, &holders) {
                    return Err(SyncError::Deadlock {
                        task_id,
                        resource: resource.to_string(),
                    });
                }

                // Add to wait queue
                self.wait_queue.push(EnqueueRequest {
                    resource: resource.to_string(),
                    lock_type,
                    task_id,
                });
                return Err(SyncError::Waiting);
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
            .or_default()
            .push(lock);

        self.task_locks
            .entry(task_id)
            .or_default()
            .push(resource.to_string());

        Ok(())
    }

    /// Detect if granting this request would cause a deadlock.
    ///
    /// A deadlock occurs when task A holds resource R1 and waits for R2,
    /// while task B holds R2 and waits for R1 (or any cycle of waiting).
    ///
    /// This uses a wait-for graph traversal: starting from the tasks
    /// that hold the contested resource, check if any of them are
    /// (directly or transitively) waiting for a resource held by `task_id`.
    fn would_deadlock(&self, task_id: u32, holders: &[u32]) -> bool {
        // Build wait-for relationships:
        // For each task in wait_queue, find which tasks hold the resource it wants
        let mut visited = HashSet::new();
        let mut stack: Vec<u32> = holders.to_vec();

        while let Some(holder) = stack.pop() {
            if holder == task_id {
                return true; // Cycle detected
            }
            if !visited.insert(holder) {
                continue; // Already visited
            }

            // Check if this holder is waiting for any resource
            for req in &self.wait_queue {
                if req.task_id == holder {
                    // This holder is waiting for req.resource — who holds it?
                    if let Some(blockers) = self.locks.get(&req.resource) {
                        for blocker in blockers {
                            if blocker.task_id != holder {
                                stack.push(blocker.task_id);
                            }
                        }
                    }
                }
            }
        }

        false
    }

    /// Get the number of waiting requests.
    pub fn wait_queue_len(&self) -> usize {
        self.wait_queue.len()
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
    /// Resource is busy (ENQBUSY) — returned when NOSUSPEND is specified
    ResourceBusy,
    /// Request is waiting
    Waiting,
    /// Resource not locked
    NotLocked,
    /// Syncpoint error
    SyncpointError,
    /// Rollback error
    RollbackError,
    /// Deadlock detected (AEYD) — a cycle in the wait-for graph
    Deadlock {
        /// Task that was detected as the deadlock victim
        task_id: u32,
        /// Resource that caused the deadlock
        resource: String,
    },
}

impl std::fmt::Display for SyncError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyncError::ResourceBusy => write!(f, "Resource busy (ENQBUSY)"),
            SyncError::Waiting => write!(f, "Request is waiting"),
            SyncError::NotLocked => write!(f, "Resource not locked"),
            SyncError::SyncpointError => write!(f, "Syncpoint error"),
            SyncError::RollbackError => write!(f, "Rollback error"),
            SyncError::Deadlock { task_id, resource } => {
                write!(f, "Deadlock detected: task {} on resource '{}' (AEYD)", task_id, resource)
            }
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

    // === Story 207.1: Deadlock detection ===

    #[test]
    fn test_deadlock_detection_simple() {
        let mut mgr = EnqueueManager::new();

        // Task 1 locks RES1
        mgr.enq("RES1", LockType::Exclusive, 1, false).unwrap();
        // Task 2 locks RES2
        mgr.enq("RES2", LockType::Exclusive, 2, false).unwrap();

        // Task 1 tries to lock RES2 (blocked, goes to wait queue)
        let result = mgr.enq("RES2", LockType::Exclusive, 1, false);
        assert!(matches!(result, Err(SyncError::Waiting)));

        // Task 2 tries to lock RES1 — deadlock! (2 waits for 1, 1 waits for 2)
        let result = mgr.enq("RES1", LockType::Exclusive, 2, false);
        assert!(matches!(result, Err(SyncError::Deadlock { task_id: 2, .. })));
    }

    #[test]
    fn test_deadlock_detection_three_way() {
        let mut mgr = EnqueueManager::new();

        // Task 1 -> RES1, Task 2 -> RES2, Task 3 -> RES3
        mgr.enq("RES1", LockType::Exclusive, 1, false).unwrap();
        mgr.enq("RES2", LockType::Exclusive, 2, false).unwrap();
        mgr.enq("RES3", LockType::Exclusive, 3, false).unwrap();

        // Task 1 waits for RES2 (held by 2)
        let _ = mgr.enq("RES2", LockType::Exclusive, 1, false);
        // Task 2 waits for RES3 (held by 3)
        let _ = mgr.enq("RES3", LockType::Exclusive, 2, false);
        // Task 3 waits for RES1 (held by 1) — cycle: 3→1→2→3
        let result = mgr.enq("RES1", LockType::Exclusive, 3, false);
        assert!(matches!(result, Err(SyncError::Deadlock { task_id: 3, .. })));
    }

    // === Story 207.2: NOSUSPEND and timeout ===

    #[test]
    fn test_nosuspend_returns_busy() {
        let mut mgr = EnqueueManager::new();

        // Task 1 locks resource
        mgr.enq("RES1", LockType::Exclusive, 1, false).unwrap();

        // Task 2 tries with NOSUSPEND
        let result = mgr.enq("RES1", LockType::Exclusive, 2, true);
        assert!(matches!(result, Err(SyncError::ResourceBusy)));
    }

    #[test]
    fn test_lock_timeout_setting() {
        let mut mgr = EnqueueManager::new();
        assert_eq!(mgr.lock_timeout(), 0);

        mgr.set_lock_timeout(5000);
        assert_eq!(mgr.lock_timeout(), 5000);
    }

    #[test]
    fn test_wait_queue_count() {
        let mut mgr = EnqueueManager::new();

        mgr.enq("RES1", LockType::Exclusive, 1, false).unwrap();
        assert_eq!(mgr.wait_queue_len(), 0);

        // Task 2 waits
        let _ = mgr.enq("RES1", LockType::Exclusive, 2, false);
        assert_eq!(mgr.wait_queue_len(), 1);

        // Release by task 1 should grant to task 2
        mgr.deq("RES1", 1).unwrap();
        assert_eq!(mgr.wait_queue_len(), 0);
    }

    #[test]
    fn test_no_deadlock_when_no_cycle() {
        let mut mgr = EnqueueManager::new();

        // Task 1 locks RES1, Task 2 locks RES2
        mgr.enq("RES1", LockType::Exclusive, 1, false).unwrap();
        mgr.enq("RES2", LockType::Exclusive, 2, false).unwrap();

        // Task 3 waits for RES1 — no cycle because task 3 holds nothing
        let result = mgr.enq("RES1", LockType::Exclusive, 3, false);
        assert!(matches!(result, Err(SyncError::Waiting)));
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
