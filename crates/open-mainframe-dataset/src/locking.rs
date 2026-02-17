//! Dataset and record-level locking for concurrent access control.
//!
//! Provides two levels of locking:
//! - **Dataset-level** (`DatasetLockManager`): Enforces DISP semantics
//!   (OLD = exclusive, SHR = shared, NEW/MOD = exclusive).
//! - **Record-level** (`LockManager`): Fine-grained shared/exclusive locks
//!   on individual records within a dataset, with stale lock detection.
//!
//! # Example
//!
//! ```
//! use open_mainframe_dataset::locking::{LockManager, LockMode};
//!
//! let mut mgr = LockManager::new();
//! mgr.acquire("MY.DATASET", "key001", LockMode::Shared, "JOB1").unwrap();
//! mgr.acquire("MY.DATASET", "key001", LockMode::Shared, "JOB2").unwrap();
//! mgr.release("MY.DATASET", "key001", "JOB1");
//! ```

use std::collections::HashMap;
use std::time::{Duration, Instant};

use crate::error::DatasetError;

/// Lock mode for record or dataset-level locks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LockMode {
    /// Shared lock — multiple holders allowed, no exclusive access.
    Shared,
    /// Exclusive lock — single holder only.
    Exclusive,
}

/// A single lock entry tracking who holds the lock and when it was acquired.
#[derive(Debug, Clone)]
pub struct LockEntry {
    /// Lock mode (shared or exclusive).
    pub mode: LockMode,
    /// Owner identifier (job name, task ID, etc.).
    pub owner: String,
    /// When the lock was acquired.
    pub acquired_at: Instant,
}

/// Record-level lock state for a single key within a dataset.
#[derive(Debug)]
struct RecordLock {
    /// Current lock mode (if any holders exist).
    mode: LockMode,
    /// Set of owners holding this lock.
    holders: Vec<LockEntry>,
}

/// Record-level lock manager for VSAM and other datasets.
///
/// Tracks fine-grained shared/exclusive locks on individual records
/// (identified by dataset name + record key). Supports stale lock
/// detection via configurable timeout.
pub struct LockManager {
    /// Locks keyed by (dataset_name, record_key).
    locks: HashMap<String, HashMap<String, RecordLock>>,
    /// Lock timeout for stale detection. Locks older than this are auto-released.
    lock_timeout: Duration,
}

impl LockManager {
    /// Create a new lock manager with default 5-minute timeout.
    pub fn new() -> Self {
        Self {
            locks: HashMap::new(),
            lock_timeout: Duration::from_secs(300),
        }
    }

    /// Create a lock manager with a custom timeout.
    pub fn with_timeout(timeout: Duration) -> Self {
        Self {
            locks: HashMap::new(),
            lock_timeout: timeout,
        }
    }

    /// Acquire a lock on a record.
    ///
    /// - Shared locks can coexist with other shared locks.
    /// - Exclusive locks require no other holders.
    /// - Stale locks (older than timeout) are purged before checking.
    pub fn acquire(
        &mut self,
        dataset: &str,
        key: &str,
        mode: LockMode,
        owner: &str,
    ) -> Result<(), DatasetError> {
        // Purge stale locks first
        self.purge_stale(dataset, key);

        let ds_locks = self.locks.entry(dataset.to_string()).or_default();
        let record_lock = ds_locks.get(key);

        match record_lock {
            None => {
                // No existing lock — grant immediately
                ds_locks.insert(
                    key.to_string(),
                    RecordLock {
                        mode,
                        holders: vec![LockEntry {
                            mode,
                            owner: owner.to_string(),
                            acquired_at: Instant::now(),
                        }],
                    },
                );
                Ok(())
            }
            Some(existing) => {
                // Check if this owner already holds the lock
                if existing.holders.iter().any(|h| h.owner == owner) {
                    return Ok(()); // Re-entrant — already held
                }

                match (existing.mode, mode) {
                    (LockMode::Shared, LockMode::Shared) => {
                        // Compatible — add another shared holder
                        let rl = ds_locks.get_mut(key).unwrap();
                        rl.holders.push(LockEntry {
                            mode,
                            owner: owner.to_string(),
                            acquired_at: Instant::now(),
                        });
                        Ok(())
                    }
                    _ => {
                        // Incompatible — shared+exclusive or exclusive+any
                        let holder_names: Vec<&str> =
                            existing.holders.iter().map(|h| h.owner.as_str()).collect();
                        Err(DatasetError::InUse {
                            name: format!(
                                "{}:{} (held by {:?} in {:?} mode)",
                                dataset, key, holder_names, existing.mode
                            ),
                        })
                    }
                }
            }
        }
    }

    /// Release a lock held by the given owner.
    ///
    /// If this was the last holder, the lock entry is removed.
    pub fn release(&mut self, dataset: &str, key: &str, owner: &str) {
        if let Some(ds_locks) = self.locks.get_mut(dataset) {
            if let Some(record_lock) = ds_locks.get_mut(key) {
                record_lock.holders.retain(|h| h.owner != owner);
                if record_lock.holders.is_empty() {
                    ds_locks.remove(key);
                }
            }
            if ds_locks.is_empty() {
                self.locks.remove(dataset);
            }
        }
    }

    /// Release all locks held by a given owner across all datasets.
    pub fn release_all(&mut self, owner: &str) {
        let mut empty_datasets = Vec::new();
        for (ds_name, ds_locks) in &mut self.locks {
            let mut empty_keys = Vec::new();
            for (key, record_lock) in ds_locks.iter_mut() {
                record_lock.holders.retain(|h| h.owner != owner);
                if record_lock.holders.is_empty() {
                    empty_keys.push(key.clone());
                }
            }
            for k in empty_keys {
                ds_locks.remove(&k);
            }
            if ds_locks.is_empty() {
                empty_datasets.push(ds_name.clone());
            }
        }
        for ds in empty_datasets {
            self.locks.remove(&ds);
        }
    }

    /// Check if a record is currently locked.
    pub fn is_locked(&self, dataset: &str, key: &str) -> bool {
        self.locks
            .get(dataset)
            .and_then(|ds| ds.get(key))
            .is_some_and(|rl| !rl.holders.is_empty())
    }

    /// Get current lock holders for a record.
    pub fn holders(&self, dataset: &str, key: &str) -> Vec<LockEntry> {
        self.locks
            .get(dataset)
            .and_then(|ds| ds.get(key))
            .map(|rl| rl.holders.clone())
            .unwrap_or_default()
    }

    /// Purge stale locks on a specific record (those older than timeout).
    fn purge_stale(&mut self, dataset: &str, key: &str) {
        let now = Instant::now();
        if let Some(ds_locks) = self.locks.get_mut(dataset) {
            if let Some(record_lock) = ds_locks.get_mut(key) {
                record_lock
                    .holders
                    .retain(|h| now.duration_since(h.acquired_at) < self.lock_timeout);
                if record_lock.holders.is_empty() {
                    ds_locks.remove(key);
                }
            }
        }
    }

    /// Purge all stale locks across all datasets.
    pub fn purge_all_stale(&mut self) {
        let now = Instant::now();
        let timeout = self.lock_timeout;
        let mut empty_datasets = Vec::new();
        for (ds_name, ds_locks) in &mut self.locks {
            let mut empty_keys = Vec::new();
            for (key, record_lock) in ds_locks.iter_mut() {
                record_lock
                    .holders
                    .retain(|h| now.duration_since(h.acquired_at) < timeout);
                if record_lock.holders.is_empty() {
                    empty_keys.push(key.clone());
                }
            }
            for k in empty_keys {
                ds_locks.remove(&k);
            }
            if ds_locks.is_empty() {
                empty_datasets.push(ds_name.clone());
            }
        }
        for ds in empty_datasets {
            self.locks.remove(&ds);
        }
    }
}

impl Default for LockManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Dataset-level lock manager enforcing z/OS DISP semantics.
///
/// - `DISP=OLD` / `DISP=NEW` / `DISP=MOD` → exclusive lock
/// - `DISP=SHR` → shared lock (multiple concurrent readers)
pub struct DatasetLockManager {
    /// Locks keyed by dataset name.
    locks: HashMap<String, DatasetLock>,
}

/// Dataset-level lock state.
#[derive(Debug)]
struct DatasetLock {
    mode: LockMode,
    holders: Vec<String>,
}

impl DatasetLockManager {
    /// Create a new dataset lock manager.
    pub fn new() -> Self {
        Self {
            locks: HashMap::new(),
        }
    }

    /// Acquire a dataset-level lock.
    ///
    /// `disp` should be one of: "OLD", "NEW", "MOD" (exclusive), "SHR" (shared).
    pub fn acquire(&mut self, dataset: &str, disp: &str, owner: &str) -> Result<(), DatasetError> {
        let mode = match disp {
            "OLD" | "NEW" | "MOD" => LockMode::Exclusive,
            "SHR" => LockMode::Shared,
            _ => LockMode::Shared,
        };

        match self.locks.get(dataset) {
            None => {
                self.locks.insert(
                    dataset.to_string(),
                    DatasetLock {
                        mode,
                        holders: vec![owner.to_string()],
                    },
                );
                Ok(())
            }
            Some(existing) => {
                if existing.holders.iter().any(|h| h == owner) {
                    return Ok(()); // Re-entrant
                }

                match (existing.mode, mode) {
                    (LockMode::Shared, LockMode::Shared) => {
                        let dl = self.locks.get_mut(dataset).unwrap();
                        dl.holders.push(owner.to_string());
                        Ok(())
                    }
                    _ => Err(DatasetError::InUse {
                        name: format!(
                            "{} (held by {:?} in {:?} mode)",
                            dataset, existing.holders, existing.mode
                        ),
                    }),
                }
            }
        }
    }

    /// Release a dataset-level lock.
    pub fn release(&mut self, dataset: &str, owner: &str) {
        if let Some(dl) = self.locks.get_mut(dataset) {
            dl.holders.retain(|h| h != owner);
            if dl.holders.is_empty() {
                self.locks.remove(dataset);
            }
        }
    }

    /// Check if a dataset is locked.
    pub fn is_locked(&self, dataset: &str) -> bool {
        self.locks
            .get(dataset)
            .is_some_and(|dl| !dl.holders.is_empty())
    }

    /// Get the current lock mode for a dataset, if locked.
    pub fn lock_mode(&self, dataset: &str) -> Option<LockMode> {
        self.locks.get(dataset).map(|dl| dl.mode)
    }
}

impl Default for DatasetLockManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---- Record-level lock tests (Epic 603.1) ----

    /// Story 603.1: Shared locks can coexist.
    #[test]
    fn test_shared_locks_coexist() {
        let mut mgr = LockManager::new();
        mgr.acquire("MY.DS", "key1", LockMode::Shared, "JOB1")
            .unwrap();
        mgr.acquire("MY.DS", "key1", LockMode::Shared, "JOB2")
            .unwrap();

        let holders = mgr.holders("MY.DS", "key1");
        assert_eq!(holders.len(), 2);
    }

    /// Story 603.1: Exclusive lock blocks others.
    #[test]
    fn test_exclusive_blocks() {
        let mut mgr = LockManager::new();
        mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB1")
            .unwrap();

        let result = mgr.acquire("MY.DS", "key1", LockMode::Shared, "JOB2");
        assert!(result.is_err());

        let result = mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB2");
        assert!(result.is_err());
    }

    /// Story 603.1: Shared blocks exclusive.
    #[test]
    fn test_shared_blocks_exclusive() {
        let mut mgr = LockManager::new();
        mgr.acquire("MY.DS", "key1", LockMode::Shared, "JOB1")
            .unwrap();

        let result = mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB2");
        assert!(result.is_err());
    }

    /// Story 603.1: Release frees lock.
    #[test]
    fn test_release_frees_lock() {
        let mut mgr = LockManager::new();
        mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB1")
            .unwrap();
        mgr.release("MY.DS", "key1", "JOB1");

        assert!(!mgr.is_locked("MY.DS", "key1"));

        // Now another job can acquire
        mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB2")
            .unwrap();
    }

    /// Story 603.1: Re-entrant lock succeeds.
    #[test]
    fn test_reentrant_lock() {
        let mut mgr = LockManager::new();
        mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB1")
            .unwrap();
        // Same owner re-acquiring should succeed
        mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB1")
            .unwrap();
    }

    /// Story 603.1: Release all by owner.
    #[test]
    fn test_release_all() {
        let mut mgr = LockManager::new();
        mgr.acquire("DS1", "k1", LockMode::Shared, "JOB1")
            .unwrap();
        mgr.acquire("DS2", "k2", LockMode::Exclusive, "JOB1")
            .unwrap();
        mgr.acquire("DS1", "k1", LockMode::Shared, "JOB2")
            .unwrap();

        mgr.release_all("JOB1");

        // DS1:k1 still held by JOB2
        assert!(mgr.is_locked("DS1", "k1"));
        // DS2:k2 released
        assert!(!mgr.is_locked("DS2", "k2"));
    }

    /// Story 603.2: Stale lock detection.
    #[test]
    fn test_stale_lock_detection() {
        // Use a very short timeout
        let mut mgr = LockManager::with_timeout(Duration::from_millis(1));
        mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB1")
            .unwrap();

        // Wait for lock to become stale
        std::thread::sleep(Duration::from_millis(10));

        // Should be able to acquire because stale lock gets purged
        mgr.acquire("MY.DS", "key1", LockMode::Exclusive, "JOB2")
            .unwrap();
    }

    /// Story 603.2: Purge all stale locks.
    #[test]
    fn test_purge_all_stale() {
        let mut mgr = LockManager::with_timeout(Duration::from_millis(1));
        mgr.acquire("DS1", "k1", LockMode::Shared, "JOB1")
            .unwrap();
        mgr.acquire("DS2", "k2", LockMode::Exclusive, "JOB2")
            .unwrap();

        std::thread::sleep(Duration::from_millis(10));
        mgr.purge_all_stale();

        assert!(!mgr.is_locked("DS1", "k1"));
        assert!(!mgr.is_locked("DS2", "k2"));
    }

    // ---- Dataset-level lock tests (Epic 603.3) ----

    /// Story 603.3: DISP=SHR allows multiple readers.
    #[test]
    fn test_dataset_shr_multiple() {
        let mut mgr = DatasetLockManager::new();
        mgr.acquire("MY.DATASET", "SHR", "JOB1").unwrap();
        mgr.acquire("MY.DATASET", "SHR", "JOB2").unwrap();
        assert!(mgr.is_locked("MY.DATASET"));
        assert_eq!(mgr.lock_mode("MY.DATASET"), Some(LockMode::Shared));
    }

    /// Story 603.3: DISP=OLD gives exclusive.
    #[test]
    fn test_dataset_old_exclusive() {
        let mut mgr = DatasetLockManager::new();
        mgr.acquire("MY.DATASET", "OLD", "JOB1").unwrap();

        let result = mgr.acquire("MY.DATASET", "SHR", "JOB2");
        assert!(result.is_err());
    }

    /// Story 603.3: DISP=SHR blocks DISP=OLD.
    #[test]
    fn test_dataset_shr_blocks_old() {
        let mut mgr = DatasetLockManager::new();
        mgr.acquire("MY.DATASET", "SHR", "JOB1").unwrap();

        let result = mgr.acquire("MY.DATASET", "OLD", "JOB2");
        assert!(result.is_err());
    }

    /// Story 603.3: Release dataset lock.
    #[test]
    fn test_dataset_release() {
        let mut mgr = DatasetLockManager::new();
        mgr.acquire("MY.DATASET", "OLD", "JOB1").unwrap();
        mgr.release("MY.DATASET", "JOB1");

        assert!(!mgr.is_locked("MY.DATASET"));
        mgr.acquire("MY.DATASET", "OLD", "JOB2").unwrap();
    }

    /// Story 603.3: DISP=NEW is exclusive.
    #[test]
    fn test_dataset_new_exclusive() {
        let mut mgr = DatasetLockManager::new();
        mgr.acquire("MY.DATASET", "NEW", "JOB1").unwrap();

        let result = mgr.acquire("MY.DATASET", "SHR", "JOB2");
        assert!(result.is_err());
        assert_eq!(mgr.lock_mode("MY.DATASET"), Some(LockMode::Exclusive));
    }

    /// Story 603.3: DISP=MOD is exclusive.
    #[test]
    fn test_dataset_mod_exclusive() {
        let mut mgr = DatasetLockManager::new();
        mgr.acquire("MY.DATASET", "MOD", "JOB1").unwrap();

        let result = mgr.acquire("MY.DATASET", "OLD", "JOB2");
        assert!(result.is_err());
    }
}
