//! Initiator Pool Management — dynamic start/stop, job dispatch, WLM integration.
//!
//! Implements JES2 initiator management:
//! - **Initiator pool**: Start/stop initiators dynamically with `$SI`/`$PI`.
//! - **Job selection**: Priority-ordered class-based selection and dispatch.
//! - **Draining**: Initiators stop after current job completes.
//! - **WLM-managed initiators**: Auto-scale based on workload goals.

use crate::commands::Initiator;
use crate::job::{JobClass, JobId};
use crate::queue::Jes2;

// ---------------------------------------------------------------------------
//  Initiator state
// ---------------------------------------------------------------------------

/// Operational state of an initiator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InitiatorState {
    /// Initiator is stopped and not accepting work.
    Stopped,
    /// Initiator is active and ready to select jobs.
    Active,
    /// Initiator is draining — will stop after current job completes.
    Draining,
}

// ---------------------------------------------------------------------------
//  Managed Initiator
// ---------------------------------------------------------------------------

/// An initiator with extended management state.
#[derive(Debug, Clone)]
pub struct ManagedInitiator {
    /// Underlying initiator data.
    pub inner: Initiator,
    /// Operational state.
    pub state: InitiatorState,
    /// Whether this initiator is WLM-managed (vs. operator-managed).
    pub wlm_managed: bool,
    /// Number of jobs this initiator has processed since last start.
    pub jobs_processed: u64,
}

impl ManagedInitiator {
    /// Create a new managed initiator.
    pub fn new(id: u32, classes: Vec<JobClass>) -> Self {
        Self {
            inner: Initiator::new(id, classes),
            state: InitiatorState::Stopped,
            wlm_managed: false,
            jobs_processed: 0,
        }
    }

    /// Create a WLM-managed initiator.
    pub fn wlm_managed(id: u32, classes: Vec<JobClass>) -> Self {
        Self {
            inner: Initiator::new(id, classes),
            state: InitiatorState::Stopped,
            wlm_managed: true,
            jobs_processed: 0,
        }
    }

    /// Check if this initiator is idle (active but no current job).
    pub fn is_idle(&self) -> bool {
        self.state == InitiatorState::Active && self.inner.current_job.is_none()
    }

    /// Check if this initiator can accept work.
    pub fn can_accept_work(&self) -> bool {
        self.state == InitiatorState::Active && self.inner.current_job.is_none()
    }
}

// ---------------------------------------------------------------------------
//  Initiator Manager
// ---------------------------------------------------------------------------

/// Manages the JES2 initiator pool.
///
/// Handles starting/stopping initiators, dispatching jobs to idle initiators,
/// draining, and WLM-managed auto-scaling.
#[derive(Debug, Clone)]
pub struct InitiatorManager {
    /// Pool of managed initiators, indexed by initiator ID.
    initiators: Vec<ManagedInitiator>,
    /// Next initiator ID to assign.
    next_id: u32,
}

impl Default for InitiatorManager {
    fn default() -> Self {
        Self::new()
    }
}

impl InitiatorManager {
    /// Create an empty initiator manager.
    pub fn new() -> Self {
        Self {
            initiators: Vec::new(),
            next_id: 1,
        }
    }

    /// Add an initiator to the pool (stopped).
    pub fn add(&mut self, classes: Vec<JobClass>) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.initiators.push(ManagedInitiator::new(id, classes));
        id
    }

    /// Add a WLM-managed initiator to the pool.
    pub fn add_wlm(&mut self, classes: Vec<JobClass>) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.initiators.push(ManagedInitiator::wlm_managed(id, classes));
        id
    }

    /// Start initiator(s) by ID range.
    ///
    /// Corresponds to `$SI<start>-<end>`.
    pub fn start_range(
        &mut self,
        start_id: u32,
        end_id: u32,
        classes: Option<&[JobClass]>,
    ) -> Vec<u32> {
        let mut started = Vec::new();
        for init in &mut self.initiators {
            if init.inner.id >= start_id && init.inner.id <= end_id {
                if let Some(cls) = classes {
                    init.inner.classes = cls.to_vec();
                }
                init.state = InitiatorState::Active;
                init.inner.active = true;
                started.push(init.inner.id);
            }
        }
        started
    }

    /// Start a single initiator by ID.
    pub fn start(&mut self, id: u32) -> bool {
        !self.start_range(id, id, None).is_empty()
    }

    /// Drain an initiator — it will stop after the current job completes.
    ///
    /// Corresponds to `$PI<id>`.
    pub fn drain(&mut self, id: u32) -> bool {
        if let Some(init) = self.initiators.iter_mut().find(|i| i.inner.id == id) {
            if init.state == InitiatorState::Active {
                if init.inner.current_job.is_some() {
                    init.state = InitiatorState::Draining;
                } else {
                    init.state = InitiatorState::Stopped;
                    init.inner.active = false;
                }
                return true;
            }
        }
        false
    }

    /// Stop an initiator immediately (forces stop even if busy).
    pub fn stop(&mut self, id: u32) -> bool {
        if let Some(init) = self.initiators.iter_mut().find(|i| i.inner.id == id) {
            init.state = InitiatorState::Stopped;
            init.inner.active = false;
            init.inner.current_job = None;
            return true;
        }
        false
    }

    /// Modify initiator classes.
    ///
    /// Corresponds to `$TI<id>,CLASSES=...`.
    pub fn modify_classes(&mut self, id: u32, classes: Vec<JobClass>) -> bool {
        if let Some(init) = self.initiators.iter_mut().find(|i| i.inner.id == id) {
            init.inner.classes = classes;
            return true;
        }
        false
    }

    /// Get an initiator by ID.
    pub fn get(&self, id: u32) -> Option<&ManagedInitiator> {
        self.initiators.iter().find(|i| i.inner.id == id)
    }

    /// Get the total number of initiators.
    pub fn count(&self) -> usize {
        self.initiators.len()
    }

    /// Get the number of active initiators.
    pub fn active_count(&self) -> usize {
        self.initiators
            .iter()
            .filter(|i| i.state == InitiatorState::Active)
            .count()
    }

    /// Get the number of idle (active, no job) initiators.
    pub fn idle_count(&self) -> usize {
        self.initiators.iter().filter(|i| i.is_idle()).count()
    }

    /// Get the number of busy (active, has job) initiators.
    pub fn busy_count(&self) -> usize {
        self.initiators
            .iter()
            .filter(|i| i.state != InitiatorState::Stopped && i.inner.current_job.is_some())
            .count()
    }

    /// Get the number of draining initiators.
    pub fn draining_count(&self) -> usize {
        self.initiators
            .iter()
            .filter(|i| i.state == InitiatorState::Draining)
            .count()
    }

    /// Dispatch jobs from the queue to idle initiators.
    ///
    /// For each idle initiator, selects the highest-priority Ready job
    /// matching the initiator's classes and assigns it.
    ///
    /// Returns the list of (initiator_id, job_id) assignments.
    pub fn dispatch(&mut self, jes: &mut Jes2) -> Vec<(u32, JobId)> {
        let mut assignments = Vec::new();

        for init in &mut self.initiators {
            if !init.can_accept_work() {
                continue;
            }
            // Select highest-priority Ready job for this initiator's classes.
            if let Some(job_id) = jes.select_for_classes(&init.inner.classes) {
                // Advance from Ready → Running.
                if jes.advance(job_id).is_ok() {
                    init.inner.current_job = Some(job_id);
                    assignments.push((init.inner.id, job_id));
                }
            }
        }

        assignments
    }

    /// Notify that a job has completed on an initiator.
    ///
    /// Advances the job to Output state and frees the initiator.
    /// If the initiator was draining, it transitions to Stopped.
    pub fn job_complete(&mut self, id: u32, jes: &mut Jes2) -> Option<JobId> {
        let init = self.initiators.iter_mut().find(|i| i.inner.id == id)?;
        let job_id = init.inner.current_job.take()?;

        // Advance job from Running → Output.
        let _ = jes.advance(job_id);

        init.jobs_processed += 1;

        // If draining, stop the initiator now.
        if init.state == InitiatorState::Draining {
            init.state = InitiatorState::Stopped;
            init.inner.active = false;
        }

        Some(job_id)
    }

    /// Generate a display report of all initiators.
    pub fn display_all(&self) -> Vec<String> {
        let mut lines = Vec::new();
        for init in &self.initiators {
            let state_str = match init.state {
                InitiatorState::Stopped => "INACTIVE",
                InitiatorState::Active => {
                    if init.inner.current_job.is_some() {
                        "ACTIVE"
                    } else {
                        "IDLE"
                    }
                }
                InitiatorState::Draining => "DRAINING",
            };
            let classes: String = init
                .inner
                .classes
                .iter()
                .filter_map(|c| match c {
                    JobClass::Standard(ch) => Some(*ch),
                    _ => None,
                })
                .collect();
            let job_info = match &init.inner.current_job {
                Some(id) => format!(", JOB={id}"),
                None => String::new(),
            };
            let wlm = if init.wlm_managed { " WLM" } else { "" };
            lines.push(format!(
                "$HASP893 INIT{:02} STATUS={},CLASSES={}{}{},PROCESSED={}",
                init.inner.id, state_str, classes, job_info, wlm, init.jobs_processed
            ));
        }
        lines
    }

    // -----------------------------------------------------------------------
    //  WLM integration
    // -----------------------------------------------------------------------

    /// WLM callback: request additional initiators to meet workload goals.
    ///
    /// Starts up to `count` stopped WLM-managed initiators.
    /// Returns the number actually started.
    pub fn wlm_start_initiators(&mut self, count: usize) -> usize {
        let mut started = 0;
        for init in &mut self.initiators {
            if started >= count {
                break;
            }
            if init.wlm_managed && init.state == InitiatorState::Stopped {
                init.state = InitiatorState::Active;
                init.inner.active = true;
                started += 1;
            }
        }
        started
    }

    /// WLM callback: drain excess WLM-managed initiators.
    ///
    /// Drains up to `count` idle WLM-managed initiators.
    /// Returns the number drained.
    pub fn wlm_drain_initiators(&mut self, count: usize) -> usize {
        let mut drained = 0;
        for init in &mut self.initiators {
            if drained >= count {
                break;
            }
            if init.wlm_managed && init.is_idle() {
                init.state = InitiatorState::Stopped;
                init.inner.active = false;
                drained += 1;
            }
        }
        drained
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::queue::Jes2;

    fn std_class(c: char) -> JobClass {
        JobClass::Standard(c)
    }

    // ─── J101.1: Initiator Pool ───

    #[test]
    fn test_add_and_start_initiator() {
        let mut mgr = InitiatorManager::new();
        let id = mgr.add(vec![std_class('A'), std_class('B')]);
        assert_eq!(id, 1);
        assert_eq!(mgr.count(), 1);
        assert_eq!(mgr.active_count(), 0);

        mgr.start(id);
        assert_eq!(mgr.active_count(), 1);
    }

    #[test]
    fn test_start_range() {
        let mut mgr = InitiatorManager::new();
        mgr.add(vec![std_class('A')]);
        mgr.add(vec![std_class('A')]);
        mgr.add(vec![std_class('A')]);
        mgr.add(vec![std_class('B')]);
        mgr.add(vec![std_class('B')]);

        // Start initiators 1-5 with classes A,B.
        let started = mgr.start_range(
            1,
            5,
            Some(&[std_class('A'), std_class('B')]),
        );
        assert_eq!(started.len(), 5);
        assert_eq!(mgr.active_count(), 5);

        // All should now have classes A,B.
        let init = mgr.get(3).unwrap();
        assert_eq!(init.inner.classes.len(), 2);
    }

    #[test]
    fn test_drain_idle_initiator() {
        let mut mgr = InitiatorManager::new();
        let id = mgr.add(vec![std_class('A')]);
        mgr.start(id);

        // Drain an idle initiator → immediately stops.
        assert!(mgr.drain(id));
        assert_eq!(mgr.get(id).unwrap().state, InitiatorState::Stopped);
    }

    #[test]
    fn test_drain_busy_initiator() {
        let mut mgr = InitiatorManager::new();
        let id = mgr.add(vec![std_class('A')]);
        mgr.start(id);

        // Simulate a busy initiator.
        mgr.initiators[0].inner.current_job = Some(JobId(1));

        // Drain → should go to Draining state.
        assert!(mgr.drain(id));
        assert_eq!(mgr.get(id).unwrap().state, InitiatorState::Draining);
        assert_eq!(mgr.draining_count(), 1);
    }

    #[test]
    fn test_stop_initiator() {
        let mut mgr = InitiatorManager::new();
        let id = mgr.add(vec![std_class('A')]);
        mgr.start(id);

        assert!(mgr.stop(id));
        assert_eq!(mgr.get(id).unwrap().state, InitiatorState::Stopped);
    }

    #[test]
    fn test_modify_classes() {
        let mut mgr = InitiatorManager::new();
        let id = mgr.add(vec![std_class('A')]);
        mgr.modify_classes(id, vec![std_class('A'), std_class('B'), std_class('C')]);

        let init = mgr.get(id).unwrap();
        assert_eq!(init.inner.classes.len(), 3);
    }

    #[test]
    fn test_dispatch_jobs() {
        let mut mgr = InitiatorManager::new();
        mgr.add(vec![std_class('A')]);
        mgr.add(vec![std_class('A')]);
        mgr.start_range(1, 2, None);

        let mut jes = Jes2::new();
        let id1 = jes.submit("JOB1", 'A', 10, false);
        let id2 = jes.submit("JOB2", 'A', 5, false);

        // Advance jobs to Ready state.
        jes.advance(id1).unwrap(); // Input → Conversion
        jes.advance(id1).unwrap(); // Conversion → Ready
        jes.advance(id2).unwrap();
        jes.advance(id2).unwrap();

        let assignments = mgr.dispatch(&mut jes);
        assert_eq!(assignments.len(), 2);
        assert_eq!(mgr.busy_count(), 2);
        assert_eq!(mgr.idle_count(), 0);
    }

    #[test]
    fn test_dispatch_priority_order() {
        let mut mgr = InitiatorManager::new();
        mgr.add(vec![std_class('A')]);
        mgr.start(1);

        let mut jes = Jes2::new();
        let low = jes.submit("LOW", 'A', 1, false);
        let high = jes.submit("HIGH", 'A', 15, false);

        // Advance to Ready.
        for id in &[low, high] {
            jes.advance(*id).unwrap();
            jes.advance(*id).unwrap();
        }

        let assignments = mgr.dispatch(&mut jes);
        assert_eq!(assignments.len(), 1);
        // Should have selected the higher-priority job.
        assert_eq!(assignments[0].1, high);
    }

    #[test]
    fn test_job_complete_frees_initiator() {
        let mut mgr = InitiatorManager::new();
        let init_id = mgr.add(vec![std_class('A')]);
        mgr.start(init_id);

        let mut jes = Jes2::new();
        let job_id = jes.submit("JOB1", 'A', 10, false);
        jes.advance(job_id).unwrap();
        jes.advance(job_id).unwrap();

        mgr.dispatch(&mut jes);
        assert_eq!(mgr.busy_count(), 1);

        let completed = mgr.job_complete(init_id, &mut jes);
        assert_eq!(completed, Some(job_id));
        assert_eq!(mgr.idle_count(), 1);
        assert_eq!(mgr.busy_count(), 0);
    }

    #[test]
    fn test_draining_stops_after_job_complete() {
        let mut mgr = InitiatorManager::new();
        let init_id = mgr.add(vec![std_class('A')]);
        mgr.start(init_id);

        let mut jes = Jes2::new();
        let job_id = jes.submit("JOB1", 'A', 10, false);
        jes.advance(job_id).unwrap();
        jes.advance(job_id).unwrap();

        mgr.dispatch(&mut jes);

        // Drain while busy → Draining.
        mgr.drain(init_id);
        assert_eq!(mgr.get(init_id).unwrap().state, InitiatorState::Draining);

        // Job completes → initiator stops.
        mgr.job_complete(init_id, &mut jes);
        assert_eq!(mgr.get(init_id).unwrap().state, InitiatorState::Stopped);
        assert_eq!(mgr.active_count(), 0);
    }

    #[test]
    fn test_display_all() {
        let mut mgr = InitiatorManager::new();
        mgr.add(vec![std_class('A'), std_class('B')]);
        mgr.start(1);

        let lines = mgr.display_all();
        assert_eq!(lines.len(), 1);
        assert!(lines[0].contains("IDLE"));
        assert!(lines[0].contains("CLASSES=AB"));
    }

    #[test]
    fn test_jobs_processed_counter() {
        let mut mgr = InitiatorManager::new();
        let init_id = mgr.add(vec![std_class('A')]);
        mgr.start(init_id);

        let mut jes = Jes2::new();
        for i in 0..3 {
            let name = format!("JOB{i}");
            let job_id = jes.submit(&name, 'A', 10, false);
            jes.advance(job_id).unwrap();
            jes.advance(job_id).unwrap();
            mgr.dispatch(&mut jes);
            mgr.job_complete(init_id, &mut jes);
        }

        assert_eq!(mgr.get(init_id).unwrap().jobs_processed, 3);
    }

    // ─── J101.2: WLM-Managed Initiators ───

    #[test]
    fn test_wlm_managed_initiator() {
        let mut mgr = InitiatorManager::new();
        let id = mgr.add_wlm(vec![std_class('A')]);
        assert!(mgr.get(id).unwrap().wlm_managed);
    }

    #[test]
    fn test_wlm_start_initiators() {
        let mut mgr = InitiatorManager::new();
        mgr.add_wlm(vec![std_class('A')]);
        mgr.add_wlm(vec![std_class('A')]);
        mgr.add_wlm(vec![std_class('A')]);
        mgr.add(vec![std_class('B')]); // Not WLM-managed.

        // Start 2 WLM initiators.
        let started = mgr.wlm_start_initiators(2);
        assert_eq!(started, 2);
        assert_eq!(mgr.active_count(), 2);
    }

    #[test]
    fn test_wlm_drain_excess() {
        let mut mgr = InitiatorManager::new();
        mgr.add_wlm(vec![std_class('A')]);
        mgr.add_wlm(vec![std_class('A')]);
        mgr.add_wlm(vec![std_class('A')]);
        mgr.wlm_start_initiators(3);
        assert_eq!(mgr.active_count(), 3);

        let drained = mgr.wlm_drain_initiators(2);
        assert_eq!(drained, 2);
        assert_eq!(mgr.active_count(), 1);
    }

    #[test]
    fn test_wlm_does_not_drain_busy() {
        let mut mgr = InitiatorManager::new();
        mgr.add_wlm(vec![std_class('A')]);
        mgr.wlm_start_initiators(1);

        // Simulate busy.
        mgr.initiators[0].inner.current_job = Some(JobId(1));

        // Should not drain a busy WLM initiator.
        let drained = mgr.wlm_drain_initiators(1);
        assert_eq!(drained, 0);
    }

    #[test]
    fn test_display_wlm_flag() {
        let mut mgr = InitiatorManager::new();
        mgr.add_wlm(vec![std_class('A')]);
        mgr.wlm_start_initiators(1);

        let lines = mgr.display_all();
        assert!(lines[0].contains("WLM"));
    }

    #[test]
    fn test_idle_and_busy_counts() {
        let mut mgr = InitiatorManager::new();
        mgr.add(vec![std_class('A')]);
        mgr.add(vec![std_class('A')]);
        mgr.start_range(1, 2, None);

        assert_eq!(mgr.idle_count(), 2);
        assert_eq!(mgr.busy_count(), 0);

        mgr.initiators[0].inner.current_job = Some(JobId(1));
        assert_eq!(mgr.idle_count(), 1);
        assert_eq!(mgr.busy_count(), 1);
    }
}
