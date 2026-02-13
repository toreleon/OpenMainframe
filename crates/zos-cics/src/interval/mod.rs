//! CICS Interval Control Services.
//!
//! Provides START, RETRIEVE, DELAY, and CANCEL commands.

use std::collections::HashMap;
use std::time::{Duration, Instant};

/// A scheduled transaction.
#[derive(Debug, Clone)]
pub struct ScheduledTransaction {
    /// Transaction ID to start
    pub transid: String,
    /// Request ID for cancellation
    pub reqid: Option<String>,
    /// Data to pass via RETRIEVE
    pub data: Option<Vec<u8>>,
    /// When to start (absolute time)
    pub start_time: Instant,
    /// Terminal ID (if applicable)
    pub termid: Option<String>,
    /// Interval type
    pub interval_type: IntervalType,
}

/// Type of interval specification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntervalType {
    /// After a time interval
    Interval,
    /// At a specific time
    Time,
    /// Immediately (for asynchronous start)
    Immediate,
}

/// Interval Control Manager.
#[derive(Debug, Default)]
pub struct IntervalManager {
    /// Scheduled transactions
    scheduled: Vec<ScheduledTransaction>,
    /// Data waiting for RETRIEVE (keyed by transid)
    retrieve_data: HashMap<String, Vec<u8>>,
    /// Request ID counter
    next_reqid: u32,
}

impl IntervalManager {
    /// Create a new interval manager.
    pub fn new() -> Self {
        Self {
            scheduled: Vec::new(),
            retrieve_data: HashMap::new(),
            next_reqid: 1,
        }
    }

    /// Schedule a transaction to start after an interval.
    ///
    /// Interval is specified as HHMMSS (e.g., 001500 = 15 minutes).
    pub fn start_interval(
        &mut self,
        transid: &str,
        interval: u32,
        data: Option<Vec<u8>>,
        reqid: Option<&str>,
        termid: Option<&str>,
    ) -> String {
        let duration = parse_interval(interval);
        let start_time = Instant::now() + duration;

        let reqid = reqid
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                let id = format!("REQ{:05}", self.next_reqid);
                self.next_reqid += 1;
                id
            });

        let scheduled = ScheduledTransaction {
            transid: transid.to_string(),
            reqid: Some(reqid.clone()),
            data,
            start_time,
            termid: termid.map(|s| s.to_string()),
            interval_type: IntervalType::Interval,
        };

        self.scheduled.push(scheduled);
        reqid
    }

    /// Schedule a transaction to start at a specific time.
    ///
    /// Time is specified as HHMMSS (e.g., 143000 = 2:30 PM).
    pub fn start_at_time(
        &mut self,
        transid: &str,
        time: u32,
        data: Option<Vec<u8>>,
        reqid: Option<&str>,
        termid: Option<&str>,
    ) -> String {
        // For simplicity, treat time as an offset from now
        // In real implementation, would calculate actual clock time
        let duration = parse_interval(time);
        let start_time = Instant::now() + duration;

        let reqid = reqid
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                let id = format!("REQ{:05}", self.next_reqid);
                self.next_reqid += 1;
                id
            });

        let scheduled = ScheduledTransaction {
            transid: transid.to_string(),
            reqid: Some(reqid.clone()),
            data,
            start_time,
            termid: termid.map(|s| s.to_string()),
            interval_type: IntervalType::Time,
        };

        self.scheduled.push(scheduled);
        reqid
    }

    /// Cancel a scheduled transaction.
    pub fn cancel(&mut self, reqid: &str) -> IntervalResult<()> {
        let pos = self
            .scheduled
            .iter()
            .position(|s| s.reqid.as_deref() == Some(reqid));

        match pos {
            Some(idx) => {
                self.scheduled.remove(idx);
                Ok(())
            }
            None => Err(IntervalError::NotFound),
        }
    }

    /// Get transactions ready to start.
    pub fn get_ready_transactions(&mut self) -> Vec<ScheduledTransaction> {
        let now = Instant::now();
        let mut ready = Vec::new();
        let mut remaining = Vec::new();

        for sched in self.scheduled.drain(..) {
            if sched.start_time <= now {
                // Store data for RETRIEVE
                if let Some(data) = &sched.data {
                    self.retrieve_data.insert(sched.transid.clone(), data.clone());
                }
                ready.push(sched);
            } else {
                remaining.push(sched);
            }
        }

        self.scheduled = remaining;
        ready
    }

    /// Retrieve data passed from START command.
    pub fn retrieve(&mut self, transid: &str) -> IntervalResult<Vec<u8>> {
        self.retrieve_data
            .remove(transid)
            .ok_or(IntervalError::NoData)
    }

    /// Delay current task.
    ///
    /// In a real implementation, this would suspend the task.
    /// Here we just return the duration to wait.
    pub fn delay(&self, interval: u32) -> Duration {
        parse_interval(interval)
    }

    /// Get number of scheduled transactions.
    pub fn scheduled_count(&self) -> usize {
        self.scheduled.len()
    }
}

/// Parse HHMMSS interval to Duration.
fn parse_interval(hhmmss: u32) -> Duration {
    let hours = hhmmss / 10000;
    let minutes = (hhmmss / 100) % 100;
    let seconds = hhmmss % 100;

    Duration::from_secs((hours * 3600 + minutes * 60 + seconds) as u64)
}

/// Interval control errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntervalError {
    /// Request not found
    NotFound,
    /// No data available for RETRIEVE
    NoData,
    /// Invalid interval
    InvalidInterval,
    /// Invalid time
    InvalidTime,
}

impl std::fmt::Display for IntervalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntervalError::NotFound => write!(f, "Request not found"),
            IntervalError::NoData => write!(f, "No data available for RETRIEVE"),
            IntervalError::InvalidInterval => write!(f, "Invalid interval"),
            IntervalError::InvalidTime => write!(f, "Invalid time"),
        }
    }
}

impl std::error::Error for IntervalError {}

/// Result type for interval operations.
pub type IntervalResult<T> = Result<T, IntervalError>;

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_parse_interval() {
        // 1 hour 30 minutes 45 seconds
        let duration = parse_interval(13045);
        assert_eq!(duration, Duration::from_secs(1 * 3600 + 30 * 60 + 45));

        // 15 minutes
        let duration = parse_interval(1500);
        assert_eq!(duration, Duration::from_secs(15 * 60));

        // 30 seconds
        let duration = parse_interval(30);
        assert_eq!(duration, Duration::from_secs(30));
    }

    #[test]
    fn test_start_interval() {
        let mut mgr = IntervalManager::new();

        let reqid = mgr.start_interval(
            "TRAN",
            100, // 1 second
            Some(b"Test data".to_vec()),
            None,
            None,
        );

        assert!(!reqid.is_empty());
        assert_eq!(mgr.scheduled_count(), 1);
    }

    #[test]
    fn test_cancel() {
        let mut mgr = IntervalManager::new();

        let reqid = mgr.start_interval("TRAN", 1000, None, None, None);
        assert_eq!(mgr.scheduled_count(), 1);

        mgr.cancel(&reqid).unwrap();
        assert_eq!(mgr.scheduled_count(), 0);
    }

    #[test]
    fn test_get_ready_transactions() {
        let mut mgr = IntervalManager::new();

        // Schedule immediate transaction (0 interval)
        mgr.start_interval("TRAN1", 0, None, None, None);

        // Get ready transactions
        let ready = mgr.get_ready_transactions();
        assert_eq!(ready.len(), 1);
        assert_eq!(ready[0].transid, "TRAN1");
    }

    #[test]
    fn test_retrieve() {
        let mut mgr = IntervalManager::new();

        // Start with data
        mgr.start_interval("TRAN", 0, Some(b"Retrieve me".to_vec()), None, None);

        // Process ready transactions (moves data to retrieve_data)
        let _ready = mgr.get_ready_transactions();

        // Retrieve the data
        let data = mgr.retrieve("TRAN").unwrap();
        assert_eq!(data, b"Retrieve me");

        // Second retrieve should fail
        assert!(mgr.retrieve("TRAN").is_err());
    }

    #[test]
    fn test_delay() {
        let mgr = IntervalManager::new();

        let duration = mgr.delay(130); // 1 minute 30 seconds
        assert_eq!(duration, Duration::from_secs(90));
    }
}
