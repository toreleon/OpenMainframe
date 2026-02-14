//! CICS Queue Services.
//!
//! Implements Temporary Storage (TS) and Transient Data (TD) queues.

pub mod ts;
pub mod td;

pub use ts::{TsQueue, TsQueueManager, TsItem, TsType};
pub use td::{TdQueue, TdQueueManager, TdDestType};
