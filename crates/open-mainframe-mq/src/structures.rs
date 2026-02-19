//! MQ Data Structures — MQMD, MQOD, MQGMO, MQPMO.
//!
//! These are the core MQ data structures used for message operations.

// ---------------------------------------------------------------------------
//  MQMD — Message Descriptor
// ---------------------------------------------------------------------------

/// Message persistence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum MqPersistence {
    /// Message is not persistent (lost on queue manager restart).
    NotPersistent,
    /// Message is persistent (survives queue manager restart).
    Persistent,
    /// Use the queue's default persistence.
    AsQueueDef,
}

impl Default for MqPersistence {
    fn default() -> Self {
        Self::AsQueueDef
    }
}

/// Message priority.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum MqPriority {
    /// Use queue default priority.
    AsQueueDef,
    /// Explicit priority (0-9, higher = more important).
    Priority(u8),
}

impl Default for MqPriority {
    fn default() -> Self {
        Self::AsQueueDef
    }
}

/// Message type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum MqMsgType {
    /// Normal datagram message.
    Datagram,
    /// Request message (expects a reply).
    Request,
    /// Reply message.
    Reply,
    /// Report message.
    Report,
}

impl Default for MqMsgType {
    fn default() -> Self {
        Self::Datagram
    }
}

/// MQMD — Message Descriptor.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Mqmd {
    /// Structure version.
    pub version: u32,
    /// Message type.
    pub msg_type: MqMsgType,
    /// Message persistence.
    pub persistence: MqPersistence,
    /// Message priority.
    pub priority: MqPriority,
    /// Expiry (tenths of a second, -1 = unlimited).
    pub expiry: i32,
    /// Feedback code (for report messages).
    pub feedback: u32,
    /// Message ID (24 bytes).
    pub msg_id: [u8; 24],
    /// Correlation ID (24 bytes).
    pub correl_id: [u8; 24],
    /// Reply-to queue name.
    pub reply_to_queue: String,
    /// Reply-to queue manager.
    pub reply_to_qmgr: String,
    /// Format name (e.g., "MQSTR", "MQHRF2").
    pub format: String,
    /// Character set ID (CCSID).
    pub ccsid: u32,
    /// Encoding (numeric encoding of data).
    pub encoding: u32,
    /// Put date (YYYYMMDD).
    pub put_date: String,
    /// Put time (HHMMSSTH).
    pub put_time: String,
    /// Application identity data.
    pub appl_identity: String,
    /// Application origin data.
    pub appl_origin: String,
    /// Put application name.
    pub put_appl_name: String,
    /// Put application type.
    pub put_appl_type: u32,
    /// Group ID (24 bytes).
    pub group_id: [u8; 24],
    /// Sequence number in group.
    pub msg_seq_number: u32,
    /// Offset in group.
    pub offset: u32,
    /// Message flags.
    pub msg_flags: u32,
    /// Original length (-1 if not truncated).
    pub original_length: i32,
}

impl Default for Mqmd {
    fn default() -> Self {
        Self {
            version: 2,
            msg_type: MqMsgType::Datagram,
            persistence: MqPersistence::AsQueueDef,
            priority: MqPriority::AsQueueDef,
            expiry: -1,
            feedback: 0,
            msg_id: [0u8; 24],
            correl_id: [0u8; 24],
            reply_to_queue: String::new(),
            reply_to_qmgr: String::new(),
            format: "MQSTR".to_string(),
            ccsid: 819,
            encoding: 546,
            put_date: String::new(),
            put_time: String::new(),
            appl_identity: String::new(),
            appl_origin: String::new(),
            put_appl_name: String::new(),
            put_appl_type: 0,
            group_id: [0u8; 24],
            msg_seq_number: 1,
            offset: 0,
            msg_flags: 0,
            original_length: -1,
        }
    }
}

// ---------------------------------------------------------------------------
//  MQOD — Object Descriptor
// ---------------------------------------------------------------------------

/// MQOD — Object Descriptor (identifies a queue for open).
#[derive(Debug, Clone, Default)]
pub struct Mqod {
    /// Object name (queue name).
    pub object_name: String,
    /// Queue manager name (blank = local).
    pub object_qmgr_name: String,
    /// Dynamic queue name template (for model queues).
    pub dynamic_q_name: String,
    /// Alternate user ID.
    pub alternate_user_id: String,
    /// Resolved queue name (filled by MQOPEN).
    pub resolved_q_name: String,
    /// Resolved queue manager name.
    pub resolved_qmgr_name: String,
}

// ---------------------------------------------------------------------------
//  MQGMO — Get Message Options
// ---------------------------------------------------------------------------

/// MQGMO — Get Message Options.
#[derive(Debug, Clone)]
pub struct Mqgmo {
    /// Wait interval in milliseconds (-1 = unlimited, 0 = no wait).
    pub wait_interval: i32,
    /// Match on message ID.
    pub match_msg_id: bool,
    /// Match on correlation ID.
    pub match_correl_id: bool,
    /// Browse (non-destructive read).
    pub browse: bool,
    /// Browse cursor position.
    pub browse_cursor: usize,
    /// Accept truncated messages.
    pub accept_truncated: bool,
    /// Convert message data.
    pub convert: bool,
    /// Syncpoint control (message under syncpoint).
    pub syncpoint: bool,
    /// No syncpoint (immediate delivery to application).
    pub no_syncpoint: bool,
}

impl Default for Mqgmo {
    fn default() -> Self {
        Self {
            wait_interval: 0,
            match_msg_id: false,
            match_correl_id: false,
            browse: false,
            browse_cursor: 0,
            accept_truncated: false,
            convert: false,
            syncpoint: false,
            no_syncpoint: true,
        }
    }
}

// ---------------------------------------------------------------------------
//  MQPMO — Put Message Options
// ---------------------------------------------------------------------------

/// MQPMO — Put Message Options.
#[derive(Debug, Clone)]
pub struct MqPmo {
    /// Generate a new message ID.
    pub new_msg_id: bool,
    /// Generate a new correlation ID.
    pub new_correl_id: bool,
    /// Syncpoint control.
    pub syncpoint: bool,
    /// No syncpoint.
    pub no_syncpoint: bool,
    /// Resolved queue name (filled by MQPUT).
    pub resolved_q_name: String,
    /// Resolved queue manager name.
    pub resolved_qmgr_name: String,
}

impl Default for MqPmo {
    fn default() -> Self {
        Self {
            new_msg_id: true,
            new_correl_id: false,
            syncpoint: false,
            no_syncpoint: true,
            resolved_q_name: String::new(),
            resolved_qmgr_name: String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mqmd_default() {
        let md = Mqmd::default();
        assert_eq!(md.version, 2);
        assert_eq!(md.msg_type, MqMsgType::Datagram);
        assert_eq!(md.format, "MQSTR");
        assert_eq!(md.ccsid, 819);
        assert_eq!(md.expiry, -1);
    }

    #[test]
    fn test_mqod_default() {
        let od = Mqod::default();
        assert!(od.object_name.is_empty());
    }

    #[test]
    fn test_mqgmo_default() {
        let gmo = Mqgmo::default();
        assert_eq!(gmo.wait_interval, 0);
        assert!(!gmo.browse);
        assert!(gmo.no_syncpoint);
    }

    #[test]
    fn test_mqpmo_default() {
        let pmo = MqPmo::default();
        assert!(pmo.new_msg_id);
        assert!(pmo.no_syncpoint);
    }

    #[test]
    fn test_persistence_default() {
        assert_eq!(MqPersistence::default(), MqPersistence::AsQueueDef);
    }

    #[test]
    fn test_priority_default() {
        assert_eq!(MqPriority::default(), MqPriority::AsQueueDef);
    }

    #[test]
    fn test_msg_type_default() {
        assert_eq!(MqMsgType::default(), MqMsgType::Datagram);
    }
}
