//! MQI (Message Queue Interface) — MQCONN, MQDISC, MQOPEN, MQCLOSE, MQPUT, MQGET.
//!
//! Provides the MQI programming interface for applications.

use crate::core::{MqError, QueueManager};
use crate::structures::{Mqgmo, Mqmd, Mqod, MqPmo};

// ---------------------------------------------------------------------------
//  Open options
// ---------------------------------------------------------------------------

/// Open options for MQOPEN.
#[derive(Debug, Clone, Default)]
pub struct OpenOptions {
    /// Open for output (MQPUT).
    pub output: bool,
    /// Open for input (shared).
    pub input_shared: bool,
    /// Open for input (exclusive).
    pub input_exclusive: bool,
    /// Open for browse.
    pub browse: bool,
    /// Open for inquire.
    pub inquire: bool,
    /// Open for set.
    pub set: bool,
}

// ---------------------------------------------------------------------------
//  Connection / Handle
// ---------------------------------------------------------------------------

/// An MQI connection handle.
#[derive(Debug)]
pub struct Connection {
    /// Connection handle.
    pub handle: u32,
    /// Queue manager name.
    pub qmgr_name: String,
    /// Whether connected.
    pub connected: bool,
    /// Open objects.
    open_objects: Vec<OpenObject>,
    /// Next object handle.
    next_obj_handle: u32,
}

/// An open MQ object.
#[derive(Debug)]
struct OpenObject {
    handle: u32,
    queue_name: String,
    options: OpenOptions,
}

/// An MQI handle (wraps connection and object operations).
#[derive(Debug)]
pub struct MqiHandle {
    handle: u32,
}

impl MqiHandle {
    /// Get the raw handle value.
    pub fn value(&self) -> u32 {
        self.handle
    }
}

impl Connection {
    /// MQCONN — connect to queue manager.
    pub fn connect(qm: &mut QueueManager) -> Result<Self, MqError> {
        if !qm.running {
            return Err(MqError::Other("Queue manager not running".to_string()));
        }
        let handle = qm.next_handle();
        Ok(Self {
            handle,
            qmgr_name: qm.name.clone(),
            connected: true,
            open_objects: Vec::new(),
            next_obj_handle: 1,
        })
    }

    /// MQDISC — disconnect from queue manager.
    pub fn disconnect(&mut self) -> Result<(), MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        self.open_objects.clear();
        self.connected = false;
        Ok(())
    }

    /// MQOPEN — open a queue.
    pub fn open(
        &mut self,
        qm: &QueueManager,
        od: &mut Mqod,
        options: OpenOptions,
    ) -> Result<MqiHandle, MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }

        // Resolve the queue name.
        let resolved = qm.resolve_alias(&od.object_name)?;
        od.resolved_q_name = resolved.clone();
        od.resolved_qmgr_name = qm.name.clone();

        let handle = self.next_obj_handle;
        self.next_obj_handle += 1;

        self.open_objects.push(OpenObject {
            handle,
            queue_name: resolved,
            options,
        });

        Ok(MqiHandle { handle })
    }

    /// MQCLOSE — close a queue.
    pub fn close(&mut self, obj_handle: &MqiHandle) -> Result<(), MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        let pos = self
            .open_objects
            .iter()
            .position(|o| o.handle == obj_handle.handle)
            .ok_or(MqError::InvalidHandle)?;
        self.open_objects.remove(pos);
        Ok(())
    }

    /// MQPUT — put a message to an open queue.
    pub fn put(
        &self,
        qm: &mut QueueManager,
        obj_handle: &MqiHandle,
        md: &mut Mqmd,
        pmo: &mut MqPmo,
        data: &[u8],
    ) -> Result<(), MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        let obj = self
            .open_objects
            .iter()
            .find(|o| o.handle == obj_handle.handle)
            .ok_or(MqError::InvalidHandle)?;

        if !obj.options.output {
            return Err(MqError::Other("Queue not open for output".to_string()));
        }

        pmo.resolved_q_name = obj.queue_name.clone();
        pmo.resolved_qmgr_name = qm.name.clone();

        // Generate new message ID if requested.
        if pmo.new_msg_id {
            let handle_bytes = obj_handle.handle.to_be_bytes();
            let counter_bytes = qm.next_handle().to_be_bytes();
            md.msg_id[..4].copy_from_slice(&handle_bytes);
            md.msg_id[4..8].copy_from_slice(&counter_bytes);
        }

        let queue = qm.get_queue_mut(&obj.queue_name)?;
        queue.put(md.clone(), data.to_vec())
    }

    /// MQPUT1 — put a single message (open, put, close in one call).
    pub fn put1(
        &mut self,
        qm: &mut QueueManager,
        od: &mut Mqod,
        md: &mut Mqmd,
        pmo: &mut MqPmo,
        data: &[u8],
    ) -> Result<(), MqError> {
        let options = OpenOptions {
            output: true,
            ..Default::default()
        };
        let handle = self.open(qm, od, options)?;
        let result = self.put(qm, &handle, md, pmo, data);
        let _ = self.close(&handle);
        result
    }

    /// MQGET — get a message from an open queue.
    pub fn get(
        &self,
        qm: &mut QueueManager,
        obj_handle: &MqiHandle,
        md: &mut Mqmd,
        gmo: &Mqgmo,
        buffer: &mut Vec<u8>,
    ) -> Result<usize, MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        let obj = self
            .open_objects
            .iter()
            .find(|o| o.handle == obj_handle.handle)
            .ok_or(MqError::InvalidHandle)?;

        if !obj.options.input_shared && !obj.options.input_exclusive && !obj.options.browse {
            return Err(MqError::Other("Queue not open for input".to_string()));
        }

        let queue = qm.get_queue_mut(&obj.queue_name)?;

        if gmo.browse {
            // Browse mode.
            if let Some(msg) = queue.browse(gmo.browse_cursor) {
                *md = msg.mqmd.clone();
                buffer.clear();
                buffer.extend_from_slice(&msg.data);
                Ok(msg.data.len())
            } else {
                Err(MqError::NoMessage(obj.queue_name.clone()))
            }
        } else if gmo.match_correl_id {
            // Get by correlation ID.
            let msg = queue.get_by_correl_id(&md.correl_id)?;
            let len = msg.data.len();
            *md = msg.mqmd;
            buffer.clear();
            buffer.extend_from_slice(&msg.data);
            Ok(len)
        } else {
            // Normal destructive get.
            let msg = queue.get()?;
            let len = msg.data.len();
            *md = msg.mqmd;
            buffer.clear();
            buffer.extend_from_slice(&msg.data);
            Ok(len)
        }
    }

    /// Number of currently open objects.
    pub fn open_count(&self) -> usize {
        self.open_objects.len()
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> (QueueManager, Connection) {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("TEST.Q", crate::core::QueueType::Local).unwrap();
        let conn = Connection::connect(&mut qm).unwrap();
        (qm, conn)
    }

    #[test]
    fn test_connect_disconnect() {
        let (mut qm, mut conn) = setup();
        assert!(conn.connected);
        conn.disconnect().unwrap();
        assert!(!conn.connected);
        // Double disconnect should fail.
        assert!(conn.disconnect().is_err());
        // Reconnect.
        let conn2 = Connection::connect(&mut qm).unwrap();
        assert!(conn2.connected);
    }

    #[test]
    fn test_open_close() {
        let (qm, mut conn) = setup();
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let options = OpenOptions {
            output: true,
            ..Default::default()
        };
        let handle = conn.open(&qm, &mut od, options).unwrap();
        assert_eq!(conn.open_count(), 1);
        assert_eq!(od.resolved_q_name, "TEST.Q");

        conn.close(&handle).unwrap();
        assert_eq!(conn.open_count(), 0);
    }

    #[test]
    fn test_put_get() {
        let (mut qm, mut conn) = setup();

        // Open for output.
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let out_handle = conn
            .open(&qm, &mut od, OpenOptions { output: true, ..Default::default() })
            .unwrap();

        // Put a message.
        let mut md = Mqmd::default();
        let mut pmo = MqPmo::default();
        conn.put(&mut qm, &out_handle, &mut md, &mut pmo, b"Hello MQ")
            .unwrap();

        conn.close(&out_handle).unwrap();

        // Open for input.
        let mut od2 = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let in_handle = conn
            .open(&qm, &mut od2, OpenOptions { input_shared: true, ..Default::default() })
            .unwrap();

        // Get the message.
        let mut md2 = Mqmd::default();
        let gmo = Mqgmo::default();
        let mut buffer = Vec::new();
        let len = conn
            .get(&mut qm, &in_handle, &mut md2, &gmo, &mut buffer)
            .unwrap();

        assert_eq!(len, 8);
        assert_eq!(&buffer, b"Hello MQ");

        conn.close(&in_handle).unwrap();
    }

    #[test]
    fn test_put1() {
        let (mut qm, mut conn) = setup();
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let mut md = Mqmd::default();
        let mut pmo = MqPmo::default();
        conn.put1(&mut qm, &mut od, &mut md, &mut pmo, b"One-shot")
            .unwrap();

        let queue = qm.get_queue("TEST.Q").unwrap();
        assert_eq!(queue.depth(), 1);
    }

    #[test]
    fn test_browse() {
        let (mut qm, mut conn) = setup();

        // Put messages.
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let out_handle = conn
            .open(&qm, &mut od, OpenOptions { output: true, ..Default::default() })
            .unwrap();
        let mut md = Mqmd::default();
        let mut pmo = MqPmo::default();
        conn.put(&mut qm, &out_handle, &mut md, &mut pmo, b"msg1").unwrap();
        conn.put(&mut qm, &out_handle, &mut md, &mut pmo, b"msg2").unwrap();
        conn.close(&out_handle).unwrap();

        // Open for browse.
        let mut od2 = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let brw_handle = conn
            .open(&qm, &mut od2, OpenOptions { browse: true, ..Default::default() })
            .unwrap();

        let mut md2 = Mqmd::default();
        let gmo = Mqgmo { browse: true, browse_cursor: 0, ..Default::default() };
        let mut buffer = Vec::new();
        conn.get(&mut qm, &brw_handle, &mut md2, &gmo, &mut buffer).unwrap();
        assert_eq!(&buffer, b"msg1");

        // Queue still has 2 messages.
        let queue = qm.get_queue("TEST.Q").unwrap();
        assert_eq!(queue.depth(), 2);
    }

    #[test]
    fn test_not_open_for_output() {
        let (mut qm, mut conn) = setup();
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let handle = conn
            .open(&qm, &mut od, OpenOptions { input_shared: true, ..Default::default() })
            .unwrap();

        let mut md = Mqmd::default();
        let mut pmo = MqPmo::default();
        assert!(conn.put(&mut qm, &handle, &mut md, &mut pmo, b"fail").is_err());
    }

    #[test]
    fn test_mqi_handle_value() {
        let handle = MqiHandle { handle: 42 };
        assert_eq!(handle.value(), 42);
    }
}
