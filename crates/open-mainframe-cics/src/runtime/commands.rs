//! CICS command execution.
//!
//! Implements LINK, XCTL, RETURN, and other program control commands.

use super::{Commarea, Eib, FileManager, TransactionContext};
use crate::queues::td::{TdQueueManager, TdError};
use crate::sync::EnqueueManager;
use crate::{CicsError, CicsResponse, CicsResult};
use std::collections::HashMap;

/// Result of executing a CICS program.
#[derive(Debug)]
pub enum ProgramResult {
    /// Normal return
    Return,
    /// Return with TRANSID for next transaction
    ReturnTransid(String),
    /// Return with COMMAREA
    ReturnCommarea(Commarea),
    /// Return with channel
    ReturnChannel(String),
    /// XCTL to another program
    Xctl { program: String, commarea: Option<Commarea>, channel: Option<String> },
    /// ABEND
    Abend(String),
}

/// Program entry point type.
pub type ProgramEntry = Box<dyn Fn(&mut CicsRuntime) -> CicsResult<ProgramResult>>;

/// Registry of available programs.
pub struct ProgramRegistry {
    programs: HashMap<String, ProgramEntry>,
}

impl ProgramRegistry {
    /// Create a new registry.
    pub fn new() -> Self {
        Self {
            programs: HashMap::new(),
        }
    }

    /// Register a program.
    pub fn register<F>(&mut self, name: &str, entry: F)
    where
        F: Fn(&mut CicsRuntime) -> CicsResult<ProgramResult> + 'static,
    {
        self.programs.insert(name.to_uppercase(), Box::new(entry));
    }

    /// Check if program exists.
    pub fn exists(&self, name: &str) -> bool {
        self.programs.contains_key(&name.to_uppercase())
    }

    /// Get program entry point.
    pub fn get(&self, name: &str) -> Option<&ProgramEntry> {
        self.programs.get(&name.to_uppercase())
    }
}

impl Default for ProgramRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// CICS runtime environment.
pub struct CicsRuntime {
    /// Execute Interface Block
    pub eib: Eib,
    /// Transaction context
    pub context: TransactionContext,
    /// File manager
    pub files: FileManager,
    /// Transient Data queue manager
    pub td_queues: TdQueueManager,
    /// Resource enqueue manager (ENQ/DEQ)
    pub enqueue: EnqueueManager,
    /// Current COMMAREA
    commarea: Option<Commarea>,
    /// Program call stack
    call_stack: Vec<String>,
    /// Mock mode
    mock_mode: bool,
    /// CONVERSE send buffer (outbound data)
    converse_send_buffer: Option<Vec<u8>>,
    /// CONVERSE receive buffer (simulated inbound data for testing)
    converse_receive_buffer: Option<Vec<u8>>,
    /// CONVERSE erase flag
    converse_erase: bool,
    /// Terminal output buffer (SEND data)
    terminal_output: Option<Vec<u8>>,
    /// Terminal input buffer (simulated RECEIVE data for testing)
    terminal_input: Option<Vec<u8>>,
    /// Terminal erase flag
    terminal_erase: bool,
    /// System ID (SYSID) — 4-char region identifier
    sysid: String,
    /// Application ID (APPLID) — VTAM application name
    applid: String,
    /// Operator ID (OPID) — 3-char operator identifier
    opid: String,
    /// Network name (NETNAME) — terminal network name
    netname: String,
    /// Start code (STARTCODE) — how the transaction was started
    startcode: String,
}

impl CicsRuntime {
    /// Create a new runtime.
    pub fn new(transaction_id: &str) -> Self {
        let mut eib = Eib::new();
        eib.set_transaction_id(transaction_id);

        Self {
            eib,
            context: TransactionContext::new(transaction_id),
            files: FileManager::new(),
            td_queues: TdQueueManager::new(),
            enqueue: EnqueueManager::new(),
            commarea: None,
            call_stack: Vec::new(),
            mock_mode: true,
            converse_send_buffer: None,
            converse_receive_buffer: None,
            converse_erase: false,
            terminal_output: None,
            terminal_input: None,
            terminal_erase: false,
            sysid: "CICS".to_string(),
            applid: "OMCICS01".to_string(),
            opid: "OPR".to_string(),
            netname: String::new(),
            startcode: "TD".to_string(),
        }
    }

    /// Get current COMMAREA.
    pub fn commarea(&self) -> Option<&Commarea> {
        self.commarea.as_ref()
    }

    /// Get mutable COMMAREA.
    pub fn commarea_mut(&mut self) -> Option<&mut Commarea> {
        self.commarea.as_mut()
    }

    /// Set COMMAREA.
    pub fn set_commarea(&mut self, commarea: Commarea) {
        self.eib.set_commarea_length(commarea.len() as u16);
        self.commarea = Some(commarea);
    }

    /// Execute LINK command.
    pub fn link(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        registry: &ProgramRegistry,
    ) -> CicsResult<()> {
        self.link_with_channel(program, commarea, None, registry)
    }

    /// Execute LINK command with an optional channel.
    ///
    /// When a channel name is provided, it becomes the current channel
    /// for the called program. Modifications to containers are visible
    /// to the caller when the called program returns.
    pub fn link_with_channel(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        channel: Option<&str>,
        registry: &ProgramRegistry,
    ) -> CicsResult<()> {
        self.eib.reset_for_command();

        // Check if program exists
        if !registry.exists(program) {
            self.eib.set_response(CicsResponse::Pgmiderr);
            if let Some(handler) = self.context.get_handler("PGMIDERR") {
                return Err(CicsError::ProgramNotFound(format!("{} -> {}", program, handler)));
            }
            return Err(CicsError::ProgramNotFound(program.to_string()));
        }

        // Save current state
        if let Some(ca) = commarea {
            self.set_commarea(ca);
        }

        // Set current channel if provided
        let prev_channel = self.context.channels.current_channel_name().map(|s| s.to_string());
        if let Some(ch_name) = channel {
            // Ensure the channel exists
            self.context.channels.get_or_create(ch_name);
            self.context.channels.set_current_channel(Some(ch_name.to_string()));
        }

        self.call_stack.push(program.to_uppercase());

        // In mock mode, just simulate success
        if self.mock_mode {
            self.eib.set_response(CicsResponse::Normal);
            self.call_stack.pop();
            // Restore previous channel
            self.context.channels.set_current_channel(prev_channel);
            return Ok(());
        }

        // Execute program
        if let Some(entry) = registry.get(program) {
            let result = entry(self)?;
            self.call_stack.pop();

            match result {
                ProgramResult::Return
                | ProgramResult::ReturnCommarea(_)
                | ProgramResult::ReturnTransid(_)
                | ProgramResult::ReturnChannel(_) => {
                    self.eib.set_response(CicsResponse::Normal);
                }
                ProgramResult::Abend(code) => {
                    return Err(CicsError::InvalidRequest(format!("ABEND {}", code)));
                }
                ProgramResult::Xctl { .. } => {
                    // XCTL from linked program returns normally to caller
                    self.eib.set_response(CicsResponse::Normal);
                }
            }
        }

        // Restore previous channel (modifications to containers persist)
        self.context.channels.set_current_channel(prev_channel);

        Ok(())
    }

    /// Execute XCTL command.
    pub fn xctl(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        registry: &ProgramRegistry,
    ) -> CicsResult<ProgramResult> {
        self.xctl_with_channel(program, commarea, None, registry)
    }

    /// Execute XCTL command with an optional channel.
    pub fn xctl_with_channel(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        channel: Option<&str>,
        registry: &ProgramRegistry,
    ) -> CicsResult<ProgramResult> {
        self.eib.reset_for_command();

        // Check if program exists
        if !registry.exists(program) {
            self.eib.set_response(CicsResponse::Pgmiderr);
            return Err(CicsError::ProgramNotFound(program.to_string()));
        }

        // Set COMMAREA
        if let Some(ca) = commarea.clone() {
            self.set_commarea(ca);
        }

        // Set channel if provided
        if let Some(ch_name) = channel {
            self.context.channels.get_or_create(ch_name);
            self.context.channels.set_current_channel(Some(ch_name.to_string()));
        }

        // Return XCTL result for caller to handle
        Ok(ProgramResult::Xctl {
            program: program.to_string(),
            commarea,
            channel: channel.map(|s| s.to_string()),
        })
    }

    /// Execute RETURN command.
    pub fn return_(&mut self, transid: Option<&str>, commarea: Option<Commarea>) -> CicsResult<ProgramResult> {
        self.return_with_channel(transid, commarea, None)
    }

    /// Execute RETURN command with optional channel.
    pub fn return_with_channel(
        &mut self,
        transid: Option<&str>,
        commarea: Option<Commarea>,
        channel: Option<&str>,
    ) -> CicsResult<ProgramResult> {
        self.eib.reset_for_command();
        self.eib.set_response(CicsResponse::Normal);

        if let Some(trans) = transid {
            Ok(ProgramResult::ReturnTransid(trans.to_string()))
        } else if let Some(ch_name) = channel {
            Ok(ProgramResult::ReturnChannel(ch_name.to_string()))
        } else if let Some(ca) = commarea {
            Ok(ProgramResult::ReturnCommarea(ca))
        } else {
            Ok(ProgramResult::Return)
        }
    }

    /// PUT CONTAINER — stores data in a container within a channel.
    pub fn put_container(
        &mut self,
        container_name: &str,
        channel_name: Option<&str>,
        data: &[u8],
    ) -> CicsResult<()> {
        self.eib.reset_for_command();
        let ch_name = self.resolve_channel_name(channel_name)?;
        self.context.channels.put_container(&ch_name, container_name, data)?;
        self.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// GET CONTAINER — retrieves data from a container.
    pub fn get_container(
        &self,
        container_name: &str,
        channel_name: Option<&str>,
    ) -> CicsResult<Vec<u8>> {
        let ch_name = self.resolve_channel_name(channel_name)?;
        let data = self.context.channels.get_container(&ch_name, container_name)?;
        Ok(data.to_vec())
    }

    /// DELETE CONTAINER — removes a container from a channel.
    pub fn delete_container(
        &mut self,
        container_name: &str,
        channel_name: Option<&str>,
    ) -> CicsResult<()> {
        self.eib.reset_for_command();
        let ch_name = self.resolve_channel_name(channel_name)?;
        self.context.channels.delete_container(&ch_name, container_name)?;
        self.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Resolve channel name: use explicit name or fall back to current.
    fn resolve_channel_name(&self, explicit: Option<&str>) -> CicsResult<String> {
        if let Some(name) = explicit {
            Ok(name.to_uppercase())
        } else if let Some(name) = self.context.channels.current_channel_name() {
            Ok(name.to_string())
        } else {
            Err(CicsError::InvalidRequest(
                "No channel specified and no current channel set".to_string(),
            ))
        }
    }

    /// Execute WRITEQ TD command — write a record to a transient data queue.
    pub fn writeq_td(&mut self, queue_name: &str, data: &[u8]) -> CicsResult<()> {
        self.eib.reset_for_command();
        match self.td_queues.writeq(queue_name, data.to_vec()) {
            Ok(()) => {
                self.eib.set_response(CicsResponse::Normal);
                Ok(())
            }
            Err(TdError::QueueNotFound) => {
                self.eib.set_response(CicsResponse::Qiderr);
                Err(CicsError::InvalidRequest(format!(
                    "TD queue '{}' not found (QIDERR)",
                    queue_name
                )))
            }
            Err(e) => {
                self.eib.set_response(CicsResponse::Ioerr);
                Err(CicsError::InvalidRequest(format!("TD write error: {}", e)))
            }
        }
    }

    /// Execute READQ TD command — read a record from a transient data queue.
    ///
    /// TD reads are destructive: the record is removed from the queue.
    pub fn readq_td(&mut self, queue_name: &str) -> CicsResult<Vec<u8>> {
        self.eib.reset_for_command();
        match self.td_queues.readq(queue_name) {
            Ok(data) => {
                self.eib.set_response(CicsResponse::Normal);
                Ok(data)
            }
            Err(TdError::QueueNotFound) => {
                self.eib.set_response(CicsResponse::Qiderr);
                Err(CicsError::InvalidRequest(format!(
                    "TD queue '{}' not found (QIDERR)",
                    queue_name
                )))
            }
            Err(TdError::QueueEmpty) => {
                self.eib.set_response(CicsResponse::Qzero);
                Err(CicsError::InvalidRequest(format!(
                    "TD queue '{}' is empty (QZERO)",
                    queue_name
                )))
            }
            Err(e) => {
                self.eib.set_response(CicsResponse::Ioerr);
                Err(CicsError::InvalidRequest(format!("TD read error: {}", e)))
            }
        }
    }

    /// Execute DELETEQ TD command — delete a transient data queue.
    pub fn deleteq_td(&mut self, queue_name: &str) -> CicsResult<()> {
        self.eib.reset_for_command();
        match self.td_queues.delete_queue(queue_name) {
            Ok(()) => {
                self.eib.set_response(CicsResponse::Normal);
                Ok(())
            }
            Err(TdError::QueueNotFound) => {
                self.eib.set_response(CicsResponse::Qiderr);
                Err(CicsError::InvalidRequest(format!(
                    "TD queue '{}' not found (QIDERR)",
                    queue_name
                )))
            }
            Err(e) => {
                self.eib.set_response(CicsResponse::Ioerr);
                Err(CicsError::InvalidRequest(format!("TD delete error: {}", e)))
            }
        }
    }

    /// Get pending trigger transactions from TD queues.
    pub fn get_td_triggers(&mut self) -> Vec<String> {
        self.td_queues.get_pending_triggers()
    }

    /// Execute ASSIGN command — retrieve system and task values.
    ///
    /// Returns a map of requested field names to their values.
    /// The caller specifies which fields they want by passing option names.
    pub fn assign(&self, requested_fields: &[&str]) -> HashMap<String, String> {
        let mut result = HashMap::new();

        for &field in requested_fields {
            let value = match field.to_uppercase().as_str() {
                "SYSID" => self.sysid.clone(),
                "APPLID" => self.applid.clone(),
                "USERID" => self.context.user_id.clone().unwrap_or_default(),
                "OPID" => self.opid.clone(),
                "FACILITY" => self.context.terminal_id.clone().unwrap_or_default(),
                "NETNAME" => self.netname.clone(),
                "STARTCODE" => self.startcode.clone(),
                "TRNID" | "TRANSID" => self.eib.transaction_id(),
                "TERMID" | "TERMCODE" => self.eib.terminal_id(),
                "SCRNHT" => {
                    // Return screen height — requires terminal info
                    "24".to_string() // Default Model 2
                }
                "SCRNWD" => {
                    "80".to_string() // Default Model 2
                }
                "CWALENG" => {
                    let len = self.commarea.as_ref().map_or(0, |c| c.len());
                    len.to_string()
                }
                "EIBCALEN" => self.eib.eibcalen.to_string(),
                _ => String::new(),
            };
            result.insert(field.to_uppercase(), value);
        }

        result
    }

    /// Set SYSID.
    pub fn set_sysid(&mut self, sysid: &str) {
        self.sysid = sysid.to_string();
    }

    /// Set APPLID.
    pub fn set_applid(&mut self, applid: &str) {
        self.applid = applid.to_string();
    }

    /// Set OPID.
    pub fn set_opid(&mut self, opid: &str) {
        self.opid = opid.to_string();
    }

    /// Set NETNAME.
    pub fn set_netname(&mut self, netname: &str) {
        self.netname = netname.to_string();
    }

    /// Set STARTCODE.
    pub fn set_startcode(&mut self, startcode: &str) {
        self.startcode = startcode.to_string();
    }

    /// Execute CONVERSE command — combined SEND + RECEIVE.
    ///
    /// CONVERSE sends data to the terminal then immediately waits for
    /// a response. It is equivalent to SEND followed by RECEIVE but
    /// in a single command.
    pub fn converse(
        &mut self,
        send_data: &[u8],
        max_receive_length: usize,
        erase: bool,
    ) -> CicsResult<Vec<u8>> {
        self.eib.reset_for_command();

        // Store the send data for the terminal to display
        self.converse_send_buffer = Some(send_data.to_vec());
        self.converse_erase = erase;

        // In mock/test mode, return from the receive buffer if set
        let received = if let Some(ref buf) = self.converse_receive_buffer {
            let mut data = buf.clone();
            data.truncate(max_receive_length);
            data
        } else {
            Vec::new()
        };

        self.converse_send_buffer = None;
        self.converse_receive_buffer = None;

        self.eib.set_response(CicsResponse::Normal);
        Ok(received)
    }

    /// Set simulated CONVERSE receive data (for testing).
    ///
    /// Call this before `converse()` to provide the data that would
    /// come back from the terminal.
    pub fn set_converse_response(&mut self, data: &[u8]) {
        self.converse_receive_buffer = Some(data.to_vec());
    }

    /// Get the last CONVERSE send buffer (for testing/inspection).
    pub fn last_converse_send(&self) -> Option<&[u8]> {
        self.converse_send_buffer.as_deref()
    }

    /// Execute SEND (basic terminal I/O — sends data to terminal).
    pub fn send_data(&mut self, data: &[u8], erase: bool) -> CicsResult<()> {
        self.eib.reset_for_command();
        self.terminal_output = Some(data.to_vec());
        self.terminal_erase = erase;
        self.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Execute RECEIVE (basic terminal I/O — receives data from terminal).
    pub fn receive_data(&mut self, max_length: usize) -> CicsResult<Vec<u8>> {
        self.eib.reset_for_command();

        let received = if let Some(ref buf) = self.terminal_input {
            let mut data = buf.clone();
            data.truncate(max_length);
            data
        } else {
            return Err(CicsError::InvalidRequest("No terminal input available".to_string()));
        };

        self.terminal_input = None;
        self.eib.set_response(CicsResponse::Normal);
        Ok(received)
    }

    /// Set simulated terminal input (for testing).
    pub fn set_terminal_input(&mut self, data: &[u8]) {
        self.terminal_input = Some(data.to_vec());
    }

    /// Get the last terminal output (for testing/inspection).
    pub fn last_terminal_output(&self) -> Option<&[u8]> {
        self.terminal_output.as_deref()
    }

    /// Execute ABEND command.
    pub fn abend(&mut self, code: &str) -> CicsResult<ProgramResult> {
        // Check for abend handler
        if let Some(handler) = &self.context.abend_handler {
            return Err(CicsError::InvalidRequest(format!(
                "ABEND {} handled by {}",
                code, handler
            )));
        }

        Ok(ProgramResult::Abend(code.to_string()))
    }

    /// Execute GETMAIN command.
    pub fn getmain(&mut self, length: usize) -> CicsResult<Vec<u8>> {
        self.eib.reset_for_command();

        if length == 0 {
            self.eib.set_response(CicsResponse::Lengerr);
            return Err(CicsError::InvalidRequest("GETMAIN length must be > 0".to_string()));
        }

        self.eib.set_response(CicsResponse::Normal);
        Ok(vec![0; length])
    }

    /// Execute FREEMAIN command.
    pub fn freemain(&mut self) -> CicsResult<()> {
        self.eib.reset_for_command();
        self.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Execute HANDLE CONDITION command.
    pub fn handle_condition(&mut self, condition: &str, label: &str) {
        self.context.handle_condition(condition, label);
    }

    /// Execute IGNORE CONDITION command.
    pub fn ignore_condition(&mut self, condition: &str) {
        self.context.ignore_condition(condition);
    }

    /// Execute HANDLE ABEND command.
    pub fn handle_abend(&mut self, label: &str) {
        self.context.handle_abend(label);
    }

    /// Get call stack depth.
    pub fn call_depth(&self) -> usize {
        self.call_stack.len()
    }
}

impl Default for CicsRuntime {
    fn default() -> Self {
        Self::new("DFLT")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_creation() {
        let runtime = CicsRuntime::new("MENU");
        assert_eq!(runtime.eib.transaction_id(), "MENU");
        assert_eq!(runtime.call_depth(), 0);
    }

    #[test]
    fn test_set_commarea() {
        let mut runtime = CicsRuntime::new("TEST");
        let ca = Commarea::new(100);
        runtime.set_commarea(ca);

        assert!(runtime.commarea().is_some());
        assert_eq!(runtime.eib.eibcalen, 100);
    }

    #[test]
    fn test_link_not_found() {
        let mut runtime = CicsRuntime::new("TEST");
        let registry = ProgramRegistry::new();

        // Mock mode - simulates program not found
        runtime.mock_mode = false;
        let result = runtime.link("NONEXIST", None, &registry);

        assert!(result.is_err());
        assert_eq!(runtime.eib.eibresp, CicsResponse::Pgmiderr as u32);
    }

    #[test]
    fn test_link_success_mock() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();
        registry.register("SUBPROG", |_rt| Ok(ProgramResult::Return));

        let result = runtime.link("SUBPROG", None, &registry);

        assert!(result.is_ok());
        assert_eq!(runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_return() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.return_(None, None).unwrap();
        assert!(matches!(result, ProgramResult::Return));
    }

    #[test]
    fn test_return_with_transid() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.return_(Some("MENU"), None).unwrap();
        assert!(matches!(result, ProgramResult::ReturnTransid(ref t) if t == "MENU"));
    }

    #[test]
    fn test_xctl() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();
        registry.register("NEXT", |_rt| Ok(ProgramResult::Return));

        let result = runtime.xctl("NEXT", None, &registry).unwrap();
        assert!(matches!(result, ProgramResult::Xctl { ref program, .. } if program == "NEXT"));
    }

    #[test]
    fn test_getmain() {
        let mut runtime = CicsRuntime::new("TEST");

        let data = runtime.getmain(1000).unwrap();
        assert_eq!(data.len(), 1000);
        assert!(data.iter().all(|&b| b == 0));
    }

    #[test]
    fn test_getmain_zero_length() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.getmain(0);
        assert!(result.is_err());
        assert_eq!(runtime.eib.eibresp, CicsResponse::Lengerr as u32);
    }

    #[test]
    fn test_handle_condition() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.handle_condition("NOTFND", "NOT-FOUND-PARA");

        assert_eq!(runtime.context.get_handler("NOTFND"), Some("NOT-FOUND-PARA"));
    }

    #[test]
    fn test_ignore_condition() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.handle_condition("NOTFND", "LABEL1");
        runtime.ignore_condition("NOTFND");

        assert_eq!(runtime.context.get_handler("NOTFND"), None);
    }

    #[test]
    fn test_abend() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.abend("ASRA").unwrap();
        assert!(matches!(result, ProgramResult::Abend(ref code) if code == "ASRA"));
    }

    // === Story 200.2: Channel passing on LINK/XCTL/RETURN ===

    #[test]
    fn test_link_with_channel() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();
        registry.register("SUBPROG", |_rt| Ok(ProgramResult::Return));

        // Set up channel with container data
        runtime.context.channels.put_container("MY-CHANNEL", "DATA1", b"payload").unwrap();

        // LINK with CHANNEL
        let result = runtime.link_with_channel("SUBPROG", None, Some("MY-CHANNEL"), &registry);
        assert!(result.is_ok());

        // Modified containers visible after return
        let data = runtime.context.channels.get_container("MY-CHANNEL", "DATA1").unwrap();
        assert_eq!(data, b"payload");
    }

    #[test]
    fn test_link_with_channel_callee_sees_channel() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();

        // The subprogram reads from the channel
        registry.register("SUB", |rt| {
            let data = rt.get_container("DATA1", None)?;
            assert_eq!(&data, b"from caller");
            // Modify a container
            rt.put_container("RESULT", None, b"from callee")?;
            Ok(ProgramResult::Return)
        });

        // Caller sets up channel
        runtime.context.channels.put_container("MY-CHANNEL", "DATA1", b"from caller").unwrap();

        // LINK with channel — not in mock mode
        runtime.mock_mode = false;
        let result = runtime.link_with_channel("SUB", None, Some("MY-CHANNEL"), &registry);
        assert!(result.is_ok());

        // Caller can see the container added by callee
        let result_data = runtime.context.channels.get_container("MY-CHANNEL", "RESULT").unwrap();
        assert_eq!(result_data, b"from callee");
    }

    #[test]
    fn test_xctl_with_channel() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();
        registry.register("NEXT", |_rt| Ok(ProgramResult::Return));

        runtime.context.channels.put_container("CH1", "D1", b"data").unwrap();

        let result = runtime.xctl_with_channel("NEXT", None, Some("CH1"), &registry).unwrap();
        assert!(matches!(result, ProgramResult::Xctl { ref channel, .. } if channel == &Some("CH1".to_string())));

        // Channel should now be current
        assert_eq!(runtime.context.channels.current_channel_name(), Some("CH1"));
    }

    #[test]
    fn test_return_with_channel() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.return_with_channel(None, None, Some("MY-CHANNEL")).unwrap();
        assert!(matches!(result, ProgramResult::ReturnChannel(ref ch) if ch == "MY-CHANNEL"));
    }

    #[test]
    fn test_put_get_delete_container() {
        let mut runtime = CicsRuntime::new("TEST");

        // Set current channel
        runtime.context.channels.get_or_create("TEST-CH");
        runtime.context.channels.set_current_channel(Some("TEST-CH".to_string()));

        // PUT CONTAINER
        runtime.put_container("DATA1", None, b"hello").unwrap();

        // GET CONTAINER
        let data = runtime.get_container("DATA1", None).unwrap();
        assert_eq!(&data, b"hello");

        // DELETE CONTAINER
        runtime.delete_container("DATA1", None).unwrap();
        assert!(runtime.get_container("DATA1", None).is_err());
    }

    #[test]
    fn test_container_with_explicit_channel() {
        let mut runtime = CicsRuntime::new("TEST");

        // PUT with explicit channel name (auto-creates channel)
        runtime.put_container("ITEM", Some("MY-CH"), b"value").unwrap();

        // GET with explicit channel name
        let data = runtime.get_container("ITEM", Some("MY-CH")).unwrap();
        assert_eq!(&data, b"value");
    }

    #[test]
    fn test_container_no_channel_error() {
        let runtime = CicsRuntime::new("TEST");
        // No current channel set and no explicit channel
        let result = runtime.get_container("DATA1", None);
        assert!(result.is_err());
    }

    // === Story 201.1: TD queue runtime commands ===

    #[test]
    fn test_writeq_td_readq_td() {
        use crate::queues::td::{TdQueue, TdDestType};

        let mut runtime = CicsRuntime::new("TEST");
        runtime.td_queues.define_queue(TdQueue::new("CSSL", TdDestType::Intrapartition));

        // WRITEQ TD
        runtime.writeq_td("CSSL", b"Log record 1").unwrap();
        runtime.writeq_td("CSSL", b"Log record 2").unwrap();
        assert_eq!(runtime.eib.eibresp, CicsResponse::Normal as u32);

        // READQ TD (destructive, FIFO)
        let rec1 = runtime.readq_td("CSSL").unwrap();
        assert_eq!(rec1, b"Log record 1");
        let rec2 = runtime.readq_td("CSSL").unwrap();
        assert_eq!(rec2, b"Log record 2");
    }

    #[test]
    fn test_readq_td_empty_queue() {
        use crate::queues::td::{TdQueue, TdDestType};

        let mut runtime = CicsRuntime::new("TEST");
        runtime.td_queues.define_queue(TdQueue::new("EMPTY", TdDestType::Intrapartition));

        let result = runtime.readq_td("EMPTY");
        assert!(result.is_err());
        assert_eq!(runtime.eib.eibresp, CicsResponse::Qzero as u32);
    }

    #[test]
    fn test_writeq_td_queue_not_found() {
        let mut runtime = CicsRuntime::new("TEST");
        let result = runtime.writeq_td("NOQUEUE", b"data");
        assert!(result.is_err());
        assert_eq!(runtime.eib.eibresp, CicsResponse::Qiderr as u32);
    }

    #[test]
    fn test_deleteq_td() {
        use crate::queues::td::{TdQueue, TdDestType};

        let mut runtime = CicsRuntime::new("TEST");
        runtime.td_queues.define_queue(TdQueue::new("TDQ1", TdDestType::Intrapartition));
        runtime.writeq_td("TDQ1", b"data").unwrap();

        runtime.deleteq_td("TDQ1").unwrap();
        assert_eq!(runtime.eib.eibresp, CicsResponse::Normal as u32);

        // Queue is gone
        let result = runtime.writeq_td("TDQ1", b"more");
        assert!(result.is_err());
    }

    #[test]
    fn test_td_trigger_via_runtime() {
        use crate::queues::td::{TdQueue, TdDestType};

        let mut runtime = CicsRuntime::new("TEST");
        let queue = TdQueue::new("TRGQ", TdDestType::Intrapartition)
            .with_trigger(2, "PROC");
        runtime.td_queues.define_queue(queue);

        runtime.writeq_td("TRGQ", b"rec1").unwrap();
        assert!(runtime.get_td_triggers().is_empty());

        runtime.writeq_td("TRGQ", b"rec2").unwrap();
        let triggers = runtime.get_td_triggers();
        assert_eq!(triggers.len(), 1);
        assert_eq!(triggers[0], "PROC");
    }

    // === Story 203.1: CONVERSE command ===

    #[test]
    fn test_converse_basic() {
        let mut runtime = CicsRuntime::new("TEST");
        // Set up simulated response
        runtime.set_converse_response(b"USER INPUT");

        let received = runtime.converse(b"ENTER NAME:", 80, false).unwrap();
        assert_eq!(received, b"USER INPUT");
        assert_eq!(runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_converse_truncates_to_maxlength() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.set_converse_response(b"THIS IS A LONG RESPONSE");

        let received = runtime.converse(b"PROMPT", 10, false).unwrap();
        assert_eq!(received.len(), 10);
        assert_eq!(received, b"THIS IS A ");
    }

    #[test]
    fn test_converse_no_response_returns_empty() {
        let mut runtime = CicsRuntime::new("TEST");
        // No response set
        let received = runtime.converse(b"PROMPT", 80, false).unwrap();
        assert!(received.is_empty());
    }

    #[test]
    fn test_converse_with_erase() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.set_converse_response(b"OK");

        let received = runtime.converse(b"HELLO", 80, true).unwrap();
        assert_eq!(received, b"OK");
    }

    // === Terminal SEND/RECEIVE ===

    #[test]
    fn test_send_data() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.send_data(b"HELLO WORLD", false).unwrap();

        assert_eq!(runtime.last_terminal_output(), Some(b"HELLO WORLD".as_slice()));
        assert_eq!(runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_receive_data() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.set_terminal_input(b"USER RESPONSE");

        let data = runtime.receive_data(80).unwrap();
        assert_eq!(data, b"USER RESPONSE");
    }

    #[test]
    fn test_receive_data_no_input() {
        let mut runtime = CicsRuntime::new("TEST");
        let result = runtime.receive_data(80);
        assert!(result.is_err());
    }

    #[test]
    fn test_receive_data_truncates() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.set_terminal_input(b"LONG INPUT DATA");

        let data = runtime.receive_data(4).unwrap();
        assert_eq!(data, b"LONG");
    }

    // === Story 205.1: ASSIGN system values ===

    #[test]
    fn test_assign_system_values() {
        let mut runtime = CicsRuntime::new("MENU");
        runtime.context.user_id = Some("USER01".to_string());
        runtime.context.terminal_id = Some("T001".to_string());

        let values = runtime.assign(&["SYSID", "APPLID", "USERID", "FACILITY", "TRNID"]);

        assert_eq!(values.get("SYSID"), Some(&"CICS".to_string()));
        assert_eq!(values.get("APPLID"), Some(&"OMCICS01".to_string()));
        assert_eq!(values.get("USERID"), Some(&"USER01".to_string()));
        assert_eq!(values.get("FACILITY"), Some(&"T001".to_string()));
        assert_eq!(values.get("TRNID"), Some(&"MENU".to_string()));
    }

    #[test]
    fn test_assign_custom_sysid() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.set_sysid("SYS1");
        runtime.set_applid("PROD01");
        runtime.set_opid("AB1");
        runtime.set_netname("NETLU01");
        runtime.set_startcode("S");

        let values = runtime.assign(&["SYSID", "APPLID", "OPID", "NETNAME", "STARTCODE"]);

        assert_eq!(values.get("SYSID"), Some(&"SYS1".to_string()));
        assert_eq!(values.get("APPLID"), Some(&"PROD01".to_string()));
        assert_eq!(values.get("OPID"), Some(&"AB1".to_string()));
        assert_eq!(values.get("NETNAME"), Some(&"NETLU01".to_string()));
        assert_eq!(values.get("STARTCODE"), Some(&"S".to_string()));
    }

    #[test]
    fn test_assign_commarea_length() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.set_commarea(super::super::Commarea::new(256));

        let values = runtime.assign(&["EIBCALEN", "CWALENG"]);

        assert_eq!(values.get("EIBCALEN"), Some(&"256".to_string()));
        assert_eq!(values.get("CWALENG"), Some(&"256".to_string()));
    }

    #[test]
    fn test_assign_unknown_field_returns_empty() {
        let runtime = CicsRuntime::new("TEST");
        let values = runtime.assign(&["NONEXISTENT"]);
        assert_eq!(values.get("NONEXISTENT"), Some(&String::new()));
    }

    #[test]
    fn test_program_registry() {
        let mut registry = ProgramRegistry::new();
        registry.register("TEST1", |_| Ok(ProgramResult::Return));
        registry.register("test2", |_| Ok(ProgramResult::Return)); // lowercase

        assert!(registry.exists("TEST1"));
        assert!(registry.exists("test1")); // case insensitive
        assert!(registry.exists("TEST2"));
        assert!(!registry.exists("NONEXIST"));
    }
}
