# CICS Crate — Architecture Decisions

## AD-3.0-01: Channel/Container Architecture

**Context:** CICS channels and containers are the modern replacement for COMMAREA, supporting data > 32KB. A channel is a named collection of containers; a container holds named byte data. Channels flow with LINK/XCTL/RETURN.

**Decision:** Implement `Channel` as a `HashMap<String, Container>` and `Container` as a named `Vec<u8>`. The `CicsRuntime` carries an optional current channel alongside the existing commarea. LINK/XCTL/RETURN accept an optional channel name. PUT/GET/MOVE/DELETE CONTAINER commands manipulate the current channel's containers.

**Consequences:**
- Backward compatible — programs using COMMAREA continue to work
- Channel data can exceed 32KB (no artificial limit)
- Must handle channel passing through the call stack (LINK saves/restores channels)
- Requires new preprocessor command types (PUT CONTAINER, GET CONTAINER, etc.)

## AD-3.0-02: Persistent File Storage via Dataset Integration

**Context:** Currently file operations use in-memory `HashMap<String, Vec<FileRecord>>`. Real CICS files are backed by VSAM datasets that persist across transactions and CICS restarts.

**Decision:** Create a `PersistentFileManager` that wraps `open-mainframe-dataset` crate's VSAM access methods. The `CicsFile` definition gains a `backing_path` field pointing to the dataset. READ/WRITE/REWRITE/DELETE translate to VSAM key-sequenced (KSDS), relative record (RRDS), or entry-sequenced (ESDS) operations. The existing in-memory `FileManager` remains available for testing (mock mode).

**Consequences:**
- File data persists across program invocations
- Depends on `open-mainframe-dataset` crate being mature enough for VSAM
- Performance characteristics change (disk I/O vs memory)
- Record locking becomes more complex (file-level vs in-memory HashMap)

## AD-3.0-03: EBCDIC via Encoding Crate

**Context:** The BMS renderer has a hand-rolled 33-character ASCII-to-EBCDIC table. The `open-mainframe-encoding` crate already provides full EBCDIC Code Page 037 translation.

**Decision:** Replace the inline translation table in `bms/render.rs` with calls to `open_mainframe_encoding::ascii_to_ebcdic()` and `ebcdic_to_ascii()`. Add `open-mainframe-encoding` as a dependency.

**Consequences:**
- Full 256-character EBCDIC support immediately
- Removes code duplication
- Adds a crate dependency (already in the workspace)

## AD-3.0-04: Preprocessor-to-Runtime Bridge

**Context:** The preprocessor generates COBOL `CALL "CICSxxxx"` statements, but there is no runtime dispatcher that receives these calls and executes the corresponding CICS commands. The gap means CICS COBOL programs cannot actually run end-to-end.

**Decision:** Create a `CicsDispatcher` module that provides a COBOL-callable entry point. When the COBOL runtime encounters `CALL "CICSLINK"`, it invokes the dispatcher with the command parameter block and DFHEIBLK. The dispatcher deserializes the parameters, executes the appropriate `CicsRuntime` method, and sets EIBRESP/EIBRESP2 in the EIB.

**Consequences:**
- Requires defining a binary format for command parameter blocks
- Must integrate with `open-mainframe-runtime`'s CALL dispatch mechanism
- Enables end-to-end CICS COBOL program execution
- Each command type needs a parameter block struct and dispatch handler

## AD-3.0-05: TD Queue DCT Configuration

**Context:** IBM CICS uses a Destination Control Table (DCT) to define TD queue properties — destination type (intra/extra), trigger level, associated transaction, output dataset.

**Decision:** Create a `DctEntry` struct with queue name, destination type, trigger level, trigger transaction, and backing file path. The `TdQueueManager` reads DCT entries at initialization. For extrapartition queues, the backing file is opened via standard file I/O.

**Consequences:**
- DCT can be loaded from a configuration file (JSON or TOML)
- Extrapartition queues actually write to files (currently in-memory only)
- Trigger-level firing can start associated transactions
- Configuration-driven behavior matches IBM CICS patterns
