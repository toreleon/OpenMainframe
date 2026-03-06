//! Subsystem reference page generators (CICS, JES2, RACF, TSO, ISPF, MQ, MVS, USS, WLM, SMF, networking).

use std::fs;

use crate::{WikiConfig, WikiResult};

/// Generate all subsystem reference pages.
pub fn generate_subsystem_pages(config: &WikiConfig) -> WikiResult<()> {
    let out = &config.output_dir;
    let sub = out.join("subsystems");
    fs::write(sub.join("index.md"), generate_subsystems_index())?;
    fs::write(sub.join("cics/index.md"), generate_cics_index())?;
    fs::write(sub.join("cics/commands.md"), generate_cics_commands())?;
    fs::write(sub.join("cics/response-codes.md"), generate_cics_response_codes())?;
    fs::write(sub.join("jes2.md"), generate_jes2())?;
    fs::write(sub.join("racf.md"), generate_racf())?;
    fs::write(sub.join("tso.md"), generate_tso())?;
    fs::write(sub.join("ispf.md"), generate_ispf())?;
    fs::write(sub.join("mq.md"), generate_mq())?;
    fs::write(sub.join("mvs.md"), generate_mvs())?;
    fs::write(sub.join("uss.md"), generate_uss())?;
    fs::write(sub.join("wlm.md"), generate_wlm())?;
    fs::write(sub.join("smf.md"), generate_smf())?;
    fs::write(sub.join("networking.md"), generate_networking())?;
    Ok(())
}

fn generate_subsystems_index() -> String {
    r#"# z/OS Subsystems

z/OS is a highly modular operating system composed of cooperating subsystems. Each subsystem manages a specific domain of functionality and communicates with others through well-defined interfaces such as the Subsystem Interface (SSI) and the System Authorization Facility (SAF).

## Subsystem Overview

| Subsystem | Full Name | Purpose | Key Feature |
|-----------|-----------|---------|-------------|
| [CICS](cics/index.md) | Customer Information Control System | Online transaction processing | Pseudo-conversational model, BMS maps, EXEC CICS API |
| [JES2](jes2.md) | Job Entry Subsystem 2 | Batch job scheduling and spooling | Job lifecycle management, spool I/O, output routing |
| [RACF](racf.md) | Resource Access Control Facility | Security and access control | User/group profiles, resource protection, SAF integration |
| [TSO](tso.md) | Time Sharing Option | Interactive user sessions | Command processing, REXX/CLIST scripting, ALLOCATE |
| [ISPF](ispf.md) | Interactive System Productivity Facility | Panel-driven user interface | Editor, browse, utilities, dialog manager services |
| [MQ](mq.md) | IBM MQ (formerly MQSeries) | Message queuing and middleware | Guaranteed delivery, distributed queuing, MQI API |
| [MVS](mvs.md) | Multiple Virtual Storage | Core operating system services | SVCs, DYNALLOC, WTO, ENQ/DEQ, ESTAE, program management |
| [USS](uss.md) | Unix System Services | POSIX environment on z/OS | Hierarchical file system, fork/exec, pipes, sockets |
| [WLM](wlm.md) | Workload Manager | Performance and resource management | Service classes, goals, resource groups, classification |
| [SMF](smf.md) | System Management Facilities | System accounting and monitoring | Record types 0-255, SMF exits, recording control |
| [Networking](networking.md) | VTAM / TCP/IP / SNA | Communications and networking | SNA/LU sessions, TCP/IP stack, TN3270, FTP |

## Subsystem Interaction Model

Subsystems on z/OS interact through several mechanisms:

- **SSI (Subsystem Interface)**: Standard interface for subsystem communication. JES2, CICS, and DB2 all register as subsystems and respond to SSI requests.
- **SAF (System Authorization Facility)**: Routes security requests from any subsystem to the active security product (RACF). Every resource access check flows through SAF.
- **Cross-memory services**: Allow address spaces to invoke services in other address spaces without task switching overhead.
- **ENQ/DEQ**: System-wide serialization mechanism used to coordinate access to shared resources across subsystems.
- **WTO/WTOR**: Write-to-operator messages allow subsystems to communicate with operations staff and automation.

## Subsystem Categories

### Transaction Processing
- **CICS** handles online transactions with sub-second response times
- **IMS/TM** (not covered here) provides similar capabilities with DL/I database access

### Batch Processing
- **JES2** manages the complete batch job lifecycle from submission to output
- **MVS** provides the foundational services (DYNALLOC, program fetch, ABEND handling)

### Security
- **RACF** protects all resources through profiles and access lists
- **SAF** provides the routing layer so that any subsystem can issue security checks

### Interactive Access
- **TSO** provides the command-line session environment
- **ISPF** layers a panel-driven interface on top of TSO

### Middleware
- **MQ** enables asynchronous messaging between applications
- **Networking** provides SNA and TCP/IP connectivity

### System Management
- **WLM** manages performance goals and resource allocation
- **SMF** collects accounting, performance, and security audit data
"#
    .to_string()
}

fn generate_cics_index() -> String {
    r#"# CICS (Customer Information Control System)

CICS is IBM's general-purpose online transaction processing (OLTP) subsystem for z/OS. It supports thousands of concurrent users executing short-duration transactions with sub-second response times.

## Transaction Processing Model

### Pseudo-Conversational Model

The pseudo-conversational model is the standard CICS programming pattern. It simulates a continuous conversation with the user while freeing resources between interactions:

1. **Transaction starts**: User presses a key (e.g., Enter, PF key)
2. **Program executes**: Processes input, updates data, builds output screen
3. **SEND MAP**: Sends the output screen to the terminal
4. **RETURN TRANSID**: Returns control to CICS, specifying the next transaction to invoke
5. **Task ends**: All resources (storage, files, enqueues) are freed
6. **User thinks**: Terminal is idle; CICS serves other users
7. **Next input**: User presses a key, CICS starts the specified transaction

```cobol
           EXEC CICS SEND MAP('MENUMAP')
                     MAPSET('MENUSET')
                     FROM(MENU-MAPO)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN TRANSID('MN01')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(WS-COMM-LEN)
           END-EXEC.
```

### Conversational Model

In a conversational transaction, the task remains active while waiting for user input. This ties up resources and is generally discouraged for production use:

```cobol
           EXEC CICS SEND MAP('MENUMAP')
                     MAPSET('MENUSET')
           END-EXEC.
           EXEC CICS RECEIVE MAP('MENUMAP')
                     MAPSET('MENUSET')
                     INTO(MENU-MAPI)
           END-EXEC.
```

## Terminal Management

### 3270 Data Streams

CICS communicates with terminals using the 3270 data stream protocol:

- **SBA (Set Buffer Address)**: Positions the cursor at a specific screen location
- **SF (Start Field)**: Defines field attributes (protected, unprotected, numeric, bright, dark)
- **SFE (Start Field Extended)**: Adds color, highlighting, and extended attributes
- **IC (Insert Cursor)**: Positions the cursor for user input
- **RA (Repeat to Address)**: Fills a screen area with a character
- **EW (Erase/Write)**: Clears the screen and writes new data
- **EAU (Erase All Unprotected)**: Clears only input fields

### BMS (Basic Mapping Support)

BMS provides a device-independent way to format 3270 screens:

- **DFHMSD**: Defines a mapset (collection of maps)
- **DFHMDI**: Defines an individual map (screen layout)
- **DFHMDF**: Defines a field within a map

```
MENUSET  DFHMSD TYPE=MAP,MODE=INOUT,LANG=COBOL,               X
               STORAGE=AUTO,TIOAPFX=YES
MENUMAP  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
TITLE    DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),       X
               INITIAL='MAIN MENU'
USERID   DFHMDF POS=(3,15),LENGTH=8,ATTRB=(UNPROT,IC)
         DFHMSD TYPE=FINAL
```

BMS generates two COBOL copybooks:
- **Input map** (xxxI): Fields populated from terminal input (RECEIVE MAP)
- **Output map** (xxxO REDEFINES xxxI): Fields you populate before SEND MAP

## Program Control

| Command | Description |
|---------|-------------|
| **LINK** | Call a program, expecting it to return. Like a subroutine call. |
| **XCTL** | Transfer control to another program. The current program is released. |
| **RETURN** | Return to the calling program (LINK) or to CICS (top-level). |
| **LOAD** | Load a program into storage and return its address. |
| **RELEASE** | Release a previously LOADed program. |

### COMMAREA (Communication Area)

The COMMAREA is the primary mechanism for passing data between programs and between pseudo-conversational transactions:

- Maximum size: 32,763 bytes
- Passed on LINK, XCTL, and RETURN TRANSID
- Available in the invoked program via `DFHCOMMAREA` or `EIBCALEN`
- On first invocation, `EIBCALEN = 0` (no COMMAREA passed)

### Channels and Containers

Channels and containers provide an alternative to COMMAREA with no size limit:

- A **channel** is a named collection of containers
- A **container** holds a named block of data (up to 2 GB)
- Passed on LINK, XCTL, and RETURN

## File Control

CICS File Control provides access to VSAM datasets and other file types:

| Operation | Command | Description |
|-----------|---------|-------------|
| Direct read | READ | Read a record by key |
| Write | WRITE | Add a new record |
| Update | READ UPDATE + REWRITE | Read for update, then rewrite |
| Delete | DELETE | Delete a record by key |
| Browse | STARTBR + READNEXT/READPREV + ENDBR | Sequential access |

### VSAM Access Methods

| Type | Description | Key Access |
|------|-------------|------------|
| KSDS | Key-Sequenced Data Set | Primary key + alternate indexes |
| ESDS | Entry-Sequenced Data Set | RBA (Relative Byte Address) |
| RRDS | Relative Record Data Set | RRN (Relative Record Number) |

### Temporary Storage (TS) Queues

TS queues provide scratchpad storage for transactions:
- **Main TS**: Stored in memory (volatile)
- **Auxiliary TS**: Written to the TS dataset (survives CICS restart)
- Items are numbered sequentially
- Common uses: screen paging data, cross-transaction data passing

### Transient Data (TD) Queues

TD queues provide sequential data streams:
- **Intrapartition**: Internal to CICS, can trigger automatic transaction initiation (ATI)
- **Extrapartition**: Mapped to external sequential datasets
- Records are read destructively (once read, they are gone)

## EIB (EXEC Interface Block)

The EIB is a CICS-managed data area available to every program. It contains information about the current task and the most recent CICS command:

| Field | Type | Description |
|-------|------|-------------|
| EIBAID | PIC X(1) | Last AID key pressed (Enter, PF1-PF24, PA1-PA3, CLEAR) |
| EIBCALEN | PIC S9(4) COMP | Length of COMMAREA passed to this program (0 = none) |
| EIBDATE | PIC S9(7) COMP-3 | Current date in 0CYYDDD packed decimal format |
| EIBDS | PIC X(8) | Last dataset name referenced in a file control command |
| EIBRCODE | PIC X(6) | Return code from the last CICS command (raw) |
| EIBRESP | PIC S9(8) COMP | Response code from the last command (NORMAL=0, see [response codes](response-codes.md)) |
| EIBRESP2 | PIC S9(8) COMP | Extended response code (command-specific detail) |
| EIBRSRCE | PIC X(8) | Resource name from the last command (file, queue, program) |
| EIBTASKN | PIC S9(7) COMP-3 | Task number assigned by CICS for this transaction |
| EIBTIME | PIC S9(7) COMP-3 | Current time in 0HHMMSS packed decimal format |
| EIBTRMID | PIC X(4) | Terminal ID associated with this task |
| EIBTRNID | PIC X(4) | Transaction ID that started this task |

### Checking EIBAID

```cobol
       01  DFHAID.
           05 DFHENTER  PIC X VALUE X'7D'.
           05 DFHCLEAR  PIC X VALUE X'6D'.
           05 DFHPA1    PIC X VALUE X'6C'.
           05 DFHPA2    PIC X VALUE X'6E'.
           05 DFHPA3    PIC X VALUE X'6B'.
           05 DFHPF1    PIC X VALUE X'F1'.
           05 DFHPF3    PIC X VALUE X'F3'.
           05 DFHPF7    PIC X VALUE X'F7'.
           05 DFHPF8    PIC X VALUE X'F8'.
           05 DFHPF12   PIC X VALUE X'7C'.

       PROCEDURE DIVISION.
           EVALUATE EIBAID
               WHEN DFHENTER  PERFORM PROCESS-INPUT
               WHEN DFHPF3    PERFORM EXIT-PROGRAM
               WHEN DFHPF7    PERFORM PAGE-BACKWARD
               WHEN DFHPF8    PERFORM PAGE-FORWARD
               WHEN DFHCLEAR  PERFORM CLEAR-SCREEN
               WHEN OTHER     PERFORM INVALID-KEY
           END-EVALUATE.
```

## CICS Regions

In a CICSplex, workload is distributed across specialized CICS regions:

| Region | Full Name | Role |
|--------|-----------|------|
| **TOR** | Terminal Owning Region | Manages terminal connections and routes transactions to AORs |
| **AOR** | Application Owning Region | Runs application programs (the workhorse region) |
| **FOR** | File Owning Region | Owns VSAM files; AORs issue function-shipped file requests |
| **QOR** | Queue Owning Region | Owns TS/TD queues; AORs function-ship queue requests |

### Benefits of Multi-Region Operation (MRO)

- **Isolation**: A failing application in one AOR does not affect others
- **Scalability**: Add more AORs to handle increased transaction volume
- **Availability**: TOR can route around a failed AOR
- **Maintenance**: Recycle individual AORs without dropping terminal connections

## Resource Definition

### CSD (CICS System Definition)

The CSD is a VSAM KSDS that stores resource definitions. Resources are organized into **groups** and **lists**:

```
CEDA DEFINE PROGRAM(COSGN00C) GROUP(CARDDEMO)
     LANGUAGE(COBOL) RELOAD(NO) STATUS(ENABLED)

CEDA DEFINE TRANSACTION(CC00) GROUP(CARDDEMO)
     PROGRAM(COSGN00C) STATUS(ENABLED)

CEDA DEFINE FILE(ACCTDAT) GROUP(CARDDEMO)
     DSNAME(CARDDEMO.ACCTDAT) STATUS(ENABLED)
     OPENTIME(FIRSTREF) DISPOSITION(SHARE)
     RECORDFORMAT(F) ADD(YES) BROWSE(YES)
     DELETE(YES) READ(YES) UPDATE(YES)
```

### RDO (Resource Definition Online)

RDO commands manage resources at runtime:

| Command | Purpose |
|---------|---------|
| CEDA DEFINE | Create a new resource definition |
| CEDA ALTER | Modify an existing definition |
| CEDA DELETE | Remove a definition |
| CEDA INSTALL | Make a definition active in the running region |
| CEDA VIEW | Display a definition |
| CEMT INQUIRE | Query runtime status of installed resources |
| CEMT SET | Change runtime attributes of installed resources |

See also: [CICS Commands](commands.md) | [EIBRESP Response Codes](response-codes.md)
"#
    .to_string()
}

fn generate_cics_commands() -> String {
    r#"# CICS Command Reference

All CICS commands are issued using the `EXEC CICS ... END-EXEC` syntax in COBOL (or the equivalent in other languages). Commands can include the `RESP(data-area)` and `RESP2(data-area)` options to capture response codes instead of raising conditions.

```cobol
           EXEC CICS READ FILE('ACCTDAT')
                     INTO(WS-RECORD)
                     RIDFLD(WS-KEY)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM HANDLE-ERROR
           END-IF.
```

---

## Program Control

### LINK

Call a program as a subroutine. The linked program receives control and returns via RETURN.

```
EXEC CICS LINK PROGRAM(name)
               [COMMAREA(data-area) LENGTH(data-value)]
               [CHANNEL(name)]
               [INPUTMSG(data-area) INPUTMSGLEN(data-value)]
               [SYSID(name)]
               [SYNCONRETURN]
END-EXEC
```

**Conditions**: PGMIDERR (27), NOTAUTH (70), LENGERR (22), SYSIDERR (52)

### XCTL

Transfer control to another program. The current program is released and cannot be returned to.

```
EXEC CICS XCTL PROGRAM(name)
               [COMMAREA(data-area) LENGTH(data-value)]
               [CHANNEL(name)]
               [INPUTMSG(data-area) INPUTMSGLEN(data-value)]
END-EXEC
```

**Conditions**: PGMIDERR (27), NOTAUTH (70), LENGERR (22)

### RETURN

Return to the calling program (if LINKed) or to CICS (if top-level). Optionally specifies the next transaction for pseudo-conversational processing.

```
EXEC CICS RETURN [TRANSID(name)]
                 [COMMAREA(data-area) LENGTH(data-value)]
                 [CHANNEL(name)]
                 [IMMEDIATE]
                 [INPUTMSG(data-area) INPUTMSGLEN(data-value)]
END-EXEC
```

**Conditions**: INVREQ (16), LENGERR (22)

### LOAD

Load a program or table into storage and return its entry point address.

```
EXEC CICS LOAD PROGRAM(name)
               SET(ptr-ref)
               [LENGTH(data-area)]
               [ENTRY(ptr-ref)]
               [HOLD]
END-EXEC
```

**Conditions**: PGMIDERR (27), NOTAUTH (70), LENGERR (22)

### RELEASE

Release a previously LOADed program.

```
EXEC CICS RELEASE PROGRAM(name)
END-EXEC
```

**Conditions**: PGMIDERR (27), INVREQ (16)

---

## Terminal Control

### SEND MAP

Send a formatted BMS map to the terminal.

```
EXEC CICS SEND MAP(name)
               MAPSET(name)
               [FROM(data-area)]
               [DATAONLY | MAPONLY]
               [ERASE | ERASEAUP]
               [FREEKB]
               [CURSOR(data-value)]
               [ALARM]
               [ACCUM]
               [PAGING]
               [PRINT]
END-EXEC
```

**Options**:
- `MAPONLY`: Send only the constant (map-defined) fields, ignoring the data area
- `DATAONLY`: Send only the variable (program-supplied) fields, leaving constants unchanged
- `ERASE`: Clear the entire screen before writing
- `ERASEAUP`: Clear only unprotected fields before writing
- `FREEKB`: Unlock the keyboard
- `CURSOR`: Position the cursor at the specified offset, or at the field with CURSOR(-1) in the symbolic map

**Conditions**: INVREQ (16), INVMPSZ (38), MAPFAIL (36)

### RECEIVE MAP

Receive input from a BMS map on the terminal.

```
EXEC CICS RECEIVE MAP(name)
                  MAPSET(name)
                  INTO(data-area)
                  [SET(ptr-ref)]
END-EXEC
```

**Conditions**: MAPFAIL (36), INVREQ (16), INVMPSZ (38)

### SEND TEXT

Send unformatted text to the terminal.

```
EXEC CICS SEND TEXT FROM(data-area)
                    LENGTH(data-value)
                    [ERASE]
                    [FREEKB]
                    [ALARM]
                    [CURSOR(data-value)]
                    [ACCUM]
                    [PAGING]
                    [HEADER(data-area)]
                    [TRAILER(data-area)]
END-EXEC
```

**Conditions**: INVREQ (16), LENGERR (22)

### SEND CONTROL

Send device control orders to the terminal without data.

```
EXEC CICS SEND CONTROL [ERASE]
                       [ERASEAUP]
                       [FREEKB]
                       [CURSOR(data-value)]
                       [ALARM]
END-EXEC
```

**Conditions**: INVREQ (16)

---

## File Control

### READ

Read a record directly by key.

```
EXEC CICS READ FILE(name)
               INTO(data-area)
               [SET(ptr-ref)]
               RIDFLD(data-area)
               [KEYLENGTH(data-value)]
               [LENGTH(data-area)]
               [UPDATE]
               [GENERIC]
               [GTEQ | EQUAL]
               [RBA | RRN | XRBA]
               [SYSID(name)]
END-EXEC
```

**Options**:
- `UPDATE`: Read for update (must be followed by REWRITE, DELETE, or UNLOCK)
- `GENERIC`: Use partial key matching (requires KEYLENGTH)
- `GTEQ`: Read the record with a key >= the specified key
- `EQUAL`: Read only if key matches exactly (default)

**Conditions**: FILENOTFOUND (12), NOTFND (13), LENGERR (22), NOTOPEN (19), DISABLED (84), DUPKEY (15), INVREQ (16), IOERR (17), ILLOGIC (21), NOTAUTH (70), LOADING (94)

### WRITE

Write a new record to a file.

```
EXEC CICS WRITE FILE(name)
                FROM(data-area)
                RIDFLD(data-area)
                [KEYLENGTH(data-value)]
                [LENGTH(data-value)]
                [RBA | RRN | XRBA]
                [SYSID(name)]
                [MASSINSERT]
END-EXEC
```

**Conditions**: FILENOTFOUND (12), DUPREC (14), NOSPACE (18), NOTOPEN (19), DISABLED (84), INVREQ (16), IOERR (17), ILLOGIC (21), LENGERR (22), NOTAUTH (70), LOADING (94)

### REWRITE

Rewrite a record that was previously read with UPDATE.

```
EXEC CICS REWRITE FILE(name)
                  FROM(data-area)
                  [LENGTH(data-value)]
                  [SYSID(name)]
END-EXEC
```

**Conditions**: FILENOTFOUND (12), NOSPACE (18), NOTOPEN (19), DISABLED (84), INVREQ (16), IOERR (17), ILLOGIC (21), LENGERR (22), DUPREC (14), NOTAUTH (70)

### DELETE

Delete one or more records.

```
EXEC CICS DELETE FILE(name)
                 RIDFLD(data-area)
                 [KEYLENGTH(data-value)]
                 [GENERIC]
                 [NUMREC(data-area)]
                 [RBA | RRN | XRBA]
                 [SYSID(name)]
END-EXEC
```

**Conditions**: FILENOTFOUND (12), NOTFND (13), NOTOPEN (19), DISABLED (84), INVREQ (16), IOERR (17), ILLOGIC (21), DUPKEY (15), NOTAUTH (70)

### STARTBR

Start a browse (sequential read) operation on a file.

```
EXEC CICS STARTBR FILE(name)
                  RIDFLD(data-area)
                  [KEYLENGTH(data-value)]
                  [GENERIC]
                  [GTEQ | EQUAL]
                  [RBA | RRN | XRBA]
                  [REQID(data-value)]
                  [SYSID(name)]
END-EXEC
```

**Conditions**: FILENOTFOUND (12), NOTFND (13), NOTOPEN (19), DISABLED (84), INVREQ (16), IOERR (17), ILLOGIC (21), NOTAUTH (70)

### READNEXT

Read the next record in a browse operation.

```
EXEC CICS READNEXT FILE(name)
                   INTO(data-area)
                   [SET(ptr-ref)]
                   RIDFLD(data-area)
                   [KEYLENGTH(data-area)]
                   [LENGTH(data-area)]
                   [REQID(data-value)]
                   [RBA | RRN | XRBA]
                   [SYSID(name)]
END-EXEC
```

**Conditions**: ENDFILE (20), FILENOTFOUND (12), NOTFND (13), LENGERR (22), NOTOPEN (19), DISABLED (84), INVREQ (16), IOERR (17), ILLOGIC (21), DUPKEY (15), NOTAUTH (70)

### READPREV

Read the previous record in a browse operation.

```
EXEC CICS READPREV FILE(name)
                   INTO(data-area)
                   [SET(ptr-ref)]
                   RIDFLD(data-area)
                   [KEYLENGTH(data-area)]
                   [LENGTH(data-area)]
                   [REQID(data-value)]
                   [RBA | RRN | XRBA]
                   [SYSID(name)]
END-EXEC
```

**Conditions**: ENDFILE (20), FILENOTFOUND (12), NOTFND (13), LENGERR (22), NOTOPEN (19), DISABLED (84), INVREQ (16), IOERR (17), ILLOGIC (21), DUPKEY (15), NOTAUTH (70)

### ENDBR

End a browse operation.

```
EXEC CICS ENDBR FILE(name)
                [REQID(data-value)]
                [SYSID(name)]
END-EXEC
```

**Conditions**: FILENOTFOUND (12), INVREQ (16), NOTAUTH (70), ILLOGIC (21)

### UNLOCK

Release an UPDATE lock without modifying the record.

```
EXEC CICS UNLOCK FILE(name)
                 [SYSID(name)]
END-EXEC
```

**Conditions**: FILENOTFOUND (12), INVREQ (16), DISABLED (84), NOTAUTH (70), IOERR (17)

---

## Queue Control

### WRITEQ TS

Write an item to a Temporary Storage queue.

```
EXEC CICS WRITEQ TS QUEUE(name)
                    FROM(data-area)
                    [LENGTH(data-value)]
                    [ITEM(data-area)]
                    [REWRITE]
                    [NUMITEMS(data-area)]
                    [MAIN | AUXILIARY]
                    [SYSID(name)]
END-EXEC
```

**Options**:
- `ITEM`: Specify or receive the item number
- `REWRITE`: Overwrite an existing item (requires ITEM)
- `MAIN`: Store in main storage (volatile)
- `AUXILIARY`: Store on the TS dataset (default, survives restart)

**Conditions**: NOSPACE (18), INVREQ (16), IOERR (17), ITEMERR (26), QIDERR (44), NOTAUTH (70), LENGERR (22)

### READQ TS

Read an item from a Temporary Storage queue.

```
EXEC CICS READQ TS QUEUE(name)
                   INTO(data-area)
                   [SET(ptr-ref)]
                   [LENGTH(data-area)]
                   [ITEM(data-value)]
                   [NEXT]
                   [NUMITEMS(data-area)]
                   [SYSID(name)]
END-EXEC
```

**Options**:
- `ITEM`: Read a specific item by number
- `NEXT`: Read the next sequential item (default)

**Conditions**: QIDERR (44), ITEMERR (26), INVREQ (16), IOERR (17), LENGERR (22), NOTAUTH (70)

### DELETEQ TS

Delete an entire Temporary Storage queue.

```
EXEC CICS DELETEQ TS QUEUE(name)
                     [SYSID(name)]
END-EXEC
```

**Conditions**: QIDERR (44), INVREQ (16), NOTAUTH (70), IOERR (17)

### WRITEQ TD

Write a record to a Transient Data queue.

```
EXEC CICS WRITEQ TD QUEUE(name)
                    FROM(data-area)
                    LENGTH(data-value)
                    [SYSID(name)]
END-EXEC
```

**Conditions**: QIDERR (44), NOSPACE (18), INVREQ (16), IOERR (17), LENGERR (22), NOTOPEN (19), DISABLED (84), NOTAUTH (70)

### READQ TD

Read a record from a Transient Data queue (destructive read).

```
EXEC CICS READQ TD QUEUE(name)
                   INTO(data-area)
                   [SET(ptr-ref)]
                   LENGTH(data-area)
                   [SYSID(name)]
END-EXEC
```

**Conditions**: QZERO (23), QIDERR (44), INVREQ (16), IOERR (17), LENGERR (22), NOTOPEN (19), DISABLED (84), NOTAUTH (70)

---

## Interval Control

### DELAY

Suspend the task for a specified duration.

```
EXEC CICS DELAY INTERVAL(hhmmss)
END-EXEC

EXEC CICS DELAY FOR HOURS(data-value)
                    MINUTES(data-value)
                    SECONDS(data-value)
END-EXEC
```

**Conditions**: INVREQ (16), EXPIRED (31)

### START

Schedule a transaction to run after an interval or at a specified time.

```
EXEC CICS START TRANSID(name)
                [INTERVAL(hhmmss)]
                [TIME(hhmmss)]
                [AFTER HOURS(h) MINUTES(m) SECONDS(s)]
                [AT HOURS(h) MINUTES(m) SECONDS(s)]
                [FROM(data-area) LENGTH(data-value)]
                [REQID(name)]
                [TERMID(name)]
                [QUEUE(name)]
                [RTRANSID(name)]
                [RTERMID(name)]
                [SYSID(name)]
                [CHANNEL(name)]
END-EXEC
```

**Conditions**: TRANSIDERR (28), INVREQ (16), IOERR (17), TERMIDERR (11), LENGERR (22), NOTAUTH (70), SYSIDERR (52)

### CANCEL

Cancel a previously started interval control request.

```
EXEC CICS CANCEL REQID(name)
                 [SYSID(name)]
                 [TRANSID(name)]
END-EXEC
```

**Conditions**: NOTFND (13), INVREQ (16), NOTAUTH (70), SYSIDERR (52)

### ASKTIME

Retrieve the current date and time as an absolute time value.

```
EXEC CICS ASKTIME [ABSTIME(data-area)]
END-EXEC
```

**Conditions**: INVREQ (16)

### FORMATTIME

Convert an absolute time value into formatted date/time components.

```
EXEC CICS FORMATTIME ABSTIME(data-area)
                     [DATE(data-area) [DATESEP(char)]]
                     [DATEFORM(data-area)]
                     [FULLDATE(data-area)]
                     [DAYCOUNT(data-area)]
                     [DAYOFWEEK(data-area)]
                     [DAYOFMONTH(data-area)]
                     [MONTHOFYEAR(data-area)]
                     [YEAR(data-area)]
                     [TIME(data-area) [TIMESEP(char)]]
                     [YYYYMMDD(data-area)]
                     [YYYYDDD(data-area)]
                     [YYDDD(data-area)]
                     [MMDDYYYY(data-area)]
                     [DDMMYYYY(data-area)]
END-EXEC
```

**Conditions**: INVREQ (16)

---

## Task Control

### SUSPEND

Voluntarily relinquish control, allowing other tasks of equal or higher priority to run.

```
EXEC CICS SUSPEND
END-EXEC
```

### ENQ

Enqueue on a named resource to serialize access.

```
EXEC CICS ENQ RESOURCE(data-area)
              [LENGTH(data-value)]
              [NOSUSPEND]
              [MAXLIFETIME(data-value)]
END-EXEC
```

**Conditions**: ENQBUSY (54), INVREQ (16), LENGERR (22)

### DEQ

Release a previously acquired enqueue.

```
EXEC CICS DEQ RESOURCE(data-area)
              [LENGTH(data-value)]
END-EXEC
```

**Conditions**: INVREQ (16), LENGERR (22)

### ASSIGN

Retrieve CICS system values into program variables.

```
EXEC CICS ASSIGN [USERID(data-area)]
                 [APPLID(data-area)]
                 [SYSID(data-area)]
                 [FACILITY(data-area)]
                 [OPID(data-area)]
                 [NETNAME(data-area)]
                 [STARTCODE(data-area)]
                 [INVOKINGPROG(data-area)]
                 [ABCODE(data-area)]
                 [CWALENG(data-area)]
END-EXEC
```

**Conditions**: INVREQ (16)

---

## Storage Control

### GETMAIN

Acquire a block of storage.

```
EXEC CICS GETMAIN SET(ptr-ref)
                  LENGTH(data-value) | FLENGTH(data-value)
                  [INITIMG(data-value)]
                  [BELOW]
                  [SHARED]
                  [CICSSTATUS(data-area)]
END-EXEC
```

**Options**:
- `INITIMG`: Initialize storage with the specified hex value (e.g., X'00')
- `BELOW`: Allocate below the 16 MB line (for AMODE 24 programs)
- `SHARED`: Storage persists beyond the task lifetime

**Conditions**: NOSTG (42), INVREQ (16), LENGERR (22)

### FREEMAIN

Release previously acquired storage.

```
EXEC CICS FREEMAIN DATA(data-area)
                   [LENGTH(data-value)]
END-EXEC
```

**Conditions**: INVREQ (16)

---

## Exception Handling

### HANDLE CONDITION

Register a label to receive control when a specific condition occurs.

```
EXEC CICS HANDLE CONDITION condition(label)
                           [condition2(label2)]
                           ...
END-EXEC
```

Example:
```cobol
           EXEC CICS HANDLE CONDITION
                     NOTFND(NOT-FOUND-RTN)
                     DUPKEY(DUP-KEY-RTN)
                     ERROR(GENERAL-ERROR)
           END-EXEC.
```

### HANDLE AID

Register labels for specific AID keys.

```
EXEC CICS HANDLE AID key(label)
                     [key2(label2)]
                     ...
                     [ANYKEY(label)]
END-EXEC
```

Example:
```cobol
           EXEC CICS HANDLE AID
                     PF3(EXIT-RTN)
                     PF7(PAGE-BACK)
                     PF8(PAGE-FWD)
                     CLEAR(CLEAR-RTN)
                     ANYKEY(INVALID-KEY)
           END-EXEC.
```

### HANDLE ABEND

Establish an abend exit.

```
EXEC CICS HANDLE ABEND LABEL(label)
END-EXEC

EXEC CICS HANDLE ABEND PROGRAM(name)
END-EXEC

EXEC CICS HANDLE ABEND CANCEL
END-EXEC

EXEC CICS HANDLE ABEND RESET
END-EXEC
```

**Options**:
- `LABEL`: Branch to a paragraph within the current program
- `PROGRAM`: Transfer control to the named program
- `CANCEL`: Cancel the current abend handler
- `RESET`: Reactivate the most recently cancelled handler

### IGNORE CONDITION

Suppress a condition so it does not raise an exception.

```
EXEC CICS IGNORE CONDITION condition
                           [condition2]
                           ...
END-EXEC
```

### PUSH HANDLE

Save the current set of HANDLE CONDITION, HANDLE AID, and HANDLE ABEND settings.

```
EXEC CICS PUSH HANDLE
END-EXEC
```

### POP HANDLE

Restore the previously saved set of HANDLE settings.

```
EXEC CICS POP HANDLE
END-EXEC
```

---

## Syncpoint

### SYNCPOINT

Commit all recoverable resource changes made during the current unit of work.

```
EXEC CICS SYNCPOINT [ROLLBACK]
END-EXEC
```

**Options**:
- *(no option)*: Commit all changes
- `ROLLBACK`: Back out all changes since the last syncpoint

**Conditions**: INVREQ (16), ROLLEDBACK (82)

---

See also: [CICS Overview](index.md) | [EIBRESP Response Codes](response-codes.md)
"#
    .to_string()
}

fn generate_cics_response_codes() -> String {
    r#"# CICS EIBRESP Response Codes

The EIBRESP field in the EIB (EXEC Interface Block) contains the response code from the most recent CICS command. Programs should check EIBRESP after every command using the `RESP` option or `DFHRESP` built-in function.

## Checking Response Codes

```cobol
           EXEC CICS READ FILE('ACCTDAT')
                     INTO(WS-ACCT-REC)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE 'RECORD NOT FOUND' TO WS-MSG
               WHEN DFHRESP(DISABLED)
                   MOVE 'FILE IS DISABLED' TO WS-MSG
               WHEN OTHER
                   MOVE 'UNEXPECTED ERROR' TO WS-MSG
           END-EVALUATE.
```

## Complete Response Code Table

| Value | Name | Meaning |
|-------|------|---------|
| 0 | NORMAL | Command completed successfully |
| 1 | ERROR | General error condition |
| 2 | RDATT | Read attention (terminal read was interrupted) |
| 3 | WRBRK | Write break (terminal output was interrupted) |
| 4 | EOF | End of file reached during sequential browse |
| 5 | EODS | End of data set |
| 6 | EOC | End of chain (SNA LU6.2) |
| 7 | INBFMH | Inbound FMH received |
| 8 | ENDINPT | End of input from terminal |
| 9 | NONVAL | Non-validated input |
| 10 | NOSTART | Distributed program link not supported |
| 11 | TERMIDERR | Terminal ID not found in the CICS terminal table |
| 12 | FILENOTFOUND | File name not defined in CICS file resource table |
| 13 | NOTFND | Record not found for the specified key |
| 14 | DUPREC | Duplicate record; a record with this key already exists |
| 15 | DUPKEY | Duplicate key detected on an alternate index |
| 16 | INVREQ | Invalid request; the command is not valid in the current context |
| 17 | IOERR | I/O error on file or queue access |
| 18 | NOSPACE | No space available on the file or queue |
| 19 | NOTOPEN | File or queue is closed and OPENTIME is not FIRSTREF |
| 20 | ENDFILE | End of file reached during browse (READNEXT/READPREV) |
| 21 | ILLOGIC | VSAM logical error (check EIBRESP2 for VSAM return/reason codes) |
| 22 | LENGERR | Length error; data area is too small or LENGTH is incorrect |
| 23 | QZERO | TD queue is empty (no records to read) |
| 24 | SIGNAL | SNA signal received |
| 25 | QBUSY | TD queue is busy (in use by another task) |
| 26 | ITEMERR | TS queue item number is out of range |
| 27 | PGMIDERR | Program not found in the CICS program resource table |
| 28 | TRANSIDERR | Transaction ID not found in the CICS transaction table |
| 29 | ENDDATA | No more data to retrieve (RETRIEVE command) |
| 30 | INVTSREQ | Invalid TS queue request |
| 31 | EXPIRED | Timer has already expired |
| 32 | RETPAGE | Page retrieval complete |
| 33 | RTEFAIL | Route request failed |
| 34 | RTESOME | Route request partially completed |
| 35 | TSIOERR | TS queue I/O error |
| 36 | MAPFAIL | No data received for the map (user pressed Enter with no input changes) |
| 37 | INVERRTERM | Invalid error terminal |
| 38 | INVMPSZ | Map size exceeds terminal screen size |
| 39 | IGREQID | Request ID already active (duplicate REQID on START/DELAY) |
| 40 | OVERFLOW | Map page overflow |
| 41 | INVLDC | Invalid logical device code |
| 42 | NOSTG | Insufficient storage for GETMAIN request |
| 43 | JIDERR | Journal ID error |
| 44 | QIDERR | Queue name not found (TS or TD queue does not exist) |
| 45 | NOJBUFSP | No journal buffer space available |
| 46 | DSSTAT | Dataset status error |
| 47 | SELNERR | Selection error |
| 48 | FUNCERR | Function error in DPL |
| 49 | UNEXPIN | Unexpected input received |
| 50 | NOPASSBKRD | No passbook read capability |
| 51 | NOPASSBKWR | No passbook write capability |
| 52 | SYSIDERR | Remote system ID not found or unavailable |
| 53 | ISCINVREQ | ISC invalid request |
| 54 | ENQBUSY | Resource already enqueued by another task (with NOSUSPEND) |
| 55 | ENVDEFERR | Environment definition error |
| 56 | IGREQCD | Request code error |
| 57 | SESSIONERR | Session error on ISC/MRO link |
| 58 | SYSBUSY | Remote system is busy |
| 59 | SESSBUSY | Session is busy |
| 60 | NOTALLOC | Session not allocated |
| 61 | CBIDERR | Container/channel error |
| 62 | INVEXITREQ | Invalid exit request |
| 63 | INVPARTNSET | Invalid partner set |
| 64 | INVPARTN | Invalid partner |
| 65 | PARTNFAIL | Partner failed |
| 66 | USERIDERR | User ID not recognized by the security manager |
| 70 | NOTAUTH | Not authorized; security check failed for the resource |
| 72 | SUPPRESSED | Condition was suppressed by IGNORE CONDITION |
| 81 | VOLIDERR | Volume ID error |
| 84 | DISABLED | Resource is disabled (file, program, or transaction) |
| 94 | LOADING | Program is currently being loaded; retry later |

## Common Response Code Patterns

### File Operations

| Scenario | EIBRESP | EIBRESP2 | Action |
|----------|---------|----------|--------|
| Record found | NORMAL (0) | 0 | Process record |
| Record not found | NOTFND (13) | 0 | Display "not found" message |
| Duplicate key on WRITE | DUPREC (14) | 0 | Display "already exists" message |
| File not defined | FILENOTFOUND (12) | 0 | Check file resource definition |
| File disabled | DISABLED (84) | 0 | Enable file via CEMT SET FILE ENABLED |
| End of browse | ENDFILE (20) | 0 | End browse with ENDBR |
| VSAM error | ILLOGIC (21) | VSAM RC/FDBK | Check VSAM return/feedback codes |

### Program Control

| Scenario | EIBRESP | EIBRESP2 | Action |
|----------|---------|----------|--------|
| Program not found | PGMIDERR (27) | 0 | Check program definition and load library |
| Not authorized | NOTAUTH (70) | 0 | Check RACF PROGRAM class profile |
| Program loading | LOADING (94) | 0 | Retry after short delay |
| Bad COMMAREA length | LENGERR (22) | 0 | Check LENGTH parameter |

### Terminal I/O

| Scenario | EIBRESP | EIBRESP2 | Action |
|----------|---------|----------|--------|
| No input changes | MAPFAIL (36) | 0 | Handle "no data" case (AID-only input) |
| Map too large | INVMPSZ (38) | 0 | Check map size vs terminal model |
| Bad terminal ID | TERMIDERR (11) | 0 | Verify terminal is installed |

See also: [CICS Overview](index.md) | [CICS Commands](commands.md)
"#
    .to_string()
}

fn generate_jes2() -> String {
    r#"# JES2 (Job Entry Subsystem 2)

JES2 is the primary job entry subsystem on z/OS, responsible for managing the lifecycle of batch jobs from submission through execution to output processing.

## Job Lifecycle

A batch job moves through six distinct phases:

```
Input --> Conversion --> Ready --> Execution --> Output --> Purge
```

| Phase | Description |
|-------|-------------|
| **Input** | Job is received (internal reader, NJE, RJE, or TSO SUBMIT) and placed on the input queue |
| **Conversion** | JES2 scans JCL, expands PROCs, allocates spool space for SYSOUT, and converts to internal text |
| **Ready** | Job is queued and waiting for an initiator with matching job class |
| **Execution** | An initiator selects the job and MVS executes it step by step |
| **Output** | SYSOUT datasets are routed to printers, held queues, or external destinations |
| **Purge** | Job output is deleted and spool space is reclaimed |

## Job Classes and Priorities

### Job Classes

Job classes (A-Z, 0-9) categorize jobs for scheduling:

```jcl
//MYJOB   JOB (ACCT),'DESC',CLASS=A,MSGCLASS=X
```

- **CLASS**: Determines which initiators can run the job
- **MSGCLASS**: Output class for JES2 messages (job log)

Initiators are configured to process specific classes:

```
$HASP426 INIT1    - CLASS=A,B,C,ACTIVE
$HASP426 INIT2    - CLASS=D,E,ACTIVE
```

### Job Priorities

Jobs within a class are scheduled by priority (0-15, 15 = highest):

```jcl
//MYJOB   JOB (ACCT),'DESC',PRTY=8
```

## JES2 Operator Commands

All JES2 commands are prefixed with `$`:

### Job Control

| Command | Description |
|---------|-------------|
| `$S JOB(n)` | Start (release) a held job |
| `$P JOB(n)` | Purge a job from the queue |
| `$A JOB(n)` | Activate a job (release from hold) |
| `$C JOB(n)` | Cancel a running job |
| `$D JOB(n)` | Display job information |
| `$T JOB(n),P=nn` | Alter job priority |
| `$H JOB(n)` | Hold a job |

### Initiator Control

| Command | Description |
|---------|-------------|
| `$S INIT(n)` | Start an initiator |
| `$P INIT(n)` | Stop an initiator (drain after current job) |
| `$T INIT(n),CLASS=ABC` | Alter initiator job classes |
| `$D INIT(n)` | Display initiator status |

### Printer/Output Control

| Command | Description |
|---------|-------------|
| `$S PRT(n)` | Start a printer |
| `$P PRT(n)` | Stop a printer |
| `$T PRT(n),CLASS=X` | Alter printer output class |
| `$D PRT(n)` | Display printer status |
| `$S PUNCH(n)` | Start a punch device |

### System Control

| Command | Description |
|---------|-------------|
| `$S JES2` | Start JES2 (cold, warm, or hot start) |
| `$P JES2` | Stop JES2 |
| `$D A,L` | Display all active jobs |
| `$D Q,L` | Display all queued jobs |
| `$D SPOOL` | Display spool utilization |
| `$VS,'cmd'` | Issue an MVS command through JES2 |
| `$T JOBCLASS(A),PGMRNUM=nn` | Alter job class attributes |
| `$D JOBCLASS(A)` | Display job class configuration |

## JES2 Exits

JES2 provides exit points for installation customization:

| Exit | Name | Purpose |
|------|------|---------|
| EXIT1 | Print/Punch Separator | Customize job separator pages |
| EXIT2 | JOB Card Scan | Validate/modify JOB statement during input |
| EXIT3 | JCL/JECL Scan | Validate/modify JCL statements during input |
| EXIT4 | JCL/JECL Converter | Modify JCL during conversion phase |
| EXIT5 | Job Output | Control output processing and routing |
| EXIT6 | JCL Statement Accounting | Customize accounting fields |
| EXIT7 | Job Queue Work Select | Customize job selection for initiators |
| EXIT8 | Control Block Read/Write | Intercept checkpoint I/O |
| EXIT9 | SYSOUT Data Set/Copy Select | Select SYSOUT datasets for processing |
| EXIT10 | SYSOUT Separator | Customize SYSOUT separator pages |
| EXIT11 | SYSOUT Data Set Unallocation | Control SYSOUT at unallocation time |
| EXIT12 | JOB Card SMF | Customize SMF record fields |
| EXIT13 | TSO Interactive Data Transmission | Control TSO data transmission |
| EXIT14 | JOB/JOBGROUP Termination | Post-execution job cleanup |
| EXIT15 | Output Overflow | Handle spool overflow conditions |
| EXIT20 | End of Input | Processing at end of job input |
| EXIT24 | Post-Initialization | Processing after JES2 initialization |
| EXIT44 | JOB/JOBGROUP Purge | Processing before job purge |

## SDSF Panels

SDSF (System Display and Search Facility) is the primary interface for viewing JES2 information:

| Panel | Command | Description |
|-------|---------|-------------|
| **ST** | Status | Display all jobs with their current status |
| **O** | Output | Display jobs on the output queue |
| **H** | Held | Display held output |
| **I** | Input | Display jobs on the input queue |
| **DA** | Active | Display currently executing jobs |
| **SE** | System Events | Display system events and messages |
| **LOG** | System Log | Display the system log (SYSLOG) |
| **MAS** | Members | Display members of the MAS (Multi-Access Spool) |
| **INIT** | Initiators | Display initiator status |
| **PR** | Printers | Display printer status |
| **PUN** | Punches | Display punch device status |

### SDSF Action Characters

| Character | Action |
|-----------|--------|
| ? | Display job details (JCL, SYSOUT, etc.) |
| S | Select (display SYSOUT) |
| P | Purge job |
| C | Cancel job |
| A | Release held job |
| H | Hold job |
| X | Print/punch output |
| SJ | Display job JCL |

## Output Routing

JES2 routes SYSOUT datasets based on several criteria:

```jcl
//SYSOUT   DD SYSOUT=A                      Output class A
//SYSOUT   DD SYSOUT=A,DEST=RMT1            Route to remote station
//SYSOUT   DD SYSOUT=A,DEST=NODE1.USER1     Route to NJE node
//SYSOUT   DD SYSOUT=(A,,FORM1)             Specific forms
//SYSOUT   DD SYSOUT=A,COPIES=3             Multiple copies
//SYSOUT   DD SYSOUT=A,HOLD=YES             Hold for review
```

### Routing Destinations

| Destination | Description |
|-------------|-------------|
| LOCAL | Local printers (default) |
| RMTnnn | Remote RJE workstation |
| Nnnn | NJE node |
| node.userid | Specific user at a node |
| ANYLOCAL | Any local printer |

## Spool Dataset Management

The JES2 spool is a set of DASD volumes that store:
- Input JCL and SYSIN data
- SYSOUT datasets (job output)
- Job logs and JCL listings
- Internal JES2 control blocks

### Spool Management Commands

| Command | Description |
|---------|-------------|
| `$D SPOOL` | Display spool volume utilization |
| `$D SPOOLDEF` | Display spool configuration |
| `$T SPOOL(volser),TGSPACE=ALARM=nn` | Set spool alarm threshold |

### Spool Space Recovery

When spool space is critical:
1. Purge completed jobs: `$P JOB(n)` or SDSF P action
2. Cancel long-running output: `$C PRT(n)`
3. Offload spool data: `$T JOB(n),Q=XA` (external writer)

## JES2 Initialization Parameters (JES2PARM)

Key initialization parameters in the JES2PARM member:

| Parameter | Description |
|-----------|-------------|
| `JOBCLASS(c)` | Define job class attributes |
| `OUTCLASS(c)` | Define output class attributes |
| `INITNUM=nn` | Number of initiators |
| `SPOOL(volser)` | Define spool volumes |
| `CKPTDEF` | Checkpoint dataset definition |
| `MASDEF` | Multi-Access Spool configuration |
| `NODE(n)` | NJE node definition |
| `LINE(n)` | Communication line definition |
| `DESTDEF` | Destination definition |
| `ESTBYTE=nnnnn` | Estimated spool bytes per track |
| `SPOOLDEF` | Global spool parameters |
| `INTRDR` | Internal reader configuration |
| `PRINTDEF` | Default print parameters |
"#
    .to_string()
}

fn generate_racf() -> String {
    r#"# RACF (Resource Access Control Facility)

RACF is IBM's primary security product for z/OS. It controls access to system resources through user profiles, group profiles, and resource profiles.

## User Profiles

### ADDUSER - Create a User

```
ADDUSER userid
        NAME('First Last')
        DFLTGRP(group)
        OWNER(owner)
        PASSWORD(password)
        [OPERATIONS | SPECIAL | AUDITOR]
        [TSO(ACCTNUM(acct) PROC(proc) SIZE(size) MAXSIZE(max))]
        [OMVS(UID(nnn) HOME('/u/userid') PROGRAM('/bin/sh'))]
        [CICS(OPCLASS(nn) OPIDENT(oid) OPPRTY(nn))]
```

### ALTUSER - Modify a User

```
ALTUSER userid
        [NAME('New Name')]
        [DFLTGRP(newgroup)]
        [PASSWORD(newpwd)]
        [OPERATIONS | NOOPERATIONS]
        [SPECIAL | NOSPECIAL]
        [AUDITOR | NOAUDITOR]
        [REVOKE | RESUME]
        [EXPIRED | NOEXPIRED]
        [TSO(ACCTNUM(acct) PROC(proc))]
        [OMVS(UID(nnn) HOME('/u/userid'))]
```

### DELUSER - Delete a User

```
DELUSER userid
```

### LISTUSER - Display a User Profile

```
LISTUSER userid [ALL] [OMVS] [TSO] [CICS]
```

### PASSWORD Rules

```
SETROPTS PASSWORD(
    RULE1(LENGTH(8) CONTENT(CCCAAAAA))
    RULE2(LENGTH(8) CONTENT(CCCCAAAA))
    HISTORY(32)
    INTERVAL(90)
    MINCHANGE(1)
    REVOKE(5)
    WARNING(14)
)
```

**Content codes**: C=consonant, V=vowel, A=alpha, N=numeric, $=national, *=any

## Group Profiles

### ADDGROUP - Create a Group

```
ADDGROUP group
         OWNER(owner)
         SUPGROUP(parent)
         [OMVS(GID(nnn))]
```

### ALTGROUP - Modify a Group

```
ALTGROUP group
         [OWNER(newowner)]
         [SUPGROUP(newparent)]
         [OMVS(GID(nnn))]
```

### DELGROUP - Delete a Group

```
DELGROUP group
```

### LISTGRP - Display a Group Profile

```
LISTGRP group [ALL] [OMVS]
```

### CONNECT - Add a User to a Group

```
CONNECT userid GROUP(group)
        [AUTHORITY(USE | CREATE | CONNECT | JOIN)]
        [OWNER(owner)]
        [SPECIAL | NOSPECIAL]
        [OPERATIONS | NOOPERATIONS]
        [AUDITOR | NOAUDITOR]
```

### REMOVE - Remove a User from a Group

```
REMOVE userid GROUP(group)
```

## Resource Profiles

### RDEFINE - Create a Resource Profile

```
RDEFINE class profile
        UACC(NONE | READ | UPDATE | CONTROL | ALTER)
        [OWNER(owner)]
        [AUDIT(SUCCESS(READ) | FAILURE(READ) | ALL(READ))]
        [WARNING]
        [LEVEL(nn)]
        [DATA('description')]
```

### RALTER - Modify a Resource Profile

```
RALTER class profile
       [UACC(level)]
       [OWNER(newowner)]
       [AUDIT(SUCCESS(READ) | FAILURE(READ))]
       [NOWARNING]
```

### RDELETE - Delete a Resource Profile

```
RDELETE class profile
```

### RLIST - Display a Resource Profile

```
RLIST class profile [ALL] [AUTHUSER]
```

### PERMIT - Grant Access to a Resource

```
PERMIT profile CLASS(class)
       ID(userid | group)
       ACCESS(NONE | READ | UPDATE | CONTROL | ALTER)
       [WHEN(TERMINAL(termid) | CONSOLE(consid) | JESINPUT(node))]
       [DELETE]
```

## Resource Classes

| Class | Protects | Examples |
|-------|----------|----------|
| DATASET | Datasets and PDS members | `SYS1.PARMLIB.**`, `USER01.**` |
| FACILITY | System facilities and functions | `BPX.SUPERUSER`, `IRR.DIGTCERT.LISTRING` |
| PROGRAM | Programs (load modules) | `IKJEFT01`, `IEFBR14` |
| TERMINAL | 3270 terminals | `TCP00001`, `L3270001` |
| SECLABEL | Security labels | `SYSLOW`, `SYSHIGH`, `CONFIDENTIAL` |
| SURROGAT | Surrogate authority | `userid.SUBMIT`, `userid.XMEM` |
| CSFSERV | Cryptographic services | `CSFENC`, `CSFDEC`, `CSFDSV` |
| CSFKEYS | Cryptographic keys | `MASTER.KEY`, `DATA.KEY.01` |
| EJBROLE | Enterprise Java beans | Role-based access for Java EE |
| SERVAUTH | TCP/IP services | `EZB.STACKACCESS.*` |
| APPL | Application resources | `CICS`, `TSO`, `OMVSAPPL` |
| STARTED | Started procedures | `JES2.*`, `VTAM.*`, `TCPIP.*` |

### Dataset Profile Naming

```
ADDSD 'USER01.**' UACC(NONE) OWNER(USER01)   Generic profile
ADDSD 'SYS1.PARMLIB' UACC(READ)              Discrete profile
PERMIT 'USER01.**' ID(USER01) ACCESS(ALTER)
PERMIT 'SYS1.PARMLIB' ID(SYSPROG) ACCESS(UPDATE)
```

**Generic characters**: `*` (single qualifier), `**` (all remaining qualifiers), `%` (single character)

## SAF (System Authorization Facility)

SAF is the z/OS routing layer that directs security requests to the active security product (RACF):

```
Application  -->  SAF Router  -->  RACF  -->  Decision
                     |                         (Allow/Deny)
                     |
                  (or other security product: ACF2, TopSecret)
```

### SAF Call Flow

1. **Resource manager** (CICS, JES2, TSO, etc.) issues a SAF call: `RACROUTE REQUEST=AUTH`
2. **SAF router** examines the request and routes it to RACF
3. **RACF** evaluates user profiles, resource profiles, and access lists
4. **Decision** is returned: allow, deny, or undefined (no profile)
5. **Resource manager** enforces the decision

### SAF Callable Services

| Macro | Purpose |
|-------|---------|
| `RACROUTE REQUEST=AUTH` | Authorization check |
| `RACROUTE REQUEST=VERIFY` | User authentication (password verify) |
| `RACROUTE REQUEST=DEFINE` | Define a resource profile |
| `RACROUTE REQUEST=LIST` | Retrieve profile information |
| `RACROUTE REQUEST=FASTAUTH` | Fast-path authorization (in-storage profiles) |
| `RACROUTE REQUEST=EXTRACT` | Extract data from profiles |

## Security Labels and SECLEVEL

Security labels provide mandatory access control (MAC):

```
RDEFINE SECLABEL CONFIDENTIAL UACC(NONE)
RALTER  SECLABEL CONFIDENTIAL SECLEVEL(SYSHIGH)

ALTUSER USER01 SECLABEL(CONFIDENTIAL)
ALTDSD 'SENSITIVE.DATA.**' SECLABEL(CONFIDENTIAL)
```

Access requires: (1) label dominance AND (2) discretionary access (PERMIT)

## Digital Certificates

### RACDCERT Commands

| Command | Purpose |
|---------|---------|
| `RACDCERT GENCERT` | Generate a certificate and key pair |
| `RACDCERT ADD` | Add an external certificate to RACF |
| `RACDCERT LIST` | List certificates for a user or certauth |
| `RACDCERT DELETE` | Delete a certificate |
| `RACDCERT CONNECT` | Connect a certificate to a key ring |
| `RACDCERT ADDRING` | Create a key ring |
| `RACDCERT LISTRING` | List key ring contents |
| `RACDCERT DELRING` | Delete a key ring |
| `RACDCERT EXPORT` | Export a certificate |
| `RACDCERT CHECKCERT` | Verify certificate chain |

### Certificate Example

```
RACDCERT ID(WEBSERV) GENCERT
    SUBJECTSDN(CN('webserver.example.com')
               OU('IT') O('COMPANY') C('US'))
    SIZE(2048)
    WITHLABEL('WebServerCert')
    NOTBEFORE(DATE(2024-01-01))
    NOTAFTER(DATE(2026-12-31))

RACDCERT ID(WEBSERV) ADDRING(SSLRING)
RACDCERT ID(WEBSERV) CONNECT(LABEL('WebServerCert')
    RING(SSLRING) USAGE(PERSONAL))
```

## SETROPTS Options

SETROPTS controls global RACF behavior:

| Option | Description |
|--------|-------------|
| `SETROPTS CLASSACT(class)` | Activate a resource class |
| `SETROPTS NOCLASSACT(class)` | Deactivate a resource class |
| `SETROPTS RACLIST(class)` | Cache profiles in storage for fast access |
| `SETROPTS NORACLIST(class)` | Remove profiles from cache |
| `SETROPTS GENERIC(class)` | Enable generic profiles for a class |
| `SETROPTS AUDIT(class)` | Enable auditing for a class |
| `SETROPTS STATISTICS(class)` | Enable statistics for a class |
| `SETROPTS PASSWORD(...)` | Set password rules (see Password Rules above) |
| `SETROPTS LOGOPTIONS(...)` | Control logging behavior |
| `SETROPTS REFRESH RACLIST(class)` | Refresh cached profiles (after PERMIT changes) |
| `SETROPTS EGN` | Enable enhanced generic naming |
| `SETROPTS GRPLIST` | Enable group list checking |
| `SETROPTS SECLABELCONTROL` | Enable security label processing |

## PassTickets

PassTickets are one-time-use, time-limited passwords that eliminate the need to transmit real passwords:

```
RDEFINE PTKTDATA APPLIC1
    SSIGNON(KEYMASKED(key-value))
PERMIT APPLIC1 CLASS(PTKTDATA) ID(USER01) ACCESS(UPDATE)
SETROPTS CLASSACT(PTKTDATA)
SETROPTS RACLIST(PTKTDATA)
```

A PassTicket is valid for approximately 10 minutes and can only be used once.

## RACF Utilities

| Utility | Description |
|---------|-------------|
| `IRRUT100` | RACF database search utility; searches for specific profile types and attributes |
| `IRRUT200` | RACF database verification utility; checks internal consistency of the RACF database |
| `IRRUT400` | RACF database mapping utility; maps the RACF database and reports on space usage |
| `IRRDBU00` | RACF database unload utility; unloads RACF profiles to a flat file for reporting |
| `IRRRID00` | RACF remove ID utility; removes a user or group from all profiles |
| `BLKUPD` | Block update utility; low-level database maintenance |

### Example: Unload RACF Database for Reporting

```jcl
//UNLOAD   EXEC PGM=IRRDBU00
//INDD1    DD DISP=SHR,DSN=SYS1.RACF.DATABASE
//OUTDD    DD DISP=(NEW,CATLG),DSN=RACF.UNLOAD,
//            SPACE=(CYL,(50,10)),UNIT=SYSDA,
//            DCB=(RECFM=VB,LRECL=4096)
//SYSPRINT DD SYSOUT=*
```
"#
    .to_string()
}

fn generate_tso() -> String {
    r#"# TSO (Time Sharing Option)

TSO provides interactive command-line access to z/OS. Users log on through a 3270 terminal or TN3270 emulator and execute commands, run programs, and manage datasets.

## TSO Session Lifecycle

1. **LOGON**: User authenticates via RACF; TSO allocates a TSO address space
2. **LOGON PROC**: JCL procedure executes to set up the environment (allocate datasets, start ISPF)
3. **READY prompt**: User enters TSO commands at the `READY` prompt
4. **LOGOFF**: Session terminates, address space is freed

## Common TSO Commands

| Command | Description |
|---------|-------------|
| `ALLOCATE` / `ALLOC` | Allocate a dataset to a DD name |
| `FREE` | Free an allocated DD name |
| `SUBMIT` | Submit JCL for batch execution |
| `STATUS` | Display job status |
| `OUTPUT` | Retrieve job output |
| `CANCEL` | Cancel a running job |
| `LISTDS` | List dataset attributes |
| `LISTCAT` | List catalog entries |
| `RENAME` | Rename a dataset |
| `DELETE` | Delete a dataset |
| `SEND` | Send a message to another user |
| `RECEIVE` | Receive data sent via TRANSMIT |
| `TRANSMIT` / `XMIT` | Transmit a dataset to another user/node |
| `EXEC` | Execute a REXX or CLIST script |
| `CALL` | Call a load module |
| `HELP` | Display help for a command |
| `PROFILE` | Set/display terminal profile settings |
| `TIME` | Display current time and resource usage |

## ALLOCATE Command

```
ALLOC FI(ddname) DA('dataset.name') SHR
ALLOC FI(SYSUT1) DA('MY.DATA') OLD
ALLOC FI(SYSOUT) SYSOUT(A)
ALLOC FI(TEMP) NEW SPACE(5,1) TRACKS LRECL(80) RECFM(F,B) BLKSIZE(3120)
FREE FI(ddname)
```

## REXX and CLIST

TSO supports two scripting languages:
- **REXX**: Preferred modern scripting; `EXEC script EXEC` or `%script`
- **CLIST**: Older command list language; `EXEC script`

Both can issue TSO commands, perform I/O, and interact with the user through the terminal.
"#
    .to_string()
}

fn generate_ispf() -> String {
    r#"# ISPF (Interactive System Productivity Facility)

ISPF provides a panel-driven interface for z/OS, layered on top of TSO. It includes an editor, browse utility, dataset management, and a dialog manager for building custom applications.

## ISPF Primary Option Menu

| Option | Name | Description |
|--------|------|-------------|
| 0 | Settings | Terminal, log, and list settings |
| 1 | View | Browse datasets without edit capability |
| 2 | Edit | Edit datasets and PDS members |
| 3 | Utilities | Dataset utilities (allocate, rename, copy, etc.) |
| 4 | Foreground | Compile, assemble, or link-edit in foreground |
| 5 | Batch | Submit JCL for batch processing |
| 6 | Command | Enter TSO/ISPF commands |
| 7 | Dialog Test | Test ISPF dialogs (panels, skeletons) |
| 10 | SCLM | Software Configuration and Library Manager |
| 11 | Workplace | Object/Action workplace |
| SD | SDSF | System Display and Search Facility |

## ISPF Editor Commands

### Primary Commands

| Command | Description |
|---------|-------------|
| `SAVE` | Save the current file |
| `CANCEL` / `CAN` | Exit without saving |
| `FIND string` | Find a string |
| `CHANGE s1 s2` | Change string s1 to s2 |
| `SUBMIT` / `SUB` | Submit the file as JCL |
| `COPY` | Copy data from another dataset |
| `MOVE` | Move data from another dataset |
| `CREATE` | Create a new dataset from selected lines |
| `REPLACE` | Replace a member with selected lines |
| `SORT` | Sort selected lines |
| `RESET` | Reset all line commands and messages |
| `PROFILE` | Display edit profile settings |
| `COLS` | Display column ruler |
| `BOUNDS` | Set edit boundaries |
| `NUMBER ON/OFF` | Toggle sequence numbering |
| `HEX ON/OFF` | Toggle hex display mode |
| `CAPS ON/OFF` | Toggle uppercase translation |
| `UNDO` | Undo the last change |

### Line Commands

| Command | Description |
|---------|-------------|
| `I` / `In` | Insert 1 or n blank lines |
| `D` / `Dn` / `DD` | Delete 1, n, or block of lines |
| `C` / `Cn` / `CC` | Copy 1, n, or block of lines |
| `M` / `Mn` / `MM` | Move 1, n, or block of lines |
| `R` / `Rn` / `RR` | Repeat 1, n, or block of lines |
| `A` | After (target for copy/move) |
| `B` | Before (target for copy/move) |
| `O` / `OO` | Overlay target |
| `X` / `Xn` / `XX` | Exclude lines from display |
| `S` / `Sn` / `SS` | Show excluded lines |
| `>` / `>n` | Shift right |
| `<` / `<n` | Shift left |
| `TS` | Text split at cursor |
| `TF` | Text flow |

## ISPF Dialog Manager Services

| Service | Description |
|---------|-------------|
| `DISPLAY` | Display a panel |
| `TBCREATE` | Create a table |
| `TBADD` | Add a row to a table |
| `TBGET` | Retrieve a row from a table |
| `TBPUT` | Update a row in a table |
| `TBDELETE` | Delete a row from a table |
| `TBSORT` | Sort a table |
| `TBOPEN` / `TBCLOSE` | Open/close a table |
| `SELECT` | Select a function (panel, program, or command) |
| `SETMSG` | Set a message for display |
| `VGET` / `VPUT` | Get/put shared or profile variables |
| `FTOPEN` / `FTCLOSE` | Open/close file tailoring |
| `FTINCL` | Include a skeleton in file tailoring |
| `BROWSE` | Browse a dataset |
| `EDIT` | Edit a dataset |
| `LMINIT` / `LMFREE` | Initialize/free a library access service |
"#
    .to_string()
}

fn generate_mq() -> String {
    r#"# IBM MQ (Message Queuing)

IBM MQ provides reliable, asynchronous message queuing between applications. On z/OS it runs as a subsystem and integrates with CICS, IMS, and batch programs.

## Core Concepts

| Concept | Description |
|---------|-------------|
| **Queue Manager** | The MQ subsystem that manages queues and connections |
| **Queue** | A named destination for messages (local, remote, alias, model) |
| **Message** | A unit of data placed on a queue |
| **Channel** | A communication link between queue managers |
| **Topic** | Publish/subscribe destination |

## Queue Types

| Type | Description |
|------|-------------|
| LOCAL | Messages stored on this queue manager |
| REMOTE | Resolves to a queue on another queue manager |
| ALIAS | An alternative name for a local or remote queue |
| MODEL | Template for creating dynamic queues |
| TRANSMISSION | Holds messages destined for a remote queue manager |
| DEAD-LETTER | Receives messages that cannot be delivered |

## MQI (Message Queue Interface)

| Call | Description |
|------|-------------|
| `MQCONN` / `MQCONNX` | Connect to a queue manager |
| `MQDISC` | Disconnect from a queue manager |
| `MQOPEN` | Open a queue or topic |
| `MQCLOSE` | Close a queue or topic |
| `MQPUT` | Put a message on a queue |
| `MQPUT1` | Open, put one message, and close |
| `MQGET` | Get a message from a queue |
| `MQINQ` | Inquire about queue attributes |
| `MQSET` | Set queue attributes |
| `MQSUB` | Subscribe to a topic |
| `MQCMIT` | Commit the current unit of work |
| `MQBACK` | Back out the current unit of work |

## MQSC Commands

| Command | Description |
|---------|-------------|
| `DEFINE QLOCAL(name)` | Define a local queue |
| `DEFINE QREMOTE(name)` | Define a remote queue |
| `DEFINE CHANNEL(name)` | Define a channel |
| `ALTER QLOCAL(name)` | Alter a queue |
| `DELETE QLOCAL(name)` | Delete a queue |
| `DISPLAY QLOCAL(name)` | Display queue attributes |
| `DISPLAY QSTATUS(name)` | Display queue status |
| `DISPLAY CHSTATUS(name)` | Display channel status |
| `START CHANNEL(name)` | Start a channel |
| `STOP CHANNEL(name)` | Stop a channel |
| `DISPLAY CONN(*)` | Display all connections |
"#
    .to_string()
}

fn generate_mvs() -> String {
    r#"# MVS (Multiple Virtual Storage)

MVS provides the core operating system services for z/OS. All other subsystems depend on MVS for storage management, task dispatching, I/O, and system services.

## Key MVS Services

### SVCs (Supervisor Calls)

SVCs provide the interface between problem programs and the operating system:

| SVC | Name | Description |
|-----|------|-------------|
| 0 | EXCP | Execute Channel Program (I/O) |
| 1 | WAIT / WAITR | Wait on an ECB |
| 2 | POST | Post an ECB |
| 3 | EXIT | Task termination |
| 4 | GETMAIN | Obtain virtual storage |
| 5 | FREEMAIN | Release virtual storage |
| 6 | LINK | Load and pass control to a program |
| 7 | XCTL | Transfer control, release caller |
| 8 | LOAD | Load a program module |
| 9 | DELETE | Delete a loaded module |
| 10 | GETMAIN (R) | Obtain storage (Register form) |
| 11 | TIME | Obtain date/time |
| 13 | ABEND | Abnormal end |
| 19 | OPEN | Open a dataset |
| 20 | CLOSE | Close a dataset |
| 34 | MGCR | Issue operator command |
| 35 | WTO | Write to Operator |
| 36 | WTL | Write to Log |
| 42 | ATTACH | Create a subtask |
| 47 | STIMER | Set timer |
| 51 | ENQ | Enqueue on a resource |
| 52 | DEQ | Dequeue a resource |
| 56 | GETMAIN (VRU) | Variable storage obtain |
| 99 | DYNALLOC | Dynamic allocation |
| 109 | ALESERV | Access List Entry services |

### DYNALLOC (Dynamic Allocation)

DYNALLOC (SVC 99) allocates and deallocates datasets without JCL:

```
Verb codes:
  01 - Allocate (DSNAME allocation)
  02 - Unallocate
  03 - Concatenation
  04 - Deconcatenation
  05 - Remove in-use
  06 - DDname allocation
  07 - Information retrieval
```

### WTO (Write to Operator)

```hlasm
         WTO   'MYAPP001I INITIALIZATION COMPLETE',                    X
               ROUTCDE=(2,11),DESC=4
```

Message conventions: `XXXXnnnT` where XXXX=component, nnn=number, T=type (I/W/E/A)

### ENQ/DEQ (Resource Serialization)

```hlasm
         ENQ   (QNAME,RNAME,E,LEN,SYSTEM)    Shared enqueue
         ENQ   (QNAME,RNAME,X,LEN,SYSTEM)    Exclusive enqueue
         DEQ   (QNAME,RNAME,LEN,SYSTEM)      Release
```

Scope: STEP (task), SYSTEM (system), SYSTEMS (sysplex)

### ESTAE (Extended Specify Task Abnormal Exit)

ESTAE establishes a recovery routine for abend processing:

```hlasm
         ESTAE EXIT_RTN,CT,PARAM=WORK_AREA
```

The recovery routine can:
- Retry the failing instruction
- Percolate (let the next recovery routine handle it)
- Record diagnostic information (SDWA)

## Program Management

| Service | Description |
|---------|-------------|
| LINK | Load and branch to a program module |
| XCTL | Transfer control (release current module) |
| LOAD | Load a module into storage |
| DELETE | Remove a loaded module |
| ATTACH | Create a new task running a program |
| IDENTIFY | Add an entry point to the current job pack area |

## System Macros

| Macro | Description |
|-------|-------------|
| OPEN | Open a DCB/ACB |
| CLOSE | Close a DCB/ACB |
| GET | Read a record (QSAM) |
| PUT | Write a record (QSAM) |
| READ | Read a block (BSAM) |
| WRITE | Write a block (BSAM) |
| CHECK | Check I/O completion |
| SNAP | Dump storage areas |
| ABEND | Abnormal termination |
| STIMER | Set interval timer |
| TTIMER | Test/cancel timer |
"#
    .to_string()
}

fn generate_uss() -> String {
    r#"# USS (Unix System Services)

Unix System Services provides a POSIX-compliant Unix environment within z/OS. It includes a hierarchical file system (HFS/zFS), shell access, and standard Unix utilities.

## Key Components

| Component | Description |
|-----------|-------------|
| **Kernel** | z/OS Unix kernel (BPX* modules) providing POSIX syscalls |
| **Shell** | `/bin/sh` (z/OS UNIX shell) or alternatives like bash |
| **HFS/zFS** | Hierarchical file systems mounted in a single namespace |
| **OMVS** | TSO command to enter the Unix shell environment |
| **BPXBATCH** | Batch program for running Unix commands in JCL |

## Entering USS

From TSO:
```
OMVS
```

From JCL:
```jcl
//STEP1    EXEC PGM=BPXBATCH
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDPARM  DD *
SH ls -la /u/user01
/*
```

## File System

USS uses a hierarchical file system rooted at `/`:

| Mount Point | Description |
|-------------|-------------|
| `/` | Root file system (zFS) |
| `/bin` | Standard Unix commands |
| `/usr` | User programs, libraries, documentation |
| `/etc` | System configuration files |
| `/tmp` | Temporary files |
| `/dev` | Device special files |
| `/u` | User home directories |
| `/var` | Variable data (logs, spool) |

### Security

USS file permissions are mapped through RACF:
- UID/GID assigned via `OMVS` segment in RACF user/group profiles
- File permission bits (rwx) enforced by the kernel
- Superuser (UID 0) or `BPX.SUPERUSER` FACILITY class profile

## Common Configuration

Key OMVS parameters in BPXPRMxx PARMLIB member:

| Parameter | Description |
|-----------|-------------|
| `MAXPROCSYS` | Maximum processes system-wide |
| `MAXPROCUSER` | Maximum processes per user |
| `MAXFILEPROC` | Maximum open files per process |
| `MAXTHREADS` | Maximum threads per process |
| `MAXASSIZE` | Maximum address space size |
| `ROOT` | Root file system definition |
| `MOUNT` | File system mount entries |
"#
    .to_string()
}

fn generate_wlm() -> String {
    r#"# WLM (Workload Manager)

WLM manages system resources to meet performance goals defined by the installation. It dynamically adjusts dispatching priorities, storage allocation, and I/O priorities to achieve service level objectives.

## Key Concepts

| Concept | Description |
|---------|-------------|
| **Service Class** | A grouping of work with a common performance goal |
| **Service Goal** | Response time, velocity, or discretionary target |
| **Classification Rule** | Maps incoming work to service classes |
| **Service Policy** | Named collection of goals (only one active at a time) |
| **Resource Group** | Limits CPU or storage for a group of service classes |
| **Report Class** | Groups work for measurement/reporting (no goals) |
| **Workload** | Logical grouping of service classes for reporting |

## Service Goal Types

| Type | Description |
|------|-------------|
| **Response Time** | Percentage of transactions completing within a target time |
| **Velocity** | Speed at which work moves through the system (0-100) |
| **Discretionary** | Best-effort, lowest priority work |

## Classification Rules

Work is classified by subsystem type and qualifiers:

| Subsystem | Qualifiers |
|-----------|------------|
| JES | Job class, job name, account, user ID |
| CICS | Transaction ID, transaction class, user ID |
| TSO | User ID, procedure name |
| DB2 | Plan name, package, connection type |
| MQ | Queue manager, queue name |
| USS | User ID, process name |

## WLM Operator Commands

| Command | Description |
|---------|-------------|
| `D WLM,SCHENV=name` | Display scheduling environment |
| `D WLM,APPLENV=name` | Display application environment |
| `V WLM,POLICY=name,REFRESH` | Activate a service policy |
| `D WLM,RESOURCE=*` | Display resource groups |
| `F WLM,RESOURCE=name,RESET` | Reset a resource group |
"#
    .to_string()
}

fn generate_smf() -> String {
    r#"# SMF (System Management Facilities)

SMF collects and records system activity data for accounting, performance analysis, security auditing, and capacity planning.

## SMF Records

SMF records are identified by type number (0-255) and optionally by subtype:

### Common Record Types

| Type | Subtype | Description |
|------|---------|-------------|
| 0 | | IPL record |
| 4 | | Step termination |
| 5 | | Job termination |
| 6 | | External writer |
| 14 | | INPUT dataset activity |
| 15 | | OUTPUT dataset activity |
| 17 | | Scratch dataset |
| 18 | | Rename dataset |
| 21 | | Error statistics by volume |
| 22 | | Configuration |
| 23 | | SMF status |
| 26 | | JES2 job purge |
| 30 | | Common address space work |
| 42 | | SMS statistics |
| 62 | | VSAM component open |
| 64 | | VSAM component status |
| 70 | | RMF CPU activity |
| 71 | | RMF paging activity |
| 72 | | RMF workload activity |
| 73 | | RMF channel activity |
| 74 | | RMF device activity |
| 75 | | RMF page/swap dataset |
| 76 | | RMF trace |
| 77 | | RMF enqueue activity |
| 78 | | RMF I/O queuing |
| 79 | | RMF monitor II |
| 80 | | RACF processing |
| 81 | | RACF initialization |
| 82 | | RACF access |
| 83 | | RACF audit |
| 89 | | RACF RRSF |
| 92 | | File system activity |
| 100 | | DB2 statistics |
| 101 | | DB2 accounting |
| 102 | | DB2 performance |
| 110 | | CICS transaction data |
| 120-122 | | WebSphere MQ |

## SMF Configuration

SMF is configured via the SMFPRMxx PARMLIB member:

```
SYS(TYPE(0,4,5,14,15,30,70:79,80:83,100:102,110))
NOTYPE(6,21,22)
INTERVAL(SMF,001500)
RECORDING(PERM)
MAXDORM(3000)
BUFUSAGE(SYSTEM)
STATUS(010000)
JWT(0600)
SID(SYS1)
```

## SMF Data Collection

| Component | Description |
|-----------|-------------|
| SMF writer | Writes records to SYS1.MANx datasets |
| SMF buffer | In-storage buffer before writing |
| SYS1.MANx | SMF recording datasets (switched when full) |
| IFASMFDP | SMF dump utility (copies records for analysis) |
| IFASMFDL | SMF log stream dump |

## SMF Operator Commands

| Command | Description |
|---------|-------------|
| `D SMF` | Display SMF status |
| `D SMF,O` | Display active SMF options |
| `I SMF` | Switch to next MANx dataset |
| `T SMF=xx` | Change SMF options to member SMFPRMxx |
| `SET SMF=xx` | Apply new SMF parameters |
"#
    .to_string()
}

fn generate_networking() -> String {
    r#"# Networking (VTAM / TCP/IP / SNA)

z/OS networking encompasses both traditional SNA (Systems Network Architecture) and modern TCP/IP communications.

## VTAM (Virtual Telecommunications Access Method)

VTAM manages SNA network resources and provides the API for application-to-terminal and application-to-application communication.

### Key Concepts

| Concept | Description |
|---------|-------------|
| **SSCP** | System Services Control Point (VTAM node identity) |
| **PU** | Physical Unit (hardware node) |
| **LU** | Logical Unit (application or terminal endpoint) |
| **Session** | Active connection between two LUs |
| **APPLID** | Application ID (e.g., CICS, IMS) |
| **Logmode** | Session parameters defining screen size, protocols |

### VTAM Commands

| Command | Description |
|---------|-------------|
| `D NET,ID=name` | Display resource status |
| `V NET,ACT,ID=name` | Activate a resource |
| `V NET,INACT,ID=name` | Deactivate a resource |
| `D NET,SESSIONS` | Display active sessions |
| `V NET,LOGON,ID=appl` | Force logon to application |
| `D NET,APPLS` | Display application nodes |
| `D NET,BFRUSE` | Display buffer pool usage |

## TCP/IP on z/OS

z/OS TCP/IP provides standard Internet protocols:

### Stack Components

| Component | Description |
|-----------|-------------|
| **TCPIP** | The TCP/IP stack address space |
| **Resolver** | DNS name resolution |
| **INETD** | Internet superserver daemon |
| **FTP** | File Transfer Protocol server |
| **TN3270** | Telnet 3270 server (for terminal access) |
| **SSH** | Secure Shell (OpenSSH for z/OS) |
| **HTTP** | Web server (IBM HTTP Server, z/OS Connect) |

### Configuration Files

| File | Description |
|------|-------------|
| `PROFILE.TCPIP` | Main TCP/IP stack configuration |
| `TCPIP.DATA` | Client TCP/IP settings (resolver, hostname) |
| `ETC.SERVICES` | Port to service name mapping |
| `ETC.HOSTS` | Static hostname resolution |

### TN3270

TN3270 provides 3270 terminal access over TCP/IP:

```
TELNETPARMS
  PORT 23
  SECUREPORT 992
  TIMEMARK 600
  SCANINTERVAL 120
  LUGROUP LU3270GP
ENDTELNETPARMS
```

## FTP on z/OS

z/OS FTP supports both MVS datasets and USS files:

| Command | Description |
|---------|-------------|
| `SITE FILETYPE=SEQ` | Transfer sequential datasets |
| `SITE FILETYPE=JES` | Submit/retrieve JES jobs |
| `SITE RECFM=FB LRECL=80` | Set dataset attributes |
| `GET 'MY.DATASET' local` | Download dataset |
| `PUT local 'MY.DATASET'` | Upload dataset |
| `QUOTE SITE FILETYPE=JES` | Switch to JES mode |
| `PUT myjob.jcl` | Submit JCL via FTP |
| `GET JOBnnnnn.x` | Retrieve job output |
"#
    .to_string()
}
