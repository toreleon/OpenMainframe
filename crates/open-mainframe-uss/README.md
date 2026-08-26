# open-mainframe-uss

A high-performance Rust implementation of **z/OS UNIX System Services (USS)** for the OpenMainframe project — providing a complete POSIX-compliant environment: hierarchical file system (zFS), process and thread management (pthreads), signals, IPC primitives, BSD sockets, a full-featured UNIX shell, and the BPXWDYN text-based dynamic allocation interface.

## Purpose

USS (formerly OpenEdition) provides a standard POSIX runtime on IBM z/OS mainframes, allowing UNIX applications, scripts, and network services to execute alongside traditional MVS batch and OLTP workloads. `open-mainframe-uss` models this subsystem within OpenMainframe:
1. **zFS Virtual File System**: Implements a hierarchical file system with inode metadata, permission checks (UID/GID mode bits), file locks, directory trees, and an MVS dataset bridge (`//'DSN'` syntax).
2. **Process & Thread Management**: Emulates POSIX processes (`fork`, `exec`, `spawn`, `waitpid`), asynchronous signal delivery (`sigaction`, `sigprocmask`), and multi-threading (`PThread`, `PthreadMutex`, `PthreadCond`).
3. **IPC & Sockets**: Provides inter-process communication mechanisms (pipes, FIFOs, message queues, shared memory, semaphores) and a BSD socket networking layer (TCP/UDP, UNIX domain sockets).
4. **UNIX Shell & Utilities**: Provides a POSIX-compliant shell interpreter with pipeline parsing, I/O redirection, variable expansions, and built-in utilities (`grep`, `sed`, `awk`, `find`, `wc`, `sort`).
5. **BPXWDYN Dynamic Allocation**: Implements the string-based dynamic allocation interface bridging USS shell scripts and programs to MVS datasets.

## Capabilities

- **zFS Hierarchical File System (`Zfs`, `Inode`, `OpenFlags`)**:
  - Inode management with POSIX file types (Regular, Directory, Symlink, FIFO, Socket, Device).
  - Fine-grained file operations: `open`, `close`, `read`, `write`, `lseek`, `mkdir`, `create_file`.
  - Advisory file locking (`FileLock`, `LockType::ReadLock`, `LockType::WriteLock`).
  - MVS dataset path resolution (`MvsDatasetRef`: `//'DSN.NAME'`).
- **POSIX Process & Thread Model (`ProcessManager`, `ThreadManager`)**:
  - Process lifecycle management (`UnixProcess`, `ProcessState`, `PID` tracking).
  - POSIX signals (`Signal`, `SignalAction`, `SignalSet`, `sigaction`, `kill`).
  - Pthreads synchronization (`PthreadMutex`, `PthreadCond`, `PthreadRwLock`).
- **Inter-Process Communication (`IpcRegistry`)**:
  - Anonymous `Pipe`, named `Fifo`, message queues (`MessageQueue`), shared memory (`SharedMemory`), and counting `Semaphore` objects.
- **POSIX Network Sockets (`SocketManager`, `Socket`)**:
  - Address families (`AddressFamily::Inet`, `AddressFamily::Unix`) and types (`SocketType::Stream`, `SocketType::Datagram`).
  - Non-blocking socket state transitions, connection handshake, and polling.
- **UNIX Shell Interpreter (`Shell`, `tokenize`, `parse_pipeline`)**:
  - Parses pipelines, background jobs (`&`), logical connectors (`&&`, `||`, `;`), and I/O redirections (`>`, `>>`, `<`, `2>&1`).
  - Variable expansion: `$VAR`, `${VAR:-default}`, and command substitution placeholder parsing.
  - Shell builtins: `cd`, `export`, `source`, `exit`, `umask`, `echo`, `pwd`, `jobs`, `fg`, `bg`.
- **Core UNIX Utilities (`utilities`)**:
  - Built-in utility functions: `grep`, `sed`, `awk`, `find`, `wc`, `head`, `tail`, `sort_lines`.
- **BPXWDYN Dynamic Allocation (`BpxwdynManager`, `AllocRequest`)**:
  - Parses and processes text allocation strings: `alloc fi(ddname) da(dsname) shr`.
- **System Daemons & Memory Mapping**:
  - Emulates `cron`, `syslogd`, `inetd`, and memory-mapped files (`mmap`, `munmap`, `mprotect`).

## Architecture

```text
    UNIX Application / Shell
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  USS System Call Interface             │
    │  - POSIX API (open, read, write, socket, fork)         │
    │  - Shell Interpreter (`Shell`, `tokenize`)             │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │               Process & Thread Manager                 │
    │  - Process tracking (`ProcessManager`, `UnixProcess`)  │
    │  - Pthreads (`ThreadManager`, `PthreadMutex`)          │
    │  - Signal delivery (`SignalSet`, `sigaction`)          │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  zFS / VFS Layer                       │
    │  - Hierarchical Inodes (`Zfs`, `Inode`)                │
    │  - Permission & Ownership (UID/GID mode bits)          │
    │  - MVS Dataset Bridge (`MvsDatasetRef`)                │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Subsystem Connectors                   │
    │  - BPXWDYN (`BpxwdynManager`: MVS SVC 99 text bridge)  │
    │  - IPC Registry (`Pipe`, `MessageQueue`, `SharedMemory`)│
    │  - BSD Socket Layer (`SocketManager`, `Socket`)        │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `zfs` | Hierarchical file system: `Zfs`, `Inode`, `FileType`, `OpenFlags`, `FileLock`, `MvsDatasetRef`. |
| `shell` | UNIX shell: `Shell`, `Token`, `tokenize`, `parse_pipeline`, `expand_variables`, `BuiltinCommand`. |
| `process` | Process management: `ProcessManager`, `UnixProcess`, `ProcessState`, `ForkResult`, `WaitStatus`. |
| `threads` | Pthreads: `ThreadManager`, `PThread`, `PthreadMutex`, `PthreadCond`, `PthreadRwLock`. |
| `signal` | Signal handling: `Signal`, `SignalAction`, `SignalSet`, `SignalState`, `DefaultAction`. |
| `ipc` | IPC mechanisms: `IpcRegistry`, `Pipe`, `Fifo`, `MessageQueue`, `SharedMemory`, `Semaphore`. |
| `socket` | Network sockets: `SocketManager`, `Socket`, `SocketAddress`, `AddressFamily`, `SocketType`. |
| `bpxwdyn` | BPXWDYN: `BpxwdynManager`, `AllocRequest`, string-based dynamic allocation. |
| `utilities`| Core UNIX utilities: `grep`, `sed`, `awk`, `find`, `wc`, `head`, `tail`, `sort_lines`. |
| `config` | PARMLIB configuration: `BpxPrmConfig`, `parse_bpxprm`, RACF OMVS segment mapping. |
| `directory`| Metadata services: `DirectoryManager`, `DirHandle`, `FileMetadata`. |
| `daemons` | System daemons: `cron`, `syslogd`, `inetd`, `iconv` codepage auto-conversion. |
| `mmap` | Memory management: `MmapManager`, `MemoryMapping`, `MapFlags`, `ProtFlags`. |

## Public API

### Core Types and Services

```rust
use open_mainframe_uss::{
    zfs::{Zfs, Inode, FileType, OpenFlags, SeekWhence},
    process::{ProcessManager, UnixProcess, ProcessState},
    threads::{ThreadManager, PThread, PthreadMutex, PthreadCond},
    signal::{Signal, SignalAction, SignalSet},
    ipc::{IpcRegistry, Pipe, MessageQueue, SharedMemory},
    socket::{SocketManager, SocketAddress, AddressFamily, SocketType},
    shell::{Shell, tokenize, parse_pipeline, expand_variables},
    bpxwdyn::{BpxwdynManager, AllocRequest},
    config::{BpxPrmConfig, parse_bpxprm},
};
```

- `Zfs`: Central virtual file system instance managing files, directories, descriptors, and permissions.
- `ProcessManager`: Coordinator for POSIX process lifecycle, signal dispatching, and wait states.
- `Shell`: Interactive and programmatic UNIX shell interpreter.
- `BpxwdynManager`: Evaluates BPXWDYN allocation requests.

## Integration

### Workspace Dependencies

- None (pure Rust library using standard workspace crates: `thiserror`, `miette`).

### Known Consumers

- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — Used by z/OSMF UNIX file and command execution endpoints.
- Interactive developer shells and script runners within the OpenMainframe ecosystem.

## Examples

### Programmatic File Access on zFS

```rust
use open_mainframe_uss::zfs::{Zfs, OpenFlags};

// Initialize file system with 1024 descriptor capacity
let mut fs = Zfs::new(1024);

// Create and open file with write/create flags
let fd = fs.open("/tmp/sample.txt", OpenFlags::write_create(), 0o644, 0, 0).unwrap();
fs.write(fd, b"Hello from z/OS UNIX System Services!\n").unwrap();
fs.close(fd).unwrap();

// Reopen and read content
let r_fd = fs.open("/tmp/sample.txt", OpenFlags::read_only(), 0o644, 0, 0).unwrap();
let content = fs.read(r_fd, 100).unwrap();
assert_eq!(content, b"Hello from z/OS UNIX System Services!\n");
fs.close(r_fd).unwrap();
```

### Parsing Shell Pipelines and Variable Expansions

```rust
use open_mainframe_uss::shell::{tokenize, parse_pipeline, expand_variables};
use std::collections::HashMap;

// 1. Tokenize and parse pipeline
let tokens = tokenize("cat /etc/passwd | grep root > /tmp/root.txt").unwrap();
let pipeline = parse_pipeline(&tokens).unwrap();

assert_eq!(pipeline.commands.len(), 2);
assert_eq!(pipeline.commands[0].name, "cat");
assert_eq!(pipeline.commands[1].name, "grep");

// 2. Variable expansion
let mut env = HashMap::new();
env.insert("USER".to_string(), "IBMUSER".to_string());
let expanded = expand_variables("Current user: ${USER:-unknown}", &env);
assert_eq!(expanded, "Current user: IBMUSER");
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-uss
```

The test suite covers:
- **`zfs::*`**: Inode allocation, hierarchical path resolution, open/read/write/lseek lifecycle, permission bit verification (owner/group/other), hard/symbolic links, and advisory locking.
- **`shell::*`**: Tokenizer rules, quoting (single and double quotes, escapes), pipeline construction, variable parameter expansion, builtin command execution, and job control state transitions.
- **`process::*`**: Process state machine, fork descriptor cloning, environment inheritance, and signal delivery.
- **`threads::*`**: Mutex locking/unlocking, condition variable wait/signal, and thread lifecycle.
- **`socket::*`**: Socket state transitions, TCP stream send/receive emulation, and address parsing.
- **`utilities::*`**: Regex pattern matching in `grep`, stream editing in `sed`, column extraction in `awk`, file searching in `find`, word counting in `wc`, and line sorting in `sort_lines`.
- **`bpxwdyn::*`**: Keyword parsing (`alloc`, `free`, `fi`, `da`, `shr`, `mod`) and option mapping.

## Limitations

- **Physical Storage**: In-memory VFS and snapshot serialization rather than physical zFS linear dataset disk partitions.
- **MVS Dubbing**: Address space dubbing coordinates via software state rather than kernel address space control blocks.
- **Character Codepages**: Automatic ASCII/EBCDIC auto-conversion on file read/write is supported when files are explicitly tagged.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [MVS System Services (`open-mainframe-mvs`)](../open-mainframe-mvs/README.md)
- [RACF Security Subsystem (`open-mainframe-racf`)](../open-mainframe-racf/README.md)
- [z/OSMF REST Subsystem (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
