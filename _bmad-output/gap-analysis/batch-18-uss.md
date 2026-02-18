# Gap Analysis: z/OS UNIX System Services (USS)

## Official Specification Summary

z/OS UNIX System Services (USS, also known as z/OS UNIX or OMVS) is a base element of z/OS that provides a POSIX-compliant UNIX environment on IBM mainframes. First introduced as OpenEdition MVS in MVS/ESA SP 4.3, USS was certified as XPG4 UNIX 95 — the first UNIX 95 implementation not derived from AT&T source code. USS enables UNIX applications (Java, Python, Perl, Node.js, Go) to run on z/OS alongside traditional MVS workloads.

USS is classified as **Core** — virtually every modern z/OS installation relies on USS:
- **POSIX compliance**: Certified UNIX 95 (XPG4); supports POSIX.1 (system interface), POSIX.2 (shell & utilities), Single UNIX Specification
- **Process model**: fork(), exec(), spawn() (BPX1SPN), wait(), kill(), signal handling, process groups, sessions
- **File system**: zFS (z/OS File System, strategic) and HFS (Hierarchical File System, deprecated); mounted into a single UNIX hierarchy with /, /usr, /bin, /etc, /tmp, /var, /dev
- **Shell**: /bin/sh (z/OS shell), tcsh, bash (via Rocket Software), BPXBATCH (JCL-to-USS bridge)
- **Pthreads**: Full POSIX threads — pthread_create, mutexes, condition variables, read-write locks, thread-specific data, pthread_atfork
- **Sockets**: AF_INET (IPv4), AF_INET6 (IPv6), AF_UNIX (local), socket/bind/listen/accept/connect/send/recv, select/poll, getaddrinfo
- **BPX callable services**: 200+ assembler-callable services (BPX1xxx for 31-bit, BPX4xxx for 64-bit AMODE) covering open/read/write/close/stat/fork/exec/spawn/wait/kill/socket/etc.
- **BPXWDYN**: Dynamic MVS dataset allocation from USS programs
- **Security**: RACF OMVS segment (UID/GID), UNIXPRIV class (granular superuser privileges), BPX.SUPERUSER, BPX.DAEMON, file permission bits, ACLs, setuid/setgid
- **IPC**: System V shared memory (shmget/shmat/shmdt/shmctl), semaphores (semget/semop/semctl), message queues (msgget/msgsnd/msgrcv/msgctl), pipes, named pipes (FIFO)
- **Memory-mapped files**: mmap/munmap/mprotect/msync
- **USS-MVS integration**: //'dataset.name' syntax, FILEDATA tags (TEXT/BINARY), chtag/iconv for code page conversion, enhanced ASCII support
- **Daemons**: inetd, cron, syslogd, _BPX_JOBNAME, _BPX_SHAREAS environment variables

Key documentation:
- **z/OS UNIX System Services Planning** (GA32-0884) — architecture, configuration, BPXPRMxx
- **z/OS UNIX System Services User's Guide** (SA23-2279) — shell, utilities, file system
- **z/OS UNIX System Services Programming: Assembler Callable Services Reference** (SA23-2281) — BPX1xxx/BPX4xxx services
- **z/OS UNIX System Services Command Reference** (SA23-2280) — shell commands and utilities

## Key Features & Capabilities

### 1. POSIX Compliance & Standards

| Standard | Status | Description |
|----------|--------|-------------|
| POSIX.1 (IEEE 1003.1) | Certified | System interfaces — process control, signals, file operations, IPC |
| POSIX.2 (IEEE 1003.2) | Certified | Shell and utilities — sh, awk, sed, grep, make |
| XPG4 | Certified | X/Open Portability Guide 4 |
| UNIX 95 | Certified | The Open Group UNIX 95 brand (first non-AT&T certified) |
| Single UNIX Specification | Supported | Extended APIs beyond POSIX base |
| ANSI C | Supported | C runtime via LE (Language Environment) |

### 2. Process Model

| Feature | Description |
|---------|-------------|
| fork() / BPX1FRK | Create child process (copy-on-write semantics) |
| exec() / BPX1EXC | Replace process image with new program |
| spawn() / BPX1SPN | Create child and exec in one call (preferred on z/OS — avoids fork overhead) |
| wait() / BPX1WAT | Wait for child process termination |
| waitpid() | Wait for specific child with options (WNOHANG, WUNTRACED) |
| kill() / BPX1KIL | Send signal to process or process group |
| getpid() / BPX1GPI | Get process ID |
| getppid() / BPX1GPP | Get parent process ID |
| setpgid() / BPX1SPG | Set process group ID |
| setsid() / BPX1SSD | Create new session |
| Signal handling | sigaction(), sigprocmask(), sigsuspend(), sigwait() — full POSIX signals |
| Process limits | MAXPROCSYS, MAXPROCUSER (BPXPRMxx tunables) |
| BPXAS | WLM-managed auxiliary address spaces for fork processing |
| Zombie handling | Parent must wait()/waitpid() or child becomes zombie |
| _BPX_SHAREAS | Control whether spawn child shares parent's address space |

### 3. File System

| Feature | Description |
|---------|-------------|
| zFS (z/OS File System) | Strategic file system — log-structured, journaled, VSAM linear dataset |
| HFS (Hierarchical File System) | Legacy file system (deprecated, migration to zFS recommended) |
| TFS (Temporary File System) | In-memory file system for /tmp |
| AUTOMNT | Automatic mount file system type |
| NFS client | Network File System client for remote file access |
| Root file system (/) | Version root (OMVS.ROOT or similar) |
| Standard directories | /bin, /usr, /etc, /tmp, /var, /dev, /opt, /home |
| Mount/unmount | MOUNT/UNMOUNT commands and BPX1MNT/BPX1UMN services |
| Symbolic links | symlink() / BPX1SYM |
| Hard links | link() / BPX1LNK |
| FIFO (named pipes) | mkfifo() / BPX1MKN |
| Character special files | /dev/null, /dev/random, /dev/urandom, /dev/console |
| chmod/chown | Change file permissions and ownership |
| File permission bits | rwx for user/group/other (standard UNIX 9-bit model) |
| ACLs (Access Control Lists) | Extended ACLs beyond basic permission bits |
| stat/fstat/lstat | BPX1STA/BPX1FST/BPX1LST — file metadata |
| Sysplex-aware file sharing | zFS shared file system support across sysplex |
| File tagging | chtag — associate code page with file (USS-specific) |
| FILEDATA | TEXT (auto-convert EBCDIC↔ASCII) or BINARY (no conversion) |

### 4. Shell & Utilities

| Feature | Description |
|---------|-------------|
| /bin/sh | z/OS UNIX shell (Bourne-compatible) |
| tcsh | Available as optional shell |
| bash | Available via Rocket Software port |
| BPXBATCH | JCL utility to run USS programs from batch JCL |
| BPX_BATCH_SPAWN | Env var: causes BPXBATCH to use spawn instead of fork/exec |
| TSO OMVS command | Enter USS shell from TSO |
| ISHELL | ISPF interface to USS shell |
| Core utilities | ls, cp, mv, rm, mkdir, cat, grep, awk, sed, find, make, tar, pax |
| Text processing | sort, uniq, wc, cut, paste, join, tr, head, tail |
| Shell scripting | Variables, loops, functions, here-docs, pipes, redirections |
| /etc/profile | System-wide shell profile |
| Environment variables | PATH, HOME, _BPX_JOBNAME, _BPX_SHAREAS, _CEE_RUNOPTS, etc. |

### 5. Pthreads (POSIX Threads)

| Feature | Description |
|---------|-------------|
| pthread_create | Create a new thread |
| pthread_join | Wait for thread termination |
| pthread_detach | Detach a thread |
| pthread_exit | Terminate calling thread |
| pthread_self | Get thread ID |
| pthread_mutex_init/lock/unlock/destroy | Mutex (mutual exclusion) locks |
| pthread_cond_init/wait/signal/broadcast/destroy | Condition variables |
| pthread_rwlock_init/rdlock/wrlock/unlock/destroy | Read-write locks |
| pthread_key_create/setspecific/getspecific | Thread-specific data |
| pthread_atfork | Register fork handlers |
| pthread_once | One-time initialization |
| Thread limits | MAXTHREADS, MAXTHREADTASKS (BPXPRMxx tunables) |
| Thread safety | LE provides thread-safe runtime |

### 6. Sockets API

| Feature | Description |
|---------|-------------|
| AF_INET (IPv4) | Internet Protocol version 4 sockets |
| AF_INET6 (IPv6) | Internet Protocol version 6 sockets |
| AF_UNIX | Local (UNIX domain) sockets |
| socket() | Create a socket |
| bind() | Bind socket to address |
| listen() | Mark socket as passive (server) |
| accept() | Accept incoming connection |
| connect() | Establish connection to server |
| send()/recv() | Send and receive data |
| sendto()/recvfrom() | Datagram (UDP) operations |
| select() | Synchronous I/O multiplexing |
| poll() | Another I/O multiplexing mechanism |
| getaddrinfo() | Address resolution (name → address) |
| gethostbyname() | DNS lookup (legacy) |
| getsockopt()/setsockopt() | Socket options |
| SOCK_STREAM | TCP (reliable, connection-oriented) |
| SOCK_DGRAM | UDP (unreliable, connectionless) |
| AT-TLS | Application Transparent TLS (z/OS Communications Server provides TLS without application changes) |

### 7. BPX Callable Services

Over 200 assembler-callable services organized by category:

| Category | Key Services |
|----------|-------------|
| File I/O | BPX1OPN (open), BPX1RED (read), BPX1WRT (write), BPX1CLO (close), BPX1WRV (writev), BPX1RDV (readv) |
| File metadata | BPX1STA (stat), BPX1FST (fstat), BPX1LST (lstat), BPX1CHR (chattr), BPX1CHM (chmod), BPX1CHO (chown) |
| Directory | BPX1OPD (opendir), BPX1RDD (readdir), BPX1CLD (closedir), BPX1MKD (mkdir), BPX1RMD (rmdir) |
| File system | BPX1MNT (mount), BPX1UMN (unmount), BPX1STV (statvfs) |
| Symbolic links | BPX1SYM (symlink), BPX1RDL (readlink), BPX1LNK (link), BPX1UNL (unlink) |
| Process | BPX1FRK (fork), BPX1EXC (exec), BPX1SPN (spawn), BPX1WAT (wait), BPX1KIL (kill), BPX1GPI (getpid), BPX1GPP (getppid) |
| Signals | BPX1SA (sigaction), BPX1SPM (sigprocmask), BPX1SSU (sigsuspend), BPX1SWT (sigwait) |
| Sockets | BPX1SOC (socket), BPX1BND (bind), BPX1LSN (listen), BPX1ACP (accept), BPX1CON (connect), BPX1SND (send), BPX1RCV (recv), BPX1SEL (select) |
| IPC | BPX1MGT (msgget), BPX1MRC (msgrcv), BPX1MSN (msgsnd), BPX1MCT (msgctl), BPX1SGT (shmget), BPX1SAT (shmat), BPX1SDT (shmdt), BPX1SCT (shmctl), BPX1SMG (semget), BPX1SMO (semop), BPX1SMC (semctl) |
| Memory | BPX1MMP (mmap), BPX1MUN (munmap), BPX1MPR (mprotect), BPX1MSY (msync) |
| Dynamic allocation | BPXWDYN — allocate/free MVS datasets from USS |

### 8. /etc Configuration Files

| File | Purpose |
|------|---------|
| /etc/profile | System-wide login profile |
| /etc/resolv.conf | DNS resolver configuration |
| /etc/hosts | Static hostname-to-IP mappings |
| /etc/services | Service name-to-port mappings |
| /etc/inetd.conf | Internet daemon (inetd) service definitions |
| /etc/syslog.conf | System logging configuration |
| /etc/rc | System initialization script |
| /etc/init.options | Initialization options |
| /etc/auto.master | Automount master map |
| /etc/group | UNIX group definitions (optional, RACF primary) |
| /etc/passwd | UNIX password file (optional, RACF primary) |

### 9. Security (RACF Integration)

| Feature | Description |
|---------|-------------|
| OMVS segment | RACF user profile section: UID, HOME, PROGRAM, ASSIZEMAX, THREADS, etc. |
| Group OMVS segment | GID for RACF groups |
| UID 0 (superuser) | Full system access — equivalent to root |
| BPX.SUPERUSER | FACILITY class profile — switchable superuser via su command |
| UNIXPRIV class | Granular superuser privileges (e.g., SUPERUSER.FILESYS.MOUNT) |
| BPX.DAEMON | Controls daemon authority for superusers |
| BPX.MAINCHECK | Controls MVS-to-USS access checking |
| BPX.SERVER | Server authority for identity switching |
| setuid/setgid bits | Program executes with file owner's UID/GID |
| File permission bits | Standard UNIX rwx for user/group/other |
| ACLs | Extended access control lists (getfacl/setfacl) |
| FSSEC class | zFS/TFS file system security profiles |
| DFTUSER / DFTGROUP | Default UID/GID for users without OMVS segment |
| Program control | BPX.DAEMON requires programs in RACF program-control list |

### 10. IPC (Interprocess Communication)

| Feature | Description |
|---------|-------------|
| Shared memory — shmget | Create/access shared memory segment |
| Shared memory — shmat | Attach shared memory to address space |
| Shared memory — shmdt | Detach shared memory |
| Shared memory — shmctl | Control (stat, remove) shared memory |
| Semaphores — semget | Create/access semaphore set |
| Semaphores — semop | Perform semaphore operations |
| Semaphores — semctl | Control (stat, remove) semaphore set |
| Message queues — msgget | Create/access message queue |
| Message queues — msgsnd | Send message to queue |
| Message queues — msgrcv | Receive message from queue |
| Message queues — msgctl | Control (stat, remove) message queue |
| Pipes | pipe() — anonymous pipes between parent/child |
| Named pipes (FIFO) | mkfifo() — named pipes in file system |
| BPXPRMxx IPC limits | IPCMSGNIDS, IPCSEMNIDS, IPCSHMNIDS, IPCSHMMPAGES, etc. |
| ipcs/ipcrm commands | List and remove IPC resources |

### 11. Memory-Mapped Files

| Feature | Description |
|---------|-------------|
| mmap / BPX1MMP | Map file into process address space |
| munmap / BPX1MUN | Unmap file from address space |
| mprotect / BPX1MPR | Change memory protection (read/write/exec) |
| msync / BPX1MSY | Synchronize mapped memory with file |
| MAP_SHARED | Shared mapping (visible to other processes) |
| MAP_PRIVATE | Private copy-on-write mapping |
| MAP_FIXED | Map at specific address |

### 12. USS-MVS Integration

| Feature | Description |
|---------|-------------|
| //'dataset.name' | Access MVS datasets from USS using // prefix syntax |
| BPXWDYN | Dynamic allocation of MVS datasets from USS programs |
| FILEDATA=TEXT | Automatic EBCDIC↔ASCII conversion for text files |
| FILEDATA=BINARY | No conversion — binary pass-through |
| chtag command | Tag files with code page information |
| ls -T | Display file tag information |
| iconv | Convert between character encodings (EBCDIC, ASCII, UTF-8, etc.) |
| Enhanced ASCII | z/OS 1.9+ supports ASCII as default code set for USS processes |
| _BPXK_AUTOCVT | Automatic conversion between tagged files |
| TSO OMVS | Enter USS shell from TSO |
| BPXBATCH | Run USS programs from JCL (JOB/EXEC/DD) |
| STDENV DD | Pass environment variables to BPXBATCH programs |
| STDOUT/STDERR DD | Map USS standard output/error to MVS datasets |

### 13. Daemon Support & System Services

| Feature | Description |
|---------|-------------|
| inetd | Internet super-daemon — listens on ports, starts services on demand |
| cron | Scheduled task execution |
| syslogd | System logging daemon |
| _BPX_JOBNAME | Assign MVS job name to USS process |
| _BPX_SHAREAS | Control address-space sharing for spawned processes |
| _BPX_BATCH_UMASK | Set umask for BPXBATCH programs |
| BPXOINIT | USS initialization — starts /etc/rc |
| OMVS address space | Kernel address space for USS |
| WLM OMVS classification | Work classified under OMVS subsystem type |

## Current OpenMainframe Status

### Codebase Search Results

A comprehensive search of the OpenMainframe codebase found **zero USS/POSIX implementation**. All 11 search categories returned no matches in Rust source files:

| Search Pattern | Matches in `.rs` Files |
|---------------|----------------------|
| USS / UNIX / OMVS | 0 |
| POSIX | 0 |
| fork / exec / spawn / BPX | 0 |
| zFS / HFS (file system context) | 0 |
| pthread | 0 |
| socket / AF_INET / AF_UNIX | 0 |
| BPXWDYN | 0 |
| BPXBATCH / /bin/sh | 0 |
| shmget / semget / msgget | 0 |
| mmap / memory-mapped | 0 |
| iconv / chtag / codepage | 0 |

### Adjacent Infrastructure

While no USS-specific code exists, the following adjacent implementations could be leveraged:

#### 1. Standard Rust File I/O (Dataset Crate)
**File:** `crates/open-mainframe-dataset/src/qsam.rs:6-49`
```rust
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
```
Uses Rust standard `std::fs` for QSAM sequential file access — not USS POSIX file I/O but conceptually similar. Could be extended with a USS file-system layer.

#### 2. Tokio Async Runtime (Deploy Crate)
**File:** `crates/open-mainframe-deploy/src/server.rs`
Uses `tokio::net::TcpListener`, `tokio::io::AsyncReadExt/AsyncWriteExt` — TCP socket functionality exists but via cloud-native async, not POSIX sockets.

#### 3. Rust Threading (Dataset/CICS Crates)
**File:** `crates/open-mainframe-dataset/src/locking.rs`
```rust
std::thread::sleep(Duration::from_millis(10));
```
**File:** `crates/open-mainframe-cics/src/interval/mod.rs`
Uses Rust `std::thread` — not POSIX pthreads but equivalent concurrency primitives.

#### 4. EBCDIC/Encoding Infrastructure
**Crate:** `open-mainframe-encoding`
21 code pages, packed/zoned decimal, DBCS — foundational for USS iconv/chtag character conversion requirements.

#### 5. Cross-Batch References
USS is referenced in existing gap analyses:
- **Batch 8 (RACF):** UNIXPRIV and FSSEC resource classes identified as missing
- **Batch 12 (LE):** POSIX(ON) runtime option and pthread support documented as missing
- **Batch 14 (SMF):** Type 92 USS file system activity records documented as missing
- **Batch 17 (WLM):** OMVS subsystem classification type documented as missing

## Gap Details

### Process Model

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| fork() / BPX1FRK | Full copy-on-write process creation | None | **Missing** |
| exec() / BPX1EXC | Replace process image | None | **Missing** |
| spawn() / BPX1SPN | Combined fork+exec (preferred on z/OS) | None | **Missing** |
| wait() / waitpid() / BPX1WAT | Wait for child termination | None | **Missing** |
| kill() / BPX1KIL | Send signals to processes | None | **Missing** |
| getpid() / getppid() | Process identification | None | **Missing** |
| Process groups / sessions | setpgid(), setsid() | None | **Missing** |
| Signal handling | sigaction, sigprocmask, sigsuspend, sigwait | None | **Missing** |
| BPXAS auxiliary address spaces | WLM-managed fork processing | None | **Missing** |
| Process limits (BPXPRMxx) | MAXPROCSYS, MAXPROCUSER | None | **Missing** |
| _BPX_SHAREAS | Address-space sharing control | None | **Missing** |

### File System

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| zFS (strategic file system) | Journaled, VSAM LDS-based | None | **Missing** |
| HFS (legacy file system) | Deprecated but supported | None | **Missing** |
| TFS (temporary file system) | In-memory for /tmp | None | **Missing** |
| Mount/unmount | BPX1MNT/BPX1UMN | None | **Missing** |
| Standard directory hierarchy | /, /bin, /usr, /etc, /tmp, /var, /dev | None | **Missing** |
| Symbolic links | symlink() / BPX1SYM | None | **Missing** |
| Hard links | link() / BPX1LNK | None | **Missing** |
| FIFO (named pipes) | mkfifo() / BPX1MKN | None | **Missing** |
| Character special files | /dev/null, /dev/random, etc. | None | **Missing** |
| chmod / chown | File permission and ownership changes | None | **Missing** |
| File permission bits (rwx) | Standard UNIX 9-bit model | None | **Missing** |
| ACLs | Extended access control lists | None | **Missing** |
| stat / fstat / lstat | File metadata retrieval | None | **Missing** |
| File tagging (chtag) | Code page association | None | **Missing** |
| FILEDATA TEXT/BINARY | Auto EBCDIC↔ASCII conversion | None | **Missing** |
| Sysplex file sharing | zFS shared across sysplex | None | **Missing** |
| NFS client | Remote file access | None | **Missing** |
| AUTOMNT | Automatic mount | None | **Missing** |

### Shell & Utilities

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| /bin/sh (z/OS shell) | Bourne-compatible shell | None | **Missing** |
| bash / tcsh | Optional shells (Rocket port) | None | **Missing** |
| BPXBATCH | JCL→USS bridge utility | None | **Missing** |
| Core utilities | ls, cp, mv, rm, mkdir, cat, grep, awk, sed, find | None | **Missing** |
| Shell scripting | Variables, loops, functions, pipes, redirections | None | **Missing** |
| /etc/profile | System-wide profile | None | **Missing** |
| TSO OMVS command | Enter USS from TSO | None | **Missing** |
| ISHELL | ISPF interface to USS | None | **Missing** |
| Environment variables | PATH, HOME, _BPX_*, _CEE_* | None | **Missing** |

### Pthreads

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| pthread_create | Create thread | Rust std::thread (different API) | **Missing** |
| pthread_join | Wait for thread | Rust std::thread::join (different API) | **Missing** |
| pthread_mutex_* | Mutex locks | Rust std::sync::Mutex (different API) | **Missing** |
| pthread_cond_* | Condition variables | Rust std::sync::Condvar (different API) | **Missing** |
| pthread_rwlock_* | Read-write locks | Rust std::sync::RwLock (different API) | **Missing** |
| pthread_key_* | Thread-specific data | Rust thread_local! (different API) | **Missing** |
| pthread_atfork | Fork handlers | None | **Missing** |
| Thread limits (BPXPRMxx) | MAXTHREADS, MAXTHREADTASKS | None | **Missing** |

### Sockets

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| AF_INET (IPv4) | Full TCP/UDP over IPv4 | tokio::net (cloud-native async, not POSIX API) | **Partial** |
| AF_INET6 (IPv6) | Full TCP/UDP over IPv6 | None | **Missing** |
| AF_UNIX | Local domain sockets | None | **Missing** |
| socket/bind/listen/accept | POSIX socket API | tokio equivalents exist (different API) | **Partial** |
| select() / poll() | I/O multiplexing | tokio (epoll-based, different API) | **Partial** |
| getaddrinfo() | Address resolution | None | **Missing** |
| AT-TLS | Application Transparent TLS | None | **Missing** |

### BPX Callable Services

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| File I/O services (BPX1OPN/RED/WRT/CLO) | 200+ callable services | None | **Missing** |
| Process services (BPX1FRK/EXC/SPN/WAT) | Fork, exec, spawn, wait | None | **Missing** |
| Signal services (BPX1SA/SPM/SSU) | Signal handling | None | **Missing** |
| Socket services (BPX1SOC/BND/LSN/ACP) | Socket programming | None | **Missing** |
| IPC services (BPX1MGT/SGT/SMG) | Shared memory, semaphores, message queues | None | **Missing** |
| Memory services (BPX1MMP/MUN/MPR) | Memory-mapped files | None | **Missing** |
| BPXWDYN | Dynamic MVS dataset allocation | None | **Missing** |

### Security

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| RACF OMVS segment (UID/GID) | UID, HOME, PROGRAM, ASSIZEMAX per user | None | **Missing** |
| Group OMVS segment (GID) | GID per RACF group | None | **Missing** |
| BPX.SUPERUSER | Switchable superuser authority | None | **Missing** |
| UNIXPRIV class | Granular superuser privilege control | None | **Missing** |
| BPX.DAEMON | Daemon authority control | None | **Missing** |
| BPX.SERVER | Server identity switching | None | **Missing** |
| setuid / setgid | Execute with file owner's identity | None | **Missing** |
| File ACLs | Extended access control (getfacl/setfacl) | None | **Missing** |
| DFTUSER / DFTGROUP | Default USS identity for undubbed users | None | **Missing** |

### IPC

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Shared memory (shmget/shmat/shmdt/shmctl) | System V shared memory | None | **Missing** |
| Semaphores (semget/semop/semctl) | System V semaphores | None | **Missing** |
| Message queues (msgget/msgsnd/msgrcv/msgctl) | System V message queues | None | **Missing** |
| Pipes (pipe()) | Anonymous pipes | None | **Missing** |
| Named pipes / FIFO (mkfifo()) | Named pipes in file system | None | **Missing** |
| IPC limits (BPXPRMxx) | IPCMSGNIDS, IPCSEMNIDS, IPCSHMNIDS, IPCSHMMPAGES | None | **Missing** |

### Memory-Mapped Files

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| mmap / BPX1MMP | Map file into memory | None | **Missing** |
| munmap / BPX1MUN | Unmap file from memory | None | **Missing** |
| mprotect / BPX1MPR | Change memory protections | None | **Missing** |
| msync / BPX1MSY | Sync mapped memory to file | None | **Missing** |
| MAP_SHARED / MAP_PRIVATE | Shared vs private mappings | None | **Missing** |

### USS-MVS Integration

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| //'dataset.name' syntax | Access MVS datasets from USS | None | **Missing** |
| BPXWDYN | Dynamic MVS allocation from USS | None | **Missing** |
| FILEDATA TEXT/BINARY | Auto code-page conversion | EBCDIC encoding exists (different layer) | **Partial** |
| chtag / iconv | Character set tagging and conversion | Encoding crate (21 code pages) | **Partial** |
| _BPXK_AUTOCVT | Automatic file conversion | None | **Missing** |
| Enhanced ASCII | ASCII as default USS code set | None | **Missing** |
| BPXBATCH | JCL-to-USS bridge | None | **Missing** |
| STDENV/STDOUT/STDERR DD | DD-to-USS stream mapping | None | **Missing** |

### Daemon Support

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| inetd | Internet super-daemon | None | **Missing** |
| cron | Scheduled execution | None | **Missing** |
| syslogd | System logging | None | **Missing** |
| _BPX_JOBNAME | MVS job name for USS processes | None | **Missing** |
| BPXOINIT | USS initialization | None | **Missing** |
| OMVS address space | USS kernel | None | **Missing** |

### BPXPRMxx Configuration

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| MAXPROCSYS | System-wide process limit | None | **Missing** |
| MAXPROCUSER | Per-user process limit | None | **Missing** |
| MAXFILEPROC | Per-process file descriptor limit | None | **Missing** |
| MAXTHREADS | Thread limit per process | None | **Missing** |
| MAXTHREADTASKS | Thread-tasks limit | None | **Missing** |
| IPCMSGNIDS / IPCSEMNIDS / IPCSHMNIDS | IPC resource limits | None | **Missing** |
| FILESYSTYPE | File system type definitions | None | **Missing** |
| ROOT | Root file system specification | None | **Missing** |
| FORKCOPY | Fork copy semantics | None | **Missing** |

## Proposed Epic Structure

### USS-100: POSIX Process Model
**Scope:** Implement the core USS process model — fork (as OS-level process or container spawn), exec (replace process image), spawn (combined fork+exec), wait/waitpid, kill/signal, getpid, process groups, sessions. Map to Linux/container process semantics while preserving z/OS API names and behavior.
**Complexity:** XL
**Rationale:** Foundational subsystem. Process model underpins everything else — shell, daemons, BPXBATCH. Fork semantics (address-space duplication, copy-on-write) and signal handling are complex.

### USS-101: zFS/HFS File System Layer
**Scope:** Implement USS file system abstraction — zFS (journaled, strategic), HFS (legacy), TFS (in-memory). Standard directory hierarchy (/, /bin, /usr, /etc, /tmp, /var, /dev). Mount/unmount, symbolic/hard links, FIFO, character specials, permission bits, stat/fstat/lstat. Map to host OS file system or virtual file system.
**Complexity:** XL
**Rationale:** Large surface area (20+ file operations), multiple FS types, permission model. Must bridge USS path semantics with existing dataset access model.

### USS-102: Shell & Utilities
**Scope:** Implement /bin/sh (Bourne shell) interpreter with shell scripting support (variables, loops, functions, pipes, redirections, here-docs). Core utilities (ls, cp, mv, rm, mkdir, cat, grep, find, etc.). /etc/profile processing. BPXBATCH JCL bridge.
**Complexity:** XL
**Rationale:** Full shell interpreter is a large effort. 30+ utilities required. Integration with JCL via BPXBATCH adds cross-subsystem complexity.

### USS-103: POSIX Threads (pthreads)
**Scope:** Implement POSIX thread API — pthread_create/join/detach/exit, mutexes, condition variables, read-write locks, thread-specific data, pthread_atfork. Map to Rust async/tokio or native threads while preserving POSIX API semantics.
**Complexity:** L
**Rationale:** Rust already has equivalent concurrency primitives. Main work is the POSIX API wrapper layer and thread-limit enforcement (BPXPRMxx MAXTHREADS).

### USS-104: Sockets API
**Scope:** Implement POSIX sockets — AF_INET, AF_INET6, AF_UNIX, socket/bind/listen/accept/connect/send/recv, select/poll, getaddrinfo, getsockopt/setsockopt. Map to Rust std::net or tokio::net while exposing POSIX API. AT-TLS (Application Transparent TLS) integration.
**Complexity:** L
**Rationale:** Rust/tokio already has TCP/UDP. Main work is AF_UNIX domain sockets, POSIX API compatibility layer, and AT-TLS policy integration.

### USS-105: BPX Callable Services Framework
**Scope:** Implement the BPX1xxx/BPX4xxx callable service framework — the assembler-level API surface that z/OS programs use. Organize 200+ services into categories (file I/O, process, signal, socket, IPC, memory). Each service delegates to USS-100 through USS-108 implementations.
**Complexity:** L
**Rationale:** API facade over the functional layers. Large number of services but most are thin wrappers. Need parameter-list marshaling for assembler-style calling convention.

### USS-106: System V IPC
**Scope:** Implement shared memory (shmget/shmat/shmdt/shmctl), semaphores (semget/semop/semctl), message queues (msgget/msgsnd/msgrcv/msgctl), pipes, and named pipes (FIFO). BPXPRMxx IPC limits enforcement. ipcs/ipcrm commands.
**Complexity:** M
**Rationale:** Standard System V IPC has well-defined semantics. Can map to host OS IPC or implement in-process for cloud-native mode.

### USS-107: Memory-Mapped Files
**Scope:** Implement mmap/munmap/mprotect/msync with MAP_SHARED, MAP_PRIVATE, MAP_FIXED options. Integrate with zFS file system layer for file-backed mappings.
**Complexity:** M
**Rationale:** Can delegate to host OS mmap. Main complexity is integration with USS file system layer and permission model.

### USS-108: Security Integration (RACF OMVS)
**Scope:** Implement RACF OMVS segment processing — UID/GID assignment, home directory, default shell. BPX.SUPERUSER, BPX.DAEMON, BPX.SERVER facility-class profiles. UNIXPRIV class for granular privilege control. File permission bits and ACL enforcement. setuid/setgid. DFTUSER/DFTGROUP.
**Complexity:** L
**Rationale:** Deep integration with RACF (Batch 8). Permission checking on every file operation and process creation. Must support the full UNIX security model.

### USS-109: USS-MVS Integration Bridge
**Scope:** Implement //'dataset.name' syntax for accessing MVS datasets from USS. BPXWDYN dynamic allocation. FILEDATA TEXT/BINARY auto-conversion. chtag/iconv for code page tagging. Enhanced ASCII support. _BPXK_AUTOCVT auto-conversion.
**Complexity:** L
**Rationale:** Bridges two fundamentally different I/O models (USS hierarchical files vs. MVS record-oriented datasets). Code-page conversion leverages existing encoding crate but needs USS-specific layers.

### USS-110: BPXPRMxx Configuration & Initialization
**Scope:** Implement BPXPRMxx parmlib member parsing and configuration — process limits (MAXPROCSYS, MAXPROCUSER), file limits (MAXFILEPROC), thread limits (MAXTHREADS), IPC limits, FILESYSTYPE definitions, ROOT file system, FORKCOPY. BPXOINIT initialization. D OMVS operator commands.
**Complexity:** M
**Rationale:** Configuration parsing is straightforward. Enforcement of limits across process, file, thread, and IPC subsystems requires integration with multiple epics.

### USS-111: Daemon Infrastructure
**Scope:** Implement inetd (internet super-daemon), cron (scheduled execution), syslogd (logging). _BPX_JOBNAME job-name assignment. WLM OMVS classification integration. Daemon lifecycle management.
**Complexity:** M
**Rationale:** Each daemon is relatively self-contained. inetd needs socket layer (USS-104), cron needs process model (USS-100), syslogd needs file system (USS-101).

## Dependencies

| Epic | Depends On |
|------|-----------|
| USS-100 | None (foundational process model) |
| USS-101 | USS-100 (file operations need process context) |
| USS-102 | USS-100 (shell needs fork/exec), USS-101 (shell needs file system) |
| USS-103 | USS-100 (pthreads run within process) |
| USS-104 | USS-100 (sockets need process context) |
| USS-105 | USS-100 through USS-109 (facade over all functional layers) |
| USS-106 | USS-100 (IPC between processes) |
| USS-107 | USS-101 (mmap maps files) |
| USS-108 | Batch 8 RACF (OMVS segment, UNIXPRIV, FACILITY class) |
| USS-109 | USS-101 (file system), `open-mainframe-dataset` (MVS dataset access), `open-mainframe-encoding` (code pages) |
| USS-110 | USS-100, USS-101, USS-103, USS-106 (enforces limits on all subsystems) |
| USS-111 | USS-100 (daemon processes), USS-104 (inetd sockets), USS-101 (syslogd files) |

### Cross-Batch Dependencies

| Batch | Relationship |
|-------|-------------|
| Batch 1 — REXX | REXX on USS (TSO/E REXX under OMVS) |
| Batch 5 — CLIST | CLIST in TSO, limited USS interaction |
| Batch 8 — RACF | OMVS segment, UNIXPRIV, BPX.SUPERUSER, BPX.DAEMON |
| Batch 9 — TSO/ISPF | TSO OMVS command, ISHELL |
| Batch 11 — JES2 | BPXBATCH submitted as JES job |
| Batch 12 — LE | POSIX(ON), pthread support within LE |
| Batch 14 — SMF | SMF type 92 (USS file system activity) |
| Batch 17 — WLM | OMVS subsystem classification, BPXAS auxiliary address spaces |
| Batch 20 — Networking | AT-TLS, TCP/IP stack under USS |

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| USS-100 | XL | Process model (fork/exec/spawn/signals) is foundational and complex |
| USS-101 | XL | Multiple FS types, 20+ operations, permission model, sysplex sharing |
| USS-102 | XL | Full shell interpreter + 30+ utilities + BPXBATCH bridge |
| USS-103 | L | POSIX API wrapper over Rust native threads |
| USS-104 | L | POSIX API wrapper over existing network stack |
| USS-105 | L | 200+ callable service facades over functional layers |
| USS-106 | M | Standard System V IPC — well-defined, can map to host OS |
| USS-107 | M | mmap delegation to host OS with USS file integration |
| USS-108 | L | RACF integration for every file and process operation |
| USS-109 | L | Bridges MVS datasets and USS files — two distinct I/O models |
| USS-110 | M | Configuration parsing + cross-subsystem limit enforcement |
| USS-111 | M | Three standalone daemons with clear interfaces |

**Overall Complexity: XL** — 12 proposed epics (4×M, 5×L, 3×XL). USS is a full UNIX operating system environment within z/OS. Implementing even the core layer (process model + file system + shell) is a substantial effort comparable to building a small OS emulation layer.

## Feature Count Summary

- **Total features analyzed:** 130+
- **Present:** 0
- **Partial:** 5 (tokio TCP sockets, Rust threading primitives, encoding crate for iconv/chtag foundation, std::fs for file I/O pattern)
- **Missing:** 125+

## Reference Documentation

- [z/OS UNIX System Services Planning (GA32-0884)](https://www.ibm.com/docs/en/zos/2.5.0?topic=zos-unix-system-services-planning)
- [z/OS UNIX System Services User's Guide (SA23-2279)](https://www.ibm.com/docs/en/zos/2.5.0?topic=zos-unix-system-services-users-guide)
- [z/OS UNIX System Services Programming: Assembler Callable Services Reference (SA23-2281)](https://www.ibm.com/docs/en/zos/2.5.0?topic=reference-callable-services-descriptions)
- [z/OS UNIX System Services Command Reference (SA23-2280)](https://www.ibm.com/docs/en/zos/2.5.0?topic=zos-unix-system-services-command-reference)
- [z/OS 2.5 Assembler Callable Services Reference (PDF)](https://www.ibm.com/docs/en/SSLTBW_2.5.0/pdf/bpxb100_v2r5.pdf)
- [z/OS BPXPRMxx Parmlib Member](https://www.ibm.com/docs/en/zos/3.1.0?topic=sys1parmlib-bpxprmxx-zos-unix-system-services-parameters)
- [z/OS UNIX Security Fundamentals (IBM Redpaper REDP4193)](https://www.redbooks.ibm.com/redpapers/pdfs/redp4193.pdf)
- [z/OS Distributed File Service zSeries File System Implementation (IBM Redbook)](https://www.redbooks.ibm.com/abstracts/sg246580.html)
- [UNIX System Services — Wikipedia](https://en.wikipedia.org/wiki/UNIX_System_Services)
- [IBM Community — What is USS?](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/dan-perkins/2024/03/13/what-is-uss)
