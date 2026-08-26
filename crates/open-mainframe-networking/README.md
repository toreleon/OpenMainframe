# open-mainframe-networking

z/OS Networking — VTAM, SNA, TCP/IP, POSIX sockets, AT-TLS, FTP, SSH/TN3270E, Sysplex networking, and IP security for the OpenMainframe project.

## Purpose

Mainframe networking combines legacy SNA (Systems Network Architecture) and APPC (Advanced Program-to-Program Communication) protocols with modern enterprise TCP/IP networking, policy-based AT-TLS encryption, FTP transfers (including JES job submission), SSH, TN3270E terminal gateways, Dynamic VIPA sysplex distribution, and IPSec filtering. This crate implements the data structures, protocol state machines, configuration parsers, and service layers for these mainframe networking capabilities.

## Capabilities

- **VTAM** (`vtam`): Application Control Blocks (`Acb`), Request Parameter Lists (`Rpl`), Exit Lists (`Exlst`), Node Initialization Blocks (`Nib`), application definitions (`APPL`), and synchronous/asynchronous session lifecycle management (`SEND`, `RECEIVE`, `CLSDST`, `TESTCB`, `SHOWCB`).
- **SNA Sessions** (`sna`): BIND parameter negotiation, LU types (LU0 raw, LU1 SCS printer, LU2 3270 data stream with orders and erase/write, LU3 DSC printer), and SCS (SNA Character String) control code processing.
- **APPC / LU 6.2** (`appc`): Common Programming Interface for Communications (CPI-C), transaction programs (`TpDefinition`, `TpRegistry`), conversation states, sync levels (`None`, `Confirm`), and Change Number of Sessions (CNOS) limit negotiation.
- **TCP/IP Configuration** (`tcpip`): Comprehensive `TCPIP.PROFILE` and `TCPIP.DATA` parser supporting `DEVICE`, `LINK`, `HOME`, `PRIMARYINTERFACE`, `AUTOLOG`, `PORT`, `PORTRANGE`, `IPCONFIG`, `TCPCONFIG` (TTLS flag), and DNS resolver configuration (`ResolverConfig`, `CinetConfig`).
- **Sockets Layer** (`sockets`): POSIX/BSD-compatible socket runtime (`SocketRuntime`) supporting `AF_INET`, `AF_INET6`, `SOCK_STREAM`, `SOCK_DGRAM`, options (`SO_REUSEADDR`, `SO_KEEPALIVE`, `SO_SNDBUF`, `SO_RCVBUF`), and multi-descriptor `select`/`poll` readiness reporting.
- **AT-TLS** (`tls`): Application Transparent TLS engine with `PolicyAgent` rule evaluation, TLS version negotiation (TLS 1.2, TLS 1.3), cipher suites, RACF `KeyringStore` certificate stores, and client certificate verification.
- **FTP Server & Client** (`ftp`): FTP protocol engine (`FtpServer`, `FtpClient`) supporting MVS datasets, USS files, directory listings (`LIST`, `NLST`), and JES2 batch job submission via `SITE FILETYPE=JES`.
- **SSH & TN3270E** (`ssh`): SSH server and session handler (password and public-key auth), plus TN3270E server and multi-session emulator with Structured Field negotiation and Query Reply parsing (Usable Area, Color, Character Sets).
- **Sysplex Networking** (`sysplex`): Static VIPA, Dynamic VIPA with automated node failover (`DynamicVipa`, `VipaManager`), and Sysplex Distributor (`SysplexDistributor`) with WLM-weighted round-robin connection balancing.
- **IP Security** (`security`): `DefenseManager`, IP filtering rules (`IpFilterRule`, `FilterAction`), CIDR matching (`IpNetwork`), and IPSec Security Association (SA) parameter negotiation.

## Architecture

```
    ┌─────────────────────────────────────────────────────────────┐
    │                      Public API (lib.rs)                    │
    ├──────────────────┬─────────────────────┬────────────────────┤
    │   SNA / VTAM     │    TCP/IP Stack     │ Higher Protocols   │
    ├──────────────────┼─────────────────────┼────────────────────┤
    │ vtam (ACB/RPL)   │ tcpip (Profiles)    │ ftp (MVS/USS/JES)  │
    │ sna (LU0/1/2/3)  │ sockets (POSIX API) │ ssh (SSH/TN3270E)  │
    │ appc (LU 6.2)    │ tls (AT-TLS Agent)  │ sysplex (DVIPA/WLM)│
    │                  │ security (IPSec)    │                    │
    └──────────────────┴─────────────────────┴────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `vtam` | VTAM ACB, RPL, EXLST, NIB structures and session control |
| `sna` | SNA session types (LU0/1/2/3), BIND negotiation, and 3270 data stream orders |
| `appc` | LU 6.2 conversations, CPI-C API, and CNOS session negotiation |
| `tcpip` | `TCPIP.PROFILE` parser, device/link configurations, and port reservations |
| `sockets` | POSIX/BSD socket runtime, address families, and socket options |
| `tls` | AT-TLS Policy Agent, rule matching, certificates, and keyring stores |
| `ftp` | FTP client and server for dataset/file transfers and JES2 job submission |
| `ssh` | SSH authentication and TN3270E multi-session terminal server |
| `sysplex` | Static/Dynamic VIPA management, failover, and Sysplex Distributor routing |
| `security` | IP filtering rules, Defense Manager, and IPSec Security Associations |

## Public API

### Primary Types and Functions

- **VTAM**: `Acb`, `AcbAuth`, `AcbMacrf`, `AcbField`, `Rpl`, `RplOperation`, `RplReturnCode`, `Exlst`, `Nib`, `Session`, `SessionState`, `VtamError`.
- **SNA**: `BindParameters`, `Command3270`, `Lu0Session`, `Lu1PrinterSession`, `Lu2Session`, `Lu3PrinterSession`, `ScsControlCode`, `SnaError`.
- **APPC**: `AppcManager`, `Conversation`, `ConversationState`, `ConversationType`, `CpiC`, `SyncLevel`, `DeallocateType`, `TpDefinition`, `TpRegistry`, `CnosResult`, `AppcError`.
- **TCP/IP**: `TcpIpProfile`, `TcpIpData`, `ResolverConfig`, `DeviceDefinition`, `LinkDefinition`, `HomeAddress`, `PortReservation`, `AutologEntry`, `CinetConfig`, `TcpIpError`.
- **Sockets**: `SocketRuntime`, `SocketError`, address constants (`AF_INET`, `AF_INET6`), socket types (`SOCK_STREAM`, `SOCK_DGRAM`), and socket options (`SOL_SOCKET`, `SO_REUSEADDR`, `SO_KEEPALIVE`, etc.).
- **AT-TLS**: `PolicyAgent`, `TlsRule`, `Certificate`, `Keyring`, `KeyringStore`, `TlsVersion`, `CipherSuite`, `HandshakeResult`, `TlsError`.
- **FTP**: `FtpServer`, `FtpClient`, `FtpReply`, `SiteFileType`, `TransferMode`, `FtpError`.
- **SSH & TN3270E**: `SshServer`, `SshSession`, `Tn3270Server`, `Tn3270Session`, `Tn3270DeviceType`, `StructuredFieldType`, `QueryReply`, `UsableArea`, `SshError`, `Tn3270Error`.
- **Sysplex**: `VipaManager`, `StaticVipa`, `DynamicVipa`, `SysplexDistributor`, `DistributorTarget`, `SysplexNetError`.
- **Security**: `DefenseManager`, `IpFilterRule`, `FilterAction`, `IpNetwork`, `SecurityAssociation`, `SecurityError`.

## Integration

- **Internal workspace dependencies**: None (depends on workspace-configured `miette`, `thiserror`).
- **Consumers**: Standalone networking subsystem crate; provides core protocols and data structures for mainframe emulation.

## Examples

### Opening a VTAM Application Control Block (ACB)

```rust
use open_mainframe_networking::vtam::{Acb, AcbAuth, AcbMacrf};

let mut acb = Acb::new("MYAPPL");
acb.set_auth(AcbAuth::ACQUIRE | AcbAuth::PASS);
acb.set_macrf(AcbMacrf::LOGON);

assert!(acb.open().is_ok());
assert!(acb.is_open());
```

### Parsing TCPIP.PROFILE Configuration

```rust
use open_mainframe_networking::tcpip::TcpIpProfile;

let config = r#"
DEVICE DEV1 CTC 0E20
LINK LNK1 CTC 1 DEV1
HOME 10.0.0.1 LNK1
PORT
    21 TCP OMVS
    23 TCP TN3270
"#;

let profile = TcpIpProfile::parse(config).expect("Valid profile");
assert_eq!(profile.home_addresses.len(), 1);
assert_eq!(profile.ports.len(), 2);
```

### AT-TLS Policy Agent Rule Evaluation

```rust
use open_mainframe_networking::tls::{
    CipherSuite, Direction, PolicyAgent, TlsRule, TlsVersion,
};

let mut agent = PolicyAgent::new();
agent.add_rule(TlsRule {
    name: "SECURE_FTP".to_string(),
    local_port: Some(21),
    remote_port: None,
    direction: Direction::Inbound,
    min_version: TlsVersion::Tls12,
    cipher_suites: vec![CipherSuite::Aes256GcmSha384],
    keyring: "FTPRING".to_string(),
    client_auth: false,
    enabled: true,
});

let matched = agent.find_matching_rule(21, 50000, Direction::Inbound);
assert!(matched.is_some());
assert_eq!(matched.unwrap().name, "SECURE_FTP");
```

## Testing

Run unit tests:

```bash
cargo test -p open-mainframe-networking
```

The test suite contains 155 unit tests covering:
- VTAM ACB open/close, RPL status transitions, and exit list invocations.
- SNA LU0 raw exchanges, LU1 SCS printer sequences, LU2 3270 order streams, and LU3 printer writes.
- APPC conversation allocation, data transfer, confirm protocols, and CNOS negotiations.
- TCP/IP profile parsing with comments, multi-homed configurations, and port reservations.
- Sockets runtime operations (`bind`, `listen`, `accept`, `send`, `recv`, `select`).
- AT-TLS handshake negotiation, cipher selection, and certificate validation.
- FTP commands, MVS/USS file retrieval, and JES job submissions.
- TN3270E negotiation and structured field query replies.
- Sysplex distributor weighted routing and dynamic VIPA failovers.
- Defense Manager IP filter rule evaluation and IPSec SA parameters.

## Limitations

- **Protocol and State Emulation**: Modules model the protocol state machines, data representations, and buffer flows in memory; they do not open raw OS kernel sockets directly unless integrated into an external network daemon.
- **Simulated Cryptography**: AT-TLS policy matching and certificate chain verification validate internal mock certificate models rather than binding native platform TLS stacks (OpenSSL/rustls).
- **Sysplex Routing**: Sysplex Distributor routes connections across modeled target servers in memory rather than binding to physical network interfaces.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-tui](../open-mainframe-tui/README.md)
- [open-mainframe-zosmf](../open-mainframe-zosmf/README.md)
