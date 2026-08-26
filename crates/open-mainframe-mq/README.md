# open-mainframe-mq

A comprehensive Rust implementation of **IBM MQ (Message Queuing)** for the OpenMainframe project — providing the complete enterprise messaging subsystem: Queue Manager lifecycle, Message Queue Interface (MQI) operations, MQSC command parsing and execution, Publish/Subscribe topic trees, triggering, and channel management.

## Purpose

IBM MQ is the premier message-oriented middleware for mainframe enterprise integration, enabling reliable, asynchronous, and decoupled communication between disparate applications. `open-mainframe-mq` models this subsystem within OpenMainframe:
1. **Queue Manager**: Manages the lifecycle and state of local, alias, remote, model, and transmission queues.
2. **Message Queue Interface (MQI)**: Implements standard MQI calls (`MQCONN`, `MQDISC`, `MQOPEN`, `MQCLOSE`, `MQPUT`, `MQPUT1`, `MQGET`, `MQINQ`, `MQSET`) with full option flags.
3. **MQ Structures**: Provides binary and struct representations for `MQMD` (Message Descriptor), `MQOD` (Object Descriptor), `MQPMO` (Put Message Options), `MQGMO` (Get Message Options), `MQRFH2` (JMS Header), and `MQDLH` (Dead Letter Header).
4. **MQSC Engine**: Parses and executes administrative MQSC commands (`DEFINE`, `ALTER`, `DELETE`, `DISPLAY`, `CLEAR`) across all MQ object types.
5. **Advanced Messaging**: Implements Publish/Subscribe topic trees with wildcard subscriptions, trigger monitor events, and channel authentication (CHLAUTH).

## Capabilities

- **MQI Interface Implementation (`MqiHandle`, `Connection`)**:
  - `MQCONN` and `MQDISC` connection management with application tracking.
  - `MQOPEN` and `MQCLOSE` with fine-grained access modes (`input`, `output`, `inquire`, `set`, `browse`).
  - `MQPUT` and `MQPUT1` supporting priority ordering, persistence flags, expiry intervals, and correlation IDs.
  - `MQGET` supporting browse cursor traversal (`BrowseFirst`, `BrowseNext`), destructive reads, match options (`MatchMsgId`, `MatchCorrelId`), and wait intervals.
  - `MQINQ` and `MQSET` for querying and modifying queue attributes dynamically.
- **Queue Manager Core (`QueueManager`, `Queue`)**:
  - Manages queues by type: `Local`, `Alias`, `Remote`, `Model`, `Transmission`.
  - Enforces `MAXDEPTH`, `MAXMSGL`, and queue inhibition flags (`GetInhibited`, `PutInhibited`).
  - Automatic Dead-Letter Queue (`DEADQ`) routing with `MQDLH` headers upon delivery failure.
- **MQSC Command Engine (`MqscEngine`)**:
  - Full syntax support for `DEFINE`, `ALTER`, `DELETE`, `DISPLAY`, `CLEAR`.
  - Object targets: `QLOCAL`, `QALIAS`, `QREMOTE`, `QMODEL`, `CHANNEL`, `TOPIC`, `PROCESS`, `AUTHINFO`.
- **Publish/Subscribe Engine (`pubsub`, `TopicTree`)**:
  - Hierarchical topic tree structure supporting wildcards (`/`, multilevel `#`, single-level `+`).
  - Subscription management (`MQSUB`, `MQSUBRQ`) supporting durable and non-durable subscribers.
- **Triggering Mechanism (`triggering`)**:
  - Generates trigger messages to initiation queues based on trigger conditions (`First`, `Every`, `Depth`).
- **Channel Management & Security (`channels`)**:
  - Channel definitions for `Sender`, `Receiver`, `ServerConnection` (SVRCONN), and `Cluster`.
  - Channel Authentication (CHLAUTH) rules supporting `BLOCKUSER`, `MAPUSER`, and SSL/TLS cipher specifications.

## Architecture

```text
    Application Source                    MQ Runtime Environment
    ┌──────────────┐                      ┌────────────────────┐
    │ CALL 'MQPUT' │    MQI Interface     │   Queue Manager    │
    │ Descriptor,  │ ──────────────────>  │   (QueueManager)   │
    │ Payload      │    Connection        │  Queues, MsgStore  │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Management        ┌────────────────────┐
    │  MQSC Cmds   │ ──────────────────>  │    Channel Mgr     │
    │  DEFINE Q... │    MqscEngine        │    SDR / RCVR      │
    └──────────────┘                      │    TLS, CHLAUTH    │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Pub/Sub           ┌────────────────────┐
    │  Topics      │ <──────────────────  │   Trigger Monitor  │
    │  Subscribers │    TopicTree         │   Process Defs     │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `core` | Queue Manager core: `QueueManager`, `Queue`, `QueueType`, queue storage, depth limits. |
| `mqi` | MQI API: `Connection`, `MqiHandle`, `OpenOptions`, `GetOptions`, `PutOptions`, handle mapping. |
| `structures`| Standard MQ structures: `Mqmd`, `Mqod`, `MqPmo`, `Mqgmo`, `Mqrfh2`, `Mqdlh`. |
| `mqsc` | Administrative command engine: `MqscEngine`, `MqscResult`, AST parsing and execution. |
| `pubsub` | Publish/Subscribe: `TopicTree`, `TopicNode`, `Subscription`, publication routing. |
| `channels` | Channel management: `ChannelManager`, `ChannelDefinition`, `ChlAuthRule`, connection security. |
| `triggering`| Triggering: `TriggerMonitor`, `TriggerType`, `ProcessDefinition`, initiation messages. |

## Public API

### Core Types and Services

```rust
use open_mainframe_mq::{
    QueueManager, QueueType, Queue,
    Connection, MqiHandle,
    Mqmd, Mqod, MqPmo, Mqgmo, Mqrfh2, Mqdlh,
    OpenOptions, GetOptions, PutOptions,
    mqsc::{MqscEngine, MqscResult},
    MqError, MqResult,
};
```

- `QueueManager`: Subsystem controller managing queues, topics, channels, and triggers.
- `Connection`: Interactive MQI connection handle implementing `MqiHandle`.
- `Mqmd` / `Mqod`: Message and Object descriptor structures.
- `MqscEngine`: Command interpreter executing MQSC administration scripts.

## Integration

### Workspace Dependencies

- None (pure Rust library using standard workspace crates: `miette`, `thiserror`, `serde`, `serde_json`, `tracing`, `chrono`, `uuid`).

### Known Consumers

- Standalone message broker within the OpenMainframe workspace, available for CICS transaction queuing and batch messaging.

## Examples

### Putting and Getting Messages via MQI

```rust
use open_mainframe_mq::{
    QueueManager, QueueType, Connection, MqiHandle,
    Mqmd, Mqod, MqPmo, Mqgmo, OpenOptions, GetOptions,
};

let mut qm = QueueManager::new("QM1");
qm.define_queue("PAYLOAD.QUEUE", QueueType::Local).unwrap();

// Connect to queue manager
let mut conn = Connection::connect(&mut qm).unwrap();

// Open queue for output
let mut od = Mqod::new("PAYLOAD.QUEUE");
let handle = conn.open(&qm, &mut od, OpenOptions {
    output: true,
    ..Default::default()
}).unwrap();

// Put a message
let mut md = Mqmd::new();
let mut pmo = MqPmo::new();
conn.put(&mut qm, &handle, &mut md, &mut pmo, b"HELLO MAINFRAME MQ").unwrap();
conn.close(&qm, handle).unwrap();

// Open queue for input and retrieve message
let in_handle = conn.open(&qm, &mut od, OpenOptions {
    input_shared: true,
    ..Default::default()
}).unwrap();

let mut gmo = Mqgmo::new();
let (retrieved_md, payload) = conn.get(&mut qm, &in_handle, &mut gmo, 1024).unwrap();
assert_eq!(payload, b"HELLO MAINFRAME MQ");

conn.close(&qm, in_handle).unwrap();
conn.disconnect(&mut qm).unwrap();
```

### Executing MQSC Administration Commands

```rust
use open_mainframe_mq::QueueManager;
use open_mainframe_mq::mqsc::MqscEngine;

let mut qm = QueueManager::new("QM1");
let mut engine = MqscEngine::new(&mut qm);

let result = engine.execute(
    "DEFINE QLOCAL(ORDERS.IN) MAXDEPTH(10000) DEFSPSR(YES) REPLACE"
).unwrap();

assert!(result.success);
println!("MQSC Response: {}", result.output);
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-mq
```

The test suite covers:
- **`mqi::*`**: Connection lifecycle, `MQOPEN` option bitmasks, `MQPUT`/`MQGET` correlation matching, browse mode sequencing, and buffer sizing.
- **`core::*`**: Queue depth limit enforcement, priority queue ordering, get/put inhibit flags, and dead-letter queue routing.
- **`structures::*`**: `MQMD`, `MQRFH2`, `MQDLH` serialization and default field initialization.
- **`mqsc::*`**: Syntax parsing for all command verbs (`DEFINE`, `ALTER`, `DELETE`, `DISPLAY`, `CLEAR`) and object attributes.
- **`pubsub::*`**: Topic tree branching, wildcard matching (`+`, `#`), and multi-subscriber delivery.
- **`channels::*`**: Channel state transitions, CHLAUTH address mapping, and user blocking rules.

## Limitations

- **Storage & XA Transactions**: Storage is managed in memory with file snapshotting rather than mainframe coupling facility structures or full XA two-phase commit coordinators.
- **Network Channels**: Distributed queue manager channel communication uses local socket emulation rather than real SNA/LU6.2 transport.
- **Clustering**: Multi-queue manager cluster repositories and automatic workload balancing across clusters are not implemented.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [CICS Subsystem (`open-mainframe-cics`)](../open-mainframe-cics/README.md)
- [SMF Subsystem (`open-mainframe-smf`)](../open-mainframe-smf/README.md)
