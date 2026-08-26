# open-mainframe-wlm

A sophisticated Rust implementation of the **z/OS Workload Manager (WLM)** for the OpenMainframe project — providing goal-oriented resource management, workload classification rules, service policies, performance index monitoring, enclave tracking, resource capping, and scheduling environments.

## Purpose

WLM is the core operating system subsystem in IBM z/OS responsible for dynamically distributing system resources (CPU, storage, I/O) according to business goals and priorities rather than static dispatching rules. `open-mainframe-wlm` models this subsystem within OpenMainframe:
1. **Work Classification**: Evaluates incoming work requests (batch jobs, CICS transactions, TSO sessions, DB2 queries) against priority-ordered qualification rules to assign service classes.
2. **Service Definition & Goals**: Defines service policies containing service classes with response time, execution velocity, or discretionary performance targets and importance levels (1–5).
3. **Performance Monitoring**: Tracks actual resource consumption and calculates Performance Index (PI) ratios to detect and remedy goal degradation.
4. **Enclaves & Capping**: Tracks cross-address space transaction units (enclaves) and enforces CPU and resource group utilization caps.

## Capabilities

- **Workload Classifier (`Classifier`, `ClassificationRule`)**:
  - Matches work attributes (`subsystem_type`, `subsystem_name`, `transaction_name`, `user_id`, `job_class`, `accounting`, `procedure_name`, `lu_name`).
  - Supports qualifier types, wildcard pattern matching (`*`), rule priorities, explicit service class overrides, and subsystem defaults.
- **Service Policies & Classes (`ServicePolicy`, `ServiceClass`)**:
  - Service goals: `GoalType::ResponseTime { target_seconds, percentile }`, `GoalType::Velocity(u8)`, `GoalType::Discretionary`.
  - Multi-period service classes with duration-based degradation.
  - Policy activation and runtime inspection.
- **Performance Evaluation (`PerformanceIndex`, `WorkUnitTracker`)**:
  - Calculates Performance Index (PI = Actual / Target for response time; PI = Target / Actual for velocity). PI > 1.0 indicates goal missed; PI < 1.0 indicates exceeding goal.
- **Enclave Management (`EnclaveManager`, `Enclave`)**:
  - Creates, joins, and tracks independent and work-dependent enclaves spanning multiple address spaces.
- **Resource Capping Engine (`CappingEngine`)**:
  - Enforces CPU caps and resource group limits, generating throttle actions (`Suspend`, `Delay`, `Resume`).
- **Scheduling & Application Environments (`InitiatorScheduler`, `AppEnvironmentManager`)**:
  - Resource-aware job scheduling based on `SchedulingEnvironment` resource states (`ON`/`OFF`).
  - Server auto-scaling for application environments.
- **Operator Commands & Telemetry (`VaryWlmCommand`, `DisplayWlmResponse`)**:
  - Supports `VARY WLM,POLICY=...` switching, health queries, Prometheus metrics, and SMF Type 72/99 data records.

## Architecture

```text
    Work Incoming                         WLM Management Hub
    ┌──────────────┐                      ┌────────────────────┐
    │  Work Request│    Classification    │    Classifier      │
    │  Job, Trans  │ ──────────────────>  │    (Rules)         │
    └──────────────┘    WorkAttribute     │  Service Classes   │
           │                              └────────────────────┘
           ▼                                        │
    ┌──────────────┐    Monitoring        ┌────────────────────┐
    │  Enclaves /  │ ──────────────────>  │   Policy Engine    │
    │  Address Spc │    PerformanceIdx    │   Goals vs Actual  │
    └──────────────┘                      │  Velocity, Resp    │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Action            ┌────────────────────┐
    │  Resource    │ <──────────────────  │   Capping Engine   │
    │  Adjustment  │    ThrottleAction    │   CPU / Mem Cap    │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `classify` | Classification engine: `Classifier`, `ClassificationRule`, `WorkAttribute`, `WorkRequest`, `QualifierType`. |
| `service` | Policy definition: `ServicePolicy`, `ServiceClass`, `ServiceGoal`, `GoalType`, `Importance`. |
| `goals` | Performance metrics: `PerformanceIndex`, `WorkUnitTracker`, `ResourceDemand`, `SlidingWindow`. |
| `enclave` | Enclave tracking: `EnclaveManager`, `Enclave`, `EnclaveState`. |
| `capping` | Resource capping: `CappingEngine`, `ThrottleAction`, `GroupUtilization`, CPU capping. |
| `scheduling`| Work dispatch: `InitiatorScheduler`, `SchedulingEnvironment`, `AppEnvironmentManager`. |
| `iwm` | Macro services: `IwmServices` (IWMCLSY, IWMENC, IWMSUP). |
| `operator` | Operator commands: `VaryWlmCommand`, `DisplayWlmResponse`, SMF metrics. |
| `health` | Subsystem telemetry: JSON health responses for classes, enclaves, and resource groups. |
| `persistence`| Policy store: Storage and activation of service definitions. |
| `policy` | Advanced policy constructs: `ResourceGroup`, `ReportClass`, `Workload`, `CpuCapType`. |

## Public API

### Core Types and Services

```rust
use open_mainframe_wlm::{
    Classifier, ClassificationRule, QualifierType, WorkRequest, SubsystemType,
    ServicePolicy, ServiceClass, ServiceGoal, GoalType, Importance,
    EnclaveManager, Enclave,
    CappingEngine, ThrottleAction,
    InitiatorScheduler, SchedulingEnvironment,
    PerformanceIndex,
};
```

- `Classifier`: Matches work requests to defined service classes based on prioritized rule sets.
- `ServicePolicy`: Named collection of service class definitions and performance targets.
- `PerformanceIndex`: Computes performance index values indicating whether work is meeting goals.
- `EnclaveManager`: Manages transaction enclaves across simulated address spaces.

## Integration

### Workspace Dependencies

- None (pure Rust library using standard workspace crates: `miette`, `thiserror`, `serde`, `serde_json`, `tracing`).

### Known Consumers

- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — Powers `/zosmf/wlm` REST endpoints for policy activation, workload inspection, and performance metrics.

## Examples

### Defining Service Policies and Classifying Work

```rust
use open_mainframe_wlm::{
    Classifier, ClassificationRule, QualifierType, WorkRequest,
    ServicePolicy, ServiceClass, GoalType, Importance,
};

// 1. Define a service policy with goals
let mut policy = ServicePolicy::new("STANDARD");
let online_class = ServiceClass::new(
    "ONLINE",
    GoalType::ResponseTime { target_seconds: 0.5, percentile: 95.0 },
    Importance(1),
);
policy.define_class(online_class).unwrap();

// 2. Configure classification rules
let mut classifier = Classifier::new("DEFAULT");
let mut rule = ClassificationRule::new("CICS_PAYROLL", "ONLINE");
rule.add_qualifier(QualifierType::SubsystemType, "CICS");
rule.add_qualifier(QualifierType::TransactionName, "PAY*");
classifier.add_rule(rule);

// 3. Classify incoming work request
let request = WorkRequest::new("CICS", "PAYROLL");
let routed_class = classifier.classify_and_verify(&request, &policy).unwrap();
assert_eq!(routed_class, "ONLINE");
```

### Monitoring Performance Index

```rust
use open_mainframe_wlm::{PerformanceIndex, GoalType};

// Response time goal: 0.5s target. Actual: 0.4s -> PI = 0.8 (Goal met)
let pi = PerformanceIndex::calculate_response_time(0.5, 0.4);
assert!(pi.is_meeting_goal());

// Actual: 1.0s -> PI = 2.0 (Goal missed)
let pi_missed = PerformanceIndex::calculate_response_time(0.5, 1.0);
assert!(!pi_missed.is_meeting_goal());
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-wlm
```

The test suite covers:
- **`classify::*`**: Qualifier type matching, wildcard prefixes, priority resolution, subsystem defaults, and high-throughput classification benchmarks.
- **`service::*`**: Policy CRUD, duplicate service class rejection, and multi-period goal structures.
- **`goals::*`**: Performance index calculation across response time and velocity goals.
- **`enclave::*`**: Enclave creation, address space joins, CPU accumulation, and termination cleanup.
- **`capping::*`**: Resource group quota calculation, throttle state transitions, and capping enforcement.

## Limitations

- **Hardware Dispatching**: Resource balancing and capping are evaluated in software logic rather than through hardware CPU dispatching priorities or PR/SM hypervisor weight adjustments.
- **Coupling Facility**: Sysplex-wide multisystem workload routing uses local coordination rather than CF structures.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [CICS Subsystem (`open-mainframe-cics`)](../open-mainframe-cics/README.md)
- [SMF Subsystem (`open-mainframe-smf`)](../open-mainframe-smf/README.md)
- [z/OSMF REST Subsystem (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
