# open-mainframe-gym

`open-mainframe-gym` is a task-based evaluation harness for mainframe
development agents. It runs OpenMainframe's z/OSMF-compatible API in-process
and exposes a small Gym-style loop:

1. create an isolated `MainframeGymEnv`
2. send a `MainframeAction`
3. receive an `Observation`
4. evaluate a `GymTask` with declarative `Check` values

The harness is intended for SWE-Gym/SWE-bench-style workflows where an agent
needs a reproducible mainframe target for dataset, JCL, TSO, CICS, and z/OSMF
development tasks.

```rust
use open_mainframe_gym::{
    dataset_create_task, GymConfig, MainframeGymEnv,
};

# async fn example() -> Result<(), String> {
let env = MainframeGymEnv::new(GymConfig::isolated())?;
let report = env
    .run_task(dataset_create_task("IBMUSER.GYM.SEQ"))
    .await?;

assert!(report.passed());
# Ok(())
# }
```

This crate deliberately uses the existing Axum router instead of starting a
network listener, which keeps tests fast and deterministic while exercising the
same z/OSMF handlers used by Zowe-compatible clients.

