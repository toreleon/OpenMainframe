# open-mainframe-pgmmgmt

z/OS Program Management — Binder (linkage editor), Object Module (OBJ) and Load Module (LMOD) parser/formatter, and Program Manager loader for the OpenMainframe project.

## Purpose

Program management on z/OS encompasses the preparation of compiled object decks into executable load modules (symbol resolution and address relocation) and the runtime loading and invocation of programs across the standard MVS library hierarchy (STEPLIB, JOBLIB, LPA, LNKLST) with APF authorization checks and macro lifecycle management (`LOAD`, `DELETE`, `LINK`, `XCTL`, `ATTACH`, `DETACH`).

## Capabilities

- **Object Module Processing** (`objmod`):
  - Parses and formats standard 80-byte card image Object (OBJ) decks and binary Load Module (LMOD) records.
  - Recognizes record types: ESD (External Symbol Dictionary), TXT (program text), RLD (Relocation Dictionary), and END (entry point and length).
  - Handles CESD (Composite External Symbol Dictionary) records in load modules.
- **Binder Engine** (`binder`):
  - Combines multiple `ObjectModule` inputs into a unified `LoadModule`.
  - External symbol resolution supporting SD (Section Definition / CSECT), LD (Label Definition / ENTRY), ER (External Reference), and WX (Weak External Reference).
  - Detects duplicate symbol definitions (`BinderError::DuplicateSymbol`) and unresolved non-weak symbols (`BinderError::UnresolvedSymbol`).
  - Relocation calculation for 3-byte and 4-byte address constants (adcons: A-type and V-type).
  - Output module naming and alias assignment.
- **Program Manager & Loader** (`program`):
  - Multi-tiered search hierarchy: `STEPLIB` → `JOBLIB` → `LPA` → `LNKLST`.
  - Module residency tracking: `LOAD` increments active reference count; `DELETE` decrements and removes unreferenced modules.
  - Program invocation macros: `LINK` (execute and return), `XCTL` (transfer control, replacing current execution stack entry), `ATTACH` (spawn subtask TCB with ECB completion posting), and `DETACH`.
  - APF (Authorized Program Facility) list management (`ApfList`) ensuring unauthorized callers cannot link to APF-restricted libraries.
  - AMODE (24, 31, 64, Any) and RMODE (24, Any) metadata tracking.

## Architecture

```
       Source Compilers (COBOL, HLASM, PL/I)
                         │
                         ▼
    ┌────────────────────────────────────────────────────────┐
    │                      objmod                            │
    │  - Parses 80-byte OBJ card images (ESD, TXT, RLD, END) │
    │  - Parses / writes binary LoadModule structures        │
    └────────────────────┬───────────────────────────────────┘
                         │ ObjectModule
                         ▼
    ┌────────────────────────────────────────────────────────┐
    │                      binder                            │
    │  - Binder: Global symbol table resolution              │
    │  - Computes section placements and adcon relocations   │
    │  - Emits bound LoadModule with aliases                 │
    └────────────────────┬───────────────────────────────────┘
                         │ LoadModule
                         ▼
    ┌────────────────────────────────────────────────────────┐
    │                      program                           │
    │  - ProgramLibrary registrations (STEPLIB, LPA, LNKLST) │
    │  - ProgramManager: search hierarchy & APF validation   │
    │  - LOAD / DELETE use count lifecycle                   │
    │  - LINK / XCTL / ATTACH invocation simulation          │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `objmod` | Low-level OBJ and LMOD record parsing, encoding, and roundtrip serialization |
| `binder` | Linkage editor: ESD resolution, RLD relocation engine, and `LoadModule` generation |
| `program` | MVS program search hierarchy, APF verification, module loader, and macro execution |

## Public API

### Primary Types and Functions

- `Binder`: Linkage editor coordinator (`new()`, `add_module()`, `set_entry()`, `add_alias()`, `bind()`).
- `ObjectModule`: Compilation unit deck containing ESD entries, text records, and RLD entries.
- `LoadModule`: Bound binary module containing merged text bytes, resolved symbols (`ResolvedSymbol`), and aliases.
- `EsdEntry` / `EsdType`: External symbol records (`SectionDef`, `LabelDef`, `ExternalRef`, `WeakExternalRef`).
- `TextRecord`: Binary code/data payload mapped to an ESDID.
- `RldEntry` / `AdconType`: Relocation dictionary entries and adcon types (`AType`, `VType`).
- `ProgramManager`: Runtime program loader and invocation controller (`new()`, `add_library()`, `apf_list_mut()`, `load()`, `delete()`, `link()`, `xctl()`, `attach()`, `detach()`, `is_loaded()`, `use_count()`).
- `ProgramLibrary`: Library dataset representation (`new()`, `add_program()`, `find()`).
- `SearchPathType`: Search hierarchy categories (`Steplib`, `Joblib`, `Lpa`, `Lnklst`).
- `LoadedProgram`: Memory-resident program descriptor (`name`, `entry_point`, `amode`, `rmode`, `apf_authorized`, `use_count`, `text`, `aliases`).
- `ApfList`: Authorized program facility list (`new()`, `add()`, `is_authorized()`, `list()`).
- `Amode` (`Amode24`, `Amode31`, `Amode64`, `Any`) / `Rmode` (`Rmode24`, `RmodeAny`).
- `Tcb`: Task Control Block representing attached subtasks.
- `ExecutionResult`: Program execution outcome (`return_code`).
- `BinderError` / `ProgramError` / `ParseError`: Subsystem error enumerations.

## Integration

- **Internal workspace dependencies**: None (depends on workspace-configured `miette`, `thiserror`).
- **Consumers**: Standalone program management library; models linkage and loader semantics for runtime and tooling components.

## Examples

### Binding Object Modules

```rust
use open_mainframe_pgmmgmt::binder::{
    Binder, EsdEntry, EsdType, ObjectModule, TextRecord,
};

let mut binder = Binder::new("PROG01");

let mut module = ObjectModule {
    name: "MOD1".to_string(),
    esd_entries: vec![EsdEntry {
        name: "MAIN".to_string(),
        esd_type: EsdType::SectionDef,
        esdid: 1,
        offset: 0,
        length: 64,
    }],
    text_records: vec![TextRecord {
        esdid: 1,
        offset: 0,
        data: vec![0x47, 0xF0, 0xF0, 0x00],
    }],
    rld_entries: Vec::new(),
};

binder.add_module(module);
binder.set_entry("MAIN");

let load_module = binder.bind().expect("Binding failed");
assert_eq!(load_module.name, "PROG01");
```

### Loading and Linking via ProgramManager

```rust
use open_mainframe_pgmmgmt::binder::{Binder, EsdEntry, EsdType, ObjectModule};
use open_mainframe_pgmmgmt::program::{
    ProgramLibrary, ProgramManager, SearchPathType,
};

let mut binder = Binder::new("PAYROLL");
binder.add_module(ObjectModule {
    name: "PAYROLL".to_string(),
    esd_entries: vec![EsdEntry {
        name: "PAYROLL".to_string(),
        esd_type: EsdType::SectionDef,
        esdid: 1,
        offset: 0,
        length: 32,
    }],
    text_records: Vec::new(),
    rld_entries: Vec::new(),
});
let load_mod = binder.bind().unwrap();

let mut steplib = ProgramLibrary::new("SYS1.STEPLIB", SearchPathType::Steplib, false);
steplib.add_program(load_mod);

let mut mgr = ProgramManager::new();
mgr.add_library(steplib);

// Load module into virtual storage
let loaded = mgr.load("PAYROLL").expect("Program should load");
assert_eq!(loaded.use_count, 1);

// Execute via LINK
let result = mgr.link("PAYROLL", None).expect("Link should succeed");
assert_eq!(result.return_code, 0);
```

## Testing

Run unit tests:

```bash
cargo test -p open-mainframe-pgmmgmt
```

The test suite contains 44 unit tests covering:
- Object and Load Module binary record parsing, serialization, and roundtrips in `objmod`.
- Relocation dictionary (RLD) arithmetic and 3-byte/4-byte adcon patch calculation.
- Duplicate symbol and unresolved external reference diagnostics.
- Library search precedence (`STEPLIB` before `JOBLIB` before `LPA` before `LNKLST`).
- APF authorization enforcement on protected programs.
- `LOAD`/`DELETE` reference counts and `ATTACH`/`DETACH` subtask management.

## Limitations

- **Object Format Support**: Parsing supports standard fixed-format 80-byte OBJ card images and LMOD records; IBM GOFF (Generalized Object File Format) is not implemented.
- **Simulated Execution**: `ProgramManager::link()`, `xctl()`, and `attach()` simulate successful program invocation returning condition codes (`ExecutionResult`); they do not interpret raw s390x machine instructions or run binary machine code in host memory.
- **Subtasking**: `ATTACH` and `DETACH` manage in-memory `Tcb` status models rather than creating OS-level threads or kernel processes.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-runtime](../open-mainframe-runtime/README.md)
- [open-mainframe-mvs](../open-mainframe-mvs/README.md)
