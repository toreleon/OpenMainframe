# open-mainframe-adabas

Software AG ADABAS (Adaptable DAta BAse System) — inverted-list database engine, Adabas Control Block (ACB) interface, FDT schema model, descriptor indexing, and ADALOD utility for the OpenMainframe project.

## Purpose

ADABAS is Software AG's high-performance, inverted-list database management system widely used on IBM mainframes for mission-critical transactional applications. Unlike relational systems, ADABAS organizes records in compressed Data Storage while maintaining secondary access paths through inverted-list descriptor indexes in the Associator. The `open-mainframe-adabas` crate provides a pure Rust implementation of the ADABAS database engine, modeling the direct ACB call interface, Field Definition Tables (FDT), multi-type descriptor indexing, search criteria evaluation, transaction management (ET/BT), and standard utilities (ADALOD, ADAUNI, ADASAV).

## Capabilities

- **ADABAS Nucleus & ACB Interface (`nucleus`, `acb`)**:
  - Direct Call interface: `AdabasNucleus` orchestrates file definitions, user sessions, and ACB command dispatch.
  - Supported command codes: Read commands (`L1`–`L6`, `L9`, `LF`), Search commands (`S1`, `S2`, `S4`, `S8`, `S9`), Modification commands (`N1`/`N2` store, `A1` update, `E1` delete), Transaction commands (`ET` commit, `BT` rollback), Session commands (`OP`, `CL`), and Lock commands (`HI` hold, `RI` release).
  - Buffer model: Format Buffer (`FormatBuffer`), Record Buffer, Search Buffer (`SearchBuffer`), Value Buffer, and ISN Buffer.
  - Standard response codes: `0` (Success), `3` (End of file), `17` (File not found), `22` (Invalid command), `113` (ISN not found), `145` (Hold queue overflow), and `148` (Security violation).
- **Field Definition Table & Schema (`fdt`)**:
  - Schema definition (`Fdt`, `FieldDef`) supporting 8 field data types: `Alpha` (A), `Numeric` (N), `Packed` (P), `Unpacked` (U), `Binary` (B), `Wide` (W), `FixedPoint` (F), and `Float` (G).
  - Field options: `Descriptor` (DE), `Unique` (UQ), `NullSuppression` (NU), `MultipleValue` (MU), `PeriodicGroup` (PE), `LongAlpha` (LA), and `LargeObject` (LB).
  - Nested group structures (`GroupField`) and multi-value occurrence tracking (`MultipleValueField`).
- **Descriptor Indexing Engine (`descriptor`)**:
  - Standard descriptors (single field index), unique descriptors, super-descriptors (composite multi-field), sub-descriptors (byte substring), phonetic descriptors (Soundex algorithm), hyper-descriptors, and collation descriptors.
  - Inverted list maintenance (`DescriptorSet`, `InvertedList`) mapping descriptor keys to sorted lists of ISNs (Internal Sequence Numbers).
- **Search Criteria Evaluation (`search`)**:
  - Compound Boolean query engine (`SearchBuffer`, `SearchCriteria`, `SearchOperator`: `Eq`, `Gt`, `Lt`, `Ge`, `Le`, `Ne`) with `AND`, `OR`, `NOT` logical operators and merge-join set operations (`intersect_sorted`, `union_sorted`, `subtract_sorted`).
- **Storage Layer Simulation (`storage`)**:
  - **Data Storage**: In-memory RABN-keyed block store (`DataStorage`) for record data.
  - **Associator**: `AssociatorStorage` housing the `AddressConverter` (ISN-to-RABN map) and per-descriptor `InvertedList` instances.
  - **Address Converter**: O(1) ISN-to-RABN translation (`AddressConverter`).
- **Transaction Management (`transaction`)**:
  - Command-Level Protection (`ClpNumber`), `HoldQueue` for pessimistic record locking, and before/after image write-ahead logging (`TransactionLog`, `ProtectionLog`).
  - Transaction commit (`ET`) and rollback (`BT`) restoration.
- **Utilities (`utilities`)**:
  - `AdalodUtility`: Bulk data loader supporting Initial and Mass-Update modes.
  - `AdauniUtility`: Data extraction utility (all records or by ISN list).
  - `AdasavUtility`: Backup and restore utility with JSON snapshot serialization.
  - `Ddm`: Natural Data Definition Module generation from FDT definitions.

## Architecture

```
                    Natural / 3GL Application
                               │
                               ▼
               ┌───────────────────────────────┐
               │    Adabas Control Block       │
               │ (ACB + FB / RB / SB / VB / IB)│
               └───────────────┬───────────────┘
                               │
                               ▼
        ┌───────────────────────────────────────────────┐
        │                 AdabasNucleus                 │
        │                                               │
        │  ┌─────────────────┐     ┌─────────────────┐  │
        │  │  Search Engine  │     │   Txn Manager   │  │
        │  │ (Inverted Lists)│     │(HoldQueue/ET/BT)│  │
        │  └────────┬────────┘     └────────┬────────┘  │
        │           │                       │           │
        │           ▼                       ▼           │
        │  ┌─────────────────────────────────────────┐  │
        │  │              Storage Engine             │  │
        │  │  Associator (AS) │ Address Converter(AC)│  │
        │  │  Data Storage(DS)│ Block Manager (RABN) │  │
        │  └─────────────────────────────────────────┘  │
        └───────────────────────────────────────────────┘
```

### Module Structure

| Module | Purpose |
|--------|---------|
| `nucleus` | Central command router (`AdabasNucleus`, `NucleusParams`, `ProtectionLog`, `CommandLog`, `WorkPool`) |
| `acb` | Adabas Control Block (`Acb`, `AcbCommand`, `AcbResult`, `FormatBuffer`, `FieldRef`, `parse_format_buffer`) |
| `fdt` | Field Definition Table (`Fdt`, `FieldDef`, `FieldType`, `FieldOption`, `GroupField`, `MultipleValueField`) |
| `descriptor` | Descriptor engine (`DescriptorSet`, `Descriptor`, `SuperDescriptor`, `SubDescriptor`, `PhoneticDescriptor`, `InvertedList`) |
| `search` | Search evaluation (`SearchBuffer`, `SearchCriteria`, `SearchOperator`, `Isnlist`) |
| `read` | Read commands (`ReadCommand`, `ReadOptions`, `ReadResult`, `ReadCursor`) |
| `modify` | Record updates (`StoreCommand`, `UpdateCommand`, `DeleteCommand`, `UpdateDescriptors`) |
| `storage` | Physical structures (`DataStorage`, `AssociatorStorage`, `AddressConverter`, `AdabasFile`, `Isn`, `Rabn`) |
| `transaction` | Transaction management (`TransactionManager`, `TransactionLog`, `HoldQueue`, `ClpNumber`, `TransactionState`) |
| `utilities` | Mainframe utilities (`AdalodUtility`, `AdauniUtility`, `AdasavUtility`, `Ddm`) |

## Public API

### Primary Types and Functions

- `AdabasNucleus`: Core engine managing files, FDT schemas, descriptor inverted lists, and executing ACB calls:
  - `new(params: NucleusParams) -> Self`: Instantiate nucleus.
  - `define_file(&mut self, file_num: u16, name: &str, fdt: Fdt, descriptors: DescriptorSet) -> Result<(), AdabasError>`: Define a file.
  - `execute_acb(&mut self, acb: &Acb) -> AcbResult`: Process an ACB command block.
- `Acb`: Adabas Control Block specifying command (`AcbCommand`), file number, ISN, and attached buffers.
- `Fdt` / `FieldDef`: Schema definitions for ADABAS files and fields.
- `DescriptorSet`: Index management structure for standard, super, sub, and phonetic descriptors.
- `SearchBuffer` / `SearchCriteria`: Evaluates search queries against inverted-list indexes.
- `AdalodUtility` / `AdasavUtility`: Batch loading and backup utilities.

## Integration

### Internal Workspace Dependencies

- `miette`: Diagnostic error reporting.
- `thiserror`: Error derive macros.
- `serde` / `serde_json`: Backup image serialization and DDM export.
- `tracing`: Diagnostic operational logging.

### Workspace Consumers

- `open-mainframe-natural`: Uses ADABAS schema (DDM) concepts for 4GL data access.
- `open-mainframe-wiki`: Documented in the database catalog.
- Root workspace member in `Cargo.toml`.

## Examples

### Defining a File and Executing ACB Commands

```rust
use open_mainframe_adabas::acb::{Acb, AcbCommand};
use open_mainframe_adabas::descriptor::{Descriptor, DescriptorSet};
use open_mainframe_adabas::fdt::{Fdt, FieldDef, FieldType};
use open_mainframe_adabas::nucleus::{AdabasNucleus, NucleusParams};

fn main() {
    let mut nucleus = AdabasNucleus::new(NucleusParams::default());

    let mut fdt = Fdt::new();
    fdt.add_field(FieldDef::new("AA", 1, FieldType::Alpha, 20).with_descriptor()).unwrap();
    fdt.add_field(FieldDef::new("AB", 1, FieldType::Alpha, 30)).unwrap();

    let mut descriptors = DescriptorSet::new();
    descriptors.descriptors.push(Descriptor::new("AA"));

    nucleus.define_file(1, "EMPLOYEES", fdt, descriptors).expect("define file failed");

    // Store record (N1)
    let store_acb = Acb::new(AcbCommand::N1, 1).with_record_buffer(b"SMITH".to_vec());
    let store_res = nucleus.execute_acb(&store_acb);
    assert_eq!(store_res.response_code, 0);
    let isn = store_res.isn;

    // Read record (L1)
    let read_acb = Acb::new(AcbCommand::L1, 1).with_isn(isn);
    let read_res = nucleus.execute_acb(&read_acb);
    assert_eq!(read_res.record_buffer, b"SMITH");
}
```

### Searching Inverted Lists

```rust
use open_mainframe_adabas::search::{SearchBuffer, SearchCriteria, SearchOperator};
use open_mainframe_adabas::storage::InvertedList;

fn main() {
    let mut inv_list = InvertedList::new();
    inv_list.insert("JONES", 101);
    inv_list.insert("JONES", 105);
    inv_list.insert("SMITH", 102);

    let criteria = SearchCriteria::new("AA", SearchOperator::Eq, "JONES");
    let search_buf = SearchBuffer::new(criteria);
    let isns = search_buf.evaluate(&inv_list);

    assert_eq!(isns.as_slice(), &[101, 105]);
}
```

## Testing

Run the full test suite:

```sh
cargo test -p open-mainframe-adabas
```

The crate contains 113 unit tests verifying:
- `storage`: File CRUD, Data Storage blocks, Address Converter lookups, and Inverted List insertion/range queries.
- `fdt`: Field definitions, builders, group fields, and multiple-value occurrence validation.
- `descriptor`: Standard descriptors, super-descriptor extraction, sub-descriptor substrings, and Soundex phonetic indexing.
- `acb`: Command code parsing, format buffer parsing, and ACB result extraction.
- `search`: Operator parsing, criteria evaluation, ISN list merge operations (`AND`/`OR`/`NOT`), and range scans.
- `read` & `modify`: N1/N2 store with ISN allocation, A1 update, E1 delete, and descriptor synchronization.
- `transaction`: Transaction begin/commit/rollback lifecycles, write-ahead protection logging, and hold queue locking.
- `nucleus`: Full ACB execution pipeline, session start/stop, work pool allocation, and response code reporting.
- `utilities`: `ADALOD` bulk load, `ADAUNI` unload, `ADASAV` backup/restore, and `Ddm` conversion.

## Limitations

- **In-Memory Storage**: Data Storage and Associator storage are held in in-memory hash maps rather than persisted to direct DASD datasets.
- **Nucleus Execution of Complex Reads/Searches**: `ReadCursor` and `SearchBuffer::evaluate` operate against internal structures directly; the `execute_acb` dispatch currently executes `L1` (read by ISN), `N1`/`N2`, `A1`, and `E1`.
- **Field-Level Buffer Packing**: Records are stored as contiguous byte buffers rather than dynamically unpacked according to format buffer field descriptors.
- **Single-Process Nucleus**: The nucleus runs in-process without OS cross-memory SVC routing or multi-threaded concurrency controls.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-natural](../open-mainframe-natural/README.md)
