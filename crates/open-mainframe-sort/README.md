# open-mainframe-sort

DFSORT (Data Facility Sort) — high-performance mainframe sort, merge, copy, and data manipulation engine for the OpenMainframe project.

## Purpose

DFSORT is the primary batch utility on IBM z/OS for sorting, merging, copying, selecting, reformatting, and reporting on datasets. The `open-mainframe-sort` crate implements a full-featured DFSORT-compatible engine in Rust, modeling multi-key collation, external disk-backed sort runs, record filtering (`INCLUDE`/`OMIT`), record reformatting (`INREC`/`OUTREC`/`OVERLAY`/`FINDREP`), conditional `IFTHEN` processing, multiple output routing (`OUTFIL`), multi-dataset relational joins (`JOINKEYS`), and `ICETOOL` reporting operators.

## Capabilities

- **Sorting & Merging**:
  - Multi-key collation supporting 5 mainframe data formats: `Character` (CH), `ZonedDecimal` (ZD), `PackedDecimal` (PD), `Binary` (BI), and `FixedPoint` (FI), with `Ascending` (A) and `Descending` (D) order per key.
  - In-memory sorting for datasets below `max_memory_records` (default 100,000 records).
  - External k-way merge sort using a min-heap over temporary run files on disk for large datasets.
  - Record formats: `LineBased` (text), `Fixed(usize)` (binary), and `VariableBlocked` with 4-byte RDW (Record Descriptor Word).
  - Format conversions for output: Fixed-to-Variable (`Ftov`) and Variable-to-Fixed (`Vtof`).
  - Copy mode (`SortEngine::copy()`) for filtering and reformatting without sorting.
  - `SUM` field accumulation for numeric fields on duplicate keys and `SUM FIELDS=NONE` for deduplication.
  - Record limits and skipping via `STOPAFT` and `SKIPREC`.
- **Filtering (`INCLUDE`/`OMIT`)**:
  - Field-to-constant and field-to-field comparisons (`CompareOp`: `Eq`, `Ne`, `Gt`, `Ge`, `Lt`, `Le`).
  - Compound Boolean expressions combining conditions with `AND` and `OR`.
- **Record Reformatting (`INREC`/`OUTREC`)**:
  - Fixed column extraction, space insertion, literal insertion, stateful sequence numbers (`SEQNUM`), record counts (`COUNT`), edit masks (`M0`–`M26`), date/time formatting, and date arithmetic.
  - In-place field modification (`OVERLAY`) and string replacement (`FINDREP`).
  - Conditional processing with `IFTHEN` clauses (`WHEN=(cond)`, `WHEN=INIT`, `WHEN=NONE`, `WHEN=GROUP`).
- **Multiple Output Routing (`OUTFIL`)**:
  - Splitting records across multiple output descriptors via filters, `SPLITBY`, or round-robin `SPLIT`.
  - Header and trailer generation (`HEADER1`, `TRAILER1`, etc.) with dynamic date stamps, page numbers, and record counts.
  - Dedicated `INREC`/`OUTREC` transformations per `OUTFIL` group.
- **Relational Joins (`JOINKEYS`)**:
  - Two-file joins with `JOINKEYS FILE=F1` and `JOINKEYS FILE=F2`.
  - Join types: `Inner`, `LeftOuter`, `RightOuter`, `FullOuter`, `UnpairedF1`, `UnpairedF2`.
  - Join record reformatting and fill byte padding for unmatched records.
- **ICETOOL Utility Operators**:
  - High-level operator commands: `COPY`, `COUNT`, `DISPLAY` (columnar reports with page headers), `OCCUR` (frequency distribution), `SELECT` (filter by occurrence count), `SORT`, `STATS` (MIN, MAX, AVG, TOTAL), and `UNIQUE`.
- **Symbols & Control Statement Parsing**:
  - DFSORT symbol dictionary support (`SYMNAMES`) and system symbol substitutions (`DATE1`–`DATE4`, `TIME1`–`TIME3`).
  - Parser for DFSORT parameter statements (`SORT`, `MERGE`, `INCLUDE`, `OMIT`, `INREC`, `OUTREC`, `OUTFIL`, `SUM`, `OPTION`).

## Architecture

```
                 Control Statements (DFSORT / ICETOOL)
                                  │
                                  ▼
                   ┌─────────────────────────────┐
                   │  parse_control_statements   │
                   └──────────────┬──────────────┘
                                  │ SortSpec / FilterSpec / OutrecSpec / OutfilSpec
                                  ▼
        ┌──────────────────────────────────────────────────┐
        │                    SortEngine                    │
        │                                                  │
        │  Input Records ──> [ INREC / IFTHEN ]           │
        │                         │                        │
        │                         ▼                        │
        │             [ INCLUDE / OMIT Filter ]            │
        │                         │                        │
        │                         ▼                        │
        │             [ In-Memory / External Sort ]        │
        │             [ SUM Duplicate Handling ]           │
        │                         │                        │
        │                         ▼                        │
        │             [ OUTREC / OVERLAY Reformat ]        │
        │                         │                        │
        │                         ▼                        │
        │             [ OUTFIL Descriptors / Split ]       │
        └─────────────────────────┬────────────────────────┘
                                  │
                                  ▼
                    Output Files / ICETOOL Reports
```

### Module Structure

| Module | Purpose |
|--------|---------|
| `engine` | Core sorting, merging, copy processing, external disk-backed run merging, `RecordFormat`, `FormatConversion` |
| `fields` | Data type specifications (`DataType`), collation ordering (`SortOrder`), key extraction, and arithmetic conversion |
| `parser` | Control statement parser for DFSORT syntax (`parse_control_statements`) |
| `filter` | Record selection engine (`FilterSpec`, `Condition`, `CompareOp`) |
| `reformat` | Record layout manipulation (`OutrecSpec`, `OutrecField`, `OverlaySpec`, `FindRepSpec`, date arithmetic) |
| `ifthen` | Conditional reformatting rules (`IfThenSpec`, `IfThenClause`, `WhenCondition`, `IfThenAction`) |
| `outfil` | Output stream routing (`OutfilSpec`, `OutfilDescriptor`, `SplitMode`, `HeaderTrailerSpec`) |
| `joinkeys` | Two-source dataset joining (`JoinKeysSpec`, `JoinKeyField`, `JoinType`) |
| `icetool` | High-level data utility commands (`IceToolOp`, `FieldStats`, `DisplayColumn`, `OnField`) |
| `symbols` | Symbol table mapping and dynamic date/time symbol expansion |
| `error` | Error definitions (`SortError`) |

## Public API

### Primary Types and Functions

- `SortEngine`: Main execution coordinator:
  - `new(sort_spec: SortSpec) -> Self`: Create engine configured with sort keys.
  - `copy() -> Self`: Create engine in COPY mode (no sorting).
  - `sort_records(&self, records: Vec<Vec<u8>>) -> Result<Vec<Vec<u8>>, SortError>`: In-memory sort and reformat.
  - `sort_file<P: AsRef<Path>>(&mut self, input: P, output: P) -> Result<(), SortError>`: File-to-file sort with automatic external sort if threshold is exceeded.
  - Builder methods: `with_include`, `with_omit`, `with_inrec`, `with_outrec`, `with_sum`, `with_record_format`, `with_stopaft`, `with_skiprec`, `with_format_conversion`.
- `SortSpec` / `SortField`: Specifies key positions (1-indexed), lengths, data types, and ascending/descending order.
- `DataType`: `Character` (CH), `ZonedDecimal` (ZD), `PackedDecimal` (PD), `Binary` (BI), `FixedPoint` (FI).
- `parse_control_statements(source: &str) -> Result<SortEngine, SortError>`: Parses standard DFSORT control statements.
- `IceToolOp`: Configures and executes ICETOOL operations (`COPY`, `COUNT`, `DISPLAY`, `OCCUR`, `SELECT`, `SORT`, `STATS`, `UNIQUE`).
- `JoinKeysSpec`: Configures two-file JOINKEYS joins with key matching and field reformatting.

## Integration

### Internal Workspace Dependencies

- `open-mainframe-encoding`: EBCDIC ↔ ASCII code page translation for character key collation and decimal conversions.
- `thiserror`: Error derive macros.

### Workspace Consumers

- `open-mainframe-jcl`: Executes DFSORT and ICETOOL steps in JCL batch jobs.
- `open-mainframe-runtime`: Bridges COBOL `SORT` and `MERGE` statements via `sort_verb.rs`.
- `open-mainframe-assess`: Inspects migration readiness for programs utilizing SORT verbs.
- Root workspace member in `Cargo.toml`.

## Examples

### Sorting In-Memory Records by Key

```rust
use open_mainframe_sort::{DataType, SortEngine, SortField, SortOrder, SortSpec};

fn main() {
    let mut spec = SortSpec::new();
    // Sort on columns 1-5 as Character Ascending
    spec.add_field(SortField::new(1, 5, DataType::Character, SortOrder::Ascending));

    let engine = SortEngine::new(spec);

    let input = vec![
        b"00003 Charlie".to_vec(),
        b"00001 Alice  ".to_vec(),
        b"00002 Bob    ".to_vec(),
    ];

    let sorted = engine.sort_records(input).expect("sort failed");
    assert_eq!(sorted[0], b"00001 Alice  ");
    assert_eq!(sorted[1], b"00002 Bob    ");
    assert_eq!(sorted[2], b"00003 Charlie");
}
```

### Filtering and Reformatting Records

```rust
use open_mainframe_sort::{
    CompareOp, Condition, DataType, FilterSpec, OutrecField, OutrecSpec, SortEngine,
};

fn main() {
    let mut filter = FilterSpec::include();
    // Include where columns 1-2 equal "NY"
    filter.add_condition(Condition::field_vs_constant(1, 2, CompareOp::Eq, b"NY".to_vec()));

    let mut outrec = OutrecSpec::new();
    // Copy columns 3-10, insert literal " - ", copy columns 1-2
    outrec.add_field(OutrecField::copy(3, 8));
    outrec.add_field(OutrecField::literal(b" - ".to_vec()));
    outrec.add_field(OutrecField::copy(1, 2));

    let engine = SortEngine::copy()
        .with_include(filter)
        .with_outrec(outrec);

    let input = vec![
        b"NYALICE   ".to_vec(),
        b"CABOB     ".to_vec(),
        b"NYCHARLIE ".to_vec(),
    ];

    let output = engine.sort_records(input).expect("processing failed");
    assert_eq!(output.len(), 2);
    assert_eq!(output[0], b"ALICE    - NY");
    assert_eq!(output[1], b"CHARLIE  - NY");
}
```

## Testing

Run the test suite for this crate:

```sh
cargo test -p open-mainframe-sort
```

The crate contains 130 unit tests verifying:
- `engine`: In-memory sorting, external sort with disk-based run files and k-way heap merging, STOPAFT/SKIPREC limits, SUM decimal accumulation, and temporary file cleanup.
- `fields`: Key extraction, packed/zoned/binary decimal conversions and comparisons.
- `filter`: INCLUDE and OMIT conditions with AND/OR logic and comparison operators.
- `reformat`: Field extraction, padding, edit masks, date arithmetic, FINDREP replacement, and OVERLAY logic.
- `ifthen`: IFTHEN clause precedence, group detection, and stateful sequence numbering.
- `outfil`: Multiple output routing, split modes, and header/trailer generation.
- `joinkeys`: Inner, outer, and unpaired join variations with fill bytes.
- `icetool`: Verification of `COPY`, `COUNT`, `DISPLAY`, `OCCUR`, `SELECT`, `SORT`, `STATS`, and `UNIQUE` operators.
- `parser`: Parsing of multi-line control statements.

## Limitations

- **Spanned Variable Records**: Variable-length blocked records (`VariableBlocked`) are supported with 4-byte RDWs, but Segment Descriptor Words (SDW) for spanned records (VBS) are not segmented across sort chunks.
- **User Exit Routines**: Standard assembler exit routines (E15 for input modification, E35 for output modification) are not emulated.
- **Dynamic Collating Sequences**: Character comparisons use EBCDIC CP037 / ASCII ordering; custom dynamic `ALTSEQ` translation tables are not yet supported.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-dataset](../open-mainframe-dataset/README.md)
- [open-mainframe-jcl](../open-mainframe-jcl/README.md)
- [open-mainframe-runtime](../open-mainframe-runtime/README.md)
