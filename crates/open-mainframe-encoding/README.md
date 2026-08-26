# open-mainframe-encoding

EBCDIC character encoding and decimal arithmetic conversions for the OpenMainframe workspace.

## Purpose

`open-mainframe-encoding` provides foundational mainframe data encoding, character page translations, and binary/decimal numeric format conversions. It serves as the low-level byte and numeric interchange layer across compilers, runtime subsystems, datasets, databases, and network bridges in OpenMainframe.

## Capabilities

- **21 EBCDIC Code Pages**: Bidirectional ASCII/UTF-8 ↔ EBCDIC conversion for:
  - 3 Base Pages: CP037 (US/Canada), CP500 (International), CP1047 (Open Systems/USS)
  - 8 European National Pages: CP273 (DE/AT), CP277 (DK/NO), CP278 (SE/FI), CP280 (IT), CP284 (ES/Latin America), CP285 (UK), CP297 (FR), CP871 (IS)
  - 10 Euro-Enabled Pages: CP1140 through CP1149
- **Packed Decimal (COMP-3)**: Binary Coded Decimal (BCD) encoding and decoding with arbitrary-precision `rust_decimal::Decimal` support (up to 18 digits) and lightweight `i64` conversion helpers.
- **Zoned Decimal (DISPLAY)**: Standard zoned numeric format with sign zone nibble (0xC0 positive, 0xD0 negative, 0xF0 unsigned).
- **Binary Integers (COMP / COMP-4 / COMP-5)**: Big-endian two's complement integer encoding for halfwords (2B), fullwords (4B), and doublewords (8B), with both PIC-digit range clamping (COMP/COMP-4) and full storage-range support (COMP-5).
- **Floating-Point Encodings**:
  - IBM Hexadecimal Floating Point (HFP): COMP-1 (4-byte short) and COMP-2 (8-byte long) with base-16 exponents and IEEE 754 cross-conversion.
  - IEEE 754 Floating Point: `FLOAT-SHORT` (f32) and `FLOAT-LONG` (f64) in big-endian byte order.
- **NATIONAL Encoding (PIC N)**: UTF-16 big-endian encoding, decoding, padding, and direct EBCDIC-to-national conversion.
- **DBCS Mixed Encoding**: SO (`0x0E`) / SI (`0x0F`) shift-state stream parsing and character counting.
- **EBCDIC Collation**: Native EBCDIC byte-value sorting and classification matching z/OS DFSORT behavior (spaces < lowercase < uppercase < digits).
- **PIC Clause Resolution**: Parsing of storage PIC clauses (`S9(5)V99`, `X(30)`, `N(10)`) and resolving `FieldEncoder` across 9 `CobolUsage` variants.

## Architecture

```
open-mainframe-encoding
├── ebcdic/                   # EBCDIC character encoding & collation
│   ├── mod.rs                # CodePage encode/decode, from_ccsid, by_name
│   ├── tables.rs             # CodePage struct + CP037/CP500/CP1047 tables
│   ├── extended_tables.rs    # 18 national + Euro code page tables
│   ├── registry.rs           # CodePageRegistry (CCSID + name lookup)
│   ├── collation.rs          # EBCDIC byte-value comparison and sorting
│   └── dbcs.rs               # SO/SI mixed SBCS/DBCS stream handling
├── decimal/                  # Numeric storage formats
│   ├── mod.rs                # Sign enum, module re-exports
│   ├── packed.rs             # COMP-3 packed decimal (BCD)
│   ├── zoned.rs              # DISPLAY zoned decimal
│   ├── binary.rs             # COMP/COMP-4 big-endian binary
│   ├── native_binary.rs      # COMP-5 native binary (full range)
│   ├── floating.rs           # IBM HFP short/long (base-16 float)
│   ├── ieee_float.rs         # IEEE 754 single/double precision
│   └── national.rs           # NATIONAL (UTF-16 BE) encoding
├── field_conversion.rs       # PIC clause parser + encoder resolver
├── error.rs                  # EncodingError with miette diagnostics
└── lib.rs                    # Crate root, re-exports
```

### Module Descriptions

| Module | Responsibility |
|---|---|
| `ebcdic::tables` | `CodePage` struct with bidirectional 256-byte translation tables and special-character overlay (e.g. Euro sign at 0x9F) |
| `ebcdic::extended_tables` | Static `CodePage` tables for 18 national and Euro code pages (CP273, CP277, CP278, CP280, CP284, CP285, CP297, CP871, CP1140–CP1149) |
| `ebcdic::registry` | `CodePageRegistry` with CCSID lookup, alias matching (CP*, IBM*, IBM-*, EBCDIC-*), and full enumeration |
| `ebcdic::collation` | Native EBCDIC ordering (`sort_ebcdic`, `is_ebcdic_sorted`, `EbcdicCharClass`) |
| `ebcdic::dbcs` | SO (0x0E) / SI (0x0F) state machine for mixed SBCS/DBCS streams |
| `decimal::packed` | `PackedDecimal`, `pack_decimal`, `unpack_decimal`, `pack_from_i64`, `unpack_to_i64` |
| `decimal::zoned` | `ZonedDecimal`, `zone_decimal`, `unzone_decimal`, `zone_from_i64`, `unzone_to_i64` |
| `decimal::binary` | `BinaryInteger`, `encode_binary`, `decode_binary` with COBOL digit sizing |
| `decimal::native_binary` | `NativeBinaryInteger`, `encode_native_binary`, `decode_native_binary` for COMP-5 |
| `decimal::floating` | `HfpFloat`, `HfpDouble`, `ieee_to_hfp_*`, `hfp_*_to_ieee` base-16 float conversions |
| `decimal::ieee_float` | `IeeeFloat`, `IeeeDouble` big-endian IEEE 754 wrappers |
| `decimal::national` | UTF-16 BE encode/decode and `ebcdic_to_national` conversion |
| `field_conversion` | `parse_pic` and `resolve_encoder` mapping PIC strings to `FieldEncoder` |
| `error` | `EncodingError` enum with `miette` diagnostic integration |

## Public API

### Primary Types and Functions

- `CodePage`: Bidirectional translation table supporting `encode`, `decode`, `ebcdic_to_char`, `ebcdic_to_ascii_byte`, `ascii_to_ebcdic_byte`, `from_ccsid`, and `by_name`.
- `CodePageRegistry`: Static registry providing lookup by CCSID integer or string alias across all 21 bundled pages.
- `PackedDecimal` / `pack_decimal` / `unpack_decimal`: BCD conversion with `rust_decimal::Decimal`.
- `ZonedDecimal` / `zone_decimal` / `unzone_decimal`: DISPLAY numeric conversion.
- `BinaryInteger` / `NativeBinaryInteger`: Fixed-width binary integer encoders.
- `HfpFloat` / `HfpDouble`: IBM hexadecimal floating point representations.
- `IeeeFloat` / `IeeeDouble`: IEEE 754 big-endian representations.
- `CobolUsage`: 9-variant enum representing COBOL storage usage (`Display`, `Comp`, `Comp3`, `Comp5`, `Comp1`, `Comp2`, `FloatShort`, `FloatLong`, `National`).
- `FieldEncoder`: Resolved encoder enum providing `byte_length()`.
- `EncodingError`: 6-variant error type (`InvalidCodePage`, `ConversionFailed`, `OutOfRange`, `InvalidDigit`, `InvalidSign`, `PrecisionLoss`).

## Integration

### Internal Workspace Dependencies

*None* — `open-mainframe-encoding` depends only on external crates (`miette`, `thiserror`, `rust_decimal`).

### Workspace Consumers

Used across 10 crates in the workspace whenever raw mainframe byte buffers are parsed or generated:

- `open-mainframe` — Top-level CLI and driver.
- `open-mainframe-cics` — CICS 3270 data stream, BMS maps, and queue records.
- `open-mainframe-cobol` — COBOL data item sizing, numeric storage conversions, and intrinsic functions.
- `open-mainframe-dataset` — QSAM, BSAM, PDS, and VSAM record byte decoding/encoding.
- `open-mainframe-ims` — IMS database segment encoding.
- `open-mainframe-jcl` — In-stream dataset encoding and SYSIN/SYSOUT conversion.
- `open-mainframe-runtime` — Language Environment runtime storage formats and arithmetic.
- `open-mainframe-sort` — DFSORT byte collation and key extraction.
- `open-mainframe-wiki` — Doc generation and sample encoding tools.
- `open-mainframe-zosmf` — REST API file/dataset payload conversion.

## Examples

### EBCDIC Character Conversion

```rust
use open_mainframe_encoding::ebcdic::{CP037, CP1140, CodePage};

// Basic encode/decode with US/Canada CP037
let ebcdic = CP037.encode("HELLO WORLD").unwrap();
let ascii = CP037.decode(&ebcdic).unwrap();
assert_eq!(ascii, "HELLO WORLD");

// Euro symbol with CP1140
let encoded = CP1140.encode("Price: 100€").unwrap();
let decoded = CP1140.decode(&encoded).unwrap();
assert_eq!(decoded, "Price: 100€");

// Lookup code page by CCSID or alias
let cp_de = CodePage::from_ccsid(273).unwrap(); // German
let cp_uss = CodePage::by_name("IBM-1047").unwrap(); // USS
```

### Packed Decimal (COMP-3)

```rust
use open_mainframe_encoding::decimal::{pack_decimal, unpack_decimal};
use rust_decimal::Decimal;
use std::str::FromStr;

let value = Decimal::from_str("123.45").unwrap();
let packed = pack_decimal(&value, 3, 2, true).unwrap();
// Result: [0x12, 0x34, 0x5C] — three BCD digit pairs + positive sign nibble

let (unpacked, sign) = unpack_decimal(&packed, 2).unwrap();
assert_eq!(unpacked, value);
```

### EBCDIC Native Collation

```rust
use open_mainframe_encoding::ebcdic::collation::sort_ebcdic;
use open_mainframe_encoding::ebcdic::CP037;

let mut data = vec!["123".to_string(), "ABC".to_string(), "abc".to_string(), " ".to_string()];
sort_ebcdic(&mut data, &CP037);
// EBCDIC ordering: space < lowercase < uppercase < digits
assert_eq!(data, vec![" ", "abc", "ABC", "123"]);
```

### PIC Clause Resolution

```rust
use open_mainframe_encoding::field_conversion::{parse_pic, resolve_encoder, CobolUsage, FieldEncoder};

let pic = parse_pic("S9(5)V99");
let encoder = resolve_encoder(&pic, &CobolUsage::Comp3);
assert_eq!(encoder, FieldEncoder::PackedDecimal { digits: 7, decimal: 2, signed: true });
assert_eq!(encoder.byte_length(), 4);
```

## Testing

The crate includes 216 unit and documentation tests verifying all code pages, arithmetic formats, and boundary conditions:

```bash
cargo test -p open-mainframe-encoding
```

Key test locations:
- `src/ebcdic/tables.rs` & `src/ebcdic/extended_tables.rs` — 256-byte roundtrips for all 21 code pages (5,376 byte combinations) and Euro sign overlays.
- `src/ebcdic/collation.rs` — Collation order and character classification tests.
- `src/ebcdic/dbcs.rs` — SO/SI shift-state parsing and error handling.
- `src/decimal/packed.rs` & `src/decimal/zoned.rs` — Positive/negative/unsigned sign handling and precision limits.
- `src/decimal/binary.rs` & `src/decimal/native_binary.rs` — Halfword, fullword, and doubleword ranges.
- `src/decimal/floating.rs` & `src/decimal/ieee_float.rs` — HFP and IEEE 754 conversions and NaN/Inf rejection.
- `src/decimal/national.rs` — UTF-16 BE padding and EBCDIC-to-national conversion.
- `src/field_conversion.rs` — PIC clause parsing and encoder resolution.

## Limitations

- **DBCS Code Pages**: Shift-state infrastructure (SO/SI) is implemented, but East Asian multi-byte character tables (CP930, CP933, CP935) are not packaged.
- **PIC Clause Subset**: `parse_pic` supports storage PIC forms (9, X, N, V, S) but does not parse display-editing PIC patterns (Z, *, $, CR, DB, comma, period insertion).
- **Floating-Point Formats**: Supports 4-byte (short) and 8-byte (long) HFP/IEEE floats; 16-byte extended precision is not supported.
- **Special Character Overlays**: The code page overlay mechanism currently only maps the Euro currency sign (€); other multi-byte EBCDIC symbols are not mapped.
- **Buffer-Based API**: Encoding and decoding operate on contiguous slices and buffers; no streaming reader/writer interfaces are provided.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace crate architecture and dependency overview.
- [open-mainframe-runtime](../open-mainframe-runtime/README.md) — LE runtime decimal math and storage.
- [open-mainframe-cobol](../open-mainframe-cobol/README.md) — COBOL data item types and storage sizing.
- [open-mainframe-dataset](../open-mainframe-dataset/README.md) — Mainframe dataset I/O and record translation.
