# Encoding Crate — Architecture Decisions

## AD-3.0-01: Code Page Registry with Static Tables

**Context:** The crate currently has three code pages as module-level constants (CP037, CP1047, CP500). Adding more code pages (10+ Euro variants, national code pages) requires a scalable lookup mechanism.

**Decision:** Create a `CodePageRegistry` backed by a static `HashMap<u16, &'static CodePage>` mapping CCSID numbers to code page instances. All code pages are defined as static arrays (256-byte translation tables) compiled into the binary. Lookup is by CCSID number or string name. The registry is populated at compile time via `lazy_static` or `once_cell`.

**Consequences:**
- O(1) lookup by CCSID number
- No runtime file loading — all code pages are statically compiled
- Binary size increases ~512 bytes per code page (two 256-byte tables)
- Adding a new code page requires only adding the two translation arrays
- Backward compatible — existing CP037/CP1047/CP500 constants remain

## AD-3.0-02: IBM Hexadecimal Floating Point Implementation

**Context:** IBM mainframes use Hexadecimal Floating Point (HFP) natively for COMP-1 and COMP-2. HFP differs from IEEE 754: base-16 exponent (not base-2), 7-bit biased exponent (bias 64), and different normalization rules. COMP-1 is 4 bytes (1 sign + 7 exponent + 24 fraction), COMP-2 is 8 bytes (1 sign + 7 exponent + 56 fraction).

**Decision:** Implement `HfpFloat` (4-byte) and `HfpDouble` (8-byte) types with encode/decode methods. Conversion to/from Rust `f32`/`f64` (IEEE 754) is provided via `From` trait implementations. The conversion handles the exponent base difference (16 vs 2), normalization differences, and edge cases (zero, denormalized numbers). No extended precision (COMP-2 extended) in initial implementation.

**Consequences:**
- Accurate representation of z/OS COMP-1/COMP-2 data
- Conversion to IEEE may lose precision for some HFP values (different radix)
- Conversion from IEEE to HFP may introduce rounding differences
- Special values (NaN, Infinity) do not exist in HFP — must handle gracefully
- Testing requires known HFP bit patterns from IBM documentation

## AD-3.0-03: COMP-5 as Separate Encoder

**Context:** COMP-5 (native binary) differs from COMP/COMP-4 in truncation behavior. COMP truncates to the PIC clause digit range; COMP-5 truncates to the storage size range. Both use big-endian two's complement with the same storage sizes.

**Decision:** Implement `NativeBinaryInteger` (COMP-5) as a separate type that reuses the same encoding/decoding byte logic as `BinaryInteger` but with different range validation. COMP-5 range is determined solely by byte count: halfword ±32767, fullword ±2147483647, doubleword ±9223372036854775807. The existing `encode_binary`/`decode_binary` functions gain an optional `native_truncation: bool` parameter.

**Consequences:**
- Minimal code duplication — shared byte encoding, different validation
- COBOL programs using COMP-5 will get correct storage-based truncation
- Must clearly document the difference for users choosing between COMP and COMP-5

## AD-3.0-04: DBCS Mixed Encoding as Stateful Parser

**Context:** EBCDIC DBCS mixed encoding uses SO (0x0E) and SI (0x0F) shift codes to switch between single-byte and double-byte regions within a byte stream. Parsing from the middle of a stream is impossible without context.

**Decision:** Implement a `MixedEncoder` that processes byte streams left-to-right, tracking SBCS/DBCS state. The encoder emits Unicode code points by combining the SBCS code page (e.g., CCSID 290 for Japanese) with the DBCS code page (e.g., CCSID 300). The `MixedCodePage` struct wraps two code pages (SBCS + DBCS) and handles SO/SI insertion during encoding and removal during decoding.

**Consequences:**
- Stateful parsing — cannot decode arbitrary byte ranges
- Requires DBCS translation tables (much larger than SBCS — 65536 entries)
- SO/SI bytes consume space in fixed-length records
- Initial implementation can support CP930 (Japanese) as proof of concept
- Other DBCS code pages (CP935, CP933) follow the same pattern
