# Encoding Crate — Product Requirements

## Overview

The `open-mainframe-encoding` crate provides EBCDIC character encoding and COBOL numeric data format support for the OpenMainframe platform. It includes bidirectional EBCDIC/ASCII translation tables for three code pages, packed decimal (COMP-3), zoned decimal (DISPLAY), and binary integer (COMP/COMP-4) encoding/decoding.

## Current State Assessment

- **Lines of code:** 1,832
- **Test count:** 48 (all passing)
- **Maturity:** High (core formats complete and well-tested, no TODOs or stubs)
- **Files:** 8 Rust source files across 2 modules (ebcdic, decimal)

### What Works Well

**EBCDIC Character Encoding:**
- Code Page 037 (US/Canada) — full 256-byte bidirectional table
- Code Page 1047 (Latin-1/Open Systems, USS) — full 256-byte bidirectional table
- Code Page 500 (International) — full 256-byte bidirectional table
- Single-byte encode/decode with inline fast path
- String-level encode/decode with UTF-8 support
- Bidirectional roundtrip verified for all printable ASCII

**Packed Decimal (COMP-3):**
- Pack/unpack with sign nibble (0xC positive, 0xD negative, 0xF unsigned)
- Up to 18-digit precision (IBM maximum)
- Decimal point insertion with configurable integer/decimal digits
- Alternate sign nibble recognition (0xA, 0xE for positive; 0xB for negative)

**Zoned Decimal (DISPLAY):**
- Zone/unzone with EBCDIC zone nibbles (0xF zone, sign in last byte)
- Signed and unsigned variants
- Decimal point support
- Storage size calculation

**Binary Integer (COMP/COMP-4):**
- Big-endian two's complement encoding
- Halfword (2 bytes, 1-4 digits), fullword (4 bytes, 5-9 digits), doubleword (8 bytes, 10-18 digits)
- Signed and unsigned variants
- Range validation against PIC clause digits

**Error Handling:**
- 6 typed error variants with `thiserror` and `miette` diagnostics
- No panics in any code path

### What Does NOT Work

- Only 3 EBCDIC code pages (037, 1047, 500) out of dozens commonly used on z/OS
- No COMP-5 (native binary) format — truncates by storage size, not PIC digits
- No COMP-1 (single precision floating point) — IBM Hexadecimal Floating Point (HFP)
- No COMP-2 (double precision floating point) — IBM HFP
- No IEEE 754 binary floating point (BFP) support
- No DBCS (Double-Byte Character Set) mixed encoding with SO/SI shift codes
- No code page registry for runtime code page selection by CCSID
- No Unicode (UTF-16, UTF-32) conversion beyond UTF-8
- No NATIONAL (UTF-16) data type support for COBOL
- No field-level data conversion (PIC clause → encoding pipeline)
- No EBCDIC collation/sorting order support

## Functional Requirements

### FR-v3.0-700: Extended EBCDIC Code Page Library
Add commonly used EBCDIC code pages beyond 037, 1047, and 500. At minimum: CP273 (Germany), CP277 (Denmark/Norway), CP278 (Finland/Sweden), CP280 (Italy), CP284 (Spain), CP285 (UK), CP297 (France), CP871 (Iceland), CP1140-1149 (Euro-enabled variants).
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** CDRA (Character Data Representation Architecture) defines 100+ EBCDIC CCSIDs. Most z/OS installations use national code pages beyond CP037. Euro-enabled variants (CP1140 series) are the modern standard.

### FR-v3.0-701: COMP-5 Native Binary Format
Implement COMP-5 encoding where truncation is based on storage size (2/4/8 bytes) rather than PIC clause digit count. COMP-5 values use the full range of the underlying binary representation.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL for z/OS — COMP-5 is native binary, unaffected by the TRUNC compiler option. Values range from -32768 to 32767 (halfword), -2147483648 to 2147483647 (fullword), etc.

### FR-v3.0-702: IBM Hexadecimal Floating Point (COMP-1/COMP-2)
Implement IBM HFP encoding/decoding for COMP-1 (4-byte single precision) and COMP-2 (8-byte double precision). IBM HFP uses base-16 exponent with a 7-bit biased exponent and 24-bit (COMP-1) or 56-bit (COMP-2) fraction.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** z/Architecture Principles of Operation — Hexadecimal Floating Point format. Enterprise COBOL COMP-1/COMP-2 use HFP by default, not IEEE 754. Conversion between HFP and IEEE BFP requires explicit handling.

### FR-v3.0-703: IEEE Binary Floating Point Support
Implement IEEE 754 single/double/extended precision encoding for interoperability with modern systems. Support conversion between IBM HFP and IEEE BFP formats.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL 6.x supports FLOAT-SHORT, FLOAT-LONG, FLOAT-EXTENDED in IEEE format. ARITH(EXTEND) compiler option enables extended precision.

### FR-v3.0-704: DBCS Mixed Encoding (SO/SI Shift Codes)
Implement DBCS mixed encoding with Shift-Out (0x0E) and Shift-In (0x0F) delimiters for Japanese, Chinese, and Korean EBCDIC code pages (e.g., CP930 Japanese, CP935 Chinese, CP933 Korean).
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** z/OS DBCS uses SO/SI shift codes to delimit transitions between SBCS and DBCS within the same byte stream. Essential for multilingual z/OS environments.

### FR-v3.0-705: Code Page Registry and Runtime Selection
Implement a CCSID-based registry that allows runtime code page selection by numeric ID or name. Support factory methods like `CodePage::from_ccsid(37)` and `CodePage::by_name("IBM-037")`.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** z/OS uses CCSID (Coded Character Set Identifier) for all code page identification. Programs specify code pages by CCSID number in JCL (CODEPAGE compiler option), DB2 (column-level CCSID), and CICS (SYSIDNT).

### FR-v3.0-706: NATIONAL Data Type (UTF-16)
Implement UTF-16 encoding support for the COBOL NATIONAL data type (PIC N). Support conversion between EBCDIC, UTF-8, and UTF-16.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL NATIONAL data items are stored in UTF-16 encoding. Used for Unicode data in modern COBOL programs.

### FR-v3.0-707: Field-Level Data Conversion Pipeline
Implement a PIC-clause-driven conversion pipeline that takes a COBOL field definition (PIC, USAGE, SIGN, DECIMAL-POINT) and produces the appropriate encoder/decoder for that field.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** z/OS data processing frequently requires converting entire records based on COBOL copybook definitions. Tools like DFSORT OUTREC and FILEAID use PIC-based field conversion.

### FR-v3.0-708: EBCDIC Collation Order
Implement EBCDIC collation sequence for sorting and comparison. EBCDIC sorts differently from ASCII (lowercase before uppercase, digits after letters).
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT and COBOL SORT use EBCDIC collation order by default. Programs using PROGRAM COLLATING SEQUENCE IS NATIVE rely on correct EBCDIC ordering.
