# Encoding Crate — Epics & Stories

## Epic 700: Extended EBCDIC Code Page Library

**Goal:** Expand the code page library to cover the most commonly used EBCDIC code pages on z/OS installations worldwide.

**Crate:** `open-mainframe-encoding`
**FRs:** FR-v3.0-700

### Story 700.1: Euro-Enabled Code Pages (CP1140-1149)

As a **developer processing European z/OS data**,
I want **Euro-enabled EBCDIC code pages (CP1140 through CP1149)**,
So that **I can correctly handle Euro sign (€) in mainframe data**.

**Acceptance Criteria:**

**Given** data encoded in CP1140 (US/Canada with Euro)
**When** byte 0x9F is decoded
**Then** the Euro sign (€, U+20AC) is produced (replacing florin sign in CP037)

**Given** all 10 Euro code pages (CP1140-1149)
**When** roundtrip-tested for all 256 byte values
**Then** encode(decode(byte)) == byte for all values

**Complexity:** M

### Story 700.2: Western European National Code Pages

As a **developer processing multilingual z/OS data**,
I want **national code pages for Western European countries (CP273, CP277, CP278, CP280, CP284, CP285, CP297, CP871)**,
So that **I can handle German, Nordic, Italian, Spanish, UK, French, and Icelandic EBCDIC data**.

**Acceptance Criteria:**

**Given** German data encoded in CP273
**When** EBCDIC bytes for ä, ö, ü, ß, Ä, Ö, Ü are decoded
**Then** the correct Unicode characters are produced

**Given** any supported national code page
**When** all printable ASCII characters are roundtrip-tested
**Then** encode(decode(byte)) == byte for the ASCII subset

**Complexity:** M

### Story 700.3: Code Page Registry with CCSID Lookup

As a **developer**,
I want **a registry that maps CCSID numbers to code page instances**,
So that **I can select the correct code page at runtime by CCSID number or name**.

**Acceptance Criteria:**

**Given** `CodePage::from_ccsid(37)`
**When** the registry is queried
**Then** the CP037 code page is returned

**Given** `CodePage::by_name("IBM-1047")`
**When** the registry is queried
**Then** the CP1047 code page is returned

**Given** an unknown CCSID (99999)
**When** the registry is queried
**Then** an `InvalidCodePage` error is returned

**Complexity:** S

---

## Epic 701: COMP-5 Native Binary Format

**Goal:** Implement native binary encoding where truncation is based on storage size rather than PIC clause digits.

**Crate:** `open-mainframe-encoding`
**FRs:** FR-v3.0-701

### Story 701.1: Native Binary Encoding/Decoding

As a **COBOL data processor**,
I want **COMP-5 encoding that uses the full range of the binary storage size**,
So that **I can correctly read and write COMP-5 fields from mainframe data**.

**Acceptance Criteria:**

**Given** a COMP-5 PIC S9(4) field (halfword) with value 32767
**When** encoded
**Then** the value is accepted (COMP would reject values > 9999)

**Given** a COMP-5 PIC S9(4) field with value 32768
**When** encoded
**Then** an out-of-range error is returned (exceeds halfword capacity)

**Given** COMP-5 encoded bytes [0x7F, 0xFF] (halfword)
**When** decoded
**Then** the value 32767 is returned

**Complexity:** S

---

## Epic 702: IBM Hexadecimal Floating Point (HFP)

**Goal:** Implement IBM HFP encoding for COMP-1 and COMP-2 data types used on z/OS.

**Crate:** `open-mainframe-encoding`
**FRs:** FR-v3.0-702

### Story 702.1: HFP Single Precision (COMP-1)

As a **COBOL data processor**,
I want **IBM HFP single precision (4-byte) encoding and decoding**,
So that **I can correctly read and write COMP-1 fields from z/OS data**.

**Acceptance Criteria:**

**Given** the decimal value 1.0
**When** encoded as IBM HFP single precision
**Then** the bytes are [0x41, 0x10, 0x00, 0x00] (exponent 64+1=65=0x41, fraction 0x100000)

**Given** HFP bytes [0xC1, 0x10, 0x00, 0x00]
**When** decoded
**Then** the value -1.0 is returned (sign bit set)

**Given** HFP zero
**When** encoded
**Then** the bytes are [0x00, 0x00, 0x00, 0x00]

**Complexity:** L

### Story 702.2: HFP Double Precision (COMP-2)

As a **COBOL data processor**,
I want **IBM HFP double precision (8-byte) encoding and decoding**,
So that **I can correctly read and write COMP-2 fields from z/OS data**.

**Acceptance Criteria:**

**Given** a decimal value with 15+ significant digits
**When** encoded as IBM HFP double precision
**Then** the full 56-bit fraction preserves the precision

**Given** known IBM HFP double precision bit patterns from IBM documentation
**When** decoded
**Then** the correct decimal values are produced

**Complexity:** L

### Story 702.3: HFP to IEEE 754 Conversion

As a **developer**,
I want **conversion between IBM HFP and IEEE 754 floating point formats**,
So that **I can interoperate between z/OS and modern system numeric formats**.

**Acceptance Criteria:**

**Given** an IBM HFP COMP-1 value
**When** converted to IEEE 754 f32
**Then** the closest representable f32 value is returned

**Given** an IEEE 754 f64 value
**When** converted to IBM HFP COMP-2
**Then** the closest representable HFP value is returned

**Given** IEEE 754 NaN or Infinity
**When** conversion to HFP is attempted
**Then** an error is returned (HFP has no NaN/Infinity)

**Complexity:** M

---

## Epic 703: IEEE Binary Floating Point Support

**Goal:** Support IEEE 754 binary floating point for modern COBOL FLOAT-SHORT/FLOAT-LONG types.

**Crate:** `open-mainframe-encoding`
**FRs:** FR-v3.0-703

### Story 703.1: IEEE Float Encoding for COBOL

As a **COBOL data processor**,
I want **IEEE 754 encoding for FLOAT-SHORT (4 bytes) and FLOAT-LONG (8 bytes)**,
So that **I can handle modern COBOL numeric types that use IEEE format**.

**Acceptance Criteria:**

**Given** a FLOAT-SHORT field with value 3.14
**When** encoded as IEEE 754 single precision (big-endian for z/OS)
**Then** the correct 4-byte big-endian IEEE representation is produced

**Given** a FLOAT-LONG field
**When** decoded from 8 big-endian bytes
**Then** the correct f64 value is returned

**Complexity:** S

---

## Epic 704: DBCS Mixed Encoding

**Goal:** Implement DBCS mixed EBCDIC encoding with SO/SI shift codes for East Asian languages.

**Crate:** `open-mainframe-encoding`
**FRs:** FR-v3.0-704

### Story 704.1: SO/SI State Machine

As a **developer processing Japanese/Chinese/Korean z/OS data**,
I want **a stateful parser that handles SO (0x0E) / SI (0x0F) shift code transitions**,
So that **I can decode mixed SBCS/DBCS EBCDIC byte streams**.

**Acceptance Criteria:**

**Given** a byte stream with SO at offset 5 and SI at offset 12
**When** decoded
**Then** bytes 0-4 are decoded as SBCS, bytes 6-11 as DBCS pairs, bytes 13+ as SBCS

**Given** a byte stream with unpaired SO (no matching SI)
**When** decoded
**Then** an error is returned indicating invalid encoding

**Complexity:** M

### Story 704.2: Japanese EBCDIC (CP930) Support

As a **developer processing Japanese z/OS data**,
I want **CP930 (Japanese EBCDIC) with SBCS (CCSID 290) and DBCS (CCSID 300) components**,
So that **I can decode Katakana, Hiragana, and Kanji from mainframe data**.

**Acceptance Criteria:**

**Given** Japanese text encoded in CP930 with SO/SI delimiters
**When** decoded
**Then** correct Unicode Katakana, Hiragana, and Kanji characters are produced

**Given** Unicode Japanese text
**When** encoded to CP930
**Then** SBCS halfwidth Katakana is encoded as single bytes; other characters are enclosed in SO/SI with DBCS encoding

**Complexity:** L

---

## Epic 705: NATIONAL Data Type (UTF-16)

**Goal:** Support the COBOL NATIONAL data type stored in UTF-16 encoding.

**Crate:** `open-mainframe-encoding`
**FRs:** FR-v3.0-706

### Story 705.1: UTF-16 Encoding/Decoding

As a **COBOL data processor**,
I want **UTF-16 big-endian encoding and decoding for PIC N fields**,
So that **I can handle COBOL NATIONAL data type values**.

**Acceptance Criteria:**

**Given** a NATIONAL field PIC N(10) containing "HELLO"
**When** encoded to UTF-16 big-endian
**Then** 20 bytes are produced (2 bytes per character, padded with UTF-16 spaces)

**Given** UTF-16 big-endian bytes
**When** decoded
**Then** the correct Unicode string is returned

**Given** EBCDIC source text
**When** converted to NATIONAL
**Then** each EBCDIC character is first decoded to Unicode, then encoded to UTF-16

**Complexity:** M

---

## Epic 706: Field-Level Data Conversion Pipeline

**Goal:** Create a PIC-clause-driven conversion pipeline for automatic field encoding/decoding.

**Crate:** `open-mainframe-encoding`
**FRs:** FR-v3.0-707

### Story 706.1: PIC Clause to Encoder Mapping

As a **developer processing COBOL record data**,
I want **a function that takes a PIC clause string and USAGE and returns the appropriate encoder/decoder**,
So that **I can convert entire records based on COBOL copybook definitions**.

**Acceptance Criteria:**

**Given** PIC S9(5)V99 COMP-3
**When** the field definition is parsed
**Then** a packed decimal encoder with 5 integer digits and 2 decimal digits is returned

**Given** PIC X(30)
**When** the field definition is parsed
**Then** an EBCDIC alphanumeric encoder with length 30 is returned

**Given** PIC S9(9) COMP-5
**When** the field definition is parsed
**Then** a native binary encoder (fullword) is returned

**Complexity:** M

---

## Epic 707: EBCDIC Collation Order

**Goal:** Implement EBCDIC sorting/comparison that matches z/OS native collation.

**Crate:** `open-mainframe-encoding`
**FRs:** FR-v3.0-708

### Story 707.1: Collation Sequence Implementation

As a **developer sorting mainframe data**,
I want **EBCDIC collation order for byte-level comparison**,
So that **sorted output matches what z/OS DFSORT or COBOL SORT would produce**.

**Acceptance Criteria:**

**Given** EBCDIC strings "abc" and "ABC"
**When** compared using EBCDIC collation
**Then** "abc" sorts before "ABC" (lowercase 0x81-0xA9 < uppercase 0xC1-0xE9)

**Given** EBCDIC strings "ABC" and "123"
**When** compared using EBCDIC collation
**Then** "ABC" sorts before "123" (letters 0xC1+ < digits 0xF0+)

**Given** a PROGRAM COLLATING SEQUENCE IS NATIVE context
**When** COBOL SORT is executed
**Then** EBCDIC byte-value ordering is used

**Complexity:** S
