# Sort Crate — Architecture Decisions

## AD-3.0-01: External Sort via K-Way Merge

**Context:** The current sort engine loads all records into memory. For production z/OS datasets (millions of records), this is impractical. DFSORT uses disk-based external sort with configurable memory limits.

**Decision:** Implement external sort as a two-phase process: (1) read input in chunks up to a configurable memory limit, sort each chunk in-memory, and write to temporary sorted run files; (2) merge all sorted runs using a k-way merge with a min-heap. The merge phase streams records from all runs simultaneously, writing the final output. The existing in-memory sort is kept for small datasets (auto-detection based on file size vs. memory limit).

**Consequences:**
- Handles arbitrarily large datasets limited only by disk space
- Merge phase uses O(k log k) per record where k is the number of runs
- Temporary files need cleanup (RAII via Drop trait)
- Memory limit is configurable (default: available system memory / 2)
- The MAX_MEMORY_RECORDS constant becomes the actual threshold

## AD-3.0-02: OUTFIL as Post-Sort Output Pipeline

**Context:** OUTFIL enables routing sorted records to multiple output files with per-file filtering, reformatting, and splitting. This is one of DFSORT's most used features for creating different views of data in a single pass.

**Decision:** Implement `OutfilSpec` as a vector of output descriptors, each with its own file path, optional INCLUDE/OMIT filter, optional BUILD/OUTREC specification, and optional SPLIT parameters. After the main sort completes, each output record is passed through all OUTFIL descriptors. Records matching an OUTFIL's filter are reformatted per its BUILD and written to its output file. SPLIT distributes records round-robin across OUTFIL outputs.

**Consequences:**
- Single-pass processing — each sorted record is evaluated against all OUTFILs
- Each OUTFIL can have a completely different record layout
- SAVE option sends unmatched records to a catch-all output
- Memory overhead is minimal (one output buffer per OUTFIL)

## AD-3.0-03: IFTHEN as Pattern-Action Rules

**Context:** IFTHEN clauses allow conditional reformatting — different records processed differently based on their content. WHEN=INIT applies to all records first, WHEN=(condition) applies to matching records, WHEN=NONE is the default fallback, and WHEN=GROUP handles record grouping.

**Decision:** Implement IFTHEN as an ordered list of `IfThenClause` rules, each with a condition (or special variant: INIT/NONE/GROUP) and a BUILD/OVERLAY action. Processing order: (1) apply all WHEN=INIT clauses; (2) for each record, evaluate WHEN=(condition) clauses in order — first match wins; (3) apply WHEN=NONE to unmatched records; (4) WHEN=GROUP maintains a group state machine.

**Consequences:**
- First-match semantics for WHEN=(condition) — order matters
- WHEN=GROUP requires stateful processing (group boundaries tracked across records)
- Can be used in INREC, OUTREC, and OUTFIL contexts
- Replaces simple OUTREC FIELDS for complex reformatting scenarios

## AD-3.0-04: JOINKEYS as Two-File Merge

**Context:** JOINKEYS matches records from two input files (F1 and F2) based on key fields, similar to SQL JOIN. The matched/unmatched records can then be processed with REFORMAT to combine fields from both files.

**Decision:** Implement JOINKEYS as a two-phase operation: (1) each input file is sorted by its join key (or assumed pre-sorted); (2) a merge-join algorithm walks both sorted files simultaneously, matching records by key. The REFORMAT statement defines how matched pairs are combined. JOIN type (INNER, LEFT, RIGHT, FULL) determines handling of unmatched records. A `?` fill character marks missing fields from unmatched sides.

**Consequences:**
- Requires both files sorted by join key (auto-sort if needed)
- Merge-join is O(n + m) for pre-sorted inputs
- REFORMAT can access fields from both F1 and F2
- Unmatched records are handled per JOIN type semantics
- JOINKEYS FIELDS=(pos,len,...) syntax parsed from control statements

## AD-3.0-05: ICETOOL as Batch Operation Orchestrator

**Context:** ICETOOL provides high-level data analysis operations (DISPLAY, OCCUR, STATS, UNIQUE, SELECT) that internally invoke DFSORT with auto-generated control statements. It simplifies common analysis tasks.

**Decision:** Implement ICETOOL as a separate module that orchestrates sort engine operations. Each ICETOOL operator (SORT, COPY, DISPLAY, OCCUR, etc.) is a function that configures and runs the sort engine with appropriate INCLUDE/OMIT, OUTREC, and reporting options. ICETOOL's TOOLIN DD and operator syntax are parsed into a sequence of operations. Each operation generates its own sort pass.

**Consequences:**
- ICETOOL reuses the sort engine — no separate sort implementation
- Multiple operators may require multiple passes over the data
- DISPLAY and OCCUR produce formatted text reports
- STATS computes min/max/avg/sum/count for numeric fields
- The batch interface is simpler for common analytics tasks
