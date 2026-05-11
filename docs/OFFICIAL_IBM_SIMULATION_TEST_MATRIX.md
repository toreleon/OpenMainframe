# Official IBM Simulation Test Matrix

This matrix maps every workspace crate to official IBM documentation and the
minimum simulation tests that should prove the crate behaves like the z/OS or
IBM subsystem it emulates.

Use it as the source of truth for crate-level conformance work:

1. Add or update tests in the owning crate.
2. Name the IBM topic, command, API, or data format being simulated.
3. Link the official IBM document used to define the expected behavior.
4. Prefer observable behavior tests over implementation-shape tests.

Run `scripts/check-official-ibm-test-matrix.sh` after editing this file. The
checker verifies that every workspace crate appears here and that the matrix
contains official IBM documentation links.

## Coverage States

| State | Meaning |
| --- | --- |
| Planned | IBM reference identified, tests still need to be added or expanded. |
| Partial | Some behavior is covered, but the crate is not yet broadly conformance-tested. |
| Covered | Core public simulation behavior is covered against the cited IBM reference. |
| N/A | Support crate where conformance is inherited from consumers; still needs invariants. |

## Official IBM References

These IBM documentation entry points should be preferred over secondary sources.
Pin to a specific IBM product/version page when a test depends on versioned
syntax or response fields.

| Domain | Official IBM reference |
| --- | --- |
| z/OSMF REST APIs | [Using the z/OSMF REST services](https://www.ibm.com/docs/en/zos/2.5.0?topic=guide-using-zosmf-rest-services) |
| JCL and JES statements | [z/OS MVS JCL Reference](https://www.ibm.com/docs/en/zos/3.2.0?topic=mvs-zos-jcl-reference) |
| JES2 | [z/OS JES2](https://www.ibm.com/docs/en/zos/2.5.0?topic=zos-jes2) |
| MVS system commands | [MVS system commands reference](https://www.ibm.com/docs/en/zos/2.5.0?topic=commands-mvs-system-reference) |
| DFSMS and data sets | [z/OS Data Sets, Access Methods, and DFSMS Overview](https://www.ibm.com/support/pages/zos-data-sets-access-methods-and-dfsms-overview) |
| COBOL | [Enterprise COBOL for z/OS documentation library](https://www.ibm.com/support/pages/enterprise-cobol-zos-documentation-library) |
| Language Environment | [z/OS Language Environment](https://www.ibm.com/docs/en/zos/3.1.0?topic=zos-language-environment) |
| CICS | [CICS TS reference information](https://www.ibm.com/docs/en/cics-ts/5.5.0?topic=reference) |
| Db2 for z/OS | [Programming for Db2 for z/OS](https://www.ibm.com/docs/en/db2-for-zos/12?topic=programming-db2-zos) |
| IMS DL/I | [DL/I calls for IMS DB system services](https://www.ibm.com/docs/en/ims/15.4.0?topic=management-dli-calls-ims-db-system-services) |
| RACF | [z/OS Security Server](https://www.ibm.com/docs/en/zos/2.5.0?topic=descriptions-security-server) |
| DFSORT | [z/OS DFSORT](https://www.ibm.com/docs/en/zos/2.5.0?topic=zos-dfsort) |
| TSO/E REXX | [z/OS TSO/E REXX Reference](https://www.ibm.com/docs/en/zos/2.5.0?topic=tsoe-zos-rexx-reference) |
| TSO/E and console commands | [System command reference](https://www.ibm.com/docs/en/zos/2.4.0?topic=console-system-command-reference) |
| ISPF | [What is ISPF?](https://www.ibm.com/docs/en/zos-basic-skills?topic=interfaces-what-is-ispf) |
| HLASM | [The Assembler language on z/OS](https://www.ibm.com/docs/en/zos-basic-skills?topic=zos-assembler-language) |
| HLASM options | [Assembler options](https://www.ibm.com/docs/en/hla-and-tf/1.6.0?topic=information-assembler-options) |
| IBM MQ | [IBM MQ documentation](https://www.ibm.com/docs/en/ibm-mq/9.4.x?topic=mq) |
| MQ for z/OS apps | [Using and writing applications on IBM MQ for z/OS](https://www.ibm.com/docs/en/ibm-mq/9.3.x?topic=queuing-using-writing-applications-mq-zos) |
| z/OS UNIX | [Introduction to z/OS UNIX](https://www.ibm.com/docs/SSLTBW_3.1.0/com.ibm.zos.v3r1.bpxb200/int.htm) |
| WLM | [How it works: z/OS Workload Manager](https://www.ibm.com/docs/SSGMCP_6.2.0/fundamentals/wlm/zos-wlm.html) |
| Program management | [z/OS Program Management components](https://www.ibm.com/docs/en/zos/3.2.0?topic=introduction-zos-program-management-components) |
| ICSF cryptographic services | [Getting started with ICSF](https://www.ibm.com/docs/en/effz/1.2.0?topic=started-getting-icsf) |
| PL/I | [IBM Enterprise PL/I for z/OS](https://www.ibm.com/docs/en/SSY2V3_6.2/pdf/pl_i_zos_6_2_licensed_prog_spec.pdf) |

## Crate Matrix

| Crate | IBM behavior reference | Minimum conformance tests | State |
| --- | --- | --- | --- |
| `open-mainframe-lang-core` | COBOL, JCL, HLASM, and diagnostics references above | Span math, diagnostic severity, and source-location rendering used by language crates | Planned |
| `open-mainframe-encoding` | DFSMS/data sets; COBOL data representation | EBCDIC round trips for supported CCSIDs; packed/zoned decimal edge cases; invalid byte handling | Planned |
| `open-mainframe-runtime` | Language Environment; COBOL runtime | LE condition handling, program call linkage, storage lifetime, decimal/string runtime helpers | Planned |
| `open-mainframe-cobol` | Enterprise COBOL for z/OS documentation library | Compiler option parsing, fixed/free source rules, COPY/REPLACE, intrinsic functions, JSON/XML, arithmetic, OCCURS, REDEFINES | Partial |
| `open-mainframe-jcl` | z/OS MVS JCL Reference; JES2 | JOB/EXEC/DD syntax, PROC expansion, IF/THEN/ELSE, COND, GDG relative references, utility invocation | Partial |
| `open-mainframe-rexx` | z/OS TSO/E REXX Reference | Expression evaluation, parsing rules, stem variables, built-ins, SIGNAL/CALL, host command routing | Planned |
| `open-mainframe-hlasm` | HLASM language and assembler options | Machine/assembler instruction parsing, macro expansion, conditional assembly, object/listing outputs | Planned |
| `open-mainframe-pli` | IBM Enterprise PL/I for z/OS | Declarations, procedure calls, condition handling, string/numeric conversions | Planned |
| `open-mainframe-clist` | TSO/E command procedures and REXX/TSO references | PROC/CONTROL/SET, variable substitution, command routing, TSO bridge behavior | Partial |
| `open-mainframe-easytrieve` | IBM Easytrieve behavior where IBM docs are available; JCL and DFSORT for execution context | FILE/JOB/SORT/REPORT parsing and generated report layout invariants | Planned |
| `open-mainframe-natural` | IBM z/OS data set and terminal behavior where Natural-compatible behavior is simulated | Program parsing, map I/O, database-adapter boundaries | Planned |
| `open-mainframe-focus` | z/OS data set behavior and IBM terminal/report conventions | DEFINE, TABLE, JOIN, FILEDEF, report formatting, parser error cases | Partial |
| `open-mainframe-precompilers` | CICS TS reference; Db2 for z/OS programming | EXEC CICS and EXEC SQL transformations, host-variable metadata, line mapping and diagnostics | Planned |
| `open-mainframe-dataset` | DFSMS/data sets | Sequential/PDS/PDSE/VSAM semantics, member naming, DISP locks, catalog operations, GDG generations | Partial |
| `open-mainframe-db2` | Programming for Db2 for z/OS | SQL parsing/precompile, host variables, SQLCODE/SQLSTATE behavior, cursor lifecycle | Planned |
| `open-mainframe-ims` | IMS DL/I calls | GU/GN/GNP/ISRT/REPL/DLET behavior, PCB status codes, SSA qualification | Planned |
| `open-mainframe-idms` | IBM mainframe database transaction and storage conventions | Record storage, set navigation, currency indicators, locking behavior | Partial |
| `open-mainframe-adabas` | IBM z/OS data set and transaction conventions where ADABAS-compatible behavior is simulated | Descriptor lookup, inverted-list behavior, CRUD record semantics, transaction boundaries | Planned |
| `open-mainframe-sort` | z/OS DFSORT | SORT/MERGE/COPY, INCLUDE/OMIT, INREC/OUTREC, key collation, stable record handling | Planned |
| `open-mainframe-cics` | CICS TS reference information | EXEC CICS command response codes, EIB fields, LINK/XCTL/RETURN, BMS SEND/RECEIVE MAP, file/queue commands | Partial |
| `open-mainframe-jes2` | z/OS JES2 | Job lifecycle, class/priority, spool files, return codes, command handling | Planned |
| `open-mainframe-racf` | z/OS Security Server RACF | User/group/resource profiles, access checks, command parsing, SAF return/reason codes | Partial |
| `open-mainframe-tso` | TSO/E REXX Reference; system command reference | Session lifecycle, command parsing, ALLOC/FREE/SUBMIT/STATUS, host command responses | Partial |
| `open-mainframe-ispf` | ISPF overview and services documentation | Panel rendering, variable pools, table services, edit/view command behavior | Planned |
| `open-mainframe-mq` | IBM MQ documentation; MQ for z/OS apps | MQOPEN/MQPUT/MQGET/MQCLOSE, queue attributes, persistence flags, return/reason codes | Planned |
| `open-mainframe-mvs` | MVS system commands reference | WTO/WTOR, ENQ/DEQ, DYNALLOC, SVC dispatch, system command side effects | Partial |
| `open-mainframe-wlm` | z/OS Workload Manager | Service/report class classification, goal evaluation, policy activation, metric reporting | Planned |
| `open-mainframe-smf` | z/OS system management and RACF/SMF references | SMF record headers, timestamps, record typing, serialization/deserialization | Planned |
| `open-mainframe-uss` | Introduction to z/OS UNIX | Path handling, permissions, process/file operations, dataset-to-USS mount semantics | Planned |
| `open-mainframe-utilities` | z/OS utilities, DFSORT, DFSMS, JCL | IEBGENER, IEBCOPY, IEBUPDTE, IDCAMS, SORT utility contracts and return codes | Partial |
| `open-mainframe-syscmd` | MVS system commands reference; JES2 commands | DISPLAY/START/STOP parsing, target subsystem routing, operator response text | Planned |
| `open-mainframe-pgmmgmt` | z/OS Program Management components | Binder symbol resolution, loader lookup, module attributes, load failure diagnostics | Planned |
| `open-mainframe-networking` | z/OS UNIX and IBM Communications Server behavior where simulated | TCP/IP service behavior, FTP command contracts, VTAM/SNA session state | Planned |
| `open-mainframe-crypto` | ICSF cryptographic services | Key generation/import/export, token metadata, callable-service return/reason code mapping | Planned |
| `open-mainframe-parmlib` | z/OS initialization and MVS command references | PARMLIB member parsing, IEASYS-style override resolution, invalid parameter diagnostics | Planned |
| `open-mainframe-zosmf` | z/OSMF REST services | REST paths, headers, auth cookie/JWT behavior, status codes, JSON fields for datasets/jobs/console/TSO/CICS | Partial |
| `open-mainframe-assess` | IBM language and subsystem docs above | Assessment classifications trace back to documented language/subsystem constructs | Planned |
| `open-mainframe-deploy` | z/OSMF and deployment-adjacent IBM docs where compatibility is claimed | Generated config validity, endpoint exposure, health/readiness contracts | Planned |
| `open-mainframe-tui` | ISPF, 3270, and CICS terminal references | 3270 key mapping, screen dimensions, field attributes, snapshot rendering | Partial |
| `open-mainframe-drda` | Db2 for z/OS programming and DRDA-compatible behavior | Handshake, SQL request/response framing, error propagation | Planned |
| `open-mainframe-gym` | Conformance harness docs in this file | Scenario fixtures, scoring, reproducibility, subsystem coverage aggregation | Planned |
| `open-mainframe-symbolic` | Equivalence strategy docs and language references | Path constraints, branch coverage accounting, counterexample rendering | Planned |
| `open-mainframe-wiki` | Official IBM references indexed in this file | Documentation ingestion, search/index correctness, stale-link detection | Planned |
| `open-mainframe` | Integrated behavior from all subsystem references | CLI/headless startup, configured app loading, z/OSMF bridge smoke flow | Planned |

## Test Naming Convention

Use names that expose the IBM behavior being checked:

```rust
#[test]
fn ibm_jcl_cond_true_bypasses_step() {
    // ...
}
```

For integration tests, include the subsystem and observable contract:

```rust
#[tokio::test]
async fn zosmf_datasets_list_returns_x_ibm_response_rows() {
    // ...
}
```

## Suggested Execution Loop

1. Pick one crate and one row from this matrix.
2. Open the cited IBM reference and identify a single normative rule.
3. Add the smallest test that fails if OpenMainframe diverges from that rule.
4. If the implementation is wrong, fix it in the crate that owns the behavior.
5. Update the row state only after the test lands.
6. Run `cargo test -p <crate>` and `scripts/check-official-ibm-test-matrix.sh`.

