//! System services reference page generators.

use std::fs;

use crate::{WikiConfig, WikiResult};

/// Generate all system services reference pages.
pub fn generate_system_pages(config: &WikiConfig) -> WikiResult<()> {
    let out = config.output_dir.join("system");
    fs::write(out.join("index.md"), system_index())?;
    fs::write(out.join("parmlib.md"), parmlib_page())?;
    fs::write(out.join("utilities.md"), utilities_page())?;
    fs::write(out.join("sort.md"), sort_page())?;
    fs::write(out.join("pgmmgmt.md"), pgmmgmt_page())?;
    fs::write(out.join("crypto.md"), crypto_page())?;
    fs::write(out.join("syscmd.md"), syscmd_page())?;
    fs::write(out.join("drda.md"), drda_page())?;
    Ok(())
}

fn system_index() -> &'static str {
    r#"# System Services

z/OS provides a comprehensive set of system services that manage program execution,
data transformation, cryptographic operations, and operator interaction. These services
form the operational backbone of any mainframe installation.

## Service Categories

### Configuration & Initialization
- **[PARMLIB Reference](parmlib.md)** -- System parameter library members that control
  z/OS initialization, subsystem configuration, and runtime behavior.

### Utilities
- **[z/OS Utilities](utilities.md)** -- Standard IBM utilities for dataset copy, compare,
  update, VSAM management, and data manipulation (IEBCOPY, IEBGENER, IDCAMS, etc.).
- **[DFSORT Reference](sort.md)** -- Sort/merge/copy utility with INCLUDE/OMIT filtering,
  INREC/OUTREC formatting, IFTHEN logic, JOINKEYS, and ICETool operators.

### Program Management
- **[Program Management](pgmmgmt.md)** -- Load module search order, link-editing (Binder),
  APF authorization, LPA, AMODE/RMODE, and the Program Properties Table.

### Security & Cryptography
- **[ICSF Cryptographic Services](crypto.md)** -- Symmetric/asymmetric encryption, key
  management, hardware acceleration, and RACF integration for cryptographic resources.

### Operations
- **[Operator Commands](syscmd.md)** -- MVS console commands: DISPLAY, START, STOP, MODIFY,
  CANCEL, VARY, REPLY, FORCE, and SDSF usage.

### Connectivity
- **[DRDA Wire Protocol](drda.md)** -- Distributed Relational Database Architecture for
  DB2 connectivity: DSS framing, DDM objects, code points, and SQL type mapping.
"#
}

fn parmlib_page() -> &'static str {
    r#"# PARMLIB Reference

PARMLIB (`SYS1.PARMLIB`) is the central configuration library for z/OS. It contains
members that control system initialization, subsystem behavior, and runtime parameters.
Each member uses a two-character suffix (e.g., `IEASYSxx`) allowing multiple
configurations to coexist.

## Key Members

### IEASYSxx -- System Initialization Parameters

The master system parameter member read during IPL. It specifies or points to most
other PARMLIB members.

| Parameter | Description | Example |
|-----------|-------------|---------|
| ALLOC | Allocation defaults suffix | `ALLOC=00` |
| APF | APF list suffix | `APF=00` |
| CMD | Auto-command suffix | `CMD=00` |
| CONSOLE | Console configuration suffix | `CONSOLE=00` |
| COUPLE | Couple dataset name | `COUPLE=SYS1.COUPLE` |
| CSA | Common storage size | `CSA=(4096,8192)` |
| DUMP | Dump options suffix | `DUMP=00` |
| FIX | Fixed LPA suffix | `FIX=00` |
| GRS | Global resource serialization | `GRS=STAR` |
| LNK | Linklist suffix | `LNK=00` |
| LPA | LPA suffix | `LPA=00` |
| LNKLST | Linklist suffix (alternative) | `LNKLST=00` |
| MSTRJCL | Master JCL suffix | `MSTRJCL=00` |
| PROG | Program properties suffix | `PROG=00` |
| RDE | Resource definition suffix | `RDE=00` |
| RSU | Recommended Service Upgrade | `RSU=2408` |
| SMF | SMF parameters suffix | `SMF=00` |
| SQA | System queue area size | `SQA=(4096,8192)` |
| SSN | Subsystem name suffix | `SSN=00` |
| VAL | Value list suffix | `VAL=00` |

### LNKLSTxx -- Linklist Concatenation

Defines the set of libraries searched (after LPA) for load modules. Modules in the
linklist are available system-wide without STEPLIB/JOBLIB.

```
LNKLST00:
  SYS1.LINKLIB
  SYS1.MIGLIB
  SYS1.CSSLIB
  ISP.SISPLOAD
  CEE.SCEERUN
  CBC.SCLBDLL
```

### PROGxx -- APF, Exits, and LPA

Replaces older IEAAPFxx and IEALPAxx members. Controls:

| Statement | Purpose | Example |
|-----------|---------|---------|
| APF ADD | Add library to APF list | `APF ADD DSNAME(MY.APF.LOAD) VOLUME(VOL001)` |
| APF ADD | SMS-managed APF entry | `APF ADD DSNAME(MY.APF.LOAD) SMS` |
| EXIT ADD | Define installation exit | `EXIT ADD EXITNAME(IEFUJI) MODNAME(MYEXIT)` |
| LPA ADD | Add module to dynamic LPA | `LPA ADD MODNAME(MYPROG) DSNAME(MY.LPA.LOAD)` |
| LNKLST ADD | Add to LNKLST | `LNKLST ADD NAME(LNKLST00) DSNAME(MY.LOAD)` |

### CONSOLxx -- Console Configuration

Defines MCS, SMCS, and subsystem consoles.

| Parameter | Description |
|-----------|-------------|
| CONSOLE | Console name and device address |
| DEFAULT | Default console attributes |
| HARDCOPY | Hardcopy log destination |
| INIT | Console initialization options |
| INTIDS | Internal console identifiers |
| ROUTCODE | Default routing codes |

### COMMNDxx -- Automatic Commands

Commands issued automatically during IPL after the master scheduler initializes.

```
COM='START JES2,,,WARM'
COM='START VTAM'
COM='START TSO'
COM='START TCPIP'
COM='S RMF,MEMBER(00)'
COM='S SDSF'
```

### IEAFIXxx -- Fixed LPA Modules

Lists modules to be loaded into fixed (non-pageable) storage in LPA for performance.

```
IEAVNP01
IEAVNP02
IGC0001A
```

### IKJTSOxx -- TSO/E Configuration

| Section | Purpose |
|---------|---------|
| AUTHCMD | Authorized TSO commands (LISTD, LISTC, etc.) |
| AUTHPGM | Authorized programs callable from TSO |
| AUTHTSF | Authorized programs under TMP |
| NOTBKGND | Commands not allowed in background |
| PLATCMD | Platform commands |
| PLATPGM | Platform programs |
| SEND | SEND command defaults |
| ALLOCATE | Default allocation parameters |
| HELP | HELP dataset names |
| LOGON | Logon defaults |
| TRANSMIT | XMIT defaults |
| RECEIVE | RECEIVE defaults |

### SMFPRMxx -- SMF Configuration

| Parameter | Description | Example |
|-----------|-------------|---------|
| ACTIVE | Activate SMF recording | `ACTIVE` |
| DSNAME | SMF dataset name | `DSNAME(SYS1.MANx)` |
| NOPROMPT | Suppress IPL prompts | `NOPROMPT` |
| REC | Record types to collect | `REC(TYPE(0:255))` |
| SYS | System type recording | `SYS(TYPE(0:255))` |
| INTVAL | Interval recording | `INTVAL(30)` |
| SYNCVAL | Sync interval | `SYNCVAL(00)` |
| MAXDORM | Max dormant time | `MAXDORM(3000)` |
| BUFUSEWARN | Buffer warning threshold | `BUFUSEWARN(50)` |
| STATUS | Write status records | `STATUS(010000)` |
| JWT | Job wait time | `JWT(0)` |
| SID | System identifier | `SID(SYS1)` |

### JES2PARM -- JES2 Initialization

| Statement | Description | Example |
|-----------|-------------|---------|
| JOBCLASS | Job class definition | `JOBCLASS(A) PGMRSEL=NO,PROCLIB=00` |
| OUTCLASS | Output class definition | `OUTCLASS(A) OUTPUT=PRINT` |
| INIT | Initiator definition | `INIT(1-5) CLASS=A,B,C` |
| PRINTDEF | Print defaults | `PRINTDEF PORTNO=0` |
| SPOOLDEF | Spool definition | `SPOOLDEF TGSPACE=(MAX=5000)` |
| DESTDEF | Destination definition | `DESTDEF DESTID=LOCAL` |
| PROCLIB | PROC library | `PROCLIB(00) DD(1)=SYS1.PROCLIB` |
| ESTLNCT | Lines per page | `ESTLNCT=60` |
| ESTPAGE | Estimated pages | `ESTPAGE=9999` |
| MASDEF | MAS definition | `MASDEF HOLD=NO` |
| DESTID | Destination IDs | `DESTID(name)` |
| SUBTDEF | Submit defaults | `SUBTDEF CNVT=YES` |

## Symbol Substitution

PARMLIB members support system symbols that are resolved at IPL time.

| Symbol | Description | Example Value |
|--------|-------------|---------------|
| `&SYSNAME` | System name | `SYS1` |
| `&SYSPLEX` | Sysplex name | `PLEX1` |
| `&SYSCLONE` | System clone ID (2 chars) | `S1` |
| `&SYSALVL` | System availability level | `2` |
| `&SYSR1` | IPL volume serial | `RES001` |
| `&LPARNAME` | LPAR name | `LP01` |
| `&VMID` | VM guest ID | `VM01` |

Usage in PARMLIB:
```
COM='START JES2.&SYSNAME'
COM='S TCPIP,,,PROFILE=TCPPARMS(&SYSNAME)'
```

## System Parameter Override

During IPL, the operator can override PARMLIB settings.

### Suffix Selection
The system reads `LOAD xx` from the operator or hardware configuration to determine
which `LOADxx` member to use. `LOADxx` in turn specifies IODF and NUCLSTxx.

```
LOAD 00 → LOAD00 member → specifies IODF and NUCLST00
         IEASYS=(00,01)  → merge IEASYSxx members
```

### IEASYS Override
```
IEASYS=(00,01)    Merge IEASYS00 and IEASYS01 (01 overrides)
IEASYS=(00)       Use only IEASYS00
SYSP=(xx,yy)      Shorthand on operator console
```

### LOADxx and NUCLSTxx

`LOADxx` specifies:
- IODF (I/O Definition File) name and suffix
- NUCLST suffix for nucleus module selection
- IEASYS suffixes

`NUCLSTxx` defines the set of nucleus load modules (IEANUC0x) to be loaded during IPL.

```
LOADxx:
  IODF   00 SYS1.IODF00
  NUCLST 00
  IEASYS (00,01)

NUCLSTxx:
  IEANUC01
  IEANUC02
  IEANUC03
```
"#
}

fn utilities_page() -> &'static str {
    r#"# z/OS Utilities Reference

IBM-supplied utility programs for dataset management, data manipulation, and system
maintenance. Each utility is invoked via JCL and produces a condition code indicating
success or failure.

## IEBCOPY -- PDS Copy/Compress

Copies, compresses, or merges partitioned datasets.

### JCL Template
```jcl
//COPY    EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INDD    DD DSN=INPUT.PDS,DISP=SHR
//OUTDD   DD DSN=OUTPUT.PDS,DISP=OLD
//SYSIN   DD *
  COPY INDD=INDD,OUTDD=OUTDD
/*
```

### Control Statements
| Statement | Purpose | Syntax |
|-----------|---------|--------|
| COPY | Copy/compress operation | `COPY INDD=dd,OUTDD=dd` |
| SELECT | Select specific members | `SELECT MEMBER=(name1,name2,...)` |
| EXCLUDE | Exclude specific members | `EXCLUDE MEMBER=(name1,name2,...)` |
| REPLACE | Replace existing members | Add `REPLACE=YES` to SELECT |
| ALTERMOD | Modify SSI info | `ALTERMOD MEMBER=name` |

### Compress In-Place
```jcl
//COMPRESS EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=MY.PDS,DISP=OLD
//SYSIN    DD *
  COPY INDD=SYSUT1,OUTDD=SYSUT1
/*
```

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | Successful completion |
| 4 | Warning (e.g., member already exists) |
| 8 | Error (e.g., member not found) |
| 12 | Severe error |
| 16 | Terminal error |

---

## IEBGENER -- Sequential Dataset Copy

Copies sequential datasets or creates PDS members from sequential input. Can also
perform simple data reformatting.

### JCL Template
```jcl
//GENER   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=INPUT.FILE,DISP=SHR
//SYSUT2   DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG),
//            SPACE=(TRK,(5,5)),DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//SYSIN    DD DUMMY
```

### Control Statements
| Statement | Purpose | Syntax |
|-----------|---------|--------|
| GENERATE | Define generation parameters | `GENERATE MAXFLDS=n,MAXLITS=n` |
| RECORD | Define record processing | `RECORD FIELD=(len,input_pos,,output_pos)` |
| MEMBER | Create PDS member | `MEMBER NAME=membername` |
| FIELD | Field reformatting | `FIELD=(length,input,conversion,output)` |

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | Successful |
| 4 | Alternate processing used |
| 8 | Unable to process |
| 12 | Unrecoverable error |
| 16 | Invalid user exit |

---

## IEBCOMPR -- Dataset Comparison

Compares two sequential or partitioned datasets record by record.

### JCL Template
```jcl
//COMPARE EXEC PGM=IEBCOMPR
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=FIRST.FILE,DISP=SHR
//SYSUT2   DD DSN=SECOND.FILE,DISP=SHR
//SYSIN    DD *
  COMPARE TYPORG=PS
/*
```

### Control Statements
| Statement | Purpose |
|-----------|---------|
| COMPARE | Specify comparison type: `TYPORG=PS` (sequential) or `TYPORG=PO` (partitioned) |
| EXITS | Specify user exit routines |

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | Datasets are identical |
| 8 | Datasets differ |
| 12 | Unrecoverable error |
| 16 | Invalid user exit |

---

## IEBUPDTE -- Source Library Update

Updates or creates members of a PDS using control statements. Commonly used for
maintaining source libraries.

### JCL Template
```jcl
//UPDATE  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD DSN=MY.PDS,DISP=OLD
//SYSIN    DD DATA
./ ADD NAME=MEMBER1
Line 1 of member
Line 2 of member
./ ENDUP
/*
```

### Control Statements
| Statement | Purpose | Syntax |
|-----------|---------|--------|
| `./ ADD` | Add new member | `./ ADD NAME=membername` |
| `./ REPL` | Replace existing member | `./ REPL NAME=membername` |
| `./ CHANGE` | Modify records in member | `./ CHANGE NAME=membername` |
| `./ NUMBER` | Renumber sequence fields | `./ NUMBER NEW1=10,INCR=10` |
| `./ DELETE` | Delete sequence range | `./ DELETE SEQ1=000100,SEQ2=000200` |
| `./ ALIAS` | Create alias | `./ ALIAS NAME=aliasname` |
| `./ ENDUP` | End of input | `./ ENDUP` |

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | Successful |
| 4 | Warning |
| 8 | Error processing input |
| 12 | Unrecoverable error |
| 16 | Invalid user exit |

---

## IEBDG -- Test Data Generator

Generates test data patterns for sequential and partitioned datasets.

### JCL Template
```jcl
//DATAGEN EXEC PGM=IEBDG
//SYSPRINT DD SYSOUT=*
//SEQOUT   DD DSN=TEST.DATA,DISP=(NEW,CATLG),
//            SPACE=(TRK,(5,5)),DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//SYSIN    DD *
  DSD OUTPUT=(SEQOUT)
  FD NAME=FIELD1,LENGTH=10,FORMAT=AL,ACTION=TL
  FD NAME=FIELD2,LENGTH=5,FORMAT=ZD,ACTION=RP
  CREATE QUANTITY=100,NAME=(FIELD1,FIELD2),FILL=X'40'
  END
/*
```

### Control Statements
| Statement | Purpose |
|-----------|---------|
| DSD | Define dataset (OUTPUT/INPUT dd names) |
| FD | Field definition (NAME, LENGTH, FORMAT, ACTION, STARTLOC) |
| CREATE | Create records (QUANTITY, NAME list, FILL character) |
| REPEAT | Repeat a set of CREATE operations |
| COPY | Copy from input to output |
| END | End of control statements |

### FD Formats
| Format | Description |
|--------|-------------|
| AL | Alphabetic (A-Z, rotating) |
| AN | Alphanumeric |
| ZD | Zoned decimal |
| PD | Packed decimal |
| BI | Binary |
| CO | Collating sequence |
| RA | Random |

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | Successful |
| 4 | Warning |
| 8 | Error |
| 12 | Unrecoverable error |

---

## IEBPTPCH -- Print/Punch Utility

Prints or punches records from sequential or partitioned datasets.

### JCL Template
```jcl
//PRINT   EXEC PGM=IEBPTPCH
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=INPUT.FILE,DISP=SHR
//SYSUT2   DD SYSOUT=*
//SYSIN    DD *
  PRINT TYPORG=PS,MAXFLDS=3
  TITLE ITEM=('REPORT HEADER',20)
  RECORD FIELD=(80)
/*
```

### Control Statements
| Statement | Purpose | Syntax |
|-----------|---------|--------|
| PRINT | Print operation | `PRINT TYPORG=PS\|PO,MAXFLDS=n,MAXNAME=n` |
| PUNCH | Punch operation | `PUNCH TYPORG=PS\|PO` |
| TITLE | Title line | `TITLE ITEM=('text',col)` |
| RECORD | Record definition | `RECORD FIELD=(len,pos,conv,col)` |
| MEMBER | PDS member selection | `MEMBER NAME=name` |
| LABELS | Label processing | `LABELS DATA=YES\|NO` |

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | Successful |
| 4 | Warning |
| 8 | Error |
| 12 | Unrecoverable error |

---

## AMASPZAP -- SuperZap Utility

Modifies load modules or datasets at the byte level. Used for emergency fixes (zaps).

### JCL Template
```jcl
//ZAP     EXEC PGM=AMASPZAP
//SYSPRINT DD SYSOUT=*
//SYSLIB   DD DSN=MY.LOADLIB,DISP=OLD
//SYSIN    DD *
  NAME MYPROG MYPROG
  VER  0A00 47F0,C008
  REP  0A00 47F0,C010
  DUMPT MYPROG
/*
```

### Control Statements
| Statement | Purpose | Syntax |
|-----------|---------|--------|
| NAME | Identify module | `NAME csect loadmod` |
| VER | Verify current content | `VER offset hex_data` |
| REP | Replace with new content | `REP offset hex_data` |
| DUMPT | Dump module contents | `DUMPT csect_name` |
| CCHHR | Direct access address | `CCHHR cchhr` |

### CSI Format (for VSAM)
```jcl
//ZAP     EXEC PGM=AMASPZAP
//SYSPRINT DD SYSOUT=*
//SYSLIB   DD DSN=VSAM.CLUSTER,DISP=OLD,
//            AMP=('BUFSP=65536')
//SYSIN    DD *
  CCHHR 0000000100
  VER   00 C1C2C3C4
  REP   00 E7E8E9F0
/*
```

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | All VER/REP successful |
| 4 | At least one VER failed (no REP applied) |
| 8 | Severe error |

---

## IDCAMS -- Access Method Services

The primary utility for VSAM catalog management and dataset operations.

### DEFINE CLUSTER -- Create VSAM Dataset
```
DEFINE CLUSTER (                        -
    NAME(MY.VSAM.KSDS)                  -
    INDEXED                             -
    RECORDS(10000 5000)                  -
    RECORDSIZE(200 500)                  -
    KEYS(11 0)                           -
    FREESPACE(20 10)                     -
    SHAREOPTIONS(2 3)                    -
    SPEED                                -
  ) DATA (                              -
    NAME(MY.VSAM.KSDS.DATA)             -
    CISZ(4096)                           -
    VOLUMES(VOL001)                      -
  ) INDEX (                             -
    NAME(MY.VSAM.KSDS.INDEX)            -
    CISZ(2048)                           -
    VOLUMES(VOL001)                      -
  ) CATALOG(MY.UCAT)
```

### DEFINE AIX -- Alternate Index
```
DEFINE AIX (                            -
    NAME(MY.VSAM.AIX)                   -
    RELATE(MY.VSAM.KSDS)                -
    KEYS(8 20)                           -
    RECORDSIZE(50 100)                   -
    UNIQUEKEY                            -
  ) DATA (                              -
    NAME(MY.VSAM.AIX.DATA)              -
  ) INDEX (                             -
    NAME(MY.VSAM.AIX.INDEX)             -
  )
DEFINE PATH (                           -
    NAME(MY.VSAM.PATH)                  -
    PATHENTRY(MY.VSAM.AIX)              -
  )
BLDINDEX INDATASET(MY.VSAM.KSDS)       -
         OUTDATASET(MY.VSAM.AIX)
```

### REPRO -- Copy/Load Data
```
REPRO INFILE(INDD) OUTFILE(OUTDD)
REPRO INDATASET(OLD.VSAM) OUTDATASET(NEW.VSAM) REPLACE
REPRO INFILE(SEQIN) OUTDATASET(MY.VSAM.KSDS)
```

### DELETE -- Remove Datasets
```
DELETE MY.VSAM.KSDS CLUSTER PURGE
DELETE MY.OLD.FILE NONVSAM
DELETE MY.GDG.BASE GDG FORCE
```

### LISTCAT -- Catalog Listing
```
LISTCAT ENTRIES(MY.VSAM.KSDS) ALL
LISTCAT LEVEL(MY.PROJECT) VOLUME
LISTCAT CATALOG(MY.UCAT) ALL
```

### ALTER -- Modify Attributes
```
ALTER MY.VSAM.KSDS FREESPACE(30 15)
ALTER MY.VSAM.KSDS BUFFERSPACE(65536)
ALTER MY.VSAM.KSDS SHAREOPTIONS(3 3)
```

### PRINT -- Print Dataset Contents
```
PRINT INDATASET(MY.VSAM.KSDS) CHARACTER COUNT(100)
PRINT INDATASET(MY.VSAM.KSDS) HEX FROMKEY(X'F0F0F0')
PRINT INFILE(INDD) DUMP
```

### EXPORT/IMPORT -- Portable Copy
```
EXPORT MY.VSAM.KSDS OUTFILE(EXPDD) TEMPORARY
IMPORT INFILE(IMPDD) OUTDATASET(MY.VSAM.NEW)
```

### VERIFY -- Reset End-of-File
```
VERIFY DATASET(MY.VSAM.KSDS)
```

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | All functions successful |
| 4 | Warning (some functions had warnings) |
| 8 | Error in at least one function |
| 12 | Severe error -- function not attempted |
| 16 | Catastrophic error |

### Conditional Execution
```
IF LASTCC = 0 THEN DO
  REPRO INDATASET(A) OUTDATASET(B)
END
IF MAXCC <= 4 THEN -
  LISTCAT ALL
SET MAXCC = 0
SET LASTCC = 0
```

---

## IEFBR14 -- Do-Nothing Program

A program that does nothing and returns RC=0. Used solely for DD statement
processing (allocating, deleting, or cataloging datasets via JCL).

```jcl
//ALLOC   EXEC PGM=IEFBR14
//NEWDS   DD DSN=MY.NEW.FILE,DISP=(NEW,CATLG),
//           SPACE=(TRK,(5,5)),DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//DELDS   DD DSN=MY.OLD.FILE,DISP=(OLD,DELETE)
```

---

## IEHMOVE -- Dataset Move/Copy

Moves or copies datasets, PDS members, or entire volumes. Unlike IEBCOPY, IEHMOVE
can handle sequential datasets and cross-volume operations.

### JCL Template
```jcl
//MOVE    EXEC PGM=IEHMOVE
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD UNIT=SYSDA,VOL=SER=VOL001,DISP=OLD
//DD1      DD DSN=INPUT.FILE,DISP=SHR
//DD2      DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG),
//            SPACE=(TRK,(50,10))
//SYSIN    DD *
  MOVE DSNAME=INPUT.FILE,TO=SYSDA=VOL002
/*
```

### Control Statements
| Statement | Purpose |
|-----------|---------|
| MOVE | Move dataset or volume |
| COPY | Copy dataset or volume |
| INCLUDE | Include specific members |
| EXCLUDE | Exclude specific members |

### Condition Codes
| RC | Meaning |
|----|---------|
| 0 | Successful |
| 4 | Warning |
| 8 | Error |
| 12 | Severe error |
| 16 | Terminal error |
"#
}

fn sort_page() -> &'static str {
    r#"# DFSORT Reference

DFSORT (Data Facility Sort) is the IBM sort/merge/copy utility for z/OS. It provides
high-performance data sorting, filtering, reformatting, and joining capabilities.

## Basic SORT Operation

### JCL Template
```jcl
//SORT    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=INPUT.FILE,DISP=SHR
//SORTOUT  DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG),
//            SPACE=(TRK,(50,10)),DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
```

## SORT FIELDS

```
SORT FIELDS=(pos,len,format,order,pos,len,format,order,...)
```

### Field Formats

| Format | Description | Example |
|--------|-------------|---------|
| CH | Character (EBCDIC) | `SORT FIELDS=(1,10,CH,A)` |
| ZD | Zoned decimal (signed) | `SORT FIELDS=(15,5,ZD,D)` |
| PD | Packed decimal | `SORT FIELDS=(20,4,PD,A)` |
| BI | Binary (unsigned) | `SORT FIELDS=(1,4,BI,A)` |
| FI | Fixed-point integer (signed) | `SORT FIELDS=(5,4,FI,D)` |
| FL | Floating-point | `SORT FIELDS=(1,8,FL,A)` |
| AC | ASCII character | `SORT FIELDS=(1,20,AC,A)` |
| AQ | Alternate collating sequence | `SORT FIELDS=(1,10,AQ,A)` |
| CSF | Numeric sign-trailing separate | `SORT FIELDS=(1,10,CSF,A)` |
| CSL | Numeric sign-leading separate | `SORT FIELDS=(1,10,CSL,A)` |
| CLO | COBOL-like numeric leading | `SORT FIELDS=(1,8,CLO,A)` |
| FS | Fixed-point signed | `SORT FIELDS=(1,2,FS,A)` |

### Sort Order
| Code | Meaning |
|------|---------|
| A | Ascending |
| D | Descending |
| E | User-defined (ALTSEQ) |

### COPY (No Sort)
```
SORT FIELDS=COPY
```

## INCLUDE / OMIT Conditions

Filter records before sorting. INCLUDE keeps matching records; OMIT removes them.

```
INCLUDE COND=(pos,len,format,comparison,value,...)
OMIT    COND=(pos,len,format,comparison,value,...)
```

### Comparison Operators
| Operator | Meaning |
|----------|---------|
| EQ | Equal |
| NE | Not equal |
| GT | Greater than |
| GE | Greater than or equal |
| LT | Less than |
| LE | Less than or equal |
| SS | Substring search |
| NUM | Numeric test |

### Examples
```
INCLUDE COND=(5,2,CH,EQ,C'NY')
INCLUDE COND=(5,2,CH,EQ,C'NY',OR,5,2,CH,EQ,C'CA')
OMIT COND=(80,1,CH,EQ,C'*')
INCLUDE COND=(1,4,ZD,GE,+1000,AND,1,4,ZD,LE,+9999)
INCLUDE COND=(10,8,CH,SS,C'ERROR')
```

## INREC / OUTREC Formatting

INREC reformats records before sorting; OUTREC reformats after sorting.

### BUILD Syntax
```
INREC  FIELDS=(pos,len,...)
INREC  BUILD=(pos,len,...)
OUTREC BUILD=(pos,len,...)
```

### Formatting Constants
| Constant | Description | Example |
|----------|-------------|---------|
| `C'text'` | Character literal | `C'HEADER'` |
| `X'hex'` | Hex literal | `X'40'` |
| `nX` | n spaces | `5X` |
| `n:` | Start at position n | `1:` |
| `SEQNUM,n,ZD` | Sequence number | `SEQNUM,8,ZD` |
| `DATE1` | Date YYYY/MM/DD | `DATE1` |
| `DATE2` | Date MM/DD/YYYY | `DATE2` |
| `DATE3` | Date DD/MM/YYYY | `DATE3` |
| `TIME1` | Time HH:MM:SS | `TIME1` |
| `TPTS` | Timestamp | `TPTS` |

### BUILD Examples
```
OUTREC BUILD=(1,10,C' - ',15,20,80:X'40')
INREC BUILD=(1,5,SEQNUM,8,ZD,C',',10,30)
OUTREC BUILD=(1:1,20,25:C'$',26:15,10,ZD,EDIT=(IIIIIII.TT))
```

### OVERLAY
Modifies specific positions without rebuilding the entire record.
```
OUTREC OVERLAY=(25:C'UPDATED',50:SEQNUM,5,ZD)
```

## IFTHEN Clauses

Conditional record transformation.

```
INREC IFTHEN=(WHEN=(condition),BUILD=(fields))
INREC IFTHEN=(WHEN=(condition),OVERLAY=(changes))
INREC IFTHEN=(WHEN=INIT,BUILD=(fields))
INREC IFTHEN=(WHEN=NONE,BUILD=(fields))
```

### WHEN Conditions
| Clause | Meaning |
|--------|---------|
| `WHEN=(cond)` | Apply when condition is true |
| `WHEN=INIT` | Apply to all records first (initialization) |
| `WHEN=NONE` | Apply to records not matched by any WHEN |
| `WHEN=GROUP` | Group-level processing |

### Examples
```
INREC IFTHEN=(WHEN=(1,1,CH,EQ,C'H'),BUILD=(1,80,81:C'HEADER')),
      IFTHEN=(WHEN=(1,1,CH,EQ,C'D'),BUILD=(1,80,81:C'DETAIL')),
      IFTHEN=(WHEN=NONE,BUILD=(1,80,81:C'OTHER'))
```

## JOINKEYS -- File Joining

Join two input files on matching keys.

### JCL Template
```jcl
//JOIN    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTJNF1 DD DSN=FILE1,DISP=SHR
//SORTJNF2 DD DSN=FILE2,DISP=SHR
//SORTOUT  DD DSN=JOINED.FILE,DISP=(NEW,CATLG),
//            SPACE=(TRK,(50,10))
//SYSIN    DD *
  JOINKEYS FILE=F1,FIELDS=(1,10,A)
  JOINKEYS FILE=F2,FIELDS=(1,10,A)
  JOIN UNPAIRED,F1,F2
  REFORMAT FIELDS=(F1:1,80,F2:11,70)
  SORT FIELDS=COPY
/*
```

### JOIN Types
| Type | Meaning |
|------|---------|
| `JOIN PAIRED` | Only matched records (inner join) |
| `JOIN UNPAIRED,F1,F2` | All records from both (full outer) |
| `JOIN UNPAIRED,F1` | All from F1 + matched F2 (left outer) |
| `JOIN UNPAIRED,F2` | All from F2 + matched F1 (right outer) |

### REFORMAT
Specifies which fields from F1 and F2 appear in the output.
```
REFORMAT FIELDS=(F1:1,50,F2:1,30,?)
```
The `?` field indicates match status: `B` (both), `1` (F1 only), `2` (F2 only).

## ICETool Operators

ICETool is a batch front-end for DFSORT providing multi-step operations.

### JCL Template
```jcl
//ICETOOL EXEC PGM=ICETOOL
//TOOLMSG  DD SYSOUT=*
//DFSMSG   DD SYSOUT=*
//IN       DD DSN=INPUT.FILE,DISP=SHR
//OUT      DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG),
//            SPACE=(TRK,(5,5))
//TOOLIN   DD *
  SORT FROM(IN) TO(OUT) USING(CTL1)
//CTL1CNTL DD *
  SORT FIELDS=(1,10,CH,A)
/*
```

### Operators

| Operator | Purpose | Syntax |
|----------|---------|--------|
| SORT | Sort a file | `SORT FROM(dd) TO(dd) USING(xxxx)` |
| COPY | Copy a file | `COPY FROM(dd) TO(dd) USING(xxxx)` |
| COUNT | Count records | `COUNT FROM(dd) WRITE(dd)` |
| SELECT | Select by occurrence | `SELECT FROM(dd) TO(dd) ON(pos,len,fmt) ALLDUPS\|NODUPS\|HIGHER(n)\|LOWER(n)` |
| DISPLAY | Display statistics | `DISPLAY FROM(dd) LIST(dd) ON(pos,len,fmt) operation` |
| STATS | Compute statistics | `STATS FROM(dd) LIST(dd) ON(pos,len,fmt)` |
| OCCUR | Count occurrences | `OCCUR FROM(dd) LIST(dd) ON(pos,len,fmt)` |
| UNIQUE | Count unique values | `UNIQUE FROM(dd) ON(pos,len,fmt)` |
| RANGE | Count values in range | `RANGE FROM(dd) ON(pos,len,fmt) HIGHER(v) LOWER(v)` |
| SPLICE | Combine records | `SPLICE FROM(dd) TO(dd) ON(pos,len,fmt) WITH(pos,len)` |

### ICETool Examples
```
* Remove duplicates
SELECT FROM(IN) TO(OUT) ON(1,10,CH) NODUPS

* Find all duplicates
SELECT FROM(IN) TO(OUT) ON(1,10,CH) ALLDUPS

* Count occurrences of each key
OCCUR FROM(IN) LIST(RPT) ON(1,10,CH)

* Display min, max, avg of a numeric field
STATS FROM(IN) LIST(RPT) ON(20,8,PD)
```

## SUM FIELDS

Summarize numeric fields or remove duplicate sort keys.

```
SUM FIELDS=(pos,len,format,pos,len,format,...)
```

### Remove Duplicates
```
SORT FIELDS=(1,10,CH,A)
SUM FIELDS=NONE
```
`SUM FIELDS=NONE` removes records with duplicate sort keys, keeping only the first.

### Accumulate Totals
```
SORT FIELDS=(1,10,CH,A)
SUM FIELDS=(20,8,PD,30,4,BI)
```
Records with the same sort key are merged, and the specified numeric fields are summed.

## E15 / E35 User Exits

| Exit | Phase | Purpose |
|------|-------|---------|
| E15 | Input | Process/insert/delete records before sort |
| E35 | Output | Process/insert/delete records after sort |
| E16 | Input (MERGE) | Same as E15 for merge operations |
| E32 | VSAM input | VSAM-specific input exit |
| E38 | Output tape | Tape-specific output exit |
| E39 | Output | Additional output processing |

### Exit Return Codes
| RC | Meaning |
|----|---------|
| 0 | Accept record as-is |
| 4 | Delete this record |
| 8 | Do not return (exit provides no more records) |
| 12 | Insert record (exit provides a new record) |
| 16 | Terminate sort |

### JCL for User Exits
```jcl
//SORT    EXEC PGM=SORT
//SORTOUT  DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG),
//            SPACE=(TRK,(50,10))
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
  MODS E15=(MYEXIT,4096,MYLIB,N),E35=(OUTEXIT,2048,MYLIB,N)
/*
```
"#
}

fn pgmmgmt_page() -> &'static str {
    r#"# Program Management Reference

Program management encompasses load module search order, link-editing (Binder),
authorization facilities, and the runtime program loading infrastructure.

## Load Module Search Order

When z/OS loads a program, it searches in this order:

1. **STEPLIB** -- DD in the JCL step (job-level library)
2. **JOBLIB** -- DD on the JOB statement (shared across steps)
3. **LPA** -- Link Pack Area (modules loaded into shared memory at IPL)
4. **LNKLST** -- Linklist concatenation (system-wide library chain)

If a module is found in an earlier location, later locations are not searched.

### JCL Examples
```jcl
//MYJOB   JOB ...
//JOBLIB   DD DSN=MY.JOBLIB.LOAD,DISP=SHR
//STEP1    EXEC PGM=MYPROG
//STEPLIB  DD DSN=MY.STEP.LOAD,DISP=SHR      ← searched first
//         DD DSN=MY.STEP2.LOAD,DISP=SHR     ← concatenated
```

## Load Module Format

A load module (or program object) contains:

| Record Type | Description |
|-------------|-------------|
| ESD | External Symbol Dictionary -- defines CSECTs, entry points, external references |
| TXT / TEXT | Object code (machine instructions and data) |
| RLD | Relocation Dictionary -- address constants requiring relocation |
| END | End of module, specifies entry point |
| CESD | Composite ESD (in program objects) |
| IDR | Identification Record (compiler/assembler stamps) |
| SYM | Symbol table (for debugging) |

## AMODE / RMODE Attributes

| Attribute | Values | Description |
|-----------|--------|-------------|
| AMODE | 24 | Program runs in 24-bit addressing mode (below 16MB line) |
| AMODE | 31 | Program runs in 31-bit addressing mode (below 2GB bar) |
| AMODE | 64 | Program runs in 64-bit addressing mode |
| AMODE | ANY | Program can run in any addressing mode |
| RMODE | 24 | Module must reside below 16MB line |
| RMODE | 31 | Module can reside below 2GB bar (but not above) |
| RMODE | ANY | Module can reside anywhere in real storage |

### Common Combinations
| AMODE | RMODE | Usage |
|-------|-------|-------|
| 24 | 24 | Legacy programs (pre-MVS/XA) |
| 31 | 24 | Programs called from below the line |
| 31 | ANY | Standard modern programs |
| ANY | ANY | Flexible modules (e.g., reentrant routines) |
| 64 | 31 | 64-bit programs loaded below the bar |

## APF -- Authorized Program Facility

APF-authorized programs can issue privileged SVCs and access protected system resources.

### Requirements for APF Authorization
1. The load library must be in the APF list (`PROGxx` or `IEAAPFxx`)
2. The program must be link-edited with `AC(1)` (authorization code 1)
3. The library must be accessed via an APF-authorized concatenation

### Adding to APF List
```
/* PROGxx member */
APF ADD DSNAME(MY.AUTH.LOADLIB) VOLUME(VOL001)
APF ADD DSNAME(MY.AUTH.LOADLIB) SMS

/* Dynamic command */
SETPROG APF,ADD,DSNAME=MY.AUTH.LOADLIB,VOLUME=VOL001
```

### Checking APF Status
```
D PROG,APF           /* Display APF list */
D PROG,APF,DSNAME=MY.AUTH.LOADLIB
```

## Link-Edit (Binder)

The Binder (program IEWL or IEWBLINK) combines object modules into executable
load modules or program objects.

### JCL Template
```jcl
//LKED    EXEC PGM=IEWL,PARM='LIST,MAP,XREF'
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=MY.OBJ,DISP=SHR
//SYSLMOD  DD DSN=MY.LOADLIB(MYPROG),DISP=OLD
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(5,2))
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DSN=MY.OBJLIB,DISP=SHR
```

### Binder Control Statements
| Statement | Purpose | Syntax |
|-----------|---------|--------|
| INCLUDE | Include object/load module | `INCLUDE SYSLIB(member)` |
| ENTRY | Specify entry point | `ENTRY entryname` |
| NAME | Name the output module | `NAME modname(R)` -- `(R)` replaces existing |
| ALIAS | Create an alias | `ALIAS aliasname` |
| MODE | Set AMODE/RMODE | `MODE AMODE(31),RMODE(ANY)` |
| SETCODE | Set authorization code | `SETCODE AC(1)` |
| CHANGE | Rename external symbol | `CHANGE oldname(newname)` |
| REPLACE | Replace CSECT | `REPLACE csectname` |
| ORDER | Order CSECTs | `ORDER csect1,csect2` |
| LIBRARY | Automatic call library | `LIBRARY SYSLIB` |
| SETSSI | Set SSI information | `SETSSI value` |

### Binder Options (PARM)
| Option | Description |
|--------|-------------|
| LIST | Produce module map listing |
| MAP | Produce cross-reference map |
| XREF | External symbol cross-reference |
| LET | Allow unresolved references (warning) |
| NCAL | No automatic library call |
| RENT | Reentrant attribute |
| REUS | Reusable attribute |
| REFR | Refreshable attribute |
| AC(n) | Authorization code (0 or 1) |
| AMODE(n) | Addressing mode |
| RMODE(n) | Residency mode |
| DYNAM(DLL) | DLL-enabled linking |

## LPA -- Link Pack Area

Modules in LPA are shared across all address spaces, reducing memory and load times.

### LPA Types
| Type | Description | When Loaded |
|------|-------------|-------------|
| PLPA | Pageable LPA | IPL (from SYS1.LPALIB) |
| FLPA | Fixed LPA (non-pageable) | IPL (from IEAFIXxx) |
| MLPA | Modified LPA (overlays PLPA) | IPL (from IEALPAxx) |
| Dynamic LPA | Added at runtime | `SETPROG LPA` command or PROGxx |

### Dynamic LPA Commands
```
SETPROG LPA,ADD,MODNAME=MYPROG,DSNAME=MY.LOAD
SETPROG LPA,DELETE,MODNAME=MYPROG
D PROG,LPA,MODNAME=MYPROG
```

### CLPA (Create LPA)
At IPL, specifying `CLPA` rebuilds LPA from scratch rather than reusing
the saved copy. Use after installing new system modules.

## Program Properties Table (PPT)

The PPT defines special execution attributes for system programs. Configured
via `SCHEDxx` member of PARMLIB.

| Property | Description |
|----------|-------------|
| KEY(n) | Storage protection key (0-15, 0 = supervisor) |
| NOSWAP | Program cannot be swapped out |
| PRIV | Privileged (can modify other address spaces) |
| SYST | System task |
| AFF(CPU) | CPU affinity |
| CANCEL | Can be cancelled |
| NOPASS | Bypass password protection |
| DSI | Disable special interrupts |
| TRANCLASS | Transaction class for WLM |

### SCHEDxx Example
```
PPT PGMNAME(MYPROG)              /* Program name                */
    KEY(8)                        /* Storage key 8               */
    NOSWAP                        /* Non-swappable               */
    PRIV                          /* Privileged                  */
    CANCEL                        /* Can be cancelled            */
    SYST                          /* System task                 */
```
"#
}

fn crypto_page() -> &'static str {
    r#"# ICSF Cryptographic Services Reference

The Integrated Cryptographic Service Facility (ICSF) provides cryptographic services
on z/OS, leveraging hardware crypto accelerators when available.

## Symmetric Algorithms

| Algorithm | Key Lengths | Description |
|-----------|-------------|-------------|
| AES | 128, 192, 256 bits | Advanced Encryption Standard |
| DES | 56 bits | Data Encryption Standard (legacy) |
| 3DES (TDES) | 112, 168 bits | Triple DES (2-key or 3-key) |
| RC4 | 40-2048 bits | Stream cipher (legacy) |

## Asymmetric Algorithms

| Algorithm | Key Lengths | Description |
|-----------|-------------|-------------|
| RSA | 1024, 2048, 3072, 4096 bits | RSA public-key encryption/signing |
| ECC P-256 | 256 bits | Elliptic Curve (NIST P-256 / secp256r1) |
| ECC P-384 | 384 bits | Elliptic Curve (NIST P-384 / secp384r1) |
| ECC P-521 | 521 bits | Elliptic Curve (NIST P-521 / secp521r1) |
| DSA | 1024-3072 bits | Digital Signature Algorithm |
| Diffie-Hellman | 1024-4096 bits | Key exchange protocol |

## Cipher Modes

| Mode | Full Name | Description |
|------|-----------|-------------|
| ECB | Electronic Code Book | Each block encrypted independently (not recommended) |
| CBC | Cipher Block Chaining | Each block XORed with previous ciphertext |
| CFB | Cipher Feedback | Converts block cipher to stream cipher |
| OFB | Output Feedback | Generates keystream from cipher output |
| CTR | Counter | Encrypts incrementing counter values |
| GCM | Galois/Counter Mode | Authenticated encryption with associated data (AEAD) |

## Key Stores

### CKDS -- Cryptographic Key Data Set
Stores symmetric keys (AES, DES, 3DES) in encrypted form. Keys are wrapped under
the master key and never stored in clear.

| Field | Description |
|-------|-------------|
| Label | Unique key identifier (64 bytes) |
| Type | Key type (DATA, CIPHER, EXPORTER, IMPORTER, etc.) |
| Algorithm | AES, DES, or TDES |
| Key Value | Encrypted key material |

### PKDS -- Public Key Data Set
Stores asymmetric key pairs (RSA, ECC, DSA).

| Field | Description |
|-------|-------------|
| Label | Unique key identifier |
| Type | RSA, ECC, DSA |
| Key Length | Modulus/curve size |
| Private Key | Encrypted private key material |
| Public Key | Clear public key |

### TKDS -- Token Key Data Set
Stores PKCS #11 tokens for applications using the PKCS #11 interface.

## RACF Integration

### CSFSERV Class
Controls access to individual ICSF callable services.

| Resource | Service | Description |
|----------|---------|-------------|
| CSFENC | CSNBENC | Symmetric encrypt |
| CSFDEC | CSNBDEC | Symmetric decrypt |
| CSFKGN | CSNBKGN | Key generate |
| CSFKRC | CSNBKRC | Key record create |
| CSFPKG | CSNBPKG | PKA key generate |
| CSFDKG | CSNBDKG | DES key generate |
| CSFSYI | CSNBSYI | Symmetric key import |
| CSFSYE | CSNBSYE | Symmetric key export |
| CSFSAD | CSNBSAD | Symmetric key add |
| CSFPKD | CSNBPKD | PKA decrypt |
| CSFPKE | CSNBPKE | PKA encrypt |
| CSFPKS | CSNBPKS | PKA key sign |
| CSFPKV | CSNBPKV | PKA key verify |

### CSFKEYS Class
Controls access to individual keys in the key stores.

```
/* Grant access to a specific key */
RDEFINE CSFKEYS MY.AES.KEY UACC(NONE)
PERMIT MY.AES.KEY CLASS(CSFKEYS) ID(MYUSER) ACCESS(UPDATE)
SETROPTS RACLIST(CSFKEYS) REFRESH

/* Wildcard key access */
RDEFINE CSFKEYS MY.KEYS.** UACC(NONE)
PERMIT MY.KEYS.** CLASS(CSFKEYS) ID(APPGRP) ACCESS(READ)
```

## Callable Services

### CSNBENC -- Symmetric Encrypt

```cobol
CALL 'CSNBENC' USING
    RETURN-CODE
    REASON-CODE
    EXIT-DATA-LENGTH
    EXIT-DATA
    KEY-IDENTIFIER
    TEXT-LENGTH
    CLEAR-TEXT
    INITIALIZATION-VECTOR
    RULE-ARRAY-COUNT
    RULE-ARRAY
    PAD-CHARACTER
    CIPHER-TEXT-LENGTH
    CIPHER-TEXT
    OPTIONAL-ICSF-DATA-1
    CHAIN-DATA-LENGTH
    CHAIN-DATA
```

| Rule Array | Values |
|-----------|--------|
| Algorithm | AES, DES, TDES |
| Mode | CBC, ECB, CFB, OFB, CTR |
| Key type | KEY-CLR (clear key), KEYIDENT (key label) |
| Padding | PKCS-PAD, PAD |
| Processing | INITIAL, CONTINUE, FINAL, ONLY |

### CSNBDEC -- Symmetric Decrypt

Same parameter list as CSNBENC, with CIPHER-TEXT as input and CLEAR-TEXT as output.

### CSNBKGN -- Key Generate

Generates a symmetric key and stores it in the CKDS.

```cobol
CALL 'CSNBKGN' USING
    RETURN-CODE
    REASON-CODE
    EXIT-DATA-LENGTH
    EXIT-DATA
    KEY-FORM
    KEY-LENGTH
    KEY-TYPE-1
    KEY-TYPE-2
    KEK-IDENTIFIER-1
    KEK-IDENTIFIER-2
    GENERATED-KEY-1
    GENERATED-KEY-2
```

| Parameter | Values |
|-----------|--------|
| KEY-FORM | OP (operational), IM (importable), EX (exportable) |
| KEY-LENGTH | SINGLE, DOUBLE, TRIPLE, KEYLN128, KEYLN192, KEYLN256 |
| KEY-TYPE-1 | DATA, CIPHER, EXPORTER, IMPORTER, DECIPHER, ENCIPHER |

### CSNBKRC -- Key Record Create

Creates a key record (label) in the CKDS or PKDS.

### CSNBDKG -- DES Key Generate

Generates DES or TDES keys specifically.

### CSNBPKG -- PKA Key Generate

Generates asymmetric key pairs (RSA, ECC).

```cobol
CALL 'CSNBPKG' USING
    RETURN-CODE
    REASON-CODE
    EXIT-DATA-LENGTH
    EXIT-DATA
    RULE-ARRAY-COUNT
    RULE-ARRAY
    SKELETON-KEY-TOKEN-LENGTH
    SKELETON-KEY-TOKEN
    TRANSPORT-KEY-IDENTIFIER
    GENERATED-KEY-TOKEN-LENGTH
    GENERATED-KEY-TOKEN
```

### CSNBSYI -- Symmetric Key Import

Imports a key encrypted under a transport key (key-encrypting key).

### CSNBSYE -- Symmetric Key Export

Exports a key encrypted under a transport key for secure transfer to another system.

## Hardware Acceleration

### CP Assist for Cryptographic Functions (CPACF)
Built into every z/Architecture processor. Provides hardware acceleration for:
- AES (128/192/256-bit) -- encrypt, decrypt, MAC
- SHA-1, SHA-224, SHA-256, SHA-384, SHA-512
- DES, 3DES
- GHASH for GCM mode

CPACF instructions:
| Instruction | Description |
|-------------|-------------|
| KM | Cipher Message (encrypt/decrypt) |
| KMC | Cipher Message with Chaining (CBC) |
| KMCTR | Cipher Message with Counter (CTR) |
| KMA | Cipher Message with Authentication (GCM) |
| KIMD | Intermediate Message Digest (hash) |
| KLMD | Last Message Digest (hash finalization) |
| KMAC | Message Authentication Code |

### Crypto Express Adapters
Hardware security modules (HSMs) installed in the CPC:
- **CEX7S / CEX8S** -- Secure key operations (master key protection)
- **CEX7A / CEX8A** -- Accelerator mode (bulk encryption offload)
- **CEX7P / CEX8P** -- EP11 coprocessor (PKCS #11 operations)

Each adapter type provides different capabilities:

| Feature | Coprocessor (S) | Accelerator (A) | EP11 (P) |
|---------|-----------------|------------------|----------|
| Secure key | Yes | No | Yes |
| Clear key | Yes | Yes | No |
| Key wrapping | Master key | N/A | Wrapping key |
| PKCS #11 | No | No | Yes |
| Performance | Medium | Highest | Medium |
"#
}

fn syscmd_page() -> &'static str {
    r#"# Operator Command Reference

MVS operator commands are issued from the system console (or SDSF) to control
system operations, display status, and manage resources.

## DISPLAY Commands

### D A -- Display Active Tasks
```
D A,L              Display all active jobs (long format)
D A,jobname        Display specific job
D A,TS             Display active TSO users
D A,LONG           Extended information
```

### D ASM -- Auxiliary Storage Manager
```
D ASM              Display page/swap dataset status
```

### D C -- Display Consoles
```
D C                Display all active consoles
D C,CN=name        Display specific console
```

### D D -- Display Dump
```
D D,TITLE          Display dump titles
D D,OPTIONS        Display dump options
```

### D ETR -- Display Timer
```
D ETR              Display External Time Reference status
```

### D GRS -- Display Enqueues
```
D GRS,RES=(qname,rname)     Display specific enqueue
D GRS,RES=(*,rname)         Display all qnames for rname
D GRS,CONTENTION             Display enqueue contention
D GRS,ANALYZE                Analyze deadlocks
```

### D IOS -- Display I/O Subsystem
```
D IOS,CONFIG        Display I/O configuration
D IOS,MHP           Display missing hot I/O interrupts
```

### D J -- Display Jobs
```
D J,LIST            Display job queue summary
```

### D M -- Display Memory
```
D M=STOR            Display real storage
D M=CSA             Display common storage
D M=SQA             Display system queue area
```

### D PROD -- Display Products
```
D PROD,STATE        Display product registration
D PROD,REGISTERED   Display registered products
```

### D R -- Display Outstanding Requests
```
D R,L               Display all outstanding replies
D R,REQ,S           Display system requests
```

### D SMF -- Display SMF Status
```
D SMF               Display SMF recording status
D SMF,M             Display SMF buffer usage
```

### D T -- Display Time
```
D T                 Display current date and time
```

### D TS -- Display TSO
```
D TS,L              Display TSO users (long format)
D TS,ALL            Display all TSO address spaces
```

### D U -- Display Units/Devices
```
D U,DASD            Display DASD status
D U,TAPE            Display tape status
D U,,,devnum        Display specific device
D U,ONLINE          Display online devices
D U,OFFLINE         Display offline devices
D U,ALLOC           Display allocated devices
```

## START Command

Start a started task (procedure) from PROCLIB.

```
S procname                       Start procedure
S procname.id                    Start with identifier
S procname,REUSASID=YES          Allow ASID reuse
S procname,SUB=MSTR              Start under master subsystem
S procname,parm1=val,parm2=val   Start with parameters
```

### Examples
```
S JES2,,,WARM              Start JES2 warm start
S VTAM                     Start VTAM
S TSO                      Start TSO
S TCPIP                    Start TCP/IP
S SDSF                     Start SDSF
S RMF,MEMBER(00)           Start RMF with member suffix
```

## STOP (P) Command

Stop a started task or job.

```
P jobname                   Stop job/started task
P JES2                      Stop JES2
P VTAM                      Stop VTAM
P TSO                       Stop TSO
```

## MODIFY (F) Command

Send a modify command to an active job or started task.

```
F jobname,parameter          Send parameter to job
F JES2,command               JES2 operator command
F VTAM,command               VTAM operator command
F TCPIP,command              TCP/IP operator command
```

### Examples
```
F JES2,$DA                   Display active jobs in JES2
F JES2,$PI1-5                Purge initiators 1-5
F JES2,$HASP                 Display JES2 status
F VTAM,D NET,ID=applname     Display VTAM node
F TCPIP,OBEYFILE             Reload TCP/IP config
F LLA,REFRESH                Refresh linklist lookaside
```

## CANCEL (C) Command

Cancel an active job or address space.

```
C jobname                    Cancel job
C jobname,DUMP               Cancel with dump
C jobname,A=asid             Cancel by ASID
C U=userid                   Cancel TSO user
```

## REPLY (R) Command

Reply to an outstanding WTOR (Write To Operator with Reply).

```
R nn,'text'                  Reply to message nn
R nn,U                       Reply "U" (often for mount requests)
R nn,CANCEL                  Cancel the request
```

## VARY (V) Command

Vary devices or resources online/offline.

```
V dev,ONLINE                 Bring device online
V dev,OFFLINE                Take device offline
V dev1-dev2,ONLINE           Range of devices
V PATH(dev,chpid),ONLINE     Vary channel path
V TCPIP,,OBEYFILE            Reload TCP/IP configuration
V TCPIP,,DROP,CONN=connid    Drop TCP connection
V TCPIP,,STOP                Stop TCP/IP
V SMS,VOLUME(volser),ENABLE  Enable SMS volume
```

## FORCE Command

Forcefully terminate a job or address space that does not respond to CANCEL.

```
FORCE jobname                Force terminate
FORCE jobname,ARM            Force with ARM restart
FORCE jobname,A=asid         Force by ASID
```

**Warning**: FORCE should be used only as a last resort. It can leave resources
in an inconsistent state.

## ROUTE Command

Route a command to another system in a sysplex.

```
RO sysname,command           Route to specific system
RO *ALL,command              Route to all systems
RO sysname,D A,L             Display active on another system
```

## SDSF -- System Display and Search Facility

SDSF provides an ISPF-based interface for system operations.

### Common SDSF Panels

| Command | Panel | Description |
|---------|-------|-------------|
| DA | Active | Display active jobs |
| ST | Status | Display job status/output |
| O | Output | Display held output |
| I | Input | Display input queue |
| H | Held output | Display held output queue |
| PR | Printers | Display printer status |
| INIT | Initiators | Display initiators |
| LOG | System log | Display system log |
| SYSLOG | Syslog | Display syslog |
| SE | Search | Search spool |
| RES | Resources | Display system resources |

### SDSF Action Characters

| Char | Action |
|------|--------|
| S | Select/browse output |
| ? | Display job details |
| P | Purge job output |
| C | Cancel job |
| A | Release job |
| H | Hold job |
| J | Submit JCL |
| SJ | Submit JCL from output |
| XDC | Print dataset to class |

### SDSF Column Commands
```
SORT fieldname A/D           Sort by column
FILTER fieldname 'value'     Filter display
PREFIX jobname               Filter by job prefix
OWNER userid                 Filter by owner
DEST destination             Filter by destination
```
"#
}

fn drda_page() -> &'static str {
    r#"# DRDA Wire Protocol Reference

Distributed Relational Database Architecture (DRDA) is the open standard wire
protocol used by DB2 for client-server communication. It defines how SQL requests
and results flow between an Application Requester (AR) and an Application Server (AS).

## Architecture Overview

```
+---------------------+        +---------------------+
|  Application        |        |  Application        |
|  Requester (AR)     | <----> |  Server (AS)        |
|  (Client / Driver)  |  TCP   |  (DB2 Engine)       |
+---------------------+        +---------------------+
        |                              |
   DRDA Protocol                  SQL Execution
   (DDM over DSS)                 (Local DB2)
```

- **AR (Application Requester)**: The client side (JDBC/ODBC driver, CLI)
- **AS (Application Server)**: The database server (DB2 for z/OS, LUW, etc.)

## DSS -- Data Stream Structure

Every DRDA message is wrapped in one or more DSS segments. A DSS is the basic
framing unit on the wire.

### DSS Header (6 bytes)

| Offset | Length | Field | Description |
|--------|--------|-------|-------------|
| 0 | 2 | Length | Total DSS length (including header) |
| 2 | 1 | Magic | Always `0xD0` |
| 3 | 1 | Flags | DSS type + chaining flags |
| 4 | 2 | CorrelID | Correlation identifier |

### DSS Type Flags (byte 3)

| Bits | Value | Type |
|------|-------|------|
| xx01 | 0x01 | Request DSS |
| xx10 | 0x02 | Reply DSS |
| xx11 | 0x03 | Object DSS |
| xx00 | 0x00 | Continuation DSS |

### Chaining Flags

| Bit | Meaning |
|-----|---------|
| 0x40 | Chained DSS (same correlation, more DSS follow) |
| 0x20 | Continue on error |
| 0x10 | Next DSS is same request |

### DSS Continuation
When a DDM object exceeds the maximum DSS size (32767 bytes), it is split across
multiple DSS segments. The first uses the normal type; subsequent segments use
continuation type (0x00).

## DDM -- Distributed Data Management Objects

DDM objects are the commands and responses carried inside DSS frames. Each starts
with a 4-byte header.

### DDM Header

| Offset | Length | Field | Description |
|--------|--------|-------|-------------|
| 0 | 2 | Length | DDM object length |
| 2 | 2 | Code Point | Command/object identifier |

### Connection Flow

```
Client (AR)                          Server (AS)
    |                                    |
    |--- EXCSAT (Exchange Server) ------>|
    |<-- EXCSATRD (Response) ------------|
    |                                    |
    |--- ACCSEC (Access Security) ------>|
    |<-- ACCSECRD (Response) ------------|
    |                                    |
    |--- SECCHK (Security Check) ------->|
    |<-- SECCHKRD (Response) ------------|
    |                                    |
    |--- ACCRDB (Access Database) ------>|
    |<-- ACCRDBRM (Response) ------------|
    |                                    |
    |  (connection established)          |
```

### SQL Execution Flow

```
Client (AR)                          Server (AS)
    |                                    |
    |--- EXCSQLIMM (Exec Immediate) --->|
    |    + SQLSTT (SQL Statement)       |
    |<-- SQLCARD (Result) --------------|
    |                                    |

    |--- PRPSQLSTT (Prepare) ---------->|
    |    + SQLSTT (SQL Statement)       |
    |<-- SQLDARD (Descriptor) ----------|
    |                                    |
    |--- OPNQRY (Open Query) ---------->|
    |    + SQLDTA (Parameters)          |
    |<-- OPNQRYRM + QRYDSC + QRYDTA --|
    |                                    |
    |--- CNTQRY (Continue Query) ------>|
    |<-- QRYDTA (More Rows) -----------|
    |                                    |
    |--- CLSQRY (Close Query) -------->|
    |<-- SQLCARD (Result) --------------|
```

## DDM Code Points Reference

### Connection & Security

| Code Point | Name | Description |
|------------|------|-------------|
| 0x1041 | EXCSAT | Exchange Server Attributes |
| 0x1443 | EXCSATRD | Exchange Server Attributes Reply |
| 0x106D | ACCSEC | Access Security |
| 0x14AC | ACCSECRD | Access Security Reply |
| 0x106E | SECCHK | Security Check |
| 0x1219 | SECCHKRD | Security Check Reply |
| 0x2001 | ACCRDB | Access RDB (Relational Database) |
| 0x2201 | ACCRDBRM | Access RDB Reply Message |

### SQL Operations

| Code Point | Name | Description |
|------------|------|-------------|
| 0x200A | EXCSQLIMM | Execute Immediate SQL |
| 0x200D | PRPSQLSTT | Prepare SQL Statement |
| 0x2002 | BGNBND | Begin Bind |
| 0x2004 | BNDSQLSTT | Bind SQL Statement |
| 0x2005 | CLSQRY | Close Query |
| 0x2006 | CNTQRY | Continue Query |
| 0x200C | OPNQRY | Open Query |
| 0x2003 | ENDBND | End Bind |
| 0x2009 | EXCSQLSTT | Execute SQL Statement |
| 0x200B | DRPPKG | Drop Package |
| 0x200E | RDBCMM | RDB Commit |
| 0x200F | RDBRLLBCK | RDB Rollback |

### Data & Descriptors

| Code Point | Name | Description |
|------------|------|-------------|
| 0x2412 | SQLDTA | SQL Data (parameter values) |
| 0x2414 | SQLSTT | SQL Statement text |
| 0x2408 | SQLCARD | SQL Communications Area (SQLCA) |
| 0x2411 | SQLDARD | SQL Descriptor Area Reply |
| 0x241B | QRYDSC | Query Answer Set Description |
| 0x241A | QRYDTA | Query Answer Set Data |
| 0x2132 | SQLAM | SQL Application Manager |
| 0x002F | TYPDEFNAM | Type Definition Name |
| 0x0035 | TYPDEFOVR | Type Definition Overrides |

### Reply Messages

| Code Point | Name | Description |
|------------|------|-------------|
| 0x2205 | OPNQRYRM | Open Query Reply Message |
| 0x2206 | ENDQRYRM | End of Query Reply Message |
| 0x220A | RDBUPDRM | RDB Update Reply Message |
| 0x2208 | CMDCHKRM | Command Check Reply Message |
| 0x2210 | SYNTAXRM | Syntax Error Reply Message |
| 0x2213 | PRCCNVRM | Conversation Protocol Error |
| 0x1232 | MGRLVLRM | Manager Level Reply Message |

## SQL Data Type Mapping

| DRDA Type Code | SQL Type | Description |
|----------------|----------|-------------|
| 0x30 | DATE | Date (10 bytes, YYYY-MM-DD) |
| 0x32 | TIME | Time (8 bytes, HH:MM:SS) |
| 0x34 | TIMESTAMP | Timestamp (26 bytes) |
| 0x40 | BLOB | Binary Large Object |
| 0x42 | CLOB | Character Large Object |
| 0x44 | DBCLOB | Double-byte CLOB |
| 0x60 | VARCHAR | Variable-length character |
| 0x64 | CHAR | Fixed-length character |
| 0x80 | SMALLINT | 16-bit integer |
| 0x84 | INTEGER | 32-bit integer |
| 0x8C | BIGINT | 64-bit integer |
| 0xC0 | FLOAT(4) | Single-precision float |
| 0xC8 | FLOAT(8) | Double-precision float |
| 0x0E | DECIMAL | Packed decimal |
| 0x10 | NUMERIC | Zoned decimal |

### Nullable Type Encoding
Nullable types add 1 to the type code (e.g., nullable CHAR = `0x65`, nullable
INTEGER = `0x85`). A 1-byte null indicator precedes the data: `0xFF` = null,
`0x00` = not null.

### TYPDEFNAM Values
| Value | Description |
|-------|-------------|
| QTDSQLASC | ASCII-based platform (LUW) |
| QTDSQLJVM | Java client |
| QTDSQLX86 | x86 Linux/Windows |
| QTDSQLEBC | EBCDIC-based platform (z/OS) |
"#
}
