//! JCL dependency analysis for batch job planning.
//!
//! Parses JCL job streams to identify which programs each step executes
//! and which datasets they reference, building a dependency map for
//! migration planning.

use std::collections::{HashMap, HashSet};

use serde::Serialize;

/// A JCL job with its steps and dependencies.
#[derive(Debug, Clone, Serialize)]
pub struct JclJob {
    /// Job name (from the JOB card).
    pub name: String,
    /// Job class (if present).
    pub class: Option<String>,
    /// Steps within this job.
    pub steps: Vec<JclStep>,
}

/// A step within a JCL job.
#[derive(Debug, Clone, Serialize)]
pub struct JclStep {
    /// Step name.
    pub step_name: String,
    /// Program executed (PGM=name).
    pub program: String,
    /// Procedure called (PROC=name), if any.
    pub proc_name: Option<String>,
    /// DD statements (dataset references).
    pub dd_statements: Vec<DdStatement>,
}

/// A DD (Data Definition) statement referencing a dataset.
#[derive(Debug, Clone, Serialize)]
pub struct DdStatement {
    /// DD name.
    pub dd_name: String,
    /// Dataset name (DSN=).
    pub dsn: Option<String>,
    /// Disposition string.
    pub disp: Option<String>,
    /// Whether this is an input (SHR/OLD) or output (NEW/MOD) reference.
    pub io_type: DdIoType,
}

/// Whether a DD statement represents input or output.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum DdIoType {
    Input,
    Output,
    Unknown,
}

/// Complete dependency analysis for a set of JCL jobs.
#[derive(Debug, Clone, Serialize)]
pub struct JclDependencyMap {
    /// All parsed jobs.
    pub jobs: Vec<JclJob>,
    /// Program-to-job mapping (which jobs run a given program).
    pub program_jobs: HashMap<String, Vec<String>>,
    /// Program-to-dataset mapping (which datasets a program touches).
    pub program_datasets: HashMap<String, HashSet<String>>,
    /// Dataset-to-program mapping (which programs touch a dataset).
    pub dataset_programs: HashMap<String, HashSet<String>>,
}

impl JclDependencyMap {
    /// Get all programs referenced across all jobs.
    pub fn all_programs(&self) -> HashSet<&str> {
        self.program_jobs.keys().map(|s| s.as_str()).collect()
    }

    /// Get all datasets referenced across all jobs.
    pub fn all_datasets(&self) -> HashSet<&str> {
        self.dataset_programs.keys().map(|s| s.as_str()).collect()
    }

    /// Get programs that share dataset dependencies (potential ordering constraints).
    pub fn shared_dataset_programs(&self) -> Vec<(String, Vec<String>)> {
        self.dataset_programs
            .iter()
            .filter(|(_, progs)| progs.len() > 1)
            .map(|(dsn, progs)| {
                let mut sorted: Vec<String> = progs.iter().cloned().collect();
                sorted.sort();
                (dsn.clone(), sorted)
            })
            .collect()
    }
}

/// Parse JCL source text and build the dependency map.
pub fn analyze_jcl(source: &str) -> JclDependencyMap {
    let jobs = parse_jcl_jobs(source);

    let mut program_jobs: HashMap<String, Vec<String>> = HashMap::new();
    let mut program_datasets: HashMap<String, HashSet<String>> = HashMap::new();
    let mut dataset_programs: HashMap<String, HashSet<String>> = HashMap::new();

    for job in &jobs {
        for step in &job.steps {
            let prog = step.program.to_uppercase();

            // Program → jobs mapping
            program_jobs
                .entry(prog.clone())
                .or_default()
                .push(job.name.clone());

            // Program ↔ dataset mappings
            for dd in &step.dd_statements {
                if let Some(ref dsn) = dd.dsn {
                    let dsn_upper = dsn.to_uppercase();
                    program_datasets
                        .entry(prog.clone())
                        .or_default()
                        .insert(dsn_upper.clone());
                    dataset_programs
                        .entry(dsn_upper)
                        .or_default()
                        .insert(prog.clone());
                }
            }
        }
    }

    JclDependencyMap {
        jobs,
        program_jobs,
        program_datasets,
        dataset_programs,
    }
}

/// Parse JCL source into jobs.
fn parse_jcl_jobs(source: &str) -> Vec<JclJob> {
    let mut jobs = Vec::new();
    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];

        // JOB card: //JOBNAME JOB ...
        if is_jcl_statement(line) {
            let upper = line.to_uppercase();
            if upper.contains(" JOB ") || upper.ends_with(" JOB") {
                let job_name = extract_label(line);
                let class = extract_param(&upper, "CLASS=");
                let mut steps = Vec::new();

                i += 1;
                // Parse steps until next JOB card or end of input
                while i < lines.len() {
                    let step_line = lines[i];
                    let step_upper = step_line.to_uppercase();

                    // New JOB card means end of current job
                    if is_jcl_statement(step_line)
                        && (step_upper.contains(" JOB ") || step_upper.ends_with(" JOB"))
                    {
                        break;
                    }

                    // EXEC statement: //STEPNAME EXEC PGM=name or EXEC PROC=name
                    if is_jcl_statement(step_line) && step_upper.contains(" EXEC ") {
                        let step_name = extract_label(step_line);
                        let program = extract_param(&step_upper, "PGM=")
                            .unwrap_or_default();
                        let proc_name = extract_param(&step_upper, "PROC=");

                        let mut dd_statements = Vec::new();

                        // Parse DD statements following this EXEC
                        i += 1;
                        while i < lines.len() {
                            let dd_line = lines[i];
                            let dd_upper = dd_line.to_uppercase();

                            // Stop at next EXEC, JOB, or end marker
                            if is_jcl_statement(dd_line)
                                && (dd_upper.contains(" EXEC ")
                                    || dd_upper.contains(" JOB ")
                                    || dd_upper.ends_with(" JOB"))
                            {
                                break;
                            }

                            // DD statement: //DDNAME DD DSN=...
                            if is_jcl_statement(dd_line) && dd_upper.contains(" DD ") {
                                let dd_name = extract_label(dd_line);
                                let dsn = extract_param(&dd_upper, "DSN=")
                                    .or_else(|| extract_param(&dd_upper, "DSNAME="));
                                let disp = extract_param(&dd_upper, "DISP=");
                                let io_type = classify_disp(disp.as_deref());

                                dd_statements.push(DdStatement {
                                    dd_name,
                                    dsn,
                                    disp,
                                    io_type,
                                });
                            }

                            i += 1;
                        }

                        if !program.is_empty() || proc_name.is_some() {
                            steps.push(JclStep {
                                step_name,
                                program,
                                proc_name,
                                dd_statements,
                            });
                        }
                        continue; // don't increment i again
                    }

                    i += 1;
                }

                jobs.push(JclJob {
                    name: job_name,
                    class,
                    steps,
                });
                continue; // don't increment i again
            }
        }

        i += 1;
    }

    jobs
}

/// Check if a line is a JCL statement (starts with //).
fn is_jcl_statement(line: &str) -> bool {
    line.starts_with("//") && !line.starts_with("//*")
}

/// Extract the label (step/job/dd name) from a JCL statement.
fn extract_label(line: &str) -> String {
    // //LABEL  VERB ... → extract LABEL
    let after_slashes = &line[2..];
    after_slashes
        .split_whitespace()
        .next()
        .unwrap_or("")
        .to_string()
}

/// Extract a parameter value like "PGM=PROGNAME" from a JCL line.
fn extract_param(upper_line: &str, param: &str) -> Option<String> {
    let pos = upper_line.find(param)?;
    let after = &upper_line[pos + param.len()..];
    let value = after
        .split([',', ' ', ')', '\'', '"'])
        .next()
        .unwrap_or("")
        .trim();
    if value.is_empty() {
        None
    } else {
        Some(value.to_string())
    }
}

/// Classify DISP parameter as input or output.
fn classify_disp(disp: Option<&str>) -> DdIoType {
    match disp {
        Some(d) => {
            let upper = d.to_uppercase();
            // First sub-parameter of DISP determines I/O:
            // (NEW,...) or (MOD,...) = output
            // (SHR,...) or (OLD,...) = input
            let first = upper
                .trim_start_matches('(')
                .split(',')
                .next()
                .unwrap_or("");
            match first {
                "SHR" | "OLD" => DdIoType::Input,
                "NEW" | "MOD" => DdIoType::Output,
                _ => DdIoType::Unknown,
            }
        }
        None => DdIoType::Unknown,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_job() {
        let jcl = r#"//CUSTJOB  JOB (ACCT),'CUST UPDATE',CLASS=A
//STEP1    EXEC PGM=CUSTUPD
//INFILE   DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR
//OUTFILE  DD DSN=PROD.CUSTOMER.UPDATE,DISP=(NEW,CATLG)
//SYSPRINT DD SYSOUT=A
"#;
        let map = analyze_jcl(jcl);
        assert_eq!(map.jobs.len(), 1);
        assert_eq!(map.jobs[0].name, "CUSTJOB");
        assert_eq!(map.jobs[0].steps.len(), 1);
        assert_eq!(map.jobs[0].steps[0].program, "CUSTUPD");
    }

    #[test]
    fn test_step_to_program_mapping() {
        let jcl = r#"//MYJOB   JOB (ACCT),'TEST'
//STEP1   EXEC PGM=CUSTUPD
//INFILE  DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR
"#;
        let map = analyze_jcl(jcl);
        assert!(map.program_jobs.contains_key("CUSTUPD"));
        assert_eq!(map.program_jobs["CUSTUPD"], vec!["MYJOB"]);
    }

    #[test]
    fn test_dataset_dependency_mapping() {
        let jcl = r#"//MYJOB   JOB (ACCT),'TEST'
//STEP1   EXEC PGM=CUSTUPD
//INFILE  DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR
//OUTFILE DD DSN=PROD.CUSTOMER.UPDATE,DISP=(NEW,CATLG)
"#;
        let map = analyze_jcl(jcl);
        let prog_ds = &map.program_datasets["CUSTUPD"];
        assert!(prog_ds.contains("PROD.CUSTOMER.MASTER"));
        assert!(prog_ds.contains("PROD.CUSTOMER.UPDATE"));

        let ds_prog = &map.dataset_programs["PROD.CUSTOMER.MASTER"];
        assert!(ds_prog.contains("CUSTUPD"));
    }

    #[test]
    fn test_multi_step_job() {
        let jcl = r#"//BATCHJOB JOB (ACCT),'BATCH RUN'
//STEP1    EXEC PGM=EXTRACT
//INPUT    DD DSN=PROD.MASTER.FILE,DISP=SHR
//STEP2    EXEC PGM=TRANSFRM
//INFILE   DD DSN=PROD.EXTRACT.DATA,DISP=SHR
//STEP3    EXEC PGM=LOADPROG
//OUTFILE  DD DSN=PROD.TARGET.FILE,DISP=(NEW,CATLG)
"#;
        let map = analyze_jcl(jcl);
        assert_eq!(map.jobs[0].steps.len(), 3);
        assert_eq!(map.jobs[0].steps[0].program, "EXTRACT");
        assert_eq!(map.jobs[0].steps[1].program, "TRANSFRM");
        assert_eq!(map.jobs[0].steps[2].program, "LOADPROG");
        assert_eq!(map.all_programs().len(), 3);
    }

    #[test]
    fn test_dd_io_classification() {
        assert_eq!(classify_disp(Some("SHR")), DdIoType::Input);
        assert_eq!(classify_disp(Some("OLD")), DdIoType::Input);
        assert_eq!(classify_disp(Some("(NEW,CATLG)")), DdIoType::Output);
        assert_eq!(classify_disp(Some("(MOD,KEEP)")), DdIoType::Output);
        assert_eq!(classify_disp(None), DdIoType::Unknown);
    }

    #[test]
    fn test_comments_are_skipped() {
        let jcl = r#"//MYJOB   JOB (ACCT),'TEST'
//* This is a comment
//STEP1   EXEC PGM=MYPROG
//* Another comment
//INFILE  DD DSN=MY.DATA.SET,DISP=SHR
"#;
        let map = analyze_jcl(jcl);
        assert_eq!(map.jobs.len(), 1);
        assert_eq!(map.jobs[0].steps.len(), 1);
        assert_eq!(map.jobs[0].steps[0].program, "MYPROG");
    }

    #[test]
    fn test_shared_dataset_programs() {
        let jcl = r#"//JOB1    JOB (ACCT),'JOB1'
//STEP1   EXEC PGM=PROGA
//INFILE  DD DSN=SHARED.DATA.SET,DISP=SHR
//JOB2    JOB (ACCT),'JOB2'
//STEP1   EXEC PGM=PROGB
//INFILE  DD DSN=SHARED.DATA.SET,DISP=OLD
"#;
        let map = analyze_jcl(jcl);
        let shared = map.shared_dataset_programs();
        assert!(!shared.is_empty());
        let (dsn, progs) = &shared[0];
        assert_eq!(dsn, "SHARED.DATA.SET");
        assert!(progs.contains(&"PROGA".to_string()));
        assert!(progs.contains(&"PROGB".to_string()));
    }

    #[test]
    fn test_multiple_jobs() {
        let jcl = r#"//JOBA    JOB (ACCT),'FIRST'
//STEP1   EXEC PGM=PROG1
//JOBB    JOB (ACCT),'SECOND'
//STEP1   EXEC PGM=PROG2
"#;
        let map = analyze_jcl(jcl);
        assert_eq!(map.jobs.len(), 2);
        assert_eq!(map.jobs[0].name, "JOBA");
        assert_eq!(map.jobs[1].name, "JOBB");
    }

    #[test]
    fn test_job_class() {
        let jcl = r#"//MYJOB   JOB (ACCT),'TEST',CLASS=B
//STEP1   EXEC PGM=MYPROG
"#;
        let map = analyze_jcl(jcl);
        assert_eq!(map.jobs[0].class.as_deref(), Some("B"));
    }

    #[test]
    fn test_all_datasets() {
        let jcl = r#"//MYJOB   JOB (ACCT),'TEST'
//STEP1   EXEC PGM=MYPROG
//IN1     DD DSN=DS.ONE,DISP=SHR
//IN2     DD DSN=DS.TWO,DISP=SHR
//OUT1    DD DSN=DS.THREE,DISP=(NEW,CATLG)
"#;
        let map = analyze_jcl(jcl);
        let datasets = map.all_datasets();
        assert_eq!(datasets.len(), 3);
        assert!(datasets.contains("DS.ONE"));
        assert!(datasets.contains("DS.TWO"));
        assert!(datasets.contains("DS.THREE"));
    }

    #[test]
    fn test_empty_jcl() {
        let map = analyze_jcl("");
        assert!(map.jobs.is_empty());
        assert!(map.all_programs().is_empty());
    }
}
