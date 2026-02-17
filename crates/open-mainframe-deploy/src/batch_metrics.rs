//! Batch job execution metrics for JCL workload monitoring.
//!
//! Records metrics for JCL batch job execution including step counts,
//! durations, return codes, and error tracking, complementing the
//! existing online CICS/COBOL metrics.

use std::collections::HashMap;
use std::time::{Duration, Instant};

use serde::Serialize;

/// Metrics recorded for a single JCL job execution.
#[derive(Debug, Clone, Serialize)]
pub struct JobMetrics {
    /// Job name.
    pub job_name: String,
    /// Job number (JES ID).
    pub job_number: u64,
    /// Total number of steps in the job.
    pub steps_total: usize,
    /// Steps executed (may be fewer if a step ABENDs).
    pub steps_executed: usize,
    /// Overall job return code (highest step RC).
    pub job_return_code: i32,
    /// Per-step metrics.
    pub step_metrics: Vec<StepMetrics>,
    /// Total job duration.
    pub total_duration: Duration,
    /// Whether the job completed successfully (RC <= max_rc).
    pub success: bool,
}

/// Metrics for a single step within a JCL job.
#[derive(Debug, Clone, Serialize)]
pub struct StepMetrics {
    /// Step name.
    pub step_name: String,
    /// Program executed (PGM=).
    pub program: String,
    /// Step return code.
    pub return_code: i32,
    /// Step execution duration.
    pub duration: Duration,
    /// Number of records read (if applicable).
    pub records_read: Option<u64>,
    /// Number of records written (if applicable).
    pub records_written: Option<u64>,
}

/// Aggregated batch metrics across multiple job runs.
#[derive(Debug, Clone, Default, Serialize)]
pub struct BatchMetricsCollector {
    /// Jobs completed (success + failure).
    pub jobs_completed: u64,
    /// Jobs that succeeded (RC <= threshold).
    pub jobs_succeeded: u64,
    /// Jobs that failed (RC > threshold or ABEND).
    pub jobs_failed: u64,
    /// Total steps executed.
    pub total_steps_executed: u64,
    /// Total errors (steps with RC > 4).
    pub errors_total: u64,
    /// Per-job-name error counts.
    pub errors_by_job: HashMap<String, u64>,
    /// Per-job-name last return codes.
    pub last_return_codes: HashMap<String, i32>,
    /// Per-job-name average durations in seconds.
    pub avg_durations: HashMap<String, f64>,
    /// Job execution count by name.
    job_counts: HashMap<String, u64>,
    /// Job total durations by name (for averaging).
    job_total_durations: HashMap<String, f64>,
}

impl BatchMetricsCollector {
    /// Create a new empty collector.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a completed job execution.
    pub fn record_job(&mut self, metrics: &JobMetrics) {
        self.jobs_completed += 1;
        if metrics.success {
            self.jobs_succeeded += 1;
        } else {
            self.jobs_failed += 1;
        }

        self.total_steps_executed += metrics.steps_executed as u64;

        // Count steps with RC > 4 as errors
        for step in &metrics.step_metrics {
            if step.return_code > 4 {
                self.errors_total += 1;
                *self.errors_by_job.entry(metrics.job_name.clone()).or_insert(0) += 1;
            }
        }

        self.last_return_codes
            .insert(metrics.job_name.clone(), metrics.job_return_code);

        // Update averages
        let dur_secs = metrics.total_duration.as_secs_f64();
        let count = self.job_counts.entry(metrics.job_name.clone()).or_insert(0);
        *count += 1;
        let total_dur = self
            .job_total_durations
            .entry(metrics.job_name.clone())
            .or_insert(0.0);
        *total_dur += dur_secs;
        self.avg_durations
            .insert(metrics.job_name.clone(), *total_dur / *count as f64);
    }

    /// Get the success rate as a percentage.
    pub fn success_rate(&self) -> f64 {
        if self.jobs_completed == 0 {
            return 100.0;
        }
        (self.jobs_succeeded as f64 / self.jobs_completed as f64) * 100.0
    }

    /// Render metrics in Prometheus text exposition format.
    pub fn to_prometheus(&self, prefix: &str) -> String {
        let mut output = String::new();

        output.push_str(&format!(
            "# HELP {prefix}_batch_jobs_completed_total Total batch jobs completed\n"
        ));
        output.push_str(&format!(
            "# TYPE {prefix}_batch_jobs_completed_total counter\n"
        ));
        output.push_str(&format!(
            "{prefix}_batch_jobs_completed_total {}\n",
            self.jobs_completed
        ));

        output.push_str(&format!(
            "# HELP {prefix}_batch_jobs_succeeded_total Total batch jobs succeeded\n"
        ));
        output.push_str(&format!(
            "# TYPE {prefix}_batch_jobs_succeeded_total counter\n"
        ));
        output.push_str(&format!(
            "{prefix}_batch_jobs_succeeded_total {}\n",
            self.jobs_succeeded
        ));

        output.push_str(&format!(
            "# HELP {prefix}_batch_jobs_failed_total Total batch jobs failed\n"
        ));
        output.push_str(&format!(
            "# TYPE {prefix}_batch_jobs_failed_total counter\n"
        ));
        output.push_str(&format!(
            "{prefix}_batch_jobs_failed_total {}\n",
            self.jobs_failed
        ));

        output.push_str(&format!(
            "# HELP {prefix}_batch_steps_executed_total Total batch steps executed\n"
        ));
        output.push_str(&format!(
            "# TYPE {prefix}_batch_steps_executed_total counter\n"
        ));
        output.push_str(&format!(
            "{prefix}_batch_steps_executed_total {}\n",
            self.total_steps_executed
        ));

        output.push_str(&format!(
            "# HELP {prefix}_batch_errors_total Total batch step errors\n"
        ));
        output.push_str(&format!(
            "# TYPE {prefix}_batch_errors_total counter\n"
        ));
        for (job, count) in &self.errors_by_job {
            output.push_str(&format!(
                "{prefix}_batch_errors_total{{job_name=\"{job}\"}} {count}\n"
            ));
        }

        output.push_str(&format!(
            "# HELP {prefix}_batch_job_return_code Last return code by job\n"
        ));
        output.push_str(&format!(
            "# TYPE {prefix}_batch_job_return_code gauge\n"
        ));
        for (job, rc) in &self.last_return_codes {
            output.push_str(&format!(
                "{prefix}_batch_job_return_code{{job_name=\"{job}\"}} {rc}\n"
            ));
        }

        output
    }
}

/// Builder for creating job metrics during execution.
pub struct JobExecutionTracker {
    job_name: String,
    job_number: u64,
    start_time: Instant,
    step_metrics: Vec<StepMetrics>,
    max_rc: i32,
    success_threshold: i32,
}

impl JobExecutionTracker {
    /// Start tracking a new job execution.
    pub fn start(job_name: &str, job_number: u64) -> Self {
        Self {
            job_name: job_name.to_string(),
            job_number,
            start_time: Instant::now(),
            step_metrics: Vec::new(),
            max_rc: 0,
            success_threshold: 4,
        }

    }

    /// Set the success threshold (default: RC <= 4).
    pub fn with_success_threshold(mut self, threshold: i32) -> Self {
        self.success_threshold = threshold;
        self
    }

    /// Record completion of a step.
    pub fn record_step(
        &mut self,
        step_name: &str,
        program: &str,
        return_code: i32,
        duration: Duration,
    ) {
        if return_code > self.max_rc {
            self.max_rc = return_code;
        }
        self.step_metrics.push(StepMetrics {
            step_name: step_name.to_string(),
            program: program.to_string(),
            return_code,
            duration,
            records_read: None,
            records_written: None,
        });
    }

    /// Record step completion with I/O stats.
    pub fn record_step_with_io(
        &mut self,
        step_name: &str,
        program: &str,
        return_code: i32,
        duration: Duration,
        records_read: u64,
        records_written: u64,
    ) {
        if return_code > self.max_rc {
            self.max_rc = return_code;
        }
        self.step_metrics.push(StepMetrics {
            step_name: step_name.to_string(),
            program: program.to_string(),
            return_code,
            duration,
            records_read: Some(records_read),
            records_written: Some(records_written),
        });
    }

    /// Finish tracking and produce the final job metrics.
    pub fn finish(self) -> JobMetrics {
        let total_duration = self.start_time.elapsed();
        let steps_total = self.step_metrics.len();
        JobMetrics {
            job_name: self.job_name,
            job_number: self.job_number,
            steps_total,
            steps_executed: steps_total,
            job_return_code: self.max_rc,
            step_metrics: self.step_metrics,
            total_duration,
            success: self.max_rc <= self.success_threshold,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_job(name: &str, rc: i32) -> JobMetrics {
        JobMetrics {
            job_name: name.to_string(),
            job_number: 1001,
            steps_total: 3,
            steps_executed: 3,
            job_return_code: rc,
            step_metrics: vec![
                StepMetrics {
                    step_name: "STEP1".to_string(),
                    program: "PROG1".to_string(),
                    return_code: 0,
                    duration: Duration::from_millis(500),
                    records_read: None,
                    records_written: None,
                },
                StepMetrics {
                    step_name: "STEP2".to_string(),
                    program: "PROG2".to_string(),
                    return_code: 4,
                    duration: Duration::from_millis(200),
                    records_read: None,
                    records_written: None,
                },
                StepMetrics {
                    step_name: "STEP3".to_string(),
                    program: "PROG3".to_string(),
                    return_code: rc,
                    duration: Duration::from_millis(300),
                    records_read: None,
                    records_written: None,
                },
            ],
            total_duration: Duration::from_secs(1),
            success: rc <= 4,
        }
    }

    #[test]
    fn test_record_successful_job() {
        let mut collector = BatchMetricsCollector::new();
        let job = sample_job("CUSTJOB", 0);
        collector.record_job(&job);

        assert_eq!(collector.jobs_completed, 1);
        assert_eq!(collector.jobs_succeeded, 1);
        assert_eq!(collector.jobs_failed, 0);
        assert_eq!(collector.total_steps_executed, 3);
    }

    #[test]
    fn test_record_failed_job() {
        let mut collector = BatchMetricsCollector::new();
        let job = sample_job("FAILJOB", 12);
        collector.record_job(&job);

        assert_eq!(collector.jobs_completed, 1);
        assert_eq!(collector.jobs_succeeded, 0);
        assert_eq!(collector.jobs_failed, 1);
        assert!(collector.errors_total > 0);
        assert_eq!(collector.last_return_codes["FAILJOB"], 12);
    }

    #[test]
    fn test_error_counting_by_job() {
        let mut collector = BatchMetricsCollector::new();
        let job = sample_job("ERRJOB", 12);
        collector.record_job(&job);

        // STEP3 has RC=12 which is > 4, so 1 error
        assert_eq!(collector.errors_by_job["ERRJOB"], 1);
    }

    #[test]
    fn test_success_rate() {
        let mut collector = BatchMetricsCollector::new();
        collector.record_job(&sample_job("JOB1", 0));
        collector.record_job(&sample_job("JOB2", 0));
        collector.record_job(&sample_job("JOB3", 12));
        collector.record_job(&sample_job("JOB4", 0));

        assert!((collector.success_rate() - 75.0).abs() < 0.01);
    }

    #[test]
    fn test_average_duration() {
        let mut collector = BatchMetricsCollector::new();
        collector.record_job(&sample_job("MYJOB", 0));
        collector.record_job(&sample_job("MYJOB", 0));

        // Both have 1s duration → avg = 1.0
        assert!((collector.avg_durations["MYJOB"] - 1.0).abs() < 0.01);
    }

    #[test]
    fn test_prometheus_output() {
        let mut collector = BatchMetricsCollector::new();
        collector.record_job(&sample_job("TESTJOB", 0));

        let output = collector.to_prometheus("omf");
        assert!(output.contains("omf_batch_jobs_completed_total 1"));
        assert!(output.contains("omf_batch_jobs_succeeded_total 1"));
        assert!(output.contains("omf_batch_steps_executed_total 3"));
    }

    #[test]
    fn test_job_execution_tracker() {
        let mut tracker = JobExecutionTracker::start("TRACKJOB", 2001);
        tracker.record_step("STEP1", "EXTRACT", 0, Duration::from_millis(100));
        tracker.record_step("STEP2", "TRANSFORM", 4, Duration::from_millis(200));
        tracker.record_step("STEP3", "LOAD", 0, Duration::from_millis(150));
        let metrics = tracker.finish();

        assert_eq!(metrics.job_name, "TRACKJOB");
        assert_eq!(metrics.steps_total, 3);
        assert_eq!(metrics.job_return_code, 4);
        assert!(metrics.success); // RC=4 <= threshold 4
    }

    #[test]
    fn test_tracker_with_custom_threshold() {
        let mut tracker = JobExecutionTracker::start("STRICTJOB", 3001)
            .with_success_threshold(0);
        tracker.record_step("STEP1", "PROG", 4, Duration::from_millis(100));
        let metrics = tracker.finish();

        assert!(!metrics.success); // RC=4 > threshold 0
    }

    #[test]
    fn test_step_with_io_stats() {
        let mut tracker = JobExecutionTracker::start("IOJOB", 4001);
        tracker.record_step_with_io(
            "STEP1", "SORTPROG", 0, Duration::from_millis(500), 10000, 9500,
        );
        let metrics = tracker.finish();

        assert_eq!(metrics.step_metrics[0].records_read, Some(10000));
        assert_eq!(metrics.step_metrics[0].records_written, Some(9500));
    }

    #[test]
    fn test_empty_collector_success_rate() {
        let collector = BatchMetricsCollector::new();
        assert_eq!(collector.success_rate(), 100.0);
    }
}
