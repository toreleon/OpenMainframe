//! OUTFIL multi-output routing.
//!
//! Routes sorted records to multiple output files with per-file filtering
//! and reformatting in a single pass. Supports SPLIT, SPLITBY, and SPLIT1R
//! for distributing records across output files.

use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::Path;

use crate::error::SortError;
use crate::filter::FilterSpec;
use crate::ifthen::IfThenSpec;
use crate::reformat::OutrecSpec;

/// Distribution strategy for splitting records across output files.
#[derive(Debug, Clone)]
pub enum SplitMode {
    /// Round-robin distribution (one record per file in rotation).
    Split,
    /// Fixed-size blocks (N records per file before moving to the next).
    SplitBy(usize),
    /// One record per file (each record goes to the next file in sequence).
    Split1R,
}

/// A header or trailer record specification.
#[derive(Debug, Clone)]
pub struct HeaderTrailerSpec {
    /// The literal/formatted content segments for this header/trailer line.
    pub segments: Vec<HeaderSegment>,
}

/// A segment within a header/trailer line.
#[derive(Debug, Clone)]
pub enum HeaderSegment {
    /// Literal text.
    Literal(Vec<u8>),
    /// Pad to a specific column with spaces (1-based target column).
    Column(usize),
    /// Insert the current date in a given format.
    Date(DateFormat),
    /// Insert the current time in a given format.
    Time(TimeFormat),
    /// Insert the record count (filled at write time).
    Count { width: usize },
    /// Insert page number.
    Page { width: usize },
}

/// Date format codes for header/trailer DATE= parameter.
#[derive(Debug, Clone, Copy)]
pub enum DateFormat {
    /// 4-digit year, 2-digit month, separator (YYYY/MM/DD)
    Ymd(char),
    /// 2-digit month, 2-digit day, 4-digit year (MM/DD/YYYY)
    Mdy(char),
    /// 2-digit day, 2-digit month, 4-digit year (DD/MM/YYYY)
    Dmy(char),
}

/// Time format codes for header/trailer TIME= parameter.
#[derive(Debug, Clone, Copy)]
pub enum TimeFormat {
    /// HH:MM:SS
    Hms(char),
    /// HH:MM
    Hm(char),
}

impl HeaderTrailerSpec {
    /// Create a new spec from a single literal.
    pub fn from_literal(text: &[u8]) -> Self {
        Self {
            segments: vec![HeaderSegment::Literal(text.to_vec())],
        }
    }

    /// Render the header/trailer line.
    pub fn render(&self, record_count: usize, page_number: usize) -> Vec<u8> {
        let mut output = Vec::new();
        let now = current_date_time();

        for segment in &self.segments {
            match segment {
                HeaderSegment::Literal(bytes) => {
                    output.extend_from_slice(bytes);
                }
                HeaderSegment::Column(col) => {
                    let target = col.saturating_sub(1);
                    if target > output.len() {
                        output.resize(target, b' ');
                    }
                }
                HeaderSegment::Date(fmt) => {
                    let (y, m, d) = (now.0, now.1, now.2);
                    let date_str = match fmt {
                        DateFormat::Ymd(sep) => format!("{:04}{}{:02}{}{:02}", y, sep, m, sep, d),
                        DateFormat::Mdy(sep) => format!("{:02}{}{:02}{}{:04}", m, sep, d, sep, y),
                        DateFormat::Dmy(sep) => format!("{:02}{}{:02}{}{:04}", d, sep, m, sep, y),
                    };
                    output.extend_from_slice(date_str.as_bytes());
                }
                HeaderSegment::Time(fmt) => {
                    let (h, min, s) = (now.3, now.4, now.5);
                    let time_str = match fmt {
                        TimeFormat::Hms(sep) => format!("{:02}{}{:02}{}{:02}", h, sep, min, sep, s),
                        TimeFormat::Hm(sep) => format!("{:02}{}{:02}", h, sep, min),
                    };
                    output.extend_from_slice(time_str.as_bytes());
                }
                HeaderSegment::Count { width } => {
                    let formatted = format!("{:>width$}", record_count, width = *width);
                    output.extend_from_slice(formatted.as_bytes());
                }
                HeaderSegment::Page { width } => {
                    let formatted = format!("{:>width$}", page_number, width = *width);
                    output.extend_from_slice(formatted.as_bytes());
                }
            }
        }

        output
    }
}

/// Get current date/time as (year, month, day, hour, minute, second).
fn current_date_time() -> (u32, u32, u32, u32, u32, u32) {
    // Use seconds since UNIX_EPOCH to compute a rough calendar date
    let secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    // Simplified date computation (days since epoch)
    let days = (secs / 86400) as i64;
    let time_of_day = secs % 86400;
    let hour = (time_of_day / 3600) as u32;
    let minute = ((time_of_day % 3600) / 60) as u32;
    let second = (time_of_day % 60) as u32;

    // Civil date from days since 1970-01-01 (algorithm from Howard Hinnant)
    let z = days + 719468;
    let era = if z >= 0 { z } else { z - 146096 } / 146097;
    let doe = (z - era * 146097) as u32;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };

    (y as u32, m, d, hour, minute, second)
}

/// A single OUTFIL output descriptor.
#[derive(Debug, Clone)]
pub struct OutfilDescriptor {
    /// Output file path.
    pub path: String,
    /// Optional INCLUDE filter for this output.
    pub include: Option<FilterSpec>,
    /// Optional OMIT filter for this output.
    pub omit: Option<FilterSpec>,
    /// Optional OUTREC/BUILD reformatting for this output.
    pub outrec: Option<OutrecSpec>,
    /// Optional IFTHEN reformatting for this output.
    pub ifthen: Option<IfThenSpec>,
    /// Optional HEADER records (written before data records).
    pub headers: Vec<HeaderTrailerSpec>,
    /// Optional TRAILER records (written after data records).
    pub trailers: Vec<HeaderTrailerSpec>,
}

impl OutfilDescriptor {
    /// Create a new OUTFIL descriptor for the given output path.
    pub fn new(path: &str) -> Self {
        Self {
            path: path.to_string(),
            include: None,
            omit: None,
            outrec: None,
            ifthen: None,
            headers: Vec::new(),
            trailers: Vec::new(),
        }
    }

    /// Set the INCLUDE filter.
    pub fn with_include(mut self, filter: FilterSpec) -> Self {
        self.include = Some(filter);
        self
    }

    /// Set the OMIT filter.
    pub fn with_omit(mut self, filter: FilterSpec) -> Self {
        self.omit = Some(filter);
        self
    }

    /// Set the OUTREC/BUILD specification.
    pub fn with_outrec(mut self, spec: OutrecSpec) -> Self {
        self.outrec = Some(spec);
        self
    }

    /// Set the IFTHEN specification.
    pub fn with_ifthen(mut self, spec: IfThenSpec) -> Self {
        self.ifthen = Some(spec);
        self
    }

    /// Add a HEADER record specification.
    pub fn with_header(mut self, spec: HeaderTrailerSpec) -> Self {
        self.headers.push(spec);
        self
    }

    /// Add a TRAILER record specification.
    pub fn with_trailer(mut self, spec: HeaderTrailerSpec) -> Self {
        self.trailers.push(spec);
        self
    }

    /// Check if a record matches this descriptor's filter.
    fn should_include(&self, record: &[u8]) -> bool {
        if let Some(ref filter) = self.include {
            if !filter.should_include(record) {
                return false;
            }
        }
        if let Some(ref filter) = self.omit {
            if !filter.should_include(record) {
                return false;
            }
        }
        true
    }

    /// Reformat a record according to this descriptor's outrec/ifthen.
    fn reformat(&self, record: &[u8]) -> Vec<u8> {
        let mut result = record.to_vec();

        if let Some(ref ifthen) = self.ifthen {
            result = ifthen.apply(&result);
        }

        if let Some(ref outrec) = self.outrec {
            result = outrec.reformat(&result);
        }

        result
    }
}

/// Complete OUTFIL specification with multiple output descriptors.
#[derive(Debug, Clone)]
pub struct OutfilSpec {
    /// Output descriptors (one per output file).
    pub descriptors: Vec<OutfilDescriptor>,
    /// Optional SAVE output for unmatched records.
    pub save_path: Option<String>,
    /// Optional split mode for load-balanced distribution.
    pub split_mode: Option<SplitMode>,
}

impl OutfilSpec {
    /// Create a new OUTFIL specification.
    pub fn new() -> Self {
        Self {
            descriptors: Vec::new(),
            save_path: None,
            split_mode: None,
        }
    }

    /// Add an output descriptor.
    pub fn add_descriptor(mut self, desc: OutfilDescriptor) -> Self {
        self.descriptors.push(desc);
        self
    }

    /// Set the SAVE output path.
    pub fn with_save(mut self, path: &str) -> Self {
        self.save_path = Some(path.to_string());
        self
    }

    /// Set the split mode.
    pub fn with_split_mode(mut self, mode: SplitMode) -> Self {
        self.split_mode = Some(mode);
        self
    }

    /// Process sorted records through OUTFIL routing.
    ///
    /// Each record is evaluated against all descriptors. Records matching
    /// a descriptor's filter are reformatted and written to its output.
    /// Unmatched records go to the SAVE output (if specified).
    pub fn process(&self, records: &[Vec<u8>]) -> Result<OutfilStats, SortError> {
        if let Some(ref split_mode) = self.split_mode {
            return self.process_split(records, split_mode);
        }

        self.process_filtered(records)
    }

    /// Process records with filter-based routing.
    fn process_filtered(&self, records: &[Vec<u8>]) -> Result<OutfilStats, SortError> {
        // Open output writers
        let mut writers: Vec<BufWriter<File>> = self
            .descriptors
            .iter()
            .map(|d| open_output(&d.path))
            .collect::<Result<Vec<_>, SortError>>()?;

        let mut save_writer = if let Some(ref path) = self.save_path {
            Some(open_output(path)?)
        } else {
            None
        };

        let mut counts = vec![0usize; self.descriptors.len()];
        let mut save_count = 0usize;
        let total_count = records.len();

        // Write HEADER records
        for (i, desc) in self.descriptors.iter().enumerate() {
            for header in &desc.headers {
                let line = header.render(total_count, 1);
                writers[i].write_all(&line)?;
                writers[i].write_all(b"\n")?;
            }
        }

        for record in records {
            let mut matched = false;

            for (i, desc) in self.descriptors.iter().enumerate() {
                if desc.should_include(record) {
                    let output = desc.reformat(record);
                    writers[i].write_all(&output)?;
                    writers[i].write_all(b"\n")?;
                    counts[i] += 1;
                    matched = true;
                    // Records can match multiple OUTFILs (per IBM behavior)
                }
            }

            if !matched {
                if let Some(ref mut writer) = save_writer {
                    writer.write_all(record)?;
                    writer.write_all(b"\n")?;
                    save_count += 1;
                }
            }
        }

        // Write TRAILER records
        for (i, desc) in self.descriptors.iter().enumerate() {
            for trailer in &desc.trailers {
                let line = trailer.render(counts[i], 1);
                writers[i].write_all(&line)?;
                writers[i].write_all(b"\n")?;
            }
        }

        // Flush all writers
        for writer in &mut writers {
            writer.flush()?;
        }
        if let Some(ref mut writer) = save_writer {
            writer.flush()?;
        }

        Ok(OutfilStats {
            output_counts: counts,
            save_count,
        })
    }

    /// Process records with split-based distribution.
    fn process_split(
        &self,
        records: &[Vec<u8>],
        split_mode: &SplitMode,
    ) -> Result<OutfilStats, SortError> {
        if self.descriptors.is_empty() {
            return Ok(OutfilStats {
                output_counts: vec![],
                save_count: 0,
            });
        }

        let mut writers: Vec<BufWriter<File>> = self
            .descriptors
            .iter()
            .map(|d| open_output(&d.path))
            .collect::<Result<Vec<_>, SortError>>()?;

        let mut counts = vec![0usize; self.descriptors.len()];
        let n = self.descriptors.len();

        match split_mode {
            SplitMode::Split | SplitMode::Split1R => {
                // Round-robin distribution
                for (i, record) in records.iter().enumerate() {
                    let idx = i % n;
                    let output = self.descriptors[idx].reformat(record);
                    writers[idx].write_all(&output)?;
                    writers[idx].write_all(b"\n")?;
                    counts[idx] += 1;
                }
            }
            SplitMode::SplitBy(block_size) => {
                // Fixed-size blocks
                for (i, record) in records.iter().enumerate() {
                    let idx = (i / block_size) % n;
                    let output = self.descriptors[idx].reformat(record);
                    writers[idx].write_all(&output)?;
                    writers[idx].write_all(b"\n")?;
                    counts[idx] += 1;
                }
            }
        }

        for writer in &mut writers {
            writer.flush()?;
        }

        Ok(OutfilStats {
            output_counts: counts,
            save_count: 0,
        })
    }
}

impl Default for OutfilSpec {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics from OUTFIL processing.
#[derive(Debug)]
pub struct OutfilStats {
    /// Number of records written to each output file.
    pub output_counts: Vec<usize>,
    /// Number of records written to SAVE output.
    pub save_count: usize,
}

/// Open an output file for writing.
fn open_output(path: &str) -> Result<BufWriter<File>, SortError> {
    let file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(Path::new(path))?;
    Ok(BufWriter::new(file))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fields::DataType;
    use crate::filter::{CompareOp, Condition, FilterType};
    use std::fs;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_path(suffix: &str) -> String {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        let path = std::env::temp_dir().join(format!("outfil_test_{}_{}", suffix, count));
        path.to_string_lossy().to_string()
    }

    fn cleanup(path: &str) {
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_outfil_filter_routing() {
        let out1 = test_path("ny");
        let out2 = test_path("ca");

        let spec = OutfilSpec::new()
            .add_descriptor(
                OutfilDescriptor::new(&out1).with_include(FilterSpec {
                    filter_type: FilterType::Include,
                    conditions: vec![Condition {
                        position: 1,
                        length: 2,
                        data_type: DataType::Character,
                        op: CompareOp::Eq,
                        value: b"NY".to_vec(),
                    }],
                    logic: None,
                }),
            )
            .add_descriptor(
                OutfilDescriptor::new(&out2).with_include(FilterSpec {
                    filter_type: FilterType::Include,
                    conditions: vec![Condition {
                        position: 1,
                        length: 2,
                        data_type: DataType::Character,
                        op: CompareOp::Eq,
                        value: b"CA".to_vec(),
                    }],
                    logic: None,
                }),
            );

        let records: Vec<Vec<u8>> = vec![
            b"NY001".to_vec(),
            b"CA002".to_vec(),
            b"NY003".to_vec(),
            b"TX004".to_vec(),
            b"CA005".to_vec(),
        ];

        let stats = spec.process(&records).unwrap();
        assert_eq!(stats.output_counts, vec![2, 2]); // 2 NY, 2 CA

        let ny = fs::read_to_string(&out1).unwrap();
        assert_eq!(ny, "NY001\nNY003\n");

        let ca = fs::read_to_string(&out2).unwrap();
        assert_eq!(ca, "CA002\nCA005\n");

        cleanup(&out1);
        cleanup(&out2);
    }

    #[test]
    fn test_outfil_save() {
        let out1 = test_path("matched");
        let save = test_path("save");

        let spec = OutfilSpec::new()
            .add_descriptor(
                OutfilDescriptor::new(&out1).with_include(FilterSpec {
                    filter_type: FilterType::Include,
                    conditions: vec![Condition {
                        position: 1,
                        length: 1,
                        data_type: DataType::Character,
                        op: CompareOp::Eq,
                        value: b"A".to_vec(),
                    }],
                    logic: None,
                }),
            )
            .with_save(&save);

        let records: Vec<Vec<u8>> = vec![
            b"A001".to_vec(),
            b"B002".to_vec(),
            b"A003".to_vec(),
            b"C004".to_vec(),
        ];

        let stats = spec.process(&records).unwrap();
        assert_eq!(stats.output_counts, vec![2]);
        assert_eq!(stats.save_count, 2);

        let saved = fs::read_to_string(&save).unwrap();
        assert_eq!(saved, "B002\nC004\n");

        cleanup(&out1);
        cleanup(&save);
    }

    #[test]
    fn test_outfil_split_round_robin() {
        let out1 = test_path("split1");
        let out2 = test_path("split2");
        let out3 = test_path("split3");

        let spec = OutfilSpec::new()
            .add_descriptor(OutfilDescriptor::new(&out1))
            .add_descriptor(OutfilDescriptor::new(&out2))
            .add_descriptor(OutfilDescriptor::new(&out3))
            .with_split_mode(SplitMode::Split);

        let records: Vec<Vec<u8>> = (1..=9)
            .map(|i| format!("R{:03}", i).into_bytes())
            .collect();

        let stats = spec.process(&records).unwrap();
        assert_eq!(stats.output_counts, vec![3, 3, 3]);

        let f1 = fs::read_to_string(&out1).unwrap();
        assert_eq!(f1, "R001\nR004\nR007\n");

        let f2 = fs::read_to_string(&out2).unwrap();
        assert_eq!(f2, "R002\nR005\nR008\n");

        let f3 = fs::read_to_string(&out3).unwrap();
        assert_eq!(f3, "R003\nR006\nR009\n");

        cleanup(&out1);
        cleanup(&out2);
        cleanup(&out3);
    }

    #[test]
    fn test_outfil_splitby() {
        let out1 = test_path("by1");
        let out2 = test_path("by2");
        let out3 = test_path("by3");

        let spec = OutfilSpec::new()
            .add_descriptor(OutfilDescriptor::new(&out1))
            .add_descriptor(OutfilDescriptor::new(&out2))
            .add_descriptor(OutfilDescriptor::new(&out3))
            .with_split_mode(SplitMode::SplitBy(3));

        let records: Vec<Vec<u8>> = (1..=8)
            .map(|i| format!("R{:03}", i).into_bytes())
            .collect();

        let stats = spec.process(&records).unwrap();
        // Records 1-3 → file 1 (3), 4-6 → file 2 (3), 7-8 → file 3 (2)
        assert_eq!(stats.output_counts, vec![3, 3, 2]);

        let f1 = fs::read_to_string(&out1).unwrap();
        assert_eq!(f1, "R001\nR002\nR003\n");

        let f3 = fs::read_to_string(&out3).unwrap();
        assert_eq!(f3, "R007\nR008\n");

        cleanup(&out1);
        cleanup(&out2);
        cleanup(&out3);
    }

    // -----------------------------------------------------------------------
    // HEADER/TRAILER Tests (Epic 809)
    // -----------------------------------------------------------------------

    #[test]
    fn test_outfil_with_header() {
        let out1 = test_path("header");

        let header = HeaderTrailerSpec {
            segments: vec![HeaderSegment::Literal(b"Sales Report".to_vec())],
        };

        let spec = OutfilSpec::new()
            .add_descriptor(
                OutfilDescriptor::new(&out1).with_header(header),
            );

        let records: Vec<Vec<u8>> = vec![b"R001".to_vec(), b"R002".to_vec()];
        spec.process(&records).unwrap();

        let output = fs::read_to_string(&out1).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines[0], "Sales Report");
        assert_eq!(lines[1], "R001");
        assert_eq!(lines[2], "R002");

        cleanup(&out1);
    }

    #[test]
    fn test_outfil_with_trailer() {
        let out1 = test_path("trailer");

        let trailer = HeaderTrailerSpec {
            segments: vec![
                HeaderSegment::Literal(b"Total Records: ".to_vec()),
                HeaderSegment::Count { width: 8 },
            ],
        };

        let spec = OutfilSpec::new()
            .add_descriptor(
                OutfilDescriptor::new(&out1).with_trailer(trailer),
            );

        let records: Vec<Vec<u8>> = vec![b"R001".to_vec(), b"R002".to_vec(), b"R003".to_vec()];
        spec.process(&records).unwrap();

        let output = fs::read_to_string(&out1).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines.len(), 4); // 3 data + 1 trailer
        assert_eq!(lines[3], "Total Records:        3");

        cleanup(&out1);
    }

    #[test]
    fn test_outfil_header_and_trailer() {
        let out1 = test_path("hdr_trl");

        let header = HeaderTrailerSpec::from_literal(b"=== REPORT ===");
        let trailer = HeaderTrailerSpec {
            segments: vec![
                HeaderSegment::Literal(b"Count: ".to_vec()),
                HeaderSegment::Count { width: 4 },
            ],
        };

        let spec = OutfilSpec::new()
            .add_descriptor(
                OutfilDescriptor::new(&out1)
                    .with_header(header)
                    .with_trailer(trailer),
            );

        let records: Vec<Vec<u8>> = vec![b"DATA1".to_vec(), b"DATA2".to_vec()];
        spec.process(&records).unwrap();

        let output = fs::read_to_string(&out1).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines[0], "=== REPORT ===");
        assert_eq!(lines[1], "DATA1");
        assert_eq!(lines[2], "DATA2");
        assert_eq!(lines[3], "Count:    2");

        cleanup(&out1);
    }

    #[test]
    fn test_header_date_segment() {
        let header = HeaderTrailerSpec {
            segments: vec![
                HeaderSegment::Literal(b"Date: ".to_vec()),
                HeaderSegment::Date(DateFormat::Ymd('/')),
            ],
        };

        let line = header.render(0, 1);
        let text = String::from_utf8_lossy(&line);
        // Should be "Date: YYYY/MM/DD" format
        assert!(text.starts_with("Date: "), "got: {}", text);
        assert_eq!(text.len(), 16); // "Date: " (6) + "YYYY/MM/DD" (10)
    }

    #[test]
    fn test_outfil_with_outrec() {
        let out1 = test_path("reformat");

        let outrec = OutrecSpec::new()
            .add_field(crate::reformat::OutrecField::Field {
                position: 3,
                length: 3,
            });

        let spec = OutfilSpec::new()
            .add_descriptor(OutfilDescriptor::new(&out1).with_outrec(outrec));

        let records: Vec<Vec<u8>> = vec![b"AABBB".to_vec(), b"CCDDD".to_vec()];

        spec.process(&records).unwrap();

        let output = fs::read_to_string(&out1).unwrap();
        assert_eq!(output, "BBB\nDDD\n");

        cleanup(&out1);
    }
}
