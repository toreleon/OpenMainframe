//! Sort engine implementation.

use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::path::Path;

use crate::error::SortError;
use crate::fields::SortSpec;
use crate::filter::FilterSpec;
use crate::reformat::OutrecSpec;

/// Maximum records to hold in memory for sorting.
#[allow(dead_code)]
const MAX_MEMORY_RECORDS: usize = 100_000;

/// Sort engine for processing records.
pub struct SortEngine {
    /// Sort specification.
    sort_spec: Option<SortSpec>,
    /// INCLUDE filter.
    include: Option<FilterSpec>,
    /// OMIT filter.
    omit: Option<FilterSpec>,
    /// INREC reformatting.
    inrec: Option<OutrecSpec>,
    /// OUTREC reformatting.
    outrec: Option<OutrecSpec>,
    /// SUM fields for duplicate handling.
    sum_fields: Option<Vec<(usize, usize, crate::fields::DataType)>>,
    /// Copy mode (no sorting).
    copy_mode: bool,
    /// Fixed record length (0 = variable/line-based).
    record_length: usize,
}

impl SortEngine {
    /// Creates a new sort engine with a sort specification.
    pub fn new(sort_spec: SortSpec) -> Self {
        Self {
            sort_spec: Some(sort_spec),
            include: None,
            omit: None,
            inrec: None,
            outrec: None,
            sum_fields: None,
            copy_mode: false,
            record_length: 0,
        }
    }

    /// Creates a sort engine for copy mode (no sorting).
    pub fn copy() -> Self {
        Self {
            sort_spec: None,
            include: None,
            omit: None,
            inrec: None,
            outrec: None,
            sum_fields: None,
            copy_mode: true,
            record_length: 0,
        }
    }

    /// Sets the INCLUDE filter.
    pub fn with_include(mut self, filter: FilterSpec) -> Self {
        self.include = Some(filter);
        self
    }

    /// Sets the OMIT filter.
    pub fn with_omit(mut self, filter: FilterSpec) -> Self {
        self.omit = Some(filter);
        self
    }

    /// Sets INREC reformatting.
    pub fn with_inrec(mut self, spec: OutrecSpec) -> Self {
        self.inrec = Some(spec);
        self
    }

    /// Sets OUTREC reformatting.
    pub fn with_outrec(mut self, spec: OutrecSpec) -> Self {
        self.outrec = Some(spec);
        self
    }

    /// Sets SUM fields.
    pub fn with_sum(mut self, fields: Vec<(usize, usize, crate::fields::DataType)>) -> Self {
        self.sum_fields = Some(fields);
        self
    }

    /// Sets fixed record length.
    pub fn with_record_length(mut self, length: usize) -> Self {
        self.record_length = length;
        self
    }

    /// Sorts a file.
    pub fn sort_file<P: AsRef<Path>>(&self, input: P, output: P) -> Result<SortStats, SortError> {
        let input_path = input.as_ref();
        let output_path = output.as_ref();

        // Read records
        let mut records = self.read_records(input_path)?;
        let input_count = records.len();

        // Apply filters
        if let Some(ref filter) = self.include {
            records.retain(|r| filter.should_include(r));
        }
        if let Some(ref filter) = self.omit {
            records.retain(|r| filter.should_include(r));
        }
        let after_filter = records.len();

        // Apply INREC
        if let Some(ref inrec) = self.inrec {
            records = records.into_iter().map(|r| inrec.reformat(&r)).collect();
        }

        // Sort (unless copy mode)
        if !self.copy_mode {
            if let Some(ref spec) = self.sort_spec {
                records.sort_by(|a, b| spec.compare(a, b));
            }
        }

        // Apply SUM for duplicates
        if let Some(ref sum_fields) = self.sum_fields {
            records = self.apply_sum(&records, sum_fields);
        }
        let after_sum = records.len();

        // Apply OUTREC
        if let Some(ref outrec) = self.outrec {
            records = records.into_iter().map(|r| outrec.reformat(&r)).collect();
        }

        // Write output
        self.write_records(output_path, &records)?;

        Ok(SortStats {
            input_records: input_count,
            filtered_records: input_count - after_filter,
            summed_records: after_filter - after_sum,
            output_records: after_sum,
        })
    }

    /// Reads records from a file.
    fn read_records(&self, path: &Path) -> Result<Vec<Vec<u8>>, SortError> {
        let file = File::open(path)?;
        let mut records = Vec::new();

        if self.record_length > 0 {
            // Fixed-length records
            let mut reader = BufReader::new(file);
            let mut buffer = vec![0u8; self.record_length];

            loop {
                match reader.read_exact(&mut buffer) {
                    Ok(()) => records.push(buffer.clone()),
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                    Err(e) => return Err(e.into()),
                }
            }
        } else {
            // Line-based records
            let reader = BufReader::new(file);
            for line in reader.lines() {
                records.push(line?.into_bytes());
            }
        }

        Ok(records)
    }

    /// Writes records to a file.
    fn write_records(&self, path: &Path, records: &[Vec<u8>]) -> Result<(), SortError> {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)?;
        let mut writer = BufWriter::new(file);

        if self.record_length > 0 {
            // Fixed-length records
            for record in records {
                writer.write_all(record)?;
            }
        } else {
            // Line-based records
            for record in records {
                writer.write_all(record)?;
                writer.write_all(b"\n")?;
            }
        }

        writer.flush()?;
        Ok(())
    }

    /// Apply SUM processing for duplicate keys.
    fn apply_sum(
        &self,
        records: &[Vec<u8>],
        _sum_fields: &[(usize, usize, crate::fields::DataType)],
    ) -> Vec<Vec<u8>> {
        if records.is_empty() {
            return Vec::new();
        }

        let sort_spec = match &self.sort_spec {
            Some(spec) => spec,
            None => return records.to_vec(),
        };

        let mut result = Vec::new();
        let mut current_key_record: Option<Vec<u8>> = None;

        for record in records {
            match &current_key_record {
                None => {
                    current_key_record = Some(record.clone());
                }
                Some(prev) => {
                    if sort_spec.compare(prev, record) == std::cmp::Ordering::Equal {
                        // Duplicate key - for now, just keep first (FIELDS=NONE behavior)
                        // TODO: Implement actual summation for numeric fields
                    } else {
                        result.push(prev.clone());
                        current_key_record = Some(record.clone());
                    }
                }
            }
        }

        if let Some(last) = current_key_record {
            result.push(last);
        }

        result
    }

    /// Merges multiple pre-sorted files.
    pub fn merge_files<P: AsRef<Path>>(
        &self,
        inputs: &[P],
        output: P,
    ) -> Result<SortStats, SortError> {
        if inputs.is_empty() {
            return Err(SortError::MissingInput("No input files for merge".to_string()));
        }

        let sort_spec = self.sort_spec.as_ref().ok_or_else(|| {
            SortError::MissingInput("Sort specification required for merge".to_string())
        })?;

        // Open all input files
        let mut readers: Vec<_> = inputs
            .iter()
            .map(|p| {
                let file = File::open(p.as_ref())?;
                Ok(BufReader::new(file))
            })
            .collect::<Result<Vec<_>, SortError>>()?;

        // Read first record from each
        let mut current_records: Vec<Option<Vec<u8>>> = readers
            .iter_mut()
            .map(|r| {
                let mut line = String::new();
                match r.read_line(&mut line) {
                    Ok(0) => None,
                    Ok(_) => Some(line.trim_end().as_bytes().to_vec()),
                    Err(_) => None,
                }
            })
            .collect();

        let output_file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(output.as_ref())?;
        let mut writer = BufWriter::new(output_file);

        let mut output_count = 0;

        loop {
            // Find the smallest record
            let mut min_idx: Option<usize> = None;
            for (i, rec) in current_records.iter().enumerate() {
                if let Some(ref record) = rec {
                    match min_idx {
                        None => min_idx = Some(i),
                        Some(idx) => {
                            if let Some(ref min_rec) = current_records[idx] {
                                if sort_spec.compare(record, min_rec) == std::cmp::Ordering::Less {
                                    min_idx = Some(i);
                                }
                            }
                        }
                    }
                }
            }

            match min_idx {
                None => break, // All inputs exhausted
                Some(idx) => {
                    // Write the record
                    let record = current_records[idx].take().unwrap();

                    // Apply OUTREC if specified
                    let output_rec = if let Some(ref outrec) = self.outrec {
                        outrec.reformat(&record)
                    } else {
                        record
                    };

                    writer.write_all(&output_rec)?;
                    writer.write_all(b"\n")?;
                    output_count += 1;

                    // Read next record from this input
                    let mut line = String::new();
                    current_records[idx] = match readers[idx].read_line(&mut line) {
                        Ok(0) => None,
                        Ok(_) => Some(line.trim_end().as_bytes().to_vec()),
                        Err(_) => None,
                    };
                }
            }
        }

        writer.flush()?;

        Ok(SortStats {
            input_records: 0, // Unknown without reading all files
            filtered_records: 0,
            summed_records: 0,
            output_records: output_count,
        })
    }
}

/// Statistics from a sort operation.
#[derive(Debug, Default)]
pub struct SortStats {
    /// Number of input records.
    pub input_records: usize,
    /// Number of records filtered out.
    pub filtered_records: usize,
    /// Number of records removed by SUM.
    pub summed_records: usize,
    /// Number of output records.
    pub output_records: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fields::{DataType, SortField, SortOrder};
    use std::fs;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_path(name: &str) -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, AtomicOrdering::SeqCst);
        std::env::temp_dir().join(format!("sort_test_{}_{}", name, count))
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_sort_simple() {
        let input_path = test_path("input.dat");
        let output_path = test_path("output.dat");

        // Create input file with consistent field widths
        fs::write(&input_path, "Charlie\nAlpha..\nBravo..\nDelta..\n").unwrap();

        // Sort on first 7 characters
        let spec = SortSpec::new()
            .add_field(SortField::new(1, 7, DataType::Character, SortOrder::Ascending));
        let engine = SortEngine::new(spec);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.input_records, 4);
        assert_eq!(stats.output_records, 4);

        // Verify output
        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "Alpha..\nBravo..\nCharlie\nDelta..\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_sort_descending() {
        let input_path = test_path("input_desc.dat");
        let output_path = test_path("output_desc.dat");

        fs::write(&input_path, "A\nC\nB\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 1, DataType::Character, SortOrder::Descending));
        let engine = SortEngine::new(spec);
        engine.sort_file(&input_path, &output_path).unwrap();

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "C\nB\nA\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_copy_mode() {
        let input_path = test_path("input_copy.dat");
        let output_path = test_path("output_copy.dat");

        fs::write(&input_path, "Three\nOne\nTwo\n").unwrap();

        let engine = SortEngine::copy();
        engine.sort_file(&input_path, &output_path).unwrap();

        // Should preserve original order
        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "Three\nOne\nTwo\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_with_filter() {
        let input_path = test_path("input_filter.dat");
        let output_path = test_path("output_filter.dat");

        fs::write(&input_path, "NY123\nCA456\nNY789\nTX000\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 5, DataType::Character, SortOrder::Ascending));

        let filter = crate::filter::FilterSpec {
            filter_type: crate::filter::FilterType::Include,
            conditions: vec![crate::filter::Condition {
                position: 1,
                length: 2,
                data_type: DataType::Character,
                op: crate::filter::CompareOp::Eq,
                value: b"NY".to_vec(),
            }],
            logic: None,
        };

        let engine = SortEngine::new(spec).with_include(filter);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.input_records, 4);
        assert_eq!(stats.filtered_records, 2);
        assert_eq!(stats.output_records, 2);

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "NY123\nNY789\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_with_outrec() {
        let input_path = test_path("input_outrec.dat");
        let output_path = test_path("output_outrec.dat");

        fs::write(&input_path, "HelloWorld\nAlphaBravo\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 5, DataType::Character, SortOrder::Ascending));

        let outrec = OutrecSpec::new()
            .add_field(crate::reformat::OutrecField::Field { position: 6, length: 5 })
            .add_field(crate::reformat::OutrecField::Literal(b"-".to_vec()))
            .add_field(crate::reformat::OutrecField::Field { position: 1, length: 5 });

        let engine = SortEngine::new(spec).with_outrec(outrec);
        engine.sort_file(&input_path, &output_path).unwrap();

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "Bravo-Alpha\nWorld-Hello\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }
}
