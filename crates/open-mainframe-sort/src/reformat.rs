//! INREC/OUTREC record reformatting.

/// A field in an OUTREC/INREC specification.
#[derive(Debug, Clone)]
pub enum OutrecField {
    /// Copy from input record (position, length).
    Field { position: usize, length: usize },
    /// Insert literal bytes.
    Literal(Vec<u8>),
    /// Insert spaces.
    Spaces(usize),
    /// Insert zeros.
    Zeros(usize),
}

impl OutrecField {
    /// Returns the output length of this field.
    pub fn output_length(&self) -> usize {
        match self {
            OutrecField::Field { length, .. } => *length,
            OutrecField::Literal(bytes) => bytes.len(),
            OutrecField::Spaces(n) => *n,
            OutrecField::Zeros(n) => *n,
        }
    }

    /// Applies this field to an input record, writing to output.
    pub fn apply(&self, input: &[u8], output: &mut Vec<u8>) {
        match self {
            OutrecField::Field { position, length } => {
                let start = position.saturating_sub(1);
                let end = (start + length).min(input.len());

                if start < input.len() {
                    output.extend_from_slice(&input[start..end]);
                    // Pad with spaces if field extends beyond input
                    let written = end - start;
                    if written < *length {
                        output.extend(std::iter::repeat(b' ').take(length - written));
                    }
                } else {
                    // Field is entirely beyond input - fill with spaces
                    output.extend(std::iter::repeat(b' ').take(*length));
                }
            }
            OutrecField::Literal(bytes) => {
                output.extend_from_slice(bytes);
            }
            OutrecField::Spaces(n) => {
                output.extend(std::iter::repeat(b' ').take(*n));
            }
            OutrecField::Zeros(n) => {
                output.extend(std::iter::repeat(b'0').take(*n));
            }
        }
    }
}

/// Complete OUTREC/INREC specification.
#[derive(Debug, Clone, Default)]
pub struct OutrecSpec {
    /// Fields in output order.
    pub fields: Vec<OutrecField>,
}

impl OutrecSpec {
    /// Creates a new empty specification.
    pub fn new() -> Self {
        Self { fields: Vec::new() }
    }

    /// Adds a field.
    pub fn add_field(mut self, field: OutrecField) -> Self {
        self.fields.push(field);
        self
    }

    /// Returns the total output record length.
    pub fn output_length(&self) -> usize {
        self.fields.iter().map(|f| f.output_length()).sum()
    }

    /// Reformats an input record according to the specification.
    pub fn reformat(&self, input: &[u8]) -> Vec<u8> {
        let mut output = Vec::with_capacity(self.output_length());

        for field in &self.fields {
            field.apply(input, &mut output);
        }

        output
    }

    /// Returns true if this is a valid (non-empty) specification.
    pub fn is_valid(&self) -> bool {
        !self.fields.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_field_copy() {
        let field = OutrecField::Field { position: 1, length: 5 };
        let mut output = Vec::new();
        field.apply(b"Hello World", &mut output);
        assert_eq!(output, b"Hello");
    }

    #[test]
    fn test_field_copy_middle() {
        let field = OutrecField::Field { position: 7, length: 5 };
        let mut output = Vec::new();
        field.apply(b"Hello World", &mut output);
        assert_eq!(output, b"World");
    }

    #[test]
    fn test_field_copy_pad() {
        let field = OutrecField::Field { position: 10, length: 5 };
        let mut output = Vec::new();
        field.apply(b"Hello World", &mut output);
        assert_eq!(output, b"ld   "); // "ld" + 3 spaces
    }

    #[test]
    fn test_literal() {
        let field = OutrecField::Literal(b"***".to_vec());
        let mut output = Vec::new();
        field.apply(b"anything", &mut output);
        assert_eq!(output, b"***");
    }

    #[test]
    fn test_spaces() {
        let field = OutrecField::Spaces(3);
        let mut output = Vec::new();
        field.apply(b"anything", &mut output);
        assert_eq!(output, b"   ");
    }

    #[test]
    fn test_outrec_spec() {
        let spec = OutrecSpec::new()
            .add_field(OutrecField::Field { position: 1, length: 5 })
            .add_field(OutrecField::Literal(b"-".to_vec()))
            .add_field(OutrecField::Field { position: 7, length: 5 });

        let result = spec.reformat(b"Hello World");
        assert_eq!(result, b"Hello-World");
    }

    #[test]
    fn test_outrec_reorder() {
        // Take positions 11-15, then 1-5
        let spec = OutrecSpec::new()
            .add_field(OutrecField::Field { position: 7, length: 5 })
            .add_field(OutrecField::Field { position: 1, length: 5 });

        let result = spec.reformat(b"Hello World");
        assert_eq!(result, b"WorldHello");
    }
}
