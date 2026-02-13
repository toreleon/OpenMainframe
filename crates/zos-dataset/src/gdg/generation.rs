//! GDG generation representation.

use std::cmp::Ordering;
use std::fmt;
use std::path::{Path, PathBuf};

use crate::error::DatasetError;

/// Generation number in GxxxxVyy format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GenerationNumber {
    /// Generation number (1-9999).
    pub generation: u16,
    /// Version number (0-99).
    pub version: u8,
}

impl GenerationNumber {
    /// Create a new generation number.
    pub fn new(generation: u16, version: u8) -> Result<Self, DatasetError> {
        if generation == 0 || generation > 9999 {
            return Err(DatasetError::InvalidParameter(format!(
                "Generation number must be 1-9999, got {}",
                generation
            )));
        }
        if version > 99 {
            return Err(DatasetError::InvalidParameter(format!(
                "Version must be 0-99, got {}",
                version
            )));
        }
        Ok(Self { generation, version })
    }

    /// Parse a generation number from GxxxxVyy suffix.
    pub fn parse(suffix: &str) -> Result<Self, DatasetError> {
        // Expected format: GxxxxVyy or Gxxxx
        let suffix = suffix.to_uppercase();
        if !suffix.starts_with('G') {
            return Err(DatasetError::InvalidParameter(format!(
                "Generation suffix must start with G: {}",
                suffix
            )));
        }

        let rest = &suffix[1..];

        // Find V separator (optional)
        let (gen_str, ver_str) = if let Some(v_pos) = rest.find('V') {
            (&rest[..v_pos], &rest[v_pos + 1..])
        } else {
            (rest, "00")
        };

        let generation: u16 = gen_str.parse().map_err(|_| {
            DatasetError::InvalidParameter(format!("Invalid generation number: {}", gen_str))
        })?;

        let version: u8 = ver_str.parse().map_err(|_| {
            DatasetError::InvalidParameter(format!("Invalid version number: {}", ver_str))
        })?;

        Self::new(generation, version)
    }

    /// Format as GxxxxVyy string.
    pub fn to_suffix(&self) -> String {
        format!("G{:04}V{:02}", self.generation, self.version)
    }

    /// Get the next generation number (version resets to 0).
    pub fn next_generation(&self) -> Result<Self, DatasetError> {
        Self::new(self.generation.saturating_add(1), 0)
    }

    /// Get the next version of this generation.
    pub fn next_version(&self) -> Result<Self, DatasetError> {
        Self::new(self.generation, self.version.saturating_add(1))
    }
}

impl Ord for GenerationNumber {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.generation.cmp(&other.generation) {
            Ordering::Equal => self.version.cmp(&other.version),
            other => other,
        }
    }
}

impl PartialOrd for GenerationNumber {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for GenerationNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_suffix())
    }
}

/// A GDG generation dataset.
#[derive(Debug, Clone)]
pub struct GdgGeneration {
    /// Base name of the GDG.
    base_name: String,
    /// Generation number.
    number: GenerationNumber,
    /// Physical path to the generation file.
    path: PathBuf,
    /// Creation timestamp (Unix seconds).
    created: u64,
}

impl GdgGeneration {
    /// Create a new generation reference.
    pub fn new(
        base_name: &str,
        number: GenerationNumber,
        path: impl AsRef<Path>,
    ) -> Self {
        Self {
            base_name: base_name.to_uppercase(),
            number,
            path: path.as_ref().to_path_buf(),
            created: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0),
        }
    }

    /// Create with explicit timestamp.
    pub fn with_timestamp(
        base_name: &str,
        number: GenerationNumber,
        path: impl AsRef<Path>,
        created: u64,
    ) -> Self {
        Self {
            base_name: base_name.to_uppercase(),
            number,
            path: path.as_ref().to_path_buf(),
            created,
        }
    }

    /// Get the full dataset name (BASE.GxxxxVyy).
    pub fn name(&self) -> String {
        format!("{}.{}", self.base_name, self.number.to_suffix())
    }

    /// Get the base name.
    pub fn base_name(&self) -> &str {
        &self.base_name
    }

    /// Get the generation number.
    pub fn number(&self) -> GenerationNumber {
        self.number
    }

    /// Get the physical path.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Get the creation timestamp.
    pub fn created(&self) -> u64 {
        self.created
    }

    /// Check if the generation file exists.
    pub fn exists(&self) -> bool {
        self.path.exists()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generation_number_new() {
        let gen = GenerationNumber::new(1, 0).unwrap();
        assert_eq!(gen.generation, 1);
        assert_eq!(gen.version, 0);

        let gen = GenerationNumber::new(9999, 99).unwrap();
        assert_eq!(gen.generation, 9999);
        assert_eq!(gen.version, 99);

        assert!(GenerationNumber::new(0, 0).is_err());
        assert!(GenerationNumber::new(10000, 0).is_err());
        assert!(GenerationNumber::new(1, 100).is_err());
    }

    #[test]
    fn test_generation_number_parse() {
        let gen = GenerationNumber::parse("G0001V00").unwrap();
        assert_eq!(gen.generation, 1);
        assert_eq!(gen.version, 0);

        let gen = GenerationNumber::parse("G1234V56").unwrap();
        assert_eq!(gen.generation, 1234);
        assert_eq!(gen.version, 56);

        let gen = GenerationNumber::parse("G0005").unwrap();
        assert_eq!(gen.generation, 5);
        assert_eq!(gen.version, 0);

        assert!(GenerationNumber::parse("X0001V00").is_err());
        assert!(GenerationNumber::parse("Gabc").is_err());
    }

    #[test]
    fn test_generation_number_suffix() {
        let gen = GenerationNumber::new(1, 0).unwrap();
        assert_eq!(gen.to_suffix(), "G0001V00");

        let gen = GenerationNumber::new(1234, 56).unwrap();
        assert_eq!(gen.to_suffix(), "G1234V56");
    }

    #[test]
    fn test_generation_number_ordering() {
        let g1v0 = GenerationNumber::new(1, 0).unwrap();
        let g1v1 = GenerationNumber::new(1, 1).unwrap();
        let g2v0 = GenerationNumber::new(2, 0).unwrap();

        assert!(g1v0 < g1v1);
        assert!(g1v1 < g2v0);
        assert!(g1v0 < g2v0);
    }

    #[test]
    fn test_generation_next() {
        let gen = GenerationNumber::new(1, 0).unwrap();

        let next_gen = gen.next_generation().unwrap();
        assert_eq!(next_gen.generation, 2);
        assert_eq!(next_gen.version, 0);

        let next_ver = gen.next_version().unwrap();
        assert_eq!(next_ver.generation, 1);
        assert_eq!(next_ver.version, 1);
    }

    #[test]
    fn test_gdg_generation_name() {
        let gen = GdgGeneration::new(
            "MY.GDG.BASE",
            GenerationNumber::new(1, 0).unwrap(),
            "/data/gen1.dat",
        );

        assert_eq!(gen.name(), "MY.GDG.BASE.G0001V00");
        assert_eq!(gen.base_name(), "MY.GDG.BASE");
    }
}
