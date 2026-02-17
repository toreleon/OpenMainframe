//! VSAM Alternate Index (AIX) support.
//!
//! An alternate index provides a secondary access path to a KSDS or ESDS
//! base cluster, enabling record lookup by an alternate key field.
//!
//! # Architecture
//!
//! Each AIX is backed by a secondary B+ tree where:
//! - Key: the alternate key value (extracted from records)
//! - Value: one or more primary keys (for unique and non-unique AIXs)
//!
//! AIX entries are automatically maintained when the base cluster is modified.
//! A PATH object connects an AIX to its base cluster for transparent access.

use super::btree::BPlusTree;
use super::cluster::KeySpec;
use crate::error::DatasetError;

/// Definition of an alternate index.
///
/// Specifies how alternate key values are extracted from base cluster
/// records and whether duplicate alternate key values are allowed.
#[derive(Debug, Clone)]
pub struct AixDefinition {
    /// Name of this alternate index (e.g., "MY.AIX").
    pub name: String,
    /// Name of the base cluster this AIX relates to.
    pub base_cluster: String,
    /// Key specification for extracting the alternate key from records.
    pub key_spec: KeySpec,
    /// Whether alternate keys must be unique.
    pub unique_key: bool,
}

/// An alternate index backed by a secondary B+ tree.
///
/// Maps alternate key values to lists of primary keys.
/// For unique AIXs, each alternate key maps to exactly one primary key.
/// For non-unique AIXs, each alternate key may map to multiple primary keys.
#[derive(Debug)]
pub struct AlternateIndex {
    /// AIX definition.
    definition: AixDefinition,
    /// Secondary B+ tree: alternate key → list of primary keys.
    index: BPlusTree<Vec<u8>, Vec<Vec<u8>>>,
}

impl AlternateIndex {
    /// Creates a new empty alternate index.
    pub fn new(definition: AixDefinition) -> Self {
        Self {
            definition,
            index: BPlusTree::new(),
        }
    }

    /// Returns the AIX definition.
    pub fn definition(&self) -> &AixDefinition {
        &self.definition
    }

    /// Returns the AIX name.
    pub fn name(&self) -> &str {
        &self.definition.name
    }

    /// Returns the base cluster name.
    pub fn base_cluster(&self) -> &str {
        &self.definition.base_cluster
    }

    /// Returns the key specification.
    pub fn key_spec(&self) -> &KeySpec {
        &self.definition.key_spec
    }

    /// Returns whether the AIX requires unique keys.
    pub fn is_unique(&self) -> bool {
        self.definition.unique_key
    }

    /// Looks up primary keys by alternate key value.
    ///
    /// For unique AIXs, returns at most one primary key.
    /// For non-unique AIXs, may return multiple primary keys.
    pub fn lookup(&self, alt_key: &[u8]) -> Option<&Vec<Vec<u8>>> {
        self.index.get(&alt_key.to_vec())
    }

    /// Returns the number of alternate key entries.
    pub fn entry_count(&self) -> usize {
        self.index.iter().len()
    }

    /// Adds an entry mapping alternate key → primary key.
    ///
    /// For unique AIXs, returns an error if the alternate key already exists
    /// with a different primary key.
    pub fn insert(
        &mut self,
        record: &[u8],
        primary_key: &[u8],
    ) -> Result<(), DatasetError> {
        let alt_key = self.definition.key_spec.extract_key(record).to_vec();
        let pk = primary_key.to_vec();

        match self.index.get(&alt_key) {
            Some(existing) => {
                if self.definition.unique_key {
                    // Unique AIX: duplicate alternate key is an error
                    if !existing.contains(&pk) {
                        return Err(DatasetError::AlreadyExists {
                            name: format!(
                                "Duplicate alternate key in unique AIX '{}'",
                                self.definition.name
                            ),
                        });
                    }
                    // Same primary key, no-op
                    Ok(())
                } else {
                    // Non-unique AIX: add to the list if not already present
                    let mut keys = existing.clone();
                    if !keys.contains(&pk) {
                        keys.push(pk);
                        self.index.insert(alt_key, keys);
                    }
                    Ok(())
                }
            }
            None => {
                self.index.insert(alt_key, vec![pk]);
                Ok(())
            }
        }
    }

    /// Removes an entry for the given record and primary key.
    ///
    /// If the alternate key has multiple primary keys (non-unique AIX),
    /// only the specified primary key is removed. If it's the last one,
    /// the entire alternate key entry is removed.
    pub fn remove(
        &mut self,
        record: &[u8],
        primary_key: &[u8],
    ) {
        let alt_key = self.definition.key_spec.extract_key(record).to_vec();
        let pk = primary_key.to_vec();

        if let Some(existing) = self.index.get(&alt_key) {
            let mut keys = existing.clone();
            keys.retain(|k| k != &pk);

            if keys.is_empty() {
                self.index.remove(&alt_key);
            } else {
                self.index.insert(alt_key, keys);
            }
        }
    }

    /// Updates the AIX when a base cluster record is rewritten.
    ///
    /// If the alternate key field changed, removes the old entry
    /// and creates a new one. If unchanged, this is a no-op.
    pub fn update(
        &mut self,
        old_record: &[u8],
        new_record: &[u8],
        primary_key: &[u8],
    ) -> Result<(), DatasetError> {
        let old_alt_key = self.definition.key_spec.extract_key(old_record);
        let new_alt_key = self.definition.key_spec.extract_key(new_record);

        if old_alt_key == new_alt_key {
            // Alternate key didn't change, nothing to do
            return Ok(());
        }

        // Remove old entry
        self.remove(old_record, primary_key);

        // Insert new entry
        self.insert(new_record, primary_key)
    }

    /// Returns all entries as an iterator of (alternate_key, primary_keys).
    pub fn iter(&self) -> Vec<(Vec<u8>, Vec<Vec<u8>>)> {
        self.index.iter()
    }
}

/// A PATH connects an AIX to its base cluster for transparent access.
///
/// When reading through a PATH, the AIX key is used for lookup,
/// and the base cluster records are returned.
#[derive(Debug, Clone)]
pub struct VsamPath {
    /// Path name (e.g., "MY.PATH").
    pub name: String,
    /// AIX name this path uses (e.g., "MY.AIX").
    pub aix_name: String,
}

impl VsamPath {
    /// Creates a new PATH definition.
    pub fn new(name: impl Into<String>, aix_name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            aix_name: aix_name.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_definition(unique: bool) -> AixDefinition {
        AixDefinition {
            name: "TEST.AIX".to_string(),
            base_cluster: "TEST.BASE".to_string(),
            key_spec: KeySpec {
                offset: 10,
                length: 30,
            },
            unique_key: unique,
        }
    }

    fn make_record(primary_key: &[u8], alt_key: &[u8]) -> Vec<u8> {
        let mut record = vec![0u8; 100];
        let pk_len = primary_key.len().min(10);
        record[0..pk_len].copy_from_slice(&primary_key[..pk_len]);
        let ak_len = alt_key.len().min(30);
        record[10..10 + ak_len].copy_from_slice(&alt_key[..ak_len]);
        record
    }

    // === Story 600.1: Alternate Index Data Structure ===

    #[test]
    fn test_aix_unique_key_secondary_btree() {
        let def = make_definition(true);
        let mut aix = AlternateIndex::new(def);

        let record = make_record(b"PK001", b"SMITH");
        aix.insert(&record, b"PK001").unwrap();

        // Lookup by alternate key
        let result = aix.lookup(b"SMITH\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        assert!(result.is_some());
        let pks = result.unwrap();
        assert_eq!(pks.len(), 1);
        assert_eq!(&pks[0], b"PK001");
    }

    #[test]
    fn test_aix_non_unique_multiple_primary_keys() {
        let def = make_definition(false);
        let mut aix = AlternateIndex::new(def);

        let record1 = make_record(b"PK001", b"SMITH");
        let record2 = make_record(b"PK002", b"SMITH");

        aix.insert(&record1, b"PK001").unwrap();
        aix.insert(&record2, b"PK002").unwrap();

        // Both should be under the same alternate key
        let alt_key = &record1[10..40]; // Extract 30 bytes at offset 10
        let result = aix.lookup(alt_key);
        assert!(result.is_some());
        let pks = result.unwrap();
        assert_eq!(pks.len(), 2);
        assert!(pks.contains(&b"PK001".to_vec()));
        assert!(pks.contains(&b"PK002".to_vec()));
    }

    #[test]
    fn test_aix_unique_duplicate_error() {
        let def = make_definition(true);
        let mut aix = AlternateIndex::new(def);

        let record1 = make_record(b"PK001", b"SMITH");
        let record2 = make_record(b"PK002", b"SMITH");

        aix.insert(&record1, b"PK001").unwrap();

        // Second insert with same alt key but different PK should fail
        let result = aix.insert(&record2, b"PK002");
        assert!(result.is_err());
    }

    // === Story 600.2: Automatic AIX Maintenance ===

    #[test]
    fn test_aix_insert_creates_entry() {
        let def = make_definition(true);
        let mut aix = AlternateIndex::new(def);

        assert_eq!(aix.entry_count(), 0);

        let record = make_record(b"PK001", b"JONES");
        aix.insert(&record, b"PK001").unwrap();

        assert_eq!(aix.entry_count(), 1);
    }

    #[test]
    fn test_aix_update_changes_alt_key() {
        let def = make_definition(true);
        let mut aix = AlternateIndex::new(def);

        let old_record = make_record(b"PK001", b"SMITH");
        aix.insert(&old_record, b"PK001").unwrap();

        // Update: alt key changes from SMITH to JONES
        let new_record = make_record(b"PK001", b"JONES");
        aix.update(&old_record, &new_record, b"PK001").unwrap();

        // Old alternate key should be gone
        let old_alt_key = &old_record[10..40];
        assert!(aix.lookup(old_alt_key).is_none());

        // New alternate key should exist
        let new_alt_key = &new_record[10..40];
        let result = aix.lookup(new_alt_key);
        assert!(result.is_some());
        assert_eq!(result.unwrap().len(), 1);
    }

    #[test]
    fn test_aix_delete_removes_entry() {
        let def = make_definition(true);
        let mut aix = AlternateIndex::new(def);

        let record = make_record(b"PK001", b"SMITH");
        aix.insert(&record, b"PK001").unwrap();
        assert_eq!(aix.entry_count(), 1);

        // Delete
        aix.remove(&record, b"PK001");
        assert_eq!(aix.entry_count(), 0);
    }

    #[test]
    fn test_aix_non_unique_remove_one_pk() {
        let def = make_definition(false);
        let mut aix = AlternateIndex::new(def);

        let record1 = make_record(b"PK001", b"SMITH");
        let record2 = make_record(b"PK002", b"SMITH");
        aix.insert(&record1, b"PK001").unwrap();
        aix.insert(&record2, b"PK002").unwrap();

        // Remove just PK001
        aix.remove(&record1, b"PK001");

        // Alt key should still exist with PK002
        let alt_key = &record1[10..40];
        let result = aix.lookup(alt_key);
        assert!(result.is_some());
        let pks = result.unwrap();
        assert_eq!(pks.len(), 1);
        assert_eq!(&pks[0], b"PK002");
    }

    // === Story 600.3: PATH definition ===

    #[test]
    fn test_vsam_path_creation() {
        let path = VsamPath::new("MY.PATH", "MY.AIX");
        assert_eq!(path.name, "MY.PATH");
        assert_eq!(path.aix_name, "MY.AIX");
    }
}
