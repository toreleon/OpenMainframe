//! Symbol table for COBOL semantic analysis.
//!
//! The symbol table tracks all declared identifiers including:
//! - Data items (variables)
//! - Paragraphs and sections
//! - Files
//! - Indexes

use super::types::CobolType;
use crate::lexer::Span;
use std::collections::HashMap;

/// Unique identifier for a symbol.
pub type SymbolId = usize;

/// Kind of symbol in the symbol table.
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    /// Data item (variable).
    DataItem {
        /// Level number.
        level: u8,
        /// Parent symbol (for qualified references).
        parent: Option<SymbolId>,
        /// Child symbols (for group items).
        children: Vec<SymbolId>,
        /// Type information.
        cobol_type: CobolType,
        /// Byte offset from start of record/working storage.
        offset: u32,
    },
    /// Paragraph in PROCEDURE DIVISION.
    Paragraph {
        /// Containing section (if any).
        section: Option<SymbolId>,
    },
    /// Section in PROCEDURE DIVISION.
    Section {
        /// Paragraphs in this section.
        paragraphs: Vec<SymbolId>,
    },
    /// File (from FILE SECTION).
    File {
        /// Record symbols.
        records: Vec<SymbolId>,
    },
    /// Index defined by INDEXED BY clause.
    Index {
        /// Table this index is associated with.
        table: SymbolId,
    },
    /// Condition name (level 88).
    ConditionName {
        /// Parent data item.
        parent: SymbolId,
    },
}

/// A symbol in the symbol table.
#[derive(Debug, Clone)]
pub struct Symbol {
    /// Unique identifier.
    pub id: SymbolId,
    /// Symbol name.
    pub name: String,
    /// Kind of symbol.
    pub kind: SymbolKind,
    /// Source span where defined.
    pub span: Span,
}

/// Scope for symbol resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    /// Global scope (program level).
    Global,
    /// File section scope.
    FileSection,
    /// Working storage section.
    WorkingStorage,
    /// Local storage section.
    LocalStorage,
    /// Linkage section.
    Linkage,
    /// Procedure division.
    Procedure,
}

/// Symbol table for a COBOL program.
#[derive(Debug)]
pub struct SymbolTable {
    /// All symbols indexed by ID.
    symbols: Vec<Symbol>,
    /// Name to symbol mapping for data items.
    data_items: HashMap<String, Vec<SymbolId>>,
    /// Name to symbol mapping for paragraphs.
    paragraphs: HashMap<String, SymbolId>,
    /// Name to symbol mapping for sections.
    sections: HashMap<String, SymbolId>,
    /// Name to symbol mapping for files.
    files: HashMap<String, SymbolId>,
    /// Name to symbol mapping for indexes.
    indexes: HashMap<String, SymbolId>,
}

impl SymbolTable {
    /// Create a new empty symbol table.
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
            data_items: HashMap::new(),
            paragraphs: HashMap::new(),
            sections: HashMap::new(),
            files: HashMap::new(),
            indexes: HashMap::new(),
        }
    }

    /// Get the next symbol ID.
    fn next_id(&self) -> SymbolId {
        self.symbols.len()
    }

    /// Add a data item to the symbol table.
    pub fn add_data_item(
        &mut self,
        name: String,
        level: u8,
        parent: Option<SymbolId>,
        cobol_type: CobolType,
        offset: u32,
        span: Span,
    ) -> SymbolId {
        let id = self.next_id();
        let symbol = Symbol {
            id,
            name: name.clone(),
            kind: SymbolKind::DataItem {
                level,
                parent,
                children: Vec::new(),
                cobol_type,
                offset,
            },
            span,
        };
        self.symbols.push(symbol);
        self.data_items
            .entry(name.to_uppercase())
            .or_default()
            .push(id);
        id
    }

    /// Add a child to a group item.
    pub fn add_child(&mut self, parent_id: SymbolId, child_id: SymbolId) {
        if let Some(symbol) = self.symbols.get_mut(parent_id) {
            if let SymbolKind::DataItem { children, .. } = &mut symbol.kind {
                children.push(child_id);
            }
        }
    }

    /// Add a paragraph to the symbol table.
    pub fn add_paragraph(
        &mut self,
        name: String,
        section: Option<SymbolId>,
        span: Span,
    ) -> SymbolId {
        let id = self.next_id();
        let symbol = Symbol {
            id,
            name: name.clone(),
            kind: SymbolKind::Paragraph { section },
            span,
        };
        self.symbols.push(symbol);
        self.paragraphs.insert(name.to_uppercase(), id);
        id
    }

    /// Add a section to the symbol table.
    pub fn add_section(&mut self, name: String, span: Span) -> SymbolId {
        let id = self.next_id();
        let symbol = Symbol {
            id,
            name: name.clone(),
            kind: SymbolKind::Section {
                paragraphs: Vec::new(),
            },
            span,
        };
        self.symbols.push(symbol);
        self.sections.insert(name.to_uppercase(), id);
        id
    }

    /// Add a paragraph to a section.
    pub fn add_paragraph_to_section(&mut self, section_id: SymbolId, para_id: SymbolId) {
        if let Some(symbol) = self.symbols.get_mut(section_id) {
            if let SymbolKind::Section { paragraphs } = &mut symbol.kind {
                paragraphs.push(para_id);
            }
        }
    }

    /// Add a file to the symbol table.
    pub fn add_file(&mut self, name: String, span: Span) -> SymbolId {
        let id = self.next_id();
        let symbol = Symbol {
            id,
            name: name.clone(),
            kind: SymbolKind::File {
                records: Vec::new(),
            },
            span,
        };
        self.symbols.push(symbol);
        self.files.insert(name.to_uppercase(), id);
        id
    }

    /// Add a record to a file.
    pub fn add_record_to_file(&mut self, file_id: SymbolId, record_id: SymbolId) {
        if let Some(symbol) = self.symbols.get_mut(file_id) {
            if let SymbolKind::File { records } = &mut symbol.kind {
                records.push(record_id);
            }
        }
    }

    /// Add an index to the symbol table.
    pub fn add_index(&mut self, name: String, table: SymbolId, span: Span) -> SymbolId {
        let id = self.next_id();
        let symbol = Symbol {
            id,
            name: name.clone(),
            kind: SymbolKind::Index { table },
            span,
        };
        self.symbols.push(symbol);
        self.indexes.insert(name.to_uppercase(), id);
        id
    }

    /// Add a condition name (level 88) to the symbol table.
    pub fn add_condition_name(&mut self, name: String, parent: SymbolId, span: Span) -> SymbolId {
        let id = self.next_id();
        let symbol = Symbol {
            id,
            name: name.clone(),
            kind: SymbolKind::ConditionName { parent },
            span,
        };
        self.symbols.push(symbol);
        self.data_items
            .entry(name.to_uppercase())
            .or_default()
            .push(id);
        id
    }

    /// Look up a symbol by ID.
    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(id)
    }

    /// Look up a data item by name.
    /// Returns all matches (COBOL allows same names at different levels).
    pub fn lookup_data_item(&self, name: &str) -> Vec<&Symbol> {
        self.data_items
            .get(&name.to_uppercase())
            .map(|ids| ids.iter().filter_map(|id| self.symbols.get(*id)).collect())
            .unwrap_or_default()
    }

    /// Look up a data item with qualification (e.g., FIELD OF RECORD).
    pub fn lookup_qualified(&self, name: &str, qualifiers: &[String]) -> Option<&Symbol> {
        let candidates = self.lookup_data_item(name);
        if candidates.len() == 1 && qualifiers.is_empty() {
            return Some(candidates[0]);
        }

        // Find the candidate that matches the qualification chain
        candidates
            .into_iter()
            .find(|&candidate| self.matches_qualifiers(candidate.id, qualifiers))
    }

    /// Check if a symbol matches the given qualifier chain.
    fn matches_qualifiers(&self, symbol_id: SymbolId, qualifiers: &[String]) -> bool {
        if qualifiers.is_empty() {
            return true;
        }

        let symbol = match self.symbols.get(symbol_id) {
            Some(s) => s,
            None => return false,
        };

        let parent_id = match &symbol.kind {
            SymbolKind::DataItem { parent, .. } => *parent,
            _ => return false,
        };

        let parent_id = match parent_id {
            Some(id) => id,
            None => return false,
        };

        let parent = match self.symbols.get(parent_id) {
            Some(s) => s,
            None => return false,
        };

        if parent.name.eq_ignore_ascii_case(&qualifiers[0]) {
            self.matches_qualifiers(parent_id, &qualifiers[1..])
        } else {
            false
        }
    }

    /// Look up a paragraph by name.
    pub fn lookup_paragraph(&self, name: &str) -> Option<&Symbol> {
        self.paragraphs
            .get(&name.to_uppercase())
            .and_then(|id| self.symbols.get(*id))
    }

    /// Look up a section by name.
    pub fn lookup_section(&self, name: &str) -> Option<&Symbol> {
        self.sections
            .get(&name.to_uppercase())
            .and_then(|id| self.symbols.get(*id))
    }

    /// Look up a file by name.
    pub fn lookup_file(&self, name: &str) -> Option<&Symbol> {
        self.files
            .get(&name.to_uppercase())
            .and_then(|id| self.symbols.get(*id))
    }

    /// Look up an index by name.
    pub fn lookup_index(&self, name: &str) -> Option<&Symbol> {
        self.indexes
            .get(&name.to_uppercase())
            .and_then(|id| self.symbols.get(*id))
    }

    /// Check if a paragraph or section exists.
    pub fn procedure_target_exists(&self, name: &str) -> bool {
        let upper = name.to_uppercase();
        self.paragraphs.contains_key(&upper) || self.sections.contains_key(&upper)
    }

    /// Get all symbols.
    pub fn all_symbols(&self) -> &[Symbol] {
        &self.symbols
    }

    /// Get the number of symbols.
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    /// Check if the symbol table is empty.
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_span() -> Span {
        Span::new(crate::lexer::FileId::MAIN, 0, 0)
    }

    #[test]
    fn test_add_and_lookup_data_item() {
        let mut table = SymbolTable::new();
        let id = table.add_data_item(
            "WS-NAME".to_string(),
            1,
            None,
            CobolType::alphanumeric(20),
            0,
            make_span(),
        );

        let symbol = table.get(id).unwrap();
        assert_eq!(symbol.name, "WS-NAME");

        let found = table.lookup_data_item("WS-NAME");
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].id, id);

        // Case insensitive lookup
        let found = table.lookup_data_item("ws-name");
        assert_eq!(found.len(), 1);
    }

    #[test]
    fn test_qualified_lookup() {
        let mut table = SymbolTable::new();

        // Create a structure: CUSTOMER-RECORD / CUSTOMER-NAME
        let record_id = table.add_data_item(
            "CUSTOMER-RECORD".to_string(),
            1,
            None,
            CobolType::group(100),
            0,
            make_span(),
        );
        let name_id = table.add_data_item(
            "CUSTOMER-NAME".to_string(),
            5,
            Some(record_id),
            CobolType::alphanumeric(30),
            0,
            make_span(),
        );
        table.add_child(record_id, name_id);

        // Unqualified lookup should find it
        let found = table.lookup_data_item("CUSTOMER-NAME");
        assert_eq!(found.len(), 1);

        // Qualified lookup
        let found = table
            .lookup_qualified("CUSTOMER-NAME", &["CUSTOMER-RECORD".to_string()])
            .unwrap();
        assert_eq!(found.id, name_id);
    }

    #[test]
    fn test_duplicate_names_different_parents() {
        let mut table = SymbolTable::new();

        // Two records with same-named fields
        let rec1 = table.add_data_item(
            "RECORD-1".to_string(),
            1,
            None,
            CobolType::group(50),
            0,
            make_span(),
        );
        let rec2 = table.add_data_item(
            "RECORD-2".to_string(),
            1,
            None,
            CobolType::group(50),
            50,
            make_span(),
        );

        let field1 = table.add_data_item(
            "FIELD-A".to_string(),
            5,
            Some(rec1),
            CobolType::alphanumeric(10),
            0,
            make_span(),
        );
        let field2 = table.add_data_item(
            "FIELD-A".to_string(),
            5,
            Some(rec2),
            CobolType::alphanumeric(10),
            50,
            make_span(),
        );

        // Unqualified lookup returns both
        let found = table.lookup_data_item("FIELD-A");
        assert_eq!(found.len(), 2);

        // Qualified lookup returns the correct one
        let found = table
            .lookup_qualified("FIELD-A", &["RECORD-1".to_string()])
            .unwrap();
        assert_eq!(found.id, field1);

        let found = table
            .lookup_qualified("FIELD-A", &["RECORD-2".to_string()])
            .unwrap();
        assert_eq!(found.id, field2);
    }

    #[test]
    fn test_paragraph_and_section() {
        let mut table = SymbolTable::new();

        let section_id = table.add_section("MAIN-SECTION".to_string(), make_span());
        let para_id = table.add_paragraph("INIT-PARA".to_string(), Some(section_id), make_span());
        table.add_paragraph_to_section(section_id, para_id);

        assert!(table.procedure_target_exists("MAIN-SECTION"));
        assert!(table.procedure_target_exists("INIT-PARA"));
        assert!(!table.procedure_target_exists("NONEXISTENT"));

        let section = table.lookup_section("MAIN-SECTION").unwrap();
        if let SymbolKind::Section { paragraphs } = &section.kind {
            assert_eq!(paragraphs.len(), 1);
            assert_eq!(paragraphs[0], para_id);
        } else {
            panic!("Expected section kind");
        }
    }
}
