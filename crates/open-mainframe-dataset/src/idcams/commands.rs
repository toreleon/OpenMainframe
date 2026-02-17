//! IDCAMS command definitions.

use crate::vsam::VsamType;

/// IDCAMS commands.
#[derive(Debug, Clone)]
pub enum IdcamsCommand {
    /// DEFINE CLUSTER - Create a VSAM cluster.
    DefineCluster {
        /// Cluster name.
        name: String,
        /// Cluster type (KSDS, ESDS, RRDS).
        cluster_type: VsamType,
        /// Key specification (length, offset) for KSDS.
        keys: Option<(u16, u16)>,
        /// Record size (average, maximum).
        recordsize: Option<(u32, u32)>,
        /// Volumes.
        volumes: Option<Vec<String>>,
    },

    /// DEFINE GDG - Create a Generation Data Group.
    DefineGdg {
        /// GDG base name.
        name: String,
        /// Maximum number of generations.
        limit: u8,
        /// Delete old generations when rolling off.
        scratch: bool,
        /// Allow empty GDG.
        empty: bool,
    },

    /// DELETE - Delete datasets and catalog entries.
    Delete {
        /// Dataset name.
        name: String,
        /// Ignore retention date.
        purge: bool,
        /// Don't fail if not found.
        force: bool,
    },

    /// ALTER - Rename datasets.
    Alter {
        /// Old dataset name.
        name: String,
        /// New dataset name.
        newname: String,
    },

    /// LISTCAT - List catalog entries.
    Listcat {
        /// Specific entry name.
        entry: Option<String>,
        /// Level/prefix to list.
        level: Option<String>,
        /// Show all details.
        all: bool,
    },

    /// PRINT - Display dataset contents.
    Print {
        /// Dataset name.
        dataset: String,
        /// Character-only output.
        character: bool,
        /// Hex-only output.
        hex: bool,
        /// Records to skip.
        skip: usize,
        /// Number of records to print.
        count: usize,
    },

    /// REPRO - Copy datasets.
    Repro {
        /// Input dataset.
        indataset: String,
        /// Output dataset.
        outdataset: String,
        /// Starting key (for KSDS).
        fromkey: Option<String>,
        /// Ending key (for KSDS).
        tokey: Option<String>,
        /// Number of records to skip from the start.
        skip: usize,
        /// Maximum number of records to copy (0 = all).
        count: usize,
    },

    /// VERIFY - Verify VSAM integrity.
    Verify {
        /// Dataset name.
        dataset: String,
    },

    /// DEFINE ALTERNATEINDEX - Create a VSAM alternate index.
    DefineAix {
        /// AIX name.
        name: String,
        /// Base cluster name (RELATE).
        relate: String,
        /// Key specification (length, offset) for the alternate key.
        keys: (u16, u16),
        /// Whether alternate keys must be unique.
        unique_key: bool,
    },

    /// DEFINE PATH - Connect an AIX to its base cluster.
    DefinePath {
        /// Path name.
        name: String,
        /// AIX entry name (PATHENTRY).
        pathentry: String,
    },
}

/// Result of executing IDCAMS commands.
#[derive(Debug, Clone)]
pub struct IdcamsResult {
    /// Output messages.
    pub output: String,
    /// Return code (0 = success, 4 = warning, 8+ = error).
    pub return_code: u32,
}

impl IdcamsResult {
    /// Check if execution was successful.
    pub fn is_success(&self) -> bool {
        self.return_code == 0
    }

    /// Check if there were warnings.
    pub fn has_warnings(&self) -> bool {
        self.return_code == 4
    }

    /// Check if there were errors.
    pub fn has_errors(&self) -> bool {
        self.return_code >= 8
    }
}
