//! z/OSMF dataset REST API types.

use serde::{Deserialize, Serialize};

/// Response body for dataset list operations.
///
/// Note: `totalRows` is returned via the `X-IBM-Response-Rows` header, not in the body.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DatasetListResponse {
    /// List of dataset items.
    pub items: Vec<DatasetListItem>,
    /// Number of rows returned.
    pub returned_rows: usize,
    /// JSON format version.
    #[serde(rename = "JSONversion")]
    pub json_version: i32,
}

/// A single dataset in a list response — field names match z/OSMF spec.
///
/// Numeric fields are serialized as JSON strings per z/OSMF convention.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatasetListItem {
    /// Dataset name.
    pub dsname: String,
    /// Dataset organization (PS, PO, VS, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dsorg: Option<String>,
    /// Record format (F, FB, V, VB, U).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recfm: Option<String>,
    /// Logical record length (as string per z/OSMF convention).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lrecl: Option<String>,
    /// Block size (as string per z/OSMF convention).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blksz: Option<String>,
    /// Volume serial.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vol: Option<String>,
    /// Creation date.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cdate: Option<String>,
    /// Last reference date.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rdate: Option<String>,
    /// Catalog name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub catnm: Option<String>,
    /// Device type.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dev: Option<String>,
    /// Expiration date.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub edate: Option<String>,
    /// Extents used.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extx: Option<u32>,
    /// Migration status ("YES" / "NO").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub migr: Option<String>,
    /// Multi-volume flag.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mvol: Option<String>,
    /// Overflow status.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ovf: Option<String>,
    /// Allocated size in tracks.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sizex: Option<u32>,
    /// Space unit (TRACKS / CYLINDERS / BLOCKS).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub spacu: Option<String>,
    /// Percentage used.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub used: Option<u32>,
    /// Volume list.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vols: Option<String>,
    /// Dataset name type (PDS, PDSE, LIBRARY, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dsntp: Option<String>,
}

/// Response body for PDS member list operations.
///
/// Note: `totalRows` is returned via the `X-IBM-Response-Rows` header, not in the body.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MemberListResponse {
    /// List of member items.
    pub items: Vec<MemberListItem>,
    /// Number of rows returned.
    pub returned_rows: usize,
    /// JSON format version.
    #[serde(rename = "JSONversion")]
    pub json_version: i32,
}

/// A PDS member entry with optional ISPF statistics.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemberListItem {
    /// Member name.
    pub member: String,
    /// Version number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vers: Option<u32>,
    /// Modification level.
    #[serde(rename = "mod", skip_serializing_if = "Option::is_none")]
    pub modification: Option<u32>,
    /// Created date (4-digit format).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub c4date: Option<String>,
    /// Modified date (4-digit format).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub m4date: Option<String>,
    /// Current number of records.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cnorc: Option<u32>,
    /// Initial number of records.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inorc: Option<u32>,
}

/// Parameters for creating a new dataset.
///
/// Zowe CLI sends `blksize` while the z/OSMF spec uses `blksz`, so both are accepted.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatasetCreateParams {
    /// Dataset organization.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dsorg: Option<String>,
    /// Record format.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recfm: Option<String>,
    /// Logical record length.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lrecl: Option<u32>,
    /// Block size (z/OSMF field name).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blksz: Option<u32>,
    /// Block size (Zowe CLI field name).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blksize: Option<u32>,
    /// Primary space allocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub primary: Option<u32>,
    /// Secondary space allocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub secondary: Option<u32>,
    /// Allocation unit (TRK, CYL, BLK).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alcunit: Option<String>,
    /// Directory blocks (for PDS).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dirblk: Option<u32>,
    /// Volume serial.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub volser: Option<String>,
    /// Device type.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unit: Option<String>,
    /// Average block length.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub avgblk: Option<u32>,
    /// SMS storage class.
    #[serde(rename = "storeclass", skip_serializing_if = "Option::is_none")]
    pub storclass: Option<String>,
    /// SMS management class.
    #[serde(rename = "mgntclass", skip_serializing_if = "Option::is_none")]
    pub mgntclass: Option<String>,
    /// SMS data class.
    #[serde(rename = "dataclass", skip_serializing_if = "Option::is_none")]
    pub dataclass: Option<String>,
    /// Dataset name type (PDS, PDSE, LIBRARY, BASIC, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dsntype: Option<String>,
}

impl DatasetCreateParams {
    /// Get the effective block size from either `blksz` or `blksize`.
    pub fn effective_blksize(&self) -> Option<u32> {
        self.blksz.or(self.blksize)
    }
}

/// Query parameters for dataset list.
#[derive(Debug, Clone, Deserialize)]
pub struct DatasetListQuery {
    /// Dataset level filter pattern (e.g., `HLQ.*`).
    #[serde(rename = "dslevel")]
    pub dslevel: String,
    /// Volume serial filter.
    #[serde(default)]
    pub volser: Option<String>,
    /// Start after this dataset name (pagination).
    #[serde(default)]
    pub start: Option<String>,
}

/// Query parameters for member list.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct MemberListQuery {
    /// Member name pattern (supports `*` wildcard).
    #[serde(default)]
    pub pattern: Option<String>,
    /// Start after this member name (pagination).
    #[serde(default)]
    pub start: Option<String>,
}
