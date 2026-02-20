//! z/OSMF jobs REST API types.

use serde::{Deserialize, Serialize};

/// A job entry in list and status responses.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobResponse {
    /// Job ID (e.g., JOB00042).
    pub jobid: String,
    /// Job name.
    pub jobname: String,
    /// Job owner userid.
    pub owner: String,
    /// Job status: INPUT, ACTIVE, or OUTPUT.
    pub status: String,
    /// Job type (JOB, STC, TSU).
    #[serde(rename = "type")]
    pub job_type: String,
    /// Job class.
    pub class: String,
    /// Return code (e.g., "CC 0000").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub retcode: Option<String>,
    /// URL for this job resource.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    /// URL to spool files.
    #[serde(rename = "files-url", skip_serializing_if = "Option::is_none")]
    pub files_url: Option<String>,
    /// Current phase number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phase: Option<i32>,
    /// Phase name.
    #[serde(rename = "phase-name", skip_serializing_if = "Option::is_none")]
    pub phase_name: Option<String>,
}

/// Response for job submit.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobSubmitResponse {
    /// Assigned job ID.
    pub jobid: String,
    /// Job name from JCL.
    pub jobname: String,
    /// Owner userid.
    pub owner: String,
    /// Initial status (INPUT).
    pub status: String,
}

/// A spool file entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpoolFile {
    /// Spool file numeric ID.
    pub id: u32,
    /// DD name.
    pub ddname: String,
    /// Step name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stepname: Option<String>,
    /// Proc step name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub procstep: Option<String>,
    /// Output class.
    pub class: String,
    /// Byte count.
    #[serde(rename = "byte-count")]
    pub byte_count: u64,
    /// Record count.
    #[serde(rename = "record-count")]
    pub record_count: u64,
    /// URL to fetch records.
    #[serde(rename = "records-url", skip_serializing_if = "Option::is_none")]
    pub records_url: Option<String>,
}

/// Job action request body (hold, release, cancel).
#[derive(Debug, Clone, Deserialize)]
pub struct JobActionRequest {
    /// Requested action: "hold", "release", or "cancel".
    pub request: String,
    /// Version for the request format.
    #[serde(default)]
    pub version: Option<String>,
}

/// Feedback response for job actions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobFeedback {
    /// Job ID.
    pub jobid: String,
    /// Job name.
    pub jobname: String,
    /// Status code (0 = success).
    pub status: i32,
    /// Feedback message.
    pub message: String,
}

/// Query parameters for job list.
#[derive(Debug, Clone, Deserialize)]
pub struct JobListQuery {
    /// Owner filter.
    #[serde(default)]
    pub owner: Option<String>,
    /// Prefix filter (supports `*` wildcard).
    #[serde(default)]
    pub prefix: Option<String>,
    /// Job ID filter.
    #[serde(default)]
    pub jobid: Option<String>,
    /// Status filter (INPUT, ACTIVE, OUTPUT).
    #[serde(default)]
    pub status: Option<String>,
}
