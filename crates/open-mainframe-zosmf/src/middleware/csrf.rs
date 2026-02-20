//! CSRF validation middleware — requires X-CSRF-ZOSMF-HEADER on mutating requests.

/// The required header name for CSRF protection.
pub const CSRF_HEADER: &str = "X-CSRF-ZOSMF-HEADER";

/// Placeholder for CSRF middleware (implemented in ZOW-100.4).
pub fn _placeholder() {
    // Will be implemented as Tower middleware layer.
}
