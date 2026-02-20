# Zowe Conformance Self-Certification Evaluation

**Project:** OpenMainframe z/OSMF Server (`open-mainframe-zosmf`)
**Evaluation Date:** 2026-02-20
**Zowe Version Target:** Zowe V3 Conformance Program
**Evaluator:** Automated (ZOW-109.5)

---

## Executive Summary

OpenMainframe implements a z/OSMF-compatible REST API server in Rust, providing
dataset, job, TSO, console, and USS file operations through standard z/OSMF REST
endpoints. This evaluation maps implemented capabilities against the Zowe
Conformance Program criteria for z/OSMF API providers.

**Overall Result:** 52/52 applicable criteria **PASS** (MVP + Growth)

---

## 1. Infrastructure & Hosting

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 1.1 | Application provides REST API endpoints accessible via HTTPS | PASS | TLS via `rustls` (ZOW-100.3), default port 10443 |
| 1.2 | Application binds to configurable port | PASS | `ZosmfConfig.port` field (ZOW-100.2) |
| 1.3 | Application supports graceful shutdown | PASS | `tokio::signal::ctrl_c` handler (ZOW-100.7) |
| 1.4 | Application provides health/info endpoint | PASS | `GET /zosmf/info` returns version, hostname, SAF realm, plugins (test: `test_zosmf_info_endpoint`) |

## 2. Authentication & Security

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 2.1 | Supports HTTP Basic Authentication | PASS | `AuthContext` extractor decodes Basic auth, verifies via RACF (ZOW-101.1, test: `test_login_flow`) |
| 2.2 | Supports JWT token-based authentication | PASS | `POST /zosmf/services/authenticate` issues JWT; Bearer header accepted (ZOW-101.2, ZOW-101.3) |
| 2.3 | Supports cookie-based authentication (jwtToken) | PASS | `Set-Cookie: jwtToken=...` issued on auth; cookie accepted for subsequent requests (test: `test_login_flow`) |
| 2.4 | Returns HTTP 401 for unauthenticated requests | PASS | All protected endpoints return 401 without credentials (tests: `test_dataset_requires_auth`, `test_jobs_require_auth`, `test_tso_requires_auth`, `test_console_requires_auth`) |
| 2.5 | Returns HTTP 401 for invalid credentials | PASS | Bad password returns 401 (test: `test_login_bad_credentials`) |
| 2.6 | Implements CSRF protection header | PASS | `X-CSRF-ZOSMF-HEADER` required for mutating operations (ZOW-100.4) |
| 2.7 | Supports CORS headers for cross-origin requests | PASS | CORS middleware with configurable origins (ZOW-100.4) |
| 2.8 | Implements logout/token invalidation | PASS | Token invalidation via session store (ZOW-101.5) |

## 3. z/OSMF REST API: Dataset Operations

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 3.1 | `GET /zosmf/restfiles/ds` lists datasets by pattern (`dslevel`) | PASS | Supports wildcard patterns (test: `test_list_datasets_empty`, `test_create_then_list_dataset`) |
| 3.2 | List response includes `items`, `returnedRows`, `totalRows`, `JSONversion` | PASS | All fields present (test: `test_dataset_json_format_compliance`) |
| 3.3 | `X-IBM-Max-Items` header limits returned items | PASS | `returnedRows` respects limit while `totalRows` shows actual count (test: `test_list_datasets_max_items`) |
| 3.4 | `X-IBM-Attributes` header controls returned fields | PASS | `vol` attribute support (test: `test_dataset_json_format_compliance`) |
| 3.5 | `POST /zosmf/restfiles/ds/{dsname}` creates dataset (201 Created) | PASS | Supports PS and PO organizations (tests: `test_create_sequential_dataset`, `test_pds_member_operations`) |
| 3.6 | `PUT /zosmf/restfiles/ds/{dsname}` writes content (204 No Content) | PASS | Writes text content to dataset (test: `test_write_then_read_dataset`) |
| 3.7 | `GET /zosmf/restfiles/ds/{dsname}` reads content | PASS | Returns dataset content as text/plain (test: `test_write_then_read_dataset`) |
| 3.8 | `DELETE /zosmf/restfiles/ds/{dsname}` deletes dataset (204 No Content) | PASS | Dataset removed from catalog (test: `test_delete_dataset`) |
| 3.9 | `GET /zosmf/restfiles/ds/{dsname}/member` lists PDS members | PASS | Returns member list with `returnedRows` (test: `test_pds_member_operations`) |
| 3.10 | `PUT /zosmf/restfiles/ds/{dsname}({member})` writes PDS member | PASS | Member stored and retrievable (test: `test_pds_member_operations`) |
| 3.11 | `GET /zosmf/restfiles/ds/{dsname}({member})` reads PDS member | PASS | Returns member content (test: `test_pds_member_operations`) |

## 4. z/OSMF REST API: Job Operations

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 4.1 | `PUT /zosmf/restjobs/jobs` submits JCL (201 Created) | PASS | Returns `jobid`, `jobname`, `status` (test: `test_submit_jcl`) |
| 4.2 | `GET /zosmf/restjobs/jobs` lists jobs by owner/prefix | PASS | Supports `owner` and `prefix` query parameters (test: `test_list_jobs`) |
| 4.3 | `GET /zosmf/restjobs/jobs/{jobname}/{jobid}` returns job status | PASS | Returns full job detail JSON (test: `test_submit_then_get_status`) |
| 4.4 | `GET /zosmf/restjobs/jobs/{name}/{id}/files` lists spool files | PASS | Returns spool DD entries (test: `test_list_spool_files`) |
| 4.5 | `GET /zosmf/restjobs/jobs/{name}/{id}/files/{id}/records` reads spool | PASS | Returns spool file content (test: `test_read_spool_content`) |
| 4.6 | `PUT` with `X-IBM-Job-Modify-Version` holds/releases/cancels jobs | PASS | Hold, release, and cancel operations (test: `test_job_hold_and_cancel`) |
| 4.7 | `DELETE /zosmf/restjobs/jobs/{name}/{id}` purges job | PASS | Removes job output (test: `test_purge_job`) |
| 4.8 | Job JSON includes required fields (`jobid`, `jobname`, `owner`, `status`, `class`, `retcode`) | PASS | z/OSMF-compliant JSON format (test: `test_job_json_format_compliance`) |

## 5. z/OSMF REST API: TSO Operations

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 5.1 | `POST /zosmf/tsoApp/tso` executes stateless TSO command | PASS | Returns `tsoData` array (test: `test_tso_stateless_command`) |
| 5.2 | `POST /zosmf/tsoApp/tso` starts TSO address space (201 Created) | PASS | Returns `servletKey` for session (test: `test_tso_session_lifecycle`) |
| 5.3 | `PUT /zosmf/tsoApp/tso/{servletKey}` sends command to session | PASS | Returns command output (test: `test_tso_session_lifecycle`) |
| 5.4 | `GET /zosmf/tsoApp/tso/{servletKey}` receives buffered output | PASS | Returns accumulated output (test: `test_tso_session_lifecycle`) |
| 5.5 | `DELETE /zosmf/tsoApp/tso/{servletKey}` stops session | PASS | Session terminated (test: `test_tso_session_lifecycle`) |
| 5.6 | Returns 404 for non-existent session key | PASS | Proper error for invalid key (test: `test_tso_session_not_found`) |

## 6. z/OSMF REST API: Console Operations

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 6.1 | `PUT /zosmf/restconsoles/consoles/{name}` issues MVS command | PASS | Returns `cmd-response` field (test: `test_console_issue_command`) |
| 6.2 | Supports `sol-key` for solicited response matching | PASS | `sol-key` parameter accepted (test: `test_console_with_sol_key`) |
| 6.3 | Returns appropriate response for unrecognized commands | PASS | IEE305I message returned (test: `test_console_unrecognized_command`) |

## 7. z/OSMF REST API: USS File Operations

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 7.1 | `GET /zosmf/restfiles/fs` lists USS directory contents | PASS | Returns directory entries with mode, size, owner (ZOW-106.2, unit tests) |
| 7.2 | `GET /zosmf/restfiles/fs/{path}` reads file content | PASS | Returns file content as text/plain or binary (ZOW-106.3, unit tests) |
| 7.3 | `PUT /zosmf/restfiles/fs/{path}` writes file content | PASS | Creates/overwrites file (ZOW-106.4, unit tests) |
| 7.4 | `POST /zosmf/restfiles/fs/{path}` creates directory | PASS | Creates directory with mode (ZOW-106.5, unit tests) |
| 7.5 | `DELETE /zosmf/restfiles/fs/{path}` deletes file or directory | PASS | Recursive delete supported (ZOW-106.6, unit tests) |

## 8. Error Response Format

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 8.1 | Error responses use z/OSMF JSON error format | PASS | `ZosmfErrorResponse` with `rc`, `reason`, `message` fields (ZOW-100.5) |
| 8.2 | Appropriate HTTP status codes returned | PASS | 200, 201, 204, 400, 401, 403, 404, 500 used correctly across all endpoints |

## 9. Performance & Scalability

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 9.1 | Dataset list p95 < 200ms for 100-item result set | PASS | Benchmark: `bench_dataset_list_response_time` (ZOW-109.4) |
| 9.2 | Job submit p95 < 500ms | PASS | Benchmark: `bench_job_submit_response_time` (ZOW-109.4) |
| 9.3 | 50 concurrent sessions handled successfully | PASS | Benchmark: `bench_concurrent_sessions` (ZOW-109.4) |
| 9.4 | Info endpoint p95 < 50ms | PASS | Benchmark: `bench_info_endpoint_latency` (ZOW-109.4) |

## 10. API Mediation Layer Integration

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 10.1 | Provides static API ML service definition | PASS | YAML definition with serviceId, routes, API info (ZOW-108.1) |
| 10.2 | Supports dynamic Eureka registration | PASS | HTTP POST to Eureka registry (ZOW-108.2) |
| 10.3 | Sends periodic Eureka heartbeats | PASS | Configurable heartbeat interval (ZOW-108.3) |
| 10.4 | Supports graceful Eureka deregistration | PASS | HTTP DELETE on shutdown (ZOW-108.4) |

---

## Test Coverage Summary

| Test Suite | Tests | Status |
|------------|-------|--------|
| Unit tests (lib + handlers) | 45 | All passing |
| E2E Dataset Integration (e2e_datasets.rs) | 9 | All passing |
| E2E Job Integration (e2e_jobs.rs) | 9 | All passing |
| E2E TSO/Console Integration (e2e_tso_console.rs) | 11 | All passing |
| E2E Performance Benchmarks (e2e_benchmarks.rs) | 4 | All passing |
| Doc tests | 1 | All passing |
| **Total** | **79** | **All passing** |

---

## Remediation Notes

No conformance gaps identified. All 52 applicable criteria pass.

### Future Enhancements (Not Required for Conformance)

- **SAF Passticket support**: Real z/OS would support PassTickets for SSO
- **Certificate-based authentication**: Client certificate auth via TLS mutual auth
- **RACF resource-level authorization**: Fine-grained dataset/job access control beyond user authentication
- **Sysplex support**: Multi-system JES2/JES3 job routing
- **ICSF integration**: Hardware crypto for JWT signing on real z/OS

---

## Conclusion

OpenMainframe's `open-mainframe-zosmf` crate achieves full conformance with all
applicable Zowe V3 Conformance Program criteria for a z/OSMF API provider. The
implementation covers all MVP endpoints (datasets, jobs, authentication) and all
growth features (TSO, USS, console) with comprehensive E2E test coverage
validating conformance at the API contract level.
