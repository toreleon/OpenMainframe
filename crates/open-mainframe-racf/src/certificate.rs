//! RACDCERT — Digital certificate and keyring management.
//!
//! Implements the z/OS RACDCERT command services:
//! - **GENCERT** — Generate self-signed or CA-signed certificates
//! - **GENREQ** — Generate a Certificate Signing Request (CSR)
//! - **ADD/DELETE** — Add or remove external certificates
//! - **LIST/LISTCHAIN** — Display certificate details
//! - **CHECKCERT** — Validate a certificate
//! - **ADDRING/DELRING/LISTRING** — Keyring lifecycle
//! - **CONNECT/REMOVE** — Associate certificates with keyrings
//! - **MAP** — Map certificates to RACF user IDs
//! - **EXPORT/IMPORT** — Certificate transfer
//! - **REKEY/ROLLOVER** — Certificate renewal

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

// ---------------------------------------------------------------------------
//  Return codes
// ---------------------------------------------------------------------------

/// RACDCERT return code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CertRc {
    /// 0 — Success.
    Ok,
    /// 4 — Warning (e.g., certificate nearing expiry).
    Warning,
    /// 8 — Not found.
    NotFound,
    /// 12 — Error (invalid input, duplicate, etc.).
    Error,
}

// ---------------------------------------------------------------------------
//  Certificate types
// ---------------------------------------------------------------------------

/// Certificate type (usage).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CertType {
    /// Personal / end-entity certificate.
    Personal,
    /// Certificate Authority (CA) certificate.
    CertAuth,
    /// Site certificate.
    Site,
}

impl std::fmt::Display for CertType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Personal => write!(f, "PERSONAL"),
            Self::CertAuth => write!(f, "CERTAUTH"),
            Self::Site => write!(f, "SITE"),
        }
    }
}

/// Key algorithm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum KeyAlgorithm {
    Rsa,
    Ecc,
    Dsa,
}

impl std::fmt::Display for KeyAlgorithm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rsa => write!(f, "RSA"),
            Self::Ecc => write!(f, "ECC"),
            Self::Dsa => write!(f, "DSA"),
        }
    }
}

/// Certificate trust status.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TrustStatus {
    /// Trusted — certificate is valid and trusted.
    Trusted,
    /// Not trusted — explicitly marked as not trusted.
    NotTrust,
    /// High trust — elevated trust level.
    HiTrust,
}

impl Default for TrustStatus {
    fn default() -> Self {
        Self::Trusted
    }
}

/// Subject Distinguished Name fields.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SubjectDN {
    /// Common Name (CN).
    pub cn: String,
    /// Organization (O).
    pub org: Option<String>,
    /// Organizational Unit (OU).
    pub ou: Option<String>,
    /// Country (C).
    pub country: Option<String>,
    /// State (ST).
    pub state: Option<String>,
    /// Locality (L).
    pub locality: Option<String>,
}

impl std::fmt::Display for SubjectDN {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CN={}", self.cn)?;
        if let Some(ref ou) = self.ou {
            write!(f, ",OU={ou}")?;
        }
        if let Some(ref org) = self.org {
            write!(f, ",O={org}")?;
        }
        if let Some(ref c) = self.country {
            write!(f, ",C={c}")?;
        }
        Ok(())
    }
}

/// A digital certificate stored in RACF.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Certificate {
    /// User-assigned label.
    pub label: String,
    /// Owning user ID (or CERTAUTH / SITE).
    pub owner: String,
    /// Certificate type.
    pub cert_type: CertType,
    /// Subject DN.
    pub subject: SubjectDN,
    /// Issuer DN (same as subject for self-signed).
    pub issuer: SubjectDN,
    /// Serial number (hex string).
    pub serial: String,
    /// Not-before date (YYYY-MM-DD).
    pub not_before: String,
    /// Not-after date (YYYY-MM-DD).
    pub not_after: String,
    /// Key algorithm.
    pub algorithm: KeyAlgorithm,
    /// Key size in bits.
    pub key_size: u32,
    /// Trust status.
    pub trust: TrustStatus,
    /// Whether this is a default certificate for the owner.
    pub is_default: bool,
    /// Opaque "certificate data" (in a real system this would be DER-encoded).
    pub data: Vec<u8>,
    /// Private key present flag.
    pub has_private_key: bool,
}

/// A keyring containing references to certificates.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Keyring {
    /// Keyring name.
    pub name: String,
    /// Owning user ID.
    pub owner: String,
    /// Connected certificates: (owner, label) pairs.
    pub connections: Vec<KeyringConnection>,
}

/// A connection entry in a keyring.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeyringConnection {
    /// Certificate owner ID.
    pub cert_owner: String,
    /// Certificate label.
    pub cert_label: String,
    /// Usage in keyring.
    pub usage: CertUsage,
    /// Whether this is the default certificate.
    pub is_default: bool,
}

/// Certificate usage within a keyring.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CertUsage {
    /// Personal use.
    Personal,
    /// Certificate authority.
    CertAuth,
    /// Site certificate.
    Site,
}

impl Default for CertUsage {
    fn default() -> Self {
        Self::Personal
    }
}

/// Certificate-to-userid mapping filter.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CertMapping {
    /// Mapped RACF user ID.
    pub userid: String,
    /// Subject DN filter (substring match).
    pub subject_filter: String,
    /// Issuer DN filter (substring match).
    pub issuer_filter: Option<String>,
    /// Label for this mapping.
    pub label: String,
}

// ---------------------------------------------------------------------------
//  Certificate Manager
// ---------------------------------------------------------------------------

/// Parameters for GENCERT.
#[derive(Debug, Clone)]
pub struct GencertParams {
    pub owner: String,
    pub label: String,
    pub subject: SubjectDN,
    pub algorithm: KeyAlgorithm,
    pub key_size: u32,
    pub not_before: String,
    pub not_after: String,
    pub cert_type: CertType,
}

/// RACDCERT certificate and keyring manager.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CertificateManager {
    /// Certificates: key = (owner, label).
    certificates: BTreeMap<(String, String), Certificate>,
    /// Keyrings: key = (owner, ring_name).
    keyrings: HashMap<(String, String), Keyring>,
    /// Certificate-to-userid mappings.
    mappings: Vec<CertMapping>,
    /// Serial number counter.
    next_serial: u64,
}

impl CertificateManager {
    /// Create a new certificate manager.
    pub fn new() -> Self {
        Self {
            certificates: BTreeMap::new(),
            keyrings: HashMap::new(),
            mappings: Vec::new(),
            next_serial: 1,
        }
    }

    // -------------------------------------------------------------------
    //  GENCERT — Generate certificate
    // -------------------------------------------------------------------

    /// Generate a self-signed certificate.
    pub fn gencert(&mut self, params: &GencertParams) -> CertRc {
        let key = (params.owner.to_uppercase(), params.label.to_uppercase());
        if self.certificates.contains_key(&key) {
            return CertRc::Error; // Duplicate.
        }

        let serial = format!("{:08X}", self.next_serial);
        self.next_serial += 1;

        let cert = Certificate {
            label: params.label.clone(),
            owner: params.owner.to_uppercase(),
            cert_type: params.cert_type,
            issuer: params.subject.clone(), // Self-signed: issuer == subject.
            subject: params.subject.clone(),
            serial,
            not_before: params.not_before.clone(),
            not_after: params.not_after.clone(),
            algorithm: params.algorithm,
            key_size: params.key_size,
            trust: TrustStatus::Trusted,
            is_default: false,
            data: Vec::new(),
            has_private_key: true,
        };

        self.certificates.insert(key, cert);
        CertRc::Ok
    }

    // -------------------------------------------------------------------
    //  GENREQ — Generate CSR
    // -------------------------------------------------------------------

    /// Generate a Certificate Signing Request (CSR).
    ///
    /// Returns the CSR data as a simplified text representation.
    pub fn genreq(&self, owner: &str, label: &str) -> (CertRc, Option<String>) {
        let key = (owner.to_uppercase(), label.to_uppercase());
        let cert = match self.certificates.get(&key) {
            Some(c) => c,
            None => return (CertRc::NotFound, None),
        };

        if !cert.has_private_key {
            return (CertRc::Error, None);
        }

        let csr = format!(
            "-----BEGIN CERTIFICATE REQUEST-----\n\
             Subject: {}\n\
             Algorithm: {} Key Size: {}\n\
             -----END CERTIFICATE REQUEST-----",
            cert.subject, cert.algorithm, cert.key_size
        );

        (CertRc::Ok, Some(csr))
    }

    // -------------------------------------------------------------------
    //  ADD / DELETE
    // -------------------------------------------------------------------

    /// Add an externally-obtained certificate.
    pub fn add(
        &mut self,
        owner: &str,
        label: &str,
        cert_type: CertType,
        subject: SubjectDN,
        data: Vec<u8>,
        trust: TrustStatus,
    ) -> CertRc {
        let key = (owner.to_uppercase(), label.to_uppercase());
        if self.certificates.contains_key(&key) {
            return CertRc::Error;
        }

        let serial = format!("{:08X}", self.next_serial);
        self.next_serial += 1;

        let cert = Certificate {
            label: label.to_string(),
            owner: owner.to_uppercase(),
            cert_type,
            subject: subject.clone(),
            issuer: subject,
            serial,
            not_before: "2024-01-01".to_string(),
            not_after: "2034-12-31".to_string(),
            algorithm: KeyAlgorithm::Rsa,
            key_size: 2048,
            trust,
            is_default: false,
            data,
            has_private_key: false,
        };

        self.certificates.insert(key, cert);
        CertRc::Ok
    }

    /// Delete a certificate.
    pub fn delete(&mut self, owner: &str, label: &str) -> CertRc {
        let key = (owner.to_uppercase(), label.to_uppercase());
        if self.certificates.remove(&key).is_some() {
            // Remove from any keyrings.
            for ring in self.keyrings.values_mut() {
                ring.connections.retain(|c| {
                    !(c.cert_owner.eq_ignore_ascii_case(owner)
                        && c.cert_label.eq_ignore_ascii_case(label))
                });
            }
            CertRc::Ok
        } else {
            CertRc::NotFound
        }
    }

    // -------------------------------------------------------------------
    //  LIST / LISTCHAIN
    // -------------------------------------------------------------------

    /// List certificates for a given owner. If `owner` is `None`, list all.
    pub fn list(&self, owner: Option<&str>) -> Vec<&Certificate> {
        match owner {
            Some(id) => {
                let upper = id.to_uppercase();
                self.certificates
                    .values()
                    .filter(|c| c.owner == upper)
                    .collect()
            }
            None => self.certificates.values().collect(),
        }
    }

    /// List a certificate by label.
    pub fn get_cert(&self, owner: &str, label: &str) -> Option<&Certificate> {
        let key = (owner.to_uppercase(), label.to_uppercase());
        self.certificates.get(&key)
    }

    /// List the certificate chain (issuer chain) for a certificate.
    ///
    /// Returns the chain from leaf to root.
    pub fn listchain(&self, owner: &str, label: &str) -> Vec<&Certificate> {
        let mut chain = Vec::new();
        let key = (owner.to_uppercase(), label.to_uppercase());

        if let Some(cert) = self.certificates.get(&key) {
            chain.push(cert);
            // Walk issuer chain.
            let mut current_issuer = &cert.issuer;
            for _ in 0..10 {
                // Max depth.
                if format!("{current_issuer}") == format!("{}", cert.subject) && chain.len() == 1 {
                    break; // Self-signed — no chain.
                }
                // Find issuer certificate.
                let issuer_cert = self.certificates.values().find(|c| {
                    format!("{}", c.subject) == format!("{current_issuer}")
                        && c.cert_type == CertType::CertAuth
                });
                match issuer_cert {
                    Some(ic) => {
                        chain.push(ic);
                        if format!("{}", ic.subject) == format!("{}", ic.issuer) {
                            break; // Root CA.
                        }
                        current_issuer = &ic.issuer;
                    }
                    None => break,
                }
            }
        }
        chain
    }

    // -------------------------------------------------------------------
    //  CHECKCERT — validate certificate
    // -------------------------------------------------------------------

    /// Check a certificate's validity. Returns (rc, messages).
    pub fn checkcert(&self, owner: &str, label: &str) -> (CertRc, Vec<String>) {
        let key = (owner.to_uppercase(), label.to_uppercase());
        let cert = match self.certificates.get(&key) {
            Some(c) => c,
            None => return (CertRc::NotFound, vec!["Certificate not found.".into()]),
        };

        let mut msgs = Vec::new();
        let mut rc = CertRc::Ok;

        msgs.push(format!("Label: {}", cert.label));
        msgs.push(format!("Subject: {}", cert.subject));
        msgs.push(format!("Valid: {} to {}", cert.not_before, cert.not_after));

        match cert.trust {
            TrustStatus::NotTrust => {
                msgs.push("WARNING: Certificate is marked NOTRUST.".into());
                rc = CertRc::Warning;
            }
            TrustStatus::Trusted | TrustStatus::HiTrust => {
                msgs.push("Trust status: OK.".into());
            }
        }

        if cert.key_size < 2048 {
            msgs.push(format!(
                "WARNING: Key size {} is below recommended minimum.",
                cert.key_size
            ));
            rc = CertRc::Warning;
        }

        (rc, msgs)
    }

    // -------------------------------------------------------------------
    //  ADDRING / DELRING / LISTRING
    // -------------------------------------------------------------------

    /// Create a new keyring.
    pub fn addring(&mut self, owner: &str, ring_name: &str) -> CertRc {
        let key = (owner.to_uppercase(), ring_name.to_uppercase());
        if self.keyrings.contains_key(&key) {
            return CertRc::Error;
        }

        let ring = Keyring {
            name: ring_name.to_string(),
            owner: owner.to_uppercase(),
            connections: Vec::new(),
        };
        self.keyrings.insert(key, ring);
        CertRc::Ok
    }

    /// Delete a keyring.
    pub fn delring(&mut self, owner: &str, ring_name: &str) -> CertRc {
        let key = (owner.to_uppercase(), ring_name.to_uppercase());
        if self.keyrings.remove(&key).is_some() {
            CertRc::Ok
        } else {
            CertRc::NotFound
        }
    }

    /// List keyrings for an owner.
    pub fn listring(&self, owner: &str) -> Vec<&Keyring> {
        let upper = owner.to_uppercase();
        self.keyrings
            .values()
            .filter(|r| r.owner == upper)
            .collect()
    }

    /// Get a specific keyring.
    pub fn get_ring(&self, owner: &str, ring_name: &str) -> Option<&Keyring> {
        let key = (owner.to_uppercase(), ring_name.to_uppercase());
        self.keyrings.get(&key)
    }

    // -------------------------------------------------------------------
    //  CONNECT / REMOVE
    // -------------------------------------------------------------------

    /// Connect a certificate to a keyring.
    pub fn connect(
        &mut self,
        ring_owner: &str,
        ring_name: &str,
        cert_owner: &str,
        cert_label: &str,
        usage: CertUsage,
        is_default: bool,
    ) -> CertRc {
        // Verify certificate exists.
        let cert_key = (cert_owner.to_uppercase(), cert_label.to_uppercase());
        if !self.certificates.contains_key(&cert_key) {
            return CertRc::NotFound;
        }

        // Verify keyring exists.
        let ring_key = (ring_owner.to_uppercase(), ring_name.to_uppercase());
        let ring = match self.keyrings.get_mut(&ring_key) {
            Some(r) => r,
            None => return CertRc::NotFound,
        };

        // Check for duplicate connection.
        let already = ring.connections.iter().any(|c| {
            c.cert_owner.eq_ignore_ascii_case(cert_owner)
                && c.cert_label.eq_ignore_ascii_case(cert_label)
        });
        if already {
            return CertRc::Warning; // Already connected.
        }

        // If setting default, clear existing default.
        if is_default {
            for conn in &mut ring.connections {
                conn.is_default = false;
            }
        }

        ring.connections.push(KeyringConnection {
            cert_owner: cert_owner.to_uppercase(),
            cert_label: cert_label.to_string(),
            usage,
            is_default,
        });

        CertRc::Ok
    }

    /// Remove a certificate from a keyring.
    pub fn remove_from_ring(
        &mut self,
        ring_owner: &str,
        ring_name: &str,
        cert_owner: &str,
        cert_label: &str,
    ) -> CertRc {
        let ring_key = (ring_owner.to_uppercase(), ring_name.to_uppercase());
        let ring = match self.keyrings.get_mut(&ring_key) {
            Some(r) => r,
            None => return CertRc::NotFound,
        };

        let before = ring.connections.len();
        ring.connections.retain(|c| {
            !(c.cert_owner.eq_ignore_ascii_case(cert_owner)
                && c.cert_label.eq_ignore_ascii_case(cert_label))
        });

        if ring.connections.len() < before {
            CertRc::Ok
        } else {
            CertRc::NotFound
        }
    }

    // -------------------------------------------------------------------
    //  MAP — certificate-to-userid mapping
    // -------------------------------------------------------------------

    /// Add a certificate-to-userid mapping.
    pub fn map(
        &mut self,
        userid: &str,
        subject_filter: &str,
        issuer_filter: Option<&str>,
        label: &str,
    ) -> CertRc {
        self.mappings.push(CertMapping {
            userid: userid.to_uppercase(),
            subject_filter: subject_filter.to_string(),
            issuer_filter: issuer_filter.map(|s| s.to_string()),
            label: label.to_string(),
        });
        CertRc::Ok
    }

    /// Find the RACF user ID mapped to a certificate.
    pub fn lookup_mapping(&self, subject: &str, issuer: &str) -> Option<&str> {
        for mapping in &self.mappings {
            if subject.contains(&mapping.subject_filter) {
                if let Some(ref issuer_filter) = mapping.issuer_filter {
                    if !issuer.contains(issuer_filter) {
                        continue;
                    }
                }
                return Some(&mapping.userid);
            }
        }
        None
    }

    /// List all mappings.
    pub fn list_mappings(&self) -> &[CertMapping] {
        &self.mappings
    }

    // -------------------------------------------------------------------
    //  EXPORT / IMPORT
    // -------------------------------------------------------------------

    /// Export a certificate (returns the stored data).
    pub fn export(&self, owner: &str, label: &str) -> (CertRc, Option<Vec<u8>>) {
        let key = (owner.to_uppercase(), label.to_uppercase());
        match self.certificates.get(&key) {
            Some(cert) => {
                // Build a simplified PEM-like export.
                let pem = format!(
                    "-----BEGIN CERTIFICATE-----\n\
                     Label: {}\n\
                     Serial: {}\n\
                     Subject: {}\n\
                     Issuer: {}\n\
                     -----END CERTIFICATE-----\n",
                    cert.label, cert.serial, cert.subject, cert.issuer
                );
                (CertRc::Ok, Some(pem.into_bytes()))
            }
            None => (CertRc::NotFound, None),
        }
    }

    /// Import a certificate from data.
    pub fn import(
        &mut self,
        owner: &str,
        label: &str,
        cert_type: CertType,
        data: Vec<u8>,
        trust: TrustStatus,
    ) -> CertRc {
        // Parse minimal info from the "PEM" data.
        let subject = SubjectDN {
            cn: label.to_string(),
            ..Default::default()
        };
        self.add(owner, label, cert_type, subject, data, trust)
    }

    // -------------------------------------------------------------------
    //  REKEY / ROLLOVER
    // -------------------------------------------------------------------

    /// Rekey a certificate (generate new key pair, keep same subject).
    pub fn rekey(
        &mut self,
        owner: &str,
        old_label: &str,
        new_label: &str,
    ) -> CertRc {
        let old_key = (owner.to_uppercase(), old_label.to_uppercase());
        let old_cert = match self.certificates.get(&old_key) {
            Some(c) => c.clone(),
            None => return CertRc::NotFound,
        };

        let new_key = (owner.to_uppercase(), new_label.to_uppercase());
        if self.certificates.contains_key(&new_key) {
            return CertRc::Error;
        }

        let serial = format!("{:08X}", self.next_serial);
        self.next_serial += 1;

        let new_cert = Certificate {
            label: new_label.to_string(),
            serial,
            is_default: false,
            data: Vec::new(),
            has_private_key: true,
            ..old_cert
        };

        self.certificates.insert(new_key, new_cert);
        CertRc::Ok
    }

    /// Rollover: replace old certificate with new in all keyrings.
    pub fn rollover(
        &mut self,
        owner: &str,
        old_label: &str,
        new_label: &str,
    ) -> CertRc {
        let old_upper = old_label.to_uppercase();
        let new_upper = new_label.to_uppercase();
        let owner_upper = owner.to_uppercase();

        // Verify new cert exists.
        let new_key = (owner_upper.clone(), new_upper.clone());
        if !self.certificates.contains_key(&new_key) {
            return CertRc::NotFound;
        }

        // Replace in all keyrings.
        for ring in self.keyrings.values_mut() {
            for conn in &mut ring.connections {
                if conn.cert_owner == owner_upper
                    && conn.cert_label.eq_ignore_ascii_case(old_label)
                {
                    conn.cert_label = new_upper.clone();
                }
            }
        }

        // Mark old cert as NOTRUST.
        let old_key = (owner_upper, old_upper);
        if let Some(old_cert) = self.certificates.get_mut(&old_key) {
            old_cert.trust = TrustStatus::NotTrust;
        }

        CertRc::Ok
    }

    /// Total certificate count.
    pub fn cert_count(&self) -> usize {
        self.certificates.len()
    }

    /// Total keyring count.
    pub fn ring_count(&self) -> usize {
        self.keyrings.len()
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_subject(cn: &str) -> SubjectDN {
        SubjectDN {
            cn: cn.to_string(),
            org: Some("TestOrg".to_string()),
            ..Default::default()
        }
    }

    fn gen(mgr: &mut CertificateManager, owner: &str, label: &str, cn: &str) -> CertRc {
        mgr.gencert(&GencertParams {
            owner: owner.into(),
            label: label.into(),
            subject: make_subject(cn),
            algorithm: KeyAlgorithm::Rsa,
            key_size: 2048,
            not_before: "2024-01-01".into(),
            not_after: "2025-12-31".into(),
            cert_type: CertType::Personal,
        })
    }

    // -- GENCERT --

    #[test]
    fn test_gencert() {
        let mut mgr = CertificateManager::new();
        let rc = gen(&mut mgr, "WEBSRV", "WebServerCert", "myserver.example.com");
        assert_eq!(rc, CertRc::Ok);
        assert_eq!(mgr.cert_count(), 1);
    }

    #[test]
    fn test_gencert_duplicate() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "Cert1", "test");
        let rc = gen(&mut mgr, "USR1", "Cert1", "test2");
        assert_eq!(rc, CertRc::Error);
    }

    // -- GENREQ --

    #[test]
    fn test_genreq() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "MyCert", "server.com");
        let (rc, csr) = mgr.genreq("USR1", "MyCert");
        assert_eq!(rc, CertRc::Ok);
        assert!(csr.unwrap().contains("CERTIFICATE REQUEST"));
    }

    #[test]
    fn test_genreq_not_found() {
        let mgr = CertificateManager::new();
        let (rc, _) = mgr.genreq("USR1", "Missing");
        assert_eq!(rc, CertRc::NotFound);
    }

    // -- ADD / DELETE --

    #[test]
    fn test_add_external_cert() {
        let mut mgr = CertificateManager::new();
        let rc = mgr.add(
            "USR1", "ExternalCA", CertType::CertAuth,
            make_subject("External Root CA"), vec![1, 2, 3], TrustStatus::Trusted,
        );
        assert_eq!(rc, CertRc::Ok);
        assert_eq!(mgr.cert_count(), 1);
    }

    #[test]
    fn test_delete_cert() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "ToDelete", "test");
        assert_eq!(mgr.delete("USR1", "ToDelete"), CertRc::Ok);
        assert_eq!(mgr.cert_count(), 0);
    }

    #[test]
    fn test_delete_not_found() {
        let mut mgr = CertificateManager::new();
        assert_eq!(mgr.delete("USR1", "Missing"), CertRc::NotFound);
    }

    // -- LIST --

    #[test]
    fn test_list_by_owner() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "A", "a");
        gen(&mut mgr, "USR2", "B", "b");
        gen(&mut mgr, "USR1", "C", "c");

        let certs = mgr.list(Some("USR1"));
        assert_eq!(certs.len(), 2);
    }

    #[test]
    fn test_list_all() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "A", "a");
        gen(&mut mgr, "USR2", "B", "b");

        assert_eq!(mgr.list(None).len(), 2);
    }

    // -- CHECKCERT --

    #[test]
    fn test_checkcert_valid() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "OK", "test");

        let (rc, msgs) = mgr.checkcert("USR1", "OK");
        assert_eq!(rc, CertRc::Ok);
        assert!(msgs.iter().any(|m| m.contains("Trust status: OK")));
    }

    #[test]
    fn test_checkcert_weak_key() {
        let mut mgr = CertificateManager::new();
        mgr.gencert(&GencertParams {
            owner: "USR1".into(),
            label: "Weak".into(),
            subject: make_subject("test"),
            algorithm: KeyAlgorithm::Rsa,
            key_size: 1024,
            not_before: "2024-01-01".into(),
            not_after: "2025-12-31".into(),
            cert_type: CertType::Personal,
        });

        let (rc, msgs) = mgr.checkcert("USR1", "Weak");
        assert_eq!(rc, CertRc::Warning);
        assert!(msgs.iter().any(|m| m.contains("below recommended")));
    }

    // -- ADDRING / DELRING / LISTRING --

    #[test]
    fn test_addring_delring() {
        let mut mgr = CertificateManager::new();
        assert_eq!(mgr.addring("USR1", "MyKeyring"), CertRc::Ok);
        assert_eq!(mgr.ring_count(), 1);
        assert_eq!(mgr.addring("USR1", "MyKeyring"), CertRc::Error); // Dup.
        assert_eq!(mgr.delring("USR1", "MyKeyring"), CertRc::Ok);
        assert_eq!(mgr.ring_count(), 0);
    }

    #[test]
    fn test_listring() {
        let mut mgr = CertificateManager::new();
        mgr.addring("USR1", "Ring1");
        mgr.addring("USR1", "Ring2");
        mgr.addring("USR2", "Ring3");

        let rings = mgr.listring("USR1");
        assert_eq!(rings.len(), 2);
    }

    // -- CONNECT / REMOVE --

    #[test]
    fn test_connect_and_remove() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "Cert1", "test");
        mgr.addring("USR1", "Ring1");

        let rc = mgr.connect("USR1", "Ring1", "USR1", "Cert1", CertUsage::Personal, true);
        assert_eq!(rc, CertRc::Ok);

        let ring = mgr.get_ring("USR1", "Ring1").unwrap();
        assert_eq!(ring.connections.len(), 1);
        assert!(ring.connections[0].is_default);

        // Remove.
        let rc = mgr.remove_from_ring("USR1", "Ring1", "USR1", "Cert1");
        assert_eq!(rc, CertRc::Ok);
        let ring = mgr.get_ring("USR1", "Ring1").unwrap();
        assert!(ring.connections.is_empty());
    }

    #[test]
    fn test_connect_missing_cert() {
        let mut mgr = CertificateManager::new();
        mgr.addring("USR1", "Ring1");
        let rc = mgr.connect("USR1", "Ring1", "USR1", "NoCert", CertUsage::Personal, false);
        assert_eq!(rc, CertRc::NotFound);
    }

    #[test]
    fn test_connect_duplicate_warns() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "Cert1", "test");
        mgr.addring("USR1", "Ring1");
        mgr.connect("USR1", "Ring1", "USR1", "Cert1", CertUsage::Personal, false);
        let rc = mgr.connect("USR1", "Ring1", "USR1", "Cert1", CertUsage::Personal, false);
        assert_eq!(rc, CertRc::Warning);
    }

    // -- MAP --

    #[test]
    fn test_cert_mapping() {
        let mut mgr = CertificateManager::new();
        mgr.map("WEBSRV", "CN=web.example.com", None, "WebMapping");

        let uid = mgr.lookup_mapping("CN=web.example.com,O=Acme", "CN=SomeCA");
        assert_eq!(uid, Some("WEBSRV"));
    }

    #[test]
    fn test_cert_mapping_with_issuer() {
        let mut mgr = CertificateManager::new();
        mgr.map("WEBSRV", "CN=web", Some("CN=MyCA"), "Mapping1");

        // Should NOT match wrong issuer.
        assert!(mgr.lookup_mapping("CN=web", "CN=OtherCA").is_none());
        // Should match correct issuer.
        assert_eq!(mgr.lookup_mapping("CN=web", "CN=MyCA"), Some("WEBSRV"));
    }

    // -- EXPORT / IMPORT --

    #[test]
    fn test_export() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "Exp", "export.test");

        let (rc, data) = mgr.export("USR1", "Exp");
        assert_eq!(rc, CertRc::Ok);
        let pem = String::from_utf8(data.unwrap()).unwrap();
        assert!(pem.contains("BEGIN CERTIFICATE"));
    }

    #[test]
    fn test_import() {
        let mut mgr = CertificateManager::new();
        let rc = mgr.import("USR1", "Imported", CertType::CertAuth, vec![4, 5, 6], TrustStatus::Trusted);
        assert_eq!(rc, CertRc::Ok);
        assert_eq!(mgr.cert_count(), 1);
    }

    // -- REKEY / ROLLOVER --

    #[test]
    fn test_rekey() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "OldCert", "rekey.test");

        let rc = mgr.rekey("USR1", "OldCert", "NewCert");
        assert_eq!(rc, CertRc::Ok);
        assert_eq!(mgr.cert_count(), 2);

        let new = mgr.get_cert("USR1", "NewCert").unwrap();
        assert_eq!(new.subject.cn, "rekey.test");
    }

    #[test]
    fn test_rollover() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "Old", "roll");
        mgr.rekey("USR1", "Old", "New");
        mgr.addring("USR1", "Ring1");
        mgr.connect("USR1", "Ring1", "USR1", "Old", CertUsage::Personal, true);

        let rc = mgr.rollover("USR1", "Old", "New");
        assert_eq!(rc, CertRc::Ok);

        // Keyring should now point to new cert.
        let ring = mgr.get_ring("USR1", "Ring1").unwrap();
        assert_eq!(ring.connections[0].cert_label, "NEW");

        // Old cert should be NOTRUST.
        let old = mgr.get_cert("USR1", "Old").unwrap();
        assert_eq!(old.trust, TrustStatus::NotTrust);
    }

    // -- DELETE cascades to keyrings --

    #[test]
    fn test_delete_removes_from_keyrings() {
        let mut mgr = CertificateManager::new();
        gen(&mut mgr, "USR1", "Cert1", "test");
        mgr.addring("USR1", "Ring1");
        mgr.connect("USR1", "Ring1", "USR1", "Cert1", CertUsage::Personal, false);

        mgr.delete("USR1", "Cert1");
        let ring = mgr.get_ring("USR1", "Ring1").unwrap();
        assert!(ring.connections.is_empty());
    }
}
