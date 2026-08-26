# open-mainframe-crypto

z/OS ICSF (Integrated Cryptographic Service Facility) and RACF cryptographic security extensions — symmetric/asymmetric cryptographic operations, key store management, RACF crypto profile authorization, Bell-LaPadula security labels, audit/SMF records, and RACF exit/utility support.

## Purpose

This crate provides a functional model of the z/OS cryptographic subsystem and RACF security extensions. It models ICSF callable services (symmetric/asymmetric encryption, hashing, PRNG, and key generation), cryptographic key stores (CKDS, PKDS, TKDS), RACF profile protection for keys (`CSFKEYS`) and services (`CSFSERV`), Multi-Level Security (MLS) Bell-LaPadula dominance checks, SMF audit records (Types 80, 81, 83), and security exit routines and utilities (IRRUT100, IRRUT200, IRRUT400).

> [!NOTE]
> All cryptographic operations use **simulated algorithms** (XOR transforms and deterministic pseudo-random generators) for functional modeling and unit testing. They are **not cryptographically secure** and must not be used for production data protection.

## Capabilities

- **Symmetric Services** (`symmetric`): AES (128/192/256-bit) and 3DES encryption/decryption in CBC, ECB, and CTR modes, SHA (256/384/512) one-way hashing, HMAC generation and verification (RFC 2104 structure), pseudo-random number generation, and `CSFPRMxx` configuration parsing.
- **Asymmetric Services** (`asymmetric`): RSA keypair generation (2048/4096-bit) with PKCS#1 v1.5 and PSS signature schemes, Elliptic Curve keypair generation (NIST P-256, P-384, P-521), ECDSA sign/verify, and ECDH key agreement.
- **Key Stores** (`keystore`):
  - **CKDS** (Cryptographic Key Data Set): Symmetric keys indexed by uppercase label.
  - **PKDS** (Public Key Data Set): RSA and EC keypairs indexed by label.
  - **TKDS** (Token Key Data Set): PKCS#11 token objects with `TokenAttribute` (Class, KeyType, Label, Encrypt, Decrypt, etc.).
  - **Master Key**: Key wrapping and unwrapping simulation.
  - **KeyLifecycle**: Unified workflows for generating, deleting, and rekeying symmetric and asymmetric keys.
- **RACF Crypto Authorization** (`racf_crypto`): `CSFKEYS` and `CSFSERV` profiles with discrete and generic wildcard matching (`*` single-qualifier, `**` multi-qualifier) and access level hierarchy (`None` < `Read` < `Update` < `Control` < `Alter`).
- **Security Labels & MLS** (`seclabel`): Bell-LaPadula Mandatory Access Control (MAC) model with hierarchical classification levels (`Unclassified`, `Confidential`, `Secret`, `TopSecret`), category compartments, `dominance_check` ("no read up"), and MLS modes (`Active`, `Quiet`, `Off`).
- **Audit & SMF Records** (`audit`): SMF Type 80 (authorization results and administrative profile changes), Type 81 (RACF initialization), Type 83 (database modification before/after records), `IRRDBU00` database unload format, and indexed `AuditTrail` query interface.
- **RACF Exits & Utilities** (`exits`): Exit point dispatcher (`ICHRTX00` pre-authorization, `ICHPWX01` password quality, `IRREVX01` event notification) and security database utilities (`IRRUT100` search, `IRRUT200` verification, `IRRUT400` split/merge).

## Architecture

```
  ┌─────────────────────────────────────────────────────────────┐
  │                    ICSF Callable Services                   │
  │                                                             │
  │  ┌──────────────┐  ┌───────────────┐  ┌──────────────────┐ │
  │  │  symmetric   │  │  asymmetric   │  │    keystore      │ │
  │  │  AES / 3DES  │  │  RSA / EC     │  │  CKDS PKDS TKDS  │ │
  │  │  SHA hash    │  │  Sign/Verify  │  │  MasterKey       │ │
  │  │  HMAC / PRNG │  │  ECDH agree   │  │  KeyLifecycle    │ │
  │  └──────────────┘  └───────────────┘  └──────────────────┘ │
  └─────────────────────────┬───────────────────────────────────┘
                            │
  ┌─────────────────────────▼───────────────────────────────────┐
  │                  RACF Security Integration                  │
  │                                                             │
  │  ┌──────────────┐  ┌───────────────┐  ┌──────────────────┐ │
  │  │ racf_crypto  │  │   seclabel    │  │     audit        │ │
  │  │ CSFKEYS/SERV │  │ Bell-LaPadula │  │ SMF 80/81/83     │ │
  │  │ Profile ACL  │  │ MAC dominance │  │ IRRDBU00         │ │
  │  │ Wildcard     │  │ MLS modes     │  │ AuditTrail       │ │
  │  └──────────────┘  └───────────────┘  └──────────────────┘ │
  │                                                             │
  │  ┌───────────────────────────────────────────────────────┐  │
  │  │                      exits                            │  │
  │  │  ICHRTX00 / ICHPWX01 / IRREVX01 exit registry        │  │
  │  │  IRRUT100 search / IRRUT200 verify / IRRUT400 split  │  │
  │  └───────────────────────────────────────────────────────┘  │
  └─────────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `symmetric` | AES/3DES symmetric encryption, SHA hashing, HMAC, PRNG, and CSFPRMxx parser |
| `asymmetric` | RSA/EC key generation, RSA/ECDSA signatures, and ECDH key agreement |
| `keystore` | CKDS, PKDS, TKDS key stores, master key wrapping, and `KeyLifecycle` workflows |
| `racf_crypto` | CSFKEYS and CSFSERV profiles, generic wildcard matching, and access checks |
| `seclabel` | Bell-LaPadula security levels, category sets, and MLS dominance checking |
| `audit` | SMF Types 80/81/83 records, IRRDBU00 database unload, and `AuditTrail` queries |
| `exits` | RACF exit points (ICHRTX00, ICHPWX01, IRREVX01) and IRRUT100/200/400 utilities |
| `error` | `CryptoError` error definitions |

## Public API

### Primary Types and Functions

- **Symmetric**: `SymmetricAlgorithm` (`Aes128`, `Aes192`, `Aes256`, `TripleDes`), `CipherMode` (`Cbc`, `Ecb`, `Ctr`), `HashAlgorithm` (`Sha256`, `Sha384`, `Sha512`), `IcsfSymmetricKey`, `IcsfResult`, `IcsfConfig`, `encrypt()`, `decrypt()`, `one_way_hash()`, `hmac_generate()`, `hmac_verify()`, `generate_random()`.
- **Asymmetric**: `SignScheme` (`Pkcs1v15`, `Pss`), `EcCurve` (`P256`, `P384`, `P521`), `RsaKeyPair`, `EcKeyPair`, `generate_rsa_keypair()`, `rsa_sign()`, `rsa_verify()`, `generate_ec_keypair()`, `ecdsa_sign()`, `ecdsa_verify()`, `ecdh_agree()`.
- **Key Stores**: `Ckds`, `Pkds`, `PkdsEntry`, `Tkds`, `TkdsEntry`, `TokenAttribute`, `MasterKey`, `KeyLifecycle`.
- **RACF Crypto**: `CryptoAccessLevel`, `CsfKeysProfile`, `CsfServProfile`, `GcsfKeysProfile`, `check_key_access()`, `check_service_access()`.
- **Security Labels**: `SecurityLevel`, `SecurityCategory`, `SecurityLabel`, `SeclabelProfile`, `MlsMode`, `dominance_check()`.
- **Audit**: `SmfType80Record`, `SmfType80ProfileChange`, `SmfType81Record`, `SmfType83Record`, `Irrdbu00`, `UnloadRecordType`, `AuditTrail`.
- **Exits & Utilities**: `ExitPoint`, `ExitAction`, `ExitContext`, `ExitRegistry`, `RacfSearchUtil`, `RacfVerifyUtil`, `RacfSplitMergeUtil`.
- **Errors**: `CryptoError`.

## Integration

- **Internal workspace dependencies**: None (depends on workspace-configured `miette`, `thiserror`).
- **Consumers**: Standalone cryptographic services library; models ICSF services and RACF security profile behaviors.

## Examples

### Symmetric Encryption and Decryption

```rust
use open_mainframe_crypto::symmetric::{
    decrypt, encrypt, CipherMode, IcsfSymmetricKey, SymmetricAlgorithm,
};

let key = IcsfSymmetricKey::new(vec![0x5A; 32]);
let plaintext = b"CONFIDENTIAL TRANSACTION RECORD";

let enc_result = encrypt(
    &key,
    plaintext,
    SymmetricAlgorithm::Aes256,
    CipherMode::Cbc,
).expect("Encryption failed");

assert!(enc_result.is_ok());

let dec_result = decrypt(
    &key,
    &enc_result.data,
    SymmetricAlgorithm::Aes256,
    CipherMode::Cbc,
).expect("Decryption failed");

assert_eq!(dec_result.data, plaintext);
```

### Key Store Lifecycle Management

```rust
use open_mainframe_crypto::keystore::KeyLifecycle;

let mut lifecycle = KeyLifecycle::new();
lifecycle
    .generate_symmetric("AES.PROD.KEY1", vec![0x42; 32])
    .expect("Failed to store key");

let key = lifecycle.ckds.get("AES.PROD.KEY1").expect("Key not found");
assert_eq!(key.len(), 32);

let old_key = lifecycle
    .rekey_symmetric("AES.PROD.KEY1", vec![0x99; 32])
    .expect("Rekey failed");
assert_eq!(old_key, vec![0x42; 32]);
```

### Bell-LaPadula Security Label Dominance

```rust
use open_mainframe_crypto::seclabel::{
    dominance_check, SecurityCategory, SecurityLabel, SecurityLevel,
};

let user_label = SecurityLabel::with_categories(
    SecurityLevel::TopSecret,
    vec![SecurityCategory::new("PAYROLL"), SecurityCategory::new("FINANCIAL")],
);

let resource_label = SecurityLabel::with_categories(
    SecurityLevel::Secret,
    vec![SecurityCategory::new("PAYROLL")],
);

// TopSecret dominates Secret, and user categories are a superset
assert!(dominance_check(&user_label, &resource_label));
```

### RACF Exit Registration

```rust
use open_mainframe_crypto::exits::{
    ExitAction, ExitContext, ExitPoint, ExitRegistry,
};

let mut registry = ExitRegistry::new();
registry
    .register(ExitPoint::PasswordQuality, |ctx| {
        if ctx.data.len() >= 8 {
            ExitAction::Allow
        } else {
            ExitAction::Deny
        }
    })
    .expect("Registration failed");

let short_ctx = ExitContext::new("USER1", "", "short");
assert_eq!(
    registry.invoke(ExitPoint::PasswordQuality, &short_ctx),
    ExitAction::Deny
);
```

## Testing

Run unit tests:

```bash
cargo test -p open-mainframe-crypto
```

The test suite contains 134 unit tests covering:
- Symmetric encryption/decryption roundtrips (AES-128/192/256, 3DES), HMAC verification, and `CSFPRMxx` parsing.
- RSA/EC keypair generation, signature verification, and ECDH shared secret derivation.
- CKDS/PKDS/TKDS insert, search, delete, and master key wrapping.
- RACF discrete and wildcard pattern matching (`*`, `**`) and service authorization checks.
- Bell-LaPadula dominance evaluation across 8 boundary scenarios and MLS operational modes.
- SMF record formatting (Types 80, 81, 83) and `IRRDBU00` unload records.
- RACF exit point registration, password quality checks, and `IRRUT` utility functions.

## Limitations

- **Simulated Algorithms**: Cryptographic routines use repeating-key XOR transforms and linear congruential pseudo-random number generation for functional testing; they are not real cryptographic primitives.
- **Asymmetric Signature Simulation**: Sign/verify operations use key-data hash mixing; real RSA modular exponentiation and EC curve mathematics are not executed.
- **ECDH Derivation**: Key agreement produces an element-wise product rather than elliptic curve point scalar multiplication.
- **SMF Timestamps**: SMF records format timestamps as ISO strings rather than binary s390x STCK (Store Clock) format.
- **IRRDBU00 Record Layout**: Unload output uses simplified text records rather than the exact fixed 80-byte IBM record formats.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-racf](../open-mainframe-racf/README.md)
- [open-mainframe-smf](../open-mainframe-smf/README.md)
