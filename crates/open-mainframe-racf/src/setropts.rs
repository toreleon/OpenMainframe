//! SETROPTS System Administration.
//!
//! Implements the SETROPTS command for controlling system-wide RACF options:
//! class activation, password policy, audit settings, system protection,
//! MAC settings, and REFRESH/RVARY operations.

use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};
use tracing::info;

use crate::auth::PasswordPolicy;
use crate::resource::ResourceManager;

// ---------------------------------------------------------------------------
//  Password rules
// ---------------------------------------------------------------------------

/// Password syntax rules (SETROPTS PASSWORD(RULES(...))).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PasswordRules {
    /// Minimum password length.
    pub min_length: u32,
    /// Maximum password length.
    pub max_length: u32,
    /// Whether at least one numeric digit is required.
    pub require_numeric: bool,
    /// Whether mixed case is required.
    pub require_mixed_case: bool,
    /// Whether special characters are required.
    pub require_special: bool,
    /// Characters that are not allowed.
    pub disallowed_chars: String,
    /// Minimum number of days between password changes.
    pub min_change_days: u32,
    /// Days before expiration to warn user.
    pub warning_days: u32,
}

impl Default for PasswordRules {
    fn default() -> Self {
        Self {
            min_length: 4,
            max_length: 8,
            require_numeric: false,
            require_mixed_case: false,
            require_special: false,
            disallowed_chars: String::new(),
            min_change_days: 0,
            warning_days: 14,
        }
    }
}

// ---------------------------------------------------------------------------
//  Audit options
// ---------------------------------------------------------------------------

/// Logging options for audit trail generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LogOption {
    /// No logging.
    None,
    /// Log failures only.
    Failures,
    /// Log successes only.
    Successes,
    /// Log both successes and failures.
    All,
    /// Log never (default for most classes).
    Default,
}

/// Audit configuration for a resource class.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassAuditConfig {
    /// Whether auditing is enabled for this class.
    pub audit: bool,
    /// Whether special audit (SAUDIT) is enabled.
    pub saudit: bool,
    /// Logging option.
    pub log_option: LogOption,
    /// Whether statistics collection is enabled.
    pub statistics: bool,
}

impl Default for ClassAuditConfig {
    fn default() -> Self {
        Self {
            audit: false,
            saudit: false,
            log_option: LogOption::Default,
            statistics: false,
        }
    }
}

// ---------------------------------------------------------------------------
//  MAC settings
// ---------------------------------------------------------------------------

/// Mandatory Access Control (MAC) / Security Label settings.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct MacSettings {
    /// MLS (Multi-Level Security) enabled.
    pub mls: bool,
    /// MLACTIVE — MAC enforcement active.
    pub mlactive: bool,
    /// MLQUIET — suppress MAC violation messages.
    pub mlquiet: bool,
    /// MLSTABLE — security labels cannot be changed.
    pub mlstable: bool,
    /// MLITSTABLE — security label inheritance is stable.
    pub mlitstable: bool,
    /// SECLABEL audit enabled.
    pub seclabel_audit: bool,
}


// ---------------------------------------------------------------------------
//  System options
// ---------------------------------------------------------------------------

/// System-wide RACF options controlled via SETROPTS.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemOptions {
    /// Password policy settings.
    pub password: PasswordPolicy,
    /// Extended password syntax rules.
    pub password_rules: PasswordRules,
    /// Classes with generic profile checking enabled.
    pub generic_classes: BTreeSet<String>,
    /// Classes with generic command processing enabled (GENCMD).
    pub gencmd_classes: BTreeSet<String>,
    /// Classes with in-storage generic lists (GENLIST).
    pub genlist_classes: BTreeSet<String>,
    /// Classes with global access checking (GLOBAL).
    pub global_classes: BTreeSet<String>,
    /// Whether group list checking is enabled.
    pub grplist: bool,
    /// PROTECTALL — fail-closed for datasets with no covering profile.
    pub protect_all: bool,
    /// Classes with ERASE enabled (erase DASD content on delete).
    pub erase_classes: BTreeSet<String>,
    /// Audit configuration per class.
    pub audit_config: std::collections::BTreeMap<String, ClassAuditConfig>,
    /// Operator command auditing.
    pub oper_audit: bool,
    /// MAC/security label settings.
    pub mac: MacSettings,
    /// Session interval (timeout) in minutes. 0 = no timeout.
    pub session_interval: u32,
    /// JES batch: BATCHALLRACF — all batch jobs require RACF authorization.
    pub jes_batch_all_racf: bool,
    /// Catalog datasets tracking.
    pub catdsns: bool,
    /// Initialization statistics.
    pub init_stats: bool,
}

impl Default for SystemOptions {
    fn default() -> Self {
        Self {
            password: PasswordPolicy::default(),
            password_rules: PasswordRules::default(),
            generic_classes: BTreeSet::new(),
            gencmd_classes: BTreeSet::new(),
            genlist_classes: BTreeSet::new(),
            global_classes: BTreeSet::new(),
            grplist: true,
            protect_all: false,
            erase_classes: BTreeSet::new(),
            audit_config: std::collections::BTreeMap::new(),
            oper_audit: false,
            mac: MacSettings::default(),
            session_interval: 0,
            jes_batch_all_racf: false,
            catdsns: false,
            init_stats: false,
        }
    }
}

// ---------------------------------------------------------------------------
//  SETROPTS command processor
// ---------------------------------------------------------------------------

/// Result of a SETROPTS LIST command.
#[derive(Debug, Clone)]
pub struct SetroptsListResult {
    /// Current system options.
    pub options: SystemOptions,
    /// Active classes.
    pub active_classes: Vec<String>,
    /// RACLISTed classes.
    pub raclisted_classes: Vec<String>,
}

/// The SETROPTS command processor.
///
/// Manages system-wide RACF options and coordinates with the
/// ResourceManager for class activation / RACLIST operations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Setropts {
    /// Current system-wide options.
    pub options: SystemOptions,
}

impl Default for Setropts {
    fn default() -> Self {
        Self::new()
    }
}

impl Setropts {
    /// Create a new SETROPTS with default options.
    pub fn new() -> Self {
        Self {
            options: SystemOptions::default(),
        }
    }

    // ─────── S103.1: Class Activation ───────

    /// SETROPTS CLASSACT(class...) — activate one or more resource classes.
    pub fn classact(&mut self, rm: &mut ResourceManager, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            rm.activate_class(&upper);
            info!("SETROPTS CLASSACT({})", upper);
        }
    }

    /// SETROPTS NOCLASSACT(class...) — deactivate one or more resource classes.
    pub fn no_classact(&mut self, rm: &mut ResourceManager, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            rm.deactivate_class(&upper);
            info!("SETROPTS NOCLASSACT({})", upper);
        }
    }

    /// SETROPTS RACLIST(class...) — enable in-storage profile processing.
    pub fn raclist(&mut self, rm: &mut ResourceManager, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            rm.raclist_class(&upper);
            info!("SETROPTS RACLIST({})", upper);
        }
    }

    /// SETROPTS GENERIC(class...) — enable generic profile checking.
    pub fn generic(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            self.options.generic_classes.insert(upper.clone());
            info!("SETROPTS GENERIC({})", upper);
        }
    }

    /// SETROPTS NOGENERIC(class...) — disable generic profile checking.
    pub fn no_generic(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            self.options.generic_classes.remove(&upper);
        }
    }

    /// SETROPTS GENCMD(class...) — enable generic command processing.
    pub fn gencmd(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            self.options.gencmd_classes.insert(upper.clone());
            info!("SETROPTS GENCMD({})", upper);
        }
    }

    /// SETROPTS GENLIST(class...) — enable in-storage generic lists.
    pub fn genlist(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            self.options.genlist_classes.insert(upper.clone());
            info!("SETROPTS GENLIST({})", upper);
        }
    }

    /// SETROPTS GLOBAL(class...) — enable global access checking.
    pub fn global(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            self.options.global_classes.insert(upper.clone());
            info!("SETROPTS GLOBAL({})", upper);
        }
    }

    /// Check if generic profile checking is enabled for a class.
    pub fn is_generic(&self, class: &str) -> bool {
        self.options.generic_classes.contains(&class.trim().to_uppercase())
    }

    /// Check if GENCMD is enabled for a class.
    pub fn is_gencmd(&self, class: &str) -> bool {
        self.options.gencmd_classes.contains(&class.trim().to_uppercase())
    }

    // ─────── S103.2: Password Policy ───────

    /// SETROPTS PASSWORD — configure password policy.
    pub fn password(
        &mut self,
        interval: Option<u32>,
        history: Option<u32>,
        min_change: Option<u32>,
        warning: Option<u32>,
        revoke: Option<u32>,
    ) {
        if let Some(v) = interval {
            self.options.password.interval = v;
            info!("SETROPTS PASSWORD(INTERVAL({}))", v);
        }
        if let Some(v) = history {
            self.options.password.history = v;
            info!("SETROPTS PASSWORD(HISTORY({}))", v);
        }
        if let Some(v) = min_change {
            self.options.password_rules.min_change_days = v;
            info!("SETROPTS PASSWORD(MINCHANGE({}))", v);
        }
        if let Some(v) = warning {
            self.options.password_rules.warning_days = v;
            info!("SETROPTS PASSWORD(WARNING({}))", v);
        }
        if let Some(v) = revoke {
            self.options.password.max_failures = v;
            info!("SETROPTS PASSWORD(REVOKE({}))", v);
        }
    }

    /// SETROPTS PASSWORD(RULES(...)) — configure password syntax rules.
    pub fn password_rules(
        &mut self,
        min_length: Option<u32>,
        max_length: Option<u32>,
        require_numeric: Option<bool>,
        require_mixed_case: Option<bool>,
        require_special: Option<bool>,
    ) {
        if let Some(v) = min_length {
            self.options.password_rules.min_length = v;
            self.options.password.min_length = v;
        }
        if let Some(v) = max_length {
            self.options.password_rules.max_length = v;
            self.options.password.max_length = v;
        }
        if let Some(v) = require_numeric {
            self.options.password_rules.require_numeric = v;
        }
        if let Some(v) = require_mixed_case {
            self.options.password_rules.require_mixed_case = v;
        }
        if let Some(v) = require_special {
            self.options.password_rules.require_special = v;
        }
    }

    /// Get the current password policy.
    pub fn password_policy(&self) -> &PasswordPolicy {
        &self.options.password
    }

    /// Get the current password rules.
    pub fn get_password_rules(&self) -> &PasswordRules {
        &self.options.password_rules
    }

    // ─────── S103.3: Audit Options ───────

    /// SETROPTS AUDIT(class) — enable auditing for a class.
    pub fn audit(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            let config = self.options.audit_config.entry(upper.clone()).or_default();
            config.audit = true;
            info!("SETROPTS AUDIT({})", upper);
        }
    }

    /// SETROPTS NOAUDIT(class) — disable auditing for a class.
    pub fn no_audit(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            if let Some(config) = self.options.audit_config.get_mut(&upper) {
                config.audit = false;
            }
        }
    }

    /// SETROPTS SAUDIT(class) — enable special auditing for a class.
    pub fn saudit(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            let config = self.options.audit_config.entry(upper.clone()).or_default();
            config.saudit = true;
            info!("SETROPTS SAUDIT({})", upper);
        }
    }

    /// SETROPTS LOGOPTIONS(option class) — set logging options.
    pub fn logoptions(&mut self, class: &str, option: LogOption) {
        let upper = class.trim().to_uppercase();
        let config = self.options.audit_config.entry(upper.clone()).or_default();
        config.log_option = option;
        info!("SETROPTS LOGOPTIONS({:?} {})", option, upper);
    }

    /// SETROPTS STATISTICS(class) — enable statistics for a class.
    pub fn statistics(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            let config = self.options.audit_config.entry(upper.clone()).or_default();
            config.statistics = true;
            info!("SETROPTS STATISTICS({})", upper);
        }
    }

    /// SETROPTS OPERAUDIT — enable operator command auditing.
    pub fn operaudit(&mut self, enabled: bool) {
        self.options.oper_audit = enabled;
        info!("SETROPTS OPERAUDIT({})", if enabled { "ON" } else { "OFF" });
    }

    /// Check if auditing is enabled for a class.
    pub fn is_audit(&self, class: &str) -> bool {
        self.options
            .audit_config
            .get(&class.trim().to_uppercase())
            .map_or(false, |c| c.audit)
    }

    /// Get audit configuration for a class.
    pub fn get_audit_config(&self, class: &str) -> Option<&ClassAuditConfig> {
        self.options.audit_config.get(&class.trim().to_uppercase())
    }

    // ─────── S103.4: System Protection ───────

    /// SETROPTS PROTECTALL — require dataset profiles for all datasets.
    pub fn protect_all(&mut self, enabled: bool) {
        self.options.protect_all = enabled;
        info!("SETROPTS PROTECTALL({})", if enabled { "ON" } else { "OFF" });
    }

    /// SETROPTS GRPLIST — enable group list checking.
    pub fn grplist(&mut self, enabled: bool) {
        self.options.grplist = enabled;
        info!("SETROPTS GRPLIST({})", if enabled { "ON" } else { "OFF" });
    }

    /// SETROPTS ERASE(class) — enable content erasure on dataset delete.
    pub fn erase(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            self.options.erase_classes.insert(upper);
        }
    }

    /// SETROPTS NOERASE(class) — disable content erasure on dataset delete.
    pub fn no_erase(&mut self, classes: &[&str]) {
        for class in classes {
            let upper = class.trim().to_uppercase();
            self.options.erase_classes.remove(&upper);
        }
    }

    /// Whether PROTECTALL is enabled.
    pub fn is_protect_all(&self) -> bool {
        self.options.protect_all
    }

    /// Whether GRPLIST is enabled.
    pub fn is_grplist(&self) -> bool {
        self.options.grplist
    }

    // ─────── S103.5: MAC Settings ───────

    /// Configure MLS (Multi-Level Security) options.
    pub fn mls(&mut self, enabled: bool) {
        self.options.mac.mls = enabled;
        info!("SETROPTS MLS({})", if enabled { "ON" } else { "OFF" });
    }

    /// Configure MLACTIVE — MAC enforcement.
    pub fn mlactive(&mut self, enabled: bool) {
        self.options.mac.mlactive = enabled;
    }

    /// Configure MLQUIET — suppress MAC messages.
    pub fn mlquiet(&mut self, enabled: bool) {
        self.options.mac.mlquiet = enabled;
    }

    /// Configure MLSTABLE — security labels immutable.
    pub fn mlstable(&mut self, enabled: bool) {
        self.options.mac.mlstable = enabled;
    }

    /// Get current MAC settings.
    pub fn mac_settings(&self) -> &MacSettings {
        &self.options.mac
    }

    // ─────── S103.6: REFRESH/RVARY ───────

    /// SETROPTS REFRESH — refresh in-storage profiles for RACLISTed classes.
    ///
    /// Returns the list of classes that were refreshed.
    pub fn refresh(&self, rm: &ResourceManager) -> Vec<String> {
        let raclisted: Vec<String> = rm
            .list_classes()
            .into_iter()
            .filter(|c| rm.is_class_raclisted(c))
            .collect();
        info!("SETROPTS REFRESH: {} classes refreshed", raclisted.len());
        raclisted
    }

    // ─────── Session ───────

    /// SETROPTS SESSIONINTERVAL — set session timeout in minutes.
    pub fn session_interval(&mut self, minutes: u32) {
        self.options.session_interval = minutes;
        info!("SETROPTS SESSIONINTERVAL({})", minutes);
    }

    /// SETROPTS JES(BATCHALLRACF).
    pub fn jes_batch_all_racf(&mut self, enabled: bool) {
        self.options.jes_batch_all_racf = enabled;
    }

    // ─────── LIST ───────

    /// SETROPTS LIST — display current system options.
    pub fn list(&self, rm: &ResourceManager) -> SetroptsListResult {
        let active_classes = rm.list_active_classes();
        let raclisted_classes: Vec<String> = rm
            .list_classes()
            .into_iter()
            .filter(|c| rm.is_class_raclisted(c))
            .collect();

        SetroptsListResult {
            options: self.options.clone(),
            active_classes,
            raclisted_classes,
        }
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─────── S103.1: Class Activation ───────

    #[test]
    fn test_classact_activates_classes() {
        let mut setropts = Setropts::new();
        let mut rm = ResourceManager::new();

        setropts.classact(&mut rm, &["FACILITY", "PROGRAM"]);

        assert!(rm.is_class_active("FACILITY"));
        assert!(rm.is_class_active("PROGRAM"));
        assert!(!rm.is_class_active("OPERCMDS"));
    }

    #[test]
    fn test_noclassact_deactivates_classes() {
        let mut setropts = Setropts::new();
        let mut rm = ResourceManager::new();

        setropts.classact(&mut rm, &["FACILITY"]);
        assert!(rm.is_class_active("FACILITY"));

        setropts.no_classact(&mut rm, &["FACILITY"]);
        assert!(!rm.is_class_active("FACILITY"));
    }

    #[test]
    fn test_raclist_enables_in_storage() {
        let mut setropts = Setropts::new();
        let mut rm = ResourceManager::new();

        setropts.raclist(&mut rm, &["FACILITY", "PROGRAM"]);

        assert!(rm.is_class_raclisted("FACILITY"));
        assert!(rm.is_class_raclisted("PROGRAM"));
    }

    #[test]
    fn test_generic_classes() {
        let mut setropts = Setropts::new();

        setropts.generic(&["FACILITY", "PROGRAM"]);
        assert!(setropts.is_generic("FACILITY"));
        assert!(setropts.is_generic("PROGRAM"));

        setropts.no_generic(&["FACILITY"]);
        assert!(!setropts.is_generic("FACILITY"));
        assert!(setropts.is_generic("PROGRAM"));
    }

    #[test]
    fn test_gencmd_classes() {
        let mut setropts = Setropts::new();
        setropts.gencmd(&["FACILITY"]);
        assert!(setropts.is_gencmd("FACILITY"));
    }

    #[test]
    fn test_genlist_classes() {
        let mut setropts = Setropts::new();
        setropts.genlist(&["FACILITY"]);
        assert!(setropts.options.genlist_classes.contains("FACILITY"));
    }

    #[test]
    fn test_global_classes() {
        let mut setropts = Setropts::new();
        setropts.global(&["FACILITY"]);
        assert!(setropts.options.global_classes.contains("FACILITY"));
    }

    // ─────── S103.2: Password Policy ───────

    #[test]
    fn test_password_interval() {
        let mut setropts = Setropts::new();
        setropts.password(Some(60), None, None, None, None);
        assert_eq!(setropts.password_policy().interval, 60);
    }

    #[test]
    fn test_password_history() {
        let mut setropts = Setropts::new();
        setropts.password(None, Some(16), None, None, None);
        assert_eq!(setropts.password_policy().history, 16);
    }

    #[test]
    fn test_password_revoke() {
        let mut setropts = Setropts::new();
        setropts.password(None, None, None, None, Some(5));
        assert_eq!(setropts.password_policy().max_failures, 5);
    }

    #[test]
    fn test_password_minchange_and_warning() {
        let mut setropts = Setropts::new();
        setropts.password(None, None, Some(1), Some(14), None);
        assert_eq!(setropts.get_password_rules().min_change_days, 1);
        assert_eq!(setropts.get_password_rules().warning_days, 14);
    }

    #[test]
    fn test_password_all_options() {
        let mut setropts = Setropts::new();
        setropts.password(Some(90), Some(32), Some(1), Some(14), Some(5));
        assert_eq!(setropts.password_policy().interval, 90);
        assert_eq!(setropts.password_policy().history, 32);
        assert_eq!(setropts.password_policy().max_failures, 5);
        assert_eq!(setropts.get_password_rules().min_change_days, 1);
        assert_eq!(setropts.get_password_rules().warning_days, 14);
    }

    #[test]
    fn test_password_rules_syntax() {
        let mut setropts = Setropts::new();
        setropts.password_rules(Some(6), Some(12), Some(true), Some(true), Some(false));

        let rules = setropts.get_password_rules();
        assert_eq!(rules.min_length, 6);
        assert_eq!(rules.max_length, 12);
        assert!(rules.require_numeric);
        assert!(rules.require_mixed_case);
        assert!(!rules.require_special);

        // Should also sync to password policy.
        assert_eq!(setropts.password_policy().min_length, 6);
        assert_eq!(setropts.password_policy().max_length, 12);
    }

    // ─────── S103.3: Audit Options ───────

    #[test]
    fn test_audit_enable_disable() {
        let mut setropts = Setropts::new();

        setropts.audit(&["FACILITY", "PROGRAM"]);
        assert!(setropts.is_audit("FACILITY"));
        assert!(setropts.is_audit("PROGRAM"));

        setropts.no_audit(&["FACILITY"]);
        assert!(!setropts.is_audit("FACILITY"));
        assert!(setropts.is_audit("PROGRAM"));
    }

    #[test]
    fn test_saudit() {
        let mut setropts = Setropts::new();
        setropts.saudit(&["FACILITY"]);

        let config = setropts.get_audit_config("FACILITY").unwrap();
        assert!(config.saudit);
    }

    #[test]
    fn test_logoptions() {
        let mut setropts = Setropts::new();
        setropts.logoptions("FACILITY", LogOption::All);

        let config = setropts.get_audit_config("FACILITY").unwrap();
        assert_eq!(config.log_option, LogOption::All);
    }

    #[test]
    fn test_statistics() {
        let mut setropts = Setropts::new();
        setropts.statistics(&["FACILITY"]);

        let config = setropts.get_audit_config("FACILITY").unwrap();
        assert!(config.statistics);
    }

    #[test]
    fn test_operaudit() {
        let mut setropts = Setropts::new();
        assert!(!setropts.options.oper_audit);

        setropts.operaudit(true);
        assert!(setropts.options.oper_audit);
    }

    // ─────── S103.4: System Protection ───────

    #[test]
    fn test_protect_all() {
        let mut setropts = Setropts::new();
        assert!(!setropts.is_protect_all());

        setropts.protect_all(true);
        assert!(setropts.is_protect_all());
    }

    #[test]
    fn test_grplist() {
        let mut setropts = Setropts::new();
        assert!(setropts.is_grplist()); // Default is true.

        setropts.grplist(false);
        assert!(!setropts.is_grplist());
    }

    #[test]
    fn test_erase() {
        let mut setropts = Setropts::new();
        setropts.erase(&["DASDVOL"]);
        assert!(setropts.options.erase_classes.contains("DASDVOL"));

        setropts.no_erase(&["DASDVOL"]);
        assert!(!setropts.options.erase_classes.contains("DASDVOL"));
    }

    // ─────── S103.5: MAC Settings ───────

    #[test]
    fn test_mls_settings() {
        let mut setropts = Setropts::new();
        assert!(!setropts.mac_settings().mls);

        setropts.mls(true);
        assert!(setropts.mac_settings().mls);
    }

    #[test]
    fn test_mlactive() {
        let mut setropts = Setropts::new();
        setropts.mlactive(true);
        assert!(setropts.mac_settings().mlactive);
    }

    #[test]
    fn test_mlquiet() {
        let mut setropts = Setropts::new();
        setropts.mlquiet(true);
        assert!(setropts.mac_settings().mlquiet);
    }

    #[test]
    fn test_mlstable() {
        let mut setropts = Setropts::new();
        setropts.mlstable(true);
        assert!(setropts.mac_settings().mlstable);
    }

    // ─────── S103.6: REFRESH and LIST ───────

    #[test]
    fn test_refresh_returns_raclisted() {
        let mut setropts = Setropts::new();
        let mut rm = ResourceManager::new();

        setropts.raclist(&mut rm, &["FACILITY", "PROGRAM"]);
        let refreshed = setropts.refresh(&rm);
        assert_eq!(refreshed.len(), 2);
        assert!(refreshed.contains(&"FACILITY".to_string()));
        assert!(refreshed.contains(&"PROGRAM".to_string()));
    }

    #[test]
    fn test_list_displays_options() {
        let mut setropts = Setropts::new();
        let mut rm = ResourceManager::new();

        setropts.classact(&mut rm, &["FACILITY"]);
        setropts.raclist(&mut rm, &["FACILITY"]);
        setropts.password(Some(60), Some(16), None, None, Some(5));

        let result = setropts.list(&rm);
        assert_eq!(result.active_classes.len(), 1);
        assert!(result.active_classes.contains(&"FACILITY".to_string()));
        assert_eq!(result.raclisted_classes.len(), 1);
        assert_eq!(result.options.password.interval, 60);
        assert_eq!(result.options.password.history, 16);
        assert_eq!(result.options.password.max_failures, 5);
    }

    // ─────── Session/JES settings ───────

    #[test]
    fn test_session_interval() {
        let mut setropts = Setropts::new();
        setropts.session_interval(30);
        assert_eq!(setropts.options.session_interval, 30);
    }

    #[test]
    fn test_jes_batch_all_racf() {
        let mut setropts = Setropts::new();
        setropts.jes_batch_all_racf(true);
        assert!(setropts.options.jes_batch_all_racf);
    }

    // ─────── Integration: classact + raclist together ───────

    #[test]
    fn test_full_class_setup() {
        let mut setropts = Setropts::new();
        let mut rm = ResourceManager::new();

        // Typical SETROPTS for activating FACILITY with all features.
        setropts.classact(&mut rm, &["FACILITY"]);
        setropts.raclist(&mut rm, &["FACILITY"]);
        setropts.generic(&["FACILITY"]);
        setropts.gencmd(&["FACILITY"]);
        setropts.audit(&["FACILITY"]);

        assert!(rm.is_class_active("FACILITY"));
        assert!(rm.is_class_raclisted("FACILITY"));
        assert!(setropts.is_generic("FACILITY"));
        assert!(setropts.is_gencmd("FACILITY"));
        assert!(setropts.is_audit("FACILITY"));
    }

    // ─────── Default values ───────

    #[test]
    fn test_default_options() {
        let setropts = Setropts::new();
        assert_eq!(setropts.password_policy().interval, 90);
        assert_eq!(setropts.password_policy().history, 32);
        assert_eq!(setropts.password_policy().max_failures, 3);
        assert_eq!(setropts.password_policy().min_length, 4);
        assert_eq!(setropts.password_policy().max_length, 8);
        assert!(setropts.is_grplist());
        assert!(!setropts.is_protect_all());
        assert!(!setropts.mac_settings().mls);
        assert_eq!(setropts.options.session_interval, 0);
    }
}
