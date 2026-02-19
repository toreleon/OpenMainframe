//! PL/I Exception Handling — ON-units, SIGNAL, REVERT, condition inquiry.
//!
//! Implements PL/I's comprehensive exception handling system with ~25 conditions,
//! ON-unit establishment, SIGNAL/REVERT, and condition inquiry built-in functions
//! (ONCODE, ONLOC, ONCHAR, ONSOURCE, ONFILE, ONKEY, ONCOUNT).

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
//  Condition codes
// ---------------------------------------------------------------------------

/// PL/I condition identifiers for ON-unit registration.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Condition {
    Area,
    Attention,
    Conversion,
    Endfile(Option<String>),
    Endpage(Option<String>),
    Error,
    Finish,
    Fixedoverflow,
    Invalidop,
    Key(Option<String>),
    Name(Option<String>),
    Overflow,
    Record(Option<String>),
    Size,
    Storage,
    Stringrange,
    Stringsize,
    Subscriptrange,
    Transmit(Option<String>),
    Undefinedfile(Option<String>),
    Underflow,
    Zerodivide,
    /// User-defined condition: CONDITION(name).
    UserDefined(String),
}

impl Condition {
    /// Get the standard numeric code for this condition.
    pub fn code(&self) -> i32 {
        match self {
            Condition::Area => 20,
            Condition::Attention => 400,
            Condition::Conversion => 600,
            Condition::Endfile(_) => 70,
            Condition::Endpage(_) => 80,
            Condition::Error => 1000,
            Condition::Finish => 90,
            Condition::Fixedoverflow => 310,
            Condition::Invalidop => 340,
            Condition::Key(_) => 50,
            Condition::Name(_) => 10,
            Condition::Overflow => 300,
            Condition::Record(_) => 40,
            Condition::Size => 320,
            Condition::Storage => 450,
            Condition::Stringrange => 350,
            Condition::Stringsize => 150,
            Condition::Subscriptrange => 520,
            Condition::Transmit(_) => 60,
            Condition::Undefinedfile(_) => 30,
            Condition::Underflow => 330,
            Condition::Zerodivide => 500,
            Condition::UserDefined(_) => 9000,
        }
    }

    /// Get the condition name for display.
    pub fn name(&self) -> String {
        match self {
            Condition::Area => "AREA".to_string(),
            Condition::Attention => "ATTENTION".to_string(),
            Condition::Conversion => "CONVERSION".to_string(),
            Condition::Endfile(f) => match f {
                Some(file) => format!("ENDFILE({file})"),
                None => "ENDFILE".to_string(),
            },
            Condition::Endpage(f) => match f {
                Some(file) => format!("ENDPAGE({file})"),
                None => "ENDPAGE".to_string(),
            },
            Condition::Error => "ERROR".to_string(),
            Condition::Finish => "FINISH".to_string(),
            Condition::Fixedoverflow => "FIXEDOVERFLOW".to_string(),
            Condition::Invalidop => "INVALIDOP".to_string(),
            Condition::Key(f) => match f {
                Some(file) => format!("KEY({file})"),
                None => "KEY".to_string(),
            },
            Condition::Name(f) => match f {
                Some(file) => format!("NAME({file})"),
                None => "NAME".to_string(),
            },
            Condition::Overflow => "OVERFLOW".to_string(),
            Condition::Record(f) => match f {
                Some(file) => format!("RECORD({file})"),
                None => "RECORD".to_string(),
            },
            Condition::Size => "SIZE".to_string(),
            Condition::Storage => "STORAGE".to_string(),
            Condition::Stringrange => "STRINGRANGE".to_string(),
            Condition::Stringsize => "STRINGSIZE".to_string(),
            Condition::Subscriptrange => "SUBSCRIPTRANGE".to_string(),
            Condition::Transmit(f) => match f {
                Some(file) => format!("TRANSMIT({file})"),
                None => "TRANSMIT".to_string(),
            },
            Condition::Undefinedfile(f) => match f {
                Some(file) => format!("UNDEFINEDFILE({file})"),
                None => "UNDEFINEDFILE".to_string(),
            },
            Condition::Underflow => "UNDERFLOW".to_string(),
            Condition::Zerodivide => "ZERODIVIDE".to_string(),
            Condition::UserDefined(name) => format!("CONDITION({name})"),
        }
    }

    /// Get the lookup key for ON-unit registration (file-specific conditions
    /// match both specific and generic registrations).
    pub fn lookup_key(&self) -> String {
        match self {
            Condition::Endfile(_) => "ENDFILE".to_string(),
            Condition::Endpage(_) => "ENDPAGE".to_string(),
            Condition::Key(_) => "KEY".to_string(),
            Condition::Name(_) => "NAME".to_string(),
            Condition::Record(_) => "RECORD".to_string(),
            Condition::Transmit(_) => "TRANSMIT".to_string(),
            Condition::Undefinedfile(_) => "UNDEFINEDFILE".to_string(),
            _ => self.name(),
        }
    }
}

// ---------------------------------------------------------------------------
//  ON-unit action
// ---------------------------------------------------------------------------

/// The action to take when a condition is raised.
#[derive(Debug, Clone)]
pub enum OnAction {
    /// Execute a block of statements (captured as an opaque action ID).
    UserBlock(usize),
    /// SYSTEM action — default system behaviour.
    System,
    /// SNAP — print traceback then continue with SYSTEM action.
    Snap,
}

// ---------------------------------------------------------------------------
//  Condition context
// ---------------------------------------------------------------------------

/// Context information about the currently raised condition.
/// Available via condition inquiry built-in functions.
#[derive(Debug, Clone, Default)]
pub struct ConditionContext {
    /// ONCODE — numeric condition code.
    pub oncode: i32,
    /// ONLOC — name of the procedure where condition was raised.
    pub onloc: String,
    /// ONCHAR — the character that caused CONVERSION.
    pub onchar: String,
    /// ONSOURCE — the source string that caused CONVERSION.
    pub onsource: String,
    /// ONFILE — the file name for I/O conditions.
    pub onfile: String,
    /// ONKEY — the key value for KEY condition.
    pub onkey: String,
    /// ONCOUNT — for GET/PUT, the number of items transferred.
    pub oncount: i32,
}

// ---------------------------------------------------------------------------
//  ON-unit stack
// ---------------------------------------------------------------------------

/// An ON-unit registration entry.
#[derive(Debug, Clone)]
struct OnUnit {
    /// The condition this ON-unit handles.
    condition_key: String,
    /// The action to take.
    action: OnAction,
    /// Scope depth at registration time (for REVERT).
    scope_depth: usize,
}

/// Manages the stack of ON-units and condition handling.
pub struct ConditionManager {
    /// Stack of ON-unit registrations (newest last).
    on_units: Vec<OnUnit>,
    /// Current scope depth (incremented/decremented by procedures and blocks).
    scope_depth: usize,
    /// Action blocks (statements stored by index).
    action_blocks: Vec<Vec<String>>,
    /// Current condition context.
    context: ConditionContext,
    /// Whether a condition is currently being handled.
    handling: bool,
    /// Enabled conditions (some conditions like SIZE, STRINGRANGE are disabled by default).
    enabled: HashMap<String, bool>,
}

impl ConditionManager {
    /// Create a new condition manager with default enabled conditions.
    pub fn new() -> Self {
        let mut enabled = HashMap::new();
        // Enabled by default.
        for cond in &[
            "AREA",
            "ATTENTION",
            "CONVERSION",
            "ENDFILE",
            "ENDPAGE",
            "ERROR",
            "FINISH",
            "FIXEDOVERFLOW",
            "INVALIDOP",
            "KEY",
            "NAME",
            "OVERFLOW",
            "RECORD",
            "STORAGE",
            "TRANSMIT",
            "UNDEFINEDFILE",
            "UNDERFLOW",
            "ZERODIVIDE",
        ] {
            enabled.insert(cond.to_string(), true);
        }
        // Disabled by default (must be explicitly enabled with prefix conditions).
        for cond in &["SIZE", "STRINGRANGE", "STRINGSIZE", "SUBSCRIPTRANGE"] {
            enabled.insert(cond.to_string(), false);
        }

        Self {
            on_units: Vec::new(),
            scope_depth: 0,
            action_blocks: Vec::new(),
            context: ConditionContext::default(),
            handling: false,
            enabled,
        }
    }

    /// Register an ON-unit for a condition.
    pub fn establish(&mut self, condition: &Condition, action: OnAction) {
        let key = condition.lookup_key();
        self.on_units.push(OnUnit {
            condition_key: key,
            action,
            scope_depth: self.scope_depth,
        });
    }

    /// Register a user block action, returning the block ID.
    pub fn register_block(&mut self, stmts: Vec<String>) -> usize {
        let id = self.action_blocks.len();
        self.action_blocks.push(stmts);
        id
    }

    /// Remove the most recent ON-unit for a condition (REVERT).
    pub fn revert(&mut self, condition: &Condition) {
        let key = condition.lookup_key();
        // Find and remove the most recent ON-unit at the current scope depth.
        if let Some(pos) = self
            .on_units
            .iter()
            .rposition(|u| u.condition_key == key && u.scope_depth == self.scope_depth)
        {
            self.on_units.remove(pos);
        }
    }

    /// Look up the ON-unit action for a raised condition.
    pub fn find_handler(&self, condition: &Condition) -> Option<&OnAction> {
        let key = condition.lookup_key();
        // Search from most recent to oldest.
        self.on_units
            .iter()
            .rev()
            .find(|u| u.condition_key == key)
            .map(|u| &u.action)
    }

    /// Raise a condition: sets context and returns the action to take.
    pub fn raise(&mut self, condition: &Condition, context: ConditionContext) -> RaiseResult {
        // Check if the condition is enabled.
        let key = condition.lookup_key();
        if let Some(false) = self.enabled.get(&key) {
            return RaiseResult::Ignored;
        }

        self.context = context;
        self.context.oncode = condition.code();
        self.handling = true;

        if let Some(action) = self.find_handler(condition).cloned() {
            match action {
                OnAction::UserBlock(id) => RaiseResult::ExecuteBlock(id),
                OnAction::System => RaiseResult::SystemAction,
                OnAction::Snap => RaiseResult::SnapAndSystem,
            }
        } else {
            // No handler registered — use ERROR handler if this isn't ERROR.
            if !matches!(condition, Condition::Error) {
                if let Some(action) = self.find_handler(&Condition::Error).cloned() {
                    match action {
                        OnAction::UserBlock(id) => return RaiseResult::ExecuteBlock(id),
                        OnAction::System => return RaiseResult::SystemAction,
                        OnAction::Snap => return RaiseResult::SnapAndSystem,
                    }
                }
            }
            RaiseResult::SystemAction
        }
    }

    /// Signal completion of condition handling.
    pub fn complete_handling(&mut self) {
        self.handling = false;
    }

    /// Enter a new scope.
    pub fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// Leave the current scope, removing all ON-units established in it.
    pub fn leave_scope(&mut self) {
        self.on_units
            .retain(|u| u.scope_depth != self.scope_depth);
        if self.scope_depth > 0 {
            self.scope_depth -= 1;
        }
    }

    /// Enable a condition (prefix condition).
    pub fn enable(&mut self, condition: &str) {
        self.enabled.insert(condition.to_uppercase(), true);
    }

    /// Disable a condition (NOSIZE, etc.).
    pub fn disable(&mut self, condition: &str) {
        self.enabled.insert(condition.to_uppercase(), false);
    }

    /// Check if a condition is enabled.
    pub fn is_enabled(&self, condition: &str) -> bool {
        self.enabled
            .get(&condition.to_uppercase())
            .copied()
            .unwrap_or(true) // unknown conditions are enabled by default
    }

    // ─── Condition inquiry functions ───

    /// ONCODE() — returns the condition code of the current condition.
    pub fn oncode(&self) -> i32 {
        self.context.oncode
    }

    /// ONLOC() — returns the procedure name where condition was raised.
    pub fn onloc(&self) -> &str {
        &self.context.onloc
    }

    /// ONCHAR() — returns the character that caused CONVERSION.
    pub fn onchar(&self) -> &str {
        &self.context.onchar
    }

    /// ONSOURCE() — returns the source string that caused CONVERSION.
    pub fn onsource(&self) -> &str {
        &self.context.onsource
    }

    /// ONFILE() — returns the file name for I/O conditions.
    pub fn onfile(&self) -> &str {
        &self.context.onfile
    }

    /// ONKEY() — returns the key value for KEY condition.
    pub fn onkey(&self) -> &str {
        &self.context.onkey
    }

    /// ONCOUNT() — returns the number of items transferred.
    pub fn oncount(&self) -> i32 {
        self.context.oncount
    }

    /// Check if currently handling a condition.
    pub fn is_handling(&self) -> bool {
        self.handling
    }

    /// Get the current scope depth.
    pub fn scope_depth(&self) -> usize {
        self.scope_depth
    }

    /// Get the number of registered ON-units.
    pub fn on_unit_count(&self) -> usize {
        self.on_units.len()
    }
}

impl Default for ConditionManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of raising a condition.
#[derive(Debug, Clone, PartialEq)]
pub enum RaiseResult {
    /// Execute the user ON-unit block (by block ID).
    ExecuteBlock(usize),
    /// Execute default system action (typically terminate).
    SystemAction,
    /// Print SNAP traceback, then system action.
    SnapAndSystem,
    /// Condition is disabled — ignore.
    Ignored,
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─── P105.1: ON-Unit establishment and condition handling ───

    #[test]
    fn test_establish_on_unit() {
        let mut mgr = ConditionManager::new();
        let block_id = mgr.register_block(vec!["PUT LIST('Error!')".to_string()]);
        mgr.establish(&Condition::Zerodivide, OnAction::UserBlock(block_id));

        let handler = mgr.find_handler(&Condition::Zerodivide);
        assert!(handler.is_some());
        assert!(matches!(handler.unwrap(), OnAction::UserBlock(0)));
    }

    #[test]
    fn test_raise_with_handler() {
        let mut mgr = ConditionManager::new();
        let block_id = mgr.register_block(vec!["GOTO RECOVERY".to_string()]);
        mgr.establish(&Condition::Zerodivide, OnAction::UserBlock(block_id));

        let ctx = ConditionContext::default();
        let result = mgr.raise(&Condition::Zerodivide, ctx);
        assert_eq!(result, RaiseResult::ExecuteBlock(0));
        assert_eq!(mgr.oncode(), 500); // ZERODIVIDE code
    }

    #[test]
    fn test_raise_without_handler() {
        let mut mgr = ConditionManager::new();
        let result = mgr.raise(&Condition::Zerodivide, ConditionContext::default());
        assert_eq!(result, RaiseResult::SystemAction);
    }

    #[test]
    fn test_raise_falls_back_to_error() {
        let mut mgr = ConditionManager::new();
        let block_id = mgr.register_block(vec!["CALL ERROR_HANDLER".to_string()]);
        mgr.establish(&Condition::Error, OnAction::UserBlock(block_id));

        // Raise CONVERSION — no specific handler, should fall to ERROR.
        let result = mgr.raise(&Condition::Conversion, ConditionContext::default());
        assert_eq!(result, RaiseResult::ExecuteBlock(0));
    }

    #[test]
    fn test_endfile_condition() {
        let mut mgr = ConditionManager::new();
        let block_id = mgr.register_block(vec!["EOF = '1'B".to_string()]);
        mgr.establish(
            &Condition::Endfile(Some("INFILE".to_string())),
            OnAction::UserBlock(block_id),
        );

        // ENDFILE(INFILE) should match the generic ENDFILE handler.
        let handler = mgr.find_handler(&Condition::Endfile(Some("INFILE".to_string())));
        assert!(handler.is_some());
    }

    #[test]
    fn test_multiple_on_units_last_wins() {
        let mut mgr = ConditionManager::new();
        let b1 = mgr.register_block(vec!["first".to_string()]);
        let b2 = mgr.register_block(vec!["second".to_string()]);
        mgr.establish(&Condition::Overflow, OnAction::UserBlock(b1));
        mgr.establish(&Condition::Overflow, OnAction::UserBlock(b2));

        // Most recent ON-unit should win.
        let handler = mgr.find_handler(&Condition::Overflow);
        assert!(matches!(handler.unwrap(), OnAction::UserBlock(1)));
    }

    #[test]
    fn test_system_action() {
        let mut mgr = ConditionManager::new();
        mgr.establish(&Condition::Overflow, OnAction::System);

        let result = mgr.raise(&Condition::Overflow, ConditionContext::default());
        assert_eq!(result, RaiseResult::SystemAction);
    }

    #[test]
    fn test_snap_action() {
        let mut mgr = ConditionManager::new();
        mgr.establish(&Condition::Error, OnAction::Snap);

        let result = mgr.raise(&Condition::Error, ConditionContext::default());
        assert_eq!(result, RaiseResult::SnapAndSystem);
    }

    #[test]
    fn test_disabled_condition_ignored() {
        let mut mgr = ConditionManager::new();
        // SIZE is disabled by default.
        let block_id = mgr.register_block(vec!["handler".to_string()]);
        mgr.establish(&Condition::Size, OnAction::UserBlock(block_id));

        let result = mgr.raise(&Condition::Size, ConditionContext::default());
        assert_eq!(result, RaiseResult::Ignored);
    }

    #[test]
    fn test_enable_disabled_condition() {
        let mut mgr = ConditionManager::new();
        mgr.enable("SIZE");
        let block_id = mgr.register_block(vec!["handler".to_string()]);
        mgr.establish(&Condition::Size, OnAction::UserBlock(block_id));

        let result = mgr.raise(&Condition::Size, ConditionContext::default());
        assert_eq!(result, RaiseResult::ExecuteBlock(0));
    }

    // ─── P105.2: SIGNAL, REVERT, and condition built-in functions ───

    #[test]
    fn test_signal_user_condition() {
        let mut mgr = ConditionManager::new();
        let block_id = mgr.register_block(vec!["handle MY_ERROR".to_string()]);
        mgr.establish(
            &Condition::UserDefined("MY_ERROR".to_string()),
            OnAction::UserBlock(block_id),
        );

        let result = mgr.raise(
            &Condition::UserDefined("MY_ERROR".to_string()),
            ConditionContext::default(),
        );
        assert_eq!(result, RaiseResult::ExecuteBlock(0));
    }

    #[test]
    fn test_revert_removes_handler() {
        let mut mgr = ConditionManager::new();
        let block_id = mgr.register_block(vec!["handler".to_string()]);
        mgr.establish(&Condition::Zerodivide, OnAction::UserBlock(block_id));

        assert!(mgr.find_handler(&Condition::Zerodivide).is_some());
        mgr.revert(&Condition::Zerodivide);
        assert!(mgr.find_handler(&Condition::Zerodivide).is_none());
    }

    #[test]
    fn test_revert_only_removes_current_scope() {
        let mut mgr = ConditionManager::new();

        // Establish in scope 0.
        let b1 = mgr.register_block(vec!["outer".to_string()]);
        mgr.establish(&Condition::Overflow, OnAction::UserBlock(b1));

        // Enter inner scope.
        mgr.enter_scope();
        let b2 = mgr.register_block(vec!["inner".to_string()]);
        mgr.establish(&Condition::Overflow, OnAction::UserBlock(b2));

        // REVERT in inner scope.
        mgr.revert(&Condition::Overflow);

        // Outer handler should still be visible.
        let handler = mgr.find_handler(&Condition::Overflow);
        assert!(matches!(handler.unwrap(), OnAction::UserBlock(0)));

        mgr.leave_scope();
    }

    #[test]
    fn test_scope_cleanup() {
        let mut mgr = ConditionManager::new();

        mgr.enter_scope();
        let b = mgr.register_block(vec!["scoped handler".to_string()]);
        mgr.establish(&Condition::Conversion, OnAction::UserBlock(b));
        assert_eq!(mgr.on_unit_count(), 1);

        mgr.leave_scope();
        assert_eq!(mgr.on_unit_count(), 0);
    }

    #[test]
    fn test_condition_inquiry_oncode() {
        let mut mgr = ConditionManager::new();
        let ctx = ConditionContext {
            oncode: 0,
            onloc: "MAIN_PROC".to_string(),
            ..Default::default()
        };
        mgr.raise(&Condition::Zerodivide, ctx);
        assert_eq!(mgr.oncode(), 500);
        assert_eq!(mgr.onloc(), "MAIN_PROC");
    }

    #[test]
    fn test_condition_inquiry_conversion() {
        let mut mgr = ConditionManager::new();
        let ctx = ConditionContext {
            onchar: "A".to_string(),
            onsource: "12A34".to_string(),
            ..Default::default()
        };
        mgr.raise(&Condition::Conversion, ctx);
        assert_eq!(mgr.oncode(), 600);
        assert_eq!(mgr.onchar(), "A");
        assert_eq!(mgr.onsource(), "12A34");
    }

    #[test]
    fn test_condition_inquiry_file() {
        let mut mgr = ConditionManager::new();
        let ctx = ConditionContext {
            onfile: "MASTER".to_string(),
            onkey: "KEY001".to_string(),
            ..Default::default()
        };
        mgr.raise(&Condition::Key(Some("MASTER".to_string())), ctx);
        assert_eq!(mgr.onfile(), "MASTER");
        assert_eq!(mgr.onkey(), "KEY001");
    }

    #[test]
    fn test_complete_handling() {
        let mut mgr = ConditionManager::new();
        mgr.raise(&Condition::Error, ConditionContext::default());
        assert!(mgr.is_handling());
        mgr.complete_handling();
        assert!(!mgr.is_handling());
    }

    #[test]
    fn test_condition_codes() {
        assert_eq!(Condition::Zerodivide.code(), 500);
        assert_eq!(Condition::Overflow.code(), 300);
        assert_eq!(Condition::Conversion.code(), 600);
        assert_eq!(Condition::Error.code(), 1000);
        assert_eq!(Condition::Endfile(None).code(), 70);
        assert_eq!(Condition::Storage.code(), 450);
        assert_eq!(Condition::UserDefined("X".to_string()).code(), 9000);
    }

    #[test]
    fn test_condition_names() {
        assert_eq!(Condition::Zerodivide.name(), "ZERODIVIDE");
        assert_eq!(
            Condition::Endfile(Some("F1".to_string())).name(),
            "ENDFILE(F1)"
        );
        assert_eq!(
            Condition::UserDefined("MY_COND".to_string()).name(),
            "CONDITION(MY_COND)"
        );
    }

    #[test]
    fn test_all_conditions_enabled_by_default() {
        let mgr = ConditionManager::new();
        assert!(mgr.is_enabled("ZERODIVIDE"));
        assert!(mgr.is_enabled("OVERFLOW"));
        assert!(mgr.is_enabled("CONVERSION"));
        assert!(mgr.is_enabled("ERROR"));
        assert!(mgr.is_enabled("ENDFILE"));
    }

    #[test]
    fn test_disabled_by_default_conditions() {
        let mgr = ConditionManager::new();
        assert!(!mgr.is_enabled("SIZE"));
        assert!(!mgr.is_enabled("STRINGRANGE"));
        assert!(!mgr.is_enabled("STRINGSIZE"));
        assert!(!mgr.is_enabled("SUBSCRIPTRANGE"));
    }

    #[test]
    fn test_nested_scopes() {
        let mut mgr = ConditionManager::new();

        // Scope 0.
        let b0 = mgr.register_block(vec!["global".to_string()]);
        mgr.establish(&Condition::Error, OnAction::UserBlock(b0));

        // Scope 1.
        mgr.enter_scope();
        let b1 = mgr.register_block(vec!["scope1".to_string()]);
        mgr.establish(&Condition::Overflow, OnAction::UserBlock(b1));
        assert_eq!(mgr.on_unit_count(), 2);

        // Scope 2.
        mgr.enter_scope();
        let b2 = mgr.register_block(vec!["scope2".to_string()]);
        mgr.establish(&Condition::Zerodivide, OnAction::UserBlock(b2));
        assert_eq!(mgr.on_unit_count(), 3);

        // Leave scope 2 — zerodivide handler removed.
        mgr.leave_scope();
        assert_eq!(mgr.on_unit_count(), 2);
        assert!(mgr.find_handler(&Condition::Zerodivide).is_none());
        assert!(mgr.find_handler(&Condition::Overflow).is_some());

        // Leave scope 1 — overflow handler removed.
        mgr.leave_scope();
        assert_eq!(mgr.on_unit_count(), 1);
        assert!(mgr.find_handler(&Condition::Error).is_some());
    }
}
