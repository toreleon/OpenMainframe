# Code Equivalence Testing Strategy

**Goal:** Prove `COBOL_Program(input) ≡ Java_Program(input)` for all valid inputs

---

## Overview: Testing Pyramid

```
                    ┌─────────────────┐
                    │  Formal Proof   │  ← Z3 SMT Solver (100% confidence)
                    │  (Z3 Equivalence)│
                   ─┴─────────────────┴─
                  ╱                   ╲
                 ╱  Concolic Testing   ╲  ← Symbolic + Concrete (95% confidence)
                ╱   (Path Coverage)     ╲
               ───────────────────────────
              ╱                           ╲
             ╱    Differential Testing     ╲  ← Same inputs, compare outputs (90% confidence)
            ╱     (Golden Master Tests)     ╲
           ───────────────────────────────────
          ╱                                   ╲
         ╱      Property-Based Testing         ╲  ← Invariants hold (85% confidence)
        ╱       (Pre/Post Conditions)           ╲
       ───────────────────────────────────────────
      ╱                                           ╲
     ╱          Shadow Mode / Parallel Run         ╲  ← Production traffic (99% confidence)
    ╱           (Real-world validation)             ╲
   ───────────────────────────────────────────────────
```

---

## 1. Formal Verification (Z3 SMT Solver)

### 1.1 Approach

```rust
// Prove: ∀inputs, COBOL_output = Java_output

pub fn prove_equivalence(
    cobol_ir: &FlatProgram,
    java_ir: &FlatProgram,
) -> EquivalenceProof {
    let solver = Z3Solver::new();
    
    // 1. Create symbolic inputs
    let inputs = create_symbolic_inputs(&cobol_ir.signature);
    
    // 2. Execute both programs symbolically
    let cobol_paths = symbolic_execute(cobol_ir, &inputs);
    let java_paths = symbolic_execute(java_ir, &inputs);
    
    // 3. For each path pair, check output equivalence
    let mut proof = EquivalenceProof::new();
    for (cobol_path, java_path) in zip(cobol_paths, java_paths) {
        let condition = cobol_path.condition && java_path.condition;
        let output_eq = cobol_path.output == java_path.output;
        
        // Ask Z3: Is there any input where outputs differ?
        let counterexample = solver.find_counterexample(
            &condition, 
            &!output_eq
        );
        
        if counterexample.is_some() {
            proof.add_counterexample(counterexample.unwrap());
        } else {
            proof.add_verified_path(condition);
        }
    }
    
    proof
}
```

### 1.2 Example: Simple Validation

**COBOL:**
```cobol
IF ACCOUNT-BALANCE > 10000
    MOVE 'PREMIUM' TO TIER
ELSE
    MOVE 'STANDARD' TO TIER
END-IF
```

**Java:**
```java
if (accountBalance > 10000) {
    tier = "PREMIUM";
} else {
    tier = "STANDARD";
}
```

**Z3 Query:**
```python
from z3 import *

# Symbolic input
balance = Int('balance')
cobol_tier = String('cobol_tier')
java_tier = String('java_tier')

# COBOL semantics
cobol_constraints = [
    Implies(balance > 10000, cobol_tier == "PREMIUM"),
    Implies(balance <= 10000, cobol_tier == "STANDARD")
]

# Java semantics
java_constraints = [
    Implies(balance > 10000, java_tier == "PREMIUM"),
    Implies(balance <= 10000, java_tier == "STANDARD")
]

# Check equivalence
s = Solver()
s.add(cobol_constraints + java_constraints)
s.add(cobol_tier != java_tier)  # Negation: find counterexample

if s.check() == unsat:
    print("✅ EQUIVALENT - No counterexample exists")
else:
    print("❌ NOT EQUIVALENT")
    print("Counterexample:", s.model())
```

### 1.3 When It Works

| Scenario | Works? | Notes |
|----------|--------|-------|
| Simple IF-THEN-ELSE | ✅ | Fast, complete proof |
| Arithmetic calculations | ✅ | Z3 handles Int/Real well |
| String manipulation | ⚠️ | Limited support, may need bounded strings |
| Array/Table operations | ⚠️ | Need to bound array sizes |
| External I/O (files, DB) | ❌ | Need to mock/symbolize |
| GO TO / complex control flow | ⚠️ | May cause path explosion |

### 1.4 Limitations

- **Path explosion:** 100 branch points → 2^100 paths
- **Undecidable:** Some theories Z3 can't solve
- **Performance:** Large programs take hours/days

**Mitigation:** Use bounded symbolic execution (limit path depth)

---

## 2. Concolic Testing (Concrete + Symbolic)

### 2.1 Approach

```
1. Run COBOL with concrete input → record path
2. Symbolically execute same path → get path condition
3. Negate one constraint → generate new input
4. Run both COBOL and Java with new input
5. Compare outputs
6. Repeat until coverage goal met
```

### 2.2 Example

```rust
pub fn concolic_test(cobol: &Program, java: &Program) -> TestReport {
    let mut queue = vec![initial_input()];
    let mut tested = HashSet::new();
    let mut report = TestReport::new();
    
    while let Some(input) = queue.pop() {
        if tested.contains(&input) { continue; }
        
        // Run both programs
        let cobol_output = cobol.run(&input);
        let java_output = java.run(&input);
        
        // Compare
        if cobol_output != java_output {
            report.add_failure(input, cobol_output, java_output);
        } else {
            report.add_success(input);
        }
        
        // Generate new inputs by negating path conditions
        let cobol_paths = symbolic_paths(cobol, &input);
        for path in cobol_paths {
            for constraint in &path.constraints {
                let new_input = negate_constraint(&constraint, &input);
                queue.push(new_input);
            }
        }
        
        tested.insert(input);
        
        // Stop when we have enough coverage
        if report.coverage() > 0.95 { break; }
    }
    
    report
}
```

### 2.3 Coverage Metrics

| Metric | Formula | Target |
|--------|---------|--------|
| Statement Coverage | statements_executed / total_statements | >95% |
| Branch Coverage | branches_taken / total_branches | >90% |
| Path Coverage | paths_explored / total_paths | >80% (bounded) |
| Condition Coverage | conditions_tested / total_conditions | >90% |

---

## 3. Differential Testing (Golden Master)

### 3.1 Approach

```
1. Capture production inputs from COBOL system
2. Run inputs through both COBOL and Java
3. Compare outputs byte-by-byte
4. Log any differences
```

### 3.2 Test Case Structure

```yaml
test_id: DIFF-001
description: Premium tier customer with balance $15,000
input:
  CUSTOMER-ID: "C001"
  ACCOUNT-BALANCE: 15000
  CUSTOMER-AGE: 45
  ACCOUNT-STATUS: "ACTIVE"

expected_output:
  source: COBOL (production)
  TIER: "PREMIUM"
  INTEREST-RATE: 0.045
  MONTHLY-FEE: 0
  checksum: "a1b2c3d4e5"

actual_output:
  source: Java (migration)
  TIER: "PREMIUM"
  INTEREST-RATE: 0.045
  MONTHLY-FEE: 0
  checksum: "a1b2c3d4e5"

result: ✅ PASS
execution_time:
  cobol: 12ms
  java: 8ms
```

### 3.3 Handling Non-Determinism

```rust
// Problem: Timestamps, random numbers, etc.

pub struct ComparisonConfig {
    // Fields to ignore in comparison
    ignore_fields: vec!["TIMESTAMP", "TRANSACTION-ID"],
    
    // Fields with tolerance (floating point)
    tolerance: HashMap<String, f64>,  // {"INTEREST": 0.001}
    
    // Normalize before compare
    normalizers: HashMap<String, fn(&Value) -> Value>,
}

pub fn compare_outputs(
    cobol: &Output, 
    java: &Output,
    config: &ComparisonConfig
) -> ComparisonResult {
    let cobol_normalized = normalize(cobol, config);
    let java_normalized = normalize(java, config);
    
    cobol_normalized == java_normalized
}
```

---

## 4. Property-Based Testing

### 4.1 Extract Invariants from COBOL

```rust
// Invariants discovered via symbolic execution

pub enum Invariant {
    // Pre-condition: must be true before execution
    PreCondition(Expr),
    
    // Post-condition: must be true after execution
    PostCondition(Expr),
    
    // Loop invariant: true before/after each iteration
    LoopInvariant {
        loop_id: String,
        condition: Expr,
    },
    
    // Type invariant: always true for a data type
    TypeInvariant {
        type_name: String,
        condition: Expr,
    },
}

// Example invariants from COBOL:
let invariants = vec![
    // ACCOUNT-BALANCE is never negative
    Invariant::PostCondition(parse("ACCOUNT-BALANCE >= 0")),
    
    // TIER is always one of valid values
    Invariant::PostCondition(parse(
        "TIER IN {'PREMIUM', 'STANDARD', 'BASIC'}"
    )),
    
    // INTEREST-RATE is between 0 and 1
    Invariant::TypeInvariant {
        type_name: "INTEREST-RATE",
        condition: parse("0 <= SELF <= 1"),
    },
];
```

### 4.2 Property Testing with QuickCheck

```rust
// Rust example using proptest
use proptest::prelude::*;

proptest! {
    #[test]
    fn tier_calculation_preserves_invariants(
        balance in 0..1_000_000i64,
        age in 18..120u8,
    ) {
        let cobol_output = run_cobol(balance, age);
        let java_output = run_java(balance, age);
        
        // Invariant: TIER is always valid
        assert!(is_valid_tier(&cobol_output.tier));
        assert!(is_valid_tier(&java_output.tier));
        
        // Invariant: outputs match
        assert_eq!(cobol_output.tier, java_output.tier);
        
        // Invariant: interest rate is valid
        assert!(0.0 <= cobol_output.interest_rate <= 1.0);
        assert!(0.0 <= java_output.interest_rate <= 1.0);
    }
}
```

---

## 5. Shadow Mode / Parallel Run

### 5.1 Architecture

```
┌─────────────────┐
│  Production     │
│  Traffic        │
└────────┬────────┘
         │
    ┌────┴────
    │         │
    ▼         ▼
┌─────────┐ ┌─────────
│  COBOL  │ │  Java   │
│  (Prod) │ │ (Shadow)│
└────┬────┘ └────┬────┘
     │           │
     ▼           ▼
┌─────────────────────────┐
│   Output Comparator     │
│   - Log differences     │
│   - Alert on mismatch   │
│   - Metrics dashboard   │
└─────────────────────────┘
```

### 5.2 Comparison Logic

```java
public class ShadowModeComparator {
    
    public void compare(String transactionId, 
                       CobolResponse cobol, 
                       JavaResponse java) {
        ComparisonResult result = new ComparisonResult();
        result.transactionId = transactionId;
        result.timestamp = Instant.now();
        
        // Compare business fields (ignore metadata)
        if (!equals(cobol.getTier(), java.getTier())) {
            result.addDifference("TIER", cobol.getTier(), java.getTier());
        }
        
        if (!equals(cobol.getInterestRate(), java.getInterestRate(), 0.001)) {
            result.addDifference("INTEREST_RATE", 
                cobol.getInterestRate(), 
                java.getInterestRate());
        }
        
        // Log and alert
        if (result.hasDifferences()) {
            logger.error("MISMATCH: {}", result);
            alertService.send(result);
            metrics.counter("migration.mismatch").increment();
        } else {
            metrics.counter("migration.match").increment();
        }
        
        // Store for analysis
        repository.save(result);
    }
}
```

### 5.3 Exit Criteria

| Metric | Threshold | Duration |
|--------|-----------|----------|
| Output Match Rate | >99.99% | 2 weeks |
| Performance (Java vs COBOL) | <2x slower | 1 week |
| Error Rate | <0.01% | 2 weeks |
| Memory Usage | <2x COBOL | 1 week |

---

## 6. Combined Strategy (Recommended)

### Phase 1: Pre-Migration (Week 1-2)

```
□ Extract invariants from COBOL via symbolic execution
□ Generate property tests from invariants
□ Set up differential testing framework
```

### Phase 2: During Migration (Week 3-8)

```
□ Run Z3 equivalence on each migrated function
□ Generate concolic tests for complex paths
□ Build golden master test suite from production data
```

### Phase 3: Pre-Production (Week 9-12)

```
□ Run full test suite (10,000+ test cases)
□ Achieve >95% branch coverage
□ Zero Z3 counterexamples
```

### Phase 4: Production (Week 13+)

```
□ Deploy in shadow mode
□ Monitor for 2-4 weeks
□ Gradual traffic shift (1% → 10% → 50% → 100%)
□ Decommission COBOL after 99.99% match for 4 weeks
```

---

## 7. Tooling Recommendations

| Tool | Purpose | Integration |
|------|---------|-------------|
| **Z3** | Formal verification | Built into symbolic engine |
| **proptest** (Rust) | Property-based testing | Test generation |
| **JUnit + ArchUnit** (Java) | Differential testing | Java test suite |
| **Diffy** (Twitter OSS) | Output comparison | Shadow mode |
| **Grafana + Prometheus** | Metrics dashboard | Monitoring |

---

## 8. Equivalence Proof Certificate

```yaml
certificate:
  id: CERT-2026-001
  program: CARDDEMO.CBL
  migrated_to: CardDemoService.java
  generated_at: 2026-03-08T12:00:00Z
  
  verification_methods:
    - method: Z3_Formal_Proof
      status: PASSED
      paths_verified: 147
      counterexamples: 0
      
    - method: Concolic_Testing
      status: PASSED
      test_cases: 1247
      branch_coverage: 96.3%
      path_coverage: 82.1%
      
    - method: Differential_Testing
      status: PASSED
      golden_tests: 5000
      mismatches: 0
      
    - method: Property_Based_Testing
      status: PASSED
      invariants_checked: 23
      property_tests: 10000
      
    - method: Shadow_Mode
      status: IN_PROGRESS
      production_transactions: 150000
      match_rate: 99.997%
      
  overall_status: EQUIVALENT
  confidence_level: 99.99%
  
  signed_by:
    - SymbolicEngine v0.1.0
    - MigrationAgent v0.1.0
```

---

## Summary

| Strategy | Confidence | Speed | Cost | When to Use |
|----------|------------|-------|------|-------------|
| **Z3 Formal** | 100% | Slow | High | Critical paths, regulations |
| **Concolic** | 95% | Medium | Medium | Complex business logic |
| **Differential** | 90% | Fast | Low | Regression testing |
| **Property-Based** | 85% | Fast | Low | Invariant validation |
| **Shadow Mode** | 99% | Real-time | Medium | Production validation |

**Best Practice:** Use ALL strategies in combination. Each catches different classes of bugs.

---

**Next Steps:**
1. Implement Z3 equivalence checker in `open-mainframe-symbolic`
2. Build differential testing framework
3. Create test case generator from symbolic paths
4. Set up shadow mode infrastructure
