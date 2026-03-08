# The Role of Test Cases in Formal Verification

**Question:** Nếu đã có Z3 prove equivalence, test cases còn ý nghĩa không?

**Answer:** CÓ, nhưng vai trò khác nhau. Formal verification ≠ thay thế testing.

---

## 1. What Formal Verification CANNOT Do

### 1.1 Limitations of Z3/SMT Solvers

```
❌ Cannot verify:
   - External systems (databases, APIs, file systems)
   - Non-deterministic behavior (timing, race conditions)
   - Performance characteristics
   - User experience
   - Business rule CORRECTNESS (only consistency)
   
❌ Cannot scale to:
   - Entire system at once (path explosion)
   - Unbounded loops/recursion
   - Complex string operations
   - I/O operations
```

### 1.2 Example: What Z3 Can't Catch

```cobol
* COBOL Program
IF ACCOUNT-BALANCE > 10000
    MOVE 'PREMIUM' TO TIER
ELSE
    MOVE 'STANDARD' TO TIER
END-IF.
```

```java
// Java Program  
if (accountBalance > 10000) {
    tier = "PREMIUM";
} else {
    tier = "STANDARD";
}
```

```
Z3 Proof: ✅ EQUIVALENT

BUT: What if business rule is WRONG?
- What if threshold should be 15000, not 10000?
- What if there should be 3 tiers, not 2?
- What if 'BASIC' tier is missing?

Z3 CANNOT catch this. It only proves CODE equivalence,
not BUSINESS CORRECTNESS.
```

---

## 2. Test Cases Still Matter - Here's Why

### 2.1 Testing Pyramid with Formal Verification

```
                    ┌─────────────────┐
                    │  Business Tests │  ← Test cases STILL needed
                    │  (Does it solve │
                    │   the right     │
                    │   problem?)     │
                   ─┴─────────────────┴─
                  ╱                   ╲
                 ╱  Integration Tests  ╲  ← Test cases STILL needed
                ╱   (Do components     ╲
               ╱    work together?)     ╲
              ───────────────────────────
             ╱                           ╲
            ╱    Contract Tests           ╲  ← Test cases STILL needed
           ╱     (Do APIs match spec?)     ╲
          ───────────────────────────────────
         ╱                                   ╲
        ╱      Property-Based Tests           ╲  ← Can be generated from Z3
       ╱       (Do invariants hold?)           ╲
      ───────────────────────────────────────────
     ╱                                           ╲
    ╱          Formal Verification                ╲  ← Z3 proofs
   ╱           (Is code equivalent?)              ╲
  ──────────────────────────────────────────────────
```

### 2.2 What Each Layer Catches

| Layer | Catches | Formal Verification Can't |
|-------|---------|---------------------------|
| **Business Tests** | Wrong requirements, missing features | ✅ Business correctness |
| **Integration Tests** | Component incompatibility, protocol mismatches | ✅ System integration |
| **Contract Tests** | API drift, breaking changes | ✅ Interface evolution |
| **Property Tests** | Invariant violations | ⚠️ Partial (bounded) |
| **Formal Verification** | Code equivalence | N/A (this is its job) |

---

## 3. Test Cases by Purpose

### 3.1 Type 1: Requirements Validation Tests

```yaml
# These CANNOT be replaced by formal verification

test_suite: "Business Requirements Validation"

tests:
  - id: REQ-001
    name: "Premium tier threshold is correct"
    description: |
      Business requirement: Premium tier at $10,000
      This test validates the REQUIREMENT, not the code
      
    input:
      balance: 10000
    expected:
      tier: "STANDARD"  # Boundary: exactly 10000 = STANDARD
      
    input:
      balance: 10001
    expected:
      tier: "PREMIUM"   # Above 10000 = PREMIUM
      
    purpose: "Validate business rule is CORRECT, not just equivalent"
    
  - id: REQ-002
    name: "All required tiers exist"
    description: |
      Business requirement: 3 tiers (BASIC, STANDARD, PREMIUM)
      
    test_all_tiers_exist:
      - BASIC: balance < 1000
      - STANDARD: 1000 <= balance <= 10000
      - PREMIUM: balance > 10000
      
    purpose: "Catch missing business rules"
```

**Why Z3 Can't Do This:**
- Z3 proves code matches spec
- Z3 CANNOT prove spec matches business reality

---

### 3.2 Type 2: Integration Tests

```yaml
test_suite: "System Integration Tests"

tests:
  - id: INT-001
    name: "API Gateway → Account Service → Database"
    description: |
      Test entire request flow across microservices
      
    flow:
      - step: 1
        action: POST /api/accounts
        body: { customer_id: "C001", type: "CHECKING" }
        
      - step: 2
        verify: HTTP 201 Created
        response_has: { account_id: "A001" }
        
      - step: 3
        verify: Database has new account
        query: SELECT * FROM accounts WHERE account_id = 'A001'
        
      - step: 4
        verify: Kafka event published
        topic: "account.created"
        event_has: { account_id: "A001" }
        
    purpose: "Catch integration issues between components"
```

**Why Z3 Can't Do This:**
- Z3 verifies individual programs
- Z3 CANNOT verify distributed system behavior
- Network failures, timeouts, eventual consistency

---

### 3.3 Type 3: Contract Tests

```yaml
test_suite: "API Contract Tests"

tests:
  - id: CONTRACT-001
    name: "Account API matches OpenAPI spec"
    
    endpoint: POST /api/accounts
    spec_version: "1.2.0"
    
    request_contract:
      required_fields:
        - customer_id
        - account_type
      field_types:
        customer_id: string
        account_type: enum[CHECKING, SAVINGS]
        
    response_contract:
      status_codes: [201, 400, 409]
      response_schema:
        account_id: string
        status: string
        
    purpose: "Catch API drift between versions"
```

**Why Z3 Can't Do This:**
- Contracts evolve independently
- Z3 proves internal logic, not external interfaces

---

### 3.4 Type 4: Regression Tests (Golden Master)

```yaml
test_suite: "Production Regression Tests"

tests:
  - id: REG-001
    name: "Real transaction from production"
    source: "Production capture 2026-03-01 14:23:45"
    
    input:
      transaction_type: "TRANSFER"
      from_account: "A123456"
      to_account: "A789012"
      amount: 2500.00
      
    expected_output:
      status: "SUCCESS"
      from_balance: 7500.00
      to_balance: 4500.00
      transaction_id: "TX987654321"
      
    purpose: "Ensure migration doesn't break existing behavior"
```

**Why Z3 Can't Do This:**
- Production data is too complex for symbolic execution
- Z3 handles bounded inputs, not real-world complexity

---

### 3.5 Type 5: Edge Case Tests

```yaml
test_suite: "Edge Case Tests"

tests:
  - id: EDGE-001
    name: "Maximum balance value"
    input:
      balance: 999999999999.99  # Max allowed
    expected:
      tier: "PREMIUM"
      no_overflow: true
      
  - id: EDGE-002
    name: "Negative balance (overdraft)"
    input:
      balance: -500.00
    expected:
      tier: "BASIC"
      overdraft_fee_applied: true
      
  - id: EDGE-003
    name: "Null/empty customer ID"
    input:
      customer_id: null
    expected:
      error: "CUSTOMER_ID_REQUIRED"
      
  - id: EDGE-004
    name: "Concurrent transfers on same account"
    input:
      parallel_transfers:
        - { to: "A002", amount: 500 }
        - { to: "A003", amount: 300 }
        - { to: "A004", amount: 200 }
    expected:
      all_processed: true
      no_race_condition: true
      final_balance: 0  # Started with 1000
      
    purpose: "Catch edge cases Z3 might miss due to bounds"
```

---

## 4. How Test Cases and Z3 Complement Each Other

### 4.1 Verification Matrix

```
┌─────────────────────────────────────────────────────────────────┐
│                    VERIFICATION COVERAGE                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Verification Method         │ What It Catches                  │
│  ────────────────────────────┼────────────────────────────────  │
│  Z3 Formal Proof             │ Code equivalence (all paths)     │
│  Requirements Tests          │ Wrong business rules             │
│  Integration Tests           │ Component incompatibility        │
│  Contract Tests              │ API drift                        │
│  Regression Tests            │ Unintended changes               │
│  Edge Case Tests             │ Boundary conditions              │
│  Performance Tests           │ SLA violations                   │
│  Security Tests              │ Vulnerabilities                  │
│                                                                  │
│  NONE of these can replace the others!                          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### 4.2 Example: Complete Verification for One Feature

```yaml
feature: "Customer Tier Calculation"

verification_plan:

  # 1. Formal Verification (Z3)
  - method: Z3_Equivalence_Proof
    scope: "COBOL program ≡ Java service"
    coverage: "All 147 symbolic paths"
    catches: "Implementation bugs"
    
  # 2. Requirements Tests
  - method: Business_Requirements_Tests
    scope: "Tier thresholds correct"
    test_cases: 6
    catches: "Wrong business rules"
    
  # 3. Integration Tests
  - method: Integration_Tests
    scope: "API → Service → DB → Events"
    test_cases: 12
    catches: "Integration issues"
    
  # 4. Contract Tests
  - method: API_Contract_Tests
    scope: "OpenAPI spec compliance"
    test_cases: 24
    catches: "API breaking changes"
    
  # 5. Regression Tests
  - method: Golden_Master_Tests
    scope: "Production transactions"
    test_cases: 5000+
    catches: "Unintended behavior changes"
    
  # 6. Edge Case Tests
  - method: Edge_Case_Tests
    scope: "Boundaries, nulls, concurrency"
    test_cases: 47
    catches: "Edge case bugs"
    
  # 7. Performance Tests
  - method: Load_Tests
    scope: "1000 req/s, p99 < 100ms"
    catches: "Performance regressions"
    
  # 8. Security Tests
  - method: Security_Tests
    scope: "Auth, authorization, injection"
    catches: "Security vulnerabilities"
    
overall_confidence: 99.99%
```

---

## 5. Test Case Generation from Symbolic Execution

### 5.1 Z3 Can HELP Generate Test Cases

```rust
// crates/open-mainframe-verification/src/test_generation.rs

pub struct TestCaseGenerator {
    symbolic_engine: SymbolicEngine,
    z3_solver: Z3Solver,
}

impl TestCaseGenerator {
    /// Generate test cases from symbolic paths
    pub fn generate_from_paths(
        &self,
        program: &Program,
    ) -> Vec<TestCase> {
        let paths = self.symbolic_engine.explore(program)?;
        let mut tests = Vec::new();
        
        for path in paths {
            // Convert symbolic path to concrete test case
            let concrete_input = path.to_concrete_input();
            let expected_output = path.output();
            
            tests.push(TestCase {
                id: format!("PATH-{:04}", tests.len()),
                description: format!("Path: {:?}", path.conditions),
                input: concrete_input,
                expected: expected_output,
                source: "Symbolic Execution",
            });
        }
        
        tests
    }
    
    /// Generate edge case tests from constraints
    pub fn generate_edge_cases(
        &self,
        program: &Program,
    ) -> Vec<TestCase> {
        let constraints = self.extract_constraints(program)?;
        let mut tests = Vec::new();
        
        for constraint in constraints {
            // Test boundary values
            tests.push(self.test_at_boundary(&constraint, Boundary::Below)?);
            tests.push(self.test_at_boundary(&constraint, Boundary::At)?);
            tests.push(self.test_at_boundary(&constraint, Boundary::Above)?);
            
            // Test null/empty
            tests.push(self.test_null_value(&constraint)?);
            
            // Test max values
            tests.push(self.test_max_value(&constraint)?);
        }
        
        tests
    }
    
    /// Generate tests to cover uncovered paths
    pub fn generate_for_coverage(
        &self,
        program: &Program,
        existing_tests: &[TestCase],
        target_coverage: f64,
    ) -> Vec<TestCase> {
        let current_coverage = self.measure_coverage(existing_tests)?;
        
        if current_coverage >= target_coverage {
            return Vec::new();
        }
        
        // Use Z3 to find inputs that reach uncovered paths
        let uncovered_paths = self.find_uncovered_paths(
            program,
            existing_tests
        )?;
        
        let mut new_tests = Vec::new();
        for path in uncovered_paths {
            let input = self.z3_solver.find_input_for_path(&path)?;
            new_tests.push(TestCase {
                id: format!("COV-{:04}", new_tests.len()),
                description: format!("Cover path: {:?}", path),
                input,
                expected: path.output(),
                source: "Coverage-Guided",
            });
        }
        
        new_tests
    }
}
```

### 5.2 Example: Auto-Generated Tests

```yaml
# Generated from symbolic execution of CARDDEMO.CBL

auto_generated_tests:
  source: "Symbolic Execution Engine"
  program: "CARDDEMO.CBL"
  paths_explored: 147
  tests_generated: 147
  
  sample_tests:
    - id: PATH-0001
      description: "Balance > 10000, Age >= 18"
      input:
        balance: 15000
        age: 25
      expected:
        tier: "PREMIUM"
        approved: true
      path_conditions:
        - "BALANCE > 10000"
        - "AGE >= 18"
        
    - id: PATH-0002
      description: "Balance <= 10000, Age >= 18"
      input:
        balance: 5000
        age: 30
      expected:
        tier: "STANDARD"
        approved: true
      path_conditions:
        - "BALANCE <= 10000"
        - "AGE >= 18"
        
    - id: PATH-0003
      description: "Balance > 10000, Age < 18"
      input:
        balance: 20000
        age: 17
      expected:
        tier: "PREMIUM"
        approved: false
        reason: "UNDERAGE"
      path_conditions:
        - "BALANCE > 10000"
        - "AGE < 18"
        
  coverage:
    statement_coverage: 100%
    branch_coverage: 100%
    path_coverage: 100% (bounded)
```

**Benefit:** Z3 generates test cases that achieve 100% path coverage automatically.

---

## 6. Test Case Categories Summary

| Category | Purpose | Can Z3 Replace? | Still Needed? |
|----------|---------|-----------------|---------------|
| **Requirements Validation** | Verify business rules correct | ❌ No | ✅ YES |
| **Integration** | Verify components work together | ❌ No | ✅ YES |
| **Contract** | Verify API compatibility | ❌ No | ✅ YES |
| **Regression** | Verify no unintended changes | ⚠️ Partial | ✅ YES |
| **Edge Cases** | Verify boundaries | ⚠️ Partial | ✅ YES |
| **Path Coverage** | Verify all code paths | ✅ YES | ⚠️ Auto-generated |
| **Property-Based** | Verify invariants | ✅ YES | ⚠️ Auto-generated |
| **Performance** | Verify SLA | ❌ No | ✅ YES |
| **Security** | Verify vulnerabilities | ❌ No | ✅ YES |

---

## 7. Recommended Test Strategy

### 7.1 Test Pyramid for Migration

```
                    ┌─────────────────┐
                    │  Business Tests │  ← 50 tests (manual)
                    │  (Requirements) │
                   ─┴─────────────────┴─
                  ╱                   ╲
                 ╱  Integration Tests  ╲  ← 200 tests (manual + auto)
                ╱   (System flows)     ╲
               ───────────────────────────
              ╱                           ╲
             ╱    Contract Tests           ╲  ← 100 tests (auto)
            ╱     (API specs)              ╲
           ───────────────────────────────────
          ╱                                   ╲
               Regression Tests               ╲  ← 5000+ tests (auto from production)
        ╱       (Golden Master)                ╲
       ───────────────────────────────────────────
      ╱                                           ╲
     ╱          Z3-Generated Tests                 ╲  ← 147 tests (auto from symbolic)
    ╱           (Path coverage)                    ╲
   ──────────────────────────────────────────────────
```

### 7.2 Test Maintenance

```yaml
test_maintenance:

  # Manual tests (high value, stable)
  business_tests:
    update_frequency: "When business rules change"
    ownership: "Product/Business team"
    
  # Semi-auto tests (medium value)
  integration_tests:
    update_frequency: "When architecture changes"
    ownership: "Engineering team"
    
  # Auto-generated tests (regenerate often)
  z3_generated_tests:
    update_frequency: "Every code change"
    ownership: "CI/CD pipeline"
    
  regression_tests:
    update_frequency: "Continuous (production capture)"
    ownership: "Monitoring system"
```

---

## 8. Equivalence Certificate (With Test Cases)

```yaml
certificate:
  id: FULL-VERIFY-CERT-001
  system: "CardDemo Banking System"
  
  verification_methods:
    
    formal_verification:
      method: "Z3 Equivalence Proof"
      status: PASSED
      paths_verified: 147
      confidence_contribution: "40%"
      
    business_tests:
      method: "Requirements Validation"
      status: PASSED
      test_cases: 50
      confidence_contribution: "20%"
      
    integration_tests:
      method: "System Integration"
      status: PASSED
      test_cases: 200
      confidence_contribution: "15%"
      
    regression_tests:
      method: "Golden Master (Production)"
      status: PASSED
      test_cases: 5247
      confidence_contribution: "15%"
      
    edge_case_tests:
      method: "Boundary Analysis"
      status: PASSED
      test_cases: 47
      confidence_contribution: "10%"
      
  overall_confidence: 99.99%
  overall_status: EQUIVALENT
  
  notes: |
    Formal verification proves code equivalence.
    Test cases prove business correctness.
    Both are necessary for full confidence.
```

---

## Summary

| Question | Answer |
|----------|--------|
| **Do test cases still matter with Z3?** | YES, absolutely |
| **What does Z3 prove?** | Code equivalence (implementation) |
| **What do test cases prove?** | Business correctness (requirements) |
| **Can Z3 replace any tests?** | Yes - path coverage tests (auto-generated) |
| **Can tests replace Z3?** | No - tests can't cover all paths |
| **Best approach?** | Z3 + Tests together (complementary) |

**Key Insight:** Formal verification and testing are COMPLEMENTARY, not competitive. Z3 proves the code is equivalent. Tests prove the code is CORRECT.

**Moat:** Competitors do only testing (incomplete). You do Z3 + Testing (complete verification).
