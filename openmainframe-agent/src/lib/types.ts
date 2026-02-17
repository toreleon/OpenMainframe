/**
 * Shared TypeScript types matching the Python AgentState.
 * Keep in sync with agent/src/state.py
 */

export interface SourceFile {
  path: string;
  type: "cobol" | "jcl" | "copybook" | "bms" | "data";
  size_bytes: number;
  line_count: number;
}

export interface ProgramMetrics {
  program_id: string;
  file_path: string;
  loc: number;
  complexity: number;
  maintainability: number;
  technical_debt_hours: number;
  features_used: string[];
}

export interface CompatibilityIssue {
  file_path: string;
  line: number | null;
  severity: "info" | "warning" | "high" | "critical";
  rule: string;
  message: string;
  recommendation: string;
}

export interface AssessmentReport {
  total_files: number;
  total_loc: number;
  average_complexity: number;
  programs: ProgramMetrics[];
  issues: CompatibilityIssue[];
  recommendations: string[];
  feature_support: Record<string, number>;
}

export interface CompilerError {
  line: number;
  column: number;
  message: string;
  severity: string;
}

export interface CompilationResult {
  file_path: string;
  success: boolean;
  errors: CompilerError[];
  warnings: CompilerError[];
  timestamp: string;
}

export interface StepResult {
  step_name: string;
  program: string;
  return_code: number;
  output: string;
  duration_ms: number;
}

export interface ExecutionResult {
  jcl_file: string;
  steps: StepResult[];
  max_return_code: number;
  timestamp: string;
}

export interface AgentState {
  project_path: string | null;
  source_files: SourceFile[];
  assessment_results: AssessmentReport | null;
  compilation_results: CompilationResult[];
  execution_results: ExecutionResult[];
  current_operation: string | null;
  operation_progress: number;
}
