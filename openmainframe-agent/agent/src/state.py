"""
Agent state definition.
Keep in sync with frontend types in src/lib/types.ts
"""

from typing import TypedDict, Optional


class SourceFile(TypedDict):
    path: str
    type: str  # "cobol", "jcl", "copybook", "bms", "data"
    size_bytes: int
    line_count: int


class AssessmentReport(TypedDict):
    total_files: int
    total_loc: int
    average_complexity: float
    programs: list[dict]
    issues: list[dict]
    recommendations: list[str]
    feature_support: dict[str, float]


class CompilationResult(TypedDict):
    file_path: str
    success: bool
    output: str
    errors: str
    timestamp: str


class ExecutionResult(TypedDict):
    jcl_file: str
    steps: list[dict]
    max_return_code: int
    output: str
    timestamp: str


class AgentState(TypedDict):
    """Main agent state shared between LangGraph agent and CopilotKit frontend."""

    # Project context
    project_path: Optional[str]
    source_files: list[SourceFile]

    # Assessment
    assessment_results: Optional[AssessmentReport]

    # Compilation
    compilation_results: list[CompilationResult]

    # Execution
    execution_results: list[ExecutionResult]

    # Operation tracking
    current_operation: Optional[str]
    operation_progress: float  # 0.0 - 1.0
