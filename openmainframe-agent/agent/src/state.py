"""
Agent state definition.
Plain dataclass — no LangGraph/CopilotKit dependency.
Keep field names in sync with frontend types in src/lib/types.ts.
"""

from dataclasses import dataclass, field


@dataclass
class AgentState:
    """Agent state emitted as STATE_SNAPSHOT events for frontend consumption."""

    # Project context
    project_path: str | None = None
    source_files: list = field(default_factory=list)

    # Assessment
    assessment_results: dict | None = None

    # Compilation
    compilation_results: list = field(default_factory=list)

    # Execution
    execution_results: list = field(default_factory=list)

    # Operation tracking
    current_operation: str | None = None
    operation_progress: float = 0.0

    def to_dict(self) -> dict:
        return {
            "project_path": self.project_path,
            "source_files": self.source_files,
            "assessment_results": self.assessment_results,
            "compilation_results": self.compilation_results,
            "execution_results": self.execution_results,
            "current_operation": self.current_operation,
            "operation_progress": self.operation_progress,
        }
