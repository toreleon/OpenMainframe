"""
Execution tools — run JCL jobs and interpret COBOL programs.
Wraps: open-mainframe run, open-mainframe interpret
"""

from .base import run_cli, sanitize_path


async def run_jcl(jcl_file: str) -> dict:
    """Execute a JCL job file. Returns step-by-step execution results
    including per-step return codes and SYSOUT content.

    WARNING: This tool executes code. It should only be called after
    human approval via the HITL interrupt flow.

    Args:
        jcl_file: Path to a JCL file (.jcl).
    """
    path = sanitize_path(jcl_file)
    return await run_cli(["run", path], timeout=300)


async def interpret_cobol(source_file: str) -> dict:
    """Run a COBOL program through the tree-walking interpreter.
    Captures DISPLAY output and final return code.

    WARNING: This tool executes code. It should only be called after
    human approval via the HITL interrupt flow.

    Args:
        source_file: Path to a COBOL source file (.cbl, .cob).
    """
    path = sanitize_path(source_file)
    return await run_cli(["interpret", path], timeout=120)
