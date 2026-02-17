"""
Compilation tools — compile and syntax-check COBOL source files.
Wraps: open-mainframe compile, open-mainframe check
"""

from langchain.tools import tool

from .base import run_cli, sanitize_path


@tool
def compile_cobol(source_file: str) -> dict:
    """Compile a COBOL source file to a native executable.
    Returns success/failure with compiler output and error messages.

    Args:
        source_file: Path to a COBOL source file (.cbl, .cob).
    """
    path = sanitize_path(source_file)
    return run_cli(["compile", path], timeout=120)


@tool
def check_cobol(source_file: str) -> dict:
    """Syntax check a COBOL source file without full compilation.
    Faster than compile — useful for quick validation.

    Args:
        source_file: Path to a COBOL source file (.cbl, .cob).
    """
    path = sanitize_path(source_file)
    return run_cli(["check", path], timeout=30)
