"""
Assessment tools — scan directories and analyze individual COBOL files.
Wraps: open-mainframe assess scan, open-mainframe assess file
"""

from langchain.tools import tool

from .base import run_cli, sanitize_path, try_parse_json


@tool
async def assess_scan(directory: str) -> dict:
    """Scan a directory of COBOL source files and return assessment metrics
    including complexity scores, feature inventory, and compatibility issues.

    Args:
        directory: Path to directory containing COBOL source files.
    """
    path = sanitize_path(directory)
    result = await run_cli(
        ["assess", "scan", path, "--format", "json"],
        timeout=300,
    )
    # Try to parse stdout as JSON for structured data
    if result["success"] and result["stdout"]:
        result["parsed"] = try_parse_json(result["stdout"])
    return result


@tool
async def assess_file(file_path: str) -> dict:
    """Assess a single COBOL source file for metrics and compatibility.
    Returns complexity, maintainability, feature usage, and issues.

    Args:
        file_path: Path to a COBOL source file (.cbl, .cob).
    """
    path = sanitize_path(file_path)
    result = await run_cli(
        ["assess", "file", path, "--format", "json"],
        timeout=120,
    )
    if result["success"] and result["stdout"]:
        result["parsed"] = try_parse_json(result["stdout"])
    return result
