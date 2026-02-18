"""
Dataset management tools — browse catalogs and manage datasets via IDCAMS.
Wraps: open-mainframe idcams
"""

from langchain.tools import tool

from .base import run_cli, sanitize_idcams


@tool
async def list_catalog(pattern: str = "*") -> dict:
    """List datasets in the catalog matching the given pattern.
    Returns dataset names, types, record formats, and record lengths.

    Args:
        pattern: Catalog entry pattern to match (default: "*" for all).
    """
    return await run_cli(["idcams", f"LISTCAT ENTRIES({pattern})"], timeout=60)


@tool
async def idcams_command(command: str) -> dict:
    """Execute an IDCAMS command for dataset management.
    Supported verbs: DEFINE, DELETE, REPRO, LISTCAT, PRINT.

    WARNING: DELETE operations should only be called after human approval.

    Args:
        command: The IDCAMS command string (e.g., "DEFINE CLUSTER(NAME(MY.DS))").
    """
    try:
        sanitized = sanitize_idcams(command)
    except ValueError as e:
        return {
            "success": False,
            "stdout": "",
            "stderr": str(e),
            "return_code": -1,
        }
    return await run_cli(["idcams", sanitized], timeout=60)
