"""
Base tool utilities for OpenMainframe CLI commands.
Provides run_cli() (bridge-aware), path sanitization, and IDCAMS security.

In bridge mode (default): commands are sent to the local bridge daemon via WebSocket.
In local mode (fallback): commands run via subprocess (for development/testing).
"""

import json
import os
import re
import subprocess
from typing import Optional

from ..bridge_client import execute_via_bridge, bridge_manager

MAX_OUTPUT_BYTES = 20_000  # 20KB truncation limit

OPEN_MAINFRAME_BIN = os.getenv(
    "OPEN_MAINFRAME_BIN", "./target/release/open-mainframe"
)
WORKSPACE_ROOT = os.getenv("WORKSPACE_ROOT", os.getcwd())


def sanitize_path(path: str) -> str:
    """Prevent directory traversal and null byte injection.

    In bridge mode, this returns the path as-is (the bridge does its own sandboxing).
    In local mode, resolves and validates against WORKSPACE_ROOT.
    """
    path = path.replace("\x00", "")

    if bridge_manager.has_connection:
        # Bridge daemon handles its own path sandboxing
        return path

    # Local mode: resolve and validate
    resolved = os.path.realpath(path)
    allowed_root = os.path.realpath(WORKSPACE_ROOT)
    if not resolved.startswith(allowed_root + os.sep) and resolved != allowed_root:
        raise ValueError(
            f"Path '{path}' resolves outside the allowed workspace '{allowed_root}'"
        )
    return resolved


def sanitize_idcams(command: str) -> str:
    """Restrict IDCAMS to allowed verbs and reject shell metacharacters."""
    ALLOWED_VERBS = {"DEFINE", "DELETE", "REPRO", "LISTCAT", "PRINT"}

    if not command or not command.strip():
        raise ValueError("IDCAMS command cannot be empty")

    verb = command.strip().split()[0].upper()
    if verb not in ALLOWED_VERBS:
        raise ValueError(
            f"IDCAMS verb '{verb}' not allowed. Allowed: {', '.join(sorted(ALLOWED_VERBS))}"
        )

    if re.search(r"[;&|`$\\]", command):
        raise ValueError("Shell metacharacters are not allowed in IDCAMS commands")

    return command


async def run_cli(
    args: list[str],
    timeout: int = 120,
    cwd: Optional[str] = None,
) -> dict:
    """Execute an OpenMainframe CLI command and return structured result.

    Routes to the bridge if connected, otherwise falls back to local subprocess.

    Args:
        args: Command-line arguments (without the binary name).
        timeout: Maximum execution time in seconds.
        cwd: Working directory (only used in local fallback mode).

    Returns:
        dict with keys: success, stdout, stderr, return_code
    """
    if bridge_manager.has_connection:
        return await execute_via_bridge(args, timeout=timeout)

    # Local fallback for development/testing
    return _run_cli_local(args, timeout=timeout, cwd=cwd)


def _run_cli_local(
    args: list[str],
    timeout: int = 120,
    cwd: Optional[str] = None,
) -> dict:
    """Execute via local subprocess (fallback when no bridge is connected)."""
    try:
        result = subprocess.run(
            [OPEN_MAINFRAME_BIN, *args],
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=cwd,
            env={**os.environ, "LANG": "C.UTF-8"},
        )
        return {
            "success": result.returncode == 0,
            "stdout": result.stdout[:MAX_OUTPUT_BYTES],
            "stderr": result.stderr[:MAX_OUTPUT_BYTES],
            "return_code": result.returncode,
        }
    except subprocess.TimeoutExpired:
        return {
            "success": False,
            "stdout": "",
            "stderr": f"Command timed out after {timeout} seconds",
            "return_code": -1,
        }
    except FileNotFoundError:
        return {
            "success": False,
            "stdout": "",
            "stderr": (
                f"OpenMainframe binary not found at '{OPEN_MAINFRAME_BIN}'. "
                "Please build it with 'cargo build --release' or set OPEN_MAINFRAME_BIN."
            ),
            "return_code": -1,
        }
    except Exception as e:
        return {
            "success": False,
            "stdout": "",
            "stderr": f"Unexpected error: {str(e)}",
            "return_code": -1,
        }


def try_parse_json(text: str) -> dict | str:
    """Attempt to parse text as JSON; return raw string on failure."""
    try:
        return json.loads(text)
    except (json.JSONDecodeError, ValueError):
        return text
