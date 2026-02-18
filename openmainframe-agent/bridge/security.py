"""
Security utilities for the local bridge daemon.

Ensures all operations are sandboxed within the project directory
and only allowlisted CLI subcommands can be executed.
"""

from __future__ import annotations

import os
import re
from pathlib import Path

# Allowlisted open-mainframe CLI subcommands
ALLOWED_SUBCOMMANDS = frozenset({
    "assess",
    "compile",
    "check",
    "run",
    "interpret",
    "parse-jcl",
    "lex",
    "idcams",
})

# Characters that could indicate shell injection
SHELL_METACHARACTERS = re.compile(r"[;&|`$(){}<>\n\r]")


def validate_project_path(project_path: str) -> Path:
    """Validate and resolve the project path.

    Returns the resolved absolute path.
    Raises ValueError if the path doesn't exist or isn't a directory.
    """
    path = Path(project_path).expanduser().resolve()
    if not path.exists():
        raise ValueError(f"Project path does not exist: {path}")
    if not path.is_dir():
        raise ValueError(f"Project path is not a directory: {path}")
    return path


def sandbox_path(file_path: str, project_root: Path) -> Path:
    """Ensure a file path resolves within the project root.

    Returns the resolved absolute path.
    Raises ValueError if the path escapes the sandbox.
    """
    resolved = (project_root / file_path).resolve()
    try:
        resolved.relative_to(project_root)
    except ValueError:
        raise ValueError(
            f"Path '{file_path}' resolves outside the project directory. "
            f"All operations must stay within: {project_root}"
        )
    return resolved


def validate_command(command: str, project_root: Path) -> list[str]:
    """Validate and parse a CLI command string.

    Ensures:
    - The subcommand is in the allowlist
    - No shell metacharacters
    - All file path arguments resolve within the project root

    Returns the command as a list of arguments.
    Raises ValueError on any violation.
    """
    if SHELL_METACHARACTERS.search(command):
        raise ValueError(
            f"Command contains disallowed characters: {command}"
        )

    parts = command.split()
    if not parts:
        raise ValueError("Empty command")

    # The command may or may not start with the binary name
    # Normalize: if first part looks like a path or binary name, skip it
    subcommand_idx = 0
    if parts[0].endswith("open-mainframe") or "/" in parts[0]:
        subcommand_idx = 1

    if subcommand_idx >= len(parts):
        raise ValueError("Command missing subcommand")

    subcommand = parts[subcommand_idx].lower()

    # Handle two-word subcommands: "assess scan", "assess file"
    if subcommand == "assess" and subcommand_idx + 1 < len(parts):
        sub_sub = parts[subcommand_idx + 1].lower()
        if sub_sub in ("scan", "file"):
            subcommand = "assess"  # still "assess"

    if subcommand not in ALLOWED_SUBCOMMANDS:
        raise ValueError(
            f"Subcommand '{subcommand}' is not allowed. "
            f"Allowed: {', '.join(sorted(ALLOWED_SUBCOMMANDS))}"
        )

    # Validate any file path arguments (non-flag arguments after the subcommand)
    for arg in parts[subcommand_idx + 1:]:
        if arg.startswith("-"):
            continue  # skip flags
        # If it looks like a path (contains / or . or starts with ~), validate it
        if "/" in arg or arg.startswith(".") or arg.startswith("~"):
            sandbox_path(arg, project_root)

    return parts


def validate_token(provided: str, expected: str) -> bool:
    """Constant-time token comparison to prevent timing attacks."""
    import hmac
    return hmac.compare_digest(provided, expected)
