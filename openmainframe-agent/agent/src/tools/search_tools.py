"""
Code search tools: grep (content search) and glob (file pattern matching).
Routes through bridge when connected, falls back to local execution.
"""

import asyncio
import json
import pathlib

from .base import sanitize_path, MAX_OUTPUT_BYTES, is_bridge_connected
from src.bridge_client import execute_via_bridge


async def grep(pattern: str, path: str = ".", include: str = "") -> dict:
    """Search file contents by regex. Returns matching lines with file paths."""

    if is_bridge_connected():
        # Build the same grep command and route through bridge
        cmd_parts = ["grep", "-rn", "--color=never"]
        if include:
            cmd_parts.extend(["--include", include])
        cmd_parts.extend(["--", repr(pattern)[1:-1], path])
        command = " ".join(cmd_parts)

        result = await execute_via_bridge([command])
        stdout = result.get("stdout", "")[:MAX_OUTPUT_BYTES]

        matches = []
        for line in stdout.splitlines():
            parts = line.split(":", 2)
            if len(parts) >= 3:
                matches.append({
                    "file": parts[0],
                    "line": int(parts[1]) if parts[1].isdigit() else 0,
                    "content": parts[2],
                })

        return {
            "matches": matches,
            "total_matches": len(matches),
        }

    # Fallback: local execution
    resolved = sanitize_path(path)

    cmd_parts = ["grep", "-rn", "--color=never"]
    if include:
        cmd_parts.extend(["--include", include])
    cmd_parts.extend(["--", pattern, resolved])

    try:
        proc = await asyncio.create_subprocess_exec(
            *cmd_parts,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        stdout_bytes, stderr_bytes = await asyncio.wait_for(
            proc.communicate(), timeout=30
        )

        stdout = stdout_bytes.decode("utf-8", errors="replace")[:MAX_OUTPUT_BYTES]

        matches = []
        for line in stdout.splitlines():
            parts = line.split(":", 2)
            if len(parts) >= 3:
                matches.append({
                    "file": parts[0],
                    "line": int(parts[1]) if parts[1].isdigit() else 0,
                    "content": parts[2],
                })

        return {
            "matches": matches,
            "total_matches": len(matches),
        }
    except asyncio.TimeoutError:
        return {"error": "Search timed out after 30 seconds"}
    except Exception as e:
        return {"error": str(e)}


async def glob(pattern: str, path: str = ".") -> dict:
    """Find files by glob pattern. Returns matching file paths."""

    if is_bridge_connected():
        cmd = (
            f"python3 -c \""
            f"import pathlib,json;"
            f"base=pathlib.Path('{path}');"
            f"files=sorted(str(p) for p in base.glob('{pattern}') if p.is_file());"
            f"print(json.dumps({{'files':files,'total':len(files)}}))"
            f"\""
        )
        result = await execute_via_bridge([cmd])
        if result.get("return_code", -1) == 0:
            try:
                return json.loads(result.get("stdout", "{}"))
            except json.JSONDecodeError:
                return {"error": "Failed to parse glob results"}
        return {"error": result.get("stderr", "Failed to glob via bridge")}

    # Fallback: local execution
    resolved = sanitize_path(path)

    try:
        base = pathlib.Path(resolved)
        files = sorted(str(p) for p in base.glob(pattern) if p.is_file())

        return {
            "files": files,
            "total": len(files),
        }
    except Exception as e:
        return {"error": str(e)}
