"""
File operation tools: read, write, edit, list directory.
Routes through bridge when connected, falls back to local filesystem.
"""

import base64
import json
import os

from .base import sanitize_path, is_bridge_connected
from src.bridge_client import execute_via_bridge, read_file_via_bridge


async def read_file(file_path: str, offset: int = 0, limit: int = 2000) -> dict:
    """Read a file with optional line range. Returns numbered lines."""

    if is_bridge_connected():
        # Use bridge read_file for the raw content
        result = await read_file_via_bridge(file_path)
        if result.get("error"):
            return {"error": result["error"]}

        content = result.get("content", "")
        all_lines = content.split("\n")
        total_lines = len(all_lines)
        selected = all_lines[offset : offset + limit]

        # Number lines (1-based)
        numbered = ""
        for i, line in enumerate(selected, start=offset + 1):
            numbered += f"{i:>6}\t{line}\n"

        truncated = total_lines > offset + limit
        return {
            "content": numbered,
            "total_lines": total_lines,
            "truncated": truncated,
        }

    # Fallback: local filesystem
    resolved = sanitize_path(file_path)

    if not os.path.isfile(resolved):
        return {"error": f"File not found: {file_path}"}

    try:
        with open(resolved, "r", encoding="utf-8", errors="replace") as f:
            all_lines = f.readlines()

        total_lines = len(all_lines)
        selected = all_lines[offset : offset + limit]

        # Number lines (1-based)
        numbered = ""
        for i, line in enumerate(selected, start=offset + 1):
            numbered += f"{i:>6}\t{line}"

        truncated = total_lines > offset + limit

        return {
            "content": numbered,
            "total_lines": total_lines,
            "truncated": truncated,
        }
    except Exception as e:
        return {"error": str(e)}


async def write_file(file_path: str, content: str) -> dict:
    """Create or overwrite a file."""

    if is_bridge_connected():
        # Encode content as base64 to safely pass through shell
        b64 = base64.b64encode(content.encode("utf-8")).decode("ascii")
        cmd = (
            f"python3 -c \""
            f"import base64,os,pathlib;"
            f"p=pathlib.Path('{file_path}');"
            f"p.parent.mkdir(parents=True,exist_ok=True);"
            f"p.write_text(base64.b64decode('{b64}').decode('utf-8'));"
            f"print(len(base64.b64decode('{b64}')))"
            f"\""
        )
        result = await execute_via_bridge([cmd])
        if result.get("return_code", -1) == 0:
            stdout = result.get("stdout", "").strip()
            try:
                bytes_written = int(stdout)
            except ValueError:
                bytes_written = len(content.encode("utf-8"))
            return {"success": True, "bytes_written": bytes_written}
        return {"error": result.get("stderr", "Failed to write file via bridge")}

    # Fallback: local filesystem
    resolved = sanitize_path(file_path)

    try:
        os.makedirs(os.path.dirname(resolved), exist_ok=True)
        with open(resolved, "w", encoding="utf-8") as f:
            f.write(content)

        return {
            "success": True,
            "bytes_written": len(content.encode("utf-8")),
        }
    except Exception as e:
        return {"error": str(e)}


async def edit_file(file_path: str, old_string: str, new_string: str) -> dict:
    """Replace exact string match in a file.

    Fails if old_string is not found or appears more than once.
    """

    if is_bridge_connected():
        # Read via bridge
        read_result = await read_file_via_bridge(file_path)
        if read_result.get("error"):
            return {"error": read_result["error"]}

        content = read_result.get("content", "")
        count = content.count(old_string)
        if count == 0:
            return {"error": "old_string not found in file"}
        if count > 1:
            return {
                "error": f"old_string appears {count} times — must be unique. "
                "Provide more surrounding context to make it unique."
            }

        new_content = content.replace(old_string, new_string, 1)
        return await write_file(file_path, new_content)

    # Fallback: local filesystem
    resolved = sanitize_path(file_path)

    if not os.path.isfile(resolved):
        return {"error": f"File not found: {file_path}"}

    try:
        with open(resolved, "r", encoding="utf-8", errors="replace") as f:
            content = f.read()

        count = content.count(old_string)
        if count == 0:
            return {"error": "old_string not found in file"}
        if count > 1:
            return {
                "error": f"old_string appears {count} times — must be unique. "
                "Provide more surrounding context to make it unique."
            }

        new_content = content.replace(old_string, new_string, 1)

        with open(resolved, "w", encoding="utf-8") as f:
            f.write(new_content)

        return {"success": True}
    except Exception as e:
        return {"error": str(e)}


async def list_directory(path: str = ".") -> dict:
    """List directory contents with file types and sizes."""

    if is_bridge_connected():
        cmd = (
            f"python3 -c \""
            f"import os,json;"
            f"p='{path}';"
            f"entries=[];"
            f"[entries.append({{'name':n,'type':'dir' if os.path.isdir(os.path.join(p,n)) else 'file','size':os.stat(os.path.join(p,n)).st_size}}) for n in sorted(os.listdir(p))];"
            f"print(json.dumps({{'entries':entries,'total':len(entries)}}))"
            f"\""
        )
        result = await execute_via_bridge([cmd])
        if result.get("return_code", -1) == 0:
            try:
                return json.loads(result.get("stdout", "{}"))
            except json.JSONDecodeError:
                return {"error": "Failed to parse directory listing"}
        return {"error": result.get("stderr", "Failed to list directory via bridge")}

    # Fallback: local filesystem
    resolved = sanitize_path(path)

    if not os.path.isdir(resolved):
        return {"error": f"Not a directory: {path}"}

    try:
        entries = []
        for name in sorted(os.listdir(resolved)):
            full = os.path.join(resolved, name)
            try:
                stat = os.stat(full)
                entries.append({
                    "name": name,
                    "type": "dir" if os.path.isdir(full) else "file",
                    "size": stat.st_size,
                })
            except OSError:
                entries.append({"name": name, "type": "unknown", "size": 0})

        return {"entries": entries, "total": len(entries)}
    except Exception as e:
        return {"error": str(e)}
