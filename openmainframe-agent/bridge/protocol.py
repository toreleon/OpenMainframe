"""
Bridge protocol message definitions.

All communication between the agent server and the local bridge daemon
uses JSON messages over WebSocket. This module defines the message types
and provides serialization/deserialization helpers.
"""

from __future__ import annotations

import json
import uuid
from dataclasses import dataclass, field, asdict
from enum import Enum
from typing import Any, Optional


class MessageType(str, Enum):
    """Message types for server<->bridge communication."""
    # Server -> Bridge
    EXEC = "exec"
    LIST_FILES = "list_files"
    READ_FILE = "read_file"
    PING = "ping"

    # Bridge -> Server
    RESULT = "result"
    STREAM = "stream"
    PONG = "pong"
    ERROR = "error"


class ResultStatus(str, Enum):
    OK = "ok"
    ERROR = "error"


# ── Server-to-Bridge messages ──────────────────────────────────────


@dataclass
class ExecMessage:
    """Execute a CLI command on the bridge."""
    command: str
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    type: str = field(default=MessageType.EXEC, init=False)


@dataclass
class ListFilesMessage:
    """List files in a directory matching a glob pattern."""
    directory: str
    pattern: str = "*"
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    type: str = field(default=MessageType.LIST_FILES, init=False)


@dataclass
class ReadFileMessage:
    """Read a file (optionally a range of lines)."""
    path: str
    lines: Optional[list[int]] = None  # [start, end] or None for full file
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    type: str = field(default=MessageType.READ_FILE, init=False)


@dataclass
class PingMessage:
    type: str = field(default=MessageType.PING, init=False)


# ── Bridge-to-Server messages ──────────────────────────────────────


@dataclass
class ResultMessage:
    """Result of a command execution."""
    id: str
    status: str  # "ok" or "error"
    data: Any = None
    error: Optional[str] = None
    type: str = field(default=MessageType.RESULT, init=False)


@dataclass
class StreamMessage:
    """Partial streaming output from a command."""
    id: str
    chunk: str
    type: str = field(default=MessageType.STREAM, init=False)


@dataclass
class PongMessage:
    """Response to ping with bridge info."""
    project_path: str
    cli_version: str
    bridge_version: str = "1.0.0"
    type: str = field(default=MessageType.PONG, init=False)


@dataclass
class ErrorMessage:
    """Protocol-level error."""
    error: str
    id: Optional[str] = None
    type: str = field(default=MessageType.ERROR, init=False)


# ── Serialization helpers ──────────────────────────────────────────


def serialize(msg) -> str:
    """Serialize a message dataclass to JSON string."""
    return json.dumps(asdict(msg))


def deserialize(raw: str) -> dict:
    """Deserialize a JSON string to a dict. Raises ValueError on bad JSON."""
    try:
        data = json.loads(raw)
    except json.JSONDecodeError as e:
        raise ValueError(f"Invalid JSON: {e}")
    if "type" not in data:
        raise ValueError("Message missing 'type' field")
    return data


MAX_OUTPUT_BYTES = 50 * 1024  # 50KB output truncation limit
