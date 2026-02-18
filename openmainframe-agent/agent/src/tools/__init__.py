"""
OpenMainframe CLI tool wrappers.
Provides TOOL_REGISTRY (name → function), TOOL_SCHEMAS (Anthropic format),
and HITL_TOOLS (tools requiring human approval before execution).
"""

from .assess_tools import assess_scan, assess_file
from .compile_tools import compile_cobol, check_cobol
from .execute_tools import run_jcl, interpret_cobol
from .parse_tools import parse_jcl, lex_cobol
from .dataset_tools import list_catalog, idcams_command

# name → async callable
TOOL_REGISTRY: dict = {
    "assess_scan": assess_scan,
    "assess_file": assess_file,
    "compile_cobol": compile_cobol,
    "check_cobol": check_cobol,
    "run_jcl": run_jcl,
    "interpret_cobol": interpret_cobol,
    "parse_jcl": parse_jcl,
    "lex_cobol": lex_cobol,
    "list_catalog": list_catalog,
    "idcams_command": idcams_command,
}

# Anthropic tool format: {name, description, input_schema}
TOOL_SCHEMAS: list[dict] = [
    {
        "name": "assess_scan",
        "description": (
            "Scan a directory of COBOL source files and return assessment metrics "
            "including complexity scores, feature inventory, and compatibility issues."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "directory": {
                    "type": "string",
                    "description": "Path to directory containing COBOL source files.",
                },
            },
            "required": ["directory"],
        },
    },
    {
        "name": "assess_file",
        "description": (
            "Assess a single COBOL source file for metrics and compatibility. "
            "Returns complexity, maintainability, feature usage, and issues."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "file_path": {
                    "type": "string",
                    "description": "Path to a COBOL source file (.cbl, .cob).",
                },
            },
            "required": ["file_path"],
        },
    },
    {
        "name": "compile_cobol",
        "description": (
            "Compile a COBOL source file to a native executable. "
            "Returns success/failure with compiler output and error messages."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "source_file": {
                    "type": "string",
                    "description": "Path to a COBOL source file (.cbl, .cob).",
                },
            },
            "required": ["source_file"],
        },
    },
    {
        "name": "check_cobol",
        "description": (
            "Syntax check a COBOL source file without full compilation. "
            "Faster than compile — useful for quick validation."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "source_file": {
                    "type": "string",
                    "description": "Path to a COBOL source file (.cbl, .cob).",
                },
            },
            "required": ["source_file"],
        },
    },
    {
        "name": "run_jcl",
        "description": (
            "Execute a JCL job file. Returns step-by-step execution results "
            "including per-step return codes and SYSOUT content. "
            "WARNING: This tool executes code — requires human approval."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "jcl_file": {
                    "type": "string",
                    "description": "Path to a JCL file (.jcl).",
                },
            },
            "required": ["jcl_file"],
        },
    },
    {
        "name": "interpret_cobol",
        "description": (
            "Run a COBOL program through the tree-walking interpreter. "
            "Captures DISPLAY output and final return code. "
            "WARNING: This tool executes code — requires human approval."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "source_file": {
                    "type": "string",
                    "description": "Path to a COBOL source file (.cbl, .cob).",
                },
            },
            "required": ["source_file"],
        },
    },
    {
        "name": "parse_jcl",
        "description": (
            "Parse a JCL file and return its AST structure. "
            "Useful for understanding job structure before execution."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "jcl_file": {
                    "type": "string",
                    "description": "Path to a JCL file (.jcl).",
                },
            },
            "required": ["jcl_file"],
        },
    },
    {
        "name": "lex_cobol",
        "description": (
            "Tokenize a COBOL source file and return the token stream. "
            "Useful for structural code analysis and explanation grounding."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "source_file": {
                    "type": "string",
                    "description": "Path to a COBOL source file (.cbl, .cob).",
                },
            },
            "required": ["source_file"],
        },
    },
    {
        "name": "list_catalog",
        "description": (
            "List datasets in the catalog matching the given pattern. "
            "Returns dataset names, types, record formats, and record lengths."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "pattern": {
                    "type": "string",
                    "description": 'Catalog entry pattern to match (default: "*" for all).',
                    "default": "*",
                },
            },
            "required": [],
        },
    },
    {
        "name": "idcams_command",
        "description": (
            "Execute an IDCAMS command for dataset management. "
            "Supported verbs: DEFINE, DELETE, REPRO, LISTCAT, PRINT. "
            'WARNING: DELETE operations require human approval.'
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "command": {
                    "type": "string",
                    "description": 'The IDCAMS command string (e.g., "DEFINE CLUSTER(NAME(MY.DS))").',
                },
            },
            "required": ["command"],
        },
    },
]

# Tools requiring human-in-the-loop approval before execution
HITL_TOOLS: set[str] = {"run_jcl", "interpret_cobol"}

__all__ = [
    "TOOL_REGISTRY",
    "TOOL_SCHEMAS",
    "HITL_TOOLS",
    "assess_scan",
    "assess_file",
    "compile_cobol",
    "check_cobol",
    "run_jcl",
    "interpret_cobol",
    "parse_jcl",
    "lex_cobol",
    "list_catalog",
    "idcams_command",
]
