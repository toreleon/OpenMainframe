"""
OpenMainframe CLI tool wrappers.
All tools are exposed as LangChain @tool functions for the LangGraph agent.
"""

from .assess_tools import assess_scan, assess_file
from .compile_tools import compile_cobol, check_cobol
from .execute_tools import run_jcl, interpret_cobol
from .parse_tools import parse_jcl, lex_cobol
from .dataset_tools import list_catalog, idcams_command

ALL_TOOLS = [
    assess_scan,
    assess_file,
    compile_cobol,
    check_cobol,
    run_jcl,
    interpret_cobol,
    parse_jcl,
    lex_cobol,
    list_catalog,
    idcams_command,
]

__all__ = [
    "ALL_TOOLS",
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
