"""
Parse tools — tokenize COBOL and parse JCL for structural analysis.
Wraps: open-mainframe parse-jcl, open-mainframe lex
"""

from .base import run_cli, sanitize_path, try_parse_json


async def parse_jcl(jcl_file: str) -> dict:
    """Parse a JCL file and return its AST structure.
    Useful for understanding job structure before execution.

    Args:
        jcl_file: Path to a JCL file (.jcl).
    """
    path = sanitize_path(jcl_file)
    result = await run_cli(["parse-jcl", path], timeout=30)
    if result["success"] and result["stdout"]:
        result["parsed"] = try_parse_json(result["stdout"])
    return result


async def lex_cobol(source_file: str) -> dict:
    """Tokenize a COBOL source file and return the token stream.
    Useful for structural code analysis and explanation grounding.

    Args:
        source_file: Path to a COBOL source file (.cbl, .cob).
    """
    path = sanitize_path(source_file)
    result = await run_cli(["lex", path], timeout=30)
    if result["success"] and result["stdout"]:
        result["parsed"] = try_parse_json(result["stdout"])
    return result
