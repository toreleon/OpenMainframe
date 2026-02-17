"""
Assess node — scan codebases and analyze COBOL files for migration readiness.
Uses assess_scan and assess_file tools.
"""

from langchain_core.messages import SystemMessage
from langgraph.types import Command

from ..config import get_model
from ..tools.assess_tools import assess_scan, assess_file
from ..util import should_route_to_tool_node

ASSESS_SYSTEM = """You are a mainframe migration assessment expert. Help users understand their COBOL codebase's modernization readiness using OpenMainframe.

You have these tools:
- assess_scan: Scan a directory of COBOL files for metrics, complexity, features, and compatibility issues
- assess_file: Analyze a single COBOL file for detailed assessment

When presenting assessment results:
1. Start with an executive summary: number of files, total lines, overall complexity
2. Highlight critical compatibility issues first
3. Group findings by category (DB2, CICS, VSAM, platform-specific)
4. Present migration complexity with effort estimates
5. List actionable recommendations in priority order
6. For single file assessments, provide section-by-section analysis

When the user specifies a project directory:
- First use assess_scan to get the big picture
- Then offer to drill into specific files with assess_file
- Track the project_path for future operations

If the assessment reveals issues you can't fully explain, say so clearly.
All file paths must be within the configured workspace."""

ASSESS_TOOLS = [assess_scan, assess_file]


async def assess_node(state, config) -> Command:
    """Handle codebase assessment and metrics requests."""
    model = get_model(config)

    # Get frontend tools from CopilotKit for generative UI
    fe_tools = state.get("copilotkit", {}).get("actions", [])

    # Bind assess tools + any frontend tools
    model_with_tools = model.bind_tools([*fe_tools, *ASSESS_TOOLS])

    response = await model_with_tools.ainvoke(
        [SystemMessage(content=ASSESS_SYSTEM), *state["messages"]],
        config,
    )

    tool_calls = getattr(response, "tool_calls", [])

    if tool_calls:
        if should_route_to_tool_node(tool_calls, fe_tools):
            return Command(goto="tools", update={
                "messages": [response],
                "active_node": "assess",
                "current_operation": "assess",
            })
        # Frontend tool — CopilotKit handles it
        return Command(goto="__end__", update={
            "messages": [response],
        })

    # No tool calls — direct response
    return Command(goto="__end__", update={
        "messages": [response],
        "current_operation": None,
    })
