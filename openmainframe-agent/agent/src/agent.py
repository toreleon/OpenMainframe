"""
LangGraph agent definition — graph skeleton for Batch 1.
Starts with a simple chat-only agent. Router and capability nodes added in later batches.
"""

from langchain.agents import create_agent
from copilotkit import CopilotKitMiddleware
from langchain_openai import ChatOpenAI

from .state import AgentState

SYSTEM_PROMPT = """You are the OpenMainframe Agent — an AI-powered mainframe modernization assistant.

You help users assess, compile, execute, and understand COBOL and JCL code running on the OpenMainframe platform.

Current capabilities (more coming soon):
- Answer questions about mainframe modernization
- Explain COBOL and JCL concepts
- Guide users through the modernization process

When tools become available, you will be able to:
- Scan COBOL codebases for assessment metrics
- Compile COBOL programs and explain errors
- Execute JCL jobs with human approval
- Explain legacy code in plain English
- Manage datasets via IDCAMS

For now, respond helpfully to questions about mainframe modernization, COBOL, JCL, CICS, DB2, VSAM, and related topics.
"""

agent = create_agent(
    model=ChatOpenAI(model="gpt-4.1-mini"),
    tools=[],
    middleware=[CopilotKitMiddleware()],
    state_schema=AgentState,
    system_prompt=SYSTEM_PROMPT,
)

graph = agent
