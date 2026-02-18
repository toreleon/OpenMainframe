"""
FastAPI entry point for the OpenMainframe Agent.
Serves the LangGraph agent via AG-UI protocol for CopilotKit.
"""

import os
from dotenv import load_dotenv

load_dotenv()

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import uvicorn

from src.agent import graph

app = FastAPI(
    title="OpenMainframe Agent",
    description="AI-powered mainframe modernization assistant",
    version="1.0.0",
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "http://localhost:3000",
        "http://127.0.0.1:3000",
    ],
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/health")
async def health():
    """Health check endpoint."""
    return JSONResponse(
        content={
            "status": "ok",
            "agent": "modernization_agent",
            "version": "1.0.0",
        }
    )


# Serve the LangGraph agent via the AG-UI protocol for CopilotKit
from ag_ui_langgraph import add_langgraph_fastapi_endpoint
from copilotkit import LangGraphAGUIAgent

add_langgraph_fastapi_endpoint(
    app=app,
    agent=LangGraphAGUIAgent(
        name="modernization_agent",
        description="AI-powered mainframe modernization assistant",
        graph=graph,
    ),
    path="/",
)


def main():
    port = int(os.getenv("PORT", "8123"))
    uvicorn.run("main:app", host="0.0.0.0", port=port, reload=True)


if __name__ == "__main__":
    main()
