"""
FastAPI entry point for the OpenMainframe Agent.
Serves the LangGraph agent via AG-UI protocol for CopilotKit.
Accepts local bridge connections via WebSocket at /ws/bridge.
"""

import os
from dotenv import load_dotenv

load_dotenv()

from fastapi import FastAPI, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import uvicorn

from src.agent import graph
from src.bridge_client import bridge_manager

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
    """Health check endpoint — includes bridge connection status."""
    bridge_status = bridge_manager.status()
    return JSONResponse(
        content={
            "status": "ok",
            "agent": "modernization_agent",
            "version": "1.0.0",
            **bridge_status,
        }
    )


@app.websocket("/ws/bridge")
async def bridge_websocket(ws: WebSocket, token: str = ""):
    """WebSocket endpoint for local bridge daemon connections."""
    await ws.accept()
    print(f"Bridge connected (token: {token[:8]}...)")

    conn = await bridge_manager.register(ws, token or "default")
    print(f"  Project: {conn.project_path}")
    print(f"  CLI:     {conn.cli_version}")

    try:
        while True:
            raw = await ws.receive_text()
            try:
                import json
                msg = json.loads(raw)
                conn.handle_response(msg)
            except Exception as e:
                print(f"  Bridge message error: {e}")
    except WebSocketDisconnect:
        print(f"Bridge disconnected (token: {token[:8]}...)")
    except Exception as e:
        print(f"Bridge error: {e}")
    finally:
        bridge_manager.unregister(token or "default")


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
