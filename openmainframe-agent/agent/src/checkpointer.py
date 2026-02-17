"""
Checkpointer factory — supports MemorySaver (default) or PostgreSQL.

Configure via environment:
  CHECKPOINTER=memory        (default, in-process)
  CHECKPOINTER=postgres      (PostgreSQL via langgraph-checkpoint-postgres)
  POSTGRES_URI=postgresql://user:pass@host:5432/dbname
"""

import os


def get_checkpointer():
    """Return a LangGraph checkpointer based on CHECKPOINTER env var."""
    backend = os.getenv("CHECKPOINTER", "memory").lower()

    if backend == "postgres":
        postgres_uri = os.getenv("POSTGRES_URI")
        if not postgres_uri:
            raise ValueError(
                "CHECKPOINTER=postgres requires POSTGRES_URI environment variable. "
                "Example: postgresql://user:pass@localhost:5432/openmainframe"
            )
        try:
            from langgraph.checkpoint.postgres import PostgresSaver

            return PostgresSaver.from_conn_string(postgres_uri)
        except ImportError:
            raise ImportError(
                "PostgreSQL checkpointer requires langgraph-checkpoint-postgres. "
                "Install it with: pip install 'openmainframe-agent[postgres]'"
            )

    # Default: in-memory checkpointer
    from langgraph.checkpoint.memory import MemorySaver

    return MemorySaver()
