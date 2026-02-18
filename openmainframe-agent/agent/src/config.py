"""
Environment configuration and Anthropic client factory.

Supported providers (LLM_PROVIDER env var):

  anthropic     — Direct Anthropic API (default). Set ANTHROPIC_API_KEY.
  litellm-proxy — LiteLLM proxy server (recommended for production).
                  Exposes an OpenAI-compatible API that the Anthropic SDK
                  can talk to via base_url override.
                  Set LITELLM_PROXY_URL (default http://localhost:4000)
                  and LITELLM_API_KEY. Model name maps to proxy config.
"""

import os

from anthropic import AsyncAnthropic


def get_client() -> AsyncAnthropic:
    """Create an AsyncAnthropic client based on environment configuration."""
    provider = os.getenv("LLM_PROVIDER", "anthropic")

    if provider == "litellm-proxy":
        return AsyncAnthropic(
            base_url=os.getenv("LITELLM_PROXY_URL", "http://localhost:4000"),
            api_key=os.getenv("LITELLM_API_KEY", "sk-1234"),
        )

    # Default: direct Anthropic API (uses ANTHROPIC_API_KEY env var)
    return AsyncAnthropic()


def get_model_name() -> str:
    """Return the model name from environment (default: claude-sonnet-4-5-20250929)."""
    return os.getenv("LLM_MODEL", "claude-sonnet-4-5-20250929")
