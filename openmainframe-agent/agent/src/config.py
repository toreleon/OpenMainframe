"""
Environment configuration and Anthropic client factory.

Supported providers (LLM_PROVIDER env var):

  anthropic     — Direct Anthropic API (default). Set ANTHROPIC_API_KEY.
  litellm-proxy — LiteLLM proxy server.
                  Set LITELLM_PROXY_URL (default http://localhost:4000)
                  and LITELLM_API_KEY. Model name maps to proxy config.
  openai        — OpenAI-compatible proxy (e.g. LiteLLM, LMStudio).
                  Uses OPENAI_BASE_URL and OPENAI_API_KEY. The proxy must
                  support the Anthropic Messages API format (/v1/messages).
"""

import os

from anthropic import AsyncAnthropic


def _strip_v1(url: str) -> str:
    """Strip trailing /v1 from a URL.

    The Anthropic SDK appends /v1/messages itself, so a base_url that
    already ends with /v1 would produce /v1/v1/messages.
    """
    return url.rstrip("/").removesuffix("/v1")


def get_client() -> AsyncAnthropic:
    """Create an AsyncAnthropic client based on environment configuration."""
    provider = os.getenv("LLM_PROVIDER", "anthropic")

    if provider == "litellm-proxy":
        return AsyncAnthropic(
            base_url=os.getenv("LITELLM_PROXY_URL", "http://localhost:4000"),
            api_key=os.getenv("LITELLM_API_KEY", "sk-1234"),
        )

    if provider == "openai":
        # Route through an OpenAI-compatible proxy that also supports the
        # Anthropic Messages API (LiteLLM, OpenRouter, etc.).
        base_url = os.getenv("OPENAI_BASE_URL")
        api_key = os.getenv("OPENAI_API_KEY", "sk-1234")
        if not base_url:
            raise ValueError(
                "LLM_PROVIDER=openai requires OPENAI_BASE_URL pointing to a "
                "proxy that supports the Anthropic Messages API format."
            )
        return AsyncAnthropic(
            base_url=_strip_v1(base_url),
            api_key=api_key,
        )

    # Default: direct Anthropic API (uses ANTHROPIC_API_KEY env var)
    return AsyncAnthropic()


def get_model_name() -> str:
    """Return the model name from environment (default: claude-sonnet-4-5-20250929)."""
    return os.getenv("LLM_MODEL", "claude-sonnet-4-5-20250929")
