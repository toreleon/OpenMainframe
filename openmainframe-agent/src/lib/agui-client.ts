/**
 * Thin SSE client for the AG-UI protocol.
 * POST to the Python agent and yield parsed events.
 */

import type { AguiEvent, RunAgentInput } from "./agui-types";

/**
 * Stream AG-UI events from the Python agent.
 *
 * @param agentUrl - Base URL of the Python agent (e.g. "http://localhost:8123")
 * @param input    - RunAgentInput payload (camelCase)
 * @param signal   - Optional AbortSignal for cancellation
 */
export async function* streamAgui(
  agentUrl: string,
  input: RunAgentInput,
  signal?: AbortSignal,
): AsyncGenerator<AguiEvent> {
  const res = await fetch(agentUrl, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Accept: "text/event-stream",
    },
    body: JSON.stringify(input),
    signal,
  });

  if (!res.ok) {
    throw new Error(`Agent returned ${res.status}: ${res.statusText}`);
  }

  const reader = res.body?.getReader();
  if (!reader) throw new Error("No response body");

  const decoder = new TextDecoder();
  let buffer = "";

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });

    // SSE format: "data: {json}\n\n"
    const parts = buffer.split("\n\n");
    // Keep the last (possibly incomplete) chunk in the buffer
    buffer = parts.pop() ?? "";

    for (const part of parts) {
      for (const line of part.split("\n")) {
        if (line.startsWith("data: ")) {
          const json_str = line.slice(6);
          if (json_str === "[DONE]") return;
          try {
            yield JSON.parse(json_str) as AguiEvent;
          } catch {
            // Skip malformed JSON
          }
        }
      }
    }
  }
}
