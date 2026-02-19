"use client";

import { useState, useRef, useCallback } from "react";
import { streamAgui } from "@/lib/agui-client";
import type {
  AguiEvent,
  AguiMessage,
  AguiToolCall,
  RunAgentInput,
} from "@/lib/agui-types";

// ── Public types ─────────────────────────────────────────────────

/** A single entry in the linear activity log (like Claude Code). */
export interface ActivityEntry {
  id: string;
  kind:
    | "user"              // user input
    | "thinking"          // spinner while LLM is generating
    | "thinking_content"  // extended thinking (reasoning text)
    | "tool_call"         // tool name + command
    | "tool_output"       // stdout/stderr/return_code
    | "text"              // assistant text (markdown)
    | "error"             // error message
    | "interrupt";        // HITL approval
  content: string;
  /** For thinking_content: still receiving deltas */
  streaming?: boolean;
  /** For tool_call: the tool name */
  toolName?: string;
  /** For tool_output: parsed result */
  toolResult?: { stdout: string; stderr: string; returnCode: number };
  /** For tool_call: streaming | executing | done | error */
  toolStatus?: "streaming" | "executing" | "done" | "error";
  /** For interrupt */
  interrupt?: InterruptPayload;
}

export interface InterruptPayload {
  action: string;
  file: string;
  description: string;
}

export interface UseAgentReturn {
  entries: ActivityEntry[];
  isStreaming: boolean;
  error: string | null;
  pendingInterrupt: InterruptPayload | null;
  sendMessage: (text: string) => Promise<void>;
  resolveInterrupt: (approved: boolean, reason?: string) => Promise<void>;
}

// ── Constants ────────────────────────────────────────────────────

const AGENT_URL =
  typeof window !== "undefined"
    ? (process.env.NEXT_PUBLIC_AGENT_URL ?? "http://localhost:8123")
    : "http://localhost:8123";

// ── Hook ─────────────────────────────────────────────────────────

export function useAgent(): UseAgentReturn {
  const [entries, setEntries] = useState<ActivityEntry[]>([]);
  const [isStreaming, setIsStreaming] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [pendingInterrupt, setPendingInterrupt] =
    useState<InterruptPayload | null>(null);

  const historyRef = useRef<AguiMessage[]>([]);
  const threadIdRef = useRef<string>(crypto.randomUUID());
  const abortRef = useRef<AbortController | null>(null);

  // ── Helpers ─────────────────────────────────────────────────

  const addEntry = useCallback((entry: ActivityEntry) => {
    setEntries((prev) => [...prev, entry]);
  }, []);

  const updateEntry = useCallback(
    (id: string, updater: (e: ActivityEntry) => ActivityEntry) => {
      setEntries((prev) => prev.map((e) => (e.id === id ? updater(e) : e)));
    },
    [],
  );

  const removeEntry = useCallback((id: string) => {
    setEntries((prev) => prev.filter((e) => e.id !== id));
  }, []);

  // ── Process SSE stream ───────────────────────────────────────

  const processStream = useCallback(
    async (input: RunAgentInput) => {
      setIsStreaming(true);
      setError(null);

      const controller = new AbortController();
      abortRef.current = controller;

      // Local accumulation state
      let currentText = "";
      let currentMessageId = "";
      let currentTextEntryId = "";
      let thinkingId = "";
      let currentThinkingEntryId = "";
      const toolCallMap = new Map<
        string,
        { entryId: string; argsJson: string; name: string }
      >();

      // For AG-UI history
      const toolCallsForHistory: AguiToolCall[] = [];
      const toolResultsForHistory: AguiMessage[] = [];

      // Show thinking spinner
      thinkingId = `thinking_${crypto.randomUUID()}`;
      addEntry({ id: thinkingId, kind: "thinking", content: "" });

      try {
        for await (const event of streamAgui(
          AGENT_URL,
          input,
          controller.signal,
        )) {
          switch (event.type) {
            case "TEXT_MESSAGE_START": {
              currentMessageId = event.messageId;
              currentText = "";
              // Remove thinking spinner
              if (thinkingId) {
                removeEntry(thinkingId);
                thinkingId = "";
              }
              // Start a text entry
              currentTextEntryId = `text_${event.messageId}`;
              addEntry({
                id: currentTextEntryId,
                kind: "text",
                content: "",
              });
              break;
            }

            case "TEXT_MESSAGE_CONTENT": {
              currentText += event.delta;
              updateEntry(currentTextEntryId, (e) => ({
                ...e,
                content: e.content + event.delta,
              }));
              break;
            }

            case "TEXT_MESSAGE_END":
              // Text done — nothing extra needed
              break;

            case "TOOL_CALL_START": {
              // Remove thinking spinner
              if (thinkingId) {
                removeEntry(thinkingId);
                thinkingId = "";
              }

              const entryId = `tool_${event.toolCallId}`;
              toolCallMap.set(event.toolCallId, {
                entryId,
                argsJson: "",
                name: event.toolCallName,
              });

              addEntry({
                id: entryId,
                kind: "tool_call",
                content: "",
                toolName: event.toolCallName,
                toolStatus: "streaming",
              });
              break;
            }

            case "TOOL_CALL_ARGS": {
              const tc = toolCallMap.get(event.toolCallId);
              if (tc) {
                tc.argsJson += event.delta;
                // Try to extract command from partial JSON
                const cmd = extractCommand(tc.argsJson);
                updateEntry(tc.entryId, (e) => ({
                  ...e,
                  content: cmd,
                }));
              }
              break;
            }

            case "TOOL_CALL_END": {
              const tc = toolCallMap.get(event.toolCallId);
              if (tc) {
                const cmd = extractCommand(tc.argsJson);
                updateEntry(tc.entryId, (e) => ({
                  ...e,
                  content: cmd,
                  toolStatus: "executing",
                }));
                // Save for history
                toolCallsForHistory.push({
                  id: event.toolCallId,
                  type: "function",
                  function: { name: tc.name, arguments: tc.argsJson },
                });
              }
              break;
            }

            case "TOOL_CALL_RESULT": {
              const tc = toolCallMap.get(event.toolCallId);
              if (tc) {
                // Mark tool call as done
                updateEntry(tc.entryId, (e) => ({
                  ...e,
                  toolStatus: "done",
                }));

                // Parse result and add output entry
                const parsed = parseToolResult(event.content);
                const outputId = `output_${event.toolCallId}`;
                addEntry({
                  id: outputId,
                  kind: "tool_output",
                  content: event.content,
                  toolResult: parsed,
                });

                // Save for history
                toolResultsForHistory.push({
                  id: `res_${event.toolCallId}`,
                  role: "tool",
                  content: event.content,
                  toolCallId: event.toolCallId,
                });
              }

              // Show thinking again (agent will process result and respond)
              thinkingId = `thinking_${crypto.randomUUID()}`;
              addEntry({ id: thinkingId, kind: "thinking", content: "" });
              break;
            }

            case "CUSTOM":
              if (event.name === "thinking_start") {
                // Replace the generic spinner with a real thinking entry
                if (thinkingId) {
                  removeEntry(thinkingId);
                  thinkingId = "";
                }
                const id = `thinking_content_${(event.value as { id: string }).id}`;
                currentThinkingEntryId = id;
                addEntry({
                  id,
                  kind: "thinking_content",
                  content: "",
                  streaming: true,
                });
              } else if (event.name === "thinking_content") {
                if (currentThinkingEntryId) {
                  const delta = (event.value as { delta: string }).delta;
                  updateEntry(currentThinkingEntryId, (e) => ({
                    ...e,
                    content: e.content + delta,
                  }));
                }
              } else if (event.name === "thinking_end") {
                if (currentThinkingEntryId) {
                  updateEntry(currentThinkingEntryId, (e) => ({
                    ...e,
                    streaming: false,
                  }));
                  currentThinkingEntryId = "";
                }
              } else if (event.name === "on_interrupt") {
                if (thinkingId) {
                  removeEntry(thinkingId);
                  thinkingId = "";
                }
                const interrupt = event.value as unknown as InterruptPayload;
                setPendingInterrupt(interrupt);
                addEntry({
                  id: `interrupt_${crypto.randomUUID()}`,
                  kind: "interrupt",
                  content: interrupt.description,
                  interrupt,
                });
              }
              break;

            case "RUN_ERROR":
              if (thinkingId) {
                removeEntry(thinkingId);
                thinkingId = "";
              }
              setError(event.message);
              addEntry({
                id: `error_${crypto.randomUUID()}`,
                kind: "error",
                content: event.message,
              });
              break;

            case "RUN_FINISHED":
              // Remove any lingering thinking spinner
              if (thinkingId) {
                removeEntry(thinkingId);
                thinkingId = "";
              }
              break;
          }
        }
      } catch (err: unknown) {
        if ((err as Error).name !== "AbortError") {
          const msg =
            err instanceof Error ? err.message : "Unknown stream error";
          setError(msg);
          addEntry({
            id: `error_${crypto.randomUUID()}`,
            kind: "error",
            content: msg,
          });
        }
      } finally {
        // Clean up thinking spinner
        if (thinkingId) {
          removeEntry(thinkingId);
        }
        setIsStreaming(false);
        abortRef.current = null;
      }

      // ── Update AG-UI history ──────────────────────────────────

      if (currentText || toolCallsForHistory.length > 0) {
        const assistantMsg: AguiMessage = {
          id: currentMessageId || crypto.randomUUID(),
          role: "assistant",
          content: currentText,
          ...(toolCallsForHistory.length > 0
            ? { toolCalls: toolCallsForHistory }
            : {}),
        };
        historyRef.current.push(assistantMsg, ...toolResultsForHistory);
      }
    },
    [addEntry, updateEntry, removeEntry],
  );

  // ── Send message ─────────────────────────────────────────────

  const sendMessage = useCallback(
    async (text: string) => {
      if (!text.trim() || isStreaming) return;

      // Add user entry
      addEntry({
        id: crypto.randomUUID(),
        kind: "user",
        content: text,
      });

      // Add to AG-UI history
      const aguiUserMsg: AguiMessage = {
        id: crypto.randomUUID(),
        role: "user",
        content: text,
      };
      historyRef.current.push(aguiUserMsg);

      const input: RunAgentInput = {
        threadId: threadIdRef.current,
        runId: crypto.randomUUID(),
        messages: historyRef.current,
        tools: [],
        context: [],
        state: {},
        forwardedProps: {},
      };

      await processStream(input);
    },
    [isStreaming, processStream, addEntry],
  );

  // ── Resolve HITL interrupt ───────────────────────────────────

  const resolveInterrupt = useCallback(
    async (approved: boolean, reason?: string) => {
      if (!pendingInterrupt) return;
      setPendingInterrupt(null);

      const input: RunAgentInput = {
        threadId: threadIdRef.current,
        runId: crypto.randomUUID(),
        messages: historyRef.current,
        tools: [],
        context: [],
        state: {},
        forwardedProps: {
          command: {
            resume: JSON.stringify({
              approved,
              ...(reason ? { reason } : {}),
            }),
          },
        },
      };

      await processStream(input);
    },
    [pendingInterrupt, processStream],
  );

  return {
    entries,
    isStreaming,
    error,
    pendingInterrupt,
    sendMessage,
    resolveInterrupt,
  };
}

// ── Helpers ──────────────────────────────────────────────────────

function extractCommand(partialJson: string): string {
  // Try to parse complete JSON first
  try {
    const args = JSON.parse(partialJson);
    return args.command || args.file_path || args.pattern || partialJson;
  } catch {
    // Extract command value from partial JSON: {"command":"ls -la...
    const match = partialJson.match(/"command"\s*:\s*"((?:[^"\\]|\\.)*)(?:"|$)/);
    if (match) return match[1].replace(/\\"/g, '"').replace(/\\\\/g, "\\");
    return partialJson;
  }
}

function parseToolResult(raw: string): {
  stdout: string;
  stderr: string;
  returnCode: number;
} {
  try {
    const parsed = JSON.parse(raw);
    return {
      stdout: parsed.stdout ?? parsed.content ?? "",
      stderr: parsed.stderr ?? "",
      returnCode: parsed.return_code ?? parsed.returnCode ?? 0,
    };
  } catch {
    return { stdout: raw, stderr: "", returnCode: 0 };
  }
}
