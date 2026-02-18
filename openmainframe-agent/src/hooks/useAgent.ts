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

export interface ToolCallInfo {
  id: string;
  name: string;
  argsJson: string;
  result?: string;
  status: "running" | "done" | "error";
}

export interface ChatMessage {
  id: string;
  role: "user" | "assistant" | "tool";
  content: string;
  toolCalls?: ToolCallInfo[];
}

export interface InterruptPayload {
  action: string;
  file: string;
  description: string;
}

export interface UseAgentReturn {
  messages: ChatMessage[];
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
  const [messages, setMessages] = useState<ChatMessage[]>([]);
  const [isStreaming, setIsStreaming] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [pendingInterrupt, setPendingInterrupt] =
    useState<InterruptPayload | null>(null);

  // Persistent AG-UI message history for the backend
  const historyRef = useRef<AguiMessage[]>([]);
  const threadIdRef = useRef<string>(crypto.randomUUID());
  const abortRef = useRef<AbortController | null>(null);

  // ── Process SSE stream ───────────────────────────────────────

  const processStream = useCallback(
    async (input: RunAgentInput) => {
      setIsStreaming(true);
      setError(null);

      const controller = new AbortController();
      abortRef.current = controller;

      // Accumulation state (local to this run)
      let currentText = "";
      let currentMessageId = "";
      const toolCalls = new Map<string, ToolCallInfo>();
      const toolCallArgBuffers = new Map<string, string>();

      try {
        for await (const event of streamAgui(
          AGENT_URL,
          input,
          controller.signal,
        )) {
          handleEvent(event);
        }
      } catch (err: unknown) {
        if ((err as Error).name !== "AbortError") {
          const msg =
            err instanceof Error ? err.message : "Unknown stream error";
          setError(msg);
        }
      } finally {
        setIsStreaming(false);
        abortRef.current = null;
      }

      // ── After stream finishes, update history ───────────────

      // Build assistant message for history
      const assistantToolCalls: AguiToolCall[] = [];
      const toolResultMessages: AguiMessage[] = [];

      for (const tc of toolCalls.values()) {
        assistantToolCalls.push({
          id: tc.id,
          type: "function",
          function: { name: tc.name, arguments: tc.argsJson },
        });
        if (tc.result !== undefined) {
          toolResultMessages.push({
            id: `res_${tc.id}`,
            role: "tool",
            content: tc.result,
            toolCallId: tc.id,
          });
        }
      }

      if (currentText || assistantToolCalls.length > 0) {
        const assistantMsg: AguiMessage = {
          id: currentMessageId || crypto.randomUUID(),
          role: "assistant",
          content: currentText,
          ...(assistantToolCalls.length > 0
            ? { toolCalls: assistantToolCalls }
            : {}),
        };
        historyRef.current.push(assistantMsg, ...toolResultMessages);
      }

      // ── Event handler ─────────────────────────────────────

      function handleEvent(event: AguiEvent) {
        switch (event.type) {
          case "TEXT_MESSAGE_START":
            currentMessageId = event.messageId;
            currentText = "";
            // Add placeholder assistant message
            setMessages((prev) => [
              ...prev,
              { id: event.messageId, role: "assistant", content: "" },
            ]);
            break;

          case "TEXT_MESSAGE_CONTENT":
            currentText += event.delta;
            setMessages((prev) =>
              prev.map((m) =>
                m.id === event.messageId
                  ? { ...m, content: m.content + event.delta }
                  : m,
              ),
            );
            break;

          case "TEXT_MESSAGE_END":
            // Nothing extra needed — content already accumulated
            break;

          case "TOOL_CALL_START": {
            const tc: ToolCallInfo = {
              id: event.toolCallId,
              name: event.toolCallName,
              argsJson: "",
              status: "running",
            };
            toolCalls.set(event.toolCallId, tc);
            toolCallArgBuffers.set(event.toolCallId, "");

            // Attach tool call to the parent assistant message
            setMessages((prev) =>
              prev.map((m) =>
                m.id === event.parentMessageId
                  ? { ...m, toolCalls: [...(m.toolCalls || []), tc] }
                  : m,
              ),
            );
            break;
          }

          case "TOOL_CALL_ARGS": {
            const buf =
              (toolCallArgBuffers.get(event.toolCallId) ?? "") + event.delta;
            toolCallArgBuffers.set(event.toolCallId, buf);

            const existing = toolCalls.get(event.toolCallId);
            if (existing) existing.argsJson = buf;

            setMessages((prev) =>
              prev.map((m) =>
                m.toolCalls
                  ? {
                      ...m,
                      toolCalls: m.toolCalls.map((tc) =>
                        tc.id === event.toolCallId
                          ? { ...tc, argsJson: buf }
                          : tc,
                      ),
                    }
                  : m,
              ),
            );
            break;
          }

          case "TOOL_CALL_END": {
            // Mark tool call as done (result may arrive later)
            const existing = toolCalls.get(event.toolCallId);
            if (existing) existing.status = "done";

            setMessages((prev) =>
              prev.map((m) =>
                m.toolCalls
                  ? {
                      ...m,
                      toolCalls: m.toolCalls.map((tc) =>
                        tc.id === event.toolCallId
                          ? { ...tc, status: "done" }
                          : tc,
                      ),
                    }
                  : m,
              ),
            );
            break;
          }

          case "TOOL_CALL_RESULT": {
            const existing = toolCalls.get(event.toolCallId);
            if (existing) {
              existing.result = event.content;
              existing.status = "done";
            }

            setMessages((prev) =>
              prev.map((m) =>
                m.toolCalls
                  ? {
                      ...m,
                      toolCalls: m.toolCalls.map((tc) =>
                        tc.id === event.toolCallId
                          ? { ...tc, result: event.content, status: "done" }
                          : tc,
                      ),
                    }
                  : m,
              ),
            );
            break;
          }

          case "CUSTOM":
            if (event.name === "on_interrupt") {
              setPendingInterrupt(event.value as unknown as InterruptPayload);
            }
            break;

          case "RUN_ERROR":
            setError(event.message);
            break;

          // RUN_STARTED, RUN_FINISHED, STATE_SNAPSHOT: no UI action needed
        }
      }
    },
    [],
  );

  // ── Send message ─────────────────────────────────────────────

  const sendMessage = useCallback(
    async (text: string) => {
      if (!text.trim() || isStreaming) return;

      // Add user message to UI
      const userMsg: ChatMessage = {
        id: crypto.randomUUID(),
        role: "user",
        content: text,
      };
      setMessages((prev) => [...prev, userMsg]);

      // Add to AG-UI history
      const aguiUserMsg: AguiMessage = {
        id: userMsg.id,
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
    [isStreaming, processStream],
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
    messages,
    isStreaming,
    error,
    pendingInterrupt,
    sendMessage,
    resolveInterrupt,
  };
}
