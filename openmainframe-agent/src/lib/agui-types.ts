/**
 * AG-UI protocol TypeScript types.
 *
 * The Python backend uses Pydantic with alias_generator=to_camel,
 * so all SSE event fields arrive in camelCase.
 */

// ── Event type enum ──────────────────────────────────────────────

export type AguiEventType =
  | "RUN_STARTED"
  | "RUN_FINISHED"
  | "RUN_ERROR"
  | "TEXT_MESSAGE_START"
  | "TEXT_MESSAGE_CONTENT"
  | "TEXT_MESSAGE_END"
  | "TOOL_CALL_START"
  | "TOOL_CALL_ARGS"
  | "TOOL_CALL_END"
  | "TOOL_CALL_RESULT"
  | "STATE_SNAPSHOT"
  | "CUSTOM";

// ── Individual event interfaces ──────────────────────────────────

export interface RunStartedEvent {
  type: "RUN_STARTED";
  threadId: string;
  runId: string;
}

export interface RunFinishedEvent {
  type: "RUN_FINISHED";
  threadId: string;
  runId: string;
}

export interface RunErrorEvent {
  type: "RUN_ERROR";
  message: string;
}

export interface TextMessageStartEvent {
  type: "TEXT_MESSAGE_START";
  messageId: string;
  role: string;
}

export interface TextMessageContentEvent {
  type: "TEXT_MESSAGE_CONTENT";
  messageId: string;
  delta: string;
}

export interface TextMessageEndEvent {
  type: "TEXT_MESSAGE_END";
  messageId: string;
}

export interface ToolCallStartEvent {
  type: "TOOL_CALL_START";
  toolCallId: string;
  toolCallName: string;
  parentMessageId: string;
}

export interface ToolCallArgsEvent {
  type: "TOOL_CALL_ARGS";
  toolCallId: string;
  delta: string;
}

export interface ToolCallEndEvent {
  type: "TOOL_CALL_END";
  toolCallId: string;
}

export interface ToolCallResultEvent {
  type: "TOOL_CALL_RESULT";
  toolCallId: string;
  messageId: string;
  content: string;
  role: string;
}

export interface StateSnapshotEvent {
  type: "STATE_SNAPSHOT";
  snapshot: Record<string, unknown>;
}

export interface CustomEvent {
  type: "CUSTOM";
  name: string;
  value: Record<string, unknown>;
}

// ── Union type ───────────────────────────────────────────────────

export type AguiEvent =
  | RunStartedEvent
  | RunFinishedEvent
  | RunErrorEvent
  | TextMessageStartEvent
  | TextMessageContentEvent
  | TextMessageEndEvent
  | ToolCallStartEvent
  | ToolCallArgsEvent
  | ToolCallEndEvent
  | ToolCallResultEvent
  | StateSnapshotEvent
  | CustomEvent;

// ── Request / message types ──────────────────────────────────────

export interface AguiToolCallFunction {
  name: string;
  arguments: string;
}

export interface AguiToolCall {
  id: string;
  type: "function";
  function: AguiToolCallFunction;
}

export interface AguiMessage {
  id: string;
  role: "user" | "assistant" | "tool" | "system";
  content: string;
  toolCalls?: AguiToolCall[];
  toolCallId?: string;
}

export interface RunAgentInput {
  threadId: string;
  runId: string;
  messages: AguiMessage[];
  tools: unknown[];
  context: unknown[];
  state: Record<string, unknown>;
  forwardedProps: Record<string, unknown>;
}
