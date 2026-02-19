"use client";

import { useState, useRef, useEffect, useCallback, type FormEvent } from "react";
import ReactMarkdown from "react-markdown";
import type { ActivityEntry, InterruptPayload } from "@/hooks/useAgent";

// ── Props ────────────────────────────────────────────────────────

interface TerminalChatProps {
  entries: ActivityEntry[];
  isStreaming: boolean;
  error: string | null;
  pendingInterrupt: InterruptPayload | null;
  onSend: (text: string) => void;
  onResolveInterrupt: (approved: boolean, reason?: string) => void;
}

// ── Component ────────────────────────────────────────────────────

export function TerminalChat({
  entries,
  isStreaming,
  pendingInterrupt,
  onSend,
  onResolveInterrupt,
}: TerminalChatProps) {
  const [input, setInput] = useState("");
  const scrollRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLTextAreaElement>(null);

  // Auto-scroll to bottom
  useEffect(() => {
    const el = scrollRef.current;
    if (el) el.scrollTop = el.scrollHeight;
  }, [entries, isStreaming, pendingInterrupt]);

  // Focus input on mount
  useEffect(() => {
    inputRef.current?.focus();
  }, []);

  const handleSubmit = useCallback(
    (e: FormEvent) => {
      e.preventDefault();
      if (!input.trim() || isStreaming) return;
      onSend(input.trim());
      setInput("");
    },
    [input, isStreaming, onSend],
  );

  const handleKeyDown = useCallback(
    (e: React.KeyboardEvent<HTMLTextAreaElement>) => {
      if (e.key === "Enter" && !e.shiftKey) {
        e.preventDefault();
        if (!input.trim() || isStreaming) return;
        onSend(input.trim());
        setInput("");
      }
    },
    [input, isStreaming, onSend],
  );

  return (
    <div className="flex flex-col h-full">
      {/* Scrollable log area */}
      <div ref={scrollRef} className="flex-1 overflow-y-auto px-4 py-4">
        <div className="max-w-[900px] mx-auto space-y-1">
          {/* Empty state */}
          {entries.length === 0 && !isStreaming && (
            <div className="text-om-muted text-xs py-1">
              Ready. Describe what you need.
            </div>
          )}

          {/* Linear activity log */}
          {entries.map((entry) => (
            <EntryBlock
              key={entry.id}
              entry={entry}
              onResolveInterrupt={onResolveInterrupt}
            />
          ))}
        </div>
      </div>

      {/* Input area */}
      <div className="shrink-0 border-t border-om-border bg-om-bg px-4 py-3">
        <form
          onSubmit={handleSubmit}
          className="max-w-[900px] mx-auto flex gap-2"
        >
          <textarea
            ref={inputRef}
            value={input}
            onChange={(e) => setInput(e.target.value)}
            onKeyDown={handleKeyDown}
            disabled={isStreaming}
            placeholder="What do you need?"
            rows={1}
            className="flex-1 bg-om-surface border border-om-border border-l-2 border-l-om-accent text-om-text font-mono text-[13px] px-3 py-2 resize-none outline-none focus:border-om-accent placeholder:text-om-muted disabled:opacity-50"
          />
          <button
            type="submit"
            disabled={isStreaming || !input.trim()}
            className="bg-om-surface border border-om-border text-om-accent font-mono text-xs px-4 py-2 hover:bg-om-border disabled:opacity-30 disabled:cursor-not-allowed"
          >
            {isStreaming ? (
              <span className="terminal-spinner" />
            ) : (
              "send"
            )}
          </button>
        </form>
      </div>
    </div>
  );
}

// ── Entry rendering ──────────────────────────────────────────────

function EntryBlock({
  entry,
  onResolveInterrupt,
}: {
  entry: ActivityEntry;
  onResolveInterrupt: (approved: boolean, reason?: string) => void;
}) {
  switch (entry.kind) {
    case "user":
      return <UserEntry content={entry.content} />;
    case "thinking":
      return <ThinkingEntry />;
    case "thinking_content":
      return (
        <ThinkingContentEntry
          content={entry.content}
          streaming={entry.streaming ?? false}
        />
      );
    case "tool_call":
      return (
        <ToolCallEntry
          toolName={entry.toolName || "bash"}
          command={entry.content}
          status={entry.toolStatus || "streaming"}
        />
      );
    case "tool_output":
      return <ToolOutputEntry result={entry.toolResult} />;
    case "text":
      return <TextEntry content={entry.content} />;
    case "error":
      return <ErrorEntry content={entry.content} />;
    case "interrupt":
      return (
        <InterruptEntry
          interrupt={entry.interrupt!}
          onResolve={onResolveInterrupt}
        />
      );
    default:
      return null;
  }
}

// ── User input ──────────────────────────────────────────────────

function UserEntry({ content }: { content: string }) {
  return (
    <div className="py-1 text-[13px]">
      <span className="text-om-accent font-bold">❯ </span>
      <span className="text-om-text">{content}</span>
    </div>
  );
}

// ── Thinking spinner ────────────────────────────────────────────

function ThinkingEntry() {
  return (
    <div className="flex items-center gap-2 py-0.5 text-xs text-om-muted">
      <span className="terminal-spinner" />
      <span>thinking...</span>
    </div>
  );
}

// ── Thinking content (extended reasoning) ───────────────────────

function ThinkingContentEntry({
  content,
  streaming,
}: {
  content: string;
  streaming: boolean;
}) {
  const [expanded, setExpanded] = useState(false);

  return (
    <div className="py-0.5 font-mono text-[11px]">
      <button
        onClick={() => setExpanded(!expanded)}
        className="text-om-muted hover:text-om-accent flex items-center gap-1"
      >
        <span className="text-[10px]">{expanded ? "▼" : "▶"}</span>
        {streaming && <span className="terminal-spinner" />}
        <span className="italic">thinking...</span>
      </button>

      {expanded && (
        <pre className="mt-0.5 ml-3 text-om-muted opacity-70 italic whitespace-pre-wrap break-words leading-relaxed">
          {content}
        </pre>
      )}
    </div>
  );
}

// ── Tool call (command line) ────────────────────────────────────

function ToolCallEntry({
  toolName,
  command,
  status,
}: {
  toolName: string;
  command: string;
  status: "streaming" | "executing" | "done" | "error";
}) {
  const isActive = status === "streaming" || status === "executing";

  return (
    <div className="py-0.5 font-mono text-xs">
      <div className="flex items-start gap-2">
        {/* Status icon */}
        {isActive ? (
          <span className="terminal-spinner shrink-0 mt-0.5" />
        ) : status === "done" ? (
          <span className="text-om-success shrink-0">✓</span>
        ) : (
          <span className="text-om-error shrink-0">✗</span>
        )}

        {/* Tool name */}
        <span className="text-om-warning font-semibold shrink-0">
          {toolName}
        </span>

        {/* Command */}
        <span
          className={`text-om-text whitespace-pre-wrap break-all ${
            status === "streaming" ? "opacity-60" : ""
          }`}
        >
          {command || "..."}
        </span>
      </div>

      {status === "executing" && (
        <div className="ml-5 text-om-muted text-[11px]">executing...</div>
      )}
    </div>
  );
}

// ── Tool output (full, inline) ──────────────────────────────────

function ToolOutputEntry({
  result,
}: {
  result?: { stdout: string; stderr: string; returnCode: number };
}) {
  const [expanded, setExpanded] = useState(false);

  if (!result) return null;

  const hasStdout = result.stdout.trim().length > 0;
  const hasStderr = result.stderr.trim().length > 0;
  const hasError = result.returnCode !== 0;
  const lines = result.stdout.split("\n");
  const lineCount = lines.length;

  // Collapsed: show a one-line summary
  const summary = hasError
    ? `exit code ${result.returnCode}`
    : hasStdout
      ? `${lineCount} line${lineCount !== 1 ? "s" : ""}`
      : hasStderr
        ? "stderr"
        : "(no output)";

  return (
    <div className="ml-5 py-0.5 font-mono text-[11px] border-l border-om-border pl-3">
      {/* Toggle button — always visible */}
      <button
        onClick={() => setExpanded(!expanded)}
        className="text-om-muted hover:text-om-accent flex items-center gap-1"
      >
        <span className="text-[10px]">{expanded ? "▼" : "▶"}</span>
        <span className={hasError ? "text-om-error" : ""}>{summary}</span>
      </button>

      {/* Expanded content */}
      {expanded && (
        <div className="mt-0.5">
          {hasError && (
            <div className="text-om-error mb-0.5">
              exit code {result.returnCode}
            </div>
          )}

          {hasStdout && (
            <pre className="text-om-muted whitespace-pre-wrap break-all leading-relaxed">
              {result.stdout}
            </pre>
          )}

          {hasStderr && (
            <pre className="text-om-error whitespace-pre-wrap break-all leading-relaxed mt-0.5">
              {result.stderr}
            </pre>
          )}
        </div>
      )}
    </div>
  );
}

// ── Assistant text (markdown) ───────────────────────────────────

function TextEntry({ content }: { content: string }) {
  if (!content) return null;
  return (
    <div className="assistant-message text-[13px] leading-relaxed text-om-text py-1">
      <ReactMarkdown>{content}</ReactMarkdown>
    </div>
  );
}

// ── Error ───────────────────────────────────────────────────────

function ErrorEntry({ content }: { content: string }) {
  return (
    <div className="py-1 text-xs text-om-error font-mono">
      <span className="font-semibold">error:</span> {content}
    </div>
  );
}

// ── HITL interrupt ──────────────────────────────────────────────

function InterruptEntry({
  interrupt,
  onResolve,
}: {
  interrupt: InterruptPayload;
  onResolve: (approved: boolean, reason?: string) => void;
}) {
  const [decided, setDecided] = useState(false);
  const [approved, setApproved] = useState(false);

  const handleApprove = () => {
    setApproved(true);
    setDecided(true);
    onResolve(true);
  };

  const handleReject = () => {
    setApproved(false);
    setDecided(true);
    onResolve(false, "User declined execution.");
  };

  return (
    <div
      className={`py-2 font-mono text-xs ${
        decided ? "opacity-50" : ""
      }`}
    >
      <div className="text-om-warning font-semibold mb-0.5">
        ⚠ approval required
      </div>
      <div className="text-om-text mb-0.5">{interrupt.action}</div>
      {interrupt.description && (
        <div className="text-om-muted mb-1">{interrupt.description}</div>
      )}

      {decided ? (
        <div
          className={`font-semibold ${
            approved ? "text-om-success" : "text-om-error"
          }`}
        >
          {approved ? "✓ approved" : "✗ rejected"}
        </div>
      ) : (
        <div className="flex gap-3">
          <button
            onClick={handleApprove}
            className="text-om-success hover:underline"
          >
            [y] approve
          </button>
          <button
            onClick={handleReject}
            className="text-om-error hover:underline"
          >
            [n] reject
          </button>
        </div>
      )}
    </div>
  );
}
