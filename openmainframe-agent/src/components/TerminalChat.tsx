"use client";

import { useState, useRef, useEffect, useCallback, type FormEvent } from "react";
import ReactMarkdown from "react-markdown";
import { ToolCallBlock } from "@/components/chat/ToolCallBlock";
import type {
  ChatMessage,
  ToolCallInfo,
  InterruptPayload,
} from "@/hooks/useAgent";

// ── Props ────────────────────────────────────────────────────────

interface TerminalChatProps {
  messages: ChatMessage[];
  isStreaming: boolean;
  error: string | null;
  pendingInterrupt: InterruptPayload | null;
  onSend: (text: string) => void;
  onResolveInterrupt: (approved: boolean, reason?: string) => void;
}

// ── Component ────────────────────────────────────────────────────

export function TerminalChat({
  messages,
  isStreaming,
  error,
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
  }, [messages, isStreaming, pendingInterrupt]);

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
      {/* Scrollable message area */}
      <div ref={scrollRef} className="flex-1 overflow-y-auto px-4 py-4">
        <div className="max-w-[900px] mx-auto space-y-3">
          {/* Initial message */}
          {messages.length === 0 && !isStreaming && (
            <div className="text-om-muted text-xs border-l-2 border-om-border pl-3 py-1">
              Ready. Describe what you need.
            </div>
          )}

          {messages.map((msg) => (
            <MessageBlock key={msg.id} message={msg} />
          ))}

          {/* Streaming indicator */}
          {isStreaming &&
            messages.length > 0 &&
            messages[messages.length - 1].role === "user" && (
              <div className="flex items-center gap-2 text-xs text-om-muted border-l-2 border-om-border pl-3 py-1">
                <span className="terminal-spinner" />
                <span>thinking...</span>
              </div>
            )}

          {/* HITL interrupt card */}
          {pendingInterrupt && (
            <InterruptCard
              interrupt={pendingInterrupt}
              onResolve={onResolveInterrupt}
            />
          )}

          {/* Error display */}
          {error && (
            <div className="border-l-2 border-om-error pl-3 py-1 text-xs text-om-error">
              Error: {error}
            </div>
          )}
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

// ── Message rendering ────────────────────────────────────────────

function MessageBlock({ message }: { message: ChatMessage }) {
  if (message.role === "user") {
    return <UserMessage content={message.content} />;
  }
  return (
    <AssistantMessage
      content={message.content}
      toolCalls={message.toolCalls}
    />
  );
}

function UserMessage({ content }: { content: string }) {
  return (
    <div className="border-l-2 border-om-accent pl-3 py-1 text-[13px] leading-relaxed">
      <span className="text-om-accent font-bold">{">"} </span>
      <span className="text-om-text">{content}</span>
    </div>
  );
}

function AssistantMessage({
  content,
  toolCalls,
}: {
  content: string;
  toolCalls?: ToolCallInfo[];
}) {
  return (
    <div className="border-l-2 border-om-border pl-3 py-1 space-y-1">
      {/* Tool calls (inline, above text) */}
      {toolCalls?.map((tc) => (
        <div key={tc.id} className="space-y-0.5">
          <ToolCallBlock
            tool={tc.name}
            args={getToolCallSummary(tc)}
            status={tc.status}
          />
          {tc.result && <ToolResultBlock result={tc.result} />}
        </div>
      ))}

      {/* Markdown text */}
      {content && (
        <div className="assistant-message text-[13px] leading-relaxed text-om-text">
          <ReactMarkdown>{content}</ReactMarkdown>
        </div>
      )}
    </div>
  );
}

// ── Tool result (collapsible) ────────────────────────────────────

function ToolResultBlock({ result }: { result: string }) {
  const [expanded, setExpanded] = useState(false);

  // Try to extract meaningful summary
  let preview = result;
  try {
    const parsed = JSON.parse(result);
    if (parsed.error) {
      preview = `error: ${parsed.error}`;
    } else if (parsed.stdout !== undefined) {
      preview = parsed.stdout || "(no output)";
    } else if (parsed.content) {
      preview = parsed.content;
    } else if (parsed.success) {
      preview = "success";
    }
  } catch {
    // Use raw string
  }

  const lines = preview.split("\n");
  const isLong = lines.length > 5 || preview.length > 300;
  const shown = expanded ? preview : lines.slice(0, 3).join("\n");

  return (
    <div className="ml-5 text-[11px] text-om-muted font-mono">
      <pre className="whitespace-pre-wrap break-all">{shown}</pre>
      {isLong && (
        <button
          onClick={() => setExpanded(!expanded)}
          className="text-om-accent hover:underline mt-0.5"
        >
          {expanded ? "collapse" : `... ${lines.length} lines (expand)`}
        </button>
      )}
    </div>
  );
}

// ── HITL interrupt card ──────────────────────────────────────────

function InterruptCard({
  interrupt,
  onResolve,
}: {
  interrupt: InterruptPayload;
  onResolve: (approved: boolean, reason?: string) => void;
}) {
  const [decided, setDecided] = useState(false);
  const [approved, setApproved] = useState(false);

  const fileName = interrupt.file?.split("/").pop() || interrupt.file;

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
      className={`border-l-2 pl-3 py-2 font-mono text-xs transition-opacity ${
        decided ? "border-om-border opacity-50" : "border-om-warning"
      }`}
    >
      <div className="flex items-center gap-2 mb-1">
        <span className="text-om-warning font-semibold">approval required</span>
      </div>
      <div className="text-om-text mb-0.5">{interrupt.action}</div>
      <div className="text-om-muted mb-0.5">
        file: <span className="text-om-text">{fileName}</span>
      </div>
      {interrupt.description && (
        <div className="text-om-muted mb-2">{interrupt.description}</div>
      )}

      {decided ? (
        <div
          className={`font-semibold ${
            approved ? "text-om-success" : "text-om-error"
          }`}
        >
          {approved ? "approved" : "rejected"}
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

// ── Helpers ──────────────────────────────────────────────────────

function getToolCallSummary(tc: ToolCallInfo): string {
  try {
    const args = JSON.parse(tc.argsJson);
    // Return the most interesting arg
    return (
      args.command ||
      args.file_path ||
      args.pattern ||
      args.path ||
      tc.argsJson.slice(0, 80)
    );
  } catch {
    return tc.argsJson.slice(0, 80) || "...";
  }
}
