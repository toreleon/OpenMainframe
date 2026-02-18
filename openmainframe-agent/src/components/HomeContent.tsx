"use client";

import { useAgent } from "@/hooks/useAgent";
import { useConnectionStatus } from "@/hooks/useConnectionStatus";
import { ConnectionStatusBadge } from "@/components/chat/ConnectionStatusBadge";
import { TerminalChat } from "@/components/TerminalChat";

export default function HomeContent() {
  const {
    messages,
    isStreaming,
    error,
    pendingInterrupt,
    sendMessage,
    resolveInterrupt,
  } = useAgent();
  const connectionStatus = useConnectionStatus();

  return (
    <main className="flex flex-col h-screen bg-om-bg font-mono">
      {/* Header — 40px, monospace terminal bar */}
      <header className="h-10 bg-om-surface border-b border-om-border flex items-center px-4 shrink-0">
        <div className="flex items-center gap-2">
          <span className="text-om-accent font-semibold text-xs tracking-wider">
            CODING AGENT
          </span>
          <span className="text-om-border">|</span>
          <span className="text-om-muted text-xs">agent</span>
        </div>
        <div className="flex-1" />
        <ConnectionStatusBadge
          connected={connectionStatus.connected}
          projectPath={connectionStatus.projectPath}
          loading={connectionStatus.loading}
        />
      </header>

      {/* Full-screen chat area */}
      <div className="chat-container flex-1 min-h-0">
        <TerminalChat
          messages={messages}
          isStreaming={isStreaming}
          error={error}
          pendingInterrupt={pendingInterrupt}
          onSend={sendMessage}
          onResolveInterrupt={resolveInterrupt}
        />
      </div>
    </main>
  );
}
