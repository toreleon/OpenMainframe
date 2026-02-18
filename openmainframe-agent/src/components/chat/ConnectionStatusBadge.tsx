"use client";

interface ConnectionStatusBadgeProps {
  connected: boolean;
  projectPath: string | null;
  loading: boolean;
}

export function ConnectionStatusBadge({
  connected,
  projectPath,
  loading,
}: ConnectionStatusBadgeProps) {
  if (loading) {
    return (
      <div className="flex items-center gap-1.5 font-mono text-xs">
        <span className="text-om-muted">●</span>
        <span className="text-om-muted">connecting...</span>
      </div>
    );
  }

  if (!connected) {
    return (
      <div className="flex items-center gap-1.5 font-mono text-xs">
        <span className="text-om-error">●</span>
        <span className="text-om-muted">disconnected</span>
      </div>
    );
  }

  return (
    <div className="flex items-center gap-1.5 font-mono text-xs">
      <span className="text-om-success">●</span>
      <span className="text-om-muted truncate max-w-xs">
        {projectPath || "connected"}
      </span>
    </div>
  );
}
