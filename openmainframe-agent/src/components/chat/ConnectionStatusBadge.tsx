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
      <div className="flex items-center gap-2 text-xs">
        <span className="w-2 h-2 rounded-full bg-om-muted animate-pulse" />
        <span className="text-om-muted">Connecting...</span>
      </div>
    );
  }

  if (!connected) {
    return (
      <div className="flex items-center gap-2 text-xs">
        <span className="w-2 h-2 rounded-full bg-om-error" />
        <span className="text-om-muted">No bridge connected</span>
      </div>
    );
  }

  return (
    <div className="flex items-center gap-2 text-xs">
      <span className="w-2 h-2 rounded-full bg-om-success" />
      <span className="text-om-muted">Connected:</span>
      <span className="text-om-text font-mono truncate max-w-xs">
        {projectPath || "unknown"}
      </span>
    </div>
  );
}
