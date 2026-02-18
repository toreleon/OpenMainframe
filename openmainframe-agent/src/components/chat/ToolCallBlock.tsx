"use client";

interface ToolCallBlockProps {
  tool: string;
  args: string;
  status: "running" | "done" | "error";
}

const TOOL_COLORS: Record<string, string> = {
  bash: "text-om-warning",
  read_file: "text-om-accent",
  write_file: "text-om-success",
  edit_file: "text-om-success",
  list_directory: "text-om-info",
  grep: "text-om-info",
  glob: "text-om-info",
};

export function ToolCallBlock({ tool, args, status }: ToolCallBlockProps) {
  const toolColor = TOOL_COLORS[tool] || "text-om-muted";
  const truncatedArgs = args.length > 80 ? args.slice(0, 77) + "..." : args;

  const statusIcon =
    status === "done"
      ? "✓"
      : status === "error"
      ? "✗"
      : null;

  const statusColor =
    status === "done"
      ? "text-om-success"
      : status === "error"
      ? "text-om-error"
      : "text-om-accent";

  return (
    <div className="flex items-center gap-2 py-0.5 border-l-2 border-om-border pl-3 font-mono text-xs">
      {status === "running" ? (
        <span className="terminal-spinner w-3" />
      ) : (
        <span className={statusColor}>{statusIcon}</span>
      )}
      <span className={`font-semibold ${toolColor}`}>{tool}</span>
      <span className="text-om-muted">›</span>
      <span className="text-om-text truncate">{truncatedArgs}</span>
    </div>
  );
}
