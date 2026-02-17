"use client";

interface CompilerOutputCardProps {
  filePath?: string;
  success?: boolean;
  errorCount?: number;
  warningCount?: number;
}

export function CompilerOutputCard({
  filePath,
  success,
  errorCount = 0,
  warningCount = 0,
}: CompilerOutputCardProps) {
  const fileName = filePath?.split("/").pop() || "unknown";

  return (
    <div className="bg-om-surface border border-om-border rounded-lg p-4 max-w-sm">
      <div className="flex items-center gap-2 mb-2">
        <span className={`text-xs font-semibold ${success ? "text-om-success" : "text-om-error"}`}>
          {success ? "Compilation Succeeded" : "Compilation Failed"}
        </span>
      </div>
      <div className="text-xs text-om-muted mb-1">
        File: <span className="font-mono text-om-text">{fileName}</span>
      </div>
      {!success && errorCount > 0 && (
        <div className="text-xs text-om-error">
          {errorCount} error{errorCount !== 1 ? "s" : ""}
          {warningCount > 0 && (
            <span className="text-om-warning ml-2">
              {warningCount} warning{warningCount !== 1 ? "s" : ""}
            </span>
          )}
        </div>
      )}
      {success && warningCount > 0 && (
        <div className="text-xs text-om-warning">
          {warningCount} warning{warningCount !== 1 ? "s" : ""}
        </div>
      )}
    </div>
  );
}
