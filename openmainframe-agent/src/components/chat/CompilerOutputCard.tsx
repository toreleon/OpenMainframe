"use client";

import { useState } from "react";
import type { CompilerError } from "@/lib/types";

interface CompilerOutputCardProps {
  filePath?: string;
  success?: boolean;
  errorCount?: number;
  warningCount?: number;
  errors?: CompilerError[];
  warnings?: CompilerError[];
}

export function CompilerOutputCard({
  filePath,
  success,
  errorCount = 0,
  warningCount = 0,
  errors = [],
  warnings = [],
}: CompilerOutputCardProps) {
  const [expanded, setExpanded] = useState(false);
  const fileName = filePath?.split("/").pop() || "unknown";
  const hasDetails = errors.length > 0 || warnings.length > 0;

  return (
    <div className="bg-om-surface border border-om-border rounded-lg overflow-hidden max-w-lg">
      {/* Summary */}
      <div className="p-4">
        <div className="flex items-center justify-between mb-2">
          <span className={`text-xs font-semibold ${success ? "text-om-success" : "text-om-error"}`}>
            {success ? "Compilation Succeeded" : "Compilation Failed"}
          </span>
          {hasDetails && (
            <button
              onClick={() => setExpanded(!expanded)}
              className="text-[10px] text-om-muted hover:text-om-accent transition-colors"
            >
              {expanded ? "Collapse" : "View Details"}
            </button>
          )}
        </div>
        <div className="text-xs text-om-muted mb-1">
          File: <span className="font-mono text-om-text">{fileName}</span>
        </div>
        <div className="flex gap-3 text-xs">
          {errorCount > 0 && (
            <span className="text-om-error">
              {errorCount} error{errorCount !== 1 ? "s" : ""}
            </span>
          )}
          {warningCount > 0 && (
            <span className="text-om-warning">
              {warningCount} warning{warningCount !== 1 ? "s" : ""}
            </span>
          )}
          {errorCount === 0 && warningCount === 0 && success && (
            <span className="text-om-muted">Clean build</span>
          )}
        </div>
      </div>

      {/* Expanded error/warning list */}
      {expanded && hasDetails && (
        <div className="border-t border-om-border p-4 space-y-3 max-h-60 overflow-y-auto">
          {errors.length > 0 && (
            <div>
              <h4 className="text-[10px] font-semibold text-om-error mb-1.5">Errors</h4>
              <div className="space-y-1">
                {errors.map((err, idx) => (
                  <div key={idx} className="text-[10px] font-mono bg-om-bg/50 rounded px-2 py-1.5">
                    <span className="text-om-muted">L{err.line}:{err.column}</span>
                    <span className="text-om-text ml-2">{err.message}</span>
                  </div>
                ))}
              </div>
            </div>
          )}
          {warnings.length > 0 && (
            <div>
              <h4 className="text-[10px] font-semibold text-om-warning mb-1.5">Warnings</h4>
              <div className="space-y-1">
                {warnings.map((warn, idx) => (
                  <div key={idx} className="text-[10px] font-mono bg-om-bg/50 rounded px-2 py-1.5">
                    <span className="text-om-muted">L{warn.line}:{warn.column}</span>
                    <span className="text-om-text ml-2">{warn.message}</span>
                  </div>
                ))}
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
