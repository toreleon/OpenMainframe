"use client";

import { useState } from "react";
import type { SourceFile } from "@/lib/types";

interface FileListCardProps {
  files: SourceFile[];
  projectPath?: string;
}

const TYPE_COLORS: Record<string, string> = {
  cobol: "text-om-accent",
  jcl: "text-om-info",
  copybook: "text-om-warning",
  bms: "text-om-success",
  data: "text-om-muted",
};

export function FileListCard({ files, projectPath }: FileListCardProps) {
  const [expanded, setExpanded] = useState(false);

  const byType = files.reduce<Record<string, SourceFile[]>>((acc, f) => {
    (acc[f.type] ??= []).push(f);
    return acc;
  }, {});

  const totalLoc = files.reduce((sum, f) => sum + f.line_count, 0);
  const previewCount = 5;

  return (
    <div className="bg-om-surface border border-om-border rounded-lg overflow-hidden max-w-lg">
      <div className="p-4">
        <div className="flex items-center justify-between mb-2">
          <div className="text-xs font-semibold text-om-accent">Source Files</div>
          {files.length > previewCount && (
            <button
              onClick={() => setExpanded(!expanded)}
              className="text-[10px] text-om-muted hover:text-om-accent transition-colors"
            >
              {expanded ? "Collapse" : `View all ${files.length}`}
            </button>
          )}
        </div>

        {projectPath && (
          <div className="text-[10px] text-om-muted mb-2 font-mono">{projectPath}</div>
        )}

        {/* Type summary */}
        <div className="flex gap-3 mb-3 flex-wrap">
          {Object.entries(byType).map(([type, list]) => (
            <span key={type} className="text-[10px]">
              <span className={TYPE_COLORS[type] || "text-om-muted"}>{list.length}</span>
              <span className="text-om-muted ml-1">{type}</span>
            </span>
          ))}
          <span className="text-[10px] text-om-muted">| {totalLoc.toLocaleString()} LOC</span>
        </div>

        {/* File list (preview or full) */}
        <div className="space-y-0.5">
          {(expanded ? files : files.slice(0, previewCount)).map((f) => {
            const name = f.path.split("/").pop() || f.path;
            return (
              <div key={f.path} className="flex items-center gap-2 text-[10px] py-0.5">
                <span className={`w-1.5 h-1.5 rounded-full shrink-0 ${
                  TYPE_COLORS[f.type]?.replace("text-", "bg-") || "bg-om-muted"
                }`} />
                <span className="font-mono text-om-text truncate">{name}</span>
                <span className="text-om-muted ml-auto shrink-0">{f.line_count} lines</span>
              </div>
            );
          })}
          {!expanded && files.length > previewCount && (
            <div className="text-[10px] text-om-muted pt-1">
              ...and {files.length - previewCount} more
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
