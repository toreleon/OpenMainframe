"use client";

import type { SourceFile } from "@/lib/types";

interface FileTreePanelProps {
  files: SourceFile[];
  projectPath: string | null;
  onFileSelect: (path: string) => void;
}

const TYPE_ICONS: Record<string, string> = {
  cobol: "CB",
  jcl: "JC",
  copybook: "CP",
  bms: "BM",
  data: "DA",
};

const TYPE_COLORS: Record<string, string> = {
  cobol: "text-om-accent",
  jcl: "text-om-info",
  copybook: "text-om-warning",
  bms: "text-om-success",
  data: "text-om-muted",
};

export function FileTreePanel({
  files,
  projectPath,
  onFileSelect,
}: FileTreePanelProps) {
  if (!projectPath) {
    return (
      <div className="w-56 bg-om-surface border-r border-om-border flex flex-col shrink-0">
        <div className="p-3 text-xs font-semibold text-om-muted uppercase tracking-wider border-b border-om-border">
          Explorer
        </div>
        <div className="flex-1 flex items-center justify-center p-4">
          <p className="text-om-muted text-xs text-center">
            No project loaded. Use the chat to set a project directory.
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="w-56 bg-om-surface border-r border-om-border flex flex-col shrink-0">
      <div className="p-3 text-xs font-semibold text-om-muted uppercase tracking-wider border-b border-om-border">
        Explorer
      </div>
      <div className="flex-1 overflow-y-auto">
        {files.length === 0 ? (
          <p className="text-om-muted text-xs p-3">
            No source files discovered yet. Run an assessment to populate.
          </p>
        ) : (
          <ul className="py-1">
            {files.map((file) => {
              const name = file.path.split("/").pop() || file.path;
              const icon = TYPE_ICONS[file.type] || "??";
              const color = TYPE_COLORS[file.type] || "text-om-muted";
              return (
                <li key={file.path}>
                  <button
                    onClick={() => onFileSelect(file.path)}
                    className="w-full text-left px-3 py-1.5 hover:bg-om-border/30 flex items-center gap-2 text-xs transition-colors"
                  >
                    <span
                      className={`${color} font-mono text-[10px] font-bold w-5 shrink-0`}
                    >
                      {icon}
                    </span>
                    <span className="text-om-text truncate">{name}</span>
                    <span className="text-om-muted ml-auto text-[10px]">
                      {file.line_count}L
                    </span>
                  </button>
                </li>
              );
            })}
          </ul>
        )}
      </div>
    </div>
  );
}
