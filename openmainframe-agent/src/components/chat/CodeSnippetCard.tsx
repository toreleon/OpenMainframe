"use client";

import { useState } from "react";

interface CodeSnippetCardProps {
  filePath: string;
  code: string;
  language?: string;
  startLine?: number;
  highlightLines?: number[];
}

export function CodeSnippetCard({
  filePath,
  code,
  language,
  startLine = 1,
  highlightLines = [],
}: CodeSnippetCardProps) {
  const [expanded, setExpanded] = useState(false);
  const fileName = filePath.split("/").pop() || filePath;
  const lines = code.split("\n");
  const previewLines = 10;
  const needsExpand = lines.length > previewLines;
  const displayLines = expanded ? lines : lines.slice(0, previewLines);

  return (
    <div className="bg-om-surface border border-om-border rounded-lg overflow-hidden max-w-lg">
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-2 border-b border-om-border">
        <div className="flex items-center gap-2">
          <span className="font-mono text-xs text-om-text">{fileName}</span>
          {language && (
            <span className="px-1.5 py-0.5 bg-om-border/50 rounded text-[10px] text-om-muted">
              {language.toUpperCase()}
            </span>
          )}
        </div>
        {needsExpand && (
          <button
            onClick={() => setExpanded(!expanded)}
            className="text-[10px] text-om-muted hover:text-om-accent transition-colors"
          >
            {expanded ? "Collapse" : `Show all ${lines.length} lines`}
          </button>
        )}
      </div>

      {/* Code content */}
      <div className="overflow-x-auto max-h-80 overflow-y-auto">
        <pre className="text-[10px] leading-relaxed">
          {displayLines.map((line, idx) => {
            const lineNum = startLine + idx;
            const isHighlighted = highlightLines.includes(lineNum);
            return (
              <div
                key={idx}
                className={`flex ${isHighlighted ? "bg-om-warning/10" : ""}`}
              >
                <span className="w-10 shrink-0 text-right pr-3 text-om-muted select-none py-px">
                  {lineNum}
                </span>
                <span className="text-om-text font-mono py-px pr-4">{line}</span>
              </div>
            );
          })}
          {!expanded && needsExpand && (
            <div className="text-om-muted text-center py-2">
              ... {lines.length - previewLines} more lines
            </div>
          )}
        </pre>
      </div>
    </div>
  );
}
