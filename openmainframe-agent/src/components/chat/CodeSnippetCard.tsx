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
    <div className="border-l-2 border-om-accent pl-3 py-1 font-mono text-xs">
      {/* Header line */}
      <div className="flex items-center gap-2 mb-1">
        <span className="text-om-accent font-semibold">{fileName}</span>
        {language && (
          <span className="text-om-muted">[{language}]</span>
        )}
        {needsExpand && (
          <button
            onClick={() => setExpanded(!expanded)}
            className="text-om-muted hover:text-om-accent transition-colors ml-auto"
          >
            [{expanded ? "collapse" : `${lines.length} lines`}]
          </button>
        )}
      </div>

      {/* Code with gutter */}
      <div className="bg-om-surface border border-om-border overflow-x-auto max-h-80 overflow-y-auto">
        <pre className="text-[11px] leading-relaxed">
          {displayLines.map((line, idx) => {
            const lineNum = startLine + idx;
            const isHighlighted = highlightLines.includes(lineNum);
            return (
              <div
                key={idx}
                className={`flex ${isHighlighted ? "bg-om-warning/10" : ""}`}
              >
                <span className="w-10 shrink-0 text-right pr-3 text-om-muted select-none py-px border-r border-om-border">
                  {lineNum}
                </span>
                <span className="text-om-text pl-3 py-px">{line}</span>
              </div>
            );
          })}
          {!expanded && needsExpand && (
            <div className="text-om-muted text-center py-1 border-t border-om-border">
              ... {lines.length - previewLines} more lines
            </div>
          )}
        </pre>
      </div>
    </div>
  );
}
