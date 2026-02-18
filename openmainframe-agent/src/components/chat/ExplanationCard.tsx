"use client";

import { useState } from "react";

interface ExplanationCardProps {
  filePath?: string;
  summary?: string;
  codeSnippet?: string;
  language?: string;
}

export function ExplanationCard({ filePath, summary, codeSnippet, language }: ExplanationCardProps) {
  const [showCode, setShowCode] = useState(false);
  const fileName = filePath?.split("/").pop() || "code";

  return (
    <div className="border-l-2 border-om-info pl-3 py-1 font-mono text-xs">
      <div className="flex items-center gap-2">
        <span className="text-om-info font-semibold">analysis</span>
        <span className="text-om-muted">›</span>
        <span className="text-om-text">{fileName}</span>
        {language && (
          <span className="text-om-muted">[{language}]</span>
        )}
        {codeSnippet && (
          <button
            onClick={() => setShowCode(!showCode)}
            className="text-om-muted hover:text-om-accent transition-colors ml-2"
          >
            [{showCode ? "hide" : "show"}]
          </button>
        )}
      </div>
      {summary && (
        <div className="text-om-text mt-1 pl-2 leading-relaxed">{summary}</div>
      )}
      {showCode && codeSnippet && (
        <pre className="mt-1 bg-om-surface border border-om-border p-2 text-[11px] text-om-text overflow-x-auto max-h-64 overflow-y-auto leading-relaxed">
          {codeSnippet}
        </pre>
      )}
    </div>
  );
}
