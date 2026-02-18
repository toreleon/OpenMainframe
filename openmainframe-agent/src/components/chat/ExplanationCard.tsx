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
  const fileName = filePath?.split("/").pop() || "Code";

  return (
    <div className="bg-om-surface border border-om-border rounded-lg overflow-hidden max-w-lg">
      <div className="p-4">
        <div className="flex items-center justify-between mb-2">
          <div className="text-xs font-semibold text-om-info">Code Analysis</div>
          {codeSnippet && (
            <button
              onClick={() => setShowCode(!showCode)}
              className="text-[10px] text-om-muted hover:text-om-accent transition-colors"
            >
              {showCode ? "Hide Code" : "View Code"}
            </button>
          )}
        </div>
        <div className="text-xs text-om-muted mb-1">
          File: <span className="font-mono text-om-text">{fileName}</span>
          {language && (
            <span className="ml-2 px-1.5 py-0.5 bg-om-border/50 rounded text-[10px]">
              {language.toUpperCase()}
            </span>
          )}
        </div>
        {summary && (
          <div className="text-xs text-om-text mt-2 leading-relaxed">{summary}</div>
        )}
      </div>

      {/* Expandable code snippet */}
      {showCode && codeSnippet && (
        <div className="border-t border-om-border">
          <pre className="p-4 text-[10px] font-mono text-om-text bg-om-bg/50 overflow-x-auto max-h-64 overflow-y-auto leading-relaxed">
            {codeSnippet}
          </pre>
        </div>
      )}
    </div>
  );
}
