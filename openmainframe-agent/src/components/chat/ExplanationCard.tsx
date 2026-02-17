"use client";

interface ExplanationCardProps {
  filePath?: string;
  summary?: string;
}

export function ExplanationCard({ filePath, summary }: ExplanationCardProps) {
  const fileName = filePath?.split("/").pop() || "Code";

  return (
    <div className="bg-om-surface border border-om-border rounded-lg p-4 max-w-sm">
      <div className="text-xs font-semibold text-om-info mb-2">
        Code Explanation
      </div>
      <div className="text-xs text-om-muted mb-1">
        File: <span className="font-mono text-om-text">{fileName}</span>
      </div>
      {summary && (
        <div className="text-xs text-om-text mt-2 leading-relaxed">{summary}</div>
      )}
    </div>
  );
}
