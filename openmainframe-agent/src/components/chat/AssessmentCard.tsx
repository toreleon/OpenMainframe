"use client";

import type { AssessmentReport } from "@/lib/types";

interface AssessmentCardProps {
  report: AssessmentReport | null;
}

export function AssessmentCard({ report }: AssessmentCardProps) {
  if (!report) {
    return (
      <div className="bg-om-surface border border-om-border rounded-lg p-3 text-xs text-om-muted">
        No assessment data available.
      </div>
    );
  }

  const issueCount = report.issues.length;
  const criticalCount = report.issues.filter((i) => i.severity === "critical").length;

  return (
    <div className="bg-om-surface border border-om-border rounded-lg p-4 max-w-sm">
      <div className="text-xs font-semibold text-om-accent mb-2">Assessment Complete</div>
      <div className="grid grid-cols-3 gap-2 text-center mb-3">
        <div>
          <div className="text-lg font-bold text-om-text">{report.total_files}</div>
          <div className="text-[10px] text-om-muted">Files</div>
        </div>
        <div>
          <div className="text-lg font-bold text-om-text">{report.total_loc.toLocaleString()}</div>
          <div className="text-[10px] text-om-muted">LOC</div>
        </div>
        <div>
          <div className="text-lg font-bold text-om-text">{report.average_complexity.toFixed(1)}</div>
          <div className="text-[10px] text-om-muted">Avg Cmplx</div>
        </div>
      </div>
      {issueCount > 0 && (
        <div className="text-xs text-om-muted">
          {issueCount} issues found
          {criticalCount > 0 && (
            <span className="text-om-error ml-1">({criticalCount} critical)</span>
          )}
        </div>
      )}
    </div>
  );
}
