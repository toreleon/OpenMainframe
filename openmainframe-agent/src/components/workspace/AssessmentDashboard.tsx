"use client";

import type { AssessmentReport } from "@/lib/types";

interface AssessmentDashboardProps {
  report: AssessmentReport;
  onFileSelect: (path: string) => void;
}

const SEVERITY_COLORS: Record<string, string> = {
  critical: "text-om-error",
  high: "text-om-warning",
  warning: "text-om-warning",
  info: "text-om-info",
};

const SEVERITY_DOTS: Record<string, string> = {
  critical: "bg-om-error",
  high: "bg-om-warning",
  warning: "bg-om-warning",
  info: "bg-om-info",
};

function MetricCard({ label, value, sub }: { label: string; value: string | number; sub?: string }) {
  return (
    <div className="bg-om-surface border border-om-border rounded-lg p-4 text-center">
      <div className="text-2xl font-bold text-om-text">{value}</div>
      <div className="text-xs text-om-muted mt-1">{label}</div>
      {sub && <div className="text-[10px] text-om-muted mt-0.5">{sub}</div>}
    </div>
  );
}

function FeatureBar({ name, pct }: { name: string; pct: number }) {
  const color = pct >= 80 ? "bg-om-success" : pct >= 50 ? "bg-om-warning" : "bg-om-error";
  return (
    <div className="flex items-center gap-2 text-xs">
      <span className="text-om-muted w-24 truncate text-right">{name}</span>
      <div className="flex-1 h-2 bg-om-border/50 rounded-full overflow-hidden">
        <div className={`h-full ${color} rounded-full`} style={{ width: `${pct}%` }} />
      </div>
      <span className="text-om-muted w-10 text-right">{pct}%</span>
    </div>
  );
}

export function AssessmentDashboard({ report, onFileSelect }: AssessmentDashboardProps) {
  const issueCount = report.issues.length;
  const totalDebt = report.programs.reduce((sum, p) => sum + p.technical_debt_hours, 0);

  return (
    <div className="p-6 overflow-auto h-full">
      <div className="flex items-center justify-between mb-6">
        <h2 className="text-lg font-bold text-om-text">Assessment Report</h2>
        <span className="text-xs text-om-muted">{report.total_files} files scanned</span>
      </div>

      {/* Metric cards */}
      <div className="grid grid-cols-3 gap-3 mb-6">
        <MetricCard label="Files" value={report.total_files} />
        <MetricCard label="Lines of Code" value={report.total_loc.toLocaleString()} />
        <MetricCard label="Avg Complexity" value={report.average_complexity.toFixed(1)} />
        <MetricCard label="Issues" value={issueCount} />
        <MetricCard label="Tech Debt" value={`${Math.round(totalDebt)}h`} />
        <MetricCard
          label="Programs"
          value={report.programs.length}
        />
      </div>

      {/* Feature support matrix */}
      {Object.keys(report.feature_support).length > 0 && (
        <div className="mb-6">
          <h3 className="text-sm font-semibold text-om-text mb-3">Feature Support</h3>
          <div className="space-y-2">
            {Object.entries(report.feature_support)
              .sort(([, a], [, b]) => b - a)
              .map(([feature, pct]) => (
                <FeatureBar key={feature} name={feature} pct={pct} />
              ))}
          </div>
        </div>
      )}

      {/* Issues list */}
      {issueCount > 0 && (
        <div className="mb-6">
          <h3 className="text-sm font-semibold text-om-text mb-3">
            Issues ({issueCount})
          </h3>
          <div className="space-y-1">
            {report.issues.map((issue, idx) => (
              <button
                key={idx}
                onClick={() => onFileSelect(issue.file_path)}
                className="w-full text-left flex items-center gap-2 px-3 py-2 text-xs rounded hover:bg-om-border/20 transition-colors"
              >
                <span className={`w-2 h-2 rounded-full shrink-0 ${SEVERITY_DOTS[issue.severity] || "bg-om-muted"}`} />
                <span className={`uppercase text-[10px] font-bold w-16 shrink-0 ${SEVERITY_COLORS[issue.severity] || "text-om-muted"}`}>
                  {issue.severity}
                </span>
                <span className="text-om-text font-mono truncate">
                  {issue.file_path.split("/").pop()}
                  {issue.line != null && `:${issue.line}`}
                </span>
                <span className="text-om-muted truncate ml-auto">{issue.message}</span>
              </button>
            ))}
          </div>
        </div>
      )}

      {/* Programs table */}
      {report.programs.length > 0 && (
        <div>
          <h3 className="text-sm font-semibold text-om-text mb-3">Programs</h3>
          <div className="overflow-x-auto">
            <table className="w-full text-xs">
              <thead>
                <tr className="border-b border-om-border text-om-muted text-left">
                  <th className="pb-2 pr-4">Program</th>
                  <th className="pb-2 pr-4">LOC</th>
                  <th className="pb-2 pr-4">Complexity</th>
                  <th className="pb-2 pr-4">Maintainability</th>
                  <th className="pb-2">Features</th>
                </tr>
              </thead>
              <tbody>
                {report.programs
                  .sort((a, b) => b.complexity - a.complexity)
                  .map((prog) => (
                    <tr
                      key={prog.program_id}
                      className="border-b border-om-border/30 hover:bg-om-border/10 cursor-pointer"
                      onClick={() => onFileSelect(prog.file_path)}
                    >
                      <td className="py-2 pr-4 font-mono text-om-accent">{prog.program_id}</td>
                      <td className="py-2 pr-4 text-om-text">{prog.loc.toLocaleString()}</td>
                      <td className="py-2 pr-4 text-om-text">{prog.complexity.toFixed(1)}</td>
                      <td className="py-2 pr-4 text-om-text">{prog.maintainability}/100</td>
                      <td className="py-2 text-om-muted truncate max-w-[200px]">
                        {prog.features_used.join(", ")}
                      </td>
                    </tr>
                  ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {/* Recommendations */}
      {report.recommendations.length > 0 && (
        <div className="mt-6">
          <h3 className="text-sm font-semibold text-om-text mb-3">Recommendations</h3>
          <ul className="space-y-2">
            {report.recommendations.map((rec, idx) => (
              <li key={idx} className="flex gap-2 text-xs text-om-muted">
                <span className="text-om-accent shrink-0">-</span>
                <span>{rec}</span>
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
