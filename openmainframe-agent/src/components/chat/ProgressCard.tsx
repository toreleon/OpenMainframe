"use client";

interface ProgressStep {
  label: string;
  status: "pending" | "running" | "done" | "error";
}

interface ProgressCardProps {
  label: string;
  progress?: number;
  steps?: ProgressStep[];
}

export function ProgressCard({ label, progress, steps }: ProgressCardProps) {
  return (
    <div className="bg-om-surface border border-om-border rounded-lg p-4 max-w-lg">
      <div className="flex items-center gap-2 mb-2">
        <span className="text-om-accent animate-pulse text-sm">●</span>
        <span className="text-xs text-om-text">{label}</span>
      </div>

      {/* Progress bar */}
      {progress != null && progress > 0 && progress < 1 && (
        <div className="h-1.5 bg-om-border/50 rounded-full overflow-hidden mb-2">
          <div
            className="h-full bg-om-accent rounded-full transition-all duration-300"
            style={{ width: `${Math.round(progress * 100)}%` }}
          />
        </div>
      )}

      {/* Step indicators */}
      {steps && steps.length > 0 && (
        <div className="space-y-1 mt-2">
          {steps.map((step, idx) => (
            <div key={idx} className="flex items-center gap-2 text-[10px]">
              <span
                className={`w-1.5 h-1.5 rounded-full shrink-0 ${
                  step.status === "done"
                    ? "bg-om-success"
                    : step.status === "running"
                    ? "bg-om-accent animate-pulse"
                    : step.status === "error"
                    ? "bg-om-error"
                    : "bg-om-border"
                }`}
              />
              <span
                className={
                  step.status === "done"
                    ? "text-om-muted line-through"
                    : step.status === "running"
                    ? "text-om-accent"
                    : step.status === "error"
                    ? "text-om-error"
                    : "text-om-muted"
                }
              >
                {step.label}
              </span>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
