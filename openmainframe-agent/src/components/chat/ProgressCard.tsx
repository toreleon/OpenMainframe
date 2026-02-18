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
  const progressText =
    progress != null && progress > 0 && progress < 1
      ? ` [${Math.round(progress * 100)}%]`
      : "";

  return (
    <div className="border-l-2 border-om-accent pl-3 py-1 font-mono text-xs">
      <div className="flex items-center gap-2">
        <span className="terminal-spinner w-3" />
        <span className="text-om-text">
          {label}
          {progressText && (
            <span className="text-om-muted">{progressText}</span>
          )}
        </span>
      </div>

      {steps && steps.length > 0 && (
        <div className="mt-1 space-y-0.5 pl-5">
          {steps.map((step, idx) => {
            const icon =
              step.status === "done"
                ? "✓"
                : step.status === "error"
                ? "✗"
                : step.status === "running"
                ? "›"
                : " ";
            const color =
              step.status === "done"
                ? "text-om-success"
                : step.status === "error"
                ? "text-om-error"
                : step.status === "running"
                ? "text-om-accent"
                : "text-om-muted";
            return (
              <div key={idx} className="flex items-center gap-2">
                <span className={color}>{icon}</span>
                <span
                  className={
                    step.status === "done"
                      ? "text-om-muted"
                      : step.status === "running"
                      ? "text-om-text"
                      : step.status === "error"
                      ? "text-om-error"
                      : "text-om-muted"
                  }
                >
                  {step.label}
                </span>
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}
