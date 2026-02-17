"use client";

interface ProgressCardProps {
  label: string;
  progress?: number;
}

export function ProgressCard({ label, progress }: ProgressCardProps) {
  return (
    <div className="bg-om-surface border border-om-border rounded-lg p-4 max-w-sm">
      <div className="flex items-center gap-2 mb-2">
        <span className="text-om-accent animate-pulse text-sm">●</span>
        <span className="text-xs text-om-text">{label}</span>
      </div>
      {progress != null && progress > 0 && progress < 1 && (
        <div className="h-1.5 bg-om-border/50 rounded-full overflow-hidden">
          <div
            className="h-full bg-om-accent rounded-full transition-all duration-300"
            style={{ width: `${Math.round(progress * 100)}%` }}
          />
        </div>
      )}
    </div>
  );
}
