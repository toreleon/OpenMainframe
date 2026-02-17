"use client";

interface StatusBarProps {
  currentOperation: string | null;
  operationProgress: number;
  fileCount: number;
}

export function StatusBar({
  currentOperation,
  operationProgress,
  fileCount,
}: StatusBarProps) {
  return (
    <footer className="h-6 bg-om-surface border-t border-om-border flex items-center px-4 text-[11px] shrink-0">
      <div className="flex items-center gap-3">
        {currentOperation ? (
          <>
            <span className="text-om-accent animate-pulse">●</span>
            <span className="text-om-text capitalize">
              {currentOperation}
            </span>
            {operationProgress > 0 && operationProgress < 1 && (
              <span className="text-om-muted">
                {Math.round(operationProgress * 100)}%
              </span>
            )}
          </>
        ) : (
          <span className="text-om-success">● Ready</span>
        )}
      </div>
      <div className="flex-1" />
      <div className="flex items-center gap-3 text-om-muted">
        {fileCount > 0 && <span>{fileCount} files</span>}
        <span>OpenMainframe Agent v1.0</span>
      </div>
    </footer>
  );
}
