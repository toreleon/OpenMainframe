"use client";

interface HeaderProps {
  projectPath: string | null;
}

export function Header({ projectPath }: HeaderProps) {
  return (
    <header className="h-12 bg-om-surface border-b border-om-border flex items-center px-4 shrink-0">
      <div className="flex items-center gap-3">
        <span className="text-om-accent font-bold text-sm tracking-wide">
          OPENMAINFRAME
        </span>
        <span className="text-om-muted text-xs">|</span>
        <span className="text-om-muted text-xs">Agent</span>
      </div>
      <div className="flex-1" />
      {projectPath && (
        <div className="flex items-center gap-2 text-xs">
          <span className="text-om-muted">Project:</span>
          <span className="text-om-text font-mono truncate max-w-xs">
            {projectPath}
          </span>
        </div>
      )}
    </header>
  );
}
