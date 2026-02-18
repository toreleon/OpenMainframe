"use client";

interface ConnectionCardProps {
  connected: boolean;
  projectPath?: string;
  cliVersion?: string;
  fileCount?: number;
}

export function ConnectionCard({
  connected,
  projectPath,
  cliVersion,
  fileCount,
}: ConnectionCardProps) {
  if (!connected) {
    return (
      <div className="bg-om-surface border border-om-warning/50 rounded-lg p-4 max-w-lg">
        <div className="flex items-center gap-2 mb-3">
          <span className="w-2 h-2 rounded-full bg-om-error" />
          <span className="text-xs font-semibold text-om-warning">No Local Environment Connected</span>
        </div>
        <div className="text-xs text-om-muted mb-3">
          To get started, install and run the local bridge on your machine:
        </div>
        <div className="bg-om-bg rounded p-3 font-mono text-[10px] text-om-text space-y-1">
          <div>pip install openmainframe-bridge</div>
          <div>openmainframe bridge connect --project ~/your-cobol-project</div>
        </div>
        <div className="text-[10px] text-om-muted mt-3">
          Your source code stays on your machine — only CLI command results flow through the agent.
        </div>
      </div>
    );
  }

  return (
    <div className="bg-om-surface border border-om-success/30 rounded-lg p-4 max-w-lg">
      <div className="flex items-center gap-2 mb-2">
        <span className="w-2 h-2 rounded-full bg-om-success" />
        <span className="text-xs font-semibold text-om-success">Connected</span>
      </div>
      {projectPath && (
        <div className="text-xs text-om-muted">
          Project: <span className="font-mono text-om-text">{projectPath}</span>
        </div>
      )}
      {cliVersion && (
        <div className="text-xs text-om-muted">
          CLI: <span className="text-om-text">v{cliVersion}</span>
        </div>
      )}
      {fileCount != null && fileCount > 0 && (
        <div className="text-xs text-om-muted mt-1">
          {fileCount} source file{fileCount !== 1 ? "s" : ""} detected
        </div>
      )}
    </div>
  );
}
