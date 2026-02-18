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
      <div className="border-l-2 border-om-warning pl-3 py-2 font-mono text-xs">
        <div className="flex items-center gap-2 mb-2">
          <span className="text-om-error">✗</span>
          <span className="text-om-warning font-semibold">no bridge connected</span>
        </div>
        <div className="text-om-muted mb-2">
          Install and connect the local bridge:
        </div>
        <div className="pl-2 text-om-text space-y-0.5">
          <div><span className="text-om-muted">$</span> pip install openmainframe-bridge</div>
          <div><span className="text-om-muted">$</span> openmainframe bridge connect --project ~/your-cobol-project</div>
        </div>
        <div className="text-om-muted mt-2">
          Source stays local — only CLI results flow through the agent.
        </div>
      </div>
    );
  }

  return (
    <div className="border-l-2 border-om-success pl-3 py-2 font-mono text-xs">
      <div className="flex items-center gap-2">
        <span className="text-om-success">✓</span>
        <span className="text-om-success font-semibold">connected</span>
      </div>
      {projectPath && (
        <div className="text-om-muted pl-4">
          project: <span className="text-om-text">{projectPath}</span>
        </div>
      )}
      {cliVersion && (
        <div className="text-om-muted pl-4">
          cli: <span className="text-om-text">v{cliVersion}</span>
        </div>
      )}
      {fileCount != null && fileCount > 0 && (
        <div className="text-om-muted pl-4">
          {fileCount} source file{fileCount !== 1 ? "s" : ""} detected
        </div>
      )}
    </div>
  );
}
