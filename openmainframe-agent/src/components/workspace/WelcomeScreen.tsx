"use client";

export function WelcomeScreen() {
  return (
    <div className="flex-1 flex items-center justify-center p-8">
      <div className="max-w-lg text-center">
        <h1 className="text-3xl font-bold text-om-text mb-4">
          OpenMainframe Agent
        </h1>
        <p className="text-om-muted mb-8">
          AI-powered mainframe modernization assistant. Use the chat panel to
          interact with your COBOL and JCL codebases.
        </p>
        <div className="grid grid-cols-3 gap-4">
          <ActionCard icon="CB" label="Assess" desc="Scan a codebase" color="text-om-accent" />
          <ActionCard icon="CO" label="Compile" desc="Build a program" color="text-om-success" />
          <ActionCard icon="EX" label="Explain" desc="Understand legacy code" color="text-om-info" />
        </div>
        <div className="mt-6 grid grid-cols-3 gap-4">
          <ActionCard icon="RN" label="Execute" desc="Run JCL jobs" color="text-om-warning" />
          <ActionCard icon="DS" label="Datasets" desc="Manage VSAM" color="text-om-info" />
          <ActionCard icon="CH" label="Chat" desc="Ask anything" color="text-om-muted" />
        </div>
      </div>
    </div>
  );
}

function ActionCard({
  icon,
  label,
  desc,
  color,
}: {
  icon: string;
  label: string;
  desc: string;
  color: string;
}) {
  return (
    <div className="bg-om-surface border border-om-border rounded-lg p-4 text-center">
      <div className={`text-lg font-mono font-bold mb-1 ${color}`}>{icon}</div>
      <div className="text-sm text-om-text font-medium">{label}</div>
      <div className="text-xs text-om-muted mt-1">{desc}</div>
    </div>
  );
}
