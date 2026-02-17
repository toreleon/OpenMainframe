"use client";

interface ApprovalCardProps {
  action: string;
  file: string;
  description: string;
  onApprove?: () => void;
  onReject?: () => void;
  decided?: boolean;
  approved?: boolean;
}

export function ApprovalCard({
  action,
  file,
  description,
  onApprove,
  onReject,
  decided = false,
  approved,
}: ApprovalCardProps) {
  const fileName = file.split("/").pop() || file;

  return (
    <div
      className={`bg-om-surface border rounded-lg p-4 max-w-sm transition-opacity ${
        decided ? "border-om-border/50 opacity-60" : "border-om-warning"
      }`}
    >
      <div className="text-xs font-semibold text-om-warning mb-2">
        Approval Required
      </div>
      <div className="text-xs text-om-text mb-1">{action}</div>
      <div className="text-xs text-om-muted mb-1">
        File: <span className="font-mono text-om-text">{fileName}</span>
      </div>
      <div className="text-xs text-om-muted mb-3">{description}</div>

      {decided ? (
        <div className={`text-xs font-semibold ${approved ? "text-om-success" : "text-om-error"}`}>
          {approved ? "Approved" : "Rejected"}
        </div>
      ) : (
        <div className="flex gap-2">
          <button
            onClick={onApprove}
            className="px-3 py-1.5 text-xs font-semibold bg-om-success/20 text-om-success border border-om-success/30 rounded hover:bg-om-success/30 transition-colors"
          >
            Approve
          </button>
          <button
            onClick={onReject}
            className="px-3 py-1.5 text-xs font-semibold bg-om-error/20 text-om-error border border-om-error/30 rounded hover:bg-om-error/30 transition-colors"
          >
            Reject
          </button>
        </div>
      )}
    </div>
  );
}
