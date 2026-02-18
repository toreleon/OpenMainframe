"use client";

import { useState } from "react";
import { useLangGraphInterrupt } from "@copilotkit/react-core";

/**
 * Interrupt payload sent from Python agent's interrupt() call.
 * Matches the format in execute.py and dataset.py:
 *   interrupt({ "action": "Execute run_jcl", "file": "path", "description": "..." })
 */
interface InterruptPayload {
  action: string;
  file: string;
  description: string;
}

/**
 * Registers a LangGraph interrupt handler that renders an ApprovalCard
 * in the chat. Sends back { approved, reason } JSON to resume the agent.
 */
export function useInterruptHandler() {
  useLangGraphInterrupt({
    render: ({ event, resolve }) => {
      const payload = (event?.value ?? {}) as InterruptPayload;
      const action = payload.action || "Unknown action";
      const file = payload.file || "unknown";
      const description = payload.description || "";

      return (
        <InterruptApprovalCard
          action={action}
          file={file}
          description={description}
          resolve={resolve}
        />
      );
    },
  });
}

function InterruptApprovalCard({
  action,
  file,
  description,
  resolve,
}: {
  action: string;
  file: string;
  description: string;
  resolve: (response: string) => void;
}) {
  const [decided, setDecided] = useState(false);
  const [approved, setApproved] = useState(false);
  const fileName = file.split("/").pop() || file;

  const handleApprove = () => {
    setApproved(true);
    setDecided(true);
    resolve(JSON.stringify({ approved: true }));
  };

  const handleReject = () => {
    setApproved(false);
    setDecided(true);
    resolve(JSON.stringify({ approved: false, reason: "User declined execution." }));
  };

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
        <div
          className={`text-xs font-semibold ${
            approved ? "text-om-success" : "text-om-error"
          }`}
        >
          {approved ? "Approved" : "Rejected"}
        </div>
      ) : (
        <div className="flex gap-2">
          <button
            onClick={handleApprove}
            className="px-3 py-1.5 text-xs font-semibold bg-om-success/20 text-om-success border border-om-success/30 rounded hover:bg-om-success/30 transition-colors"
          >
            Approve
          </button>
          <button
            onClick={handleReject}
            className="px-3 py-1.5 text-xs font-semibold bg-om-error/20 text-om-error border border-om-error/30 rounded hover:bg-om-error/30 transition-colors"
          >
            Reject
          </button>
        </div>
      )}
    </div>
  );
}
