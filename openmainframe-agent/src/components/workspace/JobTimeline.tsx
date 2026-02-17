"use client";

import { useState } from "react";
import type { ExecutionResult, StepResult } from "@/lib/types";

interface JobTimelineProps {
  result: ExecutionResult;
}

function rcColor(rc: number): string {
  if (rc === 0) return "text-om-success";
  if (rc <= 4) return "text-om-warning";
  return "text-om-error";
}

function rcBg(rc: number): string {
  if (rc === 0) return "bg-om-success";
  if (rc <= 4) return "bg-om-warning";
  return "bg-om-error";
}

function rcLabel(rc: number): string {
  if (rc === 0) return "OK";
  if (rc <= 4) return "WARN";
  return "FAIL";
}

function StepDetail({ step }: { step: StepResult }) {
  return (
    <div className="bg-om-bg border border-om-border rounded-lg p-4 mt-2 text-xs">
      <div className="grid grid-cols-3 gap-4 mb-3">
        <div>
          <span className="text-om-muted">Program:</span>
          <span className="text-om-text ml-2 font-mono">{step.program}</span>
        </div>
        <div>
          <span className="text-om-muted">Return Code:</span>
          <span className={`ml-2 font-bold ${rcColor(step.return_code)}`}>
            {step.return_code}
          </span>
        </div>
        <div>
          <span className="text-om-muted">Duration:</span>
          <span className="text-om-text ml-2">
            {(step.duration_ms / 1000).toFixed(1)}s
          </span>
        </div>
      </div>
      {step.output && (
        <div>
          <div className="text-om-muted mb-1">Output:</div>
          <pre className="bg-om-surface border border-om-border/50 rounded p-3 text-[11px] text-om-text whitespace-pre-wrap overflow-x-auto max-h-48 overflow-y-auto">
            {step.output}
          </pre>
        </div>
      )}
    </div>
  );
}

export function JobTimeline({ result }: JobTimelineProps) {
  const [expandedStep, setExpandedStep] = useState<number | null>(null);
  const totalDuration = result.steps.reduce((sum, s) => sum + s.duration_ms, 0);

  return (
    <div className="p-6 overflow-auto h-full">
      <div className="flex items-center justify-between mb-6">
        <h2 className="text-lg font-bold text-om-text">
          Job Execution: {result.jcl_file.split("/").pop()}
        </h2>
        <span className="text-xs text-om-muted">{result.timestamp}</span>
      </div>

      {/* Timeline visualization */}
      <div className="flex items-center gap-1 mb-6 overflow-x-auto py-2">
        {result.steps.map((step, idx) => (
          <div key={idx} className="flex items-center">
            <button
              onClick={() => setExpandedStep(expandedStep === idx ? null : idx)}
              className={`flex flex-col items-center px-3 py-2 rounded-lg border transition-colors ${
                expandedStep === idx
                  ? "bg-om-border/30 border-om-accent"
                  : "border-transparent hover:bg-om-border/20"
              }`}
            >
              <div className={`w-4 h-4 rounded-full ${rcBg(step.return_code)} mb-1`} />
              <span className="text-[10px] text-om-text font-mono">{step.step_name}</span>
              <span className="text-[10px] text-om-muted">{step.program}</span>
              <span className={`text-[10px] font-bold ${rcColor(step.return_code)}`}>
                RC={step.return_code}
              </span>
              <span className="text-[10px] text-om-muted">
                {(step.duration_ms / 1000).toFixed(1)}s
              </span>
            </button>
            {idx < result.steps.length - 1 && (
              <div className="w-6 h-px bg-om-border mx-1 shrink-0" />
            )}
          </div>
        ))}
      </div>

      {/* Expanded step detail */}
      {expandedStep !== null && result.steps[expandedStep] && (
        <StepDetail step={result.steps[expandedStep]} />
      )}

      {/* Summary */}
      <div className="mt-6 flex items-center gap-4 text-xs border-t border-om-border pt-4">
        <span className="text-om-muted">
          {result.steps.length} steps
        </span>
        <span className={`font-bold ${rcColor(result.max_return_code)}`}>
          Max RC={result.max_return_code} ({rcLabel(result.max_return_code)})
        </span>
        <span className="text-om-muted">
          Total: {(totalDuration / 1000).toFixed(1)}s
        </span>
      </div>
    </div>
  );
}
