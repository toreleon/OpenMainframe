"use client";

import { useEffect, useRef } from "react";
import type { AgentState } from "@/lib/types";
import type { TabType } from "@/hooks/useWorkspaceTabs";

/**
 * Watches agent state changes and triggers side effects:
 * - Opens assessment tab when assessment_results arrives
 * - Opens execution tab when new execution_results arrive
 */
export function useProgressSync(
  state: AgentState,
  openTab: (tab: { type: TabType; label: string; path?: string }) => void,
) {
  const prevAssessment = useRef(state.assessment_results);
  const prevExecutionCount = useRef(state.execution_results.length);

  useEffect(() => {
    // Assessment results arrived
    if (state.assessment_results && !prevAssessment.current) {
      openTab({ type: "assessment", label: "Assessment" });
    }
    prevAssessment.current = state.assessment_results;
  }, [state.assessment_results, openTab]);

  useEffect(() => {
    // New execution result arrived
    if (state.execution_results.length > prevExecutionCount.current) {
      const latest = state.execution_results[state.execution_results.length - 1];
      const name = latest?.jcl_file?.split("/").pop() || "Job";
      openTab({ type: "execution", label: `Run: ${name}` });
    }
    prevExecutionCount.current = state.execution_results.length;
  }, [state.execution_results, openTab]);
}
