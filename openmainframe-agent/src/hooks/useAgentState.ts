"use client";

import { useCallback } from "react";
import { useCoAgent } from "@copilotkit/react-core";
import type { AgentState } from "@/lib/types";

const INITIAL_STATE: AgentState = {
  project_path: null,
  source_files: [],
  assessment_results: null,
  compilation_results: [],
  execution_results: [],
  current_operation: null,
  operation_progress: 0,
};

export function useAgentState() {
  const coAgent = useCoAgent<AgentState>({
    name: "default",
    initialState: INITIAL_STATE,
  });

  const raw = coAgent.state;
  const state: AgentState = {
    project_path: raw?.project_path ?? INITIAL_STATE.project_path,
    source_files: raw?.source_files ?? INITIAL_STATE.source_files,
    assessment_results: raw?.assessment_results ?? INITIAL_STATE.assessment_results,
    compilation_results: raw?.compilation_results ?? INITIAL_STATE.compilation_results,
    execution_results: raw?.execution_results ?? INITIAL_STATE.execution_results,
    current_operation: raw?.current_operation ?? INITIAL_STATE.current_operation,
    operation_progress: raw?.operation_progress ?? INITIAL_STATE.operation_progress,
  };
  const { setState } = coAgent;

  /** Set the project path from the frontend. */
  const setProjectPath = useCallback(
    (path: string) => {
      setState({ ...state, project_path: path });
    },
    [state, setState],
  );

  /** Open a workspace tab for a specific assessment. */
  const hasAssessment = state.assessment_results !== null;

  /** Check if any operation is currently running. */
  const isOperating = state.current_operation !== null;

  return {
    ...coAgent,
    state,
    setProjectPath,
    hasAssessment,
    isOperating,
  };
}
