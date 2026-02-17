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
    name: "modernization_agent",
    initialState: INITIAL_STATE,
  });

  const { state, setState } = coAgent;

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
    setProjectPath,
    hasAssessment,
    isOperating,
  };
}
