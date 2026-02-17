"use client";

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
  return useCoAgent<AgentState>({
    name: "modernization_agent",
    initialState: INITIAL_STATE,
  });
}
