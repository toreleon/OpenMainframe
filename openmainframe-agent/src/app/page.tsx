"use client";

import { CopilotSidebar } from "@copilotkit/react-ui";
import { useRenderToolCall } from "@copilotkit/react-core";
import { useAgentState } from "@/hooks/useAgentState";
import { useWorkspaceTabs } from "@/hooks/useWorkspaceTabs";
import { Header } from "@/components/layout/Header";
import { FileTreePanel } from "@/components/layout/FileTreePanel";
import { WorkspacePanel } from "@/components/layout/WorkspacePanel";
import { StatusBar } from "@/components/layout/StatusBar";
import { AssessmentCard } from "@/components/chat/AssessmentCard";
import { CompilerOutputCard } from "@/components/chat/CompilerOutputCard";
import { ProgressCard } from "@/components/chat/ProgressCard";
import { ExplanationCard } from "@/components/chat/ExplanationCard";

export default function Home() {
  const { state } = useAgentState();
  const { tabs, activeTab, activeTabId, setActiveTabId, openTab, closeTab } =
    useWorkspaceTabs();

  const handleFileSelect = (path: string) => {
    const name = path.split("/").pop() || path;
    openTab({ type: "code", label: name, path });
  };

  // Generative UI — render tool calls inline in chat
  useRenderToolCall({
    name: "assess_scan",
    render: ({ status }) => {
      if (status === "inProgress") return <ProgressCard label="Scanning codebase..." />;
      return <AssessmentCard report={state.assessment_results} />;
    },
  });

  useRenderToolCall({
    name: "assess_file",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Assessing ${args?.path || "file"}...`} />;
      return <AssessmentCard report={state.assessment_results} />;
    },
  });

  useRenderToolCall({
    name: "compile_cobol",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Compiling ${args?.source_file || ""}...`} />;
      const latest = state.compilation_results[state.compilation_results.length - 1];
      return (
        <CompilerOutputCard
          filePath={latest?.file_path || args?.source_file}
          success={latest?.success}
          errorCount={latest?.errors?.length}
          warningCount={latest?.warnings?.length}
        />
      );
    },
  });

  useRenderToolCall({
    name: "check_cobol",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Checking ${args?.source_file || ""}...`} />;
      const latest = state.compilation_results[state.compilation_results.length - 1];
      return (
        <CompilerOutputCard
          filePath={latest?.file_path || args?.source_file}
          success={latest?.success}
          errorCount={latest?.errors?.length}
          warningCount={latest?.warnings?.length}
        />
      );
    },
  });

  useRenderToolCall({
    name: "lex_cobol",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Analyzing ${args?.source_file || ""}...`} />;
      return <ExplanationCard filePath={args?.source_file} />;
    },
  });

  useRenderToolCall({
    name: "parse_jcl",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Parsing ${args?.jcl_file || ""}...`} />;
      return <ExplanationCard filePath={args?.jcl_file} />;
    },
  });

  useRenderToolCall({
    name: "run_jcl",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Running ${args?.jcl_file || ""}...`} />;
      return <ProgressCard label="Job complete" />;
    },
  });

  useRenderToolCall({
    name: "interpret_cobol",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Interpreting ${args?.source_file || ""}...`} />;
      return <ProgressCard label="Interpretation complete" />;
    },
  });

  const latestExecution =
    state.execution_results.length > 0
      ? state.execution_results[state.execution_results.length - 1]
      : null;

  return (
    <CopilotSidebar
      defaultOpen={true}
      labels={{
        title: "OpenMainframe Agent",
        initial:
          "Hello! I'm your mainframe modernization assistant. I can help you assess, compile, execute, and explain COBOL and JCL code. Set a project directory to get started, or just ask me a question.",
      }}
    >
      <main className="flex flex-col h-screen bg-om-bg">
        <Header projectPath={state.project_path} />
        <div className="flex flex-1 min-h-0">
          <FileTreePanel
            files={state.source_files}
            projectPath={state.project_path}
            onFileSelect={handleFileSelect}
          />
          <WorkspacePanel
            tabs={tabs}
            activeTab={activeTab}
            activeTabId={activeTabId}
            onTabSelect={setActiveTabId}
            onTabClose={closeTab}
            assessmentReport={state.assessment_results}
            latestExecution={latestExecution}
            onFileSelect={handleFileSelect}
          />
        </div>
        <StatusBar
          currentOperation={state.current_operation}
          operationProgress={state.operation_progress}
          fileCount={state.source_files.length}
        />
      </main>
    </CopilotSidebar>
  );
}
