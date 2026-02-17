"use client";

import { CopilotSidebar } from "@copilotkit/react-ui";
import { useAgentState } from "@/hooks/useAgentState";
import { useWorkspaceTabs } from "@/hooks/useWorkspaceTabs";
import { Header } from "@/components/layout/Header";
import { FileTreePanel } from "@/components/layout/FileTreePanel";
import { WorkspacePanel } from "@/components/layout/WorkspacePanel";
import { StatusBar } from "@/components/layout/StatusBar";

export default function Home() {
  const { state } = useAgentState();
  const { tabs, activeTab, activeTabId, setActiveTabId, openTab, closeTab } =
    useWorkspaceTabs();

  const handleFileSelect = (path: string) => {
    const name = path.split("/").pop() || path;
    openTab({ type: "code", label: name, path });
  };

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
