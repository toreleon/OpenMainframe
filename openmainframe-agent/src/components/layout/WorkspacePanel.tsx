"use client";

import type { WorkspaceTab } from "@/hooks/useWorkspaceTabs";
import type { AssessmentReport, ExecutionResult } from "@/lib/types";
import { TabManager } from "@/components/workspace/TabManager";
import { CodeViewer } from "@/components/workspace/CodeViewer";
import { WelcomeScreen } from "@/components/workspace/WelcomeScreen";
import { AssessmentDashboard } from "@/components/workspace/AssessmentDashboard";
import { JobTimeline } from "@/components/workspace/JobTimeline";

interface WorkspacePanelProps {
  tabs: WorkspaceTab[];
  activeTab: WorkspaceTab;
  activeTabId: string;
  onTabSelect: (id: string) => void;
  onTabClose: (id: string) => void;
  assessmentReport: AssessmentReport | null;
  latestExecution: ExecutionResult | null;
  onFileSelect: (path: string) => void;
}

export function WorkspacePanel({
  tabs,
  activeTab,
  activeTabId,
  onTabSelect,
  onTabClose,
  assessmentReport,
  latestExecution,
  onFileSelect,
}: WorkspacePanelProps) {
  return (
    <div className="flex-1 flex flex-col min-w-0">
      <TabManager
        tabs={tabs}
        activeTabId={activeTabId}
        onTabSelect={onTabSelect}
        onTabClose={onTabClose}
      />
      <div className="flex-1 overflow-auto">
        {activeTab.type === "welcome" && <WelcomeScreen />}
        {activeTab.type === "code" && activeTab.path && (
          <CodeViewer
            filePath={activeTab.path}
            scrollToLine={activeTab.scrollToLine}
          />
        )}
        {activeTab.type === "assessment" && assessmentReport && (
          <AssessmentDashboard
            report={assessmentReport}
            onFileSelect={onFileSelect}
          />
        )}
        {activeTab.type === "assessment" && !assessmentReport && (
          <div className="p-6 text-om-muted text-sm">
            No assessment data available. Run an assessment first.
          </div>
        )}
        {activeTab.type === "execution" && latestExecution && (
          <JobTimeline result={latestExecution} />
        )}
        {activeTab.type === "execution" && !latestExecution && (
          <div className="p-6 text-om-muted text-sm">
            No execution data available. Run a JCL job first.
          </div>
        )}
      </div>
    </div>
  );
}
