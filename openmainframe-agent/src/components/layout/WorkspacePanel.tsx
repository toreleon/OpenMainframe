"use client";

import type { WorkspaceTab } from "@/hooks/useWorkspaceTabs";
import { TabManager } from "@/components/workspace/TabManager";
import { CodeViewer } from "@/components/workspace/CodeViewer";
import { WelcomeScreen } from "@/components/workspace/WelcomeScreen";

interface WorkspacePanelProps {
  tabs: WorkspaceTab[];
  activeTab: WorkspaceTab;
  activeTabId: string;
  onTabSelect: (id: string) => void;
  onTabClose: (id: string) => void;
}

export function WorkspacePanel({
  tabs,
  activeTab,
  activeTabId,
  onTabSelect,
  onTabClose,
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
        {activeTab.type === "assessment" && (
          <div className="p-6 text-om-muted text-sm">
            Assessment dashboard — loaded in Batch 9b.
          </div>
        )}
        {activeTab.type === "execution" && (
          <div className="p-6 text-om-muted text-sm">
            Execution timeline — loaded in Batch 9b.
          </div>
        )}
      </div>
    </div>
  );
}
