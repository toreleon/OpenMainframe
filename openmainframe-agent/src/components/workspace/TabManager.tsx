"use client";

import type { WorkspaceTab } from "@/hooks/useWorkspaceTabs";

interface TabManagerProps {
  tabs: WorkspaceTab[];
  activeTabId: string;
  onTabSelect: (id: string) => void;
  onTabClose: (id: string) => void;
}

export function TabManager({
  tabs,
  activeTabId,
  onTabSelect,
  onTabClose,
}: TabManagerProps) {
  if (tabs.length <= 1 && tabs[0]?.type === "welcome") {
    return null;
  }

  return (
    <div className="flex bg-om-surface border-b border-om-border overflow-x-auto shrink-0">
      {tabs.map((tab) => {
        const isActive = tab.id === activeTabId;
        return (
          <div
            key={tab.id}
            className={`flex items-center gap-1.5 px-3 py-1.5 text-xs cursor-pointer border-r border-om-border shrink-0 transition-colors ${
              isActive
                ? "bg-om-bg text-om-text border-b-2 border-b-om-accent"
                : "text-om-muted hover:text-om-text hover:bg-om-border/20"
            }`}
            onClick={() => onTabSelect(tab.id)}
          >
            <span className="truncate max-w-[120px]">{tab.label}</span>
            {tab.type !== "welcome" && (
              <button
                onClick={(e) => {
                  e.stopPropagation();
                  onTabClose(tab.id);
                }}
                className="ml-1 text-om-muted hover:text-om-error transition-colors"
              >
                x
              </button>
            )}
          </div>
        );
      })}
    </div>
  );
}
