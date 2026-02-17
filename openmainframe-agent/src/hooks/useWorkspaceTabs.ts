"use client";

import { useState, useCallback } from "react";

export type TabType = "welcome" | "code" | "assessment" | "execution";

export interface WorkspaceTab {
  id: string;
  type: TabType;
  label: string;
  path?: string;
  scrollToLine?: number;
}

const MAX_TABS = 8;

export function useWorkspaceTabs() {
  const [tabs, setTabs] = useState<WorkspaceTab[]>([
    { id: "welcome", type: "welcome", label: "Welcome" },
  ]);
  const [activeTabId, setActiveTabId] = useState("welcome");

  const openTab = useCallback(
    (tab: Omit<WorkspaceTab, "id">) => {
      setTabs((prev) => {
        // Check for duplicate (same type + path)
        const existing = prev.find(
          (t) => t.type === tab.type && t.path === tab.path
        );
        if (existing) {
          setActiveTabId(existing.id);
          return prev;
        }

        const id = `${tab.type}-${Date.now()}`;
        const newTab: WorkspaceTab = { ...tab, id };

        // Enforce max tabs — remove oldest non-welcome tab
        let updated = [...prev, newTab];
        if (updated.length > MAX_TABS) {
          const removeIdx = updated.findIndex((t) => t.type !== "welcome");
          if (removeIdx >= 0) {
            updated.splice(removeIdx, 1);
          }
        }

        setActiveTabId(id);
        return updated;
      });
    },
    []
  );

  const closeTab = useCallback(
    (tabId: string) => {
      setTabs((prev) => {
        const filtered = prev.filter((t) => t.id !== tabId);
        if (filtered.length === 0) {
          const welcome: WorkspaceTab = {
            id: "welcome",
            type: "welcome",
            label: "Welcome",
          };
          setActiveTabId("welcome");
          return [welcome];
        }
        if (activeTabId === tabId) {
          setActiveTabId(filtered[filtered.length - 1].id);
        }
        return filtered;
      });
    },
    [activeTabId]
  );

  const activeTab = tabs.find((t) => t.id === activeTabId) || tabs[0];

  return { tabs, activeTab, activeTabId, setActiveTabId, openTab, closeTab };
}
