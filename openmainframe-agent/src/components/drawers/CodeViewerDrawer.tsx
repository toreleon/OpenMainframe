"use client";

import { Drawer } from "./Drawer";
import { CodeViewer } from "@/components/workspace/CodeViewer";

interface CodeViewerDrawerProps {
  open: boolean;
  onClose: () => void;
  filePath: string;
  content?: string;
  scrollToLine?: number;
}

export function CodeViewerDrawer({
  open,
  onClose,
  filePath,
  content,
  scrollToLine,
}: CodeViewerDrawerProps) {
  const fileName = filePath.split("/").pop() || filePath;

  return (
    <Drawer open={open} onClose={onClose} title={fileName}>
      <CodeViewer filePath={filePath} content={content} scrollToLine={scrollToLine} />
    </Drawer>
  );
}
