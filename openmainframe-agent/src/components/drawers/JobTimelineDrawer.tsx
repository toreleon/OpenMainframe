"use client";

import type { ExecutionResult } from "@/lib/types";
import { Drawer } from "./Drawer";
import { JobTimeline } from "@/components/workspace/JobTimeline";

interface JobTimelineDrawerProps {
  open: boolean;
  onClose: () => void;
  result: ExecutionResult;
}

export function JobTimelineDrawer({ open, onClose, result }: JobTimelineDrawerProps) {
  const jobName = result.jcl_file.split("/").pop() || result.jcl_file;

  return (
    <Drawer open={open} onClose={onClose} title={`Job: ${jobName}`}>
      <JobTimeline result={result} />
    </Drawer>
  );
}
