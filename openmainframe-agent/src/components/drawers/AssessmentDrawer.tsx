"use client";

import type { AssessmentReport } from "@/lib/types";
import { Drawer } from "./Drawer";
import { AssessmentDashboard } from "@/components/workspace/AssessmentDashboard";

interface AssessmentDrawerProps {
  open: boolean;
  onClose: () => void;
  report: AssessmentReport;
}

export function AssessmentDrawer({ open, onClose, report }: AssessmentDrawerProps) {
  return (
    <Drawer open={open} onClose={onClose} title="Assessment Report">
      <AssessmentDashboard report={report} onFileSelect={() => {}} />
    </Drawer>
  );
}
