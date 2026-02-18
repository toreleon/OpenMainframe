"use client";

import { useState, useEffect, useCallback } from "react";

interface ConnectionStatus {
  connected: boolean;
  projectPath: string | null;
  cliVersion: string | null;
  loading: boolean;
}

const POLL_INTERVAL = 5000; // 5 seconds

export function useConnectionStatus(): ConnectionStatus {
  const [status, setStatus] = useState<ConnectionStatus>({
    connected: false,
    projectPath: null,
    cliVersion: null,
    loading: true,
  });

  const poll = useCallback(async () => {
    try {
      const agentUrl = process.env.NEXT_PUBLIC_AGENT_URL || "http://localhost:8123";
      const res = await fetch(`${agentUrl}/health`, { signal: AbortSignal.timeout(3000) });
      if (!res.ok) throw new Error("unhealthy");
      const data = await res.json();
      setStatus({
        connected: data.bridge_connected === true,
        projectPath: data.project_path || null,
        cliVersion: data.cli_version || null,
        loading: false,
      });
    } catch {
      setStatus((prev) => ({ ...prev, connected: false, loading: false }));
    }
  }, []);

  useEffect(() => {
    poll();
    const id = setInterval(poll, POLL_INTERVAL);
    return () => clearInterval(id);
  }, [poll]);

  return status;
}
