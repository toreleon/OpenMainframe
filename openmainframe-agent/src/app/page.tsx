"use client";

import dynamic from "next/dynamic";

const HomeContent = dynamic(() => import("@/components/HomeContent"), {
  ssr: false,
  loading: () => (
    <main className="flex items-center justify-center h-screen bg-om-bg font-mono">
      <div className="text-om-muted text-xs flex items-center gap-2">
        <span className="terminal-spinner" />
        <span>loading agent...</span>
      </div>
    </main>
  ),
});

export default function Home() {
  return <HomeContent />;
}
