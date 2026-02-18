"use client";

import dynamic from "next/dynamic";

const HomeContent = dynamic(() => import("@/components/HomeContent"), {
  ssr: false,
  loading: () => (
    <main className="flex items-center justify-center h-screen bg-om-bg">
      <div className="text-om-muted text-sm">Loading OpenMainframe Agent...</div>
    </main>
  ),
});

export default function Home() {
  return <HomeContent />;
}
