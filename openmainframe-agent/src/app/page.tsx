"use client";

import { CopilotSidebar } from "@copilotkit/react-ui";

export default function Home() {
  return (
    <CopilotSidebar
      defaultOpen={true}
      labels={{
        title: "OpenMainframe Agent",
        initial:
          "Hello! I'm your mainframe modernization assistant. I can help you assess, compile, execute, and explain COBOL and JCL code. Set a project directory to get started, or just ask me a question.",
      }}
    >
      <main className="flex h-screen bg-om-bg">
        {/* Workspace area — expanded in Batch 9 */}
        <div className="flex-1 flex items-center justify-center p-8">
          <div className="max-w-lg text-center">
            <h1 className="text-3xl font-bold text-om-text mb-4">
              OpenMainframe Agent
            </h1>
            <p className="text-om-muted mb-8">
              AI-powered mainframe modernization assistant. Use the chat panel
              to interact with your COBOL and JCL codebases.
            </p>
            <div className="grid grid-cols-3 gap-4">
              <div className="bg-om-surface border border-om-border rounded-lg p-4 text-center">
                <div className="text-2xl mb-2">📊</div>
                <div className="text-sm text-om-text font-medium">Assess</div>
                <div className="text-xs text-om-muted mt-1">
                  Scan a codebase
                </div>
              </div>
              <div className="bg-om-surface border border-om-border rounded-lg p-4 text-center">
                <div className="text-2xl mb-2">🔨</div>
                <div className="text-sm text-om-text font-medium">Compile</div>
                <div className="text-xs text-om-muted mt-1">
                  Build a program
                </div>
              </div>
              <div className="bg-om-surface border border-om-border rounded-lg p-4 text-center">
                <div className="text-2xl mb-2">📖</div>
                <div className="text-sm text-om-text font-medium">Explain</div>
                <div className="text-xs text-om-muted mt-1">
                  Understand legacy code
                </div>
              </div>
            </div>
          </div>
        </div>
      </main>
    </CopilotSidebar>
  );
}
