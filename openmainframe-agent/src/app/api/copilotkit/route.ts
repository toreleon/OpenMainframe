import {
  CopilotRuntime,
  ExperimentalEmptyAdapter,
  copilotRuntimeNextJSAppRouterEndpoint,
} from "@copilotkit/runtime";
import { LangGraphHttpAgent } from "@copilotkit/runtime/langgraph";
import { NextRequest } from "next/server";

const agentUrl = process.env.AGENT_URL || "http://localhost:8123";

const runtime = new CopilotRuntime({
  agents: {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    default: new LangGraphHttpAgent({
      url: agentUrl,
    }) as any,
  },
});

export const POST = async (req: NextRequest) => {
  const { handleRequest } = copilotRuntimeNextJSAppRouterEndpoint({
    endpoint: "/api/copilotkit",
    serviceAdapter: new ExperimentalEmptyAdapter(),
    runtime,
  });

  return handleRequest(req);
};
