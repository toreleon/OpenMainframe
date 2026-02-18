import type { Config } from "tailwindcss";

const config: Config = {
  content: [
    "./src/app/**/*.{js,ts,jsx,tsx,mdx}",
    "./src/components/**/*.{js,ts,jsx,tsx,mdx}",
  ],
  theme: {
    extend: {
      fontFamily: {
        mono: [
          "var(--font-mono)",
          "JetBrains Mono",
          "Fira Code",
          "ui-monospace",
          "SFMono-Regular",
          "Menlo",
          "Monaco",
          "Consolas",
          "monospace",
        ],
      },
      colors: {
        "om-bg": "#0d1117",
        "om-surface": "#161b22",
        "om-border": "#30363d",
        "om-text": "#e6edf3",
        "om-muted": "#7d8590",
        "om-accent": "#58a6ff",
        "om-success": "#3fb950",
        "om-warning": "#d29922",
        "om-error": "#f85149",
        "om-info": "#a371f7",
      },
    },
  },
  plugins: [],
};

export default config;
