import type { Config } from "tailwindcss";

const config: Config = {
  content: [
    "./src/app/**/*.{js,ts,jsx,tsx,mdx}",
    "./src/components/**/*.{js,ts,jsx,tsx,mdx}",
  ],
  theme: {
    extend: {
      colors: {
        "om-bg": "#1e1e2e",
        "om-surface": "#282a36",
        "om-border": "#44475a",
        "om-text": "#f8f8f2",
        "om-muted": "#6272a4",
        "om-accent": "#8be9fd",
        "om-success": "#50fa7b",
        "om-warning": "#f1fa8c",
        "om-error": "#ff5555",
        "om-info": "#bd93f9",
      },
    },
  },
  plugins: [],
};

export default config;
