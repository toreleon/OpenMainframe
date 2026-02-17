"use client";

import { useEffect, useRef } from "react";

interface CodeViewerProps {
  filePath: string;
  content?: string;
  scrollToLine?: number;
}

const COBOL_KEYWORDS = new Set([
  "IDENTIFICATION", "DIVISION", "PROGRAM-ID", "ENVIRONMENT", "DATA",
  "WORKING-STORAGE", "SECTION", "PROCEDURE", "PERFORM", "MOVE", "ADD",
  "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE", "IF", "ELSE", "END-IF",
  "EVALUATE", "WHEN", "END-EVALUATE", "DISPLAY", "ACCEPT", "STOP", "RUN",
  "GO", "TO", "CALL", "USING", "COPY", "REPLACING", "PIC", "PICTURE",
  "VALUE", "SPACES", "ZEROS", "ZERO", "HIGH-VALUES", "LOW-VALUES",
  "REDEFINES", "OCCURS", "TIMES", "INDEXED", "BY", "OPEN", "CLOSE",
  "READ", "WRITE", "REWRITE", "DELETE", "START", "STRING", "UNSTRING",
  "INSPECT", "TALLYING", "REPLACING", "SET", "INITIALIZE", "SEARCH",
  "EXEC", "SQL", "CICS", "END-EXEC", "SELECT", "ASSIGN", "ORGANIZATION",
  "ACCESS", "MODE", "SEQUENTIAL", "RANDOM", "DYNAMIC", "INDEXED",
  "RECORD", "KEY", "ALTERNATE", "FILE", "STATUS", "FD", "SD",
  "NOT", "AND", "OR", "THAN", "GREATER", "LESS", "EQUAL",
  "THRU", "THROUGH", "UNTIL", "VARYING", "FROM", "GIVING",
  "INTO", "WITH", "ON", "SIZE", "ERROR", "OVERFLOW",
]);

const JCL_KEYWORDS = new Set([
  "JOB", "EXEC", "DD", "PROC", "PEND", "SET", "IF", "THEN", "ELSE",
  "ENDIF", "INCLUDE", "JCLLIB", "ORDER", "OUTPUT",
  "DISP", "DSN", "DSNAME", "SPACE", "DCB", "UNIT", "VOL",
  "SYSOUT", "DUMMY", "PGM", "COND", "PARM", "CLASS", "MSGCLASS",
  "MSGLEVEL", "NOTIFY", "REGION", "TIME",
]);

function highlightLine(line: string, isJcl: boolean): JSX.Element[] {
  const elements: JSX.Element[] = [];
  const keywords = isJcl ? JCL_KEYWORDS : COBOL_KEYWORDS;

  // COBOL comment detection (column 7 = *)
  if (!isJcl && line.length > 6 && line[6] === "*") {
    return [<span key="c" className="text-om-muted italic">{line}</span>];
  }

  // JCL comment detection (//* )
  if (isJcl && line.startsWith("//*")) {
    return [<span key="c" className="text-om-muted italic">{line}</span>];
  }

  // Simple word-level highlighting
  const words = line.split(/(\s+)/);
  words.forEach((word, i) => {
    const upper = word.toUpperCase().replace(/[.,;()]*/g, "");
    if (keywords.has(upper)) {
      elements.push(
        <span key={i} className="text-om-accent font-bold">
          {word}
        </span>
      );
    } else if (/^['"].*['"]$/.test(word)) {
      elements.push(
        <span key={i} className="text-om-success">
          {word}
        </span>
      );
    } else if (/^\d+$/.test(word)) {
      elements.push(
        <span key={i} className="text-om-warning">
          {word}
        </span>
      );
    } else {
      elements.push(<span key={i}>{word}</span>);
    }
  });

  return elements;
}

export function CodeViewer({ filePath, content, scrollToLine }: CodeViewerProps) {
  const lineRef = useRef<HTMLDivElement>(null);
  const isJcl = /\.jcl$/i.test(filePath);
  const lines = (content || `// Content for ${filePath} will load from agent state`).split("\n");

  useEffect(() => {
    if (scrollToLine && lineRef.current) {
      lineRef.current.scrollIntoView({ behavior: "smooth", block: "center" });
    }
  }, [scrollToLine]);

  return (
    <div className="font-mono text-xs leading-5 bg-om-bg overflow-auto h-full">
      <div className="px-2 py-1 bg-om-surface border-b border-om-border text-om-muted text-[10px] sticky top-0">
        {filePath}
      </div>
      <div className="p-0">
        {lines.map((line, idx) => {
          const lineNum = idx + 1;
          const isTarget = lineNum === scrollToLine;
          return (
            <div
              key={idx}
              ref={isTarget ? lineRef : undefined}
              className={`flex hover:bg-om-border/20 ${
                isTarget ? "bg-om-accent/10" : ""
              }`}
            >
              <span className="w-12 text-right pr-3 text-om-muted select-none shrink-0 border-r border-om-border/50">
                {lineNum}
              </span>
              <pre className="pl-3 whitespace-pre overflow-x-auto">
                {highlightLine(line, isJcl)}
              </pre>
            </div>
          );
        })}
      </div>
    </div>
  );
}
