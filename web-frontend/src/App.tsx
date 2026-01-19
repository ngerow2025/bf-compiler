import { useState, useEffect } from "react";
import { Panel, Group } from "react-resizable-panels";
import { Code2, Cpu, Network } from "lucide-react";
import {
    tokenize_json,
    compile_steps_json,
    init
} from "./wasm/compiler_bf_target.js";

interface TokenInfo {
    kind: string;
    value?: string;
    span_start: number;
    span_len: number;
}

interface CompilationSteps {
    tokens: TokenInfo[];
    ast_debug: string;
    // typed_debug: string;
    // ir_by_function: Array<{ function_id: number; lines: string[] }>;
    // ir2_by_function: Array<{ function_id: number; lines: string[] }>;
    // ucode_by_function: Array<{ function_id: number; lines: string[] }>;
    // bf_program: string;
}

const CompilerExplorer = () => {
    const [sourceCode, setSourceCode] = useState(`fn main() {
  let x: u8 = 65u8;
  std::out(x);
}`);
    const [tokens, setTokens] = useState<TokenInfo[]>([]);
    const [compilationSteps, setCompilationSteps] =
        useState<CompilationSteps | null>(null);
    const [error, setError] = useState<string>("");
    const [hoverSpan, setHoverSpan] = useState<{ start: number; len: number } | null>(null);

    useEffect(() => {
        init();
        compileSource(sourceCode);
    }, []);

    const compileSource = (source: string) => {
        try {
            setError("");
            const tokensJson = tokenize_json(source);
            const parsedTokens = JSON.parse(tokensJson);
            setTokens(parsedTokens);

            const stepsJson = compile_steps_json(source);
            const steps = JSON.parse(stepsJson);
            setCompilationSteps(steps);
        } catch (e: any) {
            console.log(e)
            setError(e.toString());
            setTokens([]);
            setCompilationSteps(null);
        }
    };

    const handleSourceChange = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
        const newSource = e.target.value;
        setSourceCode(newSource);
        compileSource(newSource);
    };

    const getTokensByLine = () => {
        const lines: TokenInfo[][] = [];
        let currentPos = 0;
        let currentLine = 0;

        sourceCode.split('\n').forEach((line, lineIndex) => {
            lines[lineIndex] = [];
            const lineStart = currentPos;
            const lineEnd = currentPos + line.length;

            tokens.forEach((token) => {
                if (token.span_start >= lineStart && token.span_start < lineEnd) {
                    lines[lineIndex].push(token);
                }
            });

            currentPos = lineEnd + 1; // +1 for newline character
        });

        return lines;
    };

    const tokensByLine = getTokensByLine();

    const renderHighlightedSource = () => {
        if (!hoverSpan) return sourceCode;
        const start = hoverSpan.start;
        const end = hoverSpan.start + hoverSpan.len;
        const before = sourceCode.slice(0, start);
        const mid = sourceCode.slice(start, end);
        const after = sourceCode.slice(end);
        return (
            <>
                {before}
                <span className="bg-emerald-800/60 text-emerald-50">{mid || " "}</span>
                {after}
            </>
        );
    };

    return (
        <div className="h-screen w-screen flex flex-col font-sans">
            {/* Header */}
            <header className="h-12 border-b border-white/10 flex items-center px-4 bg-[#252526] justify-between">
                <h1 className="font-bold text-sm tracking-tight flex items-center gap-2">
                    <Cpu size={18} className="text-blue-400" />
                    BF COMPILER EXPLORER{" "}
                    <span className="text-white/30 font-light">v0.1.0</span>
                </h1>
                {error && <span className="text-xs text-red-400">{error}</span>}
            </header>

            {/* Resizable Layout */}
            <main className="flex-1 overflow-hidden">
                <Group orientation="horizontal">
                    {/* Pane 1: Source Editor */}
                    <Panel defaultSize={33} minSize={20}>
                        <div className="h-full flex flex-col bg-[#1e1e1e]">
                            <div className="flex items-center gap-2 px-3 py-2 bg-white/5 border-b border-white/5 text-xs font-medium uppercase text-white/50">
                                <Code2 size={14} /> Source Code
                            </div>
                            <div className="relative flex-1 overflow-hidden">
                                <pre
                                    className="absolute inset-0 p-4 font-mono text-sm leading-[27px] text-blue-300 whitespace-pre-wrap pointer-events-none"
                                    aria-hidden="true"
                                >
                                    {renderHighlightedSource()}
                                </pre>
                                <textarea
                                    wrap="off"
                                    className="relative h-full w-full p-4 font-mono text-sm leading-[27px] outline-none overflow-y-auto overflow-x-hidden bg-transparent text-transparent caret-blue-300 resize-none"
                                    value={sourceCode}
                                    onChange={handleSourceChange}
                                    spellCheck={false}
                                    onMouseLeave={() => setHoverSpan(null)}
                                />
                            </div>
                        </div>
                    </Panel>

                    {/* Pane 2: Token Stream */}
                    <Panel defaultSize={33} minSize={20}>
                        <div className="h-full flex flex-col bg-[#252526] border-x border-white/5">
                            <div className="flex items-center gap-2 px-3 py-2 bg-white/5 border-b border-white/5 text-xs font-medium uppercase text-white/50">
                                <Cpu size={14} /> Tokens ({tokens.length})
                            </div>
                            <div className="flex-1 overflow-auto font-mono text-sm">
                                <div className="p-4 space-y-0 inline-block min-w-full">
                                    {tokensByLine.map((lineTokens, lineIndex) => (
                                        <div key={lineIndex} className="flex gap-2 items-center whitespace-nowrap" style={{ height: '27px' }}>
                                            <span className="text-white/30 text-xs w-6 flex-shrink-0 text-right">
                                                {lineIndex + 1}
                                            </span>
                                            <div className="flex gap-1">
                                                {lineTokens.map((token, idx) => (
                                                    <TokenBadge
                                                        key={idx}
                                                        type={token.kind}
                                                        value={token.value}
                                                        color={getTokenColor(token.kind)}
                                                        onHover={() => setHoverSpan({ start: token.span_start, len: token.span_len })}
                                                        onLeave={() => setHoverSpan(null)}
                                                    />
                                                ))}
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </Panel>

                    {/* Pane 3: Compilation Output */}
                    <Panel defaultSize={34} minSize={20}>
                        <div className="h-full flex flex-col bg-[#1e1e1e]">
                            <div className="flex items-center gap-2 px-3 py-2 bg-white/5 border-b border-white/5 text-xs font-medium uppercase text-white/50">
                                <Network size={14} /> Compilation Output
                            </div>
                            <div className="flex-1 p-4 overflow-auto font-mono text-xs">
                                {compilationSteps && (
                                    <div className="space-y-4">
                                        <div>
                                            <div className="text-green-400 font-bold mb-1">
                                                AST
                                            </div>
                                            <pre className="text-slate-300 whitespace-pre-wrap text-[10px]">
                                                {compilationSteps.ast_debug}
                                                ...
                                            </pre>
                                        </div>

                                        
                                    </div>
                                )}
                            </div>
                        </div>
                    </Panel>
                </Group>
            </main>
        </div>
    );
};

// UI Helper Components
const getTokenColor = (kind: string): string => {
    // Keywords
    if (kind === "Fn" || kind === "Let" || kind === "Return" || kind === "If" || kind === "Else" || kind === "While") return "text-purple-400";
    // Types
    if (kind.startsWith("Type")) return "text-cyan-400";
    // Identifiers
    if (kind === "Identifier") return "text-blue-300";
    // Literals
    if (kind === "IntLiteral") return "text-green-400";
    if (kind === "StringLiteral") return "text-orange-300";
    // Operators
    if (kind === "Plus" || kind === "Minus" || kind === "Star" || kind === "Slash" || kind === "Percent") return "text-pink-400";
    if (kind === "Eq" || kind === "Ne" || kind === "Lt" || kind === "Le" || kind === "Gt" || kind === "Ge") return "text-pink-400";
    if (kind === "Assign") return "text-pink-300";
    // Punctuation
    if (kind === "LParen" || kind === "RParen" || kind === "LBrace" || kind === "RBrace" || kind === "LBracket" || kind === "RBracket") return "text-yellow-400";
    if (kind === "Semicolon" || kind === "Comma" || kind === "Colon" || kind === "DoubleColon" || kind === "Arrow") return "text-slate-500";
    // Default
    return "text-slate-400";
};

const TokenBadge = ({
    type,
    value,
    color,
    onHover,
    onLeave,
}: {
    type: string;
    value?: string;
    color: string;
    onHover?: () => void;
    onLeave?: () => void;
}) => (
    <div
        className="inline-flex items-center gap-1.5 bg-white/5 px-1.5 py-0.5 rounded text-[11px] font-mono border border-transparent hover:border-blue-500/50 cursor-default"
        onMouseEnter={onHover}
        onMouseLeave={onLeave}
    >
        <span className={color}>{type}</span>
        {value && <span className="text-white/60">"{value}"</span>}
    </div>
);

export default CompilerExplorer;
