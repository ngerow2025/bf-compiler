import { useState, useEffect, useCallback, useMemo, useRef } from "react";
import { Panel, Group } from "react-resizable-panels";
import { Code2, Cpu, Network } from "lucide-react";
import Editor, { type OnMount } from "@monaco-editor/react";
import type * as monacoEditor from "monaco-editor";
import * as bf_compiler from "./wasm/compiler_bf_target.js";
import ProgramSVG from "./AST-svgs.tsx";

// ============================================================================
// TYPES
// ============================================================================
interface TokenInfo {
    kind: string;
    value?: string;
    span_start: number;
    span_len: number;
}

interface CompilationSteps {
    tokens: TokenInfo[];
    ast_debug: string;
}

interface HoverSpan {
    start: number;
    len: number;
}

// ============================================================================
// CONSTANTS
// ============================================================================

const DEFAULT_SOURCE_CODE = `fn main() {
  let x: u8 = 65u8;
  std::out(x);
}`;


const APP_VERSION = "v0.1.0";
const APP_TITLE = "BF COMPILER EXPLORER";

// ============================================================================
// TOKEN COLOR MAPPING
// ============================================================================

const TOKEN_COLOR_MAP: Record<string, string> = {
    // Keywords
    Fn: "text-purple-400",
    Let: "text-purple-400",
    Return: "text-purple-400",
    If: "text-purple-400",
    Else: "text-purple-400",
    While: "text-purple-400",
    // Types
    IntLiteral: "text-green-400",
    StringLiteral: "text-orange-300",
    // Identifiers
    Identifier: "text-blue-300",
    // Arithmetic operators
    Plus: "text-pink-400",
    Minus: "text-pink-400",
    Star: "text-pink-400",
    Slash: "text-pink-400",
    Percent: "text-pink-400",
    // Comparison operators
    Eq: "text-pink-400",
    Ne: "text-pink-400",
    Lt: "text-pink-400",
    Le: "text-pink-400",
    Gt: "text-pink-400",
    Ge: "text-pink-400",
    Assign: "text-pink-300",
    // Brackets/Braces
    LParen: "text-yellow-400",
    RParen: "text-yellow-400",
    LBrace: "text-yellow-400",
    RBrace: "text-yellow-400",
    LBracket: "text-yellow-400",
    RBracket: "text-yellow-400",
    // Punctuation
    Semicolon: "text-slate-500",
    Comma: "text-slate-500",
    Colon: "text-slate-500",
    DoubleColon: "text-slate-500",
    Arrow: "text-slate-500",
};

const getTokenColor = (kind: string): string => {
    // Check for type keywords
    if (kind.startsWith("Type")) return "text-cyan-400";
    return TOKEN_COLOR_MAP[kind] ?? "text-slate-400";
};

const useCompiler = (initialSource: string) => {
    const [sourceCode, setSourceCode] = useState(initialSource);

    // Initialize WASM once
    useEffect(() => {
        bf_compiler.init();
    }, []);

    // Stage 1: Tokenization - only runs when sourceCode changes
    const tokenCompilation = useMemo(() => {
        try {
            const tokens = bf_compiler.tokenize(sourceCode);
            return { tokens, tokenizationError: null };
        } catch (e: unknown) {
            const errorMsg = e instanceof Error ? e.message : String(e);
            console.error("Tokenization error:", e);
            return { tokens: null, tokenizationError: errorMsg };
        }
    }, [sourceCode]);

    // Stage 2: AST Parsing - only runs when tokens change
    const astCompilation = useMemo(() => {
        if (!tokenCompilation.tokens) {
            return { ast: null, parsingError: null };
        }
        try {
            const ast = bf_compiler.parse(tokenCompilation.tokens, sourceCode);
            return { ast, parsingError: null };
        } catch (e: unknown) {
            const errorMsg = e instanceof Error ? e.message : String(e);
            console.error("Parsing error:", e);
            return { ast: null, parsingError: errorMsg };
        }
    }, [tokenCompilation.tokens, sourceCode]);

    const handleSourceChange = useCallback((newSource: string) => {
        setSourceCode(newSource);
    }, []);

    return {
        sourceCode,
        tokens: tokenCompilation.tokens,
        ast: astCompilation.ast,
        tokenizationError: tokenCompilation.tokenizationError,
        parsingError: astCompilation.parsingError,
        handleSourceChange,
    };
};

// ============================================================================
// UI COMPONENTS
// ============================================================================

/**
 * Token Badge Component - displays individual tokens with syntax highlighting
 */
interface TokenBadgeProps {
    type: string;
    value?: string;
    color: string;
    onHover?: () => void;
    onLeave?: () => void;
}

const TokenBadge = ({ type, value, color, onHover, onLeave }: TokenBadgeProps) => (
    <div
        className="inline-flex items-center gap-1.5 bg-white/5 px-1.5 py-0.5 rounded text-[11px] font-mono border border-transparent hover:border-blue-500/50 cursor-default transition-colors"
        onMouseEnter={onHover}
        onMouseLeave={onLeave}
    >
        <span className={color}>{type}</span>
        {value && <span className="text-white/60">"{value}"</span>}
    </div>
);

/**
 * Source Editor Panel - displays source code with Monaco Editor
 */
interface SourceEditorProps {
    sourceCode: string;
    onChange: (value: string) => void;
    onEditorMount: OnMount;
}

const SourceEditor = ({ sourceCode, onChange, onEditorMount }: SourceEditorProps) => (
    <Panel defaultSize={33} minSize={20}>
        <div className="h-full flex flex-col bg-[#1e1e1e]">
            <div className="flex items-center gap-2 px-3 py-2 bg-white/5 border-b border-white/5 text-xs font-medium uppercase text-white/50">
                <Code2 size={14} /> Source Code
            </div>
            <div className="flex-1 overflow-hidden">
                <Editor
                    height="100%"
                    defaultLanguage="rust"
                    theme="vs-dark"
                    value={sourceCode}
                    onChange={(value) => onChange(value ?? "")}
                    onMount={onEditorMount}
                    options={{
                        minimap: { enabled: false },
                        fontSize: 14,
                        lineHeight: 27,
                        lineNumbers: "on",
                        scrollBeyondLastLine: false,
                        automaticLayout: true,
                        tabSize: 2,
                        wordWrap: "off",
                        fontFamily: "'Fira Code', 'Cascadia Code', Consolas, monospace",
                        fontLigatures: true,
                        padding: { top: 16 },
                    }}
                />
            </div>
        </div>
    </Panel>
);

/**
 * Token Stream Panel - displays tokenized code
 */
interface TokenStreamProps {
    tokens: TokenInfo[] | null;
    tokensByLine: TokenInfo[][] | null;
    onTokenHover: (span: HoverSpan | null) => void;
    scrollRef: React.RefObject<HTMLDivElement | null>;
    lineHeight: number;
    isCompiling: boolean;
    error: string | null;
}

const TokenStream = ({ tokens, tokensByLine, onTokenHover, scrollRef, lineHeight, isCompiling, error }: TokenStreamProps) => (
    <Panel defaultSize={33} minSize={20}>
        <div className="h-full flex flex-col bg-[#252526] border-x border-white/5">
            <div className="flex items-center gap-2 px-3 py-2 bg-white/5 border-b border-white/5 text-xs font-medium uppercase text-white/50">
                <Cpu size={14} /> Tokens ({tokens?.length ?? 0})
            </div>
            <div ref={scrollRef} className="flex-1 overflow-auto font-mono text-sm">
                {error && (
                    <div className="p-4 text-red-400 text-xs">
                        <div className="font-bold mb-1">Tokenization Error</div>
                        <div className="whitespace-pre-wrap">{error}</div>
                    </div>
                )}
                {isCompiling && !tokens && (
                    <div className="p-4 space-y-2">
                        {[...Array(5)].map((_, i) => (
                            <div key={i} className="h-6 bg-white/5 rounded animate-pulse" />
                        ))}
                    </div>
                )}
                {tokens && tokensByLine && (
                    <div className="p-4 space-y-0 inline-block min-w-full">
                        {tokensByLine.map((lineTokens, lineIndex) => (
                            <div
                                key={lineIndex}
                                className="flex gap-2 items-center whitespace-nowrap"
                                style={{ height: `${lineHeight}px` }}
                            >
                                <span className="text-white/30 text-xs w-6 shrink-0 text-right">
                                    {lineIndex + 1}
                                </span>
                                <div className="flex gap-1">
                                    {lineTokens.map((token, idx) => (
                                        <TokenBadge
                                            key={idx}
                                            type={token.kind}
                                            value={token.value}
                                            color={getTokenColor(token.kind)}
                                            onHover={() =>
                                                onTokenHover({
                                                    start: token.span_start,
                                                    len: token.span_len,
                                                })
                                            }
                                            onLeave={() => onTokenHover(null)}
                                        />
                                    ))}
                                </div>
                            </div>
                        ))}
                    </div>
                )}
            </div>
        </div>
    </Panel>
);

/**
 * Compilation Output Panel - displays AST and other compilation output
 */
interface CompilationOutputProps {
    compilationSteps: CompilationSteps | null;
    isCompiling: boolean;
    error: string | null;
}

const CompilationOutput = ({ compilationSteps, isCompiling, error }: CompilationOutputProps) => (
    <Panel defaultSize={34} minSize={20}>
        <div className="h-full flex flex-col bg-[#1e1e1e]">
            <div className="flex items-center gap-2 px-3 py-2 bg-white/5 border-b border-white/5 text-xs font-medium uppercase text-white/50">
                <Network size={14} /> Compilation Output
            </div>
            <div className="flex-1 p-4 overflow-auto font-mono text-xs">
                {error && (
                    <div className="text-red-400 text-xs">
                        <div className="font-bold mb-1">Parsing Error</div>
                        <div className="whitespace-pre-wrap">{error}</div>
                    </div>
                )}
                {isCompiling && !compilationSteps && (
                    <div className="space-y-2">
                        {[...Array(6)].map((_, i) => (
                            <div key={i} className="h-4 bg-white/5 rounded animate-pulse" />
                        ))}
                    </div>
                )}
                {compilationSteps && (
                        <ProgramSVG />
                )}
            </div>
        </div>
    </Panel>
);

// ============================================================================
// MAIN COMPONENT
// ============================================================================

/**
 * Main Compiler Explorer Application
 */
const CompilerExplorer = () => {
    const { sourceCode, tokens, ast, tokenizationError, parsingError, handleSourceChange } =
        useCompiler(DEFAULT_SOURCE_CODE);
    const [hoverSpan, setHoverSpan] = useState<HoverSpan | null>(null);
    const [lineHeight, setLineHeight] = useState<number>(27);

    // Refs for Monaco editor and scroll synchronization
    const editorRef = useRef<monacoEditor.editor.IStandaloneCodeEditor | null>(null);
    const tokenStreamRef = useRef<HTMLDivElement | null>(null);
    const decorationsRef = useRef<monacoEditor.editor.IEditorDecorationsCollection | null>(null);

    // Determine if we're in a compiling state (no tokens yet means still compiling)
    const isCompiling = tokens === null && tokenizationError === null;

    // Handle Monaco editor mount
    const handleEditorMount: OnMount = useCallback((editor, monaco) => {
        editorRef.current = editor;
        
        // Get line height from editor options
        const editorLineHeight = editor.getOption(monaco.editor.EditorOption.lineHeight);
        setLineHeight(editorLineHeight);
        
        // Set up scroll synchronization
        editor.onDidScrollChange((e) => {
            if (tokenStreamRef.current) {
                tokenStreamRef.current.scrollTop = e.scrollTop;
            }
        });
    }, []);

    // Sync token stream scroll when tokens change
    useEffect(() => {
        if (editorRef.current && tokenStreamRef.current) {
            const editorScrollTop = editorRef.current.getScrollTop();
            tokenStreamRef.current.scrollTop = editorScrollTop;
        }
    }, [tokens]);

    // Update decorations when hoverSpan changes
    useEffect(() => {
        if (!editorRef.current) return;
        
        if (decorationsRef.current) {
            decorationsRef.current.clear();
        }

        if (hoverSpan) {
            const model = editorRef.current.getModel();
            if (model) {
                const startPos = model.getPositionAt(hoverSpan.start);
                const endPos = model.getPositionAt(hoverSpan.start + hoverSpan.len);
                
                decorationsRef.current = editorRef.current.createDecorationsCollection([
                    {
                        range: {
                            startLineNumber: startPos.lineNumber,
                            startColumn: startPos.column,
                            endLineNumber: endPos.lineNumber,
                            endColumn: endPos.column,
                        },
                        options: {
                            className: "monaco-highlight-span",
                            inlineClassName: "monaco-highlight-span-inline",
                        },
                    },
                ]);
            }
        }
    }, [hoverSpan]);

    // Convert WasmTokens to TokenInfo[] array for easier manipulation
    const tokensArray = useMemo(() => {
        if (!tokens) return null;
        const arr: TokenInfo[] = [];
        for (let i = 0; i < tokens.len; i++) {
            const token = tokens.get(i);
            if (token) arr.push(token);
        }
        return arr;
    }, [tokens]);

    // Get tokens organized by line
    const tokensByLine = useMemo(() => {
        const lines: TokenInfo[][] = [];
        let currentPos = 0;

        if (!tokensArray) {
            return null;
        }

        sourceCode.split("\n").forEach((line, lineIndex) => {
            lines[lineIndex] = [];
            const lineStart = currentPos;
            const lineEnd = currentPos + line.length;

            tokensArray.forEach((token) => {
                if (token.span_start >= lineStart && token.span_start < lineEnd) {
                    lines[lineIndex].push(token);
                }
            });

            currentPos = lineEnd + 1; // +1 for newline character
        });

        return lines;
    }, [sourceCode, tokensArray]);

    // Create compilation steps object with AST debug output
    const compilationSteps = useMemo(() => {
        if (!ast || !tokensArray) return null;
        return {
            tokens: tokensArray,
            ast_debug: ast.debug_string(),
        };
    }, [ast, tokensArray]);

    return (
        <div className="h-screen w-screen flex flex-col font-sans">
            {/* Header */}
            <header className="h-12 border-b border-white/10 flex items-center px-4 bg-[#252526] justify-between">
                <h1 className="font-bold text-sm tracking-tight flex items-center gap-2">
                    <Cpu size={18} className="text-blue-400" />
                    {APP_TITLE}{" "}
                    <span className="text-white/30 font-light">{APP_VERSION}</span>
                </h1>
            </header>

            {/* Resizable Layout */}
            <main className="flex-1 overflow-hidden">
                <Group orientation="horizontal">
                    <SourceEditor
                        sourceCode={sourceCode}
                        onChange={handleSourceChange}
                        onEditorMount={handleEditorMount}
                    />
                    <TokenStream
                        tokens={tokensArray}
                        tokensByLine={tokensByLine}
                        onTokenHover={setHoverSpan}
                        scrollRef={tokenStreamRef}
                        lineHeight={lineHeight}
                        isCompiling={isCompiling}
                        error={tokenizationError}
                    />
                    <CompilationOutput
                        compilationSteps={compilationSteps}
                        isCompiling={ast === null && parsingError === null}
                        error={parsingError}
                    />
                </Group>
            </main>
        </div>
    );
};

export default CompilerExplorer;
