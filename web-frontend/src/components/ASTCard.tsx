const joinClasses = (...classes: Array<string | undefined>) =>
    classes.filter(Boolean).join(" ");

type ASTCardTone = "default" | "accent" | "success" | "muted";
type ASTCardAccent =
    | "sky"
    | "emerald"
    | "amber"
    | "violet"
    | "rose"
    | "indigo"
    | "cyan"
    | "fuchsia";
type ASTCardSurface =
    | "light"
    | "dark"
    | "skySoft"
    | "emeraldSoft"
    | "amberSoft"
    | "violetSoft"
    | "roseSoft"
    | "indigoSoft"
    | "cyanSoft"
    | "fuchsiaSoft"
    | "slateSoft"
    | "zincSoft";

export interface ASTCardConfig {
    accent?: ASTCardAccent;
    surface?: ASTCardSurface;
}

export interface ASTCardRowOptions {
    tone?: ASTCardTone;
    highlight?: boolean;
}

export interface ASTCardRow {
    label: string;
    value: string;
    options?: ASTCardRowOptions;
}

export interface ASTCardTable {
    headers: string[];
    rows: string[][];
}

interface ASTCardProps {
    title?: string;
    subtitle?: string;
    rows?: ASTCardRow[];
    table?: ASTCardTable;
    config?: ASTCardConfig;
}

const ASTCard = ({
    title,
    subtitle,
    rows = [],
    table,
    config,
}: ASTCardProps) => {
    const accent = config?.accent ?? "sky";
    const surface = config?.surface ?? "light";
    const hasHeader = title !== undefined || subtitle !== undefined;
    const hasRows = rows.length > 0;
    const hasTable = table !== undefined;
    const showDivider = hasHeader && (hasRows || hasTable);

        const surfacePalette = (() => {
            switch (surface) {
                case "dark":
                    return {
                        cardBackground: "bg-slate-900",
                        cardBorder: "border-white/10",
                        baseText: "text-slate-100",
                        mutedText: "text-slate-400",
                        divider: "bg-white/10",
                        tableLine: "border-white/15",
                        successText: "text-lime-400",
                    };
                case "skySoft":
                    return {
                        cardBackground: "bg-sky-100",
                        cardBorder: "border-sky-300",
                        baseText: "text-sky-950",
                        mutedText: "text-sky-800",
                        divider: "bg-sky-300",
                        tableLine: "border-sky-300",
                        successText: "text-emerald-700",
                    };
                case "emeraldSoft":
                    return {
                        cardBackground: "bg-emerald-100",
                        cardBorder: "border-emerald-300",
                        baseText: "text-emerald-950",
                        mutedText: "text-emerald-800",
                        divider: "bg-emerald-300",
                        tableLine: "border-emerald-300",
                        successText: "text-emerald-700",
                    };
                case "amberSoft":
                    return {
                        cardBackground: "bg-amber-100",
                        cardBorder: "border-amber-300",
                        baseText: "text-amber-950",
                        mutedText: "text-amber-800",
                        divider: "bg-amber-300",
                        tableLine: "border-amber-300",
                        successText: "text-emerald-700",
                    };
                case "violetSoft":
                    return {
                        cardBackground: "bg-violet-100",
                        cardBorder: "border-violet-300",
                        baseText: "text-violet-950",
                        mutedText: "text-violet-800",
                        divider: "bg-violet-300",
                        tableLine: "border-violet-300",
                        successText: "text-emerald-700",
                    };
                case "roseSoft":
                    return {
                        cardBackground: "bg-rose-100",
                        cardBorder: "border-rose-300",
                        baseText: "text-rose-950",
                        mutedText: "text-rose-800",
                        divider: "bg-rose-300",
                        tableLine: "border-rose-300",
                        successText: "text-emerald-700",
                    };
                case "indigoSoft":
                    return {
                        cardBackground: "bg-indigo-100",
                        cardBorder: "border-indigo-300",
                        baseText: "text-indigo-950",
                        mutedText: "text-indigo-800",
                        divider: "bg-indigo-300",
                        tableLine: "border-indigo-300",
                        successText: "text-emerald-700",
                    };
                case "cyanSoft":
                    return {
                        cardBackground: "bg-cyan-100",
                        cardBorder: "border-cyan-300",
                        baseText: "text-cyan-950",
                        mutedText: "text-cyan-800",
                        divider: "bg-cyan-300",
                        tableLine: "border-cyan-300",
                        successText: "text-emerald-700",
                    };
                case "fuchsiaSoft":
                    return {
                        cardBackground: "bg-fuchsia-100",
                        cardBorder: "border-fuchsia-300",
                        baseText: "text-fuchsia-950",
                        mutedText: "text-fuchsia-800",
                        divider: "bg-fuchsia-300",
                        tableLine: "border-fuchsia-300",
                        successText: "text-emerald-700",
                    };
                case "slateSoft":
                    return {
                        cardBackground: "bg-slate-200",
                        cardBorder: "border-slate-400",
                        baseText: "text-slate-900",
                        mutedText: "text-slate-700",
                        divider: "bg-slate-400",
                        tableLine: "border-slate-400",
                        successText: "text-emerald-700",
                    };
                case "zincSoft":
                    return {
                        cardBackground: "bg-zinc-200",
                        cardBorder: "border-zinc-400",
                        baseText: "text-zinc-900",
                        mutedText: "text-zinc-700",
                        divider: "bg-zinc-400",
                        tableLine: "border-zinc-400",
                        successText: "text-emerald-700",
                    };
                case "light":
                default:
                    return {
                        cardBackground: "bg-white",
                        cardBorder: "border-black/15",
                        baseText: "text-slate-900",
                        mutedText: "text-slate-500",
                        divider: "bg-black/15",
                        tableLine: "border-black/15",
                        successText: "text-lime-700",
                    };
            }
        })();

        const accentPalette = (() => {
            switch (accent) {
                case "emerald":
                    return {
                        accentText: "text-emerald-800",
                        highlightBackground: "bg-emerald-200",
                        highlightText: "text-emerald-900",
                    };
                case "amber":
                    return {
                        accentText: "text-amber-800",
                        highlightBackground: "bg-amber-200",
                        highlightText: "text-amber-900",
                    };
                case "violet":
                    return {
                        accentText: "text-violet-800",
                        highlightBackground: "bg-violet-200",
                        highlightText: "text-violet-900",
                    };
                case "rose":
                    return {
                        accentText: "text-rose-800",
                        highlightBackground: "bg-rose-200",
                        highlightText: "text-rose-900",
                    };
                case "indigo":
                    return {
                        accentText: "text-indigo-800",
                        highlightBackground: "bg-indigo-200",
                        highlightText: "text-indigo-900",
                    };
                case "cyan":
                    return {
                        accentText: "text-cyan-800",
                        highlightBackground: "bg-cyan-200",
                        highlightText: "text-cyan-900",
                    };
                case "fuchsia":
                    return {
                        accentText: "text-fuchsia-800",
                        highlightBackground: "bg-fuchsia-200",
                        highlightText: "text-fuchsia-900",
                    };
                case "sky":
                default:
                    return {
                        accentText: "text-sky-800",
                        highlightBackground: "bg-sky-200",
                        highlightText: "text-sky-900",
                    };
            }
        })();

    return (
        <div
            className={joinClasses(
                "relative ast-card-root min-w-70 rounded-xl border px-6 py-5 font-mono shadow-sm",
                surfacePalette.cardBackground,
                surfacePalette.cardBorder,
                surfacePalette.baseText,
            )}
        >
            {hasHeader ? (
                <div className={joinClasses(showDivider ? "mb-3.5" : "mb-0", "text-center")}>
                    {title !== undefined ? (
                        <p
                            className={joinClasses(
                                subtitle !== undefined ? "mb-0.5" : "mb-0",
                                "text-[15px] font-medium",
                                surfacePalette.baseText,
                            )}
                        >
                            {title}
                        </p>
                    ) : null}
                    {subtitle !== undefined ? (
                        <p className={joinClasses("m-0 text-xs", surfacePalette.mutedText)}>
                            {subtitle}
                        </p>
                    ) : null}
                </div>
            ) : null}

            {showDivider ? (
                <div className={joinClasses("mb-3.5 h-px", surfacePalette.divider)}></div>
            ) : null}
            {table ? (
                <table className="w-full border-collapse text-[12px]">
                    <thead>
                        <tr>
                            {table.headers.map((header, headerIndex) => (
                                <th
                                    key={`${header}-${headerIndex}`}
                                    className={joinClasses(
                                        headerIndex === 0
                                            ? "px-2 py-1 text-left text-[11px] uppercase tracking-[0.06em]"
                                            : "border-l px-2 py-1 text-right text-[11px] uppercase tracking-[0.06em]",
                                        headerIndex === 0
                                            ? undefined
                                            : surfacePalette.tableLine,
                                        surfacePalette.mutedText,
                                    )}
                                >
                                    {header}
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {table.rows.map((row, index) => (
                            <tr key={`${row[0] ?? "row"}-${index}`}>
                                {row.map((cell, cellIndex) => (
                                    <td
                                        key={`${index}-${cellIndex}`}
                                        className={joinClasses(
                                            cellIndex === 0
                                                ? "border-t px-2 py-1.5 text-left"
                                                : "border-t border-l px-2 py-1.5 text-right",
                                            surfacePalette.tableLine,
                                            cellIndex === 0
                                                ? surfacePalette.baseText
                                                : surfacePalette.mutedText,
                                        )}
                                    >
                                        {cell}
                                    </td>
                                ))}
                            </tr>
                        ))}
                    </tbody>
                </table>
            ) : (
                <div className="flex flex-col gap-2 text-[13px]">
                    {rows.map((row, index) => (
                        <div key={index} className="flex items-center justify-between gap-8">
                            <span
                                className={joinClasses(
                                    "text-[11px] uppercase tracking-[0.06em]",
                                    surfacePalette.mutedText,
                                )}
                            >
                                {row.label}
                            </span>
                            <span
                                className={joinClasses(
                                    row.options?.highlight
                                        ? "rounded-lg px-2.5 py-0.5"
                                        : undefined,
                                    row.options?.highlight
                                        ? accentPalette.highlightBackground
                                        : undefined,
                                    row.options?.highlight
                                        ? accentPalette.highlightText
                                        : row.options?.tone === "accent"
                                          ? accentPalette.accentText
                                          : row.options?.tone === "success"
                                            ? surfacePalette.successText
                                            : row.options?.tone === "muted"
                                              ? surfacePalette.mutedText
                                              : surfacePalette.baseText,
                                )}
                            >
                                {row.value}
                            </span>
                        </div>
                    ))}
                </div>
            )}
        </div>
    );
};

export default ASTCard;
