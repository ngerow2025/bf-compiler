import type { TreeNodeData } from "./types";

export const createArrayAccessCard = (arrayVar: number): TreeNodeData => ({
    title: "Expression",
    subtitle: "Array Access",
    rows: [{ label: "Array Var", value: arrayVar.toString(), options: { tone: "muted" } }],
    config: {
        surface: "emeraldSoft",
        accent: "emerald",
    },
});
