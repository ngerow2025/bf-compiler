import type { TreeNodeData } from "./types";

export const createFunctionCard = (name: string, id: number, paramCount: number): TreeNodeData => ({
    title: "Function",
    rows: [
        { label: "Name", value: name, options: { tone: "accent" } },
        { label: "id", value: id.toString(), options: { tone: "muted" } },
        { label: "Params", value: paramCount.toString(), options: { tone: "muted" } },
    ],
    config: {
        surface: "violetSoft",
        accent: "violet",
    },
});
