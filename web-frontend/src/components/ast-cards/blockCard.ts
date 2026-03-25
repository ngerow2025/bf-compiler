import type { TreeNodeData } from "./types";

export const createBlockCard = (statementCount: number): TreeNodeData => ({
    title: "Block",
    rows: [
        { label: "Statements", value: statementCount.toString(), options: { tone: "muted" } },
    ],
    config: {
        surface: "slateSoft",
        accent: "indigo",
    },
});
