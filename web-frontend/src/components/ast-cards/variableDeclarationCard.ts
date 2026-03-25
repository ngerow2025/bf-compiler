import type { TreeNodeData } from "./types";

export const createVariableDeclarationCard = (
    name: string,
    variableIndex: number,
): TreeNodeData => ({
    title: "Variable Declaration",
    rows: [
        { label: "Name", value: name, options: { tone: "accent" } },
        {
            label: "Variable Index",
            value: variableIndex.toString(),
            options: { tone: "muted" },
        },
    ],
    config: {
        surface: "emeraldSoft",
        accent: "emerald",
    },
});
