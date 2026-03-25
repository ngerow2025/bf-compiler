import type { TreeNodeData } from "./types";

export const createFunctionCallCard = (qualifiedName: string, argCount: number): TreeNodeData => ({
    title: "Expression",
    subtitle: "Function Call",
    rows: [
        {
            label: "Function Name",
            value: qualifiedName,
            options: { tone: "accent" },
        },
        {
            label: "Arguments",
            value: argCount.toString(),
            options: { tone: "muted" },
        },
    ],
    config: {
        surface: "skySoft",
        accent: "sky",
    },
});
