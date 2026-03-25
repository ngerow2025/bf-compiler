import type { TreeNodeData } from "./types";

export const createProgramCard = (functionCount: number): TreeNodeData => ({
    title: "Program",
    rows: [
        {
            label: "Functions",
            value: functionCount.toString(),
            options: { tone: "muted" },
        },
    ],
    config: {
        surface: "skySoft",
        accent: "sky",
    },
});
