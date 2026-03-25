import type { TreeNodeData } from "./types";

export const createStringLiteralCard = (value: string): TreeNodeData => ({
    title: "Expression",
    subtitle: "String Literal",
    rows: [{ label: "Value", value, options: { highlight: true } }],
    config: {
        surface: "fuchsiaSoft",
        accent: "fuchsia",
    },
});
