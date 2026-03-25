import type { TreeNodeData } from "./types";

export const createIntegerLiteralCard = (value: string): TreeNodeData => ({
    title: "Expression",
    subtitle: "Integer Literal",
    rows: [{ label: "Value", value, options: { tone: "success" } }],
    config: {
        surface: "cyanSoft",
        accent: "cyan",
    },
});
