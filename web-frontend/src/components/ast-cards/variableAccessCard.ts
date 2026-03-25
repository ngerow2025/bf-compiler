import type { TreeNodeData } from "./types";

export const createVariableAccessCard = (id: number): TreeNodeData => ({
    title: "Expression",
    subtitle: "Variable Access",
    rows: [{ label: "Name", value: id.toString(), options: { tone: "accent" } }],
    config: {
        surface: "indigoSoft",
        accent: "indigo",
    },
});
