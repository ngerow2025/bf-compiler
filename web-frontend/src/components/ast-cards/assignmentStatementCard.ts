import type { TreeNodeData } from "./types";

export const createAssignmentStatementCard = (target: number): TreeNodeData => ({
    title: "Assignment Statement",
    rows: [
        {
            label: "Target",
            value: target.toString(),
            options: { highlight: true },
        },
    ],
    config: {
        surface: "roseSoft",
        accent: "rose",
    },
});
