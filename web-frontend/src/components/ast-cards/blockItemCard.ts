import type { TreeNodeData } from "./types";

export const createBlockItemCard = (isBlockBody: boolean): TreeNodeData => ({
    title: "BlockItem",
    subtitle: isBlockBody ? "Block Body" : "Statement",
    config: {
        surface: "zincSoft",
        accent: "cyan",
    },
});
