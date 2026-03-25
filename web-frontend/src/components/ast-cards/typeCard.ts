import type { ASTCardRow } from "../ASTCard";
import type { TreeNodeData } from "./types";

export const createTypeCard = (kind: string | { Str: number }): TreeNodeData => {
    const rows: ASTCardRow[] =
        typeof kind === "string"
            ? [{ label: "Kind", value: kind, options: { tone: "accent" } }]
            : [
                  { label: "Kind", value: "String", options: { tone: "accent" } },
                  {
                      label: "length",
                      value: kind.Str.toString(),
                      options: { tone: "muted" },
                  },
              ];

    return {
        title: "Type",
        rows,
        config: {
            surface: "amberSoft",
            accent: "amber",
        },
    };
};
