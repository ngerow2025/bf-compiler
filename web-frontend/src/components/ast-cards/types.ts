import type { ASTCardConfig, ASTCardRow, ASTCardTable } from "../ASTCard";

export interface TreeNodeData {
    title: string;
    subtitle?: string;
    rows?: ASTCardRow[];
    table?: ASTCardTable;
    config?: ASTCardConfig;
    leftCompanion?: TreeNodeData;
    rightCompanion?: TreeNodeData;
}
