import type { TreeNodeData } from "./types";

export const createVariableNameMappingCard = (
    mapping: Map<string | number, string>,
): TreeNodeData => {
    const mappingEntries = Array.from(mapping.entries());
    const tableRows = mappingEntries.map(([index, name]) => [
        name,
        index,
    ] as [string, string]);

    return {
        title: "Variable Mapping",
        subtitle: `${mappingEntries.length} variables`,
        table: {
            headers: ["Name", "Index"],
            rows: tableRows,
        },
        config: {
            surface: "zincSoft",
            accent: "indigo",
        },
    };
};
