import type { TreeNodeData } from "./types";

type FunctionParamData = {
    type_: {
        kind: string | { Str: number };
    };
    variable_index: number;
};

const formatParamType = (kind: string | { Str: number }): string => {
    if (typeof kind === "string") {
        return kind;
    }

    if ("Str" in kind) {
        return `str<${kind.Str}>`;
    }

    return "unknown";
};

export const createFunctionParametersCard = (
    params: FunctionParamData[],
    variableNameMapping: Map<string | number, string>,
): TreeNodeData => {
    const mappingLookup = variableNameMapping;

    const tableRows = params.map((param, index) => {
        const mappedName =
            mappingLookup.get(param.variable_index) ??
            mappingLookup.get(String(param.variable_index));
        const paramLabel = mappedName ?? `param_${index}`;
        return [
            paramLabel,
            formatParamType(param.type_.kind),
            String(param.variable_index),
        ];
    });

    return {
        title: "Function Params",
        subtitle: `${params.length} parameters`,
        table: {
            headers: ["Name", "Type", "Var Index"],
            rows: tableRows,
        },
        config: {
            surface: "indigoSoft",
            accent: "indigo",
        },
    };
};
