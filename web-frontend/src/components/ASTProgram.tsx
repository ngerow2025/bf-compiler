import type {
    AstAssignmentStmt,
    AstBlockBody,
    AstBlockItem,
    AstExpressionStmt,
    AstExpressionValue,
    AstProgramPayload,
    AstVarDeclStmt,
    IntLiteralValue,
} from "../App";
import { TransformComponent, TransformWrapper } from "react-zoom-pan-pinch";
import ASTCard from "./ASTCard"
import { createArrayAccessCard } from "./ast-cards/arrayAccessCard";
import { createAssignmentStatementCard } from "./ast-cards/assignmentStatementCard";
import { createBlockCard } from "./ast-cards/blockCard";
import { createBlockItemCard } from "./ast-cards/blockItemCard";
import { createExpressionStatementCard } from "./ast-cards/expressionStatementCard";
import { createFunctionCallCard } from "./ast-cards/functionCallCard";
import { createFunctionCard } from "./ast-cards/functionCard";
import { createFunctionParametersCard } from "./ast-cards/functionParametersCard";
import { createIntegerLiteralCard } from "./ast-cards/integerLiteralCard";
import { createProgramCard } from "./ast-cards/programCard";
import { createStringLiteralCard } from "./ast-cards/stringLiteralCard";
import { createTypeCard } from "./ast-cards/typeCard";
import type { TreeNodeData } from "./ast-cards/types";
import { createVariableAccessCard } from "./ast-cards/variableAccessCard";
import { createVariableDeclarationCard } from "./ast-cards/variableDeclarationCard";
import { createVariableNameMappingCard } from "./ast-cards/variableNameMappingCard";

interface ASTProgramProps {
    ast: AstProgramPayload
    rootNodeId?: string
}

interface SubTreeData {
    nodeData: TreeNodeData
    children: SubTreeData[]
}

interface ReactPinchPanZoomControls {
    zoomIn: () => void
    zoomOut: () => void
    resetTransform: () => void
}

interface ReactPinchPanZoomProps {
    children: (controls: ReactPinchPanZoomControls) => any
}

const ReactPinchPanZoom = ({ children }: ReactPinchPanZoomProps) => (
    <TransformWrapper
        initialScale={1}
        minScale={0.35}
        maxScale={3}
        centerOnInit
        limitToBounds={false}
        wheel={{ step: 0.12 }}
        pinch={{ step: 5 }}
        doubleClick={{ disabled: true }}
    >
        {({ zoomIn, zoomOut, resetTransform }: ReactPinchPanZoomControls) =>
            children({ zoomIn, zoomOut, resetTransform })}
    </TransformWrapper>
)

const ConnectorRow = () => (
    <div className="h-14 grid place-items-center">
        <div className="pointer-events-none left-1/2 z-0 h-full w-px bg-slate-400/70"></div>
    </div>
)

interface TreeNodeProps {
    nodeData: TreeNodeData
    bottomChildren: SubTreeData[]
}

const SubTree = ({ nodeData, bottomChildren }: TreeNodeProps) => (
    <div>
        <div className="relative grid ast-node-header" style={{ gridTemplateColumns: '1fr auto 1fr' }}>
            {nodeData.leftCompanion && (
                <div className="pr-6 flex items-start justify-end ast-companion-slot">
                    <ASTCard
                        title={nodeData.leftCompanion.title}
                        subtitle={nodeData.leftCompanion.subtitle}
                        rows={nodeData.leftCompanion.rows ?? []}
                        table={nodeData.leftCompanion.table}
                        config={nodeData.leftCompanion.config}
                    />
                </div>
            )}
            <div className="col-start-2 flex h-full flex-col items-center ast-main-slot">
                <div className="relative ast-main-card-wrap">
                    {nodeData.leftCompanion && (
                        <div className="pointer-events-none h-px bg-slate-400/70 ast-companion-link"></div>
                    )}
                    {nodeData.rightCompanion && (
                        <div className="pointer-events-none h-px bg-slate-400/70 ast-right-companion-link"></div>
                    )}
                    <ASTCard
                        title={nodeData.title}
                        subtitle={nodeData.subtitle}
                        rows={nodeData.rows ?? []}
                        table={nodeData.table}
                        config={nodeData.config}
                    />
                </div>
                {bottomChildren.length > 0 && (
                    <div className="pointer-events-none w-px flex-1 bg-slate-400/70 ast-main-vertical-extension"></div>
                )}
            </div>
            {nodeData.rightCompanion && (
                <div className="col-start-3 pl-6 flex items-start justify-start ast-right-companion-slot">
                    <ASTCard
                        title={nodeData.rightCompanion.title}
                        subtitle={nodeData.rightCompanion.subtitle}
                        rows={nodeData.rightCompanion.rows ?? []}
                        table={nodeData.rightCompanion.table}
                        config={nodeData.rightCompanion.config}
                    />
                </div>
            )}
        </div>
        {bottomChildren.length > 0 && (
            <>
                <ConnectorRow />
                <div className="flex items-start justify-center gap-6 px-4 container-custom">
                    {bottomChildren.map((child, index) => (
                        <div key={index} className="relative grid place-items-center child-custom">
                            <div className="pointer-events-none h-px bg-slate-400/70 horizontal-arrow"></div>
                            <div className="pointer-events-none h-6 w-px bg-slate-400/70"></div>
                            <SubTree nodeData={child.nodeData} bottomChildren={child.children} />
                        </div>
                    ))}
                </div>
            </>
        )}
    </div>
)

function translateAstProgramToTreeData(programAst: AstProgramPayload): SubTreeData {
    return {
        nodeData: createProgramCard(programAst.length),
        children: programAst.map((func) => {
            const functionCard = createFunctionCard(func.name, func.id, func.params.length);
            const variableNameMapping = func.variable_name_mapping;
            functionCard.leftCompanion = createVariableNameMappingCard(
                variableNameMapping,
            );
            functionCard.rightCompanion = createFunctionParametersCard(
                func.params,
                variableNameMapping,
            );
            return {
                nodeData: functionCard,
                children: [translateAstBlockToTreeData(func.body)]
            }
        })

    }
}

function formatIntLiteralValue(value: IntLiteralValue): string {
    const entries = Object.entries(value);
    if (entries.length === 0) {
        return "unknown";
    }

    const [variant, raw] = entries[0];
    return `${variant}(${String(raw)})`;
}

function translateAstBlockToTreeData(blockAst: AstBlockBody): SubTreeData {
    return {
        nodeData: createBlockCard(blockAst.statements.length),
        children: blockAst.statements.map(translateAstBlockItemToTreeData)
    }
}

function translateAstBlockItemToTreeData(blockAst: AstBlockItem): SubTreeData {
    return {
        nodeData: createBlockItemCard("Block" in blockAst),
        children: ["Block" in blockAst ? translateAstBlockToTreeData(blockAst.Block) : translateAstStatementToTreeData(blockAst.Statement)]
    }
}

function translateAstStatementToTreeData(data: | { VarDecl: AstVarDeclStmt; }
          | { Expression: AstExpressionStmt; }
          | { Assignment: AstAssignmentStmt; }): SubTreeData {
    if ("VarDecl" in data) {
        return {
            nodeData: createVariableDeclarationCard(data.VarDecl.name, data.VarDecl.variable_index),
            children: [
                translateAstExpressionToTreeData(data.VarDecl.value),
                translateAstTypeToTreeData(data.VarDecl.type_),
            ]
        }
    } else if ("Expression" in data) {
        return {
            nodeData: createExpressionStatementCard(),
            children: [
                translateAstExpressionToTreeData(data.Expression.expr)
            ]
        }
    } else {
        return {
            nodeData: createAssignmentStatementCard(data.Assignment.var),
            children: [
                translateAstExpressionToTreeData(data.Assignment.value)
            ]
        }
    }
}

function translateAstExpressionToTreeData(expr: AstExpressionValue): SubTreeData {
    if ("IntLiteral" in expr) {
        return {
            nodeData: createIntegerLiteralCard(formatIntLiteralValue(expr.IntLiteral.value)),
            children: []
        }
    } else if("StringLiteral" in expr) {
        return {
            nodeData: createStringLiteralCard(expr.StringLiteral.value),
            children: []
        }
    } else if("VariableAccess" in expr) {
        return {
            nodeData: createVariableAccessCard(expr.VariableAccess.value.id),
            children: []
        }
    } else if("ArrayAccess" in expr) {
        return {
            nodeData: createArrayAccessCard(expr.ArrayAccess.array.id),
            children: [
                translateAstExpressionToTreeData(expr.ArrayAccess.index_expr)
            ]
        }

    } else if("FnCall" in expr) {
        return {
            nodeData: createFunctionCallCard(expr.FnCall.qualified_name.parts.join("::"), expr.FnCall.arguments.length),
            children: expr.FnCall.arguments.map(translateAstExpressionToTreeData)
        }

    } else {
        throw new Error("Unknown expression type: " + JSON.stringify(expr))
    }
}

function translateAstTypeToTreeData(type: {kind: string | { Str: number; }; annotation: unknown}) {
    return {
        nodeData: createTypeCard(type.kind),
        children: []
    }
}

const ASTProgram = ({ ast, rootNodeId }: ASTProgramProps) => {
    const treeData = translateAstProgramToTreeData(ast)

    return (
        <div className="h-full w-full py-2">
            <ReactPinchPanZoom>
                {({ zoomIn, zoomOut, resetTransform }) => (
                    <div className="flex h-full w-full flex-col">
                        <div className="mb-2 flex items-center gap-2 px-3">
                            <button
                                type="button"
                                onClick={() => zoomOut()}
                                className="rounded border border-slate-600/70 bg-slate-800 px-2 py-1 text-xs text-slate-200 hover:bg-slate-700"
                            >
                                -
                            </button>
                            <button
                                type="button"
                                onClick={() => resetTransform()}
                                className="rounded border border-slate-600/70 bg-slate-800 px-2 py-1 text-xs text-slate-200 hover:bg-slate-700"
                            >
                                Reset
                            </button>
                            <button
                                type="button"
                                onClick={() => zoomIn()}
                                className="rounded border border-slate-600/70 bg-slate-800 px-2 py-1 text-xs text-slate-200 hover:bg-slate-700"
                            >
                                +
                            </button>
                            <span className="text-xs text-slate-400">
                                Scroll or pinch to zoom, drag to pan
                            </span>
                        </div>
                        <div className="min-h-0 flex-1 overflow-hidden">
                            <TransformComponent
                                wrapperStyle={{ width: "100%", height: "100%" }}
                                contentStyle={{ width: "max-content", height: "max-content" }}
                            >
                                <div className="mx-auto w-max px-8 pb-6">
                                    <div id={rootNodeId}>
                                        <SubTree nodeData={treeData.nodeData} bottomChildren={treeData.children} />
                                    </div>
                                </div>
                            </TransformComponent>
                        </div>
                    </div>
                )}
            </ReactPinchPanZoom>
        </div>
    )
}

export default ASTProgram