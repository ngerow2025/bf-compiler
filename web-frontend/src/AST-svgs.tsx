import type { JSX } from "react/jsx-dev-runtime";
import type {
    AstBlockBody,
    AstBlockItem,
    AstExpressionValue,
    AstFunctionParam,
    AstFunctionPayload,
    AstProgramPayload,
    AstVarDeclStmt,
    AstVariableAccessValue,
    IntLiteralValue,
    QualifiedName,
    SimpleAnnotation,
} from "./App";

const ProgramSVG = () => {
    let programCard = ProgramCard();
    return (
        <svg
            width="100%"
            height="100%"
            viewBox="0 0 320 420"
            xmlns="http://www.w3.org/2000/svg"
            // style={{backgroundColor}}
        >
            <programCard.svg />

            <Curve
                from={programCard.bottom_middle()}
                to={programCard.top_left()}
            />
            {/* <g transform={`translate(${programCard.x_offset + programCard.width/2}, ${programCard.y_offset + programCard.height})`}>
                <circle cx="0" cy="0" r="5" style={{fill: "red"}} />
                <path d="M 0 0 Q 0 20, -20 20" stroke="black" fill="transparent" strokeWidth={5} />
            </g> */}
        </svg>
    );
};

const createCardTemplate = (
    title: string,
    subtitle: string,
    dataRows?: string[],
    color: string = "#1e293b",
) => {
    const height = 120 + (dataRows?.length ?? 0) * 25;
    const svg = () => (
        <svg
            width="320"
            height={height + 40}
            viewBox={`0 0 320 ${height + 40}`}
            xmlns="http://www.w3.org/2000/svg"
        >
            <defs>
                <linearGradient
                    id={`cardGradient-${title}`}
                    x1="0"
                    y1="0"
                    x2="0"
                    y2="1"
                >
                    <stop offset="0%" stopColor={color} />
                    <stop
                        offset="100%"
                        stopColor={
                            parseInt(color.slice(1), 16) - 0x0f172a > 0
                                ? "#0f172a"
                                : "#000814"
                        }
                    />
                </linearGradient>
                <filter
                    id="shadow"
                    x="-20%"
                    y="-20%"
                    width="140%"
                    height="140%"
                >
                    <feDropShadow
                        dx="0"
                        dy="8"
                        stdDeviation="16"
                        floodColor="#000"
                        floodOpacity="0.35"
                    />
                </filter>
            </defs>

            <rect
                x="20"
                y="20"
                width="280"
                height={height}
                rx="16"
                fill={`url(#cardGradient-${title})`}
                filter="url(#shadow)"
            />

            <text
                x="160"
                y="55"
                textAnchor="middle"
                fontSize="28"
                fontWeight="700"
                fill="#e5e7eb"
                fontFamily="Inter, system-ui, sans-serif"
            >
                {title}
            </text>

            <line
                x1="40"
                y1="70"
                x2="300"
                y2="70"
                stroke="#334155"
                strokeWidth="1"
            />

            <text
                x="160"
                y="100"
                textAnchor="middle"
                fontSize="14"
                fontWeight="600"
                fill="#cbd5e1"
                fontFamily="Inter, system-ui, sans-serif"
            >
                {subtitle}
            </text>

            {dataRows &&
                dataRows.map((row, index) => (
                    <g key={index}>
                        <text
                            x="40"
                            y={125 + index * 25}
                            fontSize="12"
                            fontWeight="500"
                            fill="#a1a5b8"
                            fontFamily="monospace"
                        >
                            {row}
                        </text>
                    </g>
                ))}
        </svg>
    );

    return new SizedCard(280, height, 20, 20, svg);
};

interface CurveProps {
    from: [number, number];
    to: [number, number];
    strokeWidth?: number;
}

const Curve = (props: CurveProps) => {
    // Handle horizontal alignment (no vertical component needed)
    if (props.to[0] === props.from[0]) {
        return (
            <StraitLine
                from={props.from}
                to={props.to}
                strokeWidth={props.strokeWidth}
            />
        );
    }

    //do the horizontal component first then the vertical component
    //first the from virtical to horizontal quarter circle curve
    let isRight = props.to[0] > props.from[0];
    let isDown = props.to[1] > props.from[1];

    let firstCurveStart = props.from;
    let firstCurveEnd: [number, number] = [
        props.from[0] + (isRight ? 20 : -20),
        props.from[1] + (isDown ? 20 : -20),
    ];
    let firstCurvePivot: [number, number] = [
        props.from[0],
        props.from[1] + (isDown ? 20 : -20),
    ];

    let horizontalComponentStart = firstCurveEnd;
    let horizontalComponentEnd: [number, number] = [
        props.to[0] - (isRight ? 20 : -20),
        firstCurveEnd[1],
    ];

    let secondCurveStart = horizontalComponentEnd;
    let secondCurveEnd: [number, number] = [
        secondCurveStart[0] + (isRight ? 20 : -20),
        secondCurveStart[1] + (isDown ? 20 : -20),
    ];
    let secondCurvePivot: [number, number] = [
        secondCurveStart[0] + (isRight ? 20 : -20),
        secondCurveStart[1],
    ];

    let verticalComponentStart = secondCurveEnd;
    let verticalComponentEnd = props.to;

    return (
        <g>
            <CurveCorner
                from={firstCurveStart}
                to={firstCurveEnd}
                pivot={firstCurvePivot}
                strokeWidth={props.strokeWidth}
            />
            <StraitLine
                from={horizontalComponentStart}
                to={horizontalComponentEnd}
                strokeWidth={props.strokeWidth}
            />
            <CurveCorner
                from={secondCurveStart}
                to={secondCurveEnd}
                pivot={secondCurvePivot}
                strokeWidth={props.strokeWidth}
            />
            <StraitLine
                from={verticalComponentStart}
                to={verticalComponentEnd}
                strokeWidth={props.strokeWidth}
            />
        </g>
    );
};

interface CurveCornerProps {
    from: [number, number];
    to: [number, number];
    pivot: [number, number];
    strokeWidth?: number;
}

const CurveCorner = (props: CurveCornerProps) => {
    return (
        <path
            d={`M ${props.from[0]} ${props.from[1]} Q ${props.pivot[0]} ${props.pivot[1]}, ${props.to[0]} ${props.to[1]}`}
            stroke="black"
            fill="transparent"
            strokeWidth={props.strokeWidth ?? 5}
        />
    );
};

interface StraitLineProps {
    from: [number, number];
    to: [number, number];
    strokeWidth?: number;
}

const StraitLine = (props: StraitLineProps) => {
    return (
        <line
            x1={props.from[0]}
            y1={props.from[1]}
            x2={props.to[0]}
            y2={props.to[1]}
            stroke="black"
            strokeWidth={props.strokeWidth ?? 5}
        />
    );
};

class SizedCard {
    width: number;
    height: number;
    x_offset: number;
    y_offset: number;
    svg: () => JSX.Element;
    constructor(
        width: number,
        height: number,
        x_offset: number,
        y_offset: number,
        svg: () => JSX.Element,
    ) {
        this.width = width;
        this.height = height;
        this.x_offset = x_offset;
        this.y_offset = y_offset;
        this.svg = svg;
    }

    top_left(): [number, number] {
        return [this.x_offset, this.y_offset];
    }

    top_middle(): [number, number] {
        return [this.x_offset + this.width / 2, this.y_offset];
    }

    top_right(): [number, number] {
        return [this.x_offset + this.width, this.y_offset];
    }

    middle_left(): [number, number] {
        return [this.x_offset, this.y_offset + this.height / 2];
    }

    middle(): [number, number] {
        return [
            this.x_offset + this.width / 2,
            this.y_offset + this.height / 2,
        ];
    }

    middle_right(): [number, number] {
        return [this.x_offset + this.width, this.y_offset + this.height / 2];
    }

    bottom_left(): [number, number] {
        return [this.x_offset, this.y_offset + this.height];
    }

    bottom_middle(): [number, number] {
        return [this.x_offset + this.width / 2, this.y_offset + this.height];
    }

    bottom_right(): [number, number] {
        return [this.x_offset + this.width, this.y_offset + this.height];
    }
}

// ============= Card Factory Functions =============

const ProgramCard = () => createCardTemplate("Program", "Functions");

const SpanCard = (offset: number, length: number) =>
    createCardTemplate(
        "Span",
        "Position Information",
        [`offset: ${offset}`, `length: ${length}`],
        "#1e3a5f",
    );

const IntLiteralCard = (value: Record<string, number>) => {
    const typeAndValue = Object.entries(value).map(
        ([type, val]) => `${type}: ${val}`,
    );
    return createCardTemplate(
        "IntLiteral",
        "Numeric Constant",
        typeAndValue,
        "#3d2817",
    );
};

const StringLiteralCard = (value: string) =>
    createCardTemplate(
        "StringLiteral",
        "String Constant",
        [`"${value.length > 30 ? value.slice(0, 30) + "..." : value}"`],
        "#1a3a2a",
    );

const VariableAccessCard = (id: number) =>
    createCardTemplate(
        "VariableAccess",
        "Variable Reference",
        [`id: ${id}`],
        "#2a1f3d",
    );

const ArrayAccessCard = (arrayId: number) =>
    createCardTemplate(
        "ArrayAccess",
        "Array Index Access",
        [`array: ${arrayId}`, "index: <expression>"],
        "#3a2a1d",
    );

const FnCallCard = (name: string, argCount: number) =>
    createCardTemplate(
        "FnCall",
        "Function Call",
        [`name: ${name}`, `args: ${argCount}`],
        "#2a3a1d",
    );

const VarDeclCard = (name: string, typeKind: string, variableIndex: number) =>
    createCardTemplate(
        "VarDecl",
        "Variable Declaration",
        [`name: ${name}`, `type: ${typeKind}`, `index: ${variableIndex}`],
        "#1f2a3f",
    );

const AssignmentCard = (varIndex: number) =>
    createCardTemplate(
        "Assignment",
        "Variable Assignment",
        [`var: ${varIndex}`],
        "#3a1f2a",
    );

const ExpressionStmtCard = () =>
    createCardTemplate(
        "ExpressionStmt",
        "Expression Statement",
        ["value: <expression>"],
        "#2a3a3a",
    );

const FunctionParamCard = (typeKind: string, variableIndex: number) =>
    createCardTemplate(
        "Parameter",
        "Function Parameter",
        [`type: ${typeKind}`, `index: ${variableIndex}`],
        "#1a3a2a",
    );

const BlockBodyCard = (statementCount: number) =>
    createCardTemplate(
        "Block",
        "Statement Block",
        [`statements: ${statementCount}`],
        "#1f1f2f",
    );

const FunctionCard = (name: string, paramCount: number, id: number) =>
    createCardTemplate(
        "Function",
        "Function Definition",
        [`name: ${name}`, `params: ${paramCount}`, `id: ${id}`],
        "#1a2a3f",
    );

// ============= Card Gallery Components =============

const ExpressionCardsSVG = () => {
    const intLiteralCard = IntLiteralCard({ I32: 42 });
    const stringLiteralCard = StringLiteralCard("Hello, World!");
    const varAccessCard = VariableAccessCard(1);
    const fnCallCard = FnCallCard("print", 1);

    return (
        <svg
            width="100%"
            height="100%"
            viewBox="0 0 1280 400"
            xmlns="http://www.w3.org/2000/svg"
        >
            <g transform="translate(0, 0)">
                <intLiteralCard.svg />
            </g>
            <g transform="translate(320, 0)">
                <stringLiteralCard.svg />
            </g>
            <g transform="translate(640, 0)">
                <varAccessCard.svg />
            </g>
            <g transform="translate(960, 0)">
                <fnCallCard.svg />
            </g>
        </svg>
    );
};

const StatementCardsSVG = () => {
    const varDeclCard = VarDeclCard("x", "I32", 0);
    const assignmentCard = AssignmentCard(0);
    const exprStmtCard = ExpressionStmtCard();

    return (
        <svg
            width="100%"
            height="100%"
            viewBox="0 0 960 400"
            xmlns="http://www.w3.org/2000/svg"
        >
            <g transform="translate(0, 0)">
                <varDeclCard.svg />
            </g>
            <g transform="translate(320, 0)">
                <assignmentCard.svg />
            </g>
            <g transform="translate(640, 0)">
                <exprStmtCard.svg />
            </g>
        </svg>
    );
};

const StructureCardsSVG = () => {
    const programCard = ProgramCard();
    const functionCard = FunctionCard("main", 2, 0);
    const blockCard = BlockBodyCard(3);
    const spanCard = SpanCard(10, 5);

    return (
        <svg
            width="100%"
            height="100%"
            viewBox="0 0 1280 400"
            xmlns="http://www.w3.org/2000/svg"
        >
            <g transform="translate(0, 0)">
                <programCard.svg />
            </g>
            <g transform="translate(320, 0)">
                <functionCard.svg />
            </g>
            <g transform="translate(640, 0)">
                <blockCard.svg />
            </g>
            <g transform="translate(960, 0)">
                <spanCard.svg />
            </g>
        </svg>
    );
};

// ============= Tree Layout System =============

interface TreeNode {
    id: string;
    title: string;
    subtitle: string;
    data?: string[];
    color: string;
    span?: { offset: number; length: number };
    children?: TreeNode[];
}

interface PositionedNode extends TreeNode {
    x: number;
    y: number;
    width: number;
    height: number;
}

const calculateTreeLayout = (
    root: TreeNode,
    _maxWidth: number = 2000,
): PositionedNode => {
    const HORIZONTAL_SPACING = 100;
    const ROW_SPACING = 40;

    const calculateHeight = (node: TreeNode): number => {
        const dataLines = node.data?.length ?? 0;
        return 120 + dataLines * 25;
    };

    const calculateWidth = (): number => 280;

    // First pass: collect all nodes by depth and calculate max height per depth
    const nodesByDepth: Map<number, TreeNode[]> = new Map();
    const maxHeightByDepth: Map<number, number> = new Map();

    const collectNodes = (node: TreeNode, depth: number = 0) => {
        if (!nodesByDepth.has(depth)) {
            nodesByDepth.set(depth, []);
        }
        nodesByDepth.get(depth)!.push(node);

        const height = calculateHeight(node);
        const currentMax = maxHeightByDepth.get(depth) ?? 0;
        maxHeightByDepth.set(depth, Math.max(currentMax, height));

        if (node.children) {
            node.children.forEach((child) => collectNodes(child, depth + 1));
        }
    };

    collectNodes(root);

    // Calculate cumulative Y positions for each depth
    const yPositionByDepth: Map<number, number> = new Map();
    let currentY = 100;
    for (let depth = 0; depth < nodesByDepth.size; depth++) {
        yPositionByDepth.set(depth, currentY);
        const maxHeightAtDepth = maxHeightByDepth.get(depth) ?? 0;
        currentY += maxHeightAtDepth + ROW_SPACING;
    }

    // Build x-order by depth using top-down recursion:
    // start at current layer, then add each node's children to the next layer list.
    const nodeOrderByDepth: Map<number, TreeNode[]> = new Map();

    const collectLayerOrder = (nodes: TreeNode[], depth: number = 0) => {
        if (nodes.length === 0) return;

        if (!nodeOrderByDepth.has(depth)) {
            nodeOrderByDepth.set(depth, []);
        }

        const currentLayer = nodeOrderByDepth.get(depth)!;
        currentLayer.push(...nodes);

        const nextLayer: TreeNode[] = [];
        nodes.forEach((node) => {
            if (node.children && node.children.length > 0) {
                nextLayer.push(...node.children);
            }
        });

        collectLayerOrder(nextLayer, depth + 1);
    };

    collectLayerOrder([root]);

    const xIndexByNodeId: Map<string, number> = new Map();
    const layerWidthByDepth: Map<number, number> = new Map();

    nodeOrderByDepth.forEach((nodes, depth) => {
        layerWidthByDepth.set(depth, nodes.length);
        nodes.forEach((node, index) => {
            xIndexByNodeId.set(node.id, index);
        });
    });

    const widthWithSpacing = calculateWidth() + HORIZONTAL_SPACING;
    const baseXForNode = (nodeId: string, depth: number): number => {
        const siblingIndex = xIndexByNodeId.get(nodeId) ?? 0;
        const totalSiblings = layerWidthByDepth.get(depth) ?? 1;
        return (siblingIndex - (totalSiblings - 1) / 2) * widthWithSpacing;
    };

    type Edge = {
        parentId: string;
        parentDepth: number;
        childId: string;
        childDepth: number;
    };
    const edgesByChildDepth: Map<number, Edge[]> = new Map();

    const collectEdges = (node: TreeNode, depth: number = 0) => {
        if (!node.children || node.children.length === 0) return;

        node.children.forEach((child) => {
            const childDepth = depth + 1;
            if (!edgesByChildDepth.has(childDepth)) {
                edgesByChildDepth.set(childDepth, []);
            }

            edgesByChildDepth.get(childDepth)!.push({
                parentId: node.id,
                parentDepth: depth,
                childId: child.id,
                childDepth,
            });

            collectEdges(child, childDepth);
        });
    };

    collectEdges(root);

    const rowShiftByDepth: Map<number, number> = new Map();
    rowShiftByDepth.set(0, 0);

    for (let depth = 1; depth < nodeOrderByDepth.size; depth++) {
        const edges = edgesByChildDepth.get(depth) ?? [];

        if (edges.length === 0) {
            rowShiftByDepth.set(depth, 0);
            continue;
        }

        const shiftNumerator = edges.reduce((sum, edge) => {
            const parentShift = rowShiftByDepth.get(edge.parentDepth) ?? 0;
            const parentX =
                baseXForNode(edge.parentId, edge.parentDepth) + parentShift;
            const childBaseX = baseXForNode(edge.childId, edge.childDepth);
            return sum + (parentX - childBaseX);
        }, 0);

        rowShiftByDepth.set(depth, shiftNumerator / edges.length);
    }

    // Second pass: position nodes by maintaining y-layers and using per-layer top-down x-order
    const positionNodes = (
        node: TreeNode,
        depth: number = 0,
    ): PositionedNode => {
        const width = calculateWidth();
        const height = calculateHeight(node);

        // Calculate x position based on top-down ordering + per-row shift (ordering preserved)
        const x =
            baseXForNode(node.id, depth) + (rowShiftByDepth.get(depth) ?? 0);

        // Bottom-align cards within each row using that depth's max card height
        const rowTop = yPositionByDepth.get(depth) ?? 0;
        const rowMaxHeight = maxHeightByDepth.get(depth) ?? height;
        const y = rowTop + (rowMaxHeight - height);

        const positioned: PositionedNode = {
            ...node,
            x,
            y,
            width,
            height,
        };

        // Position children if they exist
        if (node.children && node.children.length > 0) {
            positioned.children = node.children.map((child) =>
                positionNodes(child, depth + 1),
            );
        }

        return positioned;
    };

    return positionNodes(root);
};

const renderTreeNode = (
    node: PositionedNode,
    onNodeHover?: (span: { offset: number; length: number } | null) => void,
): JSX.Element => {
    const factory = () =>
        createCardTemplate(node.title, node.subtitle, node.data, node.color);
    const card = factory();

    return (
        <g
            key={node.id}
            transform={`translate(${node.x + 400}, ${node.y + 100})`}
            onMouseEnter={() => onNodeHover?.(node.span ?? null)}
            onMouseLeave={() => onNodeHover?.(null)}
            style={{ cursor: node.span ? "pointer" : "default" }}
        >
            <card.svg />
        </g>
    );
};

const renderTreeConnections = (
    node: PositionedNode,
    connections: JSX.Element[] = [],
): JSX.Element[] => {
    // Card offset: all cards have x_offset=20, y_offset=20 within their SVG viewBox
    const CARD_X_OFFSET = 20;
    const CARD_Y_OFFSET = 20;

    if (node.children && node.children.length > 0) {
        node.children.forEach((child) => {
            const posChild = child as PositionedNode;
            // Parent's bottom-middle anchor (accounting for card's internal rect position)
            const parentBottom = [
                node.x + 400 + CARD_X_OFFSET + node.width / 2,
                node.y + 100 + CARD_Y_OFFSET + node.height,
            ];
            // Child's top-middle anchor (accounting for card's internal rect position)
            const childTop = [
                posChild.x + 400 + CARD_X_OFFSET + posChild.width / 2,
                posChild.y + 100 + CARD_Y_OFFSET,
            ];

            connections.push(
                <Curve
                    key={`curve-${node.id}-${child.id}`}
                    from={parentBottom as [number, number]}
                    to={childTop as [number, number]}
                    strokeWidth={3}
                />,
            );

            renderTreeConnections(posChild, connections);
        });
    }
    return connections;
};

const renderTree = (
    node: PositionedNode,
    onNodeHover?: (span: { offset: number; length: number } | null) => void,
): { nodes: JSX.Element[]; connections: JSX.Element[] } => {
    const nodes: JSX.Element[] = [];

    const traverse = (n: PositionedNode) => {
        nodes.push(renderTreeNode(n, onNodeHover));
        if (n.children) {
            n.children.forEach((child) => traverse(child as PositionedNode));
        }
    };

    traverse(node);
    const conns = renderTreeConnections(node);

    return { nodes, connections: conns };
};

interface AstTreeVisualizationProps {
    payload: AstProgramPayload | null;
    onNodeHover?: (span: { offset: number; length: number } | null) => void;
}

interface TreeBounds {
    minX: number;
    minY: number;
    maxX: number;
    maxY: number;
}

const calculateTreeBounds = (root: PositionedNode): TreeBounds => {
    const CARD_TOTAL_WIDTH = 320;
    const CARD_VERTICAL_PADDING = 40;
    const TREE_X_OFFSET = 400;
    const TREE_Y_OFFSET = 100;

    let minX = Infinity;
    let minY = Infinity;
    let maxX = -Infinity;
    let maxY = -Infinity;

    const visit = (node: PositionedNode) => {
        const x = node.x + TREE_X_OFFSET;
        const y = node.y + TREE_Y_OFFSET;
        const width = CARD_TOTAL_WIDTH;
        const height = node.height + CARD_VERTICAL_PADDING;

        minX = Math.min(minX, x);
        minY = Math.min(minY, y);
        maxX = Math.max(maxX, x + width);
        maxY = Math.max(maxY, y + height);

        if (node.children) {
            node.children.forEach((child) => visit(child as PositionedNode));
        }
    };

    visit(root);

    return { minX, minY, maxX, maxY };
};

interface MakeNodeParams {
    title: string;
    subtitle: string;
    color: string;
    span?: { offset: number; length: number };
    data?: string[];
    children?: TreeNode[];
}

const buildTreeFromAST = (payload: AstProgramPayload | null): TreeNode | null => {
    if (!payload) return null;

    let idCounter = 0;
    const getNextId = () => `node-${idCounter++}`;

    const getSpanFromAnnotation = (
        value: { annotation?: SimpleAnnotation } | null | undefined,
    ): { offset: number; length: number } | undefined => {
        const span = value?.annotation?.span;
        if (
            span &&
            typeof span.offset === "number" &&
            typeof span.length === "number"
        ) {
            return { offset: span.offset, length: span.length };
        }
        return undefined;
    };

    const mergeSpans = (
        spans: Array<{ offset: number; length: number } | undefined>,
    ): { offset: number; length: number } | undefined => {
        const validSpans = spans.filter(
            (span): span is { offset: number; length: number } =>
                !!span && span.length >= 0,
        );

        if (validSpans.length === 0) return undefined;

        const start = Math.min(...validSpans.map((span) => span.offset));
        const end = Math.max(
            ...validSpans.map((span) => span.offset + span.length),
        );
        return { offset: start, length: Math.max(0, end - start) };
    };

    const makeNode = ({
        title,
        subtitle,
        color,
        span,
        data,
        children,
    }: MakeNodeParams): TreeNode => ({
        id: getNextId(),
        title,
        subtitle,
        color,
        data,
        span,
        children: children && children.length > 0 ? children : undefined,
    });

    const buildVariableIdNode = (rawId: unknown): TreeNode =>
        makeNode({
            title: "VariableId",
            subtitle: "struct VariableId(usize)",
            color: "#24324f",
            data: [`value: ${String(rawId)}`],
            span: undefined,
        });

    const buildFunctionIdNode = (rawId: unknown): TreeNode =>
        makeNode({
            title: "FunctionId",
            subtitle: "struct FunctionId(pub(crate) usize)",
            color: "#24324f",
            data: [`value: ${String(rawId)}`],
            span: undefined,
        });

    const buildAstTypeNode = (
        typeNode: AstVarDeclStmt["type_"] | AstFunctionParam["type_"],
    ): TreeNode => {
        const kind = typeNode?.kind;
        let kindText = "unknown";

        if (typeof kind === "string") {
            kindText = kind;
        } else if (kind && typeof kind === "object") {
            const entries = Object.entries(kind);
            if (entries.length > 0) {
                const [typeName, typeValue] = entries[0];
                kindText = `${typeName}${typeValue !== undefined ? `(${String(typeValue)})` : ""}`;
            }
        }

        return makeNode({
            title: "ASTTypeNode",
            subtitle: "struct ASTTypeNode",
            color: "#2a3a5f",
            data: [`kind: ${kindText}`],
            span: typeNode.annotation.span,
        });
    };

    const buildIntLiteralNode = (value: IntLiteralValue): TreeNode => {
        const entries =
            value && typeof value === "object" ? Object.entries(value) : [];
        if (entries.length === 0) {
            return makeNode({
                title: "IntLiteral",
                subtitle: "enum IntLiteral",
                color: "#3d2817",
                data: ["variant: unknown"],
                span: undefined,
            });
        }

        const [variant, raw] = entries[0];
        return makeNode({
            title: "IntLiteral",
            subtitle: "enum IntLiteral",
            color: "#3d2817",
            data: [`variant: ${variant}`, `value: ${String(raw)}`],
            span: undefined,
        });
    };

    const buildVariableAccessNode = (access: AstVariableAccessValue): TreeNode => {
        const idNode = buildVariableIdNode(access?.id);
        return makeNode({
            title: "VariableAccess",
            subtitle: "struct VariableAccess",
            color: "#2a1f3d",
            children: [idNode],
            span: getSpanFromAnnotation(access),
        });
    };

    const buildQualifiedNameNode = (qualified_name: QualifiedName): TreeNode => {
        console.log(qualified_name)
        return makeNode({
            title: "QualifiedName",
            subtitle: "struct QualifiedName",
            color: "#2f294a",
            data: [`parts: [${qualified_name.parts.join(", ")}]`],
            span: qualified_name.annotation.span,
        });
    };

    const buildExpressionNode = (expr: AstExpressionValue): TreeNode | null => {
        if (!expr || typeof expr !== "object") return null;

        if ("IntLiteral" in expr) {
            const lit = expr.IntLiteral;
            const intLiteralNode = buildIntLiteralNode(lit.value);
            const span = getSpanFromAnnotation(lit);
            const variantNode = makeNode({
                title: "IntLiteral",
                subtitle: "variant",
                color: "#3a2a1d",
                children: [intLiteralNode],
                span,
            });
            return makeNode({
                title: "Expression",
                subtitle: "enum Expression",
                color: "#253247",
                children: [variantNode],
                span,
            });
        }

        if ("StringLiteral" in expr) {
            const lit = expr.StringLiteral;
            const valuePreview =
                lit.value.length > 30
                    ? `${lit.value.slice(0, 30)}...`
                    : lit.value;
            const span = getSpanFromAnnotation(lit);
            const variantNode = makeNode({
                title: "StringLiteral",
                subtitle: "variant",
                color: "#3a2a1d",
                data: [`value: \"${valuePreview}\"`],
                span,
            });
            return makeNode({
                title: "Expression",
                subtitle: "enum Expression",
                color: "#253247",
                children: [variantNode],
                span,
            });
        }

        if ("VariableAccess" in expr) {
            const access = expr.VariableAccess;
            const accessNode = buildVariableAccessNode(access.value);
            const span = getSpanFromAnnotation(access);
            const variantNode = makeNode({
                title: "VariableAccess",
                subtitle: "variant",
                color: "#3a2a1d",
                children: [accessNode],
                span,
            });
            return makeNode({
                title: "Expression",
                subtitle: "enum Expression",
                color: "#253247",
                children: [variantNode],
                span,
            });
        }

        if ("ArrayAccess" in expr) {
            const access = expr.ArrayAccess;
            const children: TreeNode[] = [
                buildVariableAccessNode(access.array),
            ];
            const indexExprNode = buildExpressionNode(access.index_expr);
            if (indexExprNode) children.push(indexExprNode);

            const span = getSpanFromAnnotation(access);
            const variantNode = makeNode({
                title: "ArrayAccess",
                subtitle: "variant",
                color: "#3a2a1d",
                children,
                span,
            });
            return makeNode({
                title: "Expression",
                subtitle: "enum Expression",
                color: "#253247",
                children: [variantNode],
                span,
            });
        }

        if ("FnCall" in expr) {
            console.log(expr)
            const call = expr.FnCall;
            const children: TreeNode[] = [buildQualifiedNameNode(call.qualified_name)];

            if (call.arguments && Array.isArray(call.arguments)) {
                call.arguments.forEach((arg: AstExpressionValue) => {
                    const argNode = buildExpressionNode(arg);
                    if (argNode) children.push(argNode);
                });
            }

            const span = getSpanFromAnnotation(call);
            const variantNode = makeNode({
                title: "FnCall",
                subtitle: "variant",
                color: "#3a2a1d",
                data: [`arguments: ${call.arguments?.length ?? 0}`],
                children,
                span,
            });
            return makeNode({
                title: "Expression",
                subtitle: "enum Expression",
                color: "#253247",
                children: [variantNode],
                span,
            });
        }

        return null;
    };

    const buildStatementNode = (
        statementPayload: Extract<AstBlockItem, { Statement: unknown }>["Statement"],
    ): TreeNode | null => {
        if (!statementPayload || typeof statementPayload !== "object")
            return null;

        if (statementPayload.VarDecl) {
            const varDecl = statementPayload.VarDecl;
            const children: TreeNode[] = [
                buildAstTypeNode(varDecl.type_),
                buildVariableIdNode(varDecl.variable_index),
            ];

            const valueNode = buildExpressionNode(varDecl.value);
            if (valueNode) children.push(valueNode);

            const span = getSpanFromAnnotation(varDecl);
            const variantNode = makeNode({
                title: "Statement::VarDecl",
                subtitle: "variant",
                color: "#1f2a3f",
                data: [`name: ${varDecl.name}`],
                children,
                span,
            });
            return makeNode({
                title: "Statement",
                subtitle: "enum Statement",
                color: "#2a3648",
                children: [variantNode],
                span,
            });
        }

        if (statementPayload.Assignment) {
            const assignment = statementPayload.Assignment;
            const children: TreeNode[] = [buildVariableIdNode(assignment.var)];
            const exprNode = buildExpressionNode(assignment.value);
            if (exprNode) children.push(exprNode);

            const span = getSpanFromAnnotation(assignment);
            const variantNode = makeNode({
                title: "Statement::Assignment",
                subtitle: "variant",
                color: "#1f2a3f",
                children,
                span,
            });
            return makeNode({
                title: "Statement",
                subtitle: "enum Statement",
                color: "#2a3648",
                children: [variantNode],
                span,
            });
        }

        if (statementPayload.Expression) {
            const exprStmt = statementPayload.Expression;
            const exprNode = buildExpressionNode(exprStmt.expr);
            const span = getSpanFromAnnotation(exprStmt);
            const variantNode = makeNode({
                title: "Statement::Expression",
                subtitle: "variant",
                color: "#1f2a3f",
                children: exprNode ? [exprNode] : undefined,
                span,
            });
            return makeNode({
                title: "Statement",
                subtitle: "enum Statement",
                color: "#2a3648",
                children: [variantNode],
                span,
            });
        }

        return null;
    };

    const buildBlockNode = (block: AstBlockBody): TreeNode | null => {
        if (!block || !Array.isArray(block.statements)) return null;

        const blockItemNodes: TreeNode[] = [];
        block.statements.forEach((item: AstBlockItem) => {
            const itemNode = buildBlockItemNode(item);
            if (itemNode) blockItemNodes.push(itemNode);
        });

        const span = getSpanFromAnnotation(block);
        return makeNode({
            title: "Block",
            subtitle: "struct Block",
            color: "#1f1f2f",
            data: [`statements: ${block.statements.length}`],
            children: blockItemNodes,
            span,
        });
    };

    const buildBlockItemNode = (item: AstBlockItem): TreeNode | null => {
        if (!item || typeof item !== "object") return null;

        if ("Statement" in item) {
            const statementNode = buildStatementNode(item.Statement);
            return makeNode({
                title: "BlockItem",
                subtitle: "enum BlockItem",
                color: "#3b2f45",
                data: ["variant: Statement"],
                children: statementNode ? [statementNode] : undefined,
                span: statementNode?.span,
            });
        }

        if ("Block" in item) {
            const blockNode = buildBlockNode(item.Block);
            return makeNode({
                title: "BlockItem",
                subtitle: "enum BlockItem",
                color: "#3b2f45",
                data: ["variant: Block"],
                children: blockNode ? [blockNode] : undefined,
                span: blockNode?.span,
            });
        }

        return null;
    };

    const buildFunctionParamNode = (param: AstFunctionParam): TreeNode => {
        const children: TreeNode[] = [
            buildAstTypeNode(param.type_),
            buildVariableIdNode(param.variable_index),
        ];
        return makeNode({
            title: "FunctionParam",
            subtitle: "struct FunctionParam",
            color: "#1a3a2a",
            children,
            span: getSpanFromAnnotation(param),
        });
    };

    const buildFunctionNode = (func: AstFunctionPayload): TreeNode => {
        const children: TreeNode[] = [buildFunctionIdNode(func.id)];

        if (func.params && Array.isArray(func.params)) {
            func.params.forEach((param: AstFunctionParam) => {
                children.push(buildFunctionParamNode(param));
            });
        }

        const bodyNode = buildBlockNode(func.body);
        if (bodyNode) {
            children.push(bodyNode);
        }

        const span = getSpanFromAnnotation(func);
        return makeNode({
            title: "Function",
            subtitle: "struct Function",
            color: "#1a2a3f",
            data: [
                `name: ${func.name}`,
                `params: ${func.params?.length ?? 0}`,
                `variable_name_mapping: ${Object.keys(func.variable_name_mapping ?? {}).length}`,
            ],
            children,
            span,
        });
    };

        const functions = Array.isArray(payload) ? payload : null;

    if (!functions) return null;

    const functionNodes: TreeNode[] = functions.map((func: AstFunctionPayload) =>
        buildFunctionNode(func),
    );

    const programSpan = mergeSpans(functionNodes.map((node) => node.span));

    return makeNode({
        title: "Program",
        subtitle: "struct Program",
        color: "#1e293b",
        data: [
            `functions: ${functionNodes.length}`,
        ],
        children: functionNodes,
        span: programSpan,
    });
};

const ASTTreeVisualization = ({
    payload,
    onNodeHover,
}: AstTreeVisualizationProps) => {
    const astTree = buildTreeFromAST(payload);

    if (!astTree) {
        return (
            <svg
                width="100%"
                height="100%"
                viewBox="0 0 800 200"
                xmlns="http://www.w3.org/2000/svg"
            >
                <text
                    x="400"
                    y="100"
                    textAnchor="middle"
                    fill="#a1a5b8"
                    fontSize="16"
                >
                    No AST data available
                </text>
            </svg>
        );
    }

    const positioned = calculateTreeLayout(astTree);
    const { nodes, connections } = renderTree(positioned, onNodeHover);
    const bounds = calculateTreeBounds(positioned);
    const VIEWPORT_PADDING = 120;
    const viewBoxX = bounds.minX - VIEWPORT_PADDING;
    const viewBoxY = bounds.minY - VIEWPORT_PADDING;
    const viewBoxWidth = bounds.maxX - bounds.minX + VIEWPORT_PADDING * 2;
    const viewBoxHeight = bounds.maxY - bounds.minY + VIEWPORT_PADDING * 2;
    const TREE_BACKGROUND_COLOR = "#0f172a";

    return (
        <svg
            width="100%"
            height="100%"
            viewBox={`${viewBoxX} ${viewBoxY} ${viewBoxWidth} ${viewBoxHeight}`}
            xmlns="http://www.w3.org/2000/svg"
            preserveAspectRatio="xMidYMin meet"
        >
            <rect
                x={viewBoxX}
                y={viewBoxY}
                width={viewBoxWidth}
                height={viewBoxHeight}
                fill={TREE_BACKGROUND_COLOR}
            />
            <defs>
                <filter
                    id="treeShadow"
                    x="-20%"
                    y="-20%"
                    width="140%"
                    height="140%"
                >
                    <feDropShadow
                        dx="0"
                        dy="4"
                        stdDeviation="8"
                        floodColor="#000"
                        floodOpacity="0.3"
                    />
                </filter>
            </defs>
            {connections}
            {nodes}
        </svg>
    );
};

const AllComponentsShowcase = () => {
    // Expressions
    const intLiteralCard = IntLiteralCard({ I32: 42 });
    const stringLiteralCard = StringLiteralCard("Hello, World!");
    const varAccessCard = VariableAccessCard(1);
    const arrayAccessCard = ArrayAccessCard(2);
    const fnCallCard = FnCallCard("print", 2);

    // Statements
    const varDeclCard = VarDeclCard("x", "I32", 0);
    const assignmentCard = AssignmentCard(0);
    const exprStmtCard = ExpressionStmtCard();

    // Structures
    const programCard = ProgramCard();
    const functionCard = FunctionCard("main", 3, 0);
    const blockCard = BlockBodyCard(5);
    const spanCard = SpanCard(42, 10);
    const paramCard = FunctionParamCard("I32", 1);

    return (
        <div
            style={{
                width: "100%",
                padding: "40px",
                backgroundColor: "#0f172a",
                minHeight: "100vh",
            }}
        >
            <h1
                style={{
                    color: "#e5e7eb",
                    fontFamily: "Inter, system-ui, sans-serif",
                    marginBottom: "30px",
                }}
            >
                AST Component Showcase
            </h1>

            {/* Expression Cards Section */}
            <h2
                style={{
                    color: "#cbd5e1",
                    fontFamily: "Inter, system-ui, sans-serif",
                    fontSize: "20px",
                    marginBottom: "20px",
                    marginTop: "40px",
                }}
            >
                Expression Components
            </h2>
            <svg
                width="100%"
                height="200"
                viewBox="0 0 1600 200"
                xmlns="http://www.w3.org/2000/svg"
                style={{ marginBottom: "20px" }}
            >
                <g transform="translate(0, 0)">
                    <intLiteralCard.svg />
                </g>
                <g transform="translate(320, 0)">
                    <stringLiteralCard.svg />
                </g>
                <g transform="translate(640, 0)">
                    <varAccessCard.svg />
                </g>
                <g transform="translate(960, 0)">
                    <arrayAccessCard.svg />
                </g>
                <g transform="translate(1280, 0)">
                    <fnCallCard.svg />
                </g>
            </svg>

            {/* Statement Cards Section */}
            <h2
                style={{
                    color: "#cbd5e1",
                    fontFamily: "Inter, system-ui, sans-serif",
                    fontSize: "20px",
                    marginBottom: "20px",
                    marginTop: "40px",
                }}
            >
                Statement Components
            </h2>
            <svg
                width="100%"
                height="200"
                viewBox="0 0 1280 200"
                xmlns="http://www.w3.org/2000/svg"
                style={{ marginBottom: "20px" }}
            >
                <g transform="translate(0, 0)">
                    <varDeclCard.svg />
                </g>
                <g transform="translate(320, 0)">
                    <assignmentCard.svg />
                </g>
                <g transform="translate(640, 0)">
                    <exprStmtCard.svg />
                </g>
                <g transform="translate(960, 0)">
                    <paramCard.svg />
                </g>
            </svg>

            {/* Structure Cards Section */}
            <h2
                style={{
                    color: "#cbd5e1",
                    fontFamily: "Inter, system-ui, sans-serif",
                    fontSize: "20px",
                    marginBottom: "20px",
                    marginTop: "40px",
                }}
            >
                Structure Components
            </h2>
            <svg
                width="100%"
                height="200"
                viewBox="0 0 1600 200"
                xmlns="http://www.w3.org/2000/svg"
                style={{ marginBottom: "20px" }}
            >
                <g transform="translate(0, 0)">
                    <programCard.svg />
                </g>
                <g transform="translate(320, 0)">
                    <functionCard.svg />
                </g>
                <g transform="translate(640, 0)">
                    <blockCard.svg />
                </g>
                <g transform="translate(960, 0)">
                    <spanCard.svg />
                </g>
            </svg>

            {/* Component Info Section */}
            <div
                style={{
                    marginTop: "60px",
                    padding: "20px",
                    backgroundColor: "#1e293b",
                    borderRadius: "12px",
                }}
            >
                <h3
                    style={{
                        color: "#e5e7eb",
                        fontFamily: "Inter, system-ui, sans-serif",
                        marginBottom: "15px",
                    }}
                >
                    Available Components
                </h3>
                <div
                    style={{
                        color: "#cbd5e1",
                        fontFamily: "monospace",
                        fontSize: "12px",
                        lineHeight: "1.8",
                    }}
                >
                    <p>
                        <strong>Expressions (5):</strong> IntLiteral,
                        StringLiteral, VariableAccess, ArrayAccess, FnCall
                    </p>
                    <p>
                        <strong>Statements (4):</strong> VarDecl, Assignment,
                        ExpressionStmt, FunctionParam
                    </p>
                    <p>
                        <strong>Structures (4):</strong> Program, Function,
                        Block, Span
                    </p>
                </div>
            </div>
        </div>
    );
};

export default ProgramSVG;
export {
    ExpressionCardsSVG,
    StatementCardsSVG,
    StructureCardsSVG,
    AllComponentsShowcase,
    ASTTreeVisualization,
};
