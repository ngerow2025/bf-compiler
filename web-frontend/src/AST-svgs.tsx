import type { JSX } from "react/jsx-dev-runtime";

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

            <Curve from={programCard.bottom_middle()} to={programCard.top_left()} />
            {/* <g transform={`translate(${programCard.x_offset + programCard.width/2}, ${programCard.y_offset + programCard.height})`}>
                <circle cx="0" cy="0" r="5" style={{fill: "red"}} />
                <path d="M 0 0 Q 0 20, -20 20" stroke="black" fill="transparent" strokeWidth={5} />
            </g> */}

        </svg>
    );
};

interface CurveProps {
    from: [number, number];
    to: [number, number];
    strokeWidth?: number;
}

const Curve = (props: CurveProps) => {
    //do the horizontal component first then the vertical component
    //first the from virtical to horizontal quarter circle curve
    let isRight = props.to[0] > props.from[0];
    let isDown = props.to[1] > props.from[1];
    
    let firstCurveStart = props.from;
    let firstCurveEnd: [number, number] = [props.from[0] + (isRight ? 20 : -20), props.from[1] + (isDown ? 20 : -20)];
    let firstCurvePivot: [number, number] = [props.from[0], props.from[1] + (isDown ? 20 : -20)];

    let horizontalComponentStart = firstCurveEnd;
    let horizontalComponentEnd: [number, number] = [props.to[0] - (isRight ? 20 : -20), firstCurveEnd[1]];
    
    let secondCurveStart = horizontalComponentEnd;
    let secondCurveEnd: [number, number] = [secondCurveStart[0] + (isRight ? 20 : -20), secondCurveStart[1] + (isDown ? 20 : -20)];
    let secondCurvePivot: [number, number] = [secondCurveStart[0] + (isRight ? 20 : -20), secondCurveStart[1]];

    let verticalComponentStart = secondCurveEnd;
    let verticalComponentEnd = props.to;

    return (
        <g>
            <CurveCorner from={firstCurveStart} to={firstCurveEnd} pivot={firstCurvePivot} strokeWidth={props.strokeWidth} />
            <StraitLine from={horizontalComponentStart} to={horizontalComponentEnd} strokeWidth={props.strokeWidth} />
            <CurveCorner from={secondCurveStart} to={secondCurveEnd} pivot={secondCurvePivot} strokeWidth={props.strokeWidth} />
            {/* <StraitLine from={verticalComponentStart} to={verticalComponentEnd} strokeWidth={props.strokeWidth} /> */}
        </g>
    )
}

interface CurveCornerProps {
    from: [number, number];
    to: [number, number];
    pivot: [number, number];
    strokeWidth?: number;
}

const CurveCorner = (props: CurveCornerProps) => {
    let isRight = props.to[0] > props.from[0];
    let isDown = props.to[1] > props.from[1];

    return (
        <path d={`M ${props.from[0]} ${props.from[1]} Q ${props.pivot[0]} ${props.pivot[1]}, ${props.to[0]} ${props.to[1]}`} stroke="black" fill="transparent" strokeWidth={props.strokeWidth ?? 5} />
    );
}

interface StraitLineProps {
    from: [number, number];
    to: [number, number];
    strokeWidth?: number;
}

const StraitLine = (props: StraitLineProps) => {
    return (
        <line x1={props.from[0]} y1={props.from[1]} x2={props.to[0]} y2={props.to[1]} stroke="black" strokeWidth={props.strokeWidth ?? 5} />
    );
}


class SizedCard {
    width: number;
    height: number;
    x_offset: number;
    y_offset: number;
    svg: () => JSX.Element;
    constructor(width: number, height: number, x_offset: number, y_offset: number, svg: () => JSX.Element) {
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
        return [this.x_offset + this.width / 2, this.y_offset + this.height / 2];
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


const ProgramCard = () => {
    const card: SizedCard = new SizedCard(280, 120, 20, 20, () => <svg
            width="320"
            height="160"
            viewBox="0 0 320 160"
            xmlns="http://www.w3.org/2000/svg"
        >
            <defs>
                <linearGradient id="cardGradient" x1="0" y1="0" x2="0" y2="1">
                    <stop offset="0%" stop-color="#1e293b" />
                    <stop offset="100%" stop-color="#0f172a" />
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
                        flood-color="#000"
                        flood-opacity="0.35"
                    />
                </filter>
            </defs>

            {/* <rect
                x="20"
                y="20"
                width="280"
                height="120"
                fill="#0f172a"
            /> actual content bounding box*/}

            <rect
                x="20"
                y="20"
                width="280"
                height="120"
                rx="48"
                fill="url(#cardGradient)"
                filter="url(#shadow)"
            />

            <text
                x="160"
                y="65"
                text-anchor="middle"
                font-size="32"
                font-weight="700"
                fill="#e5e7eb"
                font-family="Inter, system-ui, sans-serif"
            >
                Program
            </text>

            <line
                x1="60"
                y1="85"
                x2="260"
                y2="85"
                stroke="#334155"
                stroke-width="1"
            />

            <text
                x="160"
                y="115"
                text-anchor="middle"
                font-size="16"
                font-weight="700"
                fill="#e5e7eb"
                font-family="Inter, system-ui, sans-serif"
            >
                Functions
            </text>
        </svg>
    );

    return card;
}

export default ProgramSVG;
