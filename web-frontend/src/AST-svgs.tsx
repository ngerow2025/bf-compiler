const ProgramSVG = () => {
    return (
        <svg
            width="320"
            height="420"
            viewBox="0 0 320 420"
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

            <rect
                x="20"
                y="20"
                width="280"
                height="380"
                rx="48"
                fill="url(#cardGradient)"
                filter="url(#shadow)"
            />

            <text
                x="160"
                y="90"
                text-anchor="middle"
                font-size="32"
                font-weight="700"
                fill="#e5e7eb"
                font-family="Inter, system-ui, sans-serif"
            >
                Function
            </text>

            <line
                x1="60"
                y1="120"
                x2="260"
                y2="120"
                stroke="#334155"
                stroke-width="1"
            />

            <g font-family="Inter, system-ui, sans-serif">
                <g transform="translate(70, 300)">
                    <text x="0" y="0" font-size="14" fill="#94a3b8">
                        ATTACK
                    </text>
                    <text
                        x="0"
                        y="32"
                        font-size="28"
                        font-weight="600"
                        fill="#f87171"
                    >
                        7
                    </text>
                </g>

                <g transform="translate(200, 300)">
                    <text x="0" y="0" font-size="14" fill="#94a3b8">
                        HEALTH
                    </text>
                    <text
                        x="0"
                        y="32"
                        font-size="28"
                        font-weight="600"
                        fill="#34d399"
                    >
                        12
                    </text>
                </g>
            </g>
        </svg>
    );
};

export default ProgramSVG;
