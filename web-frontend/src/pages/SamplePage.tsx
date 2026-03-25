import { useEffect } from "react";
import { mapReviver, type AstProgramPayload } from "../App";
import ASTProgram from "../components/ASTProgram";
import { TransformComponent, TransformWrapper } from "react-zoom-pan-pinch";



const sampleData: AstProgramPayload = JSON.parse(`[{"name":"greet","params":[],"body":{"statements":[{"Statement":{"VarDecl":{"name":"hello","type_":{"kind":{"Str":10},"annotation":{"span":{"offset":28,"length":7}}},"value":{"StringLiteral":{"value":"Hello,    ","annotation":{"span":{"offset":38,"length":12}}}},"variable_index":0,"annotation":{"span":{"offset":17,"length":34}}}}},{"Statement":{"VarDecl":{"name":"world","type_":{"kind":{"Str":10},"annotation":{"span":{"offset":67,"length":7}}},"value":{"StringLiteral":{"value":"World!    ","annotation":{"span":{"offset":77,"length":12}}}},"variable_index":1,"annotation":{"span":{"offset":56,"length":34}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["strOut"],"annotation":{"span":{"offset":95,"length":6}}},"arguments":[{"VariableAccess":{"value":{"id":0,"annotation":{"span":{"offset":102,"length":5}}},"annotation":{"span":{"offset":102,"length":5}}}}],"annotation":{"span":{"offset":95,"length":13}}}},"annotation":{"span":{"offset":95,"length":14}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["strOut"],"annotation":{"span":{"offset":114,"length":6}}},"arguments":[{"VariableAccess":{"value":{"id":1,"annotation":{"span":{"offset":121,"length":5}}},"annotation":{"span":{"offset":121,"length":5}}}}],"annotation":{"span":{"offset":114,"length":13}}}},"annotation":{"span":{"offset":114,"length":14}}}}},{"Statement":{"VarDecl":{"name":"name","type_":{"kind":{"Str":10},"annotation":{"span":{"offset":144,"length":7}}},"value":{"StringLiteral":{"value":"Alice     ","annotation":{"span":{"offset":154,"length":12}}}},"variable_index":2,"annotation":{"span":{"offset":134,"length":33}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["strOut"],"annotation":{"span":{"offset":172,"length":6}}},"arguments":[{"StringLiteral":{"value":"Hello     ","annotation":{"span":{"offset":179,"length":12}}}}],"annotation":{"span":{"offset":172,"length":20}}}},"annotation":{"span":{"offset":172,"length":21}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["strOut"],"annotation":{"span":{"offset":198,"length":6}}},"arguments":[{"VariableAccess":{"value":{"id":2,"annotation":{"span":{"offset":205,"length":4}}},"annotation":{"span":{"offset":205,"length":4}}}}],"annotation":{"span":{"offset":198,"length":12}}}},"annotation":{"span":{"offset":198,"length":13}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["strOut"],"annotation":{"span":{"offset":216,"length":6}}},"arguments":[{"StringLiteral":{"value":"!\\n        ","annotation":{"span":{"offset":223,"length":12}}}}],"annotation":{"span":{"offset":216,"length":20}}}},"annotation":{"span":{"offset":216,"length":21}}}}}],"annotation":{"span":{"offset":11,"length":228}}},"id":0,"variable_name_mapping":{"__type":"Map","value":[[2,"name"],[1,"world"],[0,"hello"]]},"annotation":{"span":{"offset":0,"length":239}}},{"name":"strOut","params":[{"type_":{"kind":{"Str":10},"annotation":{"span":{"offset":254,"length":7}}},"variable_index":0,"annotation":{"span":{"offset":251,"length":10}}}],"body":{"statements":[{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":269,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":278,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":0},"annotation":{"span":{"offset":280,"length":3}}}},"annotation":{"span":{"offset":278,"length":6}}}}],"annotation":{"span":{"offset":269,"length":16}}}},"annotation":{"span":{"offset":269,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":291,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":300,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":1},"annotation":{"span":{"offset":302,"length":3}}}},"annotation":{"span":{"offset":300,"length":6}}}}],"annotation":{"span":{"offset":291,"length":16}}}},"annotation":{"span":{"offset":291,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":313,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":322,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":2},"annotation":{"span":{"offset":324,"length":3}}}},"annotation":{"span":{"offset":322,"length":6}}}}],"annotation":{"span":{"offset":313,"length":16}}}},"annotation":{"span":{"offset":313,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":335,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":344,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":3},"annotation":{"span":{"offset":346,"length":3}}}},"annotation":{"span":{"offset":344,"length":6}}}}],"annotation":{"span":{"offset":335,"length":16}}}},"annotation":{"span":{"offset":335,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":357,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":366,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":4},"annotation":{"span":{"offset":368,"length":3}}}},"annotation":{"span":{"offset":366,"length":6}}}}],"annotation":{"span":{"offset":357,"length":16}}}},"annotation":{"span":{"offset":357,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":379,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":388,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":5},"annotation":{"span":{"offset":390,"length":3}}}},"annotation":{"span":{"offset":388,"length":6}}}}],"annotation":{"span":{"offset":379,"length":16}}}},"annotation":{"span":{"offset":379,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":401,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":410,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":6},"annotation":{"span":{"offset":412,"length":3}}}},"annotation":{"span":{"offset":410,"length":6}}}}],"annotation":{"span":{"offset":401,"length":16}}}},"annotation":{"span":{"offset":401,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":423,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":432,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":7},"annotation":{"span":{"offset":434,"length":3}}}},"annotation":{"span":{"offset":432,"length":6}}}}],"annotation":{"span":{"offset":423,"length":16}}}},"annotation":{"span":{"offset":423,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":445,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":454,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":8},"annotation":{"span":{"offset":456,"length":3}}}},"annotation":{"span":{"offset":454,"length":6}}}}],"annotation":{"span":{"offset":445,"length":16}}}},"annotation":{"span":{"offset":445,"length":17}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":467,"length":8}}},"arguments":[{"ArrayAccess":{"array":{"id":0,"annotation":{"span":{"offset":476,"length":1}}},"index_expr":{"IntLiteral":{"value":{"U8":9},"annotation":{"span":{"offset":478,"length":3}}}},"annotation":{"span":{"offset":476,"length":6}}}}],"annotation":{"span":{"offset":467,"length":16}}}},"annotation":{"span":{"offset":467,"length":17}}}}}],"annotation":{"span":{"offset":263,"length":223}}},"id":1,"variable_name_mapping":{"__type":"Map","value":[[0,"s"]]},"annotation":{"span":{"offset":241,"length":245}}},{"name":"main","params":[],"body":{"statements":[{"Statement":{"VarDecl":{"name":"integer","type_":{"kind":"U8","annotation":{"span":{"offset":517,"length":2}}},"value":{"IntLiteral":{"value":{"U8":42},"annotation":{"span":{"offset":522,"length":4}}}},"variable_index":0,"annotation":{"span":{"offset":504,"length":23}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["std","out"],"annotation":{"span":{"offset":533,"length":8}}},"arguments":[{"VariableAccess":{"value":{"id":0,"annotation":{"span":{"offset":542,"length":7}}},"annotation":{"span":{"offset":542,"length":7}}}}],"annotation":{"span":{"offset":533,"length":17}}}},"annotation":{"span":{"offset":533,"length":18}}}}},{"Statement":{"Expression":{"expr":{"FnCall":{"qualified_name":{"parts":["greet"],"annotation":{"span":{"offset":557,"length":5}}},"arguments":[],"annotation":{"span":{"offset":557,"length":7}}}},"annotation":{"span":{"offset":557,"length":8}}}}}],"annotation":{"span":{"offset":498,"length":69}}},"id":2,"variable_name_mapping":{"__type":"Map","value":[[0,"integer"]]},"annotation":{"span":{"offset":488,"length":79}}}]`, mapReviver);


const SamplePage = () => {
    useEffect(() => {
        const pageBg = "rgb(2 6 23)";
        const root = document.getElementById("root");

        const previousHtmlBg = document.documentElement.style.backgroundColor;
        const previousBodyBg = document.body.style.backgroundColor;
        const previousRootBg = root?.style.backgroundColor ?? "";

        document.documentElement.style.backgroundColor = pageBg;
        document.body.style.backgroundColor = pageBg;
        if (root) {
            root.style.backgroundColor = pageBg;
        }

        return () => {
            document.documentElement.style.backgroundColor = previousHtmlBg;
            document.body.style.backgroundColor = previousBodyBg;
            if (root) {
                root.style.backgroundColor = previousRootBg;
            }
        };
    }, []);

    return (
        <main className="min-h-screen text-slate-100">
            <div className="w-full py-6">
                <TransformWrapper
                    limitToBounds={false}
                    wheel={{
                        step: 0.2,
                        smoothStep: 0.001,
                        disabled: false,
                        activationKeys: [],
                        excluded: [],
                    }}
                    pinch={{
                        step: 5,
                        disabled: false,
                        excluded: [],
                    }}
                    panning={{
                        disabled: false,
                        excluded: [],
                        activationKeys: [],
                    }}
                    doubleClick={{
                        disabled: false,
                        excluded: [],
                        step: 1.2,
                    }}
                    minScale={0.03}
                    maxScale={12}
                    initialScale={1}
                >
                    {({ zoomIn, zoomOut, zoomToElement }) => (
                        <div className="w-full">
                            <div className="sticky top-3 z-20 mx-auto mb-4 flex w-fit items-center gap-2 rounded-lg border border-slate-700/70 bg-slate-900/85 p-2 backdrop-blur">
                                <button
                                    type="button"
                                    onClick={() => zoomOut(0.8)}
                                    className="rounded-md border border-slate-600 px-3 py-1 text-sm hover:bg-slate-800"
                                >
                                    Zoom Out
                                </button>
                                <button
                                    type="button"
                                    onClick={() => zoomToElement("sample-program-root", 1, 250)}
                                    className="rounded-md border border-slate-600 px-3 py-1 text-sm hover:bg-slate-800"
                                >
                                    Reset
                                </button>
                                <button
                                    type="button"
                                    onClick={() => zoomIn(0.8)}
                                    className="rounded-md border border-slate-600 px-3 py-1 text-sm hover:bg-slate-800"
                                >
                                    Zoom In
                                </button>
                            </div>
                            <TransformComponent
                                wrapperStyle={{ width: "100%" }}
                                contentStyle={{ width: "max-content" }}
                            >
                                <ASTProgram ast={sampleData} rootNodeId="sample-program-root" />
                            </TransformComponent>
                        </div>
                    )}
                </TransformWrapper>
            </div>
        </main>
    );
};

export default SamplePage;
