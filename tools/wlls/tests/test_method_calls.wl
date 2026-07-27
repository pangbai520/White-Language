// Test: WLLS_METHOD_CALLS
// File: tools/wlls/tests/test_method_calls.wl
// Focus: Semantic indexing of member calls on parameters and local variables.

import "builtin"
import "../internal/frontend/_pkg.wl" as source
import "../internal/analysis/_pkg.wl" as analysis

func token_at(
    tokens -> Vector(Struct),
    line -> Int,
    character -> Int
) -> analysis.SemanticToken {
    let i -> Int = 0;
    while (i < tokens.length()) {
        let token -> analysis.SemanticToken = tokens[i];
        if (token.line == line && token.character == character) {
            return token;
        }
        i += 1;
    }
    return null;
}

func main() -> Int {
    let text -> String =
        "class Value {\n" +
        "    method text() -> String {\n" +
        "        return \"ok\";\n" +
        "    }\n" +
        "    method checked() -> String? {\n" +
        "        return \"ok\";\n" +
        "    }\n" +
        "}\n" +
        "\n" +
        "func string(value -> Value) -> String {\n" +
        "    let local -> Value = value;\n" +
        "    let first -> String = value.text();\n" +
        "    let second -> String = local.text();\n" +
        "    let third -> String = local.checked()?;\n" +
        "    catch(err) { return \"\"; }\n" +
        "    return first + second + third;\n" +
        "}\n";

    let workspace -> source.FrontendWorkspace = source.FrontendWorkspace();
    let result -> source.FrontendResult =
        workspace.update("memory.wl", 1, text);
    if (!result.valid) {
        builtin.print("FAIL: member call source did not parse");
        return 1;
    }

    let tokens -> Vector(Struct) =
        analysis.semantic_tokens(result, workspace, "memory.wl");
    if (tokens.length() == 0) {
        builtin.print("FAIL: member calls produced no semantic tokens");
        return 1;
    }

    let parameter_call -> analysis.SemanticToken = token_at(tokens, 11, 32);
    let local_call -> analysis.SemanticToken = token_at(tokens, 12, 33);
    let fallible_call -> analysis.SemanticToken = token_at(tokens, 13, 32);
    if (parameter_call is null ||
        local_call is null ||
        fallible_call is null ||
        parameter_call.token_type != "method" ||
        local_call.token_type != "method" ||
        fallible_call.token_type != "method") {
        builtin.print("FAIL: member calls were not classified as methods");
        return 1;
    }

    builtin.print("PASS: wlls member calls");
    return 0;
}
