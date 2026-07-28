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
        "}\n" +
        "\n" +
        "class Output {\n" +
        "    method write() -> Void { return; }\n" +
        "}\n" +
        "class Compiler {\n" +
        "    let output_file -> Output = null;\n" +
        "}\n" +
        "func emit(c -> Compiler) -> Void {\n" +
        "    c.output_file.write();\n" +
        "}\n" +
        "class Leaf {\n" +
        "    method flush() -> Void { return; }\n" +
        "}\n" +
        "class Branch {\n" +
        "    let leaf -> Leaf = null;\n" +
        "}\n" +
        "class Root {\n" +
        "    let branch -> Branch = null;\n" +
        "}\n" +
        "func flush(root -> Root) -> Void {\n" +
        "    root.branch.leaf.flush();\n" +
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

    let chained_field -> analysis.SemanticToken = token_at(tokens, 25, 6);
    let chained_call -> analysis.SemanticToken = token_at(tokens, 25, 18);
    if (chained_field is null ||
        chained_call is null ||
        chained_field.token_type != "property" ||
        chained_call.token_type != "method") {
        builtin.print("FAIL: chained member call");
        return 1;
    }

    let deep_branch -> analysis.SemanticToken = token_at(tokens, 37, 9);
    let deep_leaf -> analysis.SemanticToken = token_at(tokens, 37, 16);
    let deep_call -> analysis.SemanticToken = token_at(tokens, 37, 21);
    if (deep_branch is null ||
        deep_leaf is null ||
        deep_call is null ||
        deep_branch.token_type != "property" ||
        deep_leaf.token_type != "property" ||
        deep_call.token_type != "method") {
        builtin.print("FAIL: deep member chain");
        return 1;
    }

    let external_path -> String = "project/main.wl";
    let external -> source.FrontendResult = workspace.update(
        external_path,
        1,
        "import Driver from \"driver.wl\"\n" +
        "func run(driver -> Driver) -> Void {\n" +
        "    driver.output.write();\n" +
        "}\n"
    );
    workspace.update(
        "project/driver.wl",
        1,
        "import Sink from \"output.wl\"\n" +
        "class Driver {\n" +
        "    let output -> Sink = null;\n" +
        "}\n"
    );
    workspace.update(
        "project/output.wl",
        1,
        "class Sink {\n" +
        "    method write() -> Void { return; }\n" +
        "}\n"
    );
    let external_tokens -> Vector(Struct) =
        analysis.semantic_tokens(external, workspace, external_path);
    let external_field -> analysis.SemanticToken =
        token_at(external_tokens, 2, 11);
    let external_call -> analysis.SemanticToken =
        token_at(external_tokens, 2, 18);
    if (external_field is null ||
        external_call is null ||
        external_field.token_type != "property" ||
        external_call.token_type != "method") {
        builtin.print("FAIL: cross-document member call");
        return 1;
    }

    workspace.remove("project/output.wl");
    let closed_tokens -> Vector(Struct) =
        analysis.semantic_tokens(external, workspace, external_path);
    let closed_call -> analysis.SemanticToken =
        token_at(closed_tokens, 2, 18);
    if (closed_call is null || closed_call.token_type != "variable") {
        builtin.print("FAIL: closed document member remained indexed");
        return 1;
    }

    builtin.print("PASS: wlls member calls");
    return 0;
}
