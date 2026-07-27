// Test: SEMANTIC_TOKENS
// File: tools/wlls/tests/test_semantic_tokens.wl
// Focus: Lexer-backed semantic token types, modifiers, comments, and UTF-16 ranges.

import "builtin"
import "../internal/frontend/_pkg.wl" as source
import "../internal/analysis/_pkg.wl" as analysis

func has_token(
    tokens -> Vector(Struct),
    line -> Int,
    character -> Int,
    token_type -> String
) -> Bool {
    let i -> Int = 0;
    while (i < tokens.length()) {
        let token -> analysis.SemanticToken = tokens[i];
        if (token.line == line &&
            token.character == character &&
            token.token_type == token_type) {
            return true;
        }
        i += 1;
    }
    return false;
}

func main() -> Int {
    let path -> String = "memory.wl";
    let text -> String =
        "@ExportLib\n" +
        "const LIMIT -> Int = 2;\n" +
        "func add(value -> Int) -> Int {\n" +
        "    // note\n" +
        "    let text -> String = \"😀\";\n" +
        "    return value + LIMIT;\n" +
        "}\n";

    let workspace -> source.FrontendWorkspace = source.FrontendWorkspace();
    let result -> source.FrontendResult = workspace.update(path, 1, text);
    let tokens -> Vector(Struct) =
        analysis.semantic_tokens(result, workspace, path);

    if (tokens.length() != 26) {
        builtin.print("FAIL: semantic token count " + tokens.length());
        return 1;
    }

    let annotation -> analysis.SemanticToken = tokens[1];
    if (annotation.token_type != "annotation" ||
        annotation.line != 0 ||
        annotation.character != 1 ||
        annotation.length != 9) {
        builtin.print("FAIL: annotation token");
        return 1;
    }

    let constant -> analysis.SemanticToken = tokens[3];
    if (constant.token_type != "variable" ||
        constant.modifiers.length() != 2 ||
        constant.modifiers[0] != "declaration" ||
        constant.modifiers[1] != "readonly") {
        builtin.print("FAIL: constant declaration token");
        return 1;
    }

    let function_name -> analysis.SemanticToken = tokens[9];
    let parameter -> analysis.SemanticToken = tokens[10];
    if (function_name.token_type != "function" ||
        function_name.modifiers.length() != 1 ||
        function_name.modifiers[0] != "declaration" ||
        parameter.token_type != "parameter" ||
        parameter.modifiers.length() != 1 ||
        parameter.modifiers[0] != "declaration") {
        builtin.print("FAIL: callable declaration tokens");
        return 1;
    }

    let comment -> analysis.SemanticToken = tokens[15];
    if (comment.token_type != "comment" ||
        comment.line != 3 ||
        comment.character != 4 ||
        comment.length != 7) {
        builtin.print("FAIL: comment token");
        return 1;
    }

    let string_value -> analysis.SemanticToken = tokens[21];
    if (string_value.token_type != "string" ||
        string_value.line != 4 ||
        string_value.character != 25 ||
        string_value.length != 4) {
        builtin.print("FAIL: UTF-16 string token");
        return 1;
    }

    let parameter_use -> analysis.SemanticToken = tokens[23];
    let constant_use -> analysis.SemanticToken = tokens[25];
    if (parameter_use.token_type != "parameter" ||
        parameter_use.modifiers.length() != 0 ||
        constant_use.token_type != "variable" ||
        constant_use.modifiers.length() != 1 ||
        constant_use.modifiers[0] != "readonly") {
        builtin.print("FAIL: semantic reference tokens");
        return 1;
    }

    workspace.update(
        "project/math.wl",
        1,
        "func add(left -> Int, right -> Int) -> Int { return left + right; }\n"
    );
    let imported_path -> String = "project/main.wl";
    let imported -> source.FrontendResult = workspace.update(
        imported_path,
        1,
        "import add from \"math.wl\"\n" +
        "func main() -> Int { return add(1, 2); }\n"
    );
    let imported_tokens -> Vector(Struct) =
        analysis.semantic_tokens(imported, workspace, imported_path);
    if (!has_token(imported_tokens, 1, 28, "function")) {
        builtin.print("FAIL: imported function token");
        return 1;
    }

    builtin.print("PASS: semantic tokens");
    return 0;
}
