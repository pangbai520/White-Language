// Test: SEMANTIC_TOKEN_KINDS
// File: tools/wlls/tests/test_token_kinds.wl
// Focus: Semantic classifications for White Language declarations and members.

import "builtin"
import "../internal/frontend/_pkg.wl" as source
import "../internal/analysis/_pkg.wl" as analysis

func has_token_type(tokens -> Vector(Struct), expected -> String) -> Bool {
    let i -> Int = 0;
    while (i < tokens.length()) {
        let token -> analysis.SemanticToken = tokens[i];
        if (token.token_type == expected) { return true; }
        i += 1;
    }
    return false;
}

func main() -> Int {
    let path -> String = "kinds.wl";
    let text -> String =
        "struct Point(x -> Int)\n" +
        "enum Color { Red, Blue }\n" +
        "interface Named { method name(prefix -> String) -> String; }\n" +
        "class User with Named {\n" +
        "    let value -> Int = 0;\n" +
        "    method name(prefix -> String) -> String { return prefix + \"user\"; }\n" +
        "}\n";

    let workspace -> source.FrontendWorkspace = source.FrontendWorkspace();
    let result -> source.FrontendResult = workspace.update(path, 1, text);
    let tokens -> Vector(Struct) =
        analysis.semantic_tokens(result, workspace, path);

    let required -> Vector(String) = [
        "struct",
        "enum",
        "enumMember",
        "interface",
        "class",
        "method",
        "parameter",
        "property",
        "type",
        "keyword",
        "string",
        "number"
    ];
    let i -> Int = 0;
    while (i < required.length()) {
        if (!has_token_type(tokens, required[i])) {
            builtin.print("FAIL: missing semantic token type " + required[i]);
            return 1;
        }
        i += 1;
    }

    builtin.print("PASS: semantic token kinds");
    return 0;
}
