// Test: FRONTEND_DOCUMENT
// File: tools/wlls/tests/test_document.wl
// Focus: compiler-owned parsing, source spans, and document symbol indexing.
import "builtin"
import "../internal/frontend/_pkg.wl" as source

func main() -> Int {
    let source_text -> String =
        "const LIMIT -> Int = 4;\n" +
        "func add(left -> Int, right -> Int) -> Int { return left + right; }\n" +
        "class Counter {\n" +
        "    let value -> Int = 0;\n" +
        "    init(value -> Int) -> Void { self.value = value; }\n" +
        "    method get() -> Int { return self.value; }\n" +
        "}\n";

    let document -> source.FrontendDocument =
        source.parse_document("memory.wl", source_text);

    if (document.source_map is null ||
        document.source_map.line_count() != 8) {
        builtin.print("FAIL: document source map");
        return 1;
    }

    if (document.symbols.length() != 3) {
        builtin.print("FAIL: top-level document symbols");
        return 1;
    }

    let limit -> source.DocumentSymbol = document.symbols[0];
    let add -> source.DocumentSymbol = document.symbols[1];
    let counter -> source.DocumentSymbol = document.symbols[2];
    let left -> source.DocumentSymbol = add.children[0];
    let right -> source.DocumentSymbol = add.children[1];
    let value -> source.DocumentSymbol = counter.children[0];
    let init -> source.DocumentSymbol = counter.children[1];
    let get -> source.DocumentSymbol = counter.children[2];

    if (limit.name != "LIMIT" ||
        limit.kind != source.SYMBOL_CONSTANT ||
        limit.span.start.line != 0 ||
        limit.span.start.byte_column != 6 ||
        limit.span.end.byte_column != 11) {
        builtin.print("FAIL: declaration source span");
        return 1;
    }

    if (add.name != "add" || add.children.length() != 2 ||
        left.name != "left" || right.name != "right") {
        builtin.print("FAIL: function symbol");
        return 1;
    }

    if (counter.name != "Counter" || counter.children.length() != 3 ||
        value.name != "value" ||
        init.name != "init" ||
        get.name != "get") {
        builtin.print("FAIL: class member symbols");
        return 1;
    }

    builtin.print("PASS: compiler frontend document");
    return 0;
}
