// Test: SEMANTIC_DEFINITION
// File: tools/wlls/tests/test_semantic.wl
// Focus: Resolving local and top-level references without lowering LLVM IR.

import "builtin"
import "../internal/frontend/_pkg.wl" as source

func main() -> Int {
    let source_text -> String =
        "const BASE -> Int = 2;\n" +
        "func add(value -> Int) -> Int {\n" +
        "    let next -> Int = value + BASE;\n" +
        "    return next;\n" +
        "}\n";

    let syntax -> source.FrontendDocument =
        source.parse_document("memory.wl", source_text);
    let semantics -> source.SemanticDocument =
        source.analyze_document(syntax);

    let value -> source.SymbolDefinition =
        source.definition_at(semantics, 2, 22);
    if (value is null ||
        value.name != "value" ||
        value.range.start.line != 1 ||
        value.range.start.utf16_column != 9) {
        builtin.print("FAIL: parameter definition");
        return 1;
    }

    let base -> source.SymbolDefinition =
        source.definition_at(semantics, 2, 30);
    if (base is null ||
        base.name != "BASE" ||
        base.range.start.line != 0 ||
        base.range.start.utf16_column != 6 ||
        source.type_at(semantics, 2, 30) != "Int") {
        builtin.print("FAIL: top-level definition");
        return 1;
    }

    let next -> source.SymbolDefinition =
        source.definition_at(semantics, 3, 11);
    if (next is null ||
        next.name != "next" ||
        next.range.start.line != 2 ||
        next.range.start.utf16_column != 8 ||
        source.type_at(semantics, 3, 11) != "Int") {
        builtin.print("FAIL: local definition");
        return 1;
    }

    let class_text -> String =
        "class Counter {\n" +
        "    let value -> Int = 0;\n" +
        "    method get() -> Int {\n" +
        "        return self.value;\n" +
        "    }\n" +
        "}\n";
    let class_semantics -> source.SemanticDocument =
        source.analyze_document(
            source.parse_document("counter.wl", class_text)
        );
    let field -> source.SymbolDefinition =
        source.definition_at(class_semantics, 3, 20);
    if (field is null ||
        field.name != "value" ||
        field.kind != source.SYMBOL_FIELD ||
        field.range.start.line != 1 ||
        field.range.start.utf16_column != 8) {
        builtin.print("FAIL: class field definition");
        return 1;
    }

    builtin.print("PASS: semantic definitions");
    return 0;
}
