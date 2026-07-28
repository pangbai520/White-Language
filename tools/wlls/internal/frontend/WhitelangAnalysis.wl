import * from "WhitelangFrontend.wl"
import * from "WhitelangSemantic.wl"
import "../../../../src/core/WhitelangExceptions.wl"

class FrontendResult {
    let syntax -> FrontendDocument;
    let semantics -> SemanticDocument;
    let diagnostics -> Vector(Struct);
    let valid -> Bool;

    init(
        syntax -> FrontendDocument,
        semantics -> SemanticDocument,
        diagnostics -> Vector(Struct),
        valid -> Bool
    ) {
        self.syntax = syntax;
        self.semantics = semantics;
        self.diagnostics = diagnostics;
        self.valid = valid;
    }
}

func check_source(path -> String, text -> String) -> FrontendResult {
    // own the diagnostic session so callers never touch compiler error globals
    WhitelangExceptions.begin_error_collection();
    let syntax -> FrontendDocument = parse_document(path, text);
    let diagnostics -> Vector(Struct) = WhitelangExceptions.STRUCTURED_ERRORS;
    let valid -> Bool = WhitelangExceptions.GLOBAL_ERROR_COUNT == 0;
    let semantics -> SemanticDocument = null;
    if (valid) {
        semantics = analyze_document(syntax);
    }
    WhitelangExceptions.end_error_collection();
    WhitelangExceptions.reset_errors();
    return FrontendResult(syntax, semantics, diagnostics, valid);
}
