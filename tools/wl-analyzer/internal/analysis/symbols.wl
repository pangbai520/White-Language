// document symbol response encoding
import "../protocol/_pkg.wl" as protocol
import "../frontend/_pkg.wl" as source
import "../compiler/_pkg.wl" as compiler

func symbol_kind(kind -> Int) -> String {
    if (kind == source.SYMBOL_FUNCTION) { return "function"; }
    if (kind == source.SYMBOL_VARIABLE) { return "variable"; }
    if (kind == source.SYMBOL_CONSTANT) { return "constant"; }
    if (kind == source.SYMBOL_STRUCT) { return "struct"; }
    if (kind == source.SYMBOL_CLASS) { return "class"; }
    if (kind == source.SYMBOL_METHOD) { return "method"; }
    if (kind == source.SYMBOL_FIELD) { return "field"; }
    if (kind == source.SYMBOL_ENUM) { return "enum"; }
    if (kind == source.SYMBOL_ENUM_CASE) { return "enumCase"; }
    if (kind == source.SYMBOL_INTERFACE) { return "interface"; }
    if (kind == source.SYMBOL_ERROR) { return "error"; }
    if (kind == source.SYMBOL_ERROR_CASE) { return "errorCase"; }
    if (kind == source.SYMBOL_CONVERSION) { return "conversion"; }
    if (kind == source.SYMBOL_PARAMETER) { return "parameter"; }
    return "unknown";
}

func encode_symbols(symbols -> Vector(Struct)) -> String {
    let result -> String = "[";
    let i -> Int = 0;
    while (i < symbols.length()) {
        let symbol -> source.DocumentSymbol = symbols[i];
        let span -> compiler.WhitelangExceptions.SourceRange = symbol.span;
        if (i > 0) { result += ","; }
        result +=
            "{\"name\":" + protocol.quote(symbol.name) +
            ",\"kind\":" + protocol.quote(symbol_kind(symbol.kind)) +
            ",\"range\":{\"start\":{\"line\":" + span.start.line +
            ",\"character\":" + span.start.utf16_column +
            "},\"end\":{\"line\":" + span.end.line +
            ",\"character\":" + span.end.utf16_column +
            "}},\"children\":" + encode_symbols(symbol.children) + "}";
        i += 1;
    }
    return result + "]";
}
