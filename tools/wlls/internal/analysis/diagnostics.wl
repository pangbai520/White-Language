// structured compiler diagnostics for protocol clients
import "../protocol/_pkg.wl" as protocol
import "../compiler/_pkg.wl" as compiler

func diagnostic_severity(value -> Int) -> String {
    if (value == compiler.WhitelangExceptions.DIAGNOSTIC_WARNING) { return "warning"; }
    if (value == compiler.WhitelangExceptions.DIAGNOSTIC_INFO) { return "information"; }
    if (value == compiler.WhitelangExceptions.DIAGNOSTIC_HINT) { return "hint"; }
    return "error";
}

func encode_diagnostics(items -> Vector(Struct)) -> String {
    let result -> String = "[";
    let i -> Int = 0;
    while (i < items.length()) {
        let item -> compiler.WhitelangExceptions.CompilerDiagnostic = items[i];
        let range -> compiler.WhitelangExceptions.SourceRange = item.range;
        if (i > 0) { result += ","; }
        result +=
            "{\"severity\":" + protocol.quote(diagnostic_severity(item.severity)) +
            ",\"code\":" + protocol.quote(item.code) +
            ",\"category\":" + protocol.quote(item.category) +
            ",\"message\":" + protocol.quote(item.message) +
            ",\"range\":{\"start\":{\"line\":" + range.start.line +
            ",\"character\":" + range.start.utf16_column +
            "},\"end\":{\"line\":" + range.end.line +
            ",\"character\":" + range.end.utf16_column + "}}}";
        i += 1;
    }
    return result + "]";
}
