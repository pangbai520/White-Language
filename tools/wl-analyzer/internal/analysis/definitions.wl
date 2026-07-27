// definition locations returned by semantic queries
import "../protocol/_pkg.wl" as protocol
import "../frontend/_pkg.wl" as source
import "../compiler/_pkg.wl" as compiler

func encode_definition(definition -> source.SymbolDefinition) -> String {
    if (definition is null || definition.range is null) { return "null"; }
    let range -> compiler.WhitelangExceptions.SourceRange = definition.range;
    return
        "{\"path\":" + protocol.quote(range.file) +
        ",\"range\":{\"start\":{\"line\":" + range.start.line +
        ",\"character\":" + range.start.utf16_column +
        "},\"end\":{\"line\":" + range.end.line +
        ",\"character\":" + range.end.utf16_column + "}}}";
}
