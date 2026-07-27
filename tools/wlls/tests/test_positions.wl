// Test: SOURCE_POSITIONS
// File: tools/wlls/tests/test_positions.wl
// Focus: UTF-8 byte, Unicode scalar, and UTF-16 source columns.

import "builtin"
import "../internal/compiler/_pkg.wl" as compiler

func main() -> Int {
    let text -> String = "a中😀b\nx";

    let after_cjk -> compiler.WhitelangExceptions.SourcePosition =
        compiler.WhitelangExceptions.source_position(text, 0, 4);
    if (after_cjk.byte_offset != 4 ||
        after_cjk.byte_column != 4 ||
        after_cjk.unicode_column != 2 ||
        after_cjk.utf16_column != 2) {
        builtin.print("FAIL: CJK source position");
        return 1;
    }

    let after_emoji -> compiler.WhitelangExceptions.SourcePosition =
        compiler.WhitelangExceptions.source_position(text, 0, 8);
    if (after_emoji.byte_offset != 8 ||
        after_emoji.byte_column != 8 ||
        after_emoji.unicode_column != 3 ||
        after_emoji.utf16_column != 4) {
        builtin.print("FAIL: supplementary source position");
        return 1;
    }

    let second_line -> compiler.WhitelangExceptions.SourcePosition =
        compiler.WhitelangExceptions.source_position(text, 1, 1);
    if (second_line.line != 1 ||
        second_line.byte_column != 1 ||
        second_line.unicode_column != 1 ||
        second_line.utf16_column != 1) {
        builtin.print("FAIL: multiline source position");
        return 1;
    }

    let range -> compiler.WhitelangExceptions.SourceRange =
        compiler.WhitelangExceptions.source_range("memory.wl", text, 0, 1, 7);
    if (range.start.utf16_column != 1 ||
        range.end.utf16_column != 4 ||
        range.end.byte_column != 8) {
        builtin.print("FAIL: UTF-16 source range");
        return 1;
    }

    compiler.WhitelangExceptions.begin_error_collection();
    let diagnostic_pos -> compiler.WhitelangExceptions.Position =
        compiler.WhitelangExceptions.Position(
            idx=0,
            ln=0,
            col=8,
            text="中😀 bad",
            fn="memory.wl"
        );
    compiler.WhitelangExceptions.throw_name_error(
        diagnostic_pos,
        "unknown name"
    );
    let diagnostic -> compiler.WhitelangExceptions.CompilerDiagnostic =
        compiler.WhitelangExceptions.STRUCTURED_ERRORS[0];
    compiler.WhitelangExceptions.end_error_collection();
    compiler.WhitelangExceptions.reset_errors();

    if (diagnostic.code != "E2001" ||
        diagnostic.range.start.utf16_column != 4 ||
        diagnostic.range.end.utf16_column != 7) {
        builtin.print("FAIL: Unicode diagnostic range");
        return 1;
    }

    builtin.print("PASS: source positions");
    return 0;
}
