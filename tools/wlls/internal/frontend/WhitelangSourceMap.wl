// tools/wlls/internal/frontend/WhitelangSourceMap.wl
import "../../../../src/core/WhitelangExceptions.wl"
import "../../../../src/core/WhitelangLexer.wl"

class SourceMap {
    let text -> String = "";
    let line_starts -> Vector(Int) = null;

    init(text -> String) {
        // index line starts once; UTF-16 columns are decoded only inside the requested line
        self.text = "";
        self.line_starts = [0];
        if (text is null) { return; }
        self.text = text;

        let i -> Int = 0;
        while (i < text.length()) {
            if (text[i] == '\n') {
                self.line_starts.append(i + 1);
            }
            i += 1;
        }
    }

    method line_count() -> Int {
        return self.line_starts.length();
    }

    method line_start(line -> Int) -> Int {
        if (line < 0) { return 0; }
        if (line >= self.line_starts.length()) { return self.text.length(); }
        return self.line_starts[line];
    }

    method line_end(line -> Int) -> Int {
        let start -> Int = self.line_start(line);
        let end -> Int = self.text.length();
        if (line + 1 < self.line_starts.length()) {
            end = self.line_starts[line + 1] - 1;
        }
        if (end > start && self.text[end - 1] == '\r') { end -= 1; }
        return end;
    }

    method position(
        line -> Int,
        byte_column -> Int
    ) -> WhitelangExceptions.SourcePosition {
        let target_line -> Int = line;
        if (target_line < 0) { target_line = 0; }
        if (target_line >= self.line_starts.length()) {
            target_line = self.line_starts.length() - 1;
        }

        let start -> Int = self.line_start(target_line);
        let end -> Int = self.line_end(target_line);
        let column -> Int = byte_column;
        if (column < 0) { column = 0; }
        if (column > end - start) { column = end - start; }

        let target -> Int = start + column;
        let offset -> Int = start;
        let unicode_column -> Int = 0;
        let utf16_column -> Int = 0;
        while (offset < target) {
            let unit -> WhitelangLexer.Utf8Unit =
                WhitelangLexer.decode_utf8_unit(self.text, offset);
            if (unit.width <= 0 || offset + unit.width > target) { break; }
            offset += unit.width;
            unicode_column += 1;
            if (Int(unit.value) > 65535) {
                utf16_column += 2;
            } else {
                utf16_column += 1;
            }
        }

        return WhitelangExceptions.SourcePosition(
            byte_offset=start + column,
            line=target_line,
            byte_column=column,
            unicode_column=unicode_column,
            utf16_column=utf16_column
        );
    }

    method range(
        file -> String,
        line -> Int,
        byte_column -> Int,
        byte_width -> Int
    ) -> WhitelangExceptions.SourceRange {
        // token ranges stay line-local; multiline trivia is handled by the token encoder
        let width -> Int = byte_width;
        if (width < 0) { width = 0; }
        let start -> WhitelangExceptions.SourcePosition =
            self.position(line, byte_column);
        let finish -> WhitelangExceptions.SourcePosition =
            self.position(line, byte_column + width);
        return WhitelangExceptions.SourceRange(
            file=file,
            start=start,
            end=finish
        );
    }
}
