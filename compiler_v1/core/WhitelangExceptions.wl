// core/WhitelangException.wl
import "builtin"

extern func exit(...) -> Void from "C";

// Tracks the cursor in the source text
struct Position(
    idx  -> Int,
    ln   -> Int,
    col  -> Int,
    text -> String
)

// Helper to advance position manually
func advance_pos(pos -> Position, current_char -> Byte) -> Void {
    pos.idx = pos.idx + 1;
    pos.col = pos.col + 1;

    if (current_char == 10) { // '\n'
        pos.ln = pos.ln + 1;
        pos.col = 0;
    }
}

// Visual error reporter with caret support
func report_error(pos -> Position, name -> String, details -> String) -> Void {
    builtin.print(name + ": " + details);
    builtin.print("Line " + (pos.ln + 1) + ", column " + (pos.col + 1));

    let text -> String = pos.text;
    let target_ln -> Int = pos.ln;
    let current_ln -> Int = 0;
    let start_idx -> Int = 0;
    let i -> Int = 0;

    // Locate the start of the target line
    while (i < text.length && current_ln < target_ln) {
        if (text[i] == 10) { // '\n'
            current_ln = current_ln + 1;
            start_idx = i + 1;
        }
        i = i + 1;
    }

    // Locate the end of the target line
    let end_idx -> Int = start_idx;
    while (end_idx < text.length && text[end_idx] != 10 && text[end_idx] != 13) {
        end_idx = end_idx + 1;
    }

    // Print the source line and caret
    if (start_idx < text.length) {
        let line_text -> String = text.slice(start_idx, end_idx);
        builtin.print("");
        builtin.print(line_text);
        
        let caret_line -> String = "";
        let j -> Int = 0;
        while (j < pos.col) {
            caret_line = caret_line + " ";
            j = j + 1;
        }
        builtin.print(caret_line + "^");
    }
    
    // Critical: Stop compilation immediately to prevent malformed IR generation
    exit(1); 
}

func throw_illegal_char(pos -> Position, details -> String) -> Void {
    report_error(pos, "IllegalCharacter", details);
}

func throw_invalid_syntax(pos -> Position, details -> String) -> Void {
    report_error(pos, "InvalidSyntax", details);
}

func throw_name_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "NameError", details);
}

func throw_type_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "TypeError", details);
}

func throw_missing_initializer(pos -> Position, details -> String) -> Void {
    report_error(pos, "MissingInitializer", details);
}

func throw_missing_main_function() -> Void {
    builtin.print("MissingMainFunction: No 'main' function defined.");
    exit(1);
}
