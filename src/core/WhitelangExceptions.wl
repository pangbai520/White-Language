// core/WhitelangExceptions.wl
import "builtin"

extern func exit(status -> Int) -> Void from "C";

let GLOBAL_ERROR_COUNT -> Int = 0;
let LAST_ERROR_FILE -> String = "";

struct Position(
    idx  -> Int,
    ln   -> Int,
    col  -> Int,
    text -> String,
    fn   -> String
)

func advance_pos(pos -> Position, current_char -> Byte) -> Void {
    pos.idx = pos.idx + 1;
    pos.col = pos.col + 1;

    if (current_char == 10) { // '\n'
        pos.ln = pos.ln + 1;
        pos.col = 0;
    }
}

func report_error(pos -> Position, name -> String, details -> String) -> Void {
    GLOBAL_ERROR_COUNT = GLOBAL_ERROR_COUNT + 1;

    let ln -> Int = pos.ln + 1;
    let col -> Int = pos.col + 1;

    if (LAST_ERROR_FILE != pos.fn) {
        builtin.print("In file:");
        
    }
    builtin.print("   " + pos.fn + ":" + ln + ":" + col);
    LAST_ERROR_FILE = pos.fn;

    let text -> String = pos.text;
    let target_ln -> Int = pos.ln;
    let current_ln -> Int = 0;
    let start_idx -> Int = 0;
    let i -> Int = 0;

    while (i < text.length() && current_ln < target_ln) {
        if (text[i] == 10) { // '\n'
            current_ln += 1;
            start_idx = i + 1;
        }
        i += 1;
    }

    let end_idx -> Int = start_idx;
    while (end_idx < text.length() && text[end_idx] != 10 && text[end_idx] != 13) {
        end_idx += 1;
    }

    if (start_idx < text.length()) {
        let line_text -> String = text.slice(start_idx, end_idx);
        
        let ln_str -> String = "" + ln;
        let ln_width -> Int = ln_str.length();
        
        let empty_prefix -> String = "  ";
        let p1 -> Int = 0;
        while (p1 < ln_width) { empty_prefix = empty_prefix + " "; p1 += 1; }
        
        builtin.print(empty_prefix + "| ");
        builtin.print(" " + ln_str + " | " + line_text);

        let err_len -> Int = 1;
        let line_len -> Int = line_text.length();
        if (pos.col < line_len) {
            let ch -> Int = line_text[pos.col];
            if ((ch >= 65 && ch <= 90) || (ch >= 97 && ch <= 122) || ch == 95) {
                let cur -> Int = pos.col + 1;
                while (cur < line_len) {
                    let c2 -> Int = line_text[cur];
                    if ((c2 >= 65 && c2 <= 90) || (c2 >= 97 && c2 <= 122) || c2 == 95 || (c2 >= 48 && c2 <= 57)) {
                        cur += 1;
                    } else {
                        break;
                    }
                }
                err_len = cur - pos.col;
            }
        }

        let caret_line -> String = empty_prefix + "| ";
        let j -> Int = 0;
        while (j < pos.col) {
            let ch -> Int = 32;
            if (j < line_len) { ch = line_text[j]; }
            
            if (ch == 9) { 
                caret_line = caret_line + line_text.slice(j, j + 1);
            } else {
                caret_line = caret_line + " ";
            }
            j += 1;
        }
        
        let k -> Int = 0;
        while (k < err_len) {
            caret_line = caret_line + "^";
            k += 1;
        }
        
        builtin.print(caret_line);
    }

    builtin.print(name + ": " + details + "\n");

    if (GLOBAL_ERROR_COUNT > 50) {
        builtin.print("fatal error: too many errors emitted, stopping now");
        exit(1);
    }
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

func throw_null_dereference_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "NullDereferenceError", details);
}

func throw_index_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "IndexError", details);
}

func throw_import_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "ImportError", details);
}

func throw_IO_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "IOError", details);
}

func throw_internal_compiler_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "InternalCompilerError", details);
}

func throw_zero_division_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "ZeroDivisionError", details);
}

func throw_missing_main_function() -> Void { // special
    builtin.print("MissingMainFunction: No 'main' function defined.");
    exit(1);
}

func throw_environment_error(details -> String) -> Void { // special
    builtin.print("EnvironmentError: " + details);
    exit(1);
}

func check_errors_and_abort() -> Void {
    if (GLOBAL_ERROR_COUNT > 0) {
        let suffix -> String = " error.";
        if (GLOBAL_ERROR_COUNT > 1) { suffix = " errors."; }
        builtin.print("Found " + GLOBAL_ERROR_COUNT + suffix);
        exit(1);
    }
}