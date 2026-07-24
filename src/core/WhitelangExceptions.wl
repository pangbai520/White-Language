// core/WhitelangExceptions.wl
import "builtin"
import "file"
import "process"

let GLOBAL_ERROR_COUNT -> Int = 0;
let LAST_ERROR_FILE -> String = "";
let CLEAN_TMP_LL -> String = "";
let ACTIVE_FILE -> file.File = null;
let ERROR_BUFFER -> Vector(String) = null;

struct Position(
    idx  -> Int,
    ln   -> Int,
    col  -> Int,
    text -> String,
    fn   -> String
)

func advance_pos(pos -> Position, current_char -> Char) -> Void {
    pos.idx = pos.idx + 1;
    pos.col = pos.col + 1;

    if (current_char == '\n') {
        pos.ln = pos.ln + 1;
        pos.col = 0;
    }
}

func abort_and_clean(status -> Int) -> Void {
    if (ACTIVE_FILE is !null) {
        ACTIVE_FILE.close();
    }
    if (CLEAN_TMP_LL.length() > 0) {
        file.remove(CLEAN_TMP_LL)?;
        catch(err) { }
    }
    process.exit(status);
}

func report_error(pos -> Position, name -> String, details -> String) -> Void {
    GLOBAL_ERROR_COUNT = GLOBAL_ERROR_COUNT + 1;

    let ln -> Int = pos.ln + 1;
    let col -> Int = pos.col + 1;

    if (ERROR_BUFFER is null) { ERROR_BUFFER = []; }
    let err_msg -> String = "";

    if (LAST_ERROR_FILE != pos.fn) {
        err_msg += "In file:\n";
    }
    err_msg += "   " + pos.fn + ":" + ln + ":" + col + "\n   | \n";
    LAST_ERROR_FILE = pos.fn;

    let text -> String = pos.text;
    let target_ln -> Int = pos.ln;
    let current_ln -> Int = 0;
    let start_idx -> Int = 0;
    let i -> Int = 0;

    while (i < text.length() && current_ln < target_ln) {
        if (text[i] == '\n') { // '\n'
            current_ln += 1;
            start_idx = i + 1;
        }
        i += 1;
    }

    let end_idx -> Int = start_idx;
    while (end_idx < text.length() && text[end_idx] != '\n' && text[end_idx] != '\r') {
        end_idx += 1;
    }

    if (start_idx < text.length()) {
        let line_text -> String = text.slice(start_idx, end_idx);
        
        let ln_str -> String = "" + ln;
        let ln_width -> Int = ln_str.length();
        
        let empty_prefix -> String = "  ";
        let p1 -> Int = 0;
        while (p1 < ln_width) { empty_prefix = empty_prefix + " "; p1 += 1; }
        
        err_msg += " " + ln_str + " | " + line_text + "\n";

        let err_len -> Int = 1;
        let line_len -> Int = line_text.length();
        if (pos.col < line_len) {
            let ch -> Char = line_text[pos.col];
            if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' || (ch >= '0' && ch <= '9')) {
                let cur -> Int = pos.col + 1;
                while (cur < line_len) {
                    let c2 -> Char = line_text[cur]; 
                    if ((c2 >= 'A' && c2 <= 'Z') || (c2 >= 'a' && c2 <= 'z') || c2 == '_' || (c2 >= '0' && c2 <= '9')) {
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
            let ch -> Char = ' ';
            if (j < line_len) { ch = line_text[j]; }

            if (ch == '\t') { 
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
        
        err_msg += caret_line + "\n";
    }

    err_msg += name + ": " + details + "\n\n";
    ERROR_BUFFER.append(err_msg);

    if (GLOBAL_ERROR_COUNT > 50) {
        ERROR_BUFFER.append("fatal error: too many errors emitted, stopping now\n");
        check_errors_and_abort();
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

func throw_internal_compiler_error(pos -> Position, details -> String) -> Void {
    if (pos is null) {
        builtin.print("InternalCompilerError: " + details);
        abort_and_clean(1);
        return;
    }
    report_error(pos, "InternalCompilerError", details);
}

func throw_zero_division_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "ZeroDivisionError", details);
}

func throw_overflow_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "OverflowError", details);
}

func throw_extern_error(pos -> Position, details -> String) -> Void {
    report_error(pos, "ExternError", details);
}

func throw_missing_main_function() -> Void { // special
    builtin.print("MissingMainFunction: No 'main' function defined.");
    abort_and_clean(1);
}

func throw_environment_error(details -> String) -> Void { // special
    builtin.print("EnvironmentError: " + details);
    abort_and_clean(1);
}

func check_errors_and_abort() -> Void {
    if (GLOBAL_ERROR_COUNT > 0) {
        let i -> Int = 0;
        let buf_len -> Int = 0;
        if (ERROR_BUFFER is !null) { buf_len = ERROR_BUFFER.length(); }
        while (i < buf_len) {
            builtin.print(ERROR_BUFFER[i]);
            i += 1;
        }

        let suffix -> String = " error.\n";
        if (GLOBAL_ERROR_COUNT > 1) { suffix = " errors.\n"; }
        builtin.print("Found " + GLOBAL_ERROR_COUNT + suffix);
        abort_and_clean(1);
    }
}
