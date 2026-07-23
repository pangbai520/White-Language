import "sys"
import "internal/platform/windows"
import "internal/platform/posix"
import "internal/runtime"
import "internal/runtime/string" as runtime_string

let console_init_done -> Bool = false;

@CompilerLink("print_bytes")
func write_bytes(data -> AnyPtr, length -> Int) -> Void {
    if (data is nullptr) {
        write_char('n');
        write_char('u');
        write_char('l');
        write_char('l');
        return;
    }

    if (sys.OS == "WINDOWS") {
        let handle -> AnyPtr = windows.GetStdHandle(windows.STD_OUTPUT_HANDLE);
        if (handle is !nullptr) {
            if (console_init_done == false) {
                windows.SetConsoleOutputCP(windows.CP_UTF8);
                console_init_done = true;
            }
            let bytes_written -> Int = 0;
            windows.WriteFile(handle, data, length, ref bytes_written, nullptr);
        }
    } else {
        posix.write(1, data, Long(length));
    }
}

@CompilerLink("print_raw_string")
func write_raw_string(data -> AnyPtr) -> Void {
// raw pointers have no length; write one byte at a time to prevent strlen folding
    if (data is nullptr) {
        write_bytes(nullptr, 0);
        return;
    }
    let ptr bytes -> Byte = data;
    let i -> Int = 0;
    while (bytes[i] != 0) {
        write_char(Char(bytes[i]));
        i += 1;
    }
}

@CompilerLink("print_char")
func write_char(c -> Char) -> Void {
    let char_buf -> Byte = Byte(c);
    if (sys.OS == "WINDOWS") {
        let handle -> AnyPtr = windows.GetStdHandle(windows.STD_OUTPUT_HANDLE);
        if (handle is !nullptr) {
            if (console_init_done == false) {
                windows.SetConsoleOutputCP(windows.CP_UTF8);
                console_init_done = true;
            }
            let bytes_written -> Int = 0;
            windows.WriteFile(handle, ref char_buf, 1, ref bytes_written, nullptr);
        }
    } else {
        posix.write(1, ref char_buf, 1L);
    }
}

func write_int_digits(value -> Int) -> Void {
    if (value == 0) { return; }
    write_int_digits(value / 10);
    let digit -> Int = value % 10;
    if (digit < 0) { digit = -digit; }
    write_char(Char(digit + 48));
}

@CompilerLink("print_int")
func write_int(value -> Int) -> Void {
    if (value == 0) { write_char('0'); return; }
    if (value < 0) { write_char('-'); }
    write_int_digits(value);
}

func write_long_digits(value -> Long) -> Void {
    if (value == 0L) { return; }
    write_long_digits(value / 10L);
    let digit -> Long = value % 10L;
    if (digit < 0) { digit = -digit; }
    write_char(Char(Int(digit) + 48));
}

@CompilerLink("print_long")
func write_long(value -> Long) -> Void {
    if (value == 0L) { write_char('0'); return; }
    if (value < 0L) { write_char('-'); }
    write_long_digits(value);
}

@CompilerLink("print_float")
func write_float(value -> Float) -> Void {
    let formatted -> String = runtime.format_float(value);
    write_bytes(runtime_string.data(formatted), formatted.length());
}

@CompilerLink("print_bool")
func write_bool(value -> Bool) -> Void {
    let s -> String = "false";
    if (value) { s = "true"; }
    write_bytes(runtime_string.data(s), s.length());
}

@CompilerLink
func print(s -> String) -> Void {
    if (s is null) { s = "null"; }
    write_bytes(runtime_string.data(s), s.length());
    let newline -> String = "\n";
    write_bytes(runtime_string.data(newline), 1);
}
