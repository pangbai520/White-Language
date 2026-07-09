import "sys"
import "sys/win32"

extern func write(fd -> Int, buf -> AnyPtr, count -> Long) -> Long from "C";

let console_init_done -> Bool = false;

@CompilerLink("print_raw_string")
func __wl_print_raw_string(p -> AnyPtr) -> Void {
    if (p is nullptr) {
        __wl_print_char('n');
        __wl_print_char('u');
        __wl_print_char('l');
        __wl_print_char('l');
        return;
    }
    let ptr byte_ptr -> Byte = p;
    let len -> Int = 0;
    while (byte_ptr[len] != 0) { len = len + 1; }
    
    if (sys.OS == "WINDOWS") {
        let handle -> AnyPtr = win32.GetStdHandle(win32.STD_OUTPUT_HANDLE);
        if (handle is !nullptr) {
            if (console_init_done == false) {
                win32.SetConsoleOutputCP(win32.CP_UTF8);
                console_init_done = true;
            }
            let bytes_written -> Int = 0;
            win32.WriteFile(handle, p, len, ref bytes_written, nullptr);
        }
    } else {
        let len_long -> Long = len;
        write(1, p, len_long);
    }
}

@CompilerLink("print_char")
func __wl_print_char(c -> Char) -> Void {
    let char_buf -> Byte = Byte(c);
    if (sys.OS == "WINDOWS") {
        let handle -> AnyPtr = win32.GetStdHandle(win32.STD_OUTPUT_HANDLE);
        if (handle is !nullptr) {
            if (console_init_done == false) {
                win32.SetConsoleOutputCP(win32.CP_UTF8);
                console_init_done = true;
            }
            let bytes_written -> Int = 0;
            win32.WriteFile(handle, ref char_buf, 1, ref bytes_written, nullptr);
        }
    } else {
        write(1, ref char_buf, 1);
    }
}

func __wl_print_int_helper(v -> Int) -> Void {
    if (v == 0) { return; }
    __wl_print_int_helper(v / 10);
    let digit -> Int = v % 10;
    if (digit < 0) { digit = -digit; }
    __wl_print_char(Char(digit + 48));
}

@CompilerLink("print_int")
func __wl_print_int(v -> Int) -> Void {
    if (v == 0) { __wl_print_char('0'); return; }
    if (v < 0) { __wl_print_char('-'); }
    __wl_print_int_helper(v);
}

func __wl_print_long_helper(v -> Long) -> Void {
    if (v == 0) { return; }
    __wl_print_long_helper(v / 10);
    let digit -> Long = v % 10;
    if (digit < 0) { digit = -digit; }
    __wl_print_char(Char(Int(digit) + 48));
}

@CompilerLink("print_long")
func __wl_print_long(v -> Long) -> Void {
    if (v == 0) { __wl_print_char('0'); return; }
    if (v < 0) { __wl_print_char('-'); }
    __wl_print_long_helper(v);
}

@CompilerLink("print_float")
func __wl_print_float(v -> Float) -> Void {
    if (v < 0.0) { __wl_print_char('-'); v = -v; }
    let int_part -> Long = Long(v);
    __wl_print_long(int_part);
    __wl_print_char('.');
    let int_float -> Float = Float(int_part);
    let frac_part -> Float = v - int_float;
    let i -> Int = 0;
    while (i < 6) {
        frac_part = frac_part * 10.0;
        let digit -> Int = Int(frac_part);
        __wl_print_int(digit);
        let digit_float -> Float = Float(digit);
        frac_part = frac_part - digit_float;
        i = i + 1;
    }
}

@CompilerLink("print_bool")
func __wl_print_bool(v -> Bool) -> Void {
    let s -> String = "false";
    if (v) { s = "true"; }
    
    if (sys.OS == "WINDOWS") {
        let handle -> AnyPtr = win32.GetStdHandle(win32.STD_OUTPUT_HANDLE);
        if (handle is !nullptr) {
            let bytes_written -> Int = 0;
            win32.WriteFile(handle, s, s.length(), ref bytes_written, nullptr);
        }
    } else {
        let len_long -> Long = s.length();
        write(1, s, len_long);
    }
}

@CompilerLink
func print(s -> String) -> Void {
    if (s is null) { s = "null"; }
    if (sys.OS == "WINDOWS") {
        let handle -> AnyPtr = win32.GetStdHandle(win32.STD_OUTPUT_HANDLE);
        if (handle is !nullptr) {
            if (console_init_done == false) {
                win32.SetConsoleOutputCP(win32.CP_UTF8);
                console_init_done = true;
            }
            let bytes_written -> Int = 0;
            win32.WriteFile(handle, s, s.length(), ref bytes_written, nullptr);
            win32.WriteFile(handle, "\n", 1, ref bytes_written, nullptr);
        }
    } else {
        let len_long -> Long = s.length();
        write(1, s, len_long);
        write(1, "\n", 1);
    }
}
