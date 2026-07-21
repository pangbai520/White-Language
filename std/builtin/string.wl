// std/builtin/string.wl
//
// Standard String Library.
// This file defines low-level string operations used by the compiler
// and standard library. All functions here assume null-terminated
// byte strings and explicit heap ownership.

// Using string_XXX to represent the methods of string obj.

// ----------------------------------------------------------
// C runtime bindings (memory management)
// ----------------------------------------------------------
extern "C" {
    func wl_alloc_string(size -> Long) -> String;
    func __wl_str_set(s -> String, idx -> Int, val -> Int) -> Void;
    func __wl_str_get(s -> String, idx -> Int) -> Byte;
    func wl_string_at(s -> String, idx -> Int) -> Char;
    func wl_string_slice(s -> String, start -> Int, end -> Int) -> String;
    func wl_string_ends_with(s -> String, suffix -> String) -> Int;
    func wl_string_starts_with(s -> String, prefix -> String) -> Int;
}

// ==========================================================
// String methods (Built-in 'str.length()')
// ==========================================================
@CompilerLink
func string_at(self -> String, idx -> Int) -> Char { // str.at()
    return wl_string_at(self, idx);
}

@CompilerLink
func string_slice(self -> String, start -> Int, end -> Int) -> String { // str.slice()
    return wl_string_slice(self, start, end);
}

@CompilerLink
func string_ends_with(s -> String, suffix -> String) -> Bool {
    return wl_string_ends_with(s, suffix) != 0;
}

@CompilerLink
func string_starts_with(s -> String, prefix -> String) -> Bool {
    return wl_string_starts_with(s, prefix) != 0;
}
