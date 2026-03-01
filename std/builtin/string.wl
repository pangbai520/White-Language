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
}

// ==========================================================
// String methods (Built-in 'str.length()')
// ==========================================================
func string_at(self -> String, idx -> Int) -> Byte { // str.at()
    return self[idx];
}

func string_slice(self -> String, start -> Int, end -> Int) -> String { // str.slice()
    let self_len -> Int = self.length();

    if (start < 0) { start = 0; }
    if (end > self_len) { end = self_len; }
    if (start >= end) {
        return wl_alloc_string(0);
    }

    let new_len -> Long = end - start;
    let new_str -> String = wl_alloc_string(new_len);
    let i -> Int = 0;
    while (i < new_len) {
        let ch -> Int = __wl_str_get(self, start + i);
        __wl_str_set(new_str, i, ch);
        i++;
    }

    return new_str;
}

func string_ends_with(s -> String, suffix -> String) -> Bool {
    let s_len -> Int = s.length();
    let sub_len -> Int = suffix.length();
    
    if (sub_len > s_len) { return false; }
    
    let i -> Int = 0;
    while (i < sub_len) {
        if (s[s_len - sub_len + i] != suffix[i]) {
            return false;
        }
        i += 1;
    }
    return true;
}
