// std/builtin/string.wl
//
// Core string utilities and method extensions.
// This file defines low-level string operations used by the compiler
// and standard library. All functions here assume null-terminated
// byte strings and explicit heap ownership.

// Using string_XXX to represent the methods of string obj.

// ----------------------------------------------------------
// C runtime bindings (memory management)
// ----------------------------------------------------------
//
// The String type is represented as a pointer to heap-allocated
// null-terminated bytes. Allocation and deallocation are delegated
// to the C runtime.
extern "C" {
    func malloc(size -> Long) -> String;
    func free(p -> String) -> Void;
}

// ==========================================================
// String method extensions
//
// These functions are exposed as instance-style methods by
// compiler desugaring. For example:
//
//     s.slice(a, b)
//
// is lowered to:
//
//     string_slice(s, a, b)
// ==========================================================


// ----------------------------------------------------------
// 1. Indexed access with bounds checking
// ----------------------------------------------------------
//
// Returns the byte at the given index.
// Bounds checking is enforced by the compiler-emitted guard
// before the actual memory access.
func string_at(self -> String, idx -> Int) -> Byte {
    return self[idx];
}


// ----------------------------------------------------------
// 2. Substring slicing
// ----------------------------------------------------------
//
// Semantics:
//   - Extracts the range [start, end)
//   - `start` is inclusive, `end` is exclusive
//   - Out-of-range indices are clamped
//   - Invalid or empty ranges return a valid empty string
//
// Memory model:
//   - Always allocates a new heap string
//   - Caller owns the returned string
func string_slice(self -> String, start -> Int, end -> Int) -> String {
    let self_len -> Int = self.length;

    // Clamp indices to valid bounds
    if (start < 0) { start = 0; }
    if (end > self_len) { end = self_len; }

    // Empty or invalid range: return an allocated empty string
    if (start >= end) {
        let empty -> String = malloc(1);
        empty[0] = 0;
        return empty;
    }

    let new_len -> Int = end - start;

    // Allocate buffer for substring plus null terminator
    let new_str -> String = malloc((new_len + 1));

    // Byte-wise copy
    let i -> Int = 0;
    while (i < new_len) {
        new_str[i] = self[start + i];
        i++;
    }

    // Ensure null termination
    new_str[new_len] = 0;

    return new_str;
}
