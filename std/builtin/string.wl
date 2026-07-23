// std/builtin/string.wl
//
// String operations use the stored length and never scan for a terminator

import "internal/runtime/string" as runtime_string

func string_value_at(self -> String, idx -> Int) -> Char {
    if (self is null || idx < 0 || idx >= self.length()) { return '\0'; }
    let ptr bytes -> Byte = runtime_string.data(self);
    return Char(bytes[idx]);
}

func string_value_slice(self -> String, start -> Int, end -> Int) -> String {
    if (self is null) { return null; }
    let source_len -> Int = self.length();
    if (start < 0) { start = 0; }
    if (end > source_len) { end = source_len; }
    if (start > end) { start = end; }

    let result_len -> Int = end - start;
    let result -> String = runtime_string.alloc(Long(result_len));
    if (result is null || result_len == 0) { return result; }

    let ptr source -> Byte = runtime_string.data(self);
    let ptr output -> Byte = runtime_string.data(result);
    let i -> Int = 0;
    while (i < result_len) {
        output[i] = source[start + i];
        i += 1;
    }
    return result;
}

@CompilerLink("string_concat")
func string_concat(left -> String, right -> String) -> String {
    if (left is null || right is null) { return null; }

    let left_len -> Int = left.length();
    let right_len -> Int = right.length();
    let total_len -> Long = Long(left_len) + Long(right_len);
    let result -> String = runtime_string.alloc(total_len);
    if (result is null) { return null; }

    let ptr output -> Byte = runtime_string.data(result);
    let ptr left_bytes -> Byte = runtime_string.data(left);
    let ptr right_bytes -> Byte = runtime_string.data(right);
    let i -> Int = 0;
    while (i < left_len) {
        output[i] = left_bytes[i];
        i += 1;
    }
    let j -> Int = 0;
    while (j < right_len) {
        output[left_len + j] = right_bytes[j];
        j += 1;
    }
    return result;
}

@CompilerLink("string_compare")
func string_compare(left -> String, right -> String) -> Int {
    if (left is right) { return 0; }
    if (left is null) { return -1; }
    if (right is null) { return 1; }

    let left_len -> Int = left.length();
    let right_len -> Int = right.length();
    let common_len -> Int = left_len;
    if (right_len < common_len) { common_len = right_len; }

    let ptr left_bytes -> Byte = runtime_string.data(left);
    let ptr right_bytes -> Byte = runtime_string.data(right);
    let i -> Int = 0;
    while (i < common_len) {
        let lhs -> Int = Int(left_bytes[i]);
        let rhs -> Int = Int(right_bytes[i]);
        if (lhs < rhs) { return -1; }
        if (lhs > rhs) { return 1; }
        i += 1;
    }

    if (left_len < right_len) { return -1; }
    if (left_len > right_len) { return 1; }
    return 0;
}

func string_value_starts_with(self -> String, prefix -> String) -> Bool {
    if (self is null || prefix is null) { return false; }
    let self_len -> Int = self.length();
    let prefix_len -> Int = prefix.length();
    if (prefix_len > self_len) { return false; }

    let ptr source -> Byte = runtime_string.data(self);
    let ptr expected -> Byte = runtime_string.data(prefix);
    let i -> Int = 0;
    while (i < prefix_len) {
        if (source[i] != expected[i]) { return false; }
        i += 1;
    }
    return true;
}

func string_value_ends_with(self -> String, suffix -> String) -> Bool {
    if (self is null || suffix is null) { return false; }
    let self_len -> Int = self.length();
    let suffix_len -> Int = suffix.length();
    if (suffix_len > self_len) { return false; }

    let offset -> Int = self_len - suffix_len;
    let ptr source -> Byte = runtime_string.data(self);
    let ptr expected -> Byte = runtime_string.data(suffix);
    let i -> Int = 0;
    while (i < suffix_len) {
        if (source[offset + i] != expected[i]) { return false; }
        i += 1;
    }
    return true;
}

@CompilerLink
func string_at(self -> String, idx -> Int) -> Char {
    return string_value_at(self, idx);
}

@CompilerLink
func string_slice(self -> String, start -> Int, end -> Int) -> String {
    return string_value_slice(self, start, end);
}

@CompilerLink
func string_ends_with(self -> String, suffix -> String) -> Bool {
    return string_value_ends_with(self, suffix);
}

@CompilerLink
func string_starts_with(self -> String, prefix -> String) -> Bool {
    return string_value_starts_with(self, prefix);
}
