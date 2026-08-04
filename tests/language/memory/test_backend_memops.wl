// Test: BACKEND_MEMORY_OPERATIONS
// File: tests/language/memory/test_backend_memops.wl
// Focus: Linking and executing memory operations synthesized by the optimized Windows backend.


extern "C" {
    func memcpy(dest -> AnyPtr, src -> AnyPtr, count -> Long) -> AnyPtr;
    func memmove(dest -> AnyPtr, src -> AnyPtr, count -> Long) -> AnyPtr;
    func memset(dest -> AnyPtr, value -> Int, count -> Long) -> AnyPtr;
    func wl_alloc_string(size -> Long) -> String;
    func wl_string_set_length(value -> String, length -> Int) -> Void;
}

func string_data(value -> String) -> AnyPtr {
    let ptr fields -> AnyPtr = AnyPtr(value);
    return fields[0];
}

func shift_left(value -> String) -> Void {
    let ptr bytes -> Byte = string_data(value);
    let index -> Int = 1;
    while (index < value.length()) {
        bytes[index - 1] = bytes[index];
        index += 1;
    }
    wl_string_set_length(value, value.length() - 1);
}

func main() -> Int {
    let source -> String = "ABCDEF";
    let value -> String = wl_alloc_string(6L);
    memcpy(string_data(value), string_data(source), 6L);

    shift_left(value);
    if (value != "BCDEF") {
        print("FAIL: Optimized overlapping copy was corrupted");
        return 1;
    }

    let moved -> String = wl_alloc_string(5L);
    memmove(string_data(moved), string_data(value), 5L);
    if (moved != "BCDEF") {
        print("FAIL: Runtime memmove returned corrupted data");
        return 1;
    }

    let filled -> String = wl_alloc_string(4L);
    memset(string_data(filled), Int('x'), 4L);
    if (filled != "xxxx") {
        print("FAIL: Runtime memset returned corrupted data");
        return 1;
    }

    print("PASS: Optimized backend memory operations");
    return 0;
}
