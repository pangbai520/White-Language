// low-level String storage access shared by the standard library

extern "C" {
    func wl_alloc_string(size -> Long) -> String;
    func wl_string_set_length(value -> String, length -> Int) -> Void;
}

func alloc(length -> Long) -> String {
    return wl_alloc_string(length);
}

func data(value -> String) -> AnyPtr {
    if (value is null) { return nullptr; }
    let ptr fields -> AnyPtr = AnyPtr(value);
    return fields[0];
}

func set_length(value -> String, length -> Int) -> Void {
    wl_string_set_length(value, length);
}
