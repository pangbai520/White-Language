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

@CompilerLink("utf8_encode_char")
func encode_utf8_char(value -> Char) -> String {
    // encode one Unicode scalar as UTF-8
    let scalar -> Int = Int(value);
    if (scalar < 0 || scalar > 1114111 || (scalar >= 55296 && scalar <= 57343)) {
        return null;
    }

    let width -> Int = 1;
    if (scalar > 127) { width = 2; }
    if (scalar > 2047) { width = 3; }
    if (scalar > 65535) { width = 4; }

    let result -> String = alloc(Long(width));
    if (result is null) { return null; }
    let ptr bytes -> Byte = data(result);

    if (width == 1) {
        bytes[0] = Byte(scalar);
    } else if (width == 2) {
        bytes[0] = Byte(192 | (scalar >> 6));
        bytes[1] = Byte(128 | (scalar & 63));
    } else if (width == 3) {
        bytes[0] = Byte(224 | (scalar >> 12));
        bytes[1] = Byte(128 | ((scalar >> 6) & 63));
        bytes[2] = Byte(128 | (scalar & 63));
    } else {
        bytes[0] = Byte(240 | (scalar >> 18));
        bytes[1] = Byte(128 | ((scalar >> 12) & 63));
        bytes[2] = Byte(128 | ((scalar >> 6) & 63));
        bytes[3] = Byte(128 | (scalar & 63));
    }
    return result;
}
