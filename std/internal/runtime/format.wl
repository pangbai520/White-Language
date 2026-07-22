// formatting hooks used by compiler-generated conversions

import "builtin/string"

extern "C" {
    func wl_string_set_length(value -> String, length -> Int) -> Void;
    func wl_format_u128(buffer -> AnyPtr, low -> UInt64, high -> UInt64) -> Int;
}

func signed_decimal_digits(value -> Long) -> Int {
    let probe -> Long = value;
    let digits -> Int = 1;
    while (probe <= -10L || probe >= 10L) {
        probe /= 10L;
        digits += 1;
    }
    if (value < 0L) { digits += 1; }
    return digits;
}

@CompilerLink("format_long")
func format_long(value -> Long) -> String {
    let length -> Int = signed_decimal_digits(value);
    let result -> String = string.wl_alloc_string(Long(length));
    if (result is null) { return null; }

    let ptr output -> Byte = string.string_data(result);
    let negative -> Bool = value < 0L;
    // stay negative so Long.MIN does not overflow
    let work -> Long = value;
    if (!negative) { work = 0L - work; }

    let pos -> Int = length - 1;
    while true {
        let digit -> Int = Int(0L - (work % 10L));
        output[pos] = Byte(digit + 48);
        work /= 10L;
        if (work == 0L) { break; }
        pos -= 1;
    }
    if (negative) { output[0] = Byte(45); }
    return result;
}

@CompilerLink("format_int")
func format_int(value -> Int) -> String {
    return format_long(Long(value));
}

@CompilerLink("format_uint64")
func format_uint64(value -> UInt64) -> String {
    let result -> String = string.wl_alloc_string(20L);
    if (result is null) { return null; }
    let length -> Int = wl_format_u128(string.string_data(result), value, UInt64(0));
    wl_string_set_length(result, length);
    return result;
}

@CompilerLink("format_float")
func format_float(value -> Float) -> String {
    let negative -> Bool = value < 0.0;
    if (negative) { value = -value; }

    let whole_value -> Long = Long(value);
    let fraction_value -> Long = Long((value - Float(whole_value)) * 1000000.0 + 0.5);
    if (fraction_value >= 1000000L) {
        whole_value += 1L;
        fraction_value -= 1000000L;
    }

    let whole -> String = format_long(whole_value);
    if (negative) { whole = string.string_concat("-", whole); }

    let fraction -> String = string.wl_alloc_string(7L);
    if (fraction is null) { return null; }
    let ptr output -> Byte = string.string_data(fraction);
    output[0] = Byte(46);

    let pos -> Int = 6;
    while (pos >= 1) {
        output[pos] = Byte(Int(fraction_value % 10L) + 48);
        fraction_value /= 10L;
        pos -= 1;
    }
    return string.string_concat(whole, fraction);
}
