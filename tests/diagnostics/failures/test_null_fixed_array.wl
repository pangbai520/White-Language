// test: NULL_FIXED_ARRAY
// file: tests/diagnostics/failures/test_null_fixed_array.wl
// focus: null cannot initialize an inline fixed array

func main() -> Int {
    let values -> Array(Int, 2) = null;
    return values[0];
}
