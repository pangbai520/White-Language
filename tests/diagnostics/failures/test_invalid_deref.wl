// Test: INVALID_DEREFERENCE
// File: tests/diagnostics/failures/test_invalid_deref.wl
// Focus: Dereferencing a value must produce a type diagnostic

func main() -> Int {
    let value -> Int = 7;
    return deref value;
}
