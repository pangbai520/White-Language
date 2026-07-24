// Test: DUPLICATE_FUNCTION
// File: tests/diagnostics/failures/test_duplicate_function.wl
// Focus: Duplicate declarations must produce a diagnostic instead of crashing

func duplicate() -> Int {
    return 1;
}

func duplicate() -> Int {
    return 2;
}

func main() -> Int {
    return duplicate();
}
