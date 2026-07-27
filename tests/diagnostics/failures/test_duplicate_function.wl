// Test: DUPLICATE_FUNCTION
// File: tests/diagnostics/failures/test_duplicate_function.wl
// Focus: Duplicate function declarations must produce a name diagnostic.
// Expected Error: "NameError: Function 'duplicate' is already defined."

func duplicate() -> Int {
    return 1;
}

func duplicate() -> Int {
    return 2;
}

func main() -> Int {
    return duplicate();
}
