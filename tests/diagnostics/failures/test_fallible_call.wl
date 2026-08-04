// Test: FALLIBLE_CALL
// File: tests/diagnostics/failures/test_fallible_call.wl
// Focus: Requiring every discarded fallible call to be handled.
// Expected Error: "TypeError: call to fallible function 'read_value' requires '?'"

func read_value() -> Int? {
    return 1;
}

func main() -> Int {
    read_value();
    return 0;
}
