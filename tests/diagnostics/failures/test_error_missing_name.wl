// Test: ERROR_MISSING_NAME
// File: tests/diagnostics/failures/test_error_missing_name.wl
// Focus: An error declaration must provide a type name.
// Expected Error: "InvalidSyntax: expected a name after 'error'"

error {
    Invalid
}

func main() -> Int {
    return 0;
}
