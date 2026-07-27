// Test: ERROR_KEYWORD
// File: tests/diagnostics/failures/test_error_keyword.wl
// Focus: The reserved word 'error' cannot be used as a function name.
// Expected Error: "InvalidSyntax: expected a function name after 'func'"

func error() -> Int {
    return 1;
}

func main() -> Int {
    return 0;
}
