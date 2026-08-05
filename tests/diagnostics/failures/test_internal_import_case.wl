// Test: INTERNAL_IMPORT_CASE
// File: tests/diagnostics/failures/test_internal_import_case.wl
// Focus: Internal standard library import restrictions are case-insensitive on every platform.
// Expected Error: "ImportError: Module 'Internal/Runtime' is internal to the standard library."

import "Internal/Runtime"

func main() -> Int {
    return 0;
}
