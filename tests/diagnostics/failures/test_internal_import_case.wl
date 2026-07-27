// Test: INTERNAL_IMPORT_CASE
// File: tests/diagnostics/failures/test_internal_import_case.wl
// Focus: Internal standard library import restrictions remain case-insensitive on Windows.
// Expected Error: "ImportError: Module 'Internal/Runtime' is internal to the standard library."

import "Internal/Runtime"

func main() -> Int {
    return 0;
}
