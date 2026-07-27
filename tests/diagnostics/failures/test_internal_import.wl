// Test: STANDARD_LIBRARY_INTERNAL_IMPORT
// File: tests/diagnostics/failures/test_internal_import.wl
// Focus: Internal standard library modules are not part of the public API.
// Expected Error: "ImportError: Module 'internal/runtime' is internal to the standard library."

import "internal/runtime"

func main() -> Int {
    return 0;
}
