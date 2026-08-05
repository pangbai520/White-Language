// Test: INTERNAL_IMPORT_UPPERCASE
// File: tests/diagnostics/failures/test_internal_import_upper.wl
// Focus: Uppercase paths cannot bypass the internal standard library boundary.
// Expected Error: "ImportError: Module 'INTERNAL/RUNTIME' is internal to the standard library."

import "INTERNAL/RUNTIME"

func main() -> Int {
    return 0;
}
