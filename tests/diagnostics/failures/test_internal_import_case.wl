// Test: INTERNAL_IMPORT_CASE
// File: tests/diagnostics/failures/test_internal_import_case.wl
// Focus: Internal import restrictions remain case-insensitive on Windows

import "Internal/Runtime"

func main() -> Int {
    return 0;
}
