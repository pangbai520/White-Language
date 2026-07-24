// Fest: STANDARD_LIBRARY_INTERNAL_IMPORT
// File: tests/diagnostics/failures/test_internal_import.wl
// Focus: Internal standard library modules are not part of the public API.

import "internal/runtime"

func main() -> Int {
    return 0;
}
