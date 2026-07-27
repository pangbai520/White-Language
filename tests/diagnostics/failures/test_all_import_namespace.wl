// Test: ALL_IMPORT_NAMESPACE
// File: tests/diagnostics/failures/test_all_import_namespace.wl
// Focus: A star import binds public symbols without binding the source namespace.
// Expected Error: "NameError: Undefined variable or function 'provider'."

import * from "../../fixtures/modules/left/provider.wl"

func main() -> Int {
    return provider.label().length();
}
