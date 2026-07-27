// Test: NAMED_IMPORT_NAMESPACE
// File: tests/diagnostics/failures/test_named_import_namespace.wl
// Focus: A named import binds the requested symbol, not the source namespace.
// Expected Error: "NameError: Undefined variable or function 'provider'."

import label from "../../fixtures/modules/left/provider.wl"

func main() -> Int {
    return provider.label().length();
}
