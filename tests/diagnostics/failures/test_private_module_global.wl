// Test: PRIVATE_MODULE_GLOBAL
// File: tests/diagnostics/failures/test_private_module_global.wl
// Focus: Private globals cannot be read through a module namespace.
// Expected Error: "NameError: Undefined module variable 'provider.__private_marker'."

import "../../fixtures/modules/left/provider.wl" as provider

func main() -> Int {
    return provider.__private_marker;
}
