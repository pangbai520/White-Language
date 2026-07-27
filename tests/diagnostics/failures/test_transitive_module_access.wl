// Test: TRANSITIVE_MODULE_ACCESS
// File: tests/diagnostics/failures/test_transitive_module_access.wl
// Focus: Importing a file does not expose that file's private dependencies.
// Expected Error: "NameError: Function 'provider.hidden.hidden_value' is not defined."

import "../../fixtures/modules/dependency/provider.wl" as provider

func main() -> Int {
    return provider.hidden.hidden_value();
}
