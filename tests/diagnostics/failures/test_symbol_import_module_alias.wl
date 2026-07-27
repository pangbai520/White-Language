// Test: SYMBOL_IMPORT_MODULE_ALIAS
// File: tests/diagnostics/failures/test_symbol_import_module_alias.wl
// Focus: A named symbol import cannot also declare a module alias.
// Expected Error: "InvalidSyntax: A module alias can only be used with 'import "module" as name'. Alias imported symbols before 'from'."

import label from "../../fixtures/modules/left/provider.wl" as provider

func main() -> Int {
    return label().length();
}
