// Test: SYMBOL_IMPORT_MODULE_ALIAS
// File: tests/diagnostics/failures/test_symbol_import_module_alias.wl
// Focus: Symbol aliases belong before from and cannot be mixed with a module alias

import label from "../../fixtures/modules/left/provider.wl" as provider

func main() -> Int {
    return label().length();
}
