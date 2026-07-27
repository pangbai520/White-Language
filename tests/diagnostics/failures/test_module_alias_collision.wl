// Test: MODULE_ALIAS_COLLISION
// File: tests/diagnostics/failures/test_module_alias_collision.wl
// Focus: One source name cannot refer to two different modules.
// Expected Error: "ImportError: Module name 'provider' is already bound to another module."

import "../../fixtures/modules/left/provider.wl" as provider
import "../../fixtures/modules/right/provider.wl" as provider

func main() -> Int {
    return 0;
}
