// Test: MODULE_ALIAS_COLLISION
// File: tests/diagnostics/failures/test_module_alias_collision.wl
// Focus: One source name cannot refer to two different modules

import "../../fixtures/modules/left/provider.wl" as provider
import "../../fixtures/modules/right/provider.wl" as provider

func main() -> Int {
    return 0;
}
