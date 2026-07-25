// Test: PRIVATE_MODULE_DEPENDENCY
// File: tests/language/modules/test_private_dependency.wl
// Focus: Ordinary imports stay private while their own module can use them

import "builtin"
import "../../fixtures/modules/dependency/provider.wl" as provider

func main() -> Int {
    if (provider.exposed_value() != 42) { return 1; }
    builtin.print("PASS: private module dependency");
    return 0;
}
