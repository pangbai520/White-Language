// Test: MODULE_PRELUDE
// File: tests/language/modules/test_module_prelude.wl
// Focus: Binding builtin, Error, and Dict prelude symbols in every module.

import check_module_prelude from "../../fixtures/modules/prelude_source.wl"

func main() -> Int {
    if (!check_module_prelude()) {
        print("FAIL: imported module prelude");
        return 1;
    }
    print("PASS: imported module prelude");
    return 0;
}
