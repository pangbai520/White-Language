// Test: RELATIVE_INTERNAL_PATH
// File: tests/language/modules/test_internal_relative.wl
// Focus: Internal restrictions do not reject ordinary relative source files outside the standard library.

import value from "../../fixtures/modules/internal/provider.wl"

func main() -> Int {
    if (value() != 37) {
        print("FAIL: ordinary relative internal path was resolved incorrectly");
        return 1;
    }
    print("PASS: ordinary relative internal path");
    return 0;
}
