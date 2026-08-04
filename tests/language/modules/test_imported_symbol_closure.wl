// Test: IMPORTED_SYMBOL_IN_CLOSURE
// File: tests/language/modules/test_imported_symbol_closure.wl
// Focus: Imported functions and module namespaces are not mistaken for captures

import label from "../../fixtures/modules/left/provider.wl"
import "../../fixtures/modules/right/provider.wl" as right

func main() -> Int {
    func describe() -> String {
        return label() + "-" + right.label();
    }

    if (describe() != "left-right") { return 1; }
    print("PASS: imported symbol closure");
    return 0;
}
