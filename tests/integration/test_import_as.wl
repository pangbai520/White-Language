// Test: IMPORT_ALIASING
// File: tests/integration/test_import_as.wl
// Focus: Testing 'import as' syntax for both specific functions and entire modules.
import add_int as asd from "fixtures/module_add.wl"
import "builtin" as bt

func main() -> Int {
    // using aliased function and aliased module
    let result -> Int = asd(1, 2);
    
    if (result == 3) {
        bt.print("PASS: Import aliasing and cross-file calls");
    } else {
        bt.print("FAIL: Alias resolution or return value mismatch");
    }
    return 0;
}