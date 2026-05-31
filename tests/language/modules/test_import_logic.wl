// Test: MULTI_MODULE_INTEGRATION
// File: tests/language/integration/test_import_logic.wl
// Focus: Cross-file function invocation, built-in library resolution, and string method intrinsics.
import "../../fixtures/pkgs/math_calc_helper.wl"
import "builtin"


func main() -> Int {
    // cross-module function call
    let sum -> Int = math_calc_helper.add_int(2, 3);

    // slice method
    let text -> String = "WhiteLang";
    let sub -> String = text.slice(0, 5); // expected: "White"

    if (sum == 5 && sub == "White") {
        builtin.print("PASS: Multi-module integration and intrinsics");
    } else {
        builtin.print("FAIL: Import logic or string slicing mismatch");
    }

    return 0;
}
