// Test: PACKAGE_SCOPE_VISIBILITY
// File: tests/language/integration/test_package_scope.wl
// Focus: Verification of symbol visibility across multiple files without explicit aliases.
import "builtin"
import "../../fixtures/pkgs/math_calc_helper.wl"


func main() -> Int {
    // add_int should be available in the current scope from module_add.wl
    let sum -> Int = math_calc_helper.add_int(1, 2);
    
    if (sum == 3) {
        builtin.print("PASS: Package scope symbol visibility");
    } else {
        builtin.print("FAIL: Symbol 'add_int' not found or incorrect");
    }
    return 0;
}
