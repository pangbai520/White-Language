// Test: CONST_BINDING_VISIBILITY
// File: tests/basics/test_const_spec.wl
// Focus: Global constant value accessibility and string-int fusion.
import "builtin"

const LANG_NAME -> String = "White Language v0.";
const VERSION -> Int = 1;

func main() -> Int {
    let expected -> String = "White Language v0.1";
    let actual -> String = LANG_NAME + VERSION;

    if (actual == expected) {
        builtin.print("PASS: Constant binding and fusion");
    } else {
        builtin.print("FAIL: Constant string concatenation mismatch");
    }
    return 0;
}