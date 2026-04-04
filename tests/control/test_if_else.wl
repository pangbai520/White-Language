// Test: IF_ELSE_CONDITIONAL
// File: tests/control/test_if_else.wl
// Focus: Boolean condition evaluation, code block scoping, and post-increment side effects.
import "builtin"

func main() -> Int {
    let a -> Int = 10;

    if (true) {
        a++;
        a++;
    }

    // expected 12
    if (a == 12) {
        builtin.print("PASS: Conditional branching and increment");
    } else {
        builtin.print("FAIL: Condition logic or increment error. Got: " + a);
    }
    return 0;
}