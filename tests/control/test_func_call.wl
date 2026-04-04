// Test: FUNCTION_LOGIC_SHORT_CIRCUIT
// File: tests/control/test_func_call.wl
// Focus: Boolean short-circuiting (&&), logical NOT (!), and function return value usage.
import "builtin"

func check() -> Bool {
    return true;
}

func main() -> Int {
    // if short-circuiting works, check() shouldn't be called in (false && check())
    // the total expression !(false) is true.
    if (!(false && check())) {
        builtin.print("PASS: Function call and short-circuit logic");
    } else {
        builtin.print("FAIL: Logical operator precedence or evaluation error");
    }
    return 0;
}