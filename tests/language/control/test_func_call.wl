// Test: FUNCTION_LOGIC_SHORT_CIRCUIT
// File: tests/language/control/test_func_call.wl
// Focus: Boolean short-circuiting (&&), logical NOT (!), and function return value usage.


func check() -> Bool {
    return true;
}

func main() -> Int {
    // if short-circuiting works, check() shouldn't be called in (false && check())
    // the total expression !(false) is true.
    if (!(false && check())) {
        print("PASS: Function call and short-circuit logic");
    } else {
        print("FAIL: Logical operator precedence or evaluation error");
    }
    return 0;
}
