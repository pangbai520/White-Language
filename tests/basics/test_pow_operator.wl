// Test: POW_FUNCTIONALITY
// File: tests/basics/test_pow_operator.wl
// Focus: Power operator (**) precedence, calculation accuracy, and right-associativity.
import "builtin"

func main() -> Int {
    let a -> Int = 2;
    let b -> Int = 3;

    // 2^3 = 8
    if (a ** b == 8) {
        builtin.print("PASS: Basic power calculation");
    } else {
        builtin.print("FAIL: Power calculation error");
    }

    // right-associativity test: 2 ** (3 ** 2) = 2 ** 9 = 512
    if (2 ** 3 ** 2 == 512) {
        builtin.print("PASS: Power operator associativity");
    }
    
    return 0;
}