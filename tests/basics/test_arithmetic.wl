// Test: COMPLEX_ARITHMETIC
// File: tests/basics/test_arithmetic.wl
// Focus: Deeply nested parentheses and operator precedence.
import "builtin"

func main() -> Int {
    let res -> Float = (2 + 3) * 5 / 3 + (6 - 3 / 78) + 114 * 514;

    if (res > 58609.000000 && res < 58611.000000) {
        builtin.print("PASS: Complex arithmetic precision");
    } else {
        builtin.print("FAIL: Arithmetic mismatch, got: " + res);
    }
    return 0;
}