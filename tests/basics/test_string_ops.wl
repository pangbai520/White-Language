// Test: STRING_COMPARE
// File: tests/basics/test_string_ops.wl
// Focus: String concatenation, escape sequence parsing, and equality comparison.
import "builtin"

func main() -> Int {
    let str -> String = "Hi";
    let combined -> String = "White" + "Lang";

    if (str == "Hi" && combined == "WhiteLang") {
        builtin.print("PASS: String operations");
    } else {
        builtin.print("FAIL: String concatenation or comparison");
    }
    return 0;
}