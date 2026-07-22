// Test: LIBC_FREE_NUMERIC_FORMAT
// File: tests/language/types/test_numeric_format.wl
// Focus: Signed integer limit string conversion and six-digit float rounding independent of libc.

import "builtin"

func main() -> Int {
    let int_min -> Int = -2147483647 - 1;
    let long_min -> Long = -9223372036854775807L - 1L;

    let int_ok -> Bool = "" + int_min == "-2147483648";
    let long_ok -> Bool = "" + long_min == "-9223372036854775808";
    let zero_ok -> Bool = "" + 0 == "0";
    let round_ok -> Bool = "" + 3.14159 == "3.141590";
    let carry_ok -> Bool = "" + 1.9999996 == "2.000000";
    let negative_ok -> Bool = "" + -0.125 == "-0.125000";

    if (!int_ok || !long_ok || !zero_ok || !round_ok || !carry_ok || !negative_ok) {
        builtin.print("FAIL: libc-free numeric formatting");
        return 1;
    }

    builtin.print("PASS: libc-free numeric formatting");
    return 0;
}
