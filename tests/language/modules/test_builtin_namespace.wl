// Test: BUILTIN_NAMESPACE
// File: tests/language/modules/test_builtin_namespace.wl
// Focus: Accessing prelude functions through an explicit builtin namespace.

import "builtin"

func main() -> Int {
    builtin.print("PASS: explicit builtin namespace");
    return 0;
}
