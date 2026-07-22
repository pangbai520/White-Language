// Test: BUILTIN_PRELUDE
// File: tests/language/modules/test_builtin_prelude.wl
// Focus: Verification of builtin prelude functions being globally available without explicit import.

func main() -> Int {
    // builtin prelude
    // print must be available without an explicit import
    print("PASS: builtin prelude");
    return 0;
}