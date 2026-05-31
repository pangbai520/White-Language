// Test: IMPORT_VISIBILITY_RESTRICTION
// File: tests/diagnostics/failure/test_import_restriction.wl
// Focus: Enforcement of private symbol shadowing using '__' prefix across module boundaries.
// Expected Error: "TypeError: Cannot access private symbol '__private_func' from external module."

import "../../fixtures/pkgs/restricted_lib.wl" as mod

func main() -> Int {
    // verify standard public access
    let a -> Int = mod.public_func();

    // trigger access violation: attempting to invoke a triple-underscore private symbol
    // this must be intercepted by the compiler's symbol resolver
    let b -> Int = mod.__private_func();

    return 0;
}