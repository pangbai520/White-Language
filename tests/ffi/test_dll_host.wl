// Test: WL_TO_WL_FFI_INTEROP
// File: tests/ffi/test_wl_host.wl
// Focus: WhiteLang-to-WhiteLang FFI via ctypes, symbol resolution, and ABI validation.
// Compile: wlc test_dll_host.wl test_lib_export.dll -o test_dll && ./test_dll

import "builtin"

// map external symbols from the compiled shared object
extern "C" {
    func add(a -> Int, b -> Int) -> Int;
    func factorial(n -> Int) -> Int;
    func multiply_float(a -> Float, b -> Float) -> Float;
}

func main() -> Int {
    // trigger procedure calls across the library boundary
    let res_add -> Int = add(5, 7);
    let res_fact -> Int = factorial(4);
    let res_float -> Float = multiply_float(1.5, 4.0);

    // verify ABI and logic persistence
    let add_ok -> Bool = (res_add == 12);
    let fact_ok -> Bool = (res_fact == 24);
    let float_ok -> Bool = (res_float == 6.0);

    // finalize integrity check
    if (add_ok && fact_ok && float_ok) {
        builtin.print("PASS: WhiteLang-to-WhiteLang FFI boundary stable");
    } else {
        // failure indicates a possible symbol mismatch or stack corruption at the boundary
        builtin.print("FAIL: ABI mismatch or symbol resolution error");
        return 1;
    }

    return 0;
}