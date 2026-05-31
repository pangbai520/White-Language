// Test: DLL_C_INTEROP
// File: tests/language/integration/ffi/test_dll_host.c
// Focus: C-to-WhiteLang FFI via ctypes, symbol resolution, and ABI validation.
// Compile: gcc test_dll_host.c -L. -ltest_lib_export -o test_dll && ./test_dll

#include <stdio.h>
#include <assert.h>

// map to WhiteLang exported symbols
extern int add(int a, int b);
extern int factorial(int n);
extern double multiply_float(double a, double b);

int main() {
    // execute remote procedure calls from DLL
    assert(add(5, 7) == 12);
    assert(factorial(4) == 24);
    
    double f_res = multiply_float(1.5f, 4.0f);
    
    if (f_res == 6.0f) {
        printf("PASS: FFI boundary and symbol resolution stable\n");
        return 0;
    } else {
        printf("FAIL: Float precision mismatch or ABI corruption\n");
        return 1;
    }
}