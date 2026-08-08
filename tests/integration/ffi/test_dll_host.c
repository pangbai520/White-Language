// Test: DLL_C_INTEROP
// File: tests/integration/ffi/test_dll_host.c
// Focus: Calling a White Language shared library from C.
// Compile: gcc test_dll_host.c -L. -ltest_lib_export -o test_dll && ./test_dll

#include <stdio.h>
#include <assert.h>

extern int add(int a, int b);
extern int factorial(int n);
extern double multiply_float(double a, double b);

int main() {
    assert(add(5, 7) == 12);
    assert(factorial(4) == 24);
    
    double f_res = multiply_float(1.5f, 4.0f);
    
    if (f_res == 6.0f) {
        printf("PASS: C to White Language FFI calls\n");
        return 0;
    } else {
        printf("FAIL: C to White Language FFI result\n");
        return 1;
    }
}
