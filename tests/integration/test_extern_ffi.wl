// Test: EXTERN_C_INTEROP
// File: tests/integration/test_extern_ffi.wl
// Focus: C-style foreign function interface (FFI) declarations and external linking.

extern "C" {
    func printf(fmt -> String, ...) -> Int;
}

func main() -> Int {
    // if the linker correctly finds printf and execution proceeds, it's a success
    printf("Testing FFI: %s\n", "WhiteLang");
    printf("PASS: Testing FFI");
    return 0;
}