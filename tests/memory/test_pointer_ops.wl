// Test: POINTER_REFERENCE_DEREF
// File: tests/memory/test_pointer_ops.wl
// Focus: Memory addressing using 'ref' and value mutation via 'deref'.
import "builtin"

func main() -> Int {
    let data -> Int = 42;
    let ptr p -> Int = ref data;
    
    // mutate original value via pointer
    deref p = 100;

    if (data == 100) {
        builtin.print("PASS: Pointer reference and dereference");
    } else {
        builtin.print("FAIL: Pointer mutation failed. Expected 100, got: " + data);
    }
    return 0;
}