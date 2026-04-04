// Test: INDEXING_ACCESS
// File: tests/types/test_indexing.wl
// Focus: Array-style indexing ([]) for Vector read/write and String read-only access.
import "builtin"

func main() -> Int {
    // 1. Vector index mutation
    let v -> Vector(Int) = [10, 20, 30];
    v[1] = 999;
    
    // 2. String byte access
    let s -> String = "ABC";
    let first_byte -> Byte = s[0]; // 'A' = 65

    if (v[1] == 999 && first_byte == 65) {
        builtin.print("PASS: Vector and String indexing");
    } else {
        builtin.print("FAIL: Indexing value mismatch");
    }
    return 0;
}