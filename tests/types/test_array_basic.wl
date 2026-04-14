// Test: ARRAY_CORE_OPERATIONS
// File: tests/types/test_array_basic.wl
// Focus: Fixed-size array layout, multi-dimensional indexing, and ARC lifecycle in stack-allocated sequences.
import "builtin"

struct Point (
    x -> Int,
    y -> Int
)

func main() -> Int {
    // intercept array literals and verify stack initialization
    let nums -> Int[5] = [10, 20, 30, 40, 50];
    builtin.print("Array nums:", nums); 
    
    // test index-based mutation
    nums[2] = 999;
    let index_ok -> Bool = (nums[2] == 999);
    builtin.print("After mutation:", nums);

    // validate nested wrapping logic for multi-dimensional layout [Int[2][3]]
    let matrix -> Int[2][3] = [[1, 2, 3], [4, 5, 6]];
    let m_val -> Int = matrix[1][1]; // expected: 5
    builtin.print("Matrix row 1:", matrix[1]);
    builtin.print("Element [1][1]:", m_val);

    // ensure composite struct layout within array boundaries
    let points -> Point[2] = [Point(x=1, y=2), Point(x=10, y=20)];
    points[0].x = 88;
    let struct_ok -> Bool = (points[0].x == 88);
    builtin.print("Modified points:", points);

    // verify ARC retain/release cycles for heap-allocated elements during mutation
    let words -> String[3] = ["Hello", "White", "Language"];
    // overwrite assignment: trigger release of old value and retain of new literal
    words[1] = "Power";
    let arc_ok -> Bool = (words[1] == "Power");
    builtin.print("ARC words:", words);

    // address tracking for pointer-type arrays
    let a -> Int = 100;
    let b -> Int = 200;
    let ptrs -> ptr Int[2] = [ref a, ref b];
    let deref_ok -> Bool = (deref ptrs[0] == 100);
    builtin.print("Deref check:", deref ptrs[0]);

    // final integrity assertion
    if (index_ok && m_val == 5 && struct_ok && arc_ok && deref_ok) {
        builtin.print("PASS: Fixed-size array semantics and ARC integrity");
    } else {
        builtin.print("FAIL: Array layout corruption or indexing mismatch");
        return 1;
    }

    return 0;
}