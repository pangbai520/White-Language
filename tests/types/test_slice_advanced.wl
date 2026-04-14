// Test: SLICE_ADVANCED_SEMANTICS
// File: tests/types/test_slice_advanced.wl
// Focus: Zero-copy slice views, implicit casting, universal parameters, and mutation.
import "builtin"

// verify universal parameter binding via Array(Int) interface abstraction
// handles Int[N], Vector(Int), and Array(Int) via implicit pointer casting
func sum_slice(arr -> Array(Int)) -> Int {
    let s -> Int = 0;
    let i -> Int = 0;
    while (i < arr.length()) {
        s += arr[i];
        i++;
    }
    return s;
}

func main() -> Int {
    // fixed-size array slicing and boundary alignment
    let a -> Int[5] = [10, 20, 30, 40, 50];
    let slice_a -> Array(Int) = a[1:4]; // expected: [20, 30, 40]
    let a_ok -> Bool = (slice_a.length() == 3 && slice_a[0] == 20 && slice_a[2] == 40);

    // dynamic vector slicing after heap expansion
    let v -> Vector(Int) = [100, 200, 300, 400];
    v.append(500); // resize: [100, 200, 300, 400, 500]
    let slice_v -> Array(Int) = v[2:5]; // expected: [300, 400, 500]
    let v_ok -> Bool = (slice_v.length() == 3 && slice_v[0] == 300 && slice_v[2] == 500);

    // string slicing with copy semantics
    let str -> String = "WhiteLang";
    let slice_str -> String = str[0:5]; // expected: "White"
    let s_ok -> Bool = (slice_str.length() == 5 && slice_str == "White");

    // universal parameter dispatch and implicit cast
    let sum1 -> Int = sum_slice(a);       // implicit cast: Int[5] -> Array(Int)
    let sum2 -> Int = sum_slice(v);       // implicit cast: Vector(Int) -> Array(Int)
    let sum3 -> Int = sum_slice(slice_a); // direct Array(Int) passing
    let p_ok -> Bool = (sum1 == 150 && sum2 == 1500 && sum3 == 90);

    // confirm view-based mutation affects base memory
    let mut_slice -> Array(Int) = a[0:2]; // reference [10, 20]
    mut_slice[0] = 999; 
    let m_ok -> Bool = (a[0] == 999); // verify zero-copy reference integrity

    // nested reslicing logic and offset propagation
    // slice_v is [300, 400, 500]
    let reslice -> Array(Int) = slice_v[1:3]; // expected sub-view: [400, 500]
    let r_ok -> Bool = (reslice.length() == 2 && reslice[1] == 500);

    // Final Integrity Assertion
    // consolidate all sub-checks into a single output point
    if (a_ok && v_ok && s_ok && p_ok && m_ok && r_ok) {
        builtin.print("PASS: Slice subsystem semantics and universal interface");
    } else {
        builtin.print("FAIL: Slice logic corruption or offset calculation error");
        return 1;
    }

    return 0;
}