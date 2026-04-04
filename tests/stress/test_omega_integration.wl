// Test: OMEGA_SYSTEM_INTEGRATION
// File: tests/stress/test_omega_integration.wl
// Focus: FFI ABI stability, function pointer "fire propagation", and complex generic container recursion.
import "builtin"

extern "C" {
    func malloc(size -> Long) -> ptr Byte;
    func free(ptr p -> Byte) -> Void;
}

struct Payload(id -> Long, tag -> String)
struct Node(val -> Int, data -> Payload, next -> Node)
func compute_sum(a -> Int, b -> Int) -> Int { return a + b; }

func main() -> Int {
    // FFI and raw memory
    let raw_ptr -> ptr Byte = malloc(16);
    free(raw_ptr);

    // recursive Vector/Struct mix
    let v_test -> Vector(Int) = [100, 200, 300];
    let head -> Node = Node(1, Payload(10, "BASE"), null);
    head.next = Node(2, Payload(20, "SUB"), null);

    // higher-order function dispatch
    let fn_ptr -> Function(Int) = compute_sum;
    let calc_res -> Int = fn_ptr(50, 50);

    // final integrity assertion
    if (calc_res == 100 && v_test.length() == 3 && head.next.val == 2) {
        builtin.print("PASS: Omega system integration");
    } else {
        builtin.print("FAIL: Omega system logic collapse");
    }
    return 0;
}