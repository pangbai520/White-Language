// Test: MODULE_SYMBOL_ISOLATION
// File: tests/integration/fixtures_isolation.wl
// Focus: Ensuring symbols with the same name in different files do not collide.
import "builtin"
import "fixtures/module_a.wl" as modA
import "fixtures/module_b.wl" as modB

func main() -> Int {
    let str1 -> String = modA.str("A");
    let str2 -> String = modB.str("B");

    if (str1 == "A" && str2 == "B") {
        builtin.print("PASS: Module symbol isolation");
    } else {
        builtin.print("FAIL: Namespace collision detected");
    }
    return 0;
}