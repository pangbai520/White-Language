// Test: NAME_COLLISION_DETECTION
// File: tests/failure/test_name_collision.wl
// Focus: Enforcement of ImportError when wildcard importing colliding symbols from different modules.
// Expected Error: "ImportError: Name collision for function 'collision_target'. Please use explicit alias."

import "fixtures/pkg_a.wl"
import * from "fixtures/pkg_a.wl"
import * from "fixtures/pkg_b.wl" // This must throw ImportError due to 'collision_target' conflict

func main() -> Int {
    collision_target();
    return 0;
}
