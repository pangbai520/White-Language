// Support: CIRCULAR_DEPENDENCY_A
// File: tests/fixtures/pkgs/circular_dep_a.wl
// Focus: Providing symbol 'foo' for circular dependency graph traversal.

import "builtin"
import bar from "./circular_dep_b.wl"

func foo() -> Int {
    return 10;
}