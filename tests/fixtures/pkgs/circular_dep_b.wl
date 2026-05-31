// Support: CIRCULAR_DEPENDENCY_B
// File: tests/fixtures/pkgs/circular_dep_b.wl
// Focus: Providing symbol 'bar' to complete the circular dependency cycle.

import "builtin"
import foo from "./circular_dep_a.wl"

func bar() -> Int {
    return foo() + 20;
}