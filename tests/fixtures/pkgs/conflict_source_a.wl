// Support: SYMBOL_CONFLICT_SOURCE_A
// File: tests/fixtures/pkgs/conflict_source_a.wl
// Focus: Providing 'collision_target' to test symbol shadowing and conflict resolution.

import "builtin"
func collision_target() -> Void {
    builtin.print("I am from pkg_a");
}
