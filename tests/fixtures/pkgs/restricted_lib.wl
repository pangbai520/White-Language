// Support: MODULE_VISIBILITY_FIXTURE
// File: tests/fixtures/pkgs/restricted_lib.wl
// Focus: Defining public and private symbols for visibility testing.

func public_func() -> Int {
    return 200;
}

// private symbol: prefixed with '__' to restrict external access
func __private_func() -> Int {
    return 403;
}
