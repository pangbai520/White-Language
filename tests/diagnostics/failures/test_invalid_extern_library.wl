// Test: INVALID_EXTERN_LIBRARY_NAME
// File: tests/diagnostics/failures/test_invalid_extern_library.wl
// Focus: Rejection of paths, flags, and non-linker characters in extern library names.
// Expected Error: " ExternError: Invalid extern library name '../unsafe -flag'. Use a linker library name without paths or flags. "

extern "C" in "../unsafe -flag" {
    func invalid_library_probe() -> Int;
}

func main() -> Int {
    return 0;
}
