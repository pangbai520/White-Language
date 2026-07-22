// Test: UNKNOWN_EXTERN_ABI
// File: tests/diagnostics/failures/test_unknown_extern_abi.wl
// Focus: Rejection of ABI specifiers that lack a code generation contract.
// Expected Error: " ExternError: Unsupported extern ABI 'native'. Expected 'C' or 'system'. "

extern "native" {
    func unknown_abi_probe() -> Int;
}

func main() -> Int {
    return 0;
}
