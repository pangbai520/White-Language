// Test: INVALID_ERROR_MEMBER
// File: tests/diagnostics/failures/test_throw_member.wl
// Focus: Reporting an unknown error member without exposing recovery types.
// Expected Error: "NameError: Enum 'Error' has no member 'Unknow'."

func run() -> Int? {
    throw Error.Unknow;
}

func main() -> Int {
    return 0;
}
