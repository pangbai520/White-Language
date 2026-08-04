// Test: THROW_PLAIN_ENUM
// File: tests/diagnostics/failures/test_throw_plain_enum.wl
// Focus: Throw accepts error values, not ordinary enum members.
// Expected Error: "TypeError: Cannot throw Status, expected an error value"

enum Status {
    Failed
}

func run() -> Int? {
    throw Status.Failed;
}

func main() -> Int {
    return 0;
}
