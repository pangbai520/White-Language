// Test: ENUM_RANGE
// File: tests/diagnostics/failures/test_enum_range.wl
// Focus: Rejecting enum values that do not fit the runtime representation.
// Expected Error: "OverflowError: value for 'TooLarge' is outside the Int range"

enum Number { TooLarge = 2147483648L }
func main() -> Int { return 0; }
