// Test: OVERRIDE_SIGNATURE
// File: tests/diagnostics/failures/test_override_signature.wl
// Focus: Requiring an override to preserve the parent ABI.
// Expected Error: "TypeError: Override of 'value' does not match the parent method signature"

class Base { method value(input -> Int) -> Int { return input; } }
class Child(Base) { method value(input -> String) -> String { return input; } }
func main() -> Int { return 0; }
