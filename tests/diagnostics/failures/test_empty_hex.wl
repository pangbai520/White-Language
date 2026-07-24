// Test: EMPTY_HEX_LITERAL
// File: tests/diagnostics/failures/test_empty_hex.wl
// Focus: Base-prefixed literals require at least one digit

func main() -> Int {
    let value -> Int = 0x;
    return value;
}
