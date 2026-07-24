// Test: MALFORMED_DECIMAL_LITERAL
// File: tests/diagnostics/failures/test_malformed_decimal.wl
// Focus: Repeated decimal points must be rejected by the lexer

func main() -> Int {
    let value -> Float = 1.2.3;
    return Int(value);
}
