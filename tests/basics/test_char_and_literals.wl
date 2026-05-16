// Test: CHAR_AND_LITERALS_VALIDATION
// File: tests/basics/test_char_and_literals.wl
// Focus: Constant folding (CTFE), radix literal parsing, type-safe character scalars, and string coercion.
import "builtin"

// validate constant folding (CTFE) and multi-base integer resolution
const CONST_HEX  -> Int = 0x20;                // hex representation
const CONST_BIN  -> Int = 0b1100_1010;         // binary with underscore separators
const CONST_OCT  -> Int = 0o75;                // octal representation
const CONST_CALC -> Auto = (0x10 + 0b11) * 2;  // compile-time mixed radix evaluation
const CONST_LONG -> Long = 9223372036854775807L;

func main() -> Int {
    // verify compile-time evaluation accuracy
    let ctfe_ok -> Bool = (CONST_HEX == 32) && (CONST_BIN == 202) && (CONST_OCT == 61) && (CONST_CALC == 38);

    // evaluate runtime radix parsing and underscore separator stripping
    let hex_val   -> Int  = 0xFF;
    let bin_val   -> Int  = 0b1010;
    let large_val -> Int  = 1_000_000;
    let long_val  -> Long = 123L;
    
    let runtime_int_ok -> Bool = (hex_val == 255) && (bin_val == 10) && (large_val == 1000000) && (long_val == 123L);

    // verify scalar character type initialization
    let char_a  -> Char = 'A';
    let char_b  -> Char = 'B';
    let char_nl -> Char = '\n';

    // enforce type-safe relational icmp operations on character scalars
    let char_cmp_ok -> Bool = (char_a == 'A') && (char_a < char_b);

    // test string appending coercion with char type 
    let concat_str -> String = "Character token data: " + char_a;
    let string_ok -> Bool = (concat_str == "Character token data: A");

    // note: type safety bounds check
    // compiling should fail if unchecked conversions are uncommented:
    // let invalid_assign -> Int = 'A';       // TypeError: type mismatch
    // let invalid_calc   -> Char = 'A' + 1;   // TypeError: binary operation disallowed
    // if (char_a == 65) { }                   // TypeError: type mismatch in relation

    // --- Final Integrity Assertion ---
    if (ctfe_ok && runtime_int_ok && char_cmp_ok && string_ok) {
        builtin.print("PASS: Multi-base literals, constant folding, and strong-typed character semantics");
    } else {
        // potential front-end scanner regression or folding calculator failure
        builtin.print("FAIL: Literal value mismatch or type-safe character comparison error");
        return 1;
    }

    return 0;
}