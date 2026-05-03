// Test: BITWISE_OPERATIONS_INTEGRITY
// File: tests/basics/test_bitwise.wl
// Focus: Integer bitwise logic, shift arithmetic, and sign-aware inversion.
import "builtin"

func main() -> Int {
    let a -> Int = 12;      // operand a: 0000 1100
    let b -> Int = 10;      // operand b: 0000 1010
    
    // verify standard bitwise logic gates (AND, OR, XOR)
    let r_and -> Int = a & b;  // expected: 8  (0000 1000)
    let r_or  -> Int = a | b;  // expected: 14 (0000 1110)
    let r_xor -> Int = a ^ b;  // expected: 6  (0000 0110)
    
    // check bit-shift arithmetic and power-of-two scaling
    let r_shl -> Int = a << 2; // expected: 48 (0011 0000)
    let r_shr -> Int = a >> 1; // expected: 6  (0000 0110)
    
    // validate bitwise NOT (32-bit two's complement inversion)
    let r_not -> Int = ~a;     // expected: -13
    
    // test inplace mutation via bitwise compound assignments
    let c -> Int = 5;       // initial: 0000 0101
    c &= 3;                 // mask to 1  (0000 0001)
    c |= 8;                 // set to 9   (0000 1001)
    c ^= 12;                // flip to 5  (0000 0101)
    c <<= 1;                // scale to 10
    c >>= 1;                // scale back to 5

    // aggregate results for final integrity check
    let logic_ok -> Bool = (r_and == 8) && (r_or == 14) && (r_xor == 6);
    let shift_ok -> Bool = (r_shl == 48) && (r_shr == 6) && (r_not == -13);
    let compound_ok -> Bool = (c == 5);

    // finalize validation without intermediate noise
    if (logic_ok && shift_ok && compound_ok) {
        builtin.print("PASS: Bitwise logic gates and compound mutation");
    } else {
        // likely an LLVM IR generation error in bitwise mapping
        builtin.print("FAIL: Bitwise operator mismatch or sign-extension error");
        return 1;
    }

    return 0;
}