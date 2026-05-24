// Test: EXPLICIT_TYPE_CAST_AND_DECLARATION
// File: tests/types/test_new_types.wl
// Focus: Integer bit-width resolution, explicit cast semantics (trunc/ext), and pointer-integer roundtripping.
import "builtin"

func main() -> Int {
    // ensure compiler successfully resolves various width identifiers
    let i8_val  -> Int8  = -12;
    let i64_val -> Int64 = -8000000000000000000L;
    let u32_val -> UInt32 = 4000000000L;
    let decl_ok -> Bool  = (i8_val == -12 && u32_val > 0);

    // validate float to integer
    let f_val -> Float = 3.99;
    let i_val -> Int = Int(f_val); 
    let cast_a_ok -> Bool = (i_val == 3);

    // validate integer to float conversion
    let i_val2 -> Int = -42;
    let f_val2 -> Float32 = Float32(i_val2);
    let cast_b_ok -> Bool = (f_val2 == -42.0);

    // truncate 300 (0x12C) to UInt8 (0x2C -> 44)
    let big_int -> Int = 300; 
    let small_int -> UInt8 = UInt8(big_int); 
    let trunc_ok -> Bool = (small_int == 44);

    // Int8 -> Long
    let neg_i8 -> Int8 = -5;
    let ext_long -> Long = Long(neg_i8);
    let sext_ok -> Bool = (ext_long == -5);

    // UInt8 -> UInt64
    let pos_u8 -> UInt8 = 250;
    let ext_u64 -> UInt64 = UInt64(pos_u8);
    let zext_ok -> Bool = (ext_u64 == 250);

    // validate boolean to integer coercion
    let b_to_i -> Int = Int(true);
    let bool_ok -> Bool = (b_to_i == 1);

    // validate pointer/integer round-trip (ptrtoint/inttoptr)
    let fake_addr -> Long = 123456789L;
    let ptr_val -> AnyPtr = AnyPtr(fake_addr);
    let addr_back -> UIntSize = UIntSize(ptr_val);
    let ptr_ok -> Bool = (addr_back == 123456789);

    if (decl_ok && cast_a_ok && cast_b_ok && trunc_ok && sext_ok && zext_ok && bool_ok && ptr_ok) {
        builtin.print("PASS: Modern type declarations and zero-cost conversion semantics");
    } else {
        // failure indicates a regression in LLVM IR cast instructions (sext/zext/trunc)
        builtin.print("FAIL: Type declaration mismatch or cast semantics corruption");
        return 1;
    }

    return 0;
}