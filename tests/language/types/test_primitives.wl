// Test: PRIMITIVE_TYPES_AND_CASTS
// File: tests/language/types/test_primitives.wl
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
    let cast_b_ok -> Bool = (f_val2 == -42.0f);

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

    // validate literal suffixes (LL, ULL, U, UL, L) and Auto deduction
    let auto_i128 -> Auto = 170141183460469231731687303715884105727LL; // ((2^127) - 1)
    let auto_u128 -> Auto = 340282366920938463463374607431768211455ULL;// ((2^128) - 1)
    let auto_u64  -> Auto = 18446744073709551615UL;                    // ((2^64) - 1)
    let auto_u32  -> Auto = 4294967295U;                               // ((2^32) - 1)

    let suffix_ok -> Bool = (auto_i128 == 170141183460469231731687303715884105727LL) && 
                            (auto_u128 == 340282366920938463463374607431768211455ULL) && 
                            (auto_u64 == 18446744073709551615UL) && 
                            (auto_u32 == 4294967295U);

    if (decl_ok && cast_a_ok && cast_b_ok && trunc_ok && sext_ok && zext_ok && bool_ok && ptr_ok && suffix_ok) {
        builtin.print("PASS: Modern type declarations and zero-cost conversion semantics");
    } else {
        // failure indicates a regression in LLVM IR cast instructions (sext/zext/trunc)
        builtin.print("FAIL: Type declaration mismatch or cast semantics corruption");
        return 1;
    }

    return 0;
}
