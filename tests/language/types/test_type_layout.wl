// Test: TYPE_LAYOUT
// File: tests/language/types/test_type_layout.wl
// Focus: Compile-time size and alignment queries for concrete value representations.

const POINTER_SIZE -> UIntSize = size_of(AnyPtr);
const POINTER_ALIGN -> UIntSize = align_of(AnyPtr);

interface Marker {
    method value() -> Int;
}

class Box with Marker {
    method value() -> Int { return 1; }
}

func main() -> Int {
    let primitive_ok -> Bool = size_of(Byte) == UIntSize(1) && size_of(Int) == UIntSize(4) && size_of(Long) == UIntSize(8) && size_of(Int128) == UIntSize(16);
    let array_ok -> Bool = size_of(Int[4]) == UIntSize(16) && align_of(Int[4]) == UIntSize(4);
    let reference_ok -> Bool = size_of(String) == POINTER_SIZE && size_of(Box) == POINTER_SIZE && align_of(String) == POINTER_ALIGN;
    let aggregate_ok -> Bool = size_of(Marker) == UIntSize(16) && align_of(Marker) == POINTER_ALIGN && size_of(Int?) == UIntSize(32);

    if (primitive_ok && array_ok && reference_ok && aggregate_ok) {
        print("PASS: type layout queries");
        return 0;
    }
    print("FAIL: type layout query returned the wrong value");
    return 1;
}
