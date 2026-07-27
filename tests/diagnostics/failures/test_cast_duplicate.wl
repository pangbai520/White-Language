// Test: CAST_DUPLICATE
// File: tests/diagnostics/failures/test_cast_duplicate.wl
// Focus: A class cannot define two conversions to the same canonical target type.
// Expected Error: "NameError: class 'Value' already defines a conversion to Int"

class Value {
    type Int {
        return 1;
    }

    type Int32 {
        return 2;
    }
}

func main() -> Int {
    return 0;
}
