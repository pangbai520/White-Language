// Test: CAST_TARGET
// File: tests/diagnostics/failures/test_cast_target.wl
// Focus: Class conversion targets are restricted to built-in value types.
// Expected Error: "TypeError: conversion target Other is not a built-in value type"

class Other {
}

class Value {
    type Other {
        return Other();
    }
}

func main() -> Int {
    return 0;
}
