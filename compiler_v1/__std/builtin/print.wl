// std/builtin/print.wl
//
// Standard print Library.
// This file implements multiple print_X variants to allow print to work with arbitrary objects.

extern func printf(fmt -> String, ...) -> Int from "C";

func print_i(n -> Int) -> Void {
    // Int print
    printf("%d", n);
}

func print_l(n -> Long) -> Void {
    // Long int print
    printf("%d", n);
}

func print_f(f -> Float) -> Void {
    // Float print
    printf("%f", f);
}

func print_s(s -> String) -> Void {
    // String print
    printf("%s", s);
}

func print_b(b -> Bool) -> Void {
    // Bool Print
    if b {
        printf("true");
    } else {
        printf("false"); 
    }
}

func print_c(c -> Byte) -> Void {
    // Byte Print
    printf("%c", c); 
}

func print_struct(s -> Struct) -> Void {} // Implement in compiler
func print_vector(v -> Struct) -> Void {} // Implement in compiler

func print() -> Void {} // declared print()