// Test: PRINT_INTRINSICS_VALIDATION
// File: tests/language/basics/test_print_features.wl
// Focus: Variadic arguments, type-to-string conversion (Vector/Struct), and Unicode console support.


struct Point (
    x -> Int,
    y -> Int
)

struct User (
    id -> Int,
    name -> String,
    pos -> Point
)

func main() -> Int {
    // verify implicit cast and basic type stringification (emit_implicit_cast)
    print("--- Basic Types ---");
    print("Int:", 1024);
    print("Long:", 9223372036854775807);
    print("Float:", 3.14159);
    print("Bool:", true, "and", false);
    print("Byte:", "A");
    print("");

    // validate UTF-8 encoding and Unicode symbol rendering (Windows WriteConsoleW)
    print("--- Unicode / Language Test ---");
    print("你好，WhiteLanguage！");
    print("こんにちは，WhiteLanguage！");
    print("မင်္ဂလာပါ，WhiteLanguage！");
    print("Привет，WhiteLanguage！");
    print("混合测试: 数字", 123123, " 字符: ❤️ 语言: 中文");
    print("");

    // check vector element iteration and formatting (compile_print_vector_internal)
    print("--- Vector Test ---");
    let nums -> Vector(Int) = [1, 2, 3, 4, 5];
    let words -> Vector(String) = ["Apple", "Banana", "Cherry"];
    print("Numbers:", nums);
    print("Fruits:", words);
    print("");

    // test recursive struct field traversal and layout printing (compile_print_struct_internal)
    print("--- Struct Test ---");
    let p -> Point = Point(x=10, y=20);
    let u -> User = User(
        id=1, 
        name="WhiteLang", 
        pos=p
    );
    print("Point struct:", p);
    print("User (Nested):", u);
    print("");

    // ensure proper spacing and interceptor logic in variadic arguments
    print("--- Multi-Arg Test ---");
    print("Arg1", "Arg2", 100, true, p);
    print("");

    // handle null references and address printing
    print("--- Null & Pointer Test ---");
    let n -> String = null;
    print("Null string:", n);
    
    // final integrity check
    // if execution reaches here without a trap or crash, the print pipeline is stable
    print("PASS: Variadic print and complex type stringification");

    return 0;
}
