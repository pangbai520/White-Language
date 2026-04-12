// Test: PRINT_INTRINSICS_VALIDATION
// File: tests/basics/test_print_features.wl
// Focus: Variadic arguments, type-to-string conversion (Vector/Struct), and Unicode console support.
import "builtin"

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
    builtin.print("--- Basic Types ---");
    builtin.print("Int:", 1024);
    builtin.print("Long:", 9223372036854775807);
    builtin.print("Float:", 3.14159);
    builtin.print("Bool:", true, "and", false);
    builtin.print("Byte:", "A");
    builtin.print("");

    // validate UTF-8 encoding and Unicode symbol rendering (Windows WriteConsoleW)
    builtin.print("--- Unicode / Chinese Test ---");
    builtin.print("你好，WhiteLanguage！");
    builtin.print("こんにちは，WhiteLanguage！");
    builtin.print("မင်္ဂလာပါ，WhiteLanguage！");
    builtin.print("Привет，WhiteLanguage！");
    builtin.print("混合测试: 数字", 123123, " 字符: ❤️ 语言: 中文");
    builtin.print("");

    // check vector element iteration and formatting (compile_print_vector_internal)
    builtin.print("--- Vector Test ---");
    let nums -> Vector(Int) = [1, 2, 3, 4, 5];
    let words -> Vector(String) = ["Apple", "Banana", "Cherry"];
    builtin.print("Numbers:", nums);
    builtin.print("Fruits:", words);
    builtin.print("");

    // test recursive struct field traversal and layout printing (compile_print_struct_internal)
    builtin.print("--- Struct Test ---");
    let p -> Point = Point(x=10, y=20);
    let u -> User = User(
        id=1, 
        name="WhiteLang", 
        pos=p
    );
    builtin.print("Point struct:", p);
    builtin.print("User (Nested):", u);
    builtin.print("");

    // ensure proper spacing and interceptor logic in variadic arguments
    builtin.print("--- Multi-Arg Test ---");
    builtin.print("Arg1", "Arg2", 100, true, p);
    builtin.print("");

    // handle null references and address printing
    builtin.print("--- Null & Pointer Test ---");
    let n -> String = null;
    builtin.print("Null string:", n);
    
    // final integrity check
    // if execution reaches here without a trap or crash, the print pipeline is stable
    builtin.print("PASS: Variadic print and complex type stringification");

    return 0;
}