import "tests/import_test/math_test.wl"
import "builtin"

func main() -> Int {
    builtin.print(add_int(2, 3));
    return 0;
}