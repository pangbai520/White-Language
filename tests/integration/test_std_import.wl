// Test: STD_LIBRARY_IMPORT
// File: tests/integration/test_std_import.wl
// Focus: Resolving built-in standard library paths and namespace availability.
import "builtin/print"

func main() -> Int {
    // print_i is expected to be exported from builtin/print
    print_i(520);
    print_s("\nPASS: Standard library module resolution");
    
    return 0;
}