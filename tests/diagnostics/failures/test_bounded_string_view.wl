// Test: BOUNDED_STRING_VIEW
// File: tests/diagnostics/failures/test_bounded_string_view.wl
// Focus: Bounded zero-copy String views are not implemented yet

func main() -> Int {
    let value -> String = "WhiteLang";
    let invalid -> String = ref value[0:5];
    return 0;
}
