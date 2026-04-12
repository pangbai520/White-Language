// std/builtin/print.wl
// Standard print Library.
extern func wl_print_utf8(s -> String) -> Void from "C";

func print(s -> String) -> Void {
    wl_print_utf8(s);
}
