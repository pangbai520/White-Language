func check() -> Bool {
    pseudo_print("run");
    return true;
}

func main() -> Int {
    if (!(false && check())) {
        pseudo_print("!(false && check())");
    };
    return 0;
}