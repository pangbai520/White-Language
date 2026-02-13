func return_str() -> String {
    return "Hello White Lang World!";
}

func return_int() -> Int {
    return 1234;
}

func output(f -> Function(String)) -> Void {
    pseudo_print(f());
}

func main() -> Int {
    output(return_str);
    return 0;
}