extern func printf(...) -> Int from "C"; // 单句extern
extern "C" {                             // extern多个
    func malloc(size -> Long) -> ptr Byte;
    func exit(code -> Int) -> Void;
}

func main() -> Int {
    printf("Hello %s, code: %d\n", "World", 100);
    
    exit(0);
    return 0;
}