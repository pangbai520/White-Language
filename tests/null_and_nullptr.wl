// test_null_safety.wl

struct Box(val -> Int)

func main() -> Int {
    pseudo_print("=== Test 1: String & Null ===");
    
    // 1. 初始化为 null
    let s -> String = null;
    
    if (s is null) {
        pseudo_print("[OK] s is initialized to null.");
    } else {
        pseudo_print("[FAIL] s is NOT null?");
    }

    // 2. 赋值为对象
    s = "Hello WhiteLang";
    if (s is ! null) {
        pseudo_print("[OK] s is now: " + s);
    }

    // 3. 重新赋值为 null (这里会触发 ARC 释放 "Hello WhiteLang")
    s = null;
    if (s is null) {
        pseudo_print("[OK] s is reset to null.");
    }

    pseudo_print("");
    pseudo_print("=== Test 2: Pointer & Nullptr ===");

    // 4. 指针初始化为 nullptr
    let ptr p -> Int = nullptr;
    
    if (p is nullptr) { // 也可以写 if (p is null)，底层都是0，但语义上用 nullptr 更佳
        pseudo_print("[OK] p is nullptr.");
    }

    // 5. 指针赋值
    let x -> Int = 42;
    let ptr p2 -> Int = ref x;
    
    if (p2 is ! nullptr) {
        pseudo_print("[OK] p2 points to valid address.");
    }

    pseudo_print("");
    pseudo_print("=== Test 3: Struct & Null ===");

    // 6. 结构体初始化为 null
    let b -> Box = null;
    if (b is null) {
        pseudo_print("[OK] Box b is null.");
    }

    // 7. 赋值新对象
    b = Box(999);
    if (b is ! null) {
        pseudo_print("[OK] Box b is allocated.");
    }

    // 8. 验证禁止项 (如果取消注释下面这行，编译器应报错)
    // if (b == null) { pseudo_print("Error"); } 

    return 0;
}