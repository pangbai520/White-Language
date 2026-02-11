func main() -> Int {
    let a -> Int = 10;
    let ptr p -> Int = ref a; // 取地址
    
    pseudo_print(deref p);    // 输出 10
    
    deref p = 20;             // 指针赋值
    pseudo_print(a);          // 输出 20 (原值被修改)
    
    return 0;
}