func main() -> Int {
    // 1. Vector 索引读写
    let v -> Vector(Int) = [10, 20, 30];
    pseudo_print(v[1]); // 应输出 20
    
    v[1] = 999;
    pseudo_print(v[1]); // 应输出 999
    
    // 2. String 索引读取
    let s -> String = "ABC";
    let b -> Byte = s[0]; // 'A' 的 ASCII 是 65
    pseudo_print(b);      // 应输出 65
    
    // s[0] = 66; // 这一行如果取消注释，编译应该报错
    v[114514]; // also error
    
    return 0;
}