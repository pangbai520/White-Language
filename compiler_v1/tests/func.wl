let a -> Int = 10;
func main() -> Int {
    // 1. 基础负数与空格
    let a -> Int = - 5; // 定义域测试，应该得到5而非10
    let b -> Int = - - 10;          // 负负得正：10
    
    // 2. 括号嵌套与逻辑非
    let c -> Bool = !(! (a < 0));   // 应该还是 true
    
    // 3. 负数参与幂运算（测试解析顺序）
    let d -> Float = - 2.0 ** 2.0; 
    // 预期：- (2.0 ** 2.0) = -4.0 (因为幂运算优先级高于一元负号)
    
    let e -> Float = (- 2.0) ** 2.0;
    // 预期：4.0
    
    // 4. 混合运算
    let f -> Float = - 5.0 + 10.0 * - 2.0;
    // 预期：-5.0 + (-20.0) = -25.0

    pseudo_print(a);
    pseudo_print(b);
    pseudo_print(c);
    pseudo_print(d);
    pseudo_print(e);
    pseudo_print(f);
    
    return 0;
}