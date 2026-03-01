// WhiteLanguage 暴力测试 1 （其实这是从当时 Python 版 White Lang 那里偷来的hhh）
let a -> Int = 10;
let b -> Int = 20;

// 测试深层嵌套函数与作用域覆盖
func math_chaos(a -> Float, b -> Int) -> Float {
    let result -> Float = (a ** 2.0) + (b / 2);
  
    // 在 IF 内部重新定义同名变量（测试符号表栈）
    if (result > 10.0) {
        let result -> Int = 100; 
        pseudo_print(result); // 应该输出整数 100
    }
  
    // 返回的应该是最外层的 Float result
    return result + 1.23;
}

// 测试逻辑短路与 PHI 节点陷阱
func logic_hell(x -> Int) -> Bool {
    if (!(x > 5) && (x % 2 == 0) || (x ** 2 > 100)) {
        pseudo_print("Logic Path A");
        return true;
    }
    return false;
}

func fib(n -> Int) -> Int {
    if (n < 2) { return n; }
    return fib(n - 1) + fib(n - 2);
}

func main() -> Int {
    // 原全局逻辑迁移至此
    let a -> Int = 10;
    let b -> Int = 20;

    /* 测试块注释
       以及嵌套逻辑 
    */
    let isOk -> Bool = true; 
    let result -> Int = 0;

    if (isOk) {
        let inner -> Int = 5;
        if (a < b) {
            result = a + b + inner; // 计算结果 35
        } else {
            result = a - b;
        };
    } else {
        result = 999;
    };

    pseudo_print(result); // 输出: 35

    // 测试复合赋值与后缀运算
    let counter -> Int = 0;
    counter += 10;
    counter *= 2;
    counter /= (5 - 3); // counter 现在应该是 10

    pseudo_print(counter++); // 输出 10，实际变成 11
    pseudo_print(counter);
  
    // 阴间测试 For 循环
    for (i -> Int = 0; i < 5; i++) {
        let j -> Int = i;
        while (j > 0) {
            if (j == 2) {
                pseudo_print("Hit target");
                j--;
                continue; 
            }
            if (i == 4) {
                break; // 退出 while
            }
            pseudo_print(j);
            j--;
        }
        if (i == 4) { break; } // 退出 for
    };

    // 混合类型大杂烩 (Implicit Cast)
    let mixed -> Float = 10 + 5.5 * (2 ** 3); 
    pseudo_print(mixed);

    // 字符串与布尔打印
    let is_wl_cool -> Bool = true;
    if (is_wl_cool && !false) {
        pseudo_print("WhiteLanguage LLVM Backend works!");
    }

    // 递归压力测试
    let rec -> Int = fib(10); 
    pseudo_print(rec); // 预期 55

    // 阴间赋值测试 (a = b = c = 100)
    let x_val -> Int = 1;
    let y_val -> Int = 2;
    let z_val -> Int = 3;
    // 链式赋值
    x_val = y_val = z_val = 100; 
  
    pseudo_print(x_val);

    // 调用之前定义的复杂函数
    let chaos_res -> Float = math_chaos(5.0, 10);
    pseudo_print(chaos_res);

    return 0;
}