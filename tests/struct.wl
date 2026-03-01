struct Config(id->Int, ratio->Float) {
    this.id = 999;
    this.ratio = 0.5;
}

struct Entity(hp->Int, tag->String, conf->Config) {
    this.hp = 100;
}

// 全局变量遮蔽测试准备
let val -> Int = 10;

func complex_calc(base->Float, offset->Int) -> Float {
    // 这里的 offset 会触发 Int -> Float 的 _cast
    return base * offset + 2.5;
}

func main() -> Int {
    pseudo_print("--- Start Hell Test ---");

    // 1. 作用域遮蔽与运算测试
    let val -> Float = 5.5; // 遮蔽全局 val
    let result -> Float = complex_calc(val, 2); // 传入 Float(5.5) 和 Int(2)
    pseudo_print("Calc Result (Expected 13.5): ");
    pseudo_print(result);

    // 2. 结构体三级初始化优先级测试
    // h1: 完全默认 (hp=100, id=999)
    let h1 -> Entity;
    
    // h2: 传参覆盖默认 (hp=500, ratio=0.8)
    // 这里混合了位置参数和关键字参数
    let h2 -> Entity = Entity(500, tag="BOSS", conf=Config(ratio=0.8));

    pseudo_print("H1 HP (Default 100): ");
    pseudo_print(h1.hp);
    pseudo_print("H2 HP (User 500): ");
    pseudo_print(h2.hp);
    pseudo_print("H2 Ratio (User 0.8): ");
    pseudo_print(h2.conf.ratio);

    // 3. 循环、条件与语法糖测试
    let i -> Int = 0;
    let log -> String = "";
    
    while (i < 5) {
        if (i == 3) {
            pseudo_print("Found Three!");
        } else {
            // 测试基础运算
            let step-> Int = i * 2;
        }
        i++; // PostfixOpNode 测试
    }

    // 4. 字符串拼接
    pseudo_print("Final Counter: ");
    pseudo_print(i);

    pseudo_print("--- Hell Test Passed ---");
    return 0;
}