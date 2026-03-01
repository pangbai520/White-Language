// =====================================================================
// WhiteLang 宇宙无敌地狱阴间爆炸压测代码 (Python版wl偷来的, AI生成并由我修复bug)
// =====================================================================

import "builtin";

// 1. 外部 C 函数声明 (用于 ABI 与原始内存测试)
extern "C" {
    func snprintf(ptr str -> Byte, size -> Long, fmt -> String, ...) -> Int;
    func malloc(size -> Long) -> ptr Byte;
    func free(ptr p -> Byte) -> Void;
    func exit(code -> Int) -> Void;
}

// 2. 核心测试结构体
struct Payload(id -> Long, score -> Float, tag -> String) {
    this.id = 0;
    this.score = 0.0;
    this.tag = "DEFAULT";
}

struct Container(meta -> Struct, version -> Float) {
    this.version = 1.0;
    this.meta = null;
}

struct Node(val -> Int, data -> Struct, next -> Struct) {
    this.val = 0;
    this.data = null;
    this.next = null;
}

// 3. 高阶函数测试准备
func compute_sum(a -> Int, b -> Int) -> Int { return a + b; }
func format_msg(id -> Int) -> String { return "ID_REPORT_VALID"; }
func subtract(a -> Int, b -> Int) -> Int { return a - b; }

struct Handler(id -> Int, worker -> Function(Int), logger -> Function(String))

func bridge(f -> Function(Int)) -> Void {
    // 验证火种传递：在高阶函数内部执行
    let res -> Int = f(10, 5); 
    builtin.print("Bridge Execution Result:");
    builtin.print(res);
}

func main() -> Int {
    builtin.print(">>> INITIALIZING OMEGA STRESS TEST V2 <<<");

    // --- Part A: 自动初始化语法糖 (Syntax Sugar Test) ---
    // 🚩 压测点：利用 let s -> Tester; 自动调用构造函数，不再触发 Null Pointer
    let auto_s -> Payload; 
    builtin.print("Part A - Auto Init Payload:");
    builtin.print(auto_s); // 应输出：Payload(id=0, score=0.000000, tag=DEFAULT）


    // --- Part B: 暴力手动内存与 C-ABI 混合 ---
    let ptr raw_buf -> Byte = malloc(256);
    if (raw_buf is null) {
        builtin.print("Failed to allocate raw_buf");
        return 1;
    }

    let lang_name -> String = "WhiteLang";
    let major -> Int = 1;
    let minor -> Float = 0.99;

    // 验证可变参数链接
    snprintf(raw_buf, 256, "Engine: %s, Ver: %d.%.2f", lang_name, major, minor);
    builtin.print("Part B - Formatted ABI Report:");
    builtin.print(raw_buf);


    // --- Part C: ARC 自动引用计数极限压测 ---
    // 循环创建对象并嵌套，验证作用域结束时的自动释放逻辑
    let i -> Long = 0;
    while (i < 1000) {
        {
            let p -> Payload = Payload(id = i, score = 99.9, tag = "TEMP");
            let c -> Container = Container(meta = p, version = 2.0);
            if (i % 500 == 0) {
                builtin.print("ARC Checkpoint (Iteration):");
                builtin.print(i);
            }
        } // 离开作用域：c 释放 -> p 的 RC 减一 -> p 释放
        i++;
    }
    builtin.print("Part C - ARC Scoping Test: PASSED");


    // --- Part D: 泛型类型擦除与 Hint 找回 (Metadata Test) ---
    // 🚩 压测点：将 Payload 存入 Struct 泛型，再通过 builtin.print 找回并打印
    let master_data -> Payload = Payload(id = 777, score = 88.8, tag = "MASTER");
    let wrapper -> Container = Container(meta = master_data, version = 3.14);

    // 提取为 Struct 类型
    let erased -> Struct = wrapper.meta; 
    builtin.print("Part D - Generic Metadata Recovery (Erased Struct):");
    builtin.print(erased); // 依靠编译器挂载的 func_ret_type_hint，应正确输出 Payload 内容


    // --- Part E: 阴间递归链表与 Vector 混合打印 ---
    let head -> Node = Node(val = 0);
    let current -> Node = head;

    let k -> Int = 1;
    while (k < 5) {
        let p_item -> Payload = Payload(id = k * 10, score = 0.5, tag = "NODE");
        let newNode -> Node = Node(val = k, data = p_item);
        current.next = newNode; 
        current = newNode;
        k++;
    }
    
    let vec_test -> Vector(Int) = [100, 200, 300];
    
    builtin.print("Part E - Recursive Structure & Vector Print:");
    builtin.print(head); // 自动递归打印链表
    builtin.print(vec_test); // 自动循环打印 Vector


    // --- Part F: 函数指针“火种”传递地狱测试 ---
    builtin.print("Part F - Function Fire Propagation:");
    let myHandler -> Handler = Handler(id = 1, worker = compute_sum, logger = format_msg);

    // 1. 验证字段函数调用类型追踪
    let sum_res -> Int = myHandler.worker(50, 50); 
    let log_res -> String = myHandler.logger(999);
    builtin.print("Sum (50+50):");
    builtin.print(sum_res);

    // 2. 验证火种重燃：赋值后重新追踪返回类型
    myHandler.worker = subtract;
    builtin.print("Sub (100-40) After Assignment:");
    builtin.print(myHandler.worker(100, 40));

    // 3. 验证高阶函数
    bridge(myHandler.worker);


    // --- Part G: 运行时安全兜底测试 ---
    builtin.print("Part G - Runtime Null Guard Test...");
    let ptr safe_ptr -> Payload = nullptr;
    // 如果取消下面一行的注释，程序应打印 "Runtime Error: Null pointer dereference." 并安全退出
    // builtin.print(safe_ptr.id); 


    // --- 清理与退出 ---
    free(raw_buf);
    builtin.print("=== [OMEGA TEST] WHITE-LANG IS IMMORTAL! ===");

    return 0;
}