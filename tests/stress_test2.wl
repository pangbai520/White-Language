//! 此测试由AI生成，非本人所写。

// ==========================================
// 1. 定义数据结构
// ==========================================
struct Node(
    id   -> Int,
    data -> Int,
    next -> Node
)

struct Point(x -> Int, y -> Int)

struct Rect(
    top_left -> Point,
    width -> Int,
    height -> Int
)

// ==========================================
// 2. 辅助函数
// ==========================================
func create_node(id -> Int) -> Node {
    return Node(id=id, data=id * 10, next=nullptr);
}

func build_long_chain(count -> Int) -> Node {
    pseudo_print(">> Building chain of size: ");
    pseudo_print(count);

    let head -> Node = create_node(0);
    let current -> Node = head;
    let i -> Int = 1;

    while (i < count) {
        let new_node -> Node = create_node(i);

        current.next = new_node;
        
        current = new_node;
        i++;
    }
    
    return head;
}

// 压力测试 2: 遍历并验证
func verify_chain(head -> Node, count -> Int) -> Int {
    pseudo_print(">> Verifying chain...");
    let curr -> Node = head;
    let i -> Int = 0;
    
    while (i < count) {
        if (curr.id != i) {
            pseudo_print("ERROR: ID mismatch at index: ");
            pseudo_print(i);
            return 1; // Fail
        }
        
        // 测试读写
        curr.data = curr.data + 1;
        
        curr = curr.next;
        i++;
    }
    
    if (i != count) {
        pseudo_print("ERROR: Count mismatch.");
        return 1;
    }
    
    pseudo_print("Chain verified successfully.");
    return 0;
}

func swap_head_data(ptr head_ref -> Node) -> Void {
    pseudo_print(">> Testing Ptr Deref...");
    
    // (deref head_ref) 得到的是 Node (引用)
    let old_val -> Int = (deref head_ref).data;
    
    // 修改数据
    (deref head_ref).data = 99999;
    
    pseudo_print("Old head data (should be 1):");
    pseudo_print(old_val);
    pseudo_print("New head data (should be 99999):");
    pseudo_print((deref head_ref).data);
}


func test_nested_structs() -> Void {
    pseudo_print(">> Testing Nested Structs...");
    
    let p -> Point = Point(x=10, y=20);
    let r -> Rect = Rect(top_left=p, width=100, height=200);

    let val -> Int = r.top_left.y;
    
    if (val == 20) {
        pseudo_print("Nested access SUCCESS.");
    } else {
        pseudo_print("Nested access FAILED.");
    }

    let temp -> Point = r.top_left;
    temp.x = 888;
    
    pseudo_print("Modified deep field x (should be 888):");
    pseudo_print(p.x); 
}

// ==========================================
// 3. 主程序
// ==========================================

func main() -> Int {
    pseudo_print("=== STARTING STRESS TEST ===");

    let count -> Int = 1000;

    let list_head -> Node = build_long_chain(count);
    
    if (verify_chain(list_head, count) != 0) {
        return 1;
    }

    // 传入 Ref (地址)，对应函数签名的 ptr Node
    swap_head_data(ref list_head);

    test_nested_structs();

    pseudo_print("=== ALL TESTS PASSED ===");
    return 0;
}