func main() -> Int {
    pseudo_print("=== Phase 1: Basic Append ===");

    let v -> Vector(Int) = [1, 2];
    pseudo_print(v.length()); // Expect: 2
    
    v.append(3);
    pseudo_print(v.length()); // Expect: 3
    pseudo_print(v[2]);       // Expect: 3

    pseudo_print("=== Phase 2: Realloc Stress Test ===");

    let i -> Int = 0;
    while (i < 20) {
        v.append(i + 100);
        i = i + 1;
    }
    
    pseudo_print("Final Length:");
    pseudo_print(v.length()); // Expect: 23
    
    pseudo_print("Check last element:");
    pseudo_print(v[22]);      // Expect: 119 (19 + 100)

    pseudo_print("=== Phase 3: Drop (Pop) logic ===");

    let popped_val -> Int = v.drop();
    
    pseudo_print("Dropped value (should be 119):");
    pseudo_print(popped_val); 
    
    pseudo_print("Length after drop (should be 22):");
    pseudo_print(v.length());

    pseudo_print("=== Phase 4: String Vector (Reference Types) ===");

    let s_vec -> Vector(String) = ["Hello"];
    s_vec.append("World");
    s_vec.append("WhiteLang");
    
    pseudo_print(s_vec[0]); // Hello
    pseudo_print(s_vec[1]); // World
    pseudo_print(s_vec[2]); // WhiteLang
    
    let last_str -> String = s_vec.drop();
    pseudo_print("Dropped string:");
    pseudo_print(last_str); // WhiteLang
    pseudo_print(s_vec.length()); // 2

    pseudo_print("=== Phase 5: Error Test (Empty Drop) ===");

    let empty_v -> Vector(Int) = [42];
    empty_v.drop(); // 变成空了
    pseudo_print("Vector is now empty. Next line should crash:");
    
    // 这一行应该触发红色报错
    empty_v.drop(); 

    return 0;
}