struct Point(x -> Int, y -> Int)

func test_scope() -> Void {
    pseudo_print(111);
    {
        let s -> String = "I am a temporary string";
        let p -> Point = Point(10, 20);
    } 
    pseudo_print(222);
}

func test_assign() -> Void {
    pseudo_print(333);
    let s1 -> String = "Hello";
    
    let s2 -> String = "World"; 

    s1 = s2; 
    pseudo_print(444);
}

func main() -> Int {
    test_scope();
    test_assign();
    return 0;
}