func main() -> Int {
    let v -> Vector(Int) = [10, 20, 30];
    let len -> Int = v.length();
    pseudo_print(len); // Should print 3
    
    let s -> String = "Hello";
    pseudo_print(s.length()); // Should print 5
    
    return 0;
}