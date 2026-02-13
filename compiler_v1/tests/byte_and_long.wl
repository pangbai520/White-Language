func main() -> Int {
    let b1 -> Byte = 250;
    let b2 -> Byte = 10;
    
    // Test Overflow/Wrap behavior (250 + 10 = 260 -> 4 in 8-bit)
    let b3 -> Byte = b1 + b2; 
    pseudo_print(b3); 

    // Test Promotion (250 + 100 = 350 in Int)
    let i -> Int = b1 + 100;
    pseudo_print(i);
    
    return 0;
}