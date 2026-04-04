// Test: RUNTIME_ZERO_DIVISION
// File: tests/failure/test_zero_division.wl
// Focus: Constant folding or runtime protection against division by zero.
// Expected Error: " Runtime Error: Division by zero "

import "builtin"

func main() -> Int {
    let a -> Int = 0;
    
    // this should trigger a hardware trap or a software check
    builtin.print(3 / a); 
    
    return 0;
}