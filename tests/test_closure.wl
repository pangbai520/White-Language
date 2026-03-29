import "builtin"

func main() -> Int {
    let name -> String = "WhiteLang Closure";

    func outer() -> Function(Int) {
        let age -> Int = 1;
        
        func inner() -> Int {
            let temp -> Int = 5;
            builtin.print(name); 
            age = age + temp;
            return age;
        }

        return inner; 
    }
    
    builtin.print("--- Test Start ---");

    let my_closure -> Function(Int) = outer();
    
    let res1 -> Int = my_closure();
    builtin.print(res1); 
    
    let res2 -> Int = my_closure();
    builtin.print(res2);
    
    builtin.print("--- Test End ---");
    return 0;
}