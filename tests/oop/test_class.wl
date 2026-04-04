// Test: OOP_CORE_LOGIC
// File: tests/oop/test_class.wl
// Focus: VTable method dispatch, self-pointer (this) field mutation, and heap allocation integrity.
import "builtin"

// POD struct - Ensure stack layout remains contiguous
struct Vector3(x -> Int, y -> Int, z -> Int)

class BankAccount {
    let _id -> Int = 0;
    let owner -> String = null;
    let balance -> Int = 0;

    // constructor binding test
    init(id -> Int, name -> String, val -> Int) -> Void {
        self._id = id;
        self.owner = name;
        self.balance = val;
    }

    method deposit(amount -> Int) -> Void {
        self.balance = self.balance + amount;
    }

    method withdraw(amount -> Int) -> Bool {
        if (self.balance >= amount) {
            self.balance = self.balance - amount;
            return true;
        }
        return false;
    }

    method get_balance() -> Int {
        return self.balance;
    }
}

func main() -> Int {
    // POD layout verification
    let v -> Vector3 = Vector3(10, 20, 30);
    if (v.z != 30) {
        builtin.print("FAIL: Struct field offset error");
        return 1;
    }

    // heap allocation & init injection
    let acc -> BankAccount = BankAccount(1001, "dev_test_user", 500);
    
    // method dispatch & state mutation
    acc.deposit(200); 
    let deposit_ok -> Bool = (acc.get_balance() == 700);

    // logic branching (negative & positive cases)
    let overdraw_blocked -> Bool = (acc.withdraw(1000) == false);
    let valid_draw_ok -> Bool = acc.withdraw(600);

    // final integrity check
    if (deposit_ok && overdraw_blocked && valid_draw_ok && acc.get_balance() == 100) {
        builtin.print("PASS: OOP method dispatch and state mutation");
    } else {
        builtin.print("FAIL: BankAccount state corruption or logic error");
        return 1;
    }

    return 0;
}