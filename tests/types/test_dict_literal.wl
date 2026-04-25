// Test: DICT_LITERAL_ALL_TYPES
// File: tests/types/test_dict_literal.wl
// Focus: Generic boxing of primitives, composites, and closures via Map literal syntax sugar.
import "builtin"
import "dict"

struct _Variant(hp -> Int) {
    this.hp = 100; 
}

class Weapon {
    let damage -> Int = 0;
    init(d -> Int) -> Void {
        self.damage = d;
    }
    method attack() -> Int {
        return self.damage;
    }
}

func global_heal() -> Int {
    return 999;
}

func main() -> Int {
    // prepare complex objects and bound closures for boxing
    let h -> _Variant = _Variant(hp=888);
    let w -> Weapon = Weapon(d=500);
    let m_ptr -> Method(Int) = w.attack;
    let f_ptr -> Function(Int) = global_heal;

    // trigger compile_map_lit to box diverse types into the Map structure
    // handles automatic __wl_variant wrapping for all supported data types
    let map -> Dict = {
        "type_int": 42,
        "type_float": 3.1415,
        "type_bool": true,
        "type_string": "WhiteLang_String",
        "type_struct": h,
        "type_class": w,
        "type_method": m_ptr,
        "type_function": f_ptr,
    };

    // verify unboxing integrity via bracket access syntax
    let r_int -> Int = map["type_int"];
    let r_float -> Float = map["type_float"];
    let r_bool -> Bool = map["type_bool"];
    let r_str -> String = map["type_string"];
    
    // validate primitive data persistence
    let prim_ok -> Bool = (r_int == 42 && r_float == 3.1415 && r_bool == true && r_str == "WhiteLang_String");

    // check memory layout and pointer stability for composites
    let r_struct -> _Variant = map["type_struct"];
    let r_class -> Weapon = map["type_class"];
    let comp_ok -> Bool = (r_struct.hp == 888 && r_class.damage == 500);

    // ensure environment capture and VTable routing for closures
    let r_method -> Method(Int) = map["type_method"];
    let r_func -> Function(Int) = map["type_function"];
    let clos_ok -> Bool = (r_method() == 500 && r_func() == 999);

    // final integrity assertion
    if (prim_ok && comp_ok && clos_ok) {
        builtin.print("PASS: Dictionary literal boxing and multi-type unboxing");
    } else {
        // likely a variant type tagging error or dictionary storage corruption
        builtin.print("FAIL: Type corruption detected during Dict literal unboxing");
        return 1;
    }

    return 0;
}