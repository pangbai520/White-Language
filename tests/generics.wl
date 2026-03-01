struct Dog(name -> String)


func bark() -> String {
    return "Woof!";
}


func play_with(pet -> Struct, action -> Function(String)) -> Void {
    let d -> Dog = pet;
    pseudo_print("Pet Name: " + d.name);
    pseudo_print("Action: " + action());
}

func main() -> Int {
    let my_dog -> Dog = Dog("Buddy");
    
    let my_action -> Function(String) = bark;

    play_with(my_dog, my_action);

    play_with(my_dog, bark);

    return 0;
}