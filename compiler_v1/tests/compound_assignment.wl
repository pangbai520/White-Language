struct Point(x -> Int, y -> Int)

func main() -> Int {
    let a -> Int = 10;
    a += 5;
    pseudo_print("a (15): " + a);

    let p -> Point = Point(x=10, y=20);
    p.x += 10;
    p.y -= 5;
    pseudo_print("p.x (20): " + p.x);
    pseudo_print("p.y (15): " + p.y);

    let ptr val_ptr -> Int = ref a;
    (deref val_ptr) += 100;
    pseudo_print("a via ptr (115): " + a);

    return 0;
}