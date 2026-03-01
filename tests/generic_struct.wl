struct Test(x->Int, y->Float) {
    this.x = 3;
    this.y = 2.3;
}

func main() -> Int {
    let generic->Struct = Test();
    pseudo_print(generic.x);
    pseudo_print(generic.y);
    return 0;
}