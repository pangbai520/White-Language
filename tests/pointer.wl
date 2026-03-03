struct Test()

func main() -> Int {
    let ptr p->Int = nullptr;
    let a->Int = 0;
    pseudo_print("Old value of a: " + a);
    p = ref a;
    deref p = 114514;
    pseudo_print("New value of a:" + a);

    // 报错测试
    // pseudo_print(deref nullptr);

    // nullptr测试
    // let b->Int = nullptr;
    let b->Test = nullptr;
    return 0;
}