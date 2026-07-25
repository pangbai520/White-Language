import "./hidden.wl"

func exposed_value() -> Int {
    return hidden.hidden_value() + 1;
}
