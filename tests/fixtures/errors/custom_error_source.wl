error WireError {
    Disconnected,
    InvalidFrame
}

func fail_wire() -> Int? {
    throw WireError.InvalidFrame;
}
