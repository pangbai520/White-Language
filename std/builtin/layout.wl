// std/builtin/layout.wl
// compiler-provided type layout queries

@CompilerIntrinsic("size_of")
func size_of() -> UIntSize {
    // compiler internal implementation
    return UIntSize(0);
}

@CompilerIntrinsic("align_of")
func align_of() -> UIntSize {
    // compiler internal implementation
    return UIntSize(0);
}
