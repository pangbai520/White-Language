// core/WhitelangNodes.wl

const NODE_INTEGER -> Int = 1;
const NODE_BINOP   -> Int = 2;

struct BaseNode(type -> Int) // Used to read type

struct IntegerNode(
    type -> Int,
    value -> String // Source Str
)

struct BinOpNode(
    type  -> Int,
    left  -> Struct,
    op    -> Int,    // Token ID
    right -> Struct
)