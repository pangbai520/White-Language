// core/WhitelangNodes.wl

const NODE_INT     -> Int = 1;
const NODE_FLOAT   -> Int = 2;
const NODE_BINOP   -> Int = 3;
const NODE_UNARYOP -> Int = 4;

struct BaseNode(type -> Int) // Used to read type

struct IntNode(
    type  -> Int,
    value -> String // Source str
)

struct FloatNode(
    type  -> Int,
    value -> String // Source str
)

struct BinOpNode(
    type  -> Int,
    left  -> Struct,
    op    -> Int,    // Token ID
    right -> Struct
)

struct UnaryOpNode(
    type -> Int,
    op   -> Int,
    node -> Struct
)