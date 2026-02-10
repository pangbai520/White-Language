// core/WhitelangNodes.wl

const NODE_INT     -> Int = 1;
const NODE_FLOAT   -> Int = 2;
const NODE_BINOP   -> Int = 3;
const NODE_UNARYOP -> Int = 4;
const NODE_VAR_DECL   -> Int = 5; // let identi -> Type = ...
const NODE_VAR_ACCESS -> Int = 6; // x
const NODE_BLOCK      -> Int = 7; // { stmt1; stmt2; ... }

struct BaseNode(type -> Int) // Used to read node type

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

struct VarDeclareNode(
    type     -> Int,    // NODE_VAR_DECL
    name     -> String,
    var_type -> String, // Int Float etc
    value    -> Struct, 
    pos  -> Position    // Error position
)

struct VarAccessNode(
    type -> Int,    // NODE_VAR_ACCESS
    name -> String, 
    pos -> Position // Error position
)

struct StmtListNode(
    stmt -> Struct,
    next -> Struct
)

struct BlockNode(
    type -> Int,      // NODE_BLOCK
    stmts -> Struct   // StmtListNode head node
)