// core/WhitelangNodes.wl
import "WhitelangTokens.wl"

const NODE_INT        -> Int = 1;
const NODE_FLOAT      -> Int = 2;
const NODE_BINOP      -> Int = 3;
const NODE_UNARYOP    -> Int = 4;
const NODE_VAR_DECL   -> Int = 5; // let identi -> Type = ...
const NODE_VAR_ACCESS -> Int = 6; // x
const NODE_BLOCK      -> Int = 7; // { stmt1; stmt2; ... }
const NODE_POSTFIX    -> Int = 8; // a++, a--
const NODE_BOOL       -> Int = 9;
const NODE_IF         -> Int = 10;

struct BaseNode(type -> Int) // Used to read node type

struct IntNode(
    type  -> Int,
    tok   -> Token,
    pos   -> Position
)

struct FloatNode(
    type  -> Int,
    tok   -> Token, 
    pos   -> Position
)

struct BooleanNode(
    type -> Int, 
    tok -> Token,
    value -> Int, // 1 for true, 0 for false
    pos   -> Position
)

struct BinOpNode(
    type     -> Int,
    left     -> Struct,
    op_tok   -> Token,    // Token object
    right    -> Struct, 
    pos      -> Position
)

struct UnaryOpNode(
    type   -> Int,
    op_tok -> Token,
    node   -> Struct, 
    pos    -> Position
)

struct PostfixOpNode(
    type   -> Int,     // NODE_POSTFIX
    node   -> Struct,  // VarAccessNode
    op_tok -> Token,   // ++ or --
    pos    -> Position
)

struct VarDeclareNode(
    type         -> Int,    // NODE_VAR_DECL
    name_tok     -> Token,  // Variable Name Token
    type_tok     -> Token,  // Type Name Token
    value        -> Struct, 
    pos          -> Position    // Error position
)

struct VarAccessNode(
    type     -> Int,    // NODE_VAR_ACCESS
    name_tok -> Token,  // Variable Name Token
    pos      -> Position
)

struct StmtListNode(
    stmt -> Struct,
    next -> Struct
)

struct BlockNode(
    type  -> Int,      // NODE_BLOCK
    stmts -> Struct   // StmtListNode head node
)

struct IfNode(
    type      -> Int,       // NODE_IF
    condition -> Struct,    // Boolean expression
    body      -> Struct,    // BlockNode
    else_body -> Struct,    // BlockNode or IfNode (else if) or null
    pos       -> Position
)