// core/WhitelangNodes.wl
import "WhitelangTokens.wl"

const NODE_INT            -> Int = 1;
const NODE_FLOAT          -> Int = 2;
const NODE_BINOP          -> Int = 3;
const NODE_UNARYOP        -> Int = 4;
const NODE_VAR_DECL       -> Int = 5; // let identi -> Type = ...
const NODE_VAR_ACCESS     -> Int = 6; // x
const NODE_VAR_ASSIGN     -> Int = 7;
const NODE_BLOCK          -> Int = 8; // { stmt1; stmt2; ... }
const NODE_POSTFIX        -> Int = 9; // a++, a--
const NODE_BOOL           -> Int = 10;
const NODE_IF             -> Int = 11;
const NODE_WHILE          -> Int = 12;
const NODE_BREAK          -> Int = 13;
const NODE_CONTINUE       -> Int = 14;
const NODE_FOR            -> Int = 15;
const NODE_CALL           -> Int = 16; // func()
const NODE_FUNC_DEF       -> Int = 17; // func foo
const NODE_RETURN         -> Int = 18; // return ...
const NODE_PARAM          -> Int = 19; // func foo(...)
const NODE_STRING         -> Int = 20;
const NODE_STRUCT_DEF     -> Int = 21;
const NODE_FIELD_ACCESS   -> Int = 22;
const NODE_FIELD_ASSIGN   -> Int = 23;
const NODE_PTR_TYPE       -> Int = 24;
const NODE_REF            -> Int = 25;
const NODE_DEREF          -> Int = 26;
const NODE_PTR_ASSIGN     -> Int = 27;
const NODE_NULLPTR        -> Int = 28;
const NODE_FUNCTION_TYPE  -> Int = 29;
const NODE_NULL           -> Int = 30;
const NODE_IS             -> Int = 32; // is ...
const NODE_IS_NOT         -> Int = 33; // is !...
const NODE_EXTERN_BLOCK   -> Int = 34;
const NODE_EXTERN_FUNC    -> Int = 35;
const NODE_VECTOR_TYPE    -> Int = 36;
const NODE_VECTOR_LIT     -> Int = 37;
const NODE_INDEX_ACCESS   -> Int = 38;
const NODE_INDEX_ASSIGN   -> Int = 39;
const NODE_IMPORT         -> Int = 40;


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

struct StringNode(
    type -> Int,    // NODE_STRING
    tok  -> Token,  // TOK_STR_LIT
    pos  -> Position
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
    type_node     -> Struct,  // Type Name Token
    value        -> Struct, 
    pos          -> Position    // Error position
)

struct VarAccessNode(
    type     -> Int,    // NODE_VAR_ACCESS
    name_tok -> Token,  // Variable Name Token
    pos      -> Position
)

struct VarAssignNode(
    type      -> Int,       // NODE_VAR_ASSIGN
    name_tok  -> Token,
    value     -> Struct,
    pos       -> Position
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

struct WhileNode(
    type      -> Int,       // NODE_WHILE
    condition -> Struct,    // Boolean expression
    body      -> Struct,    // BlockNode
    pos       -> Position
)

struct BreakNode(
    type -> Int,    // NODE_BREAK
    pos  -> Position
)

struct ContinueNode(
    type -> Int,   // NODE_CONTINUE
    pos  -> Position
)

struct ForNode(
    type -> Int,        // NODE_FOR
    init -> Struct,
    cond -> Struct,
    step -> Struct,
    body -> Struct,
    pos  -> Position
)

struct CallNode(
    type   -> Int,    // NODE_CALL
    callee -> Struct,
    args   -> Struct,
    pos    -> Position
)

struct ArgNode(
    val  -> Struct, // expression
    name -> String,
    next -> Struct  // next parameter
)

struct ParamNode(
    type     -> Int,    // NODE_PARAM
    name_tok -> Token,
    type_tok -> Struct,
    pos      -> Position
)


struct ParamListNode(
    param -> Struct, // ParamNode
    next  -> Struct
)


// func name(params...) -> RetType { body }
struct FunctionDefNode(
    type     -> Int,    // NODE_FUNC_DEF
    name_tok -> Token,
    params   -> Struct, // ParamListNode
    ret_type_tok -> Struct,
    body     -> Struct,
    pos      -> Position
)

struct ReturnNode(
    type  -> Int,       // NODE_RETURN
    value -> Struct,
    pos   -> Position
)

// Function(Type)
struct FunctionTypeNode(
    type        -> Int,    // NODE_FUNCTION_TYPE
    return_type -> Struct,
    pos         -> Position
)

struct StructDefNode(
    type     -> Int,    // NODE_STRUCT_DEF
    name_tok -> Token,
    fields   -> Struct,
    body     -> Struct,
    pos      -> Position
)

struct FieldAccessNode(
    type -> Int,      // NODE_FIELD_ACCESS
    obj -> Struct,
    field_name -> String,
    pos -> Position
)

struct FieldAssignNode(type -> Int,
    obj -> Struct,      // NODE_FIELD_ASSIGN
    field_name -> String,
    value -> Struct,
    pos -> Position
)


struct PointerTypeNode(
    type      -> Int,    // NODE_PTR_TYPE
    base_type -> Struct,
    level     -> Int,    // depth
    pos       -> Position
)
struct RefNode(
    type -> Int, // NODE_REF
    node -> Struct,
    pos  -> Position
)
struct DerefNode(
    type  -> Int, // NODE_DEREF
    node  -> Struct,
    level -> Int,
    pos   -> Position
)
struct PtrAssignNode(
    type  -> Int, // NODE_PTR_ASSIGN
    pointer   -> Struct, // DerefNode
    value -> Struct,
    pos   -> Position
)
struct NullPtrNode(
    type -> Int,      // NODE_NULLPTR
    pos  -> Position
)

struct NullNode(
    type -> Int,
    pos  -> Position
)

struct ExternBlockNode(
    type  -> Int, // NODE_EXTERN_BLOCK
    funcs -> Struct,
    pos   -> Position
)

struct ExternFuncNode(
    type         -> Int, // NODE_EXTERN_FUNC
    name_tok     -> Token,
    params       -> Struct,
    ret_type_tok -> Struct,
    is_varargs   -> Bool,    // 1 if has '...', else 0
    pos          -> Position
)

struct VectorTypeNode(
    type         -> Int, // NODE_VECTOR_TYPE
    element_type -> Struct, // Type Node (e.g. IntNode)
    pos          -> Position
)

struct VectorLitNode(
    type     -> Int, // NODE_VECTOR_LIT
    elements -> Struct,
    count    -> Int,
    pos      -> Position
)

struct IndexAccessNode(
    type       -> Int, // NODE_INDEX_ACCESS
    target     -> Struct,
    index_node -> Struct,
    pos        -> Position
)

struct IndexAssignNode(
    type       -> Int, // NODE_INDEX_ASSIGN
    target     -> Struct,
    index_node -> Struct,
    value      -> Struct,
    pos        -> Position
)

struct ImportSymbolNode(
    name_tok -> Token,
    next     -> Struct
)

struct ImportNode(
    type       -> Int,    // NODE_IMPORT
    path_tok   -> Token,
    symbols    -> Struct,
    pos        -> Position
)