// core/WhitelangParser.wl
import "builtin"
import "WhitelangLexer.wl"
import "WhitelangTokens.wl"
import "WhitelangNodes.wl"

struct Parser(
    lexer -> Lexer,
    current_tok -> Token
)

func advance(p -> Parser) -> Void {
    p.current_tok = get_next_token(p.lexer);
}

func factor(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;
    
    // Unary operations: Handles sign prefixes (e.g., -5, +3.14, --2)
    if (tok.type == TOK_PLUS || tok.type == TOK_SUB) {
        let op_type -> Int = tok.type;
        advance(p);
        // Recursive call allows nested unary operators
        return UnaryOpNode(type=NODE_UNARYOP, op=op_type, node=factor(p));
    }
    
    // Integer literals
    if (tok.type == TOK_INT) {
        let node -> IntNode = IntNode(type=NODE_INT, value=tok.value);
        advance(p);
        return node;
    }

    // Floating-point literals
    if (tok.type == TOK_FLOAT) {
        let node -> FloatNode = FloatNode(type=NODE_FLOAT, value=tok.value);
        advance(p);
        return node;
    }
    
    // Parenthesized expressions
    if (tok.type == TOK_LPAREN) {
        advance(p);
        let node -> Struct = arith_expr(p);
        
        if (p.current_tok.type != TOK_RPAREN) {
            builtin.print("Parser Error: Expected ')'");
            return null;
        }
        advance(p);
        return node;
    }

    builtin.print("Parser Error: Unexpected token in factor");
    return null;
}

func term(p -> Parser) -> Struct {
    let left -> Struct = factor(p);
    
    // Multiplication and Division: Left-associative
    while (p.current_tok.type == TOK_MUL || p.current_tok.type == TOK_DIV) {
        let op_type -> Int = p.current_tok.type;
        advance(p);
        let right -> Struct = factor(p);
        left = BinOpNode(type=NODE_BINOP, left=left, op=op_type, right=right);
    }
    
    return left;
}

func arith_expr(p -> Parser) -> Struct {
    let left -> Struct = term(p);
    
    // Addition and Subtraction: Left-associative
    while (p.current_tok.type == TOK_PLUS || p.current_tok.type == TOK_SUB) {
        let op_type -> Int = p.current_tok.type;
        advance(p);
        let right -> Struct = term(p);
        left = BinOpNode(type=NODE_BINOP, left=left, op=op_type, right=right);
    }
    
    return left;
}

func parse(p -> Parser) -> Struct {
    return arith_expr(p);
}