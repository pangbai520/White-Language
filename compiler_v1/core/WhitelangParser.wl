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
    
    if (tok.type == TOK_NUMBER) {
        let node -> IntegerNode = IntegerNode(type=NODE_INTEGER, value=tok.value);
        advance(p);
        return node;
    }
    
    if (tok.type == TOK_LPAREN) {
        advance(p);
        let node -> Struct = arith_expr(p);
        
        if (p.current_tok.type != TOK_RPAREN) {
            builtin.print("Parser Error: Expected ')'"); // Fake exception
            return null;
        }
        advance(p);
        return node;
    }

    builtin.print("Parser Error: Unexpected token"); // Fake exception
    return null;
}


func term(p -> Parser) -> Struct {
    let left -> Struct = factor(p);
    
    while (p.current_tok.type == TOK_MUL || p.current_tok.type == TOK_DIV) {
        let op_type -> Int = p.current_tok.type;
        advance(p);
        let right -> Struct = factor(p);
        // 复刻 BinOpNode(left, op_token, right) 逻辑
        left = BinOpNode(type=NODE_BINOP, left=left, op=op_type, right=right);
    }
    
    return left;
}


func arith_expr(p -> Parser) -> Struct {
    let left -> Struct = term(p);
    
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