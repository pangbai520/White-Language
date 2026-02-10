// core/WhitelangParser.wl
import "builtin"
import "WhitelangLexer.wl"
import "WhitelangTokens.wl"
import "WhitelangNodes.wl"
import "WhitelangExceptions.wl"


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

    // Variable assignment
    if (tok.type == TOK_IDENTIFIER) {
        let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
        let node -> VarAccessNode = VarAccessNode(type=NODE_VAR_ACCESS, name=tok.value, pos=pos);
        advance(p);
        return node;
    }
    
    // Parenthesized expressions
    if (tok.type == TOK_LPAREN) {
        advance(p);
        let node -> Struct = arith_expr(p);
        
        if (p.current_tok.type != TOK_RPAREN) {
            let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
            throw_invalid_syntax(err_pos, "Expected ')'. ");
            return null;
        }
        advance(p);
        return node;
    }

    let err_pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
    throw_invalid_syntax(err_pos, "Unexpected token: " + get_token_name(tok.type));
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

func var_decl(p -> Parser) -> Struct {
    let text_ref -> String = p.lexer.text;
    advance(p); // skip 'let'
    
    if (p.current_tok.type != TOK_IDENTIFIER) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected variable name. ");
    }
    let name -> String = p.current_tok.value;
    advance(p);
    
    // '->'
    if (p.current_tok.type != TOK_TYPE_ARROW) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected '->' after variable name. ");
    }
    advance(p);
    
    // Type
    let type_val -> String = "";
    let tt -> Int = p.current_tok.type;
    if (tt == TOK_T_INT || tt == TOK_T_FLOAT) {
        type_val = p.current_tok.value;
        advance(p);
    } else {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected type name (Int, Float), got " + get_token_name(tt));
    }

    // '='
    if (p.current_tok.type != TOK_ASSIGN) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected '='. ");
    }
    advance(p);

    let val_ln -> Int = p.current_tok.line;
    let val_col -> Int = p.current_tok.col;
    let val_node -> Struct = arith_expr(p);

    let pos -> Position = Position(idx=0, ln=val_ln, col=val_col, text=text_ref);
    
    return VarDeclareNode(type=NODE_VAR_DECL, name=name, var_type=type_val, value=val_node, pos=pos);
}

func statement(p -> Parser) -> Struct {
    if (p.current_tok.type == TOK_LET) {
        return var_decl(p);
    }
    return arith_expr(p);
}

func parse(p -> Parser) -> Struct {
    let head -> StmtListNode = null;
    let curr -> StmtListNode = null;
    
    while (p.current_tok.type != TOK_EOF) {
        let stmt_pos_ln -> Int = p.current_tok.line;
        let stmt_pos_col -> Int = p.current_tok.col;

        let stmt -> Struct = statement(p);

        if (p.current_tok.type != TOK_SEMICOLON) {
            let err_pos -> Position = Position(
                idx=0, 
                ln=p.current_tok.line, 
                col=p.current_tok.col, 
                text=p.lexer.text
            );
            throw_invalid_syntax(err_pos, "Expected ';' after statement.");
        }

        let node -> StmtListNode = StmtListNode(stmt=stmt, next=null);
        if (head == null) {
            head = node;
            curr = node;
        } else {
            curr.next = node;
            curr = node;
        }

        advance(p);
    }
    
    return BlockNode(type=NODE_BLOCK, stmts=head);
}