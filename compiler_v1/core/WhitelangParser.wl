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
func peek_type(p -> Parser) -> Int {
    let l -> Lexer = p.lexer;
    
    // save current lexer
    let save_idx  -> Int = l.pos.idx;
    let save_ln   -> Int = l.pos.ln;
    let save_col  -> Int = l.pos.col;
    let save_char -> Byte = l.current_char;
    
    // get next token
    let tok -> Token = get_next_token(l);
    let type -> Int = tok.type;
    
    // load lexer
    l.pos.idx = save_idx;
    l.pos.ln  = save_ln;
    l.pos.col = save_col;
    l.current_char = save_char;
    
    return type;
}

func atom(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;

    // Integer literals
    if (tok.type == TOK_INT) {
        advance(p);
        let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
        return IntNode(type=NODE_INT, tok=tok, pos=pos);
    }

    // Floating-point literals
    if (tok.type == TOK_FLOAT) {
        advance(p);
        let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
        return FloatNode(type=NODE_FLOAT, tok=tok, pos=pos);
    }

    // Boolean
    if (tok.type == TOK_TRUE) { 
        advance(p);
        let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
        return BooleanNode(type=NODE_BOOL, tok=tok, value=1, pos=pos); 
    }
    if (tok.type == TOK_FALSE) { 
        advance(p);
        let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
        return BooleanNode(type=NODE_BOOL, tok=tok, value=0, pos=pos); 
    }
    
    // Variable access
    if (tok.type == TOK_IDENTIFIER) {
        advance(p);
        let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
        return VarAccessNode(type=NODE_VAR_ACCESS, name_tok=tok, pos=pos);
    }

    // Parenthesized expressions
    if (tok.type == TOK_LPAREN) {
        advance(p);
        let node -> Struct = expression(p);
        
        if (p.current_tok.type != TOK_RPAREN) {
            let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
            throw_invalid_syntax(err_pos, "Expected ')'. ");
        }
        advance(p);
        return node;
    }

    let err_pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
    throw_invalid_syntax(err_pos, "Unexpected token: " + get_token_name(tok.type));
    return null;
}

func parse_args(p -> Parser) -> Struct {
    if (p.current_tok.type == TOK_RPAREN) {
        return null; // no param
    }

    // first parameter
    let first_val -> Struct = expression(p);
    let head -> ArgNode = ArgNode(val=first_val, next=null);
    let curr -> ArgNode = head;

    // subsequent param
    while (p.current_tok.type == TOK_COMMA) {
        advance(p); // skip ','
        let next_val -> Struct = expression(p);
        let new_node -> ArgNode = ArgNode(val=next_val, next=null);
        curr.next = new_node;
        curr = new_node;
    }
    return head;
}

func postfix_expr(p -> Parser) -> Struct {
    let node -> Struct = atom(p);

    while (p.current_tok.type == TOK_INC || p.current_tok.type == TOK_DEC || p.current_tok.type == TOK_LPAREN) {
        // ++ / --
        if (p.current_tok.type == TOK_INC || p.current_tok.type == TOK_DEC) {
            let op_tok -> Token = p.current_tok;
            advance(p);
            let pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
            node = PostfixOpNode(type=NODE_POSTFIX, node=node, op_tok=op_tok, pos=pos);
        }

        // corresponding to Python: elif tok.type == TokenType.LPAREN: ... node = CallNode(node, args)
        else if (p.current_tok.type == TOK_LPAREN) {
            let paren_tok -> Token = p.current_tok; // '('
            advance(p); // skip '('
            
            let args -> Struct = parse_args(p);
            
            if (p.current_tok.type != TOK_RPAREN) {
                let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
                throw_invalid_syntax(err_pos, "Expected ')' after arguments. ");
            }
            advance(p); // skip ')'

            let pos -> Position = Position(idx=0, ln=paren_tok.line, col=paren_tok.col, text=p.lexer.text);

            node = CallNode(type=NODE_CALL, callee=node, args=args, pos=pos);
        }
    }
    return node;
}

func power(p -> Parser) -> Struct {
    let left -> Struct = postfix_expr(p);

    if (p.current_tok.type == TOK_POW) {
        let op_tok -> Token = p.current_tok;
        advance(p);
        let right -> Struct = factor(p);
        let pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
        return BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    
    return left;
}

func comp_expr(p -> Parser) -> Struct {
    // '!a+b' means '(!a)+b', not '!(a+b)' (unlike in Python)
    let left -> Struct = arith_expr(p);

    while (p.current_tok.type == TOK_EE || p.current_tok.type == TOK_NE || 
           p.current_tok.type == TOK_LT || p.current_tok.type == TOK_GT ||
           p.current_tok.type == TOK_LTE || p.current_tok.type == TOK_GTE) {
        let op_tok -> Token = p.current_tok;
        advance(p);
        let right -> Struct = arith_expr(p);
        let pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func logic_and(p -> Parser) -> Struct {
    let left -> Struct = comp_expr(p);

    while (p.current_tok.type == TOK_AND) {
        let op_tok -> Token = p.current_tok;
        advance(p);
        let right -> Struct = comp_expr(p);
        let pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func logic_or(p -> Parser) -> Struct {
    let left -> Struct = logic_and(p);

    while (p.current_tok.type == TOK_OR) {
        let op_tok -> Token = p.current_tok;
        advance(p);
        let right -> Struct = logic_and(p);
        let pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func assignment(p -> Parser) -> Struct {
    let left -> Struct = logic_or(p);

    if (p.current_tok.type == TOK_ASSIGN) {
        let op_tok -> Token = p.current_tok;
        advance(p); // skip '='
        
        // a = b = c
        let right -> Struct = assignment(p);

        let base -> BaseNode = left;
        if (base.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = left;
            let pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
            return VarAssignNode(type=NODE_VAR_ASSIGN, name_tok=v_node.name_tok, value=right, pos=pos);
        } else {
            let err_pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
            throw_invalid_syntax(err_pos, "Invalid assignment target. Only variables can be assigned. ");
        }
    }
    
    return left;
}

func factor(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;
    
    // -5, +3.14, --2
    if (tok.type == TOK_PLUS || tok.type == TOK_SUB || tok.type == TOK_NOT) {
        advance(p);
        let node -> Struct = factor(p);
        let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
        return UnaryOpNode(type=NODE_UNARYOP, op_tok=tok, node=node, pos=pos);
    }
    
    return power(p);
}

func expression(p -> Parser) -> Struct {
    return assignment(p);
}

func term(p -> Parser) -> Struct {
    let left -> Struct = factor(p);

    // left-associative
    while (p.current_tok.type == TOK_MUL || p.current_tok.type == TOK_DIV || p.current_tok.type == TOK_MOD) {
        let op_tok -> Token = p.current_tok;
        advance(p);
        let right -> Struct = factor(p);
        let pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    
    return left;
}

func arith_expr(p -> Parser) -> Struct {
    let left -> Struct = term(p);

    // left-associative
    while (p.current_tok.type == TOK_PLUS || p.current_tok.type == TOK_SUB) {
        let op_tok -> Token = p.current_tok;
        advance(p);
        let right -> Struct = term(p);
        let pos -> Position = Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    
    return left;
}

func var_decl_core(p -> Parser) -> Struct {
    let text_ref -> String = p.lexer.text;
    if (p.current_tok.type != TOK_IDENTIFIER) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected variable name. ");
    }
    let name_tok -> Token = p.current_tok;
    advance(p);

    // 2. ->
    if (p.current_tok.type != TOK_TYPE_ARROW) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected '->' after variable name. ");
    }
    advance(p);

    let type_tok -> Token = p.current_tok;
    let tt -> Int = p.current_tok.type;
    if (tt == TOK_T_INT || tt == TOK_T_FLOAT || tt == TOK_T_STRING || tt == TOK_T_BOOL || tt == TOK_T_VOID) {
        advance(p);
    } else {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected type name (Int, Float, etc.), got " + get_token_name(tt));
    }

    let val_node -> Struct = null;
    if (p.current_tok.type == TOK_ASSIGN) {
        advance(p);
        val_node = expression(p);
    } else {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_missing_initializer(err_pos, "variable declaration requires an initializer. ");
    }
    
    let pos -> Position = Position(idx=0, ln=name_tok.line, col=name_tok.col, text=text_ref);
    return VarDeclareNode(type=NODE_VAR_DECL, name_tok=name_tok, type_tok=type_tok, value=val_node, pos=pos);
}


func var_decl(p -> Parser) -> Struct {
    advance(p); // skip 'let'
    return var_decl_core(p);
}

func parse_block(p -> Parser) -> Struct {
    // '{'
    if (p.current_tok.type != TOK_LBRACE) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected '{' to start a block. ");
    }
    advance(p); // skip {

    let head -> StmtListNode = null;
    let curr -> StmtListNode = null;

    while (p.current_tok.type != TOK_RBRACE && p.current_tok.type != TOK_EOF) {
        let stmt -> Struct = statement(p);
        let base -> BaseNode = stmt;
        let is_compound -> Bool = false;

        if (base.type == NODE_IF || base.type == NODE_BLOCK || base.type == NODE_WHILE) {
            is_compound = true;
        }

        if (p.current_tok.type == TOK_SEMICOLON) {
            advance(p);
        } else {
            if !is_compound {
                let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
                throw_invalid_syntax(err_pos, "Expected ';' after statement in block. ");
            }
        }
        let node -> StmtListNode = StmtListNode(stmt=stmt, next=null);
        if (head == null) { head = node; curr = node; } 
        else { curr.next = node; curr = node; }
    }
    if (p.current_tok.type != TOK_RBRACE) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected '}' to close block. ");
    }
    advance(p); // skip }

    return BlockNode(type=NODE_BLOCK, stmts=head);
}

func if_stmt(p -> Parser) -> Struct {
    let if_tok -> Token = p.current_tok;
    advance(p); // skip 'if'
    
    let cond -> Struct = atom(p);
    let body -> Struct = parse_block(p);

    let else_body -> Struct = null;
    if (p.current_tok.type == TOK_ELSE) {
        advance(p); // skip 'else'
        
        if (p.current_tok.type == TOK_IF) { // else if
            else_body = if_stmt(p);
        } else {
            else_body = parse_block(p);
        }
    }
    
    let pos -> Position = Position(idx=0, ln=if_tok.line, col=if_tok.col, text=p.lexer.text);
    return IfNode(type=NODE_IF, condition=cond, body=body, else_body=else_body, pos=pos);
}

func while_stmt(p -> Parser) -> Struct {
    let while_tok -> Token = p.current_tok;
    advance(p); // skip 'while'
    let cond -> Struct = atom(p);
    let body -> Struct = parse_block(p);

    let pos -> Position = Position(idx=0, ln=while_tok.line, col=while_tok.col, text=p.lexer.text);
    return WhileNode(type=NODE_WHILE, condition=cond, body=body, pos=pos);
}
func break_stmt(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;
    advance(p);
    let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
    return BreakNode(type=NODE_BREAK, pos=pos);
}
func continue_stmt(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;
    advance(p);
    let pos -> Position = Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text);
    return ContinueNode(type=NODE_CONTINUE, pos=pos);
}

func for_stmt(p -> Parser) -> Struct {
    let for_tok -> Token = p.current_tok;
    advance(p); // skip 'for'

    if (p.current_tok.type != TOK_LPAREN) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected '(' after 'for'. ");
    }
    advance(p); // skip '('

    let init -> Struct = null;
    if (p.current_tok.type == TOK_SEMICOLON) {
        init = null;
    } else {
        if (p.current_tok.type == TOK_IDENTIFIER && peek_type(p) == TOK_TYPE_ARROW) {
            init = var_decl_core(p); // i -> Int = 0
        } else {
            init = expression(p);    // i = 0
        }
    }

    if (p.current_tok.type != TOK_SEMICOLON) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected ';' after for-init. ");
    }
    advance(p); // skip ';'

    let cond -> Struct = null;
    if (p.current_tok.type != TOK_SEMICOLON) {
        cond = expression(p);
    }

    if (p.current_tok.type != TOK_SEMICOLON) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected ';' after for-condition. ");
    }
    advance(p); // skip ';'

    let step -> Struct = null;
    if (p.current_tok.type != TOK_RPAREN) {
        step = expression(p);
    }

    if (p.current_tok.type != TOK_RPAREN) {
        let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
        throw_invalid_syntax(err_pos, "Expected ')' after for-step. ");
    }
    advance(p); // skip ')'

    let body -> Struct = parse_block(p);
    let pos -> Position = Position(idx=0, ln=for_tok.line, col=for_tok.col, text=p.lexer.text);
    return ForNode(type=NODE_FOR, init=init, cond=cond, step=step, body=body, pos=pos);
}

func statement(p -> Parser) -> Struct {
    if (p.current_tok.type == TOK_LET) { return var_decl(p); }
    if (p.current_tok.type == TOK_IF)  { return if_stmt(p); }
    if (p.current_tok.type == TOK_WHILE) { return while_stmt(p); }
    if (p.current_tok.type == TOK_LBRACE) { return parse_block(p); }
    if (p.current_tok.type == TOK_BREAK) { return break_stmt(p); }
    if (p.current_tok.type == TOK_CONTINUE) { return continue_stmt(p); }
    if (p.current_tok.type == TOK_FOR) { return for_stmt(p); }

    return expression(p);
}

func parse(p -> Parser) -> Struct {
    let head -> StmtListNode = null;
    let curr -> StmtListNode = null;
    
    while (p.current_tok.type != TOK_EOF) {
        let stmt -> Struct = statement(p);
        let base -> BaseNode = stmt;
        let is_compound -> Bool = false;
        if (base.type == NODE_IF || base.type == NODE_BLOCK || base.type == NODE_WHILE || base.type == NODE_FOR) {
            is_compound = true;
        }

        if (p.current_tok.type == TOK_SEMICOLON) {
            advance(p);
        } else {
            if !is_compound {
                let err_pos -> Position = Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text);
                throw_invalid_syntax(err_pos, "Expected ';' after statement. ");
            }
        }

        let node -> StmtListNode = StmtListNode(stmt=stmt, next=null);
        if (head == null) { head = node; curr = node; } 
        else { curr.next = node; curr = node; }
    }

    return BlockNode(type=NODE_BLOCK, stmts=head);
}