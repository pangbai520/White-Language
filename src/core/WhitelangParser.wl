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

struct TypedIdent(
    name_tok -> Token,
    type_node -> Struct
)

func synchronize(p -> Parser) -> Void {
    parser_advance(p);
    while (p.current_tok.type != TOK_EOF) {
        if (p.current_tok.type == TOK_SEMICOLON) {
            parser_advance(p);
            return;
        }
        let type -> Int = p.current_tok.type;
        if (type == TOK_FUNC || type == TOK_LET || type == TOK_CONST || type == TOK_IF || type == TOK_WHILE || type == TOK_FOR || type == TOK_RETURN || type == TOK_CLASS || type == TOK_STRUCT) {
            return;
        }
        parser_advance(p);
    }
}

func parse(p -> Parser) -> Struct {
    let stmts -> Vector(Struct) = [];
    
    while (p.current_tok.type != TOK_EOF) {
        let stmt -> Struct = null;
        if (p.current_tok.type == TOK_FUNC) {
            stmt = func_def(p);
        } else if (p.current_tok.type == TOK_STRUCT) {
            stmt = parse_struct_def(p);
        } else if (p.current_tok.type == TOK_CLASS) {
            stmt = parse_class_def(p);
        } else if (p.current_tok.type == TOK_IMPORT) { 
            stmt = parse_import(p);
        } else if (p.current_tok.type == TOK_LET || p.current_tok.type == TOK_CONST) {
            stmt = var_decl(p);
            if (p.current_tok.type == TOK_SEMICOLON) {
                parser_advance(p);
            } else {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ';' after global variable declaration.");
            }
        } else if (p.current_tok.type == TOK_EXTERN) {
            stmt = parse_extern(p);
        } else {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Top level code must be function definitions or global variables. Found: " + get_token_name(p.current_tok.type));
            synchronize(p);
            continue;
        }
        if (stmt is !null) {
            stmts.append(stmt);
        }
    }
    
    return BlockNode(type=NODE_BLOCK, stmts=stmts);
}

func parser_advance(p -> Parser) -> Void {
    p.current_tok = WhitelangLexer.get_next_token(p.lexer);
}

func peek_type(p -> Parser) -> Int {
    let l -> Lexer = p.lexer;
    
    // save current lexer
    let save_idx  -> Int = l.pos.idx;
    let save_ln   -> Int = l.pos.ln;
    let save_col  -> Int = l.pos.col;
    let save_char -> Byte = l.current_char;
    
    // get next token
    let tok -> Token = WhitelangLexer.get_next_token(l);
    let type -> Int = tok.type;
    
    // load lexer
    let p_pos -> Position = l.pos;
    p_pos.idx = save_idx;
    p_pos.ln  = save_ln;
    p_pos.col = save_col;
    l.current_char = save_char;
    
    return type;
}

func String_to_Int(s -> String) -> Int {
    let res -> Int = 0;
    let i -> Int = 0;
    while (i < s.length()) {
        let code -> Int = s[i];
        if (code >= 48 && code <= 57) {
            res = res * 10 + (code - 48);
        }
        i += 1;
    }
    return res;
}

func parse_type_base(p -> Parser) -> Struct {
    // Support 'ptr' in type position (e.g. -> ptr Int) for compatibility
    if (p.current_tok.type == TOK_PTR) {
        let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        parser_advance(p); // skip ptr

        let level -> Int = 1;
        if (p.current_tok.type == TOK_MUL) {
            parser_advance(p);
            if (p.current_tok.type == TOK_INT) {
                level = String_to_Int(p.current_tok.value);
                parser_advance(p);
            } else {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected pointer level.");
            }
        }

        let base -> Struct = parse_type_base(p);
        return PointerTypeNode(type=NODE_PTR_TYPE, base_type=base, level=level, pos=start_pos);
    }

    let tok -> Token = p.current_tok;
    let tt -> Int = p.current_tok.type;
    let type_node -> Struct = null;
    let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);

    if (tt == TOK_T_INT || tt == TOK_T_FLOAT || tt == TOK_T_STRING || tt == TOK_T_BOOL || tt == TOK_T_VOID || tt == TOK_IDENTIFIER) {
        if (tok.value == "Function") {
            parser_advance(p); // skip Function
            if (p.current_tok.type == TOK_LPAREN) {
                parser_advance(p); // skip '('
                let ret_ty -> Struct = parse_return_type(p);
                if (p.current_tok.type != TOK_RPAREN) {
                    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after Function return type.");
                }
                parser_advance(p); // skip ')'

                let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                type_node = FunctionTypeNode(type=NODE_FUNCTION_TYPE, return_type=ret_ty, pos=pos);
            } else {
                let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                type_node = VarAccessNode(type=NODE_VAR_ACCESS, name_tok=tok, pos=pos);
            }
        }
        else if (tok.value == "Method") {
            parser_advance(p); // skip Method
            if (p.current_tok.type == TOK_LPAREN) {
                parser_advance(p); // skip '('
                let ret_ty -> Struct = parse_return_type(p);
                if (p.current_tok.type != TOK_RPAREN) {
                    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after Method return type.");
                }
                parser_advance(p); // skip ')'

                let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                type_node = MethodTypeNode(type=NODE_METHOD_TYPE, return_type=ret_ty, pos=pos);
            } else {
                let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                type_node = VarAccessNode(type=NODE_VAR_ACCESS, name_tok=tok, pos=pos);
            }
        }
        else if (tok.value == "Vector") {
            parser_advance(p); // skip Vector
            if (p.current_tok.type != TOK_LPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after Vector.");
            }
            parser_advance(p); // skip (
            
            let elem_type -> Struct = parse_return_type(p);
            
            if (p.current_tok.type != TOK_RPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after Vector type.");
            }
            parser_advance(p); // skip )
            
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            type_node = VectorTypeNode(type=NODE_VECTOR_TYPE, element_type=elem_type, pos=pos);
        }
        else if (tok.value == "Array") {
            parser_advance(p); // skip Array
            if (p.current_tok.type != TOK_LPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after Array.");
            }
            parser_advance(p); // skip (
            
            let elem_type -> Struct = parse_return_type(p);
            
            if (p.current_tok.type != TOK_RPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after Array type.");
            }
            parser_advance(p); // skip )
            
            type_node = SliceTypeNode(type=NODE_SLICE_TYPE, element_type=elem_type, pos=start_pos);
        }
        else {
            parser_advance(p);
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            type_node = VarAccessNode(type=NODE_VAR_ACCESS, name_tok=tok, pos=pos);
        }
        return type_node;
    }
    
    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected type name.");
    return null;
}

func parse_return_type(p -> Parser) -> Struct {
    let type_node -> Struct = parse_type_base(p);
    if (type_node is null) { return null; }

    let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    let sizes -> Vector(Struct) = [];

    while (p.current_tok.type == TOK_LBRACKET) {
        parser_advance(p); // skip '['

        if (p.current_tok.type != TOK_INT) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected integer literal for array size.");
        }
        sizes.append(p.current_tok);
        parser_advance(p);
        
        if (p.current_tok.type != TOK_RBRACKET) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ']' after array size.");
        }
        parser_advance(p); // skip ']'
    }

    let s_len -> Int = 0;
    if (sizes is !null) { s_len = sizes.length(); }
    let s_i -> Int = s_len - 1;
    while (s_i >= 0) {
        let s_tok -> Token = sizes[s_i];
        type_node = ArrayTypeNode(
            type=NODE_ARRAY_TYPE, 
            base_type=type_node, 
            size_tok=s_tok, 
            pos=start_pos
        );
        s_i -= 1;
    }

    return type_node;
}

// parse "ptr(opt *N) name -> Type"
func parse_typed_identifier_param(p -> Parser) -> Struct {
    let is_ptr -> Bool = false;
    let level -> Int = 0;
    let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);

    if (p.current_tok.type == TOK_PTR) {
        is_ptr = true;
        level = 1;
        parser_advance(p); // skip ptr

        if (p.current_tok.type == TOK_MUL) {
            parser_advance(p);
            if (p.current_tok.type == TOK_INT) {
                level = String_to_Int(p.current_tok.value);
                parser_advance(p);
            } else {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected pointer level.");
            }
        }
    }

    if (p.current_tok.type != TOK_IDENTIFIER && p.current_tok.type != TOK_SELF && p.current_tok.type != TOK_THIS) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected identifier.");
    }
    let name_tok -> Token = p.current_tok;
    parser_advance(p);

    if (p.current_tok.type != TOK_TYPE_ARROW) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '->'.");
    }
    parser_advance(p);

    let type_node -> Struct = parse_return_type(p);

    if (type_node is !null) {
        let t_base -> BaseNode = type_node;
        if (t_base.type == NODE_PTR_TYPE) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=name_tok.line, col=name_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Syntax Error: Cannot use '-> ptr Type' for variable or parameter declarations. Use 'ptr " + name_tok.value + " -> Type' instead.");
        }
    }

    if is_ptr {
        type_node = PointerTypeNode(type=NODE_PTR_TYPE, base_type=type_node, level=level, pos=start_pos);
    }

    return TypedIdent(name_tok=name_tok, type_node=type_node);
}

func atom(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;

    // Integer literals
    if (tok.type == TOK_INT) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return IntNode(type=NODE_INT, tok=tok, pos=pos);
    }

    // Floating-point literals
    if (tok.type == TOK_FLOAT) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return FloatNode(type=NODE_FLOAT, tok=tok, pos=pos);
    }

    // Boolean
    if (tok.type == TOK_TRUE) { 
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return BooleanNode(type=NODE_BOOL, tok=tok, value=1, pos=pos); 
    }
    if (tok.type == TOK_FALSE) { 
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return BooleanNode(type=NODE_BOOL, tok=tok, value=0, pos=pos); 
    }

    // String
    if (tok.type == TOK_STR_LIT) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return StringNode(type=NODE_STRING, tok=tok, pos=pos);
    }

    // nullptr
    if (tok.type == TOK_NULLPTR) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return NullPtrNode(type=NODE_NULLPTR, pos=pos);
    }

    if (tok.type == TOK_NULL) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return NullNode(type=NODE_NULL, pos=pos);
    }
    
    // Variable access
    if (tok.type == TOK_IDENTIFIER) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return VarAccessNode(type=NODE_VAR_ACCESS, name_tok=tok, pos=pos);
    }

    if (tok.type == TOK_THIS) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        let this_tok -> Token = Token(type=TOK_IDENTIFIER, value="this", line=tok.line, col=tok.col);
        return VarAccessNode(type=NODE_VAR_ACCESS, name_tok=this_tok, pos=pos);
    }

    if (tok.type == TOK_SELF) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        let self_tok -> Token = Token(type=TOK_IDENTIFIER, value="self", line=tok.line, col=tok.col);
        return VarAccessNode(type=NODE_VAR_ACCESS, name_tok=self_tok, pos=pos);
    }

    if (tok.type == TOK_SUPER) {
        parser_advance(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return SuperNode(type=NODE_SUPER, pos=pos);
    }

    // Parenthesized expressions
    if (tok.type == TOK_LPAREN) {
        parser_advance(p);
        let node -> Struct = expression(p);
        
        if (p.current_tok.type != TOK_RPAREN) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')'. ");
        }
        parser_advance(p);
        return node;
    }

    if (tok.type == TOK_LBRACE) {
        parser_advance(p); // skip '{'
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        
        let pairs -> Vector(Struct) = [];

        if (p.current_tok.type != TOK_RBRACE) {
            while (true) {
                let key_node -> Struct = expression(p);
                
                if (p.current_tok.type != TOK_COLON) {
                    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ':' after map key.");
                }
                parser_advance(p); // skip ':'
                
                let val_node -> Struct = expression(p);
                
                pairs.append(MapPairNode(key=key_node, value=val_node));
                
                if (p.current_tok.type == TOK_COMMA) {
                    parser_advance(p); // skip ','
                } else {
                    break;
                }
            }
        }
        
        if (p.current_tok.type != TOK_RBRACE) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '}' to close map literal.");
        }
        parser_advance(p); // skip '}'
        
        return MapLitNode(type=NODE_MAP_LIT, pairs=pairs, pos=pos);
    }

    if (tok.type == TOK_LBRACKET) {
        parser_advance(p); // skip [
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        
        let elements -> Vector(Struct) = [];
        let count -> Int = 0;
        
        if (p.current_tok.type != TOK_RBRACKET) {
            while (true) {
                let val -> Struct = expression(p);
                elements.append(ArgNode(val=val, name=null));
                count += 1;
                
                if (p.current_tok.type == TOK_COMMA) {
                    parser_advance(p);
                } else {
                    break;
                }
            }
        }
        
        if (p.current_tok.type != TOK_RBRACKET) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ']' after vector elements.");
        }
        parser_advance(p); // skip ]
        
        return VectorLitNode(type=NODE_VECTOR_LIT, elements=elements, count=count, pos=pos);
    }

    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    WhitelangExceptions.throw_invalid_syntax(err_pos, "Unexpected token: " + get_token_name(tok.type));
    return null;
}

func parse_args(p -> Parser) -> Vector(Struct) {
    if (p.current_tok.type == TOK_RPAREN) { return null; }

    let args -> Vector(Struct) = [];
    
    while (p.current_tok.type != TOK_RPAREN && p.current_tok.type != TOK_EOF) {
        let arg_name -> String = null;
        if (p.current_tok.type == TOK_IDENTIFIER && peek_type(p) == TOK_ASSIGN) {
            arg_name = p.current_tok.value;
            parser_advance(p);
            parser_advance(p);
        }
        
        let val -> Struct = expression(p);
        args.append(ArgNode(val=val, name=arg_name));

        if (p.current_tok.type == TOK_COMMA) { parser_advance(p); }
        else { break; }
    }
    return args;
}

func postfix_expr(p -> Parser) -> Struct {
    let node -> Struct = atom(p);

    while (p.current_tok.type == TOK_INC || p.current_tok.type == TOK_DEC || p.current_tok.type == TOK_LPAREN ||
           p.current_tok.type == TOK_DOT || p.current_tok.type == TOK_LBRACKET) {
        // ++ / --
        if (p.current_tok.type == TOK_INC || p.current_tok.type == TOK_DEC) {
            let op_tok -> Token = p.current_tok;
            parser_advance(p);
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            node = PostfixOpNode(type=NODE_POSTFIX, node=node, op_tok=op_tok, pos=pos);
        }

        else if (p.current_tok.type == TOK_LPAREN) {
            let paren_tok -> Token = p.current_tok; // '('
            parser_advance(p); // skip '('
            
            let args -> Struct = parse_args(p);
            
            if (p.current_tok.type != TOK_RPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after arguments. ");
            }
            parser_advance(p); // skip ')'

            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=paren_tok.line, col=paren_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            node = CallNode(type=NODE_CALL, callee=node, args=args, pos=pos);
        }

        else if (p.current_tok.type == TOK_DOT) {
            parser_advance(p); // skip .
            if (p.current_tok.type != TOK_IDENTIFIER) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected field name after '.'.");
            }
            let field_name -> String = p.current_tok.value;
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            parser_advance(p);
    
            node = FieldAccessNode(type=NODE_FIELD_ACCESS, obj=node, field_name=field_name, pos=pos);
        }

        else if (p.current_tok.type == TOK_LBRACKET) {
            let bracket_tok -> Token = p.current_tok;
            parser_advance(p); // skip [
            
            let first_idx -> Struct = expression(p);
            if (p.current_tok.type == TOK_COLON) {
                parser_advance(p); // skip ':'
                let second_idx -> Struct = expression(p);
                
                if (p.current_tok.type != TOK_RBRACKET) {
                    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ']' after slice end index.");
                }
                parser_advance(p); // skip ']'
                
                let pos -> Position = WhitelangExceptions.Position(idx=0, ln=bracket_tok.line, col=bracket_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                node = SliceAccessNode(type=NODE_SLICE_ACCESS, target=node, start_idx=first_idx, end_idx=second_idx, pos=pos);
            }
            else {
                if (p.current_tok.type != TOK_RBRACKET) {
                    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ']' after index.");
                }
                parser_advance(p); // skip ']'
                
                let pos -> Position = WhitelangExceptions.Position(idx=0, ln=bracket_tok.line, col=bracket_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                node = IndexAccessNode(type=NODE_INDEX_ACCESS, target=node, index_node=first_idx, pos=pos);
            }
        }
    }
    return node;
}

func unary_expr(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;

    // ref x
    if (tok.type == TOK_REF) {
        parser_advance(p);
        let node -> Struct = unary_expr(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return RefNode(type=NODE_REF, node=node, pos=pos);
    }
    
    // deref x or deref*N x
    if (tok.type == TOK_DEREF) {
        parser_advance(p);
        let level -> Int = 1;
        if (p.current_tok.type == TOK_MUL) {
            parser_advance(p);
            if (p.current_tok.type == TOK_INT) {
                level = String_to_Int(p.current_tok.value);
                parser_advance(p);
            } else {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected dereference level.");
            }
        }
        let node -> Struct = unary_expr(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return DerefNode(type=NODE_DEREF, node=node, level=level, pos=pos);
    }
    
    // -5, +3.14, !b, ~c
    if (tok.type == TOK_PLUS || tok.type == TOK_SUB || tok.type == TOK_NOT || tok.type == TOK_BIT_NOT) {
        parser_advance(p);
        let node -> Struct = unary_expr(p); // recursive
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return UnaryOpNode(type=NODE_UNARYOP, op_tok=tok, node=node, pos=pos);
    }
    
    return power(p);
}

func power(p -> Parser) -> Struct {
    let left -> Struct = postfix_expr(p);

    if (p.current_tok.type == TOK_POW) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = factor(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        return BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    
    return left;
}

func shift_expr(p -> Parser) -> Struct {
    let left -> Struct = arith_expr(p);
    while (p.current_tok.type == TOK_LSHIFT || p.current_tok.type == TOK_RSHIFT) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = arith_expr(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func comp_expr(p -> Parser) -> Struct {
    let left -> Struct = shift_expr(p);

    while (p.current_tok.type == TOK_EE  || p.current_tok.type == TOK_NE  || 
           p.current_tok.type == TOK_LT  || p.current_tok.type == TOK_GT  ||
           p.current_tok.type == TOK_LTE || p.current_tok.type == TOK_GTE ||
           p.current_tok.type == TOK_IS) {
        let op_tok -> Token = p.current_tok;
        let node_type -> Int = NODE_BINOP;

        if (op_tok.type == TOK_IS) {
            parser_advance(p); // skip 'is'
            if (p.current_tok.type == TOK_NOT) {
                let not_tok -> Token = p.current_tok;
                parser_advance(p); // skip '!'
                op_tok = Token(type=TOK_IS, value="is !", line=op_tok.line, col=op_tok.col);
                node_type = NODE_IS_NOT;
            } else {
                node_type = NODE_IS;
            }
            
            let right -> Struct = shift_expr(p);
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            left = BinOpNode(type=node_type, left=left, op_tok=op_tok, right=right, pos=pos);
            
            // continue loop
        } else {
            parser_advance(p);
            let right -> Struct = shift_expr(p);
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
        }
    }
    return left;
}

func bitwise_and(p -> Parser) -> Struct {
    let left -> Struct = comp_expr(p);
    while (p.current_tok.type == TOK_BIT_AND) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = comp_expr(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func bitwise_xor(p -> Parser) -> Struct {
    let left -> Struct = bitwise_and(p);
    while (p.current_tok.type == TOK_BIT_XOR) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = bitwise_and(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func bitwise_or(p -> Parser) -> Struct {
    let left -> Struct = bitwise_xor(p);
    while (p.current_tok.type == TOK_BIT_OR) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = bitwise_xor(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func logic_and(p -> Parser) -> Struct {
    let left -> Struct = bitwise_or(p);
    while (p.current_tok.type == TOK_AND) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = bitwise_or(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func logic_or(p -> Parser) -> Struct {
    let left -> Struct = logic_and(p);

    while (p.current_tok.type == TOK_OR) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = logic_and(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    return left;
}

func assignment(p -> Parser) -> Struct {
    let left -> Struct = logic_or(p);

    // =
    if (p.current_tok.type == TOK_ASSIGN) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p); // skip '='
        let right -> Struct = assignment(p);
        
        let base -> BaseNode = left;
        if (base.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = left;
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            return VarAssignNode(type=NODE_VAR_ASSIGN, name_tok=v_node.name_tok, value=right, pos=pos);
        } else if (base.type == NODE_FIELD_ACCESS) {
            let f_node -> FieldAccessNode = left;
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            return FieldAssignNode(type=NODE_FIELD_ASSIGN, obj=f_node.obj, field_name=f_node.field_name, value=right, pos=pos);
        } else if (base.type == NODE_DEREF) {
            let d_node -> DerefNode = left;
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            return PtrAssignNode(type=NODE_PTR_ASSIGN, pointer=d_node, value=right, pos=pos);
        } else if (base.type == NODE_INDEX_ACCESS) {
            let idx_node -> IndexAccessNode = left;
            let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            return IndexAssignNode(type=NODE_INDEX_ASSIGN, target=idx_node.target, index_node=idx_node.index_node, value=right, pos=pos);
        } else {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Invalid assignment target.");
        }
    }

    let op_type -> Int = p.current_tok.type;
    if (op_type == TOK_PLUS_ASSIGN || op_type == TOK_SUB_ASSIGN || 
        op_type == TOK_MUL_ASSIGN || op_type == TOK_DIV_ASSIGN || 
        op_type == TOK_MOD_ASSIGN || op_type == TOK_POW_ASSIGN ||
        op_type == TOK_BIT_AND_ASSIGN || op_type == TOK_BIT_OR_ASSIGN || 
        op_type == TOK_BIT_XOR_ASSIGN || op_type == TOK_LSHIFT_ASSIGN || op_type == TOK_RSHIFT_ASSIGN) {
        
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = assignment(p);

        let bin_op_type -> Int = 0;
        if (op_type == TOK_PLUS_ASSIGN) { bin_op_type = TOK_PLUS; }
        if (op_type == TOK_SUB_ASSIGN)  { bin_op_type = TOK_SUB; }
        if (op_type == TOK_MUL_ASSIGN)  { bin_op_type = TOK_MUL; }
        if (op_type == TOK_DIV_ASSIGN)  { bin_op_type = TOK_DIV; }
        if (op_type == TOK_MOD_ASSIGN)  { bin_op_type = TOK_MOD; }
        if (op_type == TOK_POW_ASSIGN)  { bin_op_type = TOK_POW; }
        if (op_type == TOK_BIT_AND_ASSIGN) { bin_op_type = TOK_BIT_AND; }
        if (op_type == TOK_BIT_OR_ASSIGN)  { bin_op_type = TOK_BIT_OR; }
        if (op_type == TOK_BIT_XOR_ASSIGN) { bin_op_type = TOK_BIT_XOR; }
        if (op_type == TOK_LSHIFT_ASSIGN)  { bin_op_type = TOK_LSHIFT; }
        if (op_type == TOK_RSHIFT_ASSIGN)  { bin_op_type = TOK_RSHIFT; }
        
        let bin_tok -> Token = Token(type=bin_op_type, value="compound_op", line=op_tok.line, col=op_tok.col);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);

        let bin_node -> BinOpNode = BinOpNode(type=NODE_BINOP, left=left, op_tok=bin_tok, right=right, pos=pos);

        let base -> BaseNode = left;
        
        // a += 1
        if (base.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = left;
            return VarAssignNode(type=NODE_VAR_ASSIGN, name_tok=v_node.name_tok, value=bin_node, pos=pos);
        } 
        // s.x += 1
        else if (base.type == NODE_FIELD_ACCESS) {
            let f_node -> FieldAccessNode = left;
            return FieldAssignNode(type=NODE_FIELD_ASSIGN, obj=f_node.obj, field_name=f_node.field_name, value=bin_node, pos=pos);
        }
        // (deref p) += 1
        else if (base.type == NODE_DEREF) {
            let d_node -> DerefNode = left;
            return PtrAssignNode(type=NODE_PTR_ASSIGN, pointer=d_node, value=bin_node, pos=pos);
        }

        else {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Invalid compound assignment target. Only variables, fields, and pointers are supported.");
        }
    }

    return left;
}

func factor(p -> Parser) -> Struct {
    return unary_expr(p);
}

func expression(p -> Parser) -> Struct {
    return assignment(p);
}

func term(p -> Parser) -> Struct {
    let left -> Struct = factor(p);

    // left-associative
    while (p.current_tok.type == TOK_MUL || p.current_tok.type == TOK_DIV || p.current_tok.type == TOK_MOD) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = factor(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    
    return left;
}

func arith_expr(p -> Parser) -> Struct {
    let left -> Struct = term(p);

    // left-associative
    while (p.current_tok.type == TOK_PLUS || p.current_tok.type == TOK_SUB) {
        let op_tok -> Token = p.current_tok;
        parser_advance(p);
        let right -> Struct = term(p);
        let pos -> Position = WhitelangExceptions.Position(idx=0, ln=op_tok.line, col=op_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        left = BinOpNode(type=NODE_BINOP, left=left, op_tok=op_tok, right=right, pos=pos);
    }
    
    return left;
}

func var_decl_core(p -> Parser, is_const -> Bool) -> Struct {
    let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);

    let tid -> TypedIdent = parse_typed_identifier_param(p);

    let val_node -> Struct = null;
    if (p.current_tok.type == TOK_ASSIGN) {
        parser_advance(p);
        val_node = expression(p);
    }
    
    return VarDeclareNode(type=NODE_VAR_DECL, name_tok=tid.name_tok, type_node=tid.type_node, value=val_node, is_const=is_const, alloc_reg = "", pos=start_pos);
}

func var_decl(p -> Parser) -> Struct {
    let is_const -> Bool = false;
    if (p.current_tok.type == TOK_CONST) {
        is_const = true;
    }
    parser_advance(p); // skip 'let' or 'const'
    return var_decl_core(p, is_const);
}

func parse_block(p -> Parser) -> Struct {
    // '{'
    if (p.current_tok.type != TOK_LBRACE) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '{' to start a block. ");
    }
    parser_advance(p); // skip {

    let stmts -> Vector(Struct) = [];
    while (p.current_tok.type != TOK_RBRACE && p.current_tok.type != TOK_EOF) {
        let stmt -> Struct = statement(p);
        let base -> BaseNode = stmt;
        let is_compound -> Bool = false;
        if (base.type == NODE_IF || base.type == NODE_BLOCK || base.type == NODE_WHILE || base.type == NODE_FOR || base.type == NODE_FUNC_DEF) {
            is_compound = true;
        }

        if (p.current_tok.type == TOK_SEMICOLON) {
            parser_advance(p);
        } else {
            if (!is_compound) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ';' after statement in block. ");
                synchronize(p);
            }
        }
        if (stmt is !null) {
            stmts.append(stmt);
        }
    }
    if (p.current_tok.type != TOK_RBRACE) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '}' to close block. ");
    }
    parser_advance(p); // skip }

    return BlockNode(type=NODE_BLOCK, stmts=stmts);
}

func if_stmt(p -> Parser) -> Struct {
    let if_tok -> Token = p.current_tok;
    parser_advance(p); // skip 'if'
    
    let cond -> Struct = atom(p);
    let body -> Struct = parse_block(p);

    let else_body -> Struct = null;
    if (p.current_tok.type == TOK_ELSE) {
        parser_advance(p); // skip 'else'
        
        if (p.current_tok.type == TOK_IF) { // else if
            else_body = if_stmt(p);
        } else {
            else_body = parse_block(p);
        }
    }
    
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=if_tok.line, col=if_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    return IfNode(type=NODE_IF, condition=cond, body=body, else_body=else_body, pos=pos);
}

func while_stmt(p -> Parser) -> Struct {
    let while_tok -> Token = p.current_tok;
    parser_advance(p); // skip 'while'
    let cond -> Struct = atom(p);
    let body -> Struct = parse_block(p);

    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=while_tok.line, col=while_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    return WhileNode(type=NODE_WHILE, condition=cond, body=body, pos=pos);
}

func break_stmt(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;
    parser_advance(p);
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    return BreakNode(type=NODE_BREAK, pos=pos);
}

func continue_stmt(p -> Parser) -> Struct {
    let tok -> Token = p.current_tok;
    parser_advance(p);
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tok.line, col=tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    return ContinueNode(type=NODE_CONTINUE, pos=pos);
}

func for_stmt(p -> Parser) -> Struct {
    let for_tok -> Token = p.current_tok;
    parser_advance(p); // skip 'for'

    if (p.current_tok.type != TOK_LPAREN) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after 'for'. ");
    }
    parser_advance(p); // skip '('

    let init -> Struct = null;
    if (p.current_tok.type == TOK_SEMICOLON) {
        init = null;
    } else {
        if (p.current_tok.type == TOK_LET) {
            init = var_decl(p);
        } else if ((p.current_tok.type == TOK_IDENTIFIER && peek_type(p) == TOK_TYPE_ARROW) || (p.current_tok.type == TOK_PTR)) {
            init = var_decl_core(p, false); 
        } else {
            init = expression(p);    // i = 0
        }
    }

    if (p.current_tok.type != TOK_SEMICOLON) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ';' after for-init. ");
    }
    parser_advance(p); // skip ';'

    let cond -> Struct = null;
    if (p.current_tok.type != TOK_SEMICOLON) {
        cond = expression(p);
    }

    if (p.current_tok.type != TOK_SEMICOLON) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ';' after for-condition. ");
    }
    parser_advance(p); // skip ';'

    let step -> Struct = null;
    if (p.current_tok.type != TOK_RPAREN) {
        step = expression(p);
    }

    if (p.current_tok.type != TOK_RPAREN) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after for-step. ");
    }
    parser_advance(p); // skip ')'

    let body -> Struct = parse_block(p);
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=for_tok.line, col=for_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    return ForNode(type=NODE_FOR, init=init, cond=cond, step=step, body=body, pos=pos);
}

func statement(p -> Parser) -> Struct {
    if (p.current_tok.type == TOK_LET || p.current_tok.type == TOK_CONST) { return var_decl(p); }
    if (p.current_tok.type == TOK_IF)  { return if_stmt(p); }
    if (p.current_tok.type == TOK_WHILE) { return while_stmt(p); }
    if (p.current_tok.type == TOK_LBRACE) { return parse_block(p); }
    if (p.current_tok.type == TOK_BREAK) { return break_stmt(p); }
    if (p.current_tok.type == TOK_CONTINUE) { return continue_stmt(p); }
    if (p.current_tok.type == TOK_FOR) { return for_stmt(p); }
    if (p.current_tok.type == TOK_FUNC) { return func_def(p); }

    if (p.current_tok.type == TOK_RETURN) { return return_stmt(p); }

    return expression(p);
}


func parse_params(p -> Parser) -> Vector(Struct) {
    if (p.current_tok.type == TOK_RPAREN) {
        return null;
    }
    let params -> Vector(Struct) = [];
    
    let tid -> TypedIdent = parse_typed_identifier_param(p);
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=tid.name_tok.line, col=tid.name_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    let p_node -> ParamNode = ParamNode(type=NODE_PARAM, name_tok=tid.name_tok, type_tok=tid.type_node, pos=pos);
    
    params.append(p_node);
    
    while (p.current_tok.type == TOK_COMMA) {
        parser_advance(p); // skip ','
        
        let next_tid -> TypedIdent = parse_typed_identifier_param(p);
        let next_pos -> Position = WhitelangExceptions.Position(idx=0, ln=next_tid.name_tok.line, col=next_tid.name_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        
        let next_p -> ParamNode = ParamNode(type=NODE_PARAM, name_tok=next_tid.name_tok, type_tok=next_tid.type_node, pos=next_pos);
        params.append(next_p);
    }
    
    return params;
}

func func_def(p -> Parser) -> Struct {
    let func_tok -> Token = p.current_tok;
    parser_advance(p); // skip 'func'
    
    if (p.current_tok.type != TOK_IDENTIFIER) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected function name.");
    }
    let name_tok -> Token = p.current_tok;
    parser_advance(p);

    if (p.current_tok.type != TOK_LPAREN) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after function name.");
    }
    parser_advance(p); // skip '('
    
    let params -> Struct = parse_params(p);
    
    if (p.current_tok.type != TOK_RPAREN) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after parameters.");
    }
    parser_advance(p); // skip ')'
    
    // -> RetType
    if (p.current_tok.type != TOK_TYPE_ARROW) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '->' for return type.");
    }
    parser_advance(p);

    let ret_type_node -> Struct = parse_return_type(p);

    let body -> Struct = parse_block(p);
    
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=func_tok.line, col=func_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    return FunctionDefNode(type=NODE_FUNC_DEF, name_tok=name_tok, params=params, ret_type_tok=ret_type_node, body=body, pos=pos);
}

func return_stmt(p -> Parser) -> Struct {
    let ret_tok -> Token = p.current_tok;
    parser_advance(p);
    
    let val -> Struct = null;
    if (p.current_tok.type != TOK_SEMICOLON) {
        val = expression(p);
    }
    
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=ret_tok.line, col=ret_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    return ReturnNode(type=NODE_RETURN, value=val, pos=pos);
}

func parse_struct_def(p -> Parser) -> Struct {
    let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);

    parser_advance(p); // skip 'struct'

    if (p.current_tok.type != TOK_IDENTIFIER) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected struct name.");
    }
    let name_tok -> Token = p.current_tok;
    parser_advance(p);

    if (p.current_tok.type != TOK_LPAREN) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after struct name.");
    }
    parser_advance(p); // skip '('

    let fields -> Struct = parse_params(p);

    if (p.current_tok.type != TOK_RPAREN) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after struct fields.");
    }
    parser_advance(p); // skip ')'

    let body -> Struct = null;
    if (p.current_tok.type == TOK_LBRACE) {
        body = parse_block(p); // initialization
    }
    return StructDefNode(type=NODE_STRUCT_DEF, name_tok=name_tok, fields=fields, body=body, pos=start_pos);
}

func parse_extern_func(p -> Parser) -> Struct {
    let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    parser_advance(p); // skip 'func'
    
    if (p.current_tok.type != TOK_IDENTIFIER) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected function name in extern block.");
    }
    let name_tok -> Token = p.current_tok;
    parser_advance(p);
    
    if (p.current_tok.type != TOK_LPAREN) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after function name.");
    }
    parser_advance(p); // skip '('

    let params -> Vector(Struct) = [];
    let is_varargs -> Bool = false;
    
    if (p.current_tok.type != TOK_RPAREN) {
        while (true) {
            if (p.current_tok.type == TOK_ELLIPSIS) {
                is_varargs = true;
 
                parser_advance(p); // skip ...
                if (p.current_tok.type == TOK_COMMA) {
                    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                    WhitelangExceptions.throw_invalid_syntax(err_pos, "Varargs '...' must be the last parameter.");
      
                }
                break;
            } else {
                let tid -> TypedIdent = parse_typed_identifier_param(p);
                let param_pos -> Position = WhitelangExceptions.Position(idx=0, ln=tid.name_tok.line, col=tid.name_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                let new_param -> ParamNode = ParamNode(type=NODE_PARAM, name_tok=tid.name_tok, type_tok=tid.type_node, pos=param_pos);

                params.append(new_param);

                if (p.current_tok.type == TOK_COMMA) {
                    parser_advance(p);
                } else {
                    break;
                }
            }
        }
    }
    
    if (p.current_tok.type != TOK_RPAREN) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after extern parameters.");
    }
    parser_advance(p); // skip ')'
    
    let ret_type -> Struct = null;
    if (p.current_tok.type == TOK_TYPE_ARROW) {
        parser_advance(p);
        ret_type = parse_return_type(p);
    } else {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected return type ('-> Type').");
    }
    
    return ExternFuncNode(type=NODE_EXTERN_FUNC, name_tok=name_tok, params=params, ret_type_tok=ret_type, is_varargs=is_varargs, pos=start_pos);
}

func parse_extern(p -> Parser) -> Struct {
    let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    parser_advance(p); // skip 'extern'

    // extern "C" { func ...; }
    if (p.current_tok.type == TOK_STR_LIT) {
        if (p.current_tok.value != "C") {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Only extern \"C\" is supported.");
        }
        parser_advance(p); // skip "C"
        
        if (p.current_tok.type != TOK_LBRACE) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '{' to start extern block.");
        }
        parser_advance(p); // skip '{'
        
        let funcs -> Vector(Struct) = [];
        
        while (p.current_tok.type != TOK_RBRACE && p.current_tok.type != TOK_EOF) {
            if (p.current_tok.type == TOK_FUNC) {
                let func_node -> Struct = parse_extern_func(p);
                if (p.current_tok.type != TOK_SEMICOLON) {
                    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ';' after extern function declaration.");
                }
                parser_advance(p); // skip ';'
                
                funcs.append(func_node);
            } else {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Only function declarations are allowed in extern blocks.");
            }
        }
        
        if (p.current_tok.type != TOK_RBRACE) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '}' to end extern block.");
        }
        parser_advance(p); // skip '}'
        
        return ExternBlockNode(type=NODE_EXTERN_BLOCK, funcs=funcs, pos=start_pos);
    }

    // extern func foo() -> Void from "C";
    else if (p.current_tok.type == TOK_FUNC) {
        let func_node -> Struct = parse_extern_func(p);

        if (p.current_tok.type == TOK_FROM) { 
            parser_advance(p); 
        } else if (p.current_tok.type == TOK_IDENTIFIER) {
            if (p.current_tok.value == "from") {
                parser_advance(p);
            } else {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected 'from' after single-line extern declaration.");
            }
        } else {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected 'from' after single-line extern declaration.");
        }
        
        if (p.current_tok.type != TOK_STR_LIT) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected library name string (e.g. \"C\") after 'from'.");
        }
        if (p.current_tok.value != "C") {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Only \"C\" is supported.");
        }
        parser_advance(p); // skip "C"
        
        if (p.current_tok.type != TOK_SEMICOLON) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ';' at the end of extern declaration.");
        }
        parser_advance(p); // skip ';'
        
        let funcs -> Vector(Struct) = [];
        funcs.append(func_node);
        return ExternBlockNode(type=NODE_EXTERN_BLOCK, funcs=funcs, pos=start_pos);
    }
    
    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected string literal \"C\"or 'func' after extern.");
    return null;
}

func parse_import(p -> Parser) -> Struct {
    let start_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    parser_advance(p); // skip 'import'

    
    let symbols -> Vector(Struct) = [];
    let path_tok -> Token = null;
    // import A, B from "..."
    if (p.current_tok.type == TOK_IDENTIFIER) {
        let parsing -> Bool = true;
        while (parsing) {
            if (p.current_tok.type != TOK_IDENTIFIER) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected identifier in import list.");
            }
            
            let name_tok -> Token = p.current_tok;
            parser_advance(p); // skip name

            let alias_tok -> Token = null;
            if (p.current_tok.type == TOK_AS) {
                parser_advance(p); // skip 'as'
                if (p.current_tok.type != TOK_IDENTIFIER) {
                    let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                    WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected identifier after 'as'.");
                }
                alias_tok = p.current_tok;
                parser_advance(p); // skip alias name
            }

            let node -> ImportSymbolNode = ImportSymbolNode(name_tok=name_tok, alias_tok=alias_tok);
            symbols.append(node);

            if (p.current_tok.type == TOK_COMMA) {
                parser_advance(p); // skip ','
            } else {
                parsing = false;
            }
        }

        if (p.current_tok.type != TOK_FROM) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected 'from' after import symbols.");
        }
        parser_advance(p); // skip 'from'
    }

    if (p.current_tok.type != TOK_STR_LIT) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected string literal for import path.");
    }
    path_tok = p.current_tok;
    parser_advance(p); // skip string

    let alias_tok -> Token = null;
    if (p.current_tok.type == TOK_AS) {
        parser_advance(p); // skip 'as'
        if (p.current_tok.type != TOK_IDENTIFIER) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected identifier after 'as' for module alias.");
        }
        alias_tok = p.current_tok;
        parser_advance(p); // skip alias identifier
    }

    if (p.current_tok.type == TOK_SEMICOLON) {
        parser_advance(p);
    }

    return ImportNode(type=NODE_IMPORT, path_tok=path_tok, symbols=symbols, alias_tok=alias_tok, pos=start_pos);
}

func parse_class_def(p -> Parser) -> Struct {
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
    parser_advance(p); // skip 'class'

    if (p.current_tok.type != TOK_IDENTIFIER) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected class name.");
    }
    let name_tok -> Token = p.current_tok;
    parser_advance(p);

    let parent_tok -> Token = null;
    if (p.current_tok.type == TOK_LPAREN) {
        parser_advance(p); // skip '('
        if (p.current_tok.type != TOK_IDENTIFIER) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected parent class name after '('.");
        }
        parent_tok = p.current_tok;
        parser_advance(p); // skip identifier
        if (p.current_tok.type != TOK_RPAREN) {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after parent class name.");
        }
        parser_advance(p); // skip ')'
    }

    if (p.current_tok.type != TOK_LBRACE) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '{' before class body.");
    }
    parser_advance(p); // skip '{'

    let fields -> Vector(Struct) = [];
    let methods -> Vector(Struct) = [];

    while (p.current_tok.type != TOK_RBRACE && p.current_tok.type != TOK_EOF) {
        if (p.current_tok.type == TOK_LET) {
            let f_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            parser_advance(p); // skip 'let'

            let tid -> TypedIdent = parse_typed_identifier_param(p);

            if (p.current_tok.type != TOK_ASSIGN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Class fields must be initialized (e.g., = null;).");
            }
            parser_advance(p); // skip '='
    
            let default_val -> Struct = expression(p); 

            if (p.current_tok.type != TOK_SEMICOLON) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ';' after field declaration.");
            }
            parser_advance(p); // skip ';'

            fields.append(VarDeclareNode(type=NODE_VAR_DECL, name_tok=tid.name_tok, type_node=tid.type_node, value=default_val, is_const=false, alloc_reg="", pos=f_pos));
            
        } else if (p.current_tok.type == TOK_METHOD) {
            let m_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            parser_advance(p); // skip 'method'
            let m_name -> Token = p.current_tok;
            parser_advance(p);

            if (p.current_tok.type != TOK_LPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after method name.");
            }
            parser_advance(p); // skip '('

            let params -> Vector(Struct) = parse_params(p); 

            if (p.current_tok.type != TOK_RPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after parameters.");
            }
            parser_advance(p); // skip ')'

            if (p.current_tok.type != TOK_TYPE_ARROW) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '->' for return type.");
            }
            parser_advance(p); // skip '->'

            let ret_type -> Struct = parse_return_type(p);

            let body -> Struct = parse_block(p); 
            
            methods.append(MethodDefNode(type=NODE_METHOD_DEF, pos=m_pos, name_tok=m_name, params=params, return_type=ret_type, body=body, is_override=false));

        } else if (p.current_tok.type == TOK_IDENTIFIER && p.current_tok.value == "init") {
            let init_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            parser_advance(p); // skip 'init'

            let init_name_tok -> Token = Token(type=TOK_IDENTIFIER, value="$init", line=init_pos.ln, col=init_pos.col);
            
            if (p.current_tok.type != TOK_LPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after init.");
            }
            parser_advance(p); // skip '('

            let params -> Vector(Struct) = parse_params(p); 

            if (p.current_tok.type != TOK_RPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after parameters.");
            }
            parser_advance(p); // skip ')'

            let void_tok -> Token = Token(type=TOK_T_VOID, value="Void", line=init_pos.ln, col=init_pos.col);
            let ret_type -> Struct = VarAccessNode(type=NODE_VAR_ACCESS, name_tok=void_tok, pos=init_pos);
            if (p.current_tok.type == TOK_TYPE_ARROW) {
                parser_advance(p);
                parse_return_type(p);
            }

            let body -> Struct = parse_block(p); 
            
            methods.append(MethodDefNode(type=NODE_METHOD_DEF, pos=init_pos, name_tok=init_name_tok, params=params, return_type=ret_type, body=body, is_override=false));

        } else if (p.current_tok.type == TOK_IDENTIFIER && p.current_tok.value == "deinit") {
            let deinit_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            parser_advance(p); // skip 'deinit'

            let deinit_name_tok -> Token = Token(type=TOK_IDENTIFIER, value="$deinit", line=deinit_pos.ln, col=deinit_pos.col);
            
            if (p.current_tok.type != TOK_LPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '(' after deinit.");
            }
            parser_advance(p); // skip '('

            let params -> Vector(Struct) = parse_params(p);

            if (p.current_tok.type != TOK_RPAREN) {
                let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
                WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected ')' after parameters.");
            }
            parser_advance(p); // skip ')'

            let void_tok -> Token = Token(type=TOK_T_VOID, value="Void", line=deinit_pos.ln, col=deinit_pos.col);
            let ret_type -> Struct = VarAccessNode(type=NODE_VAR_ACCESS, name_tok=void_tok, pos=deinit_pos);
            if (p.current_tok.type == TOK_TYPE_ARROW) {
                parser_advance(p);
                parse_return_type(p);
            }

            let body -> Struct = parse_block(p); 
            methods.append(MethodDefNode(type=NODE_METHOD_DEF, pos=deinit_pos, name_tok=deinit_name_tok, params=params, return_type=ret_type, body=body, is_override=false));

        } else {
            let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
            WhitelangExceptions.throw_invalid_syntax(err_pos, "Only fields , methods , constructors and destructors are allowed in class body.");
        }
    }

    if (p.current_tok.type != TOK_RBRACE) {
        let err_pos -> Position = WhitelangExceptions.Position(idx=0, ln=p.current_tok.line, col=p.current_tok.col, text=p.lexer.text, fn=p.lexer.pos.fn);
        WhitelangExceptions.throw_invalid_syntax(err_pos, "Expected '}' after class body.");
    }
    parser_advance(p); // skip '}'

    return ClassDefNode(type=NODE_CLASS_DEF, pos=pos, name_tok=name_tok, parent_tok=parent_tok, fields=fields, methods=methods);
}