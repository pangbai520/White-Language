// core/WhitelangLexer.wl
import "builtin"
import "WhitelangTokens.wl"
import "WhitelangExceptions.wl"

struct Lexer(
    text -> String,
    pos  -> Position, 
    current_char -> Byte
)


func is_space(c -> Byte) -> Bool {
    let b -> Int = c;
    return (b == 32) || (b == 9) || (b == 10) || (b == 13); 
}

func is_digit(c -> Byte) -> Bool {
    let b -> Int = c;
    return (b >= 48) && (b <= 57); 
}

func is_alpha(c -> Byte) -> Bool {
    let b -> Int = c;
    return (b >= 97 && b <= 122) || (b >= 65 && b <= 90) || (b == 95); 
}

func new_lexer(fn -> String, text -> String) -> Lexer {
    let pos -> Position = Position(idx=-1, ln=0, col=-1, text=text, fn=fn);
    let l -> Lexer = Lexer(text=text, pos=pos, current_char=0);
    lexer_advance(l);
    return l;
}

func lexer_advance(l -> Lexer) -> Void {
    advance_pos(l.pos, l.current_char);
    if (l.pos.idx < l.text.length()) {
        l.current_char = l.text[l.pos.idx];
    } else {
        l.current_char = 0;
    }
}

func get_string(l -> Lexer) -> Token {
    let start_ln -> Int = l.pos.ln;
    let start_col -> Int = l.pos.col;
    
    lexer_advance(l); // skip opening "
    
    let result -> String = "";
    
    // " and \
    while (l.current_char != 34 && l.current_char != 0) {
        if (l.current_char == 92) { // \
            lexer_advance(l); 
            if (l.current_char == 110) { // 'n' -> newline
                result = result + "\n";
            } else if (l.current_char == 116) { // 't' -> tab
                result = result + "\t";
            } else if (l.current_char == 114) { // 'r' -> return
                result = result + "\r";
            } else if (l.current_char == 34) { // '"' -> "
                result = result + "\"";
            } else if (l.current_char == 92) { // '\' -> \
                result = result + "\\";
            } else {
                let idx -> Int = l.pos.idx;
                result += l.text.slice(idx, idx + 1); 
            }
            lexer_advance(l); // consume the escaped char
        } else {
            let idx -> Int = l.pos.idx;
            result += l.text.slice(idx, idx + 1);
            lexer_advance(l);
        }
    }
    
    if (l.current_char == 34) {
        lexer_advance(l); // skip closing "
        return WhitelangTokens.Token(type=TOK_STR_LIT, value=result, line=start_ln, col=start_col);
    }
    
    WhitelangExceptions.throw_illegal_char(l.pos, "Unterminated string literal. ");
    return WhitelangTokens.Token(type=TOK_EOF, value="", line=0, col=0);
}


func get_number(l -> Lexer) -> Token {
    let start_line -> Int = l.pos.ln;
    let start_col  -> Int = l.pos.col;
    let start_pos  -> Int = l.pos.idx;
    
    let dot_count -> Int = 0;
    while (l.current_char != 0 && (is_digit(l.current_char) || l.current_char == 46)) {
        if (l.current_char == 46) {
            if (dot_count == 1) { break; }
            dot_count = 1;
        }
        lexer_advance(l);
    }
    
    let value -> String = l.text.slice(start_pos, l.pos.idx);

    if (value.length() > 0) {
        if (value[0] == 46) { // .
            value = "0" + value;
        }
    }

    if (dot_count == 1) {
        return WhitelangTokens.Token(type=TOK_FLOAT, value=value, line=start_line, col=start_col);
    }
    return WhitelangTokens.Token(type=TOK_INT, value=value, line=start_line, col=start_col);
}

func get_identifier(l -> Lexer) -> Token {
    let start_line -> Int = l.pos.ln;
    let start_col  -> Int = l.pos.col;
    let start_pos  -> Int = l.pos.idx;

    while (l.current_char != 0 && (is_alpha(l.current_char) || is_digit(l.current_char))) {
        lexer_advance(l);
    }

    let value -> String = l.text.slice(start_pos, l.pos.idx);

    // Keywords mapping
    if (value == "let") {return WhitelangTokens.Token(type=TOK_LET, value=value, line=start_line, col=start_col);}
    if (value == "Int") {return WhitelangTokens.Token(type=TOK_T_INT, value=value, line=start_line, col=start_col);}
    if (value == "Float") {return WhitelangTokens.Token(type=TOK_T_FLOAT, value=value, line=start_line, col=start_col);}
    if (value == "String") {return WhitelangTokens.Token(type=TOK_T_STRING, value=value, line=start_line, col=start_col);}
    if (value == "Bool") {return WhitelangTokens.Token(type=TOK_T_BOOL, value=value, line=start_line, col=start_col);}
    if (value == "Void") {return WhitelangTokens.Token(type=TOK_T_VOID, value=value, line=start_line, col=start_col);}
    if (value == "true") {return WhitelangTokens.Token(type=TOK_TRUE, value=value, line=start_line, col=start_col);}
    if (value == "false") {return WhitelangTokens.Token(type=TOK_FALSE, value=value, line=start_line, col=start_col);}
    if (value == "if") {return WhitelangTokens.Token(type=TOK_IF, value=value, line=start_line, col=start_col);}
    if (value == "else") {return WhitelangTokens.Token(type=TOK_ELSE, value=value, line=start_line, col=start_col);}
    if (value == "while") {return WhitelangTokens.Token(type=TOK_WHILE, value=value, line=start_line, col=start_col);}
    if (value == "break") { return WhitelangTokens.Token(type=TOK_BREAK, value=value, line=start_line, col=start_col); }
    if (value == "continue") { return WhitelangTokens.Token(type=TOK_CONTINUE, value=value, line=start_line, col=start_col); }
    if (value == "for")      { return WhitelangTokens.Token(type=TOK_FOR, value=value, line=start_line, col=start_col); }
    if (value == "func")     { return WhitelangTokens.Token(type=TOK_FUNC, value=value, line=start_line, col=start_col); }
    if (value == "return")   { return WhitelangTokens.Token(type=TOK_RETURN, value=value, line=start_line, col=start_col); }
    if (value == "struct") { return WhitelangTokens.Token(type=TOK_STRUCT, value=value, line=start_line, col=start_col); }
    if (value == "this")   { return WhitelangTokens.Token(type=TOK_THIS, value=value, line=start_line, col=start_col); }
    if (value == "ptr") { return WhitelangTokens.Token(type=TOK_PTR, value=value, line=start_line, col=start_col); }
    if (value == "ref") { return WhitelangTokens.Token(type=TOK_REF, value=value, line=start_line, col=start_col); }
    if (value == "deref") { return WhitelangTokens.Token(type=TOK_DEREF, value=value, line=start_line, col=start_col); }
    if (value == "nullptr") { return WhitelangTokens.Token(type=TOK_NULLPTR, value=value, line=start_line, col=start_col); }
    if (value == "null") { return WhitelangTokens.Token(type=TOK_NULL, value=value, line=start_line, col=start_col); }
    if (value == "is") { return WhitelangTokens.Token(type=TOK_IS, value=value, line=start_line, col=start_col); }
    if (value == "extern") { return WhitelangTokens.Token(type=TOK_EXTERN, value=value, line=start_line, col=start_col); }
    if (value == "from") { return WhitelangTokens.Token(type=TOK_FROM, value=value, line=start_line, col=start_col); }
    if (value == "import") { return WhitelangTokens.Token(type=TOK_IMPORT, value=value, line=start_line, col=start_col); }
    if (value == "const") { return WhitelangTokens.Token(type=TOK_CONST, value=value, line=start_line, col=start_col); }
    if (value == "as") { return WhitelangTokens.Token(type=TOK_AS, value=value, line=start_line, col=start_col); }
    if (value == "class") { return WhitelangTokens.Token(type=TOK_CLASS, value=value, line=start_line, col=start_col); }
    if (value == "method") { return WhitelangTokens.Token(type=TOK_METHOD, value=value, line=start_line, col=start_col); }
    if (value == "self") { return WhitelangTokens.Token(type=TOK_SELF, value=value, line=start_line, col=start_col); }
    if (value == "super") { return WhitelangTokens.Token(type=TOK_SUPER, value=value, line=start_line, col=start_col); }

    return WhitelangTokens.Token(type=TOK_IDENTIFIER, value=value, line=start_line, col=start_col);
}


func handle_slash(l -> Lexer) -> Token {
    let line -> Int = l.pos.ln;
    let col  -> Int = l.pos.col;
    lexer_advance(l); // skip first /

    // /=
    if (l.current_char == 61) {
        lexer_advance(l);
        return WhitelangTokens.Token(type=TOK_DIV_ASSIGN, value="/=", line=line, col=col);
    }

    // //
    if (l.current_char == 47) {
        while (l.current_char != 0 && l.current_char != 10) { lexer_advance(l); }
        return null;
    }

    // /*  */
    if (l.current_char == 42) {
        lexer_advance(l);
        let comment_closed -> Int = 0;
        while (l.current_char != 0 && comment_closed == 0) {
            if (l.current_char == 42) {
                lexer_advance(l);
                if (l.current_char == 47) {
                    lexer_advance(l);
                    comment_closed = 1;
                }
            } else { lexer_advance(l); }
        }
        if (comment_closed == 0) { WhitelangExceptions.throw_illegal_char(l.pos, "Unterminated block comment."); }
        return null;
    }

    return WhitelangTokens.Token(type=TOK_DIV, value="/", line=line, col=col);
}


func get_next_token(l -> Lexer) -> Token {
    while (l.current_char != 0) {
        if (is_space(l.current_char)) {
            lexer_advance(l);
            continue;
        }

        if (is_digit(l.current_char)) {
            return get_number(l);
        }

        if (is_alpha(l.current_char)) {
            return get_identifier(l);
        }

        let char      -> Byte = l.current_char;
        let char_line -> Int  = l.pos.ln;
        let char_col  -> Int  = l.pos.col;

        if (char == 34) {
            return get_string(l);
        }

        // . and ...
        if (char == 46) {
            let is_ellipsis -> Bool = false;
            if (l.pos.idx + 2 < l.text.length()) {
                let n1 -> Byte = l.text[l.pos.idx + 1];
                let n2 -> Byte = l.text[l.pos.idx + 2];
                if (n1 == 46 && n2 == 46) { // . .
                    is_ellipsis = true;
                }
            }

            if is_ellipsis {
                lexer_advance(l); lexer_advance(l); lexer_advance(l); // consume ...
                return WhitelangTokens.Token(type=TOK_ELLIPSIS, value="...", line=char_line, col=char_col);
            } else {
                lexer_advance(l);
                return WhitelangTokens.Token(type=TOK_DOT, value=".", line=char_line, col=char_col);
            }
        }

        // + and ++ and +=
        if (char == 43) { 
            lexer_advance(l);
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_PLUS_ASSIGN, value="+=", line=char_line, col=char_col); } // +=
            if (l.current_char == 43) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_INC, value="++", line=char_line, col=char_col); } // ++
            return WhitelangTokens.Token(type=TOK_PLUS, value="+", line=char_line, col=char_col); 
        }

        // - and -- and -> and -=
        if (char == 45) { 
            lexer_advance(l); 
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_SUB_ASSIGN, value="-=", line=char_line, col=char_col); } // -=
            if (l.current_char == 62) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_TYPE_ARROW, value="->", line=char_line, col=char_col); } // ->
            if (l.current_char == 45) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_DEC, value="--", line=char_line, col=char_col); } // --
            return WhitelangTokens.Token(type=TOK_SUB, value="-", line=char_line, col=char_col); 
        }

        // * *= ** **=
        if (char == 42) { 
            lexer_advance(l); 
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_MUL_ASSIGN, value="*=", line=char_line, col=char_col); } // *=
            if (l.current_char == 42) { 
                lexer_advance(l); 
                if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_POW_ASSIGN, value="**=", line=char_line, col=char_col); } // **=
                return WhitelangTokens.Token(type=TOK_POW, value="**", line=char_line, col=char_col); // **
            }
            return WhitelangTokens.Token(type=TOK_MUL, value="*", line=char_line, col=char_col); 
        }

        // / /= // /*
        if (char == 47) {
            let tok -> Token = handle_slash(l);
            if (tok is null) { continue; }
            return tok;
        }

        if (char == 37) { 
            lexer_advance(l); 
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_MOD_ASSIGN, value="%=", line=char_line, col=char_col); } // %=
            return WhitelangTokens.Token(type=TOK_MOD, value="%", line=char_line, col=char_col); 
        }

        // ! and !=
        if (char == 33) {
            lexer_advance(l);
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_NE, value="!=", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_NOT, value="!", line=char_line, col=char_col);
        }

        // = and ==
        if (char == 61) {
            lexer_advance(l);
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_EE, value="==", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_ASSIGN, value="=", line=char_line, col=char_col);
        }

        // <, <=, <<, <<=
        if (char == 60) {
            lexer_advance(l);
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_LTE, value="<=", line=char_line, col=char_col); }
            if (l.current_char == 60) { // <<
                lexer_advance(l);
                if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_LSHIFT_ASSIGN, value="<<=", line=char_line, col=char_col); }
                return WhitelangTokens.Token(type=TOK_LSHIFT, value="<<", line=char_line, col=char_col);
            }
            return WhitelangTokens.Token(type=TOK_LT, value="<", line=char_line, col=char_col);
        }

        // >, >=, >>, >>=
        if (char == 62) {
            lexer_advance(l);
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_GTE, value=">=", line=char_line, col=char_col); }
            if (l.current_char == 62) { // >>
                lexer_advance(l);
                if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_RSHIFT_ASSIGN, value=">>=", line=char_line, col=char_col); }
                return WhitelangTokens.Token(type=TOK_RSHIFT, value=">>", line=char_line, col=char_col);
            }
            return WhitelangTokens.Token(type=TOK_GT, value=">", line=char_line, col=char_col);
        }

        // &, &&, &=
        if (char == 38) {
            lexer_advance(l);
            if (l.current_char == 38) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_AND, value="&&", line=char_line, col=char_col); }
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_BIT_AND_ASSIGN, value="&=", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_BIT_AND, value="&", line=char_line, col=char_col);
        }

        // |, ||, |=
        if (char == 124) {
            lexer_advance(l);
            if (l.current_char == 124) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_OR, value="||", line=char_line, col=char_col); }
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_BIT_OR_ASSIGN, value="|=", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_BIT_OR, value="|", line=char_line, col=char_col);
        }

        // ^, ^=
        if (char == 94) {
            lexer_advance(l);
            if (l.current_char == 61) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_BIT_XOR_ASSIGN, value="^=", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_BIT_XOR, value="^", line=char_line, col=char_col);
        }

        // ~
        if (char == 126) {
            lexer_advance(l);
            return WhitelangTokens.Token(type=TOK_BIT_NOT, value="~", line=char_line, col=char_col);
        }

        // Single char tokens
        if (char == 40) { lexer_advance(l);  return WhitelangTokens.Token(type=TOK_LPAREN,   value="(", line=char_line, col=char_col); }
        if (char == 41) { lexer_advance(l);  return WhitelangTokens.Token(type=TOK_RPAREN,   value=")", line=char_line, col=char_col); }
        if (char == 59) { lexer_advance(l);  return WhitelangTokens.Token(type=TOK_SEMICOLON,value=";", line=char_line, col=char_col); }
        if (char == 123) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_LBRACE,   value="{", line=char_line, col=char_col); }
        if (char == 125) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_RBRACE,   value="}", line=char_line, col=char_col); }
        if (char == 44) { lexer_advance(l);  return WhitelangTokens.Token(type=TOK_COMMA,    value=",", line=char_line, col=char_col); }
        if (char == 91) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_LBRACKET, value="[", line=char_line, col=char_col); }
        if (char == 93) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_RBRACKET, value="]", line=char_line, col=char_col); }
        if (char == 58) { lexer_advance(l); return WhitelangTokens.Token(type=TOK_COLON, value=":", line=char_line, col=char_col); }

        WhitelangExceptions.throw_illegal_char(l.pos, "unknown character '" + char + "'. ");
    }

    return WhitelangTokens.Token(type=TOK_EOF, value="", line=l.pos.ln, col=l.pos.col);
}
