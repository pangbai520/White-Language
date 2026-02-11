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

func new_lexer(text -> String) -> Lexer {
    let pos -> Position = Position(idx=-1, ln=0, col=-1, text=text);
    let l -> Lexer = Lexer(text=text, pos=pos, current_char=0);
    advance(l);
    return l;
}

func advance(l -> Lexer) -> Void {
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
    
    advance(l); // skip opening "
    
    let result -> String = "";
    
    // " and \
    while (l.current_char != 34 && l.current_char != 0) {
        if (l.current_char == 92) { // \
            advance(l); 
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
                result = result + l.text.slice(idx, idx + 1); 
            }
            advance(l); // consume the escaped char
        } else {
            let idx -> Int = l.pos.idx;
            result = result + l.text.slice(idx, idx + 1);
            advance(l);
        }
    }
    
    if (l.current_char == 34) {
        advance(l); // skip closing "
        return Token(type=TOK_STR_LIT, value=result, line=start_ln, col=start_col);
    }
    
    throw_illegal_char(l.pos, "Unterminated string literal. ");
    return Token(type=TOK_EOF, value="", line=0, col=0);
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
        advance(l);
    }
    
    let value -> String = l.text.slice(start_pos, l.pos.idx);
    if (dot_count == 1) {
        return Token(type=TOK_FLOAT, value=value, line=start_line, col=start_col);
    }
    return Token(type=TOK_INT, value=value, line=start_line, col=start_col);
}

func get_identifier(l -> Lexer) -> Token {
    let start_line -> Int = l.pos.ln;
    let start_col  -> Int = l.pos.col;
    let start_pos  -> Int = l.pos.idx;

    while (l.current_char != 0 && (is_alpha(l.current_char) || is_digit(l.current_char))) {
        advance(l);
    }

    let value -> String = l.text.slice(start_pos, l.pos.idx);

    // Keywords mapping
    if (value == "let") {return Token(type=TOK_LET, value=value, line=start_line, col=start_col);}
    if (value == "Int") {return Token(type=TOK_T_INT, value=value, line=start_line, col=start_col);}
    if (value == "Float") {return Token(type=TOK_T_FLOAT, value=value, line=start_line, col=start_col);}
    if (value == "String") {return Token(type=TOK_T_STRING, value=value, line=start_line, col=start_col);}
    if (value == "Bool") {return Token(type=TOK_T_BOOL, value=value, line=start_line, col=start_col);}
    if (value == "Void") {return Token(type=TOK_T_VOID, value=value, line=start_line, col=start_col);}
    if (value == "true") {return Token(type=TOK_TRUE, value=value, line=start_line, col=start_col);}
    if (value == "false") {return Token(type=TOK_FALSE, value=value, line=start_line, col=start_col);}
    if (value == "if") {return Token(type=TOK_IF, value=value, line=start_line, col=start_col);}
    if (value == "else") {return Token(type=TOK_ELSE, value=value, line=start_line, col=start_col);}
    if (value == "while") {return Token(type=TOK_WHILE, value=value, line=start_line, col=start_col);}
    if (value == "break") { return Token(type=TOK_BREAK, value=value, line=start_line, col=start_col); }
    if (value == "continue") { return Token(type=TOK_CONTINUE, value=value, line=start_line, col=start_col); }
    if (value == "for")      { return Token(type=TOK_FOR, value=value, line=start_line, col=start_col); }
    if (value == "func")     { return Token(type=TOK_FUNC, value=value, line=start_line, col=start_col); }
    if (value == "return")   { return Token(type=TOK_RETURN, value=value, line=start_line, col=start_col); }
    
    return Token(type=TOK_IDENTIFIER, value=value, line=start_line, col=start_col);
}


func handle_slash(l -> Lexer) -> Token {
    let line -> Int = l.pos.ln;
    let col  -> Int = l.pos.col;
    advance(l); // skip first /

    // /=
    if (l.current_char == 61) {
        advance(l);
        return Token(type=TOK_DIV_ASSIGN, value="/=", line=line, col=col);
    }

    // //
    if (l.current_char == 47) {
        while (l.current_char != 0 && l.current_char != 10) { advance(l); }
        return null;
    }

    // /*  */
    if (l.current_char == 42) {
        advance(l);
        let comment_closed -> Int = 0;
        while (l.current_char != 0 && comment_closed == 0) {
            if (l.current_char == 42) {
                advance(l);
                if (l.current_char == 47) {
                    advance(l);
                    comment_closed = 1;
                }
            } else { advance(l); }
        }
        if (comment_closed == 0) { throw_illegal_char(l.pos, "Unterminated block comment."); }
        return null;
    }

    return Token(type=TOK_DIV, value="/", line=line, col=col);
}


func get_next_token(l -> Lexer) -> Token {
    while (l.current_char != 0) {
        if (is_space(l.current_char)) {
            advance(l);
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

        // + and ++ and +=
        if (char == 43) { 
            advance(l);
            if (l.current_char == 61) { advance(l); return Token(type=TOK_PLUS_ASSIGN, value="+=", line=char_line, col=char_col); } // +=
            if (l.current_char == 43) { advance(l); return Token(type=TOK_INC, value="++", line=char_line, col=char_col); } // ++
            return Token(type=TOK_PLUS, value="+", line=char_line, col=char_col); 
        }

        // - and -- and -> and -=
        if (char == 45) { 
            advance(l); 
            if (l.current_char == 61) { advance(l); return Token(type=TOK_SUB_ASSIGN, value="-=", line=char_line, col=char_col); } // -=
            if (l.current_char == 62) { advance(l); return Token(type=TOK_TYPE_ARROW, value="->", line=char_line, col=char_col); } // ->
            if (l.current_char == 45) { advance(l); return Token(type=TOK_DEC, value="--", line=char_line, col=char_col); } // --
            return Token(type=TOK_SUB, value="-", line=char_line, col=char_col); 
        }

        // * *= ** **=
        if (char == 42) { 
            advance(l); 
            if (l.current_char == 61) { advance(l); return Token(type=TOK_MUL_ASSIGN, value="*=", line=char_line, col=char_col); } // *=
            if (l.current_char == 42) { 
                advance(l); 
                if (l.current_char == 61) { advance(l); return Token(type=TOK_POW_ASSIGN, value="**=", line=char_line, col=char_col); } // **=
                return Token(type=TOK_POW, value="**", line=char_line, col=char_col); // **
            }
            return Token(type=TOK_MUL, value="*", line=char_line, col=char_col); 
        }

        // / /= // /*
        if (char == 47) {
            let tok -> Token = handle_slash(l);
            if (tok == null) { continue; }
            return tok;
        }

        if (char == 37) { 
            advance(l); 
            if (l.current_char == 61) { advance(l); return Token(type=TOK_MOD_ASSIGN, value="%=", line=char_line, col=char_col); } // %=
            return Token(type=TOK_MOD, value="%", line=char_line, col=char_col); 
        }

        // ! and !=
        if (char == 33) {
            advance(l);
            if (l.current_char == 61) { advance(l); return Token(type=TOK_NE, value="!=", line=char_line, col=char_col); }
            return Token(type=TOK_NOT, value="!", line=char_line, col=char_col);
        }

        // = and ==
        if (char == 61) {
            advance(l);
            if (l.current_char == 61) { advance(l); return Token(type=TOK_EE, value="==", line=char_line, col=char_col); }
            return Token(type=TOK_ASSIGN, value="=", line=char_line, col=char_col);
        }

        // < and <=
        if (char == 60) {
            advance(l);
            if (l.current_char == 61) { advance(l); return Token(type=TOK_LTE, value="<=", line=char_line, col=char_col); }
            return Token(type=TOK_LT, value="<", line=char_line, col=char_col);
        }

        // > and >=
        if (char == 62) {
            advance(l);
            if (l.current_char == 61) { advance(l); return Token(type=TOK_GTE, value=">=", line=char_line, col=char_col); }
            return Token(type=TOK_GT, value=">", line=char_line, col=char_col);
        }

        // &&
        if (char == 38) {
            advance(l);
            if (l.current_char == 38) { advance(l); return Token(type=TOK_AND, value="&&", line=char_line, col=char_col); }
            throw_illegal_char(l.pos, "Expected '&' after '&'.");
        }

        // ||
        if (char == 124) {
            advance(l);
            if (l.current_char == 124) { advance(l); return Token(type=TOK_OR, value="||", line=char_line, col=char_col); }
            throw_illegal_char(l.pos, "Expected '|' after '|'.");
        }

        // Single char tokens
        if (char == 40) { advance(l);  return Token(type=TOK_LPAREN,   value="(", line=char_line, col=char_col); }
        if (char == 41) { advance(l);  return Token(type=TOK_RPAREN,   value=")", line=char_line, col=char_col); }
        if (char == 59) { advance(l);  return Token(type=TOK_SEMICOLON,value=";", line=char_line, col=char_col); }
        if (char == 123) { advance(l); return Token(type=TOK_LBRACE,   value="{", line=char_line, col=char_col); }
        if (char == 125) { advance(l); return Token(type=TOK_RBRACE,   value="}", line=char_line, col=char_col); }
        if (char == 44) { advance(l);  return Token(type=TOK_COMMA,    value=",", line=char_line, col=char_col); }

        throw_illegal_char(l.pos, "unknown character '" + char + "'. ");
    }

    return Token(type=TOK_EOF, value="", line=l.pos.ln, col=l.pos.col);
}