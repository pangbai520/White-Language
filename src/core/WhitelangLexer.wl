// core/WhitelangLexer.wl
import "builtin"

import * from "WhitelangTokens.wl"
import Position from "WhitelangExceptions.wl"

struct Lexer(
    text -> String,
    pos  -> Position, 
    current_char -> Char
)


func is_space(c -> Char) -> Bool {
    return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r');
}

func is_digit(c -> Char) -> Bool {
    return (c >= '0') && (c <= '9');
}

func is_alpha(c -> Char) -> Bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

func is_digit_for_base(c -> Char, base -> Int) -> Bool {
    if (c >= '0' && c <= '9') {
        return Int(c) - Int('0') < base;
    }
    if (base == 16) {
        return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }
    return false;
}

func report_bad_number(l -> Lexer, line -> Int, col -> Int, value -> String) -> Void {
    let pos -> Position = WhitelangExceptions.Position(idx=0, ln=line, col=col, text=l.text, fn=l.pos.fn);
    WhitelangExceptions.throw_invalid_syntax(pos, "Invalid numeric literal '" + value + "'.");
}

func validate_number(l -> Lexer, line -> Int, col -> Int, value -> String, is_float -> Bool) -> Bool {
    if (value.length() == 0) { return false; }

    let end -> Int = value.length();
    if is_float {
        if (value.ends_with("f") || value.ends_with("F")) { end -= 1; }
        if (end == 0) {
            report_bad_number(l, line, col, value);
            return false;
        }

        let dot_seen -> Bool = false;
        let digit_seen -> Bool = false;
        let prev_digit -> Bool = false;
        let i -> Int = 0;
        while (i < end) {
            let ch -> Char = value[i];
            if (is_digit(ch)) {
                digit_seen = true;
                prev_digit = true;
            } else if (ch == '.') {
                if (dot_seen) {
                    report_bad_number(l, line, col, value);
                    return false;
                }
                dot_seen = true;
                prev_digit = false;
            } else if (ch == '_') {
                if (!prev_digit || i + 1 >= end || !is_digit(value[i + 1])) {
                    report_bad_number(l, line, col, value);
                    return false;
                }
                prev_digit = false;
            } else {
                report_bad_number(l, line, col, value);
                return false;
            }
            i += 1;
        }

        if (!dot_seen || !digit_seen) {
            report_bad_number(l, line, col, value);
            return false;
        }
        return true;
    }

    let suffix_len -> Int = 0;
    if (value.ends_with("ULL") || value.ends_with("ull")) {
        suffix_len = 3;
    } else if (value.ends_with("LL") || value.ends_with("ll") ||
               value.ends_with("UL") || value.ends_with("ul")) {
        suffix_len = 2;
    } else if (value.ends_with("U") || value.ends_with("u") ||
               value.ends_with("L") || value.ends_with("l")) {
        suffix_len = 1;
    }

    end -= suffix_len;
    let base -> Int = 10;
    let start -> Int = 0;
    if (end >= 2 && value[0] == '0') {
        let prefix -> Char = value[1];
        if (prefix == 'x' || prefix == 'X') { base = 16; start = 2; }
        else if (prefix == 'b' || prefix == 'B') { base = 2; start = 2; }
        else if (prefix == 'o' || prefix == 'O') { base = 8; start = 2; }
    }

    if (start >= end) {
        report_bad_number(l, line, col, value);
        return false;
    }

    let prev_digit -> Bool = false;
    let i -> Int = start;
    while (i < end) {
        let ch -> Char = value[i];
        if (is_digit_for_base(ch, base)) {
            prev_digit = true;
        } else if (ch == '_') {
            if (!prev_digit || i + 1 >= end || !is_digit_for_base(value[i + 1], base)) {
                report_bad_number(l, line, col, value);
                return false;
            }
            prev_digit = false;
        } else {
            report_bad_number(l, line, col, value);
            return false;
        }
        i += 1;
    }
    return true;
}

func new_lexer(fn -> String, text -> String) -> Lexer {
    let pos -> Position = Position(idx=-1, ln=0, col=-1, text=text, fn=fn);
    let l -> Lexer = Lexer(text=text, pos=pos, current_char='\0');
    lexer_advance(l);
    return l;
}

func lexer_advance(l -> Lexer) -> Void {
    WhitelangExceptions.advance_pos(l.pos, l.current_char);
    if (l.pos.idx < l.text.length()) {
        l.current_char = l.text[l.pos.idx];
    } else {
        l.current_char = '\0';
    }
}

func get_string(l -> Lexer) -> Token {
    let start_ln -> Int = l.pos.ln;
    let start_col -> Int = l.pos.col;
    
    lexer_advance(l); // skip opening "
    
    let result -> String = "";
    
    // " and \
    while (l.current_char != '"' && l.current_char != '\0') {
        if (l.current_char == '\\') { // \
            lexer_advance(l); 
            if (l.current_char == 'n') { // 'n' -> newline
                result = result + "\n";
            } else if (l.current_char == 't') { // 't' -> tab
                result = result + "\t";
            } else if (l.current_char == 'r') { // 'r' -> return
                result = result + "\r";
            } else if (l.current_char == '"') { // '"' -> "
                result = result + "\"";
            } else if (l.current_char == '\\') { // '\' -> \
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
    
    if (l.current_char == '"') {
        lexer_advance(l); // skip closing "
        return WhitelangTokens.Token(type=TOK_STR_LIT, value=result, line=start_ln, col=start_col);
    }
    
    WhitelangExceptions.throw_illegal_char(l.pos, "Unterminated string literal. ");
    return WhitelangTokens.Token(type=TOK_STR_LIT, value=result, line=start_ln, col=start_col);
}

func get_char_literal(l -> Lexer) -> Token {
    let start_ln -> Int = l.pos.ln;
    let start_col -> Int = l.pos.col;
    
    lexer_advance(l); // skip opening '
    let char_val -> Int = 0;
    
    if (l.current_char == '\\') { // '\'
        lexer_advance(l);
        if (l.current_char == 'n') { char_val = 10; } // \n
        else if (l.current_char == 't') { char_val = 9; } // \t
        else if (l.current_char == 'r') { char_val = 13; } // \r
        else if (l.current_char == '0') { char_val = 0; } // \0
        else if (l.current_char == '\\') { char_val = 92; } // \\
        else if (l.current_char == '\'') { char_val = 39; } // \'
        else { char_val = Int(l.current_char); }
        lexer_advance(l);
    } else {
        char_val = Int(l.current_char);
        lexer_advance(l);
    }
    
    if (l.current_char == '\'') {
        lexer_advance(l); // skip closing '
    } else {
        WhitelangExceptions.throw_illegal_char(l.pos, "Unterminated char literal.");
    }

    return WhitelangTokens.Token(type=TOK_CHAR_LIT, value="" + char_val, line=start_ln, col=start_col);
}


func get_number(l -> Lexer) -> Token {
    let start_line -> Int = l.pos.ln;
    let start_col  -> Int = l.pos.col;
    let start_pos  -> Int = l.pos.idx;
    
    let dot_count -> Int = 0;
    while (l.current_char != '\0') {
        if (l.current_char == '.') {
            if (dot_count == 1) { break; }
            dot_count = 1;
            lexer_advance(l);
            continue;
        }
        if (is_digit(l.current_char) || is_alpha(l.current_char)) {
            lexer_advance(l);
        } else {
            break;
        }
    }
    
    let value -> String = l.text.slice(start_pos, l.pos.idx);

    if (value.length() > 0) {
        if (value[0] == '.') {
            value = "0" + value;
        }
    }

    if (dot_count == 1) {
        validate_number(l, start_line, start_col, value, true);
        return WhitelangTokens.Token(type=TOK_FLOAT, value=value, line=start_line, col=start_col);
    }
    validate_number(l, start_line, start_col, value, false);
    return WhitelangTokens.Token(type=TOK_INT, value=value, line=start_line, col=start_col);
}

func get_identifier(l -> Lexer) -> Token {
    let start_line -> Int = l.pos.ln;
    let start_col  -> Int = l.pos.col;
    let start_pos  -> Int = l.pos.idx;

    while (l.current_char != '\0' && (is_alpha(l.current_char) || is_digit(l.current_char))) {
        lexer_advance(l);
    }

    let value -> String = l.text.slice(start_pos, l.pos.idx);

    // Keywords mapping
    if (value == "let") {return WhitelangTokens.Token(type=TOK_LET, value=value, line=start_line, col=start_col);}
    if (value == "Int") {return WhitelangTokens.Token(type=TOK_T_INT, value=value, line=start_line, col=start_col);}
    if (value == "Float") {return WhitelangTokens.Token(type=TOK_T_FLOAT, value=value, line=start_line, col=start_col);}
    if (value == "String") {return WhitelangTokens.Token(type=TOK_T_STRING, value=value, line=start_line, col=start_col);}
    if (value == "Char") {return WhitelangTokens.Token(type=TOK_T_CHAR, value=value, line=start_line, col=start_col);}
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
    if (value == "enum") { return WhitelangTokens.Token(type=TOK_ENUM, value=value, line=start_line, col=start_col); }
    if (value == "interface") { return WhitelangTokens.Token(type=TOK_INTERFACE, value=value, line=start_line, col=start_col); }
    if (value == "with") { return WhitelangTokens.Token(type=TOK_WITH, value=value, line=start_line, col=start_col); }
    if (value == "catch") { return WhitelangTokens.Token(type=TOK_CATCH, value=value, line=start_line, col=start_col); }
    if (value == "throw") { return WhitelangTokens.Token(type=TOK_THROW, value=value, line=start_line, col=start_col); }
    if (value == "in") { return WhitelangTokens.Token(type=TOK_IN, value=value, line=start_line, col=start_col); }

    return WhitelangTokens.Token(type=TOK_IDENTIFIER, value=value, line=start_line, col=start_col);
}


func handle_slash(l -> Lexer) -> Token {
    let line -> Int = l.pos.ln;
    let col  -> Int = l.pos.col;
    lexer_advance(l); // skip first /

    // /=
    if (l.current_char == '=') {
        lexer_advance(l);
        return WhitelangTokens.Token(type=TOK_DIV_ASSIGN, value="/=", line=line, col=col);
    }

    // //
    if (l.current_char == '/') {
        while (l.current_char != '\0' && l.current_char != '\n') { lexer_advance(l); }
        return null;
    }

    // /*  */
    if (l.current_char == '*') {
        lexer_advance(l);
        let comment_closed -> Int = 0;
        while (l.current_char != '\0' && comment_closed == 0) {
            if (l.current_char == '*') {
                lexer_advance(l);
                if (l.current_char == '/') {
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
    while (l.current_char != '\0') {
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

        let char      -> Char = l.current_char;
        let char_line -> Int  = l.pos.ln;
        let char_col  -> Int  = l.pos.col;

        if (char == '"') {
            return get_string(l);
        }
        if (char == '\'') {
            return get_char_literal(l);
        }

        // . and ...
        if (char == '.') {
            let is_ellipsis -> Bool = false;
            if (l.pos.idx + 2 < l.text.length()) {
                let n1 -> Char = l.text[l.pos.idx + 1];
                let n2 -> Char = l.text[l.pos.idx + 2];
                if (n1 == '.' && n2 == '.') { // . .
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
        if (char == '+') { 
            lexer_advance(l);
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_PLUS_ASSIGN, value="+=", line=char_line, col=char_col); } // +=
            if (l.current_char == '+') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_INC, value="++", line=char_line, col=char_col); } // ++
            return WhitelangTokens.Token(type=TOK_PLUS, value="+", line=char_line, col=char_col); 
        }

        // - and -- and -> and -=
        if (char == '-') { 
            lexer_advance(l); 
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_SUB_ASSIGN, value="-=", line=char_line, col=char_col); } // -=
            if (l.current_char == '>') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_TYPE_ARROW, value="->", line=char_line, col=char_col); } // ->
            if (l.current_char == '-') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_DEC, value="--", line=char_line, col=char_col); } // --
            return WhitelangTokens.Token(type=TOK_SUB, value="-", line=char_line, col=char_col); 
        }

        // * *= ** **=
        if (char == '*') { 
            lexer_advance(l); 
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_MUL_ASSIGN, value="*=", line=char_line, col=char_col); } // *=
            if (l.current_char == '*') { 
                lexer_advance(l); 
                if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_POW_ASSIGN, value="**=", line=char_line, col=char_col); } // **=
                return WhitelangTokens.Token(type=TOK_POW, value="**", line=char_line, col=char_col); // **
            }
            return WhitelangTokens.Token(type=TOK_MUL, value="*", line=char_line, col=char_col); 
        }

        // / /= // /*
        if (char == '/') {
            let tok -> Token = handle_slash(l);
            if (tok is null) { continue; }
            return tok;
        }

        if (char == '%') { 
            lexer_advance(l); 
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_MOD_ASSIGN, value="%=", line=char_line, col=char_col); } // %=
            return WhitelangTokens.Token(type=TOK_MOD, value="%", line=char_line, col=char_col); 
        }

        // ! and !=
        if (char == '!') {
            lexer_advance(l);
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_NE, value="!=", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_NOT, value="!", line=char_line, col=char_col);
        }

        // = and ==
        if (char == '=') {
            lexer_advance(l);
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_EE, value="==", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_ASSIGN, value="=", line=char_line, col=char_col);
        }

        // <, <=, <<, <<=
        if (char == '<') {
            lexer_advance(l);
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_LTE, value="<=", line=char_line, col=char_col); }
            if (l.current_char == '<') { // <<
                lexer_advance(l);
                if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_LSHIFT_ASSIGN, value="<<=", line=char_line, col=char_col); }
                return WhitelangTokens.Token(type=TOK_LSHIFT, value="<<", line=char_line, col=char_col);
            }
            return WhitelangTokens.Token(type=TOK_LT, value="<", line=char_line, col=char_col);
        }

        // >, >=, >>, >>=
        if (char == '>') {
            lexer_advance(l);
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_GTE, value=">=", line=char_line, col=char_col); }
            if (l.current_char == '>') { // >>
                lexer_advance(l);
                if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_RSHIFT_ASSIGN, value=">>=", line=char_line, col=char_col); }
                return WhitelangTokens.Token(type=TOK_RSHIFT, value=">>", line=char_line, col=char_col);
            }
            return WhitelangTokens.Token(type=TOK_GT, value=">", line=char_line, col=char_col);
        }

        // &, &&, &=
        if (char == '&') {
            lexer_advance(l);
            if (l.current_char == '&') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_AND, value="&&", line=char_line, col=char_col); }
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_BIT_AND_ASSIGN, value="&=", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_BIT_AND, value="&", line=char_line, col=char_col);
        }

        // |, ||, |=
        if (char == '|') {
            lexer_advance(l);
            if (l.current_char == '|') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_OR, value="||", line=char_line, col=char_col); }
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_BIT_OR_ASSIGN, value="|=", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_BIT_OR, value="|", line=char_line, col=char_col);
        }

        // ^, ^=
        if (char == '^') {
            lexer_advance(l);
            if (l.current_char == '=') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_BIT_XOR_ASSIGN, value="^=", line=char_line, col=char_col); }
            return WhitelangTokens.Token(type=TOK_BIT_XOR, value="^", line=char_line, col=char_col);
        }

        // ~
        if (char == '~') {
            lexer_advance(l);
            return WhitelangTokens.Token(type=TOK_BIT_NOT, value="~", line=char_line, col=char_col);
        }

        // @
        if (char == '@') {
            lexer_advance(l);
            return WhitelangTokens.Token(type=TOK_AT, value="@", line=char_line, col=char_col);
        }

        // Single char tokens
        if (char == '(') { lexer_advance(l);  return WhitelangTokens.Token(type=TOK_LPAREN,   value="(", line=char_line, col=char_col); }
        if (char == ')') { lexer_advance(l);  return WhitelangTokens.Token(type=TOK_RPAREN,   value=")", line=char_line, col=char_col); }
        if (char == ';') { lexer_advance(l);  return WhitelangTokens.Token(type=TOK_SEMICOLON,value=";", line=char_line, col=char_col); }
        if (char == '{') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_LBRACE,   value="{", line=char_line, col=char_col); }
        if (char == '}') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_RBRACE,   value="}", line=char_line, col=char_col); }
        if (char == ',') { lexer_advance(l);  return WhitelangTokens.Token(type=TOK_COMMA,    value=",", line=char_line, col=char_col); }
        if (char == '[') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_LBRACKET, value="[", line=char_line, col=char_col); }
        if (char == ']') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_RBRACKET, value="]", line=char_line, col=char_col); }
        if (char == ':') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_COLON, value=":", line=char_line, col=char_col); }
        if (char == '?') { lexer_advance(l); return WhitelangTokens.Token(type=TOK_QUESTION, value="?", line=char_line, col=char_col); }

        WhitelangExceptions.throw_illegal_char(l.pos, "unknown character '" + char + "'. ");
        lexer_advance(l);
        continue;
    }

    return WhitelangTokens.Token(type=TOK_EOF, value="", line=l.pos.ln, col=l.pos.col);
}
