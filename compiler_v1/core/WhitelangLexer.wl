// core/WhitelangLexer.wl
import "builtin"
import "WhitelangTokens.wl"
import "WhitelangExceptions.wl"

extern func strcmp(s1 -> String, s2 -> String) -> Int from "C";

struct Lexer(
    text -> String,
    pos  -> Position, // Using Position object for tracking
    current_char -> Byte
)

func is_space(c -> Byte) -> Bool {
    let b -> Int = c;
    return (b == 32) || (b == 9) || (b == 10) || (b == 13); // space, backspace, LF/NL, return
}

func is_digit(c -> Byte) -> Bool {
    let b -> Int = c;
    return (b >= 48) && (b <= 57); // 0~9
}

func is_operator_char(c -> Byte) -> Bool {
    let b -> Int = c;
    return b == 33 || b == 38 || b == 124 || b == 60 || b == 62 || b == 61; // !, &, |, <, >, =
}

func is_alpha(c -> Byte) -> Bool {
    // [TODO]: Future support for Chinese variable names
    let b -> Int = c;
    return (b >= 97 && b <= 122) || (b >= 65 && b <= 90) || (b == 95); // a~z, A~Z, _
}

func new_lexer(text -> String) -> Lexer {
    // Initialize Position at start
    let p -> Position = Position(idx=-1, ln=0, col=-1, text=text);
    let l -> Lexer = Lexer(text=text, pos=p, current_char=0);
    advance(l);
    return l;
}

func advance(l -> Lexer) -> Void {
    // Delegate coordinate updates to Position
    advance_pos(l.pos, l.current_char);

    if (l.pos.idx < l.text.length) {
        l.current_char = l.text[l.pos.idx];
    } else {
        l.current_char = 0;
    }
}

func get_number(l -> Lexer) -> Token {
    let start_line -> Int = l.pos.ln;
    let start_col  -> Int = l.pos.col;
    let start_pos  -> Int = l.pos.idx;
    
    let dot_count -> Int = 0;
    // Support floating point numbers
    while (l.current_char != 0 && (is_digit(l.current_char) || l.current_char == 46)) {
        if (l.current_char == 46) {
            if (dot_count == 1) {
                break;
            }
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

    if (value == "let") {
        return Token(type=TOK_LET, value=value, line=start_line, col=start_col);
    }
    if (value == "Int") {
        return Token(type=TOK_T_INT, value=value, line=start_line, col=start_col);
    }
    if (value == "Float") {
        return Token(type=TOK_T_FLOAT, value=value, line=start_line, col=start_col);
    }
    if (value == "String") {
        return Token(type=TOK_T_STRING, value=value, line=start_line, col=start_col);
    }
    if (value == "Bool") {
        return Token(type=TOK_T_BOOL, value=value, line=start_line, col=start_col);
    }
    if (value == "Void") {
        return Token(type=TOK_T_VOID, value=value, line=start_line, col=start_col);
    }
    if (value == "true") {
        return Token(type=TOK_TRUE, value=value, line=start_line, col=start_col);
    }
    if (value == "false") {
        return Token(type=TOK_FALSE, value=value, line=start_line, col=start_col);
    }
    
    return Token(type=TOK_IDENTIFIER, value=value, line=start_line, col=start_col);
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

        let char   -> Byte = l.current_char;
        let char_line -> Int = l.pos.ln;
        let char_col  -> Int = l.pos.col;

        // + and x++
        if (char == 43) { 
            advance(l);
            if (l.current_char == 43) {
                advance(l);
                return Token(type=TOK_INC, value="++", line=char_line, col=char_col);
            }
            return Token(type=TOK_PLUS, value="+", line=char_line, col=char_col); 
        }

        // -, x-- and ->
        if (char == 45) { 
            advance(l); 
            if (l.current_char == 62) {
                advance(l);
                return Token(type=TOK_TYPE_ARROW, value="->", line=char_line, col=char_col);
            }
            if (l.current_char == 45) {
                advance(l);
                return Token(type=TOK_DEC, value="--", line=char_line, col=char_col);
            }
            return Token(type=TOK_SUB, value="-", line=char_line, col=char_col); 
        }

        // * and x ** y
        if (char == 42) { 
            advance(l); 
            if (l.current_char == 42) {
                advance(l);
                return Token(type=TOK_POW, value="**", line=char_line, col=char_col);
            }
            return Token(type=TOK_MUL, value="*", line=char_line, col=char_col); 
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
        //  &&
        if (char == 38) {
            advance(l);
            if (l.current_char == 38) { advance(l); return Token(type=TOK_AND, value="&&", line=char_line, col=char_col); }
            throw_illegal_char(l.pos, "Expected '&' after '&'.");
        }
        //  ||
        if (char == 124) {
            advance(l);
            if (l.current_char == 124) { advance(l); return Token(type=TOK_OR, value="||", line=char_line, col=char_col); }
            throw_illegal_char(l.pos, "Expected '|' after '|'.");
        }

        if (char == 47) { advance(l); return Token(type=TOK_DIV,       value="/", line=char_line, col=char_col); }
        if (char == 37) { advance(l); return Token(type=TOK_MOD,       value="%", line=char_line, col=char_col); }
        if (char == 40) { advance(l); return Token(type=TOK_LPAREN,    value="(", line=char_line, col=char_col); }
        if (char == 41) { advance(l); return Token(type=TOK_RPAREN,    value=")", line=char_line, col=char_col); }
        if (char == 59) { advance(l); return Token(type=TOK_SEMICOLON, value=";", line=char_line, col=char_col); }

        // Trigger visual error reporting and stop compilation
        throw_illegal_char(l.pos, "unknown character '" + char + "'. ");
    }

    return Token(type=TOK_EOF, value="", line=l.pos.ln, col=l.pos.col);
}