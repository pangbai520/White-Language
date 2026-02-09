// core/WhitelangLexer.wl
import "builtin"
import "WhitelangTokens.wl"
import "WhitelangExceptions.wl"

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

func get_next_token(l -> Lexer) -> Token {
    while (l.current_char != 0) {
        if (is_space(l.current_char)) {
            advance(l);
            continue;
        }

        if (is_digit(l.current_char)) {
            return get_number(l);
        }

        let char   -> Byte = l.current_char;
        let c_line -> Int = l.pos.ln;
        let c_col  -> Int = l.pos.col;

        // Operator handling
        if (char == 43) { advance(l); return Token(type=TOK_PLUS,   value="+", line=c_line, col=c_col); }
        if (char == 45) { advance(l); return Token(type=TOK_SUB,    value="-", line=c_line, col=c_col); }
        if (char == 42) { advance(l); return Token(type=TOK_MUL,    value="*", line=c_line, col=c_col); }
        if (char == 47) { advance(l); return Token(type=TOK_DIV,    value="/", line=c_line, col=c_col); }
        if (char == 40) { advance(l); return Token(type=TOK_LPAREN, value="(", line=c_line, col=c_col); }
        if (char == 41) { advance(l); return Token(type=TOK_RPAREN, value=")", line=c_line, col=c_col); }

        // Trigger visual error reporting and stop compilation
        throw_illegal_char(l.pos, "unknow character '" + char + "'");
    }

    return Token(type=TOK_EOF, value="", line=l.pos.ln, col=l.pos.col);
}