// core/WhitelangLexer.wl
import "builtin"
import "WhitelangTokens.wl"

struct Lexer(
    text -> String,
    pos  -> Int,
    line -> Int,
    col  -> Int,
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


func new_lexer(text -> String) -> Lexer {
    let l -> Lexer = Lexer(text=text, pos=-1, line=1, col=0, current_char=0);
    advance(l);
    return l;
}


func advance(l -> Lexer) -> Void {
    if (l.current_char == 10) { // '\n'
        l.line = l.line + 1;
        l.col = 0;
    } else {
        l.col = l.col + 1;
    }

    l.pos = l.pos + 1;
    if (l.pos < l.text.length) {
        l.current_char = l.text[l.pos];
    } else {
        l.current_char = 0; // None
    }
}


func get_number(l -> Lexer) -> Token {
    let start_line -> Int = l.line;
    let start_col  -> Int = l.col;
    let start_pos  -> Int = l.pos;
    
    while (l.current_char != 0 && is_digit(l.current_char)) {
        advance(l);
    }
    
    let value -> String = l.text.slice(start_pos, l.pos);
    return Token(type=TOK_NUMBER, value=value, line=start_line, col=start_col);
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

        // Record the current position for token creation
        let c_line -> Int = l.line;
        let c_col  -> Int = l.col;
        let char   -> Byte = l.current_char;

        if (char == 43) { advance(l); return Token(type=TOK_PLUS,   value="+", line=c_line, col=c_col); }
        if (char == 45) { advance(l); return Token(type=TOK_SUB,    value="-", line=c_line, col=c_col); }
        if (char == 42) { advance(l); return Token(type=TOK_MUL,    value="*", line=c_line, col=c_col); }
        if (char == 47) { advance(l); return Token(type=TOK_DIV,    value="/", line=c_line, col=c_col); }
        if (char == 40) { advance(l); return Token(type=TOK_LPAREN, value="(", line=c_line, col=c_col); }
        if (char == 41) { advance(l); return Token(type=TOK_RPAREN, value=")", line=c_line, col=c_col); }

        builtin.print("Illegal Character: "); // Fake exception
        builtin.print(char);
        advance(l);
    }

    return Token(type=TOK_EOF, value="", line=l.line, col=l.col);
}