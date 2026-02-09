// core/WhitelangTokens.wl

const TOK_EOF   -> Int = 0; // end of file
const TOK_NUMBER-> Int = 1; // number
const TOK_PLUS  -> Int = 2; // +
const TOK_SUB   -> Int = 3; // -
const TOK_MUL   -> Int = 4; // *
const TOK_DIV   -> Int = 5; // /
const TOK_LPAREN-> Int = 6; // (
const TOK_RPAREN-> Int = 7; // )


func get_token_name(type -> Int) -> String {
    if (type == TOK_NUMBER) { return "NUMBER"; }
    if (type == TOK_PLUS)   { return "+"; }
    if (type == TOK_SUB)    { return "-"; }
    if (type == TOK_MUL)    { return "*"; }
    if (type == TOK_DIV)    { return "/"; }
    if (type == TOK_LPAREN) { return "("; }
    if (type == TOK_RPAREN) { return ")"; }
    return "EOF";
}

struct Token(
    type  -> Int,
    value -> String,
    line  -> Int,
    col   -> Int
)