// core/WhitelangTokens.wl

const TOK_EOF   -> Int = 0; // end of file
const TOK_INT   -> Int = 1; // Integer
const TOK_FLOAT -> Int = 2; // Decimals
const TOK_PLUS  -> Int = 3; // +
const TOK_SUB   -> Int = 4; // -
const TOK_MUL   -> Int = 5; // *
const TOK_DIV   -> Int = 6; // /
const TOK_LPAREN-> Int = 7; // (
const TOK_RPAREN-> Int = 8; // )


func get_token_name(type -> Int) -> String {
    if (type == TOK_INT) { return "INT"; }
    if (type == TOK_FLOAT) { return "FLOAT"; }
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