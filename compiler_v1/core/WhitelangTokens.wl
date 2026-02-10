// core/WhitelangTokens.wl

const TOK_EOF           -> Int = 0;  // end of file
const TOK_INT           -> Int = 1;  // Integer
const TOK_FLOAT         -> Int = 2;  // Decimals
const TOK_PLUS          -> Int = 3;  // +
const TOK_SUB           -> Int = 4;  // -
const TOK_MUL           -> Int = 5;  // *
const TOK_DIV           -> Int = 6;  // /
const TOK_LPAREN        -> Int = 7;  // (
const TOK_RPAREN        -> Int = 8;  // )
const TOK_LET           -> Int = 9;  // let
const TOK_IDENTIFIER    -> Int = 10; // variable name
const TOK_ASSIGN        -> Int = 11; // =
const TOK_TYPE_ARROW    -> Int = 12; // ->
const TOK_SEMICOLON     -> Int = 13; // ;

const TOK_T_INT         -> Int = 14; // "Int"
const TOK_T_FLOAT       -> Int = 15; // "Float"
const TOK_T_STRING      -> Int = 16; // "String"
const TOK_T_BOOL        -> Int = 17; // "Bool"
const TOK_T_VOID        -> Int = 18; // "Void"


func get_token_name(type -> Int) -> String {
    if (type == TOK_INT) { return "INT"; }
    if (type == TOK_FLOAT) { return "FLOAT"; }
    if (type == TOK_PLUS)   { return "+"; }
    if (type == TOK_SUB)    { return "-"; }
    if (type == TOK_MUL)    { return "*"; }
    if (type == TOK_DIV)    { return "/"; }
    if (type == TOK_LPAREN) { return "("; }
    if (type == TOK_RPAREN) { return ")"; }
    if (type == TOK_LET)    { return "let"; }
    if (type == TOK_TYPE_ARROW) { return "->"; }
    if (type == TOK_ASSIGN) { return "="; }
    if (type == TOK_IDENTIFIER) { return "IDENTIFIER"; }
    if (type == TOK_SEMICOLON)  { return ";"; }
    return "EOF";
}

struct Token(
    type  -> Int,
    value -> String,
    line  -> Int,
    col   -> Int
)