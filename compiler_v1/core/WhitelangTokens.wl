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
const TOK_MOD           -> Int = 14; // %
const TOK_POW           -> Int = 15; // **
const TOK_INC           -> Int = 16; // x++
const TOK_DEC           -> Int = 17; // x--

const TOK_T_INT         -> Int = 18;
const TOK_T_FLOAT       -> Int = 19;
const TOK_T_STRING      -> Int = 20;
const TOK_T_BOOL        -> Int = 21;
const TOK_T_VOID        -> Int = 22;

const TOK_EE            -> Int = 23; // ==
const TOK_NE            -> Int = 24; // !=
const TOK_GT            -> Int = 25; // >
const TOK_LT            -> Int = 26; // <
const TOK_GTE           -> Int = 27; // >=
const TOK_LTE           -> Int = 28; // <=
const TOK_AND           -> Int = 29; // &&
const TOK_OR            -> Int = 30; // ||
const TOK_NOT           -> Int = 31; // !
const TOK_TRUE          -> Int = 32;
const TOK_FALSE         -> Int = 33;

const TOK_IF            -> Int = 34; // if
const TOK_ELSE          -> Int = 35; // else
const TOK_LBRACE        -> Int = 36; // {
const TOK_RBRACE        -> Int = 37; // }

const TOK_WHILE         -> Int = 38; // while
const TOK_BREAK         -> Int = 39; // break
const TOK_CONTINUE      -> Int = 40; // continue    but why am i writing so many useless comments O.O
const TOK_FOR           -> Int = 41;

const TOK_COMMA         -> Int = 42;
const TOK_FUNC          -> Int = 43; // func
const TOK_RETURN        -> Int = 44; // return
// These things are fucking insane, absolutely have to make enum [TODO]
const TOK_STR_LIT       -> Int = 45;

const TOK_PLUS_ASSIGN   -> Int = 46; // +=
const TOK_SUB_ASSIGN    -> Int = 47; // -=
const TOK_MUL_ASSIGN    -> Int = 48; // *=
const TOK_DIV_ASSIGN    -> Int = 49; // /=
const TOK_MOD_ASSIGN    -> Int = 50; // %=
const TOK_POW_ASSIGN    -> Int = 51; // **=

const TOK_STRUCT        -> Int = 52;
const TOK_THIS          -> Int = 53; // this
const TOK_DOT           -> Int = 53; // .

const TOK_PTR           -> Int = 55; // ptr
const TOK_REF           -> Int = 56; // ref
const TOK_DEREF         -> Int = 57; // deref

const TOK_NULLPTR       -> Int = 58;
const TOK_NULL          -> Int = 59;
const TOK_IS            -> Int = 60;

const TOK_EXTERN        -> Int = 61; // extern
const TOK_ELLIPSIS      -> Int = 62; // ...
const TOK_FROM          -> Int = 63;
const TOK_LBRACKET      -> Int = 64; // [
const TOK_RBRACKET      -> Int = 65; // ]


func get_token_name(type -> Int) -> String {
    if (type == TOK_INT) { return "INT"; }
    if (type == TOK_FLOAT) { return "FLOAT"; }
    if (type == TOK_PLUS)   { return "+"; }
    if (type == TOK_SUB)    { return "-"; }
    if (type == TOK_MUL)    { return "*"; }
    if (type == TOK_DIV)    { return "/"; }

    if (type == TOK_EE)     { return "=="; }
    if (type == TOK_NE)     { return "!="; }
    if (type == TOK_GT)     { return ">"; }
    if (type == TOK_LT)     { return "<"; }
    if (type == TOK_GTE)    { return ">="; }
    if (type == TOK_LTE)    { return "<="; }
    if (type == TOK_AND)    { return "&&"; }
    if (type == TOK_OR)     { return "||"; }
    if (type == TOK_NOT)    { return "!"; }
    if (type == TOK_TRUE)   { return "true"; }
    if (type == TOK_FALSE)  { return "false"; }

    if (type == TOK_LPAREN) { return "("; }
    if (type == TOK_RPAREN) { return ")"; }
    if (type == TOK_LET)    { return "let"; }
    if (type == TOK_TYPE_ARROW) { return "->"; }
    if (type == TOK_ASSIGN) { return "="; }
    if (type == TOK_IDENTIFIER) { return "IDENTIFIER"; }
    if (type == TOK_SEMICOLON)  { return ";"; }

    if (type == TOK_IF)     { return "if"; }
    if (type == TOK_ELSE)   { return "else"; }
    if (type == TOK_LBRACE) { return "{"; }
    if (type == TOK_RBRACE) { return "}"; }

    if (type == TOK_WHILE) { return "while"; }
    if (type == TOK_BREAK) { return "break"; }
    if (type == TOK_CONTINUE) { return "continue"; }

    if (type == TOK_FOR)      { return "for"; }

    if (type == TOK_COMMA)    { return ","; }
    if (type == TOK_FUNC)       { return "func"; }
    if (type == TOK_RETURN)     { return "return"; }
    if (type == TOK_STR_LIT)    { return "STRING_LITERAL"; }

    if (type == TOK_PLUS_ASSIGN) { return "+="; }
    if (type == TOK_SUB_ASSIGN)  { return "-="; }
    if (type == TOK_MUL_ASSIGN)  { return "*="; }
    if (type == TOK_DIV_ASSIGN)  { return "/="; }
    if (type == TOK_MOD_ASSIGN)  { return "%="; }
    if (type == TOK_POW_ASSIGN)  { return "**="; }

    if (type == TOK_STRUCT)  { return "struct"; }
    if (type == TOK_THIS)  { return "this"; }
    if (type == TOK_DOT)  { return "."; }

    if (type == TOK_PTR)  { return "ptr"; }
    if (type == TOK_REF)  { return "ref"; }
    if (type == TOK_DEREF)  { return "deref"; }

    if (type == TOK_NULLPTR)  { return "nullptr"; }
    if (type == TOK_NULL)     { return "null"; }
    if (type == TOK_IS)  { return "is"; }
    if (type == TOK_EXTERN)  { return "extern"; }
    if (type == TOK_ELLIPSIS)  { return "..."; }
    if (type == TOK_FROM)  { return "from"; }
    if (type == TOK_LBRACKET)  { return "["; }
    if (type == TOK_RBRACKET)  { return "]"; }

    return "EOF";
}

struct Token(
    type  -> Int,
    value -> String,
    line  -> Int,
    col   -> Int
)