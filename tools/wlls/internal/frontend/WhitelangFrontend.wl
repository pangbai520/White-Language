// tools/wlls/internal/frontend/WhitelangFrontend.wl
import * from "../../../../src/core/WhitelangNodes.wl"
import Token from "../../../../src/core/WhitelangTokens.wl"
import Lexer, new_lexer, get_next_token from "../../../../src/core/WhitelangLexer.wl"
import Parser, parse from "../../../../src/core/WhitelangParser.wl"
import "../../../../src/core/WhitelangExceptions.wl"

const SYMBOL_FUNCTION  -> Int = 1;
const SYMBOL_VARIABLE  -> Int = 2;
const SYMBOL_CONSTANT  -> Int = 3;
const SYMBOL_STRUCT    -> Int = 4;
const SYMBOL_CLASS     -> Int = 5;
const SYMBOL_METHOD    -> Int = 6;
const SYMBOL_FIELD     -> Int = 7;
const SYMBOL_ENUM      -> Int = 8;
const SYMBOL_ENUM_CASE -> Int = 9;
const SYMBOL_INTERFACE -> Int = 10;
const SYMBOL_ERROR      -> Int = 11;
const SYMBOL_ERROR_CASE -> Int = 12;
const SYMBOL_CONVERSION -> Int = 13;
const SYMBOL_MODULE     -> Int = 14;
const SYMBOL_IMPORT     -> Int = 15;
const SYMBOL_PARAMETER  -> Int = 16;

struct DocumentSymbol(
    name     -> String,
    kind     -> Int,
    span     -> WhitelangExceptions.SourceRange,
    children -> Vector(Struct)
)

struct FrontendDocument(
    path    -> String,
    source  -> String,
    ast     -> Struct,
    symbols -> Vector(Struct)
)

func token_span(path -> String, source -> String, tok -> Token) -> WhitelangExceptions.SourceRange {
    // token columns are UTF-8 byte offsets; protocol consumers use the derived UTF-16 columns
    let width -> Int = 1;
    if (tok.value is !null && tok.value.length() > 0) {
        width = tok.value.length();
    }
    return WhitelangExceptions.source_range(path, source, tok.line, tok.col, width);
}

func make_symbol(path -> String, source -> String, tok -> Token, kind -> Int) -> DocumentSymbol {
    return DocumentSymbol(
        name=tok.value,
        kind=kind,
        span=token_span(path, source, tok),
        children=[]
    );
}

func index_params(
    path -> String,
    source -> String,
    params -> Vector(Struct),
    out -> Vector(Struct)
) -> Void {
    if (params is null) { return; }
    let i -> Int = 0;
    while (i < params.length()) {
        let param -> ParamNode = params[i];
        out.append(make_symbol(path, source, param.name_tok, SYMBOL_PARAMETER));
        i += 1;
    }
}

func index_method(path -> String, source -> String, node -> MethodDefNode) -> DocumentSymbol {
    if (node.name_tok.value == "$init") {
        let span -> WhitelangExceptions.SourceRange =
            WhitelangExceptions.source_range(path, source, node.pos.ln, node.pos.col, 4);
        let symbol -> DocumentSymbol = DocumentSymbol(name="init", kind=SYMBOL_METHOD, span=span, children=[]);
        index_params(path, source, node.params, symbol.children);
        return symbol;
    }
    if (node.name_tok.value == "$deinit") {
        let span -> WhitelangExceptions.SourceRange =
            WhitelangExceptions.source_range(path, source, node.pos.ln, node.pos.col, 6);
        let symbol -> DocumentSymbol = DocumentSymbol(name="deinit", kind=SYMBOL_METHOD, span=span, children=[]);
        index_params(path, source, node.params, symbol.children);
        return symbol;
    }
    if (node.name_tok.value == "$type") {
        let span -> WhitelangExceptions.SourceRange =
            WhitelangExceptions.source_range(path, source, node.pos.ln, node.pos.col, 4);
        return DocumentSymbol(name="type", kind=SYMBOL_CONVERSION, span=span, children=[]);
    }
    let symbol -> DocumentSymbol = make_symbol(path, source, node.name_tok, SYMBOL_METHOD);
    index_params(path, source, node.params, symbol.children);
    return symbol;
}

func index_top_level(path -> String, source -> String, ast -> Struct) -> Vector(Struct) {
    // keep this index syntactic; resolution is built in a separate pass
    let result -> Vector(Struct) = [];
    if (ast is null) { return result; }

    let block -> BlockNode = ast;
    if (block.stmts is null) { return result; }

    let i -> Int = 0;
    while (i < block.stmts.length()) {
        let node -> Struct = block.stmts[i];
        let base -> BaseNode = node;

        if (base.type == NODE_FUNC_DEF) {
            let func_node -> FunctionDefNode = node;
            let symbol -> DocumentSymbol = make_symbol(path, source, func_node.name_tok, SYMBOL_FUNCTION);
            index_params(path, source, func_node.params, symbol.children);
            result.append(symbol);
        } else if (base.type == NODE_EXTERN_FUNC) {
            let extern_node -> ExternFuncNode = node;
            let symbol -> DocumentSymbol = make_symbol(path, source, extern_node.name_tok, SYMBOL_FUNCTION);
            index_params(path, source, extern_node.params, symbol.children);
            result.append(symbol);
        } else if (base.type == NODE_EXTERN_BLOCK) {
            let extern_block -> ExternBlockNode = node;
            if (extern_block.funcs is !null) {
                let j -> Int = 0;
                while (j < extern_block.funcs.length()) {
                    let extern_node -> ExternFuncNode = extern_block.funcs[j];
                    let symbol -> DocumentSymbol = make_symbol(path, source, extern_node.name_tok, SYMBOL_FUNCTION);
                    index_params(path, source, extern_node.params, symbol.children);
                    result.append(symbol);
                    j += 1;
                }
            }
        } else if (base.type == NODE_VAR_DECL) {
            let var_node -> VarDeclareNode = node;
            let kind -> Int = SYMBOL_VARIABLE;
            if (var_node.is_const) { kind = SYMBOL_CONSTANT; }
            result.append(make_symbol(path, source, var_node.name_tok, kind));
        } else if (base.type == NODE_STRUCT_DEF) {
            let struct_node -> StructDefNode = node;
            let symbol -> DocumentSymbol = make_symbol(path, source, struct_node.name_tok, SYMBOL_STRUCT);
            if (struct_node.fields is !null) {
                let j -> Int = 0;
                while (j < struct_node.fields.length()) {
                    let field -> ParamNode = struct_node.fields[j];
                    symbol.children.append(make_symbol(path, source, field.name_tok, SYMBOL_FIELD));
                    j += 1;
                }
            }
            result.append(symbol);
        } else if (base.type == NODE_CLASS_DEF) {
            let class_node -> ClassDefNode = node;
            let symbol -> DocumentSymbol = make_symbol(path, source, class_node.name_tok, SYMBOL_CLASS);
            if (class_node.fields is !null) {
                let j -> Int = 0;
                while (j < class_node.fields.length()) {
                    let field -> VarDeclareNode = class_node.fields[j];
                    symbol.children.append(make_symbol(path, source, field.name_tok, SYMBOL_FIELD));
                    j += 1;
                }
            }
            if (class_node.methods is !null) {
                let j -> Int = 0;
                while (j < class_node.methods.length()) {
                    symbol.children.append(index_method(path, source, class_node.methods[j]));
                    j += 1;
                }
            }
            result.append(symbol);
        } else if (base.type == NODE_ENUM_DEF) {
            let enum_node -> EnumDefNode = node;
            let enum_kind -> Int = SYMBOL_ENUM;
            let field_kind -> Int = SYMBOL_ENUM_CASE;
            if (enum_node.is_error) {
                enum_kind = SYMBOL_ERROR;
                field_kind = SYMBOL_ERROR_CASE;
            }
            let symbol -> DocumentSymbol =
                make_symbol(path, source, enum_node.name_tok, enum_kind);
            if (enum_node.fields is !null) {
                let j -> Int = 0;
                while (j < enum_node.fields.length()) {
                    let enum_field -> EnumFieldNode = enum_node.fields[j];
                    symbol.children.append(
                        make_symbol(path, source, enum_field.name_tok, field_kind)
                    );
                    j += 1;
                }
            }
            result.append(symbol);
        } else if (base.type == NODE_INTERFACE_DEF) {
            let interface_node -> InterfaceDefNode = node;
            let symbol -> DocumentSymbol =
                make_symbol(path, source, interface_node.name_tok, SYMBOL_INTERFACE);
            if (interface_node.methods is !null) {
                let j -> Int = 0;
                while (j < interface_node.methods.length()) {
                    symbol.children.append(index_method(path, source, interface_node.methods[j]));
                    j += 1;
                }
            }
            result.append(symbol);
        }
        i += 1;
    }
    return result;
}

func parse_document(path -> String, source -> String) -> FrontendDocument {
    // use the compiler parser so tooling and builds always accept the same language
    let lexer -> Lexer = new_lexer(path, source);
    let parser -> Parser = Parser(lexer=lexer, current_tok=get_next_token(lexer), nesting=0);
    let ast -> Struct = parse(parser);
    return FrontendDocument(
        path=path,
        source=source,
        ast=ast,
        symbols=index_top_level(path, source, ast)
    );
}
