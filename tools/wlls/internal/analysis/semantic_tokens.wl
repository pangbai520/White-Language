// semantic token collection and protocol encoding
import Dict from "dict"
import "json"
import "../frontend/_pkg.wl" as source
import "../compiler/_pkg.wl" as compiler

class SemanticToken {
    let line -> Int = 0;
    let character -> Int = 0;
    let length -> Int = 0;
    let token_type -> String = "";
    let modifiers -> Vector(String) = null;

    init(
        line -> Int,
        character -> Int,
        length -> Int,
        token_type -> String,
        modifiers -> Vector(String)
    ) {
        self.line = line;
        self.character = character;
        self.length = length;
        self.token_type = token_type;
        self.modifiers = modifiers;
    }
}

class __TokenBinding {
    let kind -> Int = 0;
    let readonly -> Bool = false;
    let top_level -> Bool = false;
    let declaration -> Bool = false;

    init(kind -> Int, readonly -> Bool, top_level -> Bool, declaration -> Bool) {
        self.kind = kind;
        self.readonly = readonly;
        self.top_level = top_level;
        self.declaration = declaration;
    }
}

func __token_key(line -> Int, character -> Int) -> String {
    return line + ":" + character;
}

func __index_document_symbols(
    symbols -> Vector(Struct),
    bindings -> Dict,
    top_level -> Bool
) -> Void {
    let i -> Int = 0;
    while (i < symbols.length()) {
        let symbol -> source.DocumentSymbol = symbols[i];
        let position -> compiler.WhitelangExceptions.SourcePosition =
            symbol.span.start;
        bindings.put(
            __token_key(position.line, position.utf16_column),
            __TokenBinding(
                symbol.kind,
                symbol.kind == source.SYMBOL_CONSTANT,
                top_level,
                true
            )
        );
        __index_document_symbols(symbol.children, bindings, false);
        i += 1;
    }
}

func __binding_from_definition(
    definition -> source.SymbolDefinition,
    declaration -> Bool
) -> __TokenBinding {
    if (definition is null) { return null; }
    return __TokenBinding(
        definition.kind,
        definition.kind == source.SYMBOL_CONSTANT,
        definition.top_level,
        declaration
    );
}

func __build_bindings(
    result -> source.FrontendResult,
    workspace -> source.FrontendWorkspace,
    path -> String
) -> Dict {
    let bindings -> Dict = Dict(64);
    __index_document_symbols(result.syntax.symbols, bindings, true);

    let i -> Int = 0;
    while (i < result.semantics.definitions.length()) {
        let definition -> source.SymbolDefinition =
            result.semantics.definitions[i];
        bindings.put(
            __token_key(
                definition.range.start.line,
                definition.range.start.utf16_column
            ),
            __binding_from_definition(definition, true)
        );
        i += 1;
    }

    i = 0;
    while (i < result.semantics.references.length()) {
        let reference -> source.SymbolReference =
            result.semantics.references[i];
        let definition -> source.SymbolDefinition = reference.definition;
        if (definition is null ||
            definition.kind == source.SYMBOL_IMPORT ||
            definition.kind == source.SYMBOL_MODULE) {
            definition = workspace.resolve_reference(path, reference);
        }
        if (definition is !null) {
            bindings.put(
                __token_key(
                    reference.range.start.line,
                    reference.range.start.utf16_column
                ),
                __binding_from_definition(definition, false)
            );
        }
        i += 1;
    }
    return bindings;
}

func __semantic_type(kind -> Int) -> String {
    if (kind == source.SYMBOL_FUNCTION) { return "function"; }
    if (kind == source.SYMBOL_METHOD ||
        kind == source.SYMBOL_CONVERSION) {
        return "method";
    }
    if (kind == source.SYMBOL_PARAMETER) { return "parameter"; }
    if (kind == source.SYMBOL_FIELD) { return "property"; }
    if (kind == source.SYMBOL_CLASS) { return "class"; }
    if (kind == source.SYMBOL_STRUCT) { return "struct"; }
    if (kind == source.SYMBOL_INTERFACE) { return "interface"; }
    if (kind == source.SYMBOL_ENUM ||
        kind == source.SYMBOL_ERROR) {
        return "enum";
    }
    if (kind == source.SYMBOL_ENUM_CASE ||
        kind == source.SYMBOL_ERROR_CASE) {
        return "enumMember";
    }
    if (kind == source.SYMBOL_MODULE) { return "variable"; }
    if (kind == source.SYMBOL_IMPORT) { return "variable"; }
    return "variable";
}

func __binding_modifiers(binding -> __TokenBinding) -> Vector(String) {
    let modifiers -> Vector(String) = [];
    if (binding is null) { return modifiers; }
    if (binding.declaration) { modifiers.append("declaration"); }
    if (binding.readonly) { modifiers.append("readonly"); }
    return modifiers;
}

func __is_builtin_type(token_type -> Int) -> Bool {
    return token_type == compiler.WhitelangTokens.TOK_T_INT ||
           token_type == compiler.WhitelangTokens.TOK_T_FLOAT ||
           token_type == compiler.WhitelangTokens.TOK_T_STRING ||
           token_type == compiler.WhitelangTokens.TOK_T_BOOL ||
           token_type == compiler.WhitelangTokens.TOK_T_CHAR ||
           token_type == compiler.WhitelangTokens.TOK_T_VOID;
}

func __is_operator(token_type -> Int) -> Bool {
    return token_type == compiler.WhitelangTokens.TOK_PLUS ||
           token_type == compiler.WhitelangTokens.TOK_SUB ||
           token_type == compiler.WhitelangTokens.TOK_MUL ||
           token_type == compiler.WhitelangTokens.TOK_DIV ||
           token_type == compiler.WhitelangTokens.TOK_MOD ||
           token_type == compiler.WhitelangTokens.TOK_POW ||
           token_type == compiler.WhitelangTokens.TOK_INC ||
           token_type == compiler.WhitelangTokens.TOK_DEC ||
           token_type == compiler.WhitelangTokens.TOK_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_TYPE_ARROW ||
           token_type == compiler.WhitelangTokens.TOK_EE ||
           token_type == compiler.WhitelangTokens.TOK_NE ||
           token_type == compiler.WhitelangTokens.TOK_GT ||
           token_type == compiler.WhitelangTokens.TOK_LT ||
           token_type == compiler.WhitelangTokens.TOK_GTE ||
           token_type == compiler.WhitelangTokens.TOK_LTE ||
           token_type == compiler.WhitelangTokens.TOK_AND ||
           token_type == compiler.WhitelangTokens.TOK_OR ||
           token_type == compiler.WhitelangTokens.TOK_NOT ||
           token_type == compiler.WhitelangTokens.TOK_PLUS_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_SUB_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_MUL_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_DIV_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_MOD_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_POW_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_BIT_AND ||
           token_type == compiler.WhitelangTokens.TOK_BIT_OR ||
           token_type == compiler.WhitelangTokens.TOK_BIT_XOR ||
           token_type == compiler.WhitelangTokens.TOK_BIT_NOT ||
           token_type == compiler.WhitelangTokens.TOK_LSHIFT ||
           token_type == compiler.WhitelangTokens.TOK_RSHIFT ||
           token_type == compiler.WhitelangTokens.TOK_BIT_AND_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_BIT_OR_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_BIT_XOR_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_LSHIFT_ASSIGN ||
           token_type == compiler.WhitelangTokens.TOK_RSHIFT_ASSIGN;
}

func __is_keyword(token_type -> Int) -> Bool {
    if (token_type == compiler.WhitelangTokens.TOK_IDENTIFIER ||
        token_type == compiler.WhitelangTokens.TOK_INT ||
        token_type == compiler.WhitelangTokens.TOK_FLOAT ||
        token_type == compiler.WhitelangTokens.TOK_STR_LIT ||
        token_type == compiler.WhitelangTokens.TOK_CHAR_LIT ||
        token_type == compiler.WhitelangTokens.TOK_EOF ||
        token_type == compiler.WhitelangTokens.TOK_AT ||
        __is_builtin_type(token_type) ||
        __is_operator(token_type)) {
        return false;
    }

    return token_type != compiler.WhitelangTokens.TOK_LPAREN &&
           token_type != compiler.WhitelangTokens.TOK_RPAREN &&
           token_type != compiler.WhitelangTokens.TOK_SEMICOLON &&
           token_type != compiler.WhitelangTokens.TOK_LBRACE &&
           token_type != compiler.WhitelangTokens.TOK_RBRACE &&
           token_type != compiler.WhitelangTokens.TOK_COMMA &&
           token_type != compiler.WhitelangTokens.TOK_DOT &&
           token_type != compiler.WhitelangTokens.TOK_ELLIPSIS &&
           token_type != compiler.WhitelangTokens.TOK_LBRACKET &&
           token_type != compiler.WhitelangTokens.TOK_RBRACKET &&
           token_type != compiler.WhitelangTokens.TOK_COLON &&
           token_type != compiler.WhitelangTokens.TOK_QUESTION;
}

func __append_span(
    tokens -> Vector(Struct),
    source_map -> source.SourceMap,
    start_line -> Int,
    start_col -> Int,
    end_line -> Int,
    end_col -> Int,
    token_type -> String,
    modifiers -> Vector(String)
) -> Void {
    let line -> Int = start_line;
    while (line <= end_line) {
        let line_start -> Int = source_map.line_start(line);
        let byte_start -> Int = 0;
        if (line == start_line) { byte_start = start_col; }

        let byte_end -> Int =
            source_map.line_end(line) - line_start;
        if (line == end_line) { byte_end = end_col; }

        if (byte_end > byte_start) {
            let start -> compiler.WhitelangExceptions.SourcePosition =
                source_map.position(line, byte_start);
            let end -> compiler.WhitelangExceptions.SourcePosition =
                source_map.position(line, byte_end);
            let length -> Int = end.utf16_column - start.utf16_column;
            if (length > 0) {
                tokens.append(SemanticToken(
                    line,
                    start.utf16_column,
                    length,
                    token_type,
                    modifiers
                ));
            }
        }
        line += 1;
    }
}

func __append_trivia(
    tokens -> Vector(Struct),
    source_map -> source.SourceMap,
    trivia -> compiler.WhitelangLexer.LexerTrivia
) -> Void {
    __append_span(
        tokens,
        source_map,
        trivia.start_line,
        trivia.start_col,
        trivia.end_line,
        trivia.end_col,
        "comment",
        []
    );
}

func semantic_tokens(
    result -> source.FrontendResult,
    workspace -> source.FrontendWorkspace,
    path -> String
) -> Vector(Struct) {
    // lexical tokens come from wlc; the semantic index only refines identifiers
    let tokens -> Vector(Struct) = [];
    if (result is null || !result.valid || result.semantics is null) {
        return tokens;
    }

    let bindings -> Dict = __build_bindings(result, workspace, path);
    let source_map -> source.SourceMap = result.syntax.source_map;
    let lexer -> compiler.WhitelangLexer.Lexer =
        compiler.WhitelangLexer.new_lexer_trivia(
            result.syntax.path,
            result.syntax.source
        );
    let trivia_index -> Int = 0;
    let annotation_name -> Bool = false;

    while true {
        let token -> compiler.WhitelangTokens.Token =
            compiler.WhitelangLexer.get_next_token(lexer);

        while (trivia_index < lexer.trivia.length()) {
            __append_trivia(tokens, source_map, lexer.trivia[trivia_index]);
            trivia_index += 1;
        }

        if (token.type == compiler.WhitelangTokens.TOK_EOF) { break; }

        let start -> compiler.WhitelangExceptions.SourcePosition =
            source_map.position(token.line, token.col);
        let token_type -> String = "";
        let modifiers -> Vector(String) = [];

        if (token.type == compiler.WhitelangTokens.TOK_AT) {
            token_type = "annotation";
            annotation_name = true;
        } else if (annotation_name) {
            token_type = "annotation";
            annotation_name = false;
        } else if (__is_builtin_type(token.type)) {
            token_type = "type";
            modifiers.append("defaultLibrary");
        } else if (token.type == compiler.WhitelangTokens.TOK_STR_LIT ||
                   token.type == compiler.WhitelangTokens.TOK_CHAR_LIT) {
            token_type = "string";
        } else if (token.type == compiler.WhitelangTokens.TOK_INT ||
                   token.type == compiler.WhitelangTokens.TOK_FLOAT) {
            token_type = "number";
        } else if (__is_operator(token.type)) {
            token_type = "operator";
        } else if (token.type == compiler.WhitelangTokens.TOK_IDENTIFIER ||
                   token.type == compiler.WhitelangTokens.TOK_TYPE) {
            let binding -> __TokenBinding =
                bindings[__token_key(token.line, start.utf16_column)];
            if (binding is !null) {
                token_type = __semantic_type(binding.kind);
                modifiers = __binding_modifiers(binding);
            } else if (token.type == compiler.WhitelangTokens.TOK_TYPE) {
                token_type = "keyword";
            } else {
                token_type = "variable";
            }
        } else if (__is_keyword(token.type)) {
            token_type = "keyword";
        }

        if (token_type.length() > 0) {
            __append_span(
                tokens,
                source_map,
                token.line,
                token.col,
                lexer.pos.ln,
                lexer.pos.col,
                token_type,
                modifiers
            );
        }
    }
    return tokens;
}

func __json_modifiers(modifiers -> Vector(String)) -> json.Value? {
    let result -> json.Value = json.array();
    let i -> Int = 0;
    while (i < modifiers.length()) {
        result.append(json.string(modifiers[i])?)?;
        i += 1;
    }
    return result;
}

func __encode_semantic_tokens(tokens -> Vector(Struct)) -> String? {
    let result -> json.Value = json.array();
    let i -> Int = 0;
    while (i < tokens.length()) {
        let token -> SemanticToken = tokens[i];
        let encoded -> json.Value = json.object();
        encoded.set("line", json.integer(Long(token.line))?)?;
        encoded.set("character", json.integer(Long(token.character))?)?;
        encoded.set("length", json.integer(Long(token.length))?)?;
        encoded.set("type", json.string(token.token_type)?)?;
        encoded.set("modifiers", __json_modifiers(token.modifiers)?)?;
        result.append(encoded)?;
        i += 1;
    }
    return json.encode(result)?;
}

func encode_semantic_tokens(tokens -> Vector(Struct)) -> String {
    let encoded -> String = __encode_semantic_tokens(tokens)?;
    catch(err) {
        return "[]";
    }
    return encoded;
}
