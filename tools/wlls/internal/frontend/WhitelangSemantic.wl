import Dict from "dict"
import * from "WhitelangFrontend.wl"
import * from "../../../../src/core/WhitelangNodes.wl"
import "../../../../src/core/WhitelangTokens.wl"
import Token from "../../../../src/core/WhitelangTokens.wl"
import "../../../../src/core/WhitelangExceptions.wl"

class SymbolDefinition {
    let name -> String = "";
    let kind -> Int = 0;
    let range -> WhitelangExceptions.SourceRange = null;
    let type_name -> String = "";
    let import_path -> String = "";
    let import_name -> String = "";
    let owner_type -> String = "";
    let top_level -> Bool = false;

    init(name -> String, kind -> Int, range -> WhitelangExceptions.SourceRange) {
        self.name = name;
        self.kind = kind;
        self.range = range;
        self.type_name = "";
        self.import_path = "";
        self.import_name = "";
        self.owner_type = "";
        self.top_level = false;
    }
}

class SymbolReference {
    let name -> String = "";
    let range -> WhitelangExceptions.SourceRange = null;
    let definition -> SymbolDefinition = null;
    let qualifier -> String = "";

    init(
        name -> String,
        range -> WhitelangExceptions.SourceRange,
        definition -> SymbolDefinition
    ) {
        self.name = name;
        self.range = range;
        self.definition = definition;
        self.qualifier = "";
    }
}

class SemanticDocument {
    let syntax -> FrontendDocument = null;
    let definitions -> Vector(Struct) = null;
    let references -> Vector(Struct) = null;
    let members -> Dict = null;

    init(
        syntax -> FrontendDocument,
        definitions -> Vector(Struct),
        references -> Vector(Struct)
    ) {
        self.syntax = syntax;
        self.definitions = definitions;
        self.references = references;
        self.members = Dict(32);
    }
}

func __is_type_symbol(kind -> Int) -> Bool {
    return kind == SYMBOL_STRUCT ||
           kind == SYMBOL_CLASS ||
           kind == SYMBOL_INTERFACE ||
           kind == SYMBOL_ENUM ||
           kind == SYMBOL_ERROR;
}

func __index_member_symbols(document -> SemanticDocument) -> Void {
    let symbols -> Vector(Struct) = document.syntax.symbols;
    let i -> Int = 0;
    while (i < symbols.length()) {
        let owner -> DocumentSymbol = symbols[i];
        if (__is_type_symbol(owner.kind)) {
            let j -> Int = 0;
            while (j < owner.children.length()) {
                let member -> DocumentSymbol = owner.children[j];
                let definition -> SymbolDefinition = SymbolDefinition(
                    member.name,
                    member.kind,
                    member.span
                );
                definition.owner_type = owner.name;
                document.members.put(
                    owner.name + "." + member.name,
                    definition
                );
                j += 1;
            }
        }
        i += 1;
    }
}

func __register_member(
    document -> SemanticDocument,
    owner -> String,
    definition -> SymbolDefinition
) -> Void {
    definition.owner_type = owner;
    document.members.put(owner + "." + definition.name, definition);
}

func __find_member(
    document -> SemanticDocument,
    owner -> String,
    name -> String
) -> SymbolDefinition {
    if (owner is null || owner.length() == 0) { return null; }
    return document.members[owner + "." + name];
}

class __Scope {
    let parent -> __Scope = null;
    let symbols -> Dict = null;

    init(parent -> __Scope) {
        self.parent = parent;
        self.symbols = Dict(16);
    }

    method define(definition -> SymbolDefinition) -> Void {
        self.symbols.put(definition.name, definition);
    }

    method find(name -> String) -> SymbolDefinition {
        let current -> __Scope = self;
        while (current is !null) {
            let definition -> SymbolDefinition = current.symbols[name];
            if (definition is !null) { return definition; }
            current = current.parent;
        }
        return null;
    }
}

func __definition(
    document -> SemanticDocument,
    scope -> __Scope,
    token -> Token,
    kind -> Int
) -> SymbolDefinition {
    let definition -> SymbolDefinition = SymbolDefinition(
        token.value,
        kind,
        token_span(document.syntax.path, document.syntax.source, token)
    );
    document.definitions.append(definition);
    scope.define(definition);
    return definition;
}

func type_text(node -> Struct) -> String {
    if (node is null) { return "Void"; }
    let base -> BaseNode = node;
    if (base.type == NODE_VAR_ACCESS) {
        let access -> VarAccessNode = node;
        return access.name_tok.value;
    }
    if (base.type == NODE_FIELD_ACCESS) {
        let access -> FieldAccessNode = node;
        return access.field_name;
    }
    if (base.type == NODE_PTR_TYPE) {
        let pointer -> PointerTypeNode = node;
        let result -> String = type_text(pointer.base_type);
        let i -> Int = 0;
        while (i < pointer.level) {
            result = "ptr " + result;
            i += 1;
        }
        return result;
    }
    if (base.type == NODE_VECTOR_TYPE) {
        let vector -> VectorTypeNode = node;
        return "Vector(" + type_text(vector.element_type) + ")";
    }
    if (base.type == NODE_ARRAY_TYPE) {
        let array -> ArrayTypeNode = node;
        return "Array(" + type_text(array.base_type) + ", " + array.size_tok.value + ")";
    }
    if (base.type == NODE_SLICE_TYPE) {
        let slice -> SliceTypeNode = node;
        return "Array(" + type_text(slice.element_type) + ")";
    }
    if (base.type == NODE_FALLIBLE_TYPE) {
        let fallible -> FallibleTypeNode = node;
        return type_text(fallible.base_type) + "?";
    }
    if (base.type == NODE_FUNCTION_TYPE) { return "Function"; }
    if (base.type == NODE_METHOD_TYPE) { return "Method"; }
    return "";
}

func __expression_type(scope -> __Scope, node -> Struct) -> String {
    if (node is null) { return ""; }
    let base -> BaseNode = node;
    if (base.type == NODE_INT) {
        let value -> IntNode = node;
        if (value.tok.value.ends_with("L") || value.tok.value.ends_with("l")) {
            return "Long";
        }
        return "Int";
    }
    if (base.type == NODE_FLOAT) { return "Float"; }
    if (base.type == NODE_BOOL) { return "Bool"; }
    if (base.type == NODE_STRING) { return "String"; }
    if (base.type == NODE_CHAR) { return "Char"; }
    if (base.type == NODE_NULL) { return "Null"; }
    if (base.type == NODE_NULLPTR) { return "AnyPtr"; }
    if (base.type == NODE_VAR_ACCESS) {
        let access -> VarAccessNode = node;
        let definition -> SymbolDefinition = scope.find(access.name_tok.value);
        if (definition is !null) { return definition.type_name; }
        return access.name_tok.value;
    }
    if (base.type == NODE_BINOP) {
        let binary -> BinOpNode = node;
        let op -> Int = binary.op_tok.type;
        if (op == WhitelangTokens.TOK_EE ||
            op == WhitelangTokens.TOK_NE ||
            op == WhitelangTokens.TOK_GT ||
            op == WhitelangTokens.TOK_LT ||
            op == WhitelangTokens.TOK_GTE ||
            op == WhitelangTokens.TOK_LTE ||
            op == WhitelangTokens.TOK_AND ||
            op == WhitelangTokens.TOK_OR ||
            op == WhitelangTokens.TOK_IS) {
            return "Bool";
        }
        let left -> String = __expression_type(scope, binary.left);
        let right -> String = __expression_type(scope, binary.right);
        if (left == "String" || right == "String") { return "String"; }
        if (left == "Float" || right == "Float") { return "Float"; }
        if (left == "Long" || right == "Long") { return "Long"; }
        return left;
    }
    if (base.type == NODE_UNARYOP) {
        let unary -> UnaryOpNode = node;
        if (unary.op_tok.type == WhitelangTokens.TOK_NOT) { return "Bool"; }
        return __expression_type(scope, unary.node);
    }
    if (base.type == NODE_CALL) {
        let call -> CallNode = node;
        return __expression_type(scope, call.callee);
    }
    if (base.type == NODE_TRY_UNWRAP) {
        let unwrap -> TryUnwrapNode = node;
        let wrapped -> String = __expression_type(scope, unwrap.expr);
        if (wrapped.ends_with("?")) {
            return wrapped.slice(0, wrapped.length() - 1);
        }
        return wrapped;
    }
    if (base.type == NODE_REF) { return "AnyPtr"; }
    return "";
}

func __reference(
    document -> SemanticDocument,
    scope -> __Scope,
    token -> Token
) -> SymbolReference {
    let reference -> SymbolReference = SymbolReference(
        token.value,
        token_span(document.syntax.path, document.syntax.source, token),
        scope.find(token.value)
    );
    document.references.append(reference);
    return reference;
}

func __field_token(access -> FieldAccessNode) -> Token {
    return Token(
        type=WhitelangTokens.TOK_IDENTIFIER,
        value=access.field_name,
        line=access.pos.ln,
        col=access.pos.col
    );
}

func __walk_type(document -> SemanticDocument, scope -> __Scope, node -> Struct) -> Void {
    if (node is null) { return; }
    let base -> BaseNode = node;
    if (base.type == NODE_VAR_ACCESS) {
        let access -> VarAccessNode = node;
        if (access.name_tok.type == WhitelangTokens.TOK_IDENTIFIER ||
            access.name_tok.type == WhitelangTokens.TOK_TYPE) {
            __reference(document, scope, access.name_tok);
        }
    } else if (base.type == NODE_PTR_TYPE) {
        let pointer -> PointerTypeNode = node;
        __walk_type(document, scope, pointer.base_type);
    } else if (base.type == NODE_VECTOR_TYPE) {
        let vector -> VectorTypeNode = node;
        __walk_type(document, scope, vector.element_type);
    } else if (base.type == NODE_ARRAY_TYPE) {
        let array -> ArrayTypeNode = node;
        __walk_type(document, scope, array.base_type);
    } else if (base.type == NODE_SLICE_TYPE) {
        let slice -> SliceTypeNode = node;
        __walk_type(document, scope, slice.element_type);
    } else if (base.type == NODE_FALLIBLE_TYPE) {
        let fallible -> FallibleTypeNode = node;
        __walk_type(document, scope, fallible.base_type);
    } else if (base.type == NODE_FUNCTION_TYPE) {
        let function_type -> FunctionTypeNode = node;
        let i -> Int = 0;
        while (i < function_type.arg_types.length()) {
            __walk_type(document, scope, function_type.arg_types[i]);
            i += 1;
        }
        __walk_type(document, scope, function_type.return_type);
    } else if (base.type == NODE_METHOD_TYPE) {
        let method_type -> MethodTypeNode = node;
        let i -> Int = 0;
        while (i < method_type.arg_types.length()) {
            __walk_type(document, scope, method_type.arg_types[i]);
            i += 1;
        }
        __walk_type(document, scope, method_type.return_type);
    }
}

func __walk_node(document -> SemanticDocument, scope -> __Scope, node -> Struct) -> Void {
    if (node is null) { return; }
    let base -> BaseNode = node;

    if (base.type == NODE_BLOCK) {
        let block -> BlockNode = node;
        let block_scope -> __Scope = __Scope(scope);
        let i -> Int = 0;
        while (i < block.stmts.length()) {
            __walk_node(document, block_scope, block.stmts[i]);
            i += 1;
        }
    } else if (base.type == NODE_VAR_DECL) {
        let declaration -> VarDeclareNode = node;
        __walk_type(document, scope, declaration.type_node);
        __walk_node(document, scope, declaration.value);
        let kind -> Int = SYMBOL_VARIABLE;
        if (declaration.is_const) { kind = SYMBOL_CONSTANT; }
        let definition -> SymbolDefinition =
            __definition(document, scope, declaration.name_tok, kind);
        definition.type_name = type_text(declaration.type_node);
        if (definition.type_name == "Auto") {
            definition.type_name = __expression_type(scope, declaration.value);
        }
    } else if (base.type == NODE_VAR_ACCESS) {
        let access -> VarAccessNode = node;
        __reference(document, scope, access.name_tok);
    } else if (base.type == NODE_VAR_ASSIGN) {
        let assignment -> VarAssignNode = node;
        __reference(document, scope, assignment.name_tok);
        __walk_node(document, scope, assignment.value);
    } else if (base.type == NODE_BINOP) {
        let binary -> BinOpNode = node;
        __walk_node(document, scope, binary.left);
        __walk_node(document, scope, binary.right);
    } else if (base.type == NODE_UNARYOP) {
        let unary -> UnaryOpNode = node;
        __walk_node(document, scope, unary.node);
    } else if (base.type == NODE_POSTFIX) {
        let postfix -> PostfixOpNode = node;
        __walk_node(document, scope, postfix.node);
    } else if (base.type == NODE_IF) {
        let conditional -> IfNode = node;
        __walk_node(document, scope, conditional.condition);
        __walk_node(document, scope, conditional.body);
        __walk_node(document, scope, conditional.else_body);
    } else if (base.type == NODE_WHILE) {
        let loop -> WhileNode = node;
        __walk_node(document, scope, loop.condition);
        __walk_node(document, scope, loop.body);
    } else if (base.type == NODE_FOR) {
        let loop -> ForNode = node;
        let loop_scope -> __Scope = __Scope(scope);
        __walk_node(document, loop_scope, loop.init);
        __walk_node(document, loop_scope, loop.cond);
        __walk_node(document, loop_scope, loop.step);
        __walk_node(document, loop_scope, loop.body);
    } else if (base.type == NODE_CALL) {
        let call -> CallNode = node;
        __walk_node(document, scope, call.callee);
        let i -> Int = 0;
        let count -> Int = 0;
        if (call.args is !null) { count = call.args.length(); }
        while (i < count) {
            let arg -> ArgNode = call.args[i];
            __walk_node(document, scope, arg.val);
            i += 1;
        }
    } else if (base.type == NODE_RETURN) {
        let return_node -> ReturnNode = node;
        __walk_node(document, scope, return_node.value);
    } else if (base.type == NODE_FIELD_ACCESS) {
        let access -> FieldAccessNode = node;
        __walk_node(document, scope, access.obj);
        let field_reference -> SymbolReference =
            __reference(document, scope, __field_token(access));
        if (field_reference.definition is null) {
            field_reference.definition = __find_member(
                document,
                __expression_type(scope, access.obj),
                access.field_name
            );
        }
        let object_base -> BaseNode = access.obj;
        if (object_base.type == NODE_VAR_ACCESS) {
            let object -> VarAccessNode = access.obj;
            field_reference.qualifier = object.name_tok.value;
        }
    } else if (base.type == NODE_FIELD_ASSIGN) {
        let assignment -> FieldAssignNode = node;
        __walk_node(document, scope, assignment.obj);
        let field_token -> Token = Token(
            type=WhitelangTokens.TOK_IDENTIFIER,
            value=assignment.field_name,
            line=assignment.pos.ln,
            col=assignment.pos.col
        );
        let field_reference -> SymbolReference =
            __reference(document, scope, field_token);
        if (field_reference.definition is null) {
            field_reference.definition = __find_member(
                document,
                __expression_type(scope, assignment.obj),
                assignment.field_name
            );
        }
        let object_base -> BaseNode = assignment.obj;
        if (object_base.type == NODE_VAR_ACCESS) {
            let object -> VarAccessNode = assignment.obj;
            field_reference.qualifier = object.name_tok.value;
        }
        __walk_node(document, scope, assignment.value);
    } else if (base.type == NODE_REF) {
        let reference -> RefNode = node;
        __walk_node(document, scope, reference.node);
    } else if (base.type == NODE_DEREF) {
        let dereference -> DerefNode = node;
        __walk_node(document, scope, dereference.node);
    } else if (base.type == NODE_PTR_ASSIGN) {
        let assignment -> PtrAssignNode = node;
        __walk_node(document, scope, assignment.pointer);
        __walk_node(document, scope, assignment.value);
    } else if (base.type == NODE_VECTOR_LIT) {
        let vector -> VectorLitNode = node;
        let i -> Int = 0;
        while (i < vector.elements.length()) {
            __walk_node(document, scope, vector.elements[i]);
            i += 1;
        }
    } else if (base.type == NODE_INDEX_ACCESS) {
        let access -> IndexAccessNode = node;
        __walk_node(document, scope, access.target);
        __walk_node(document, scope, access.index_node);
    } else if (base.type == NODE_INDEX_ASSIGN) {
        let assignment -> IndexAssignNode = node;
        __walk_node(document, scope, assignment.target);
        __walk_node(document, scope, assignment.index_node);
        __walk_node(document, scope, assignment.value);
    } else if (base.type == NODE_SLICE_ACCESS) {
        let slice -> SliceAccessNode = node;
        __walk_node(document, scope, slice.target);
        __walk_node(document, scope, slice.start_idx);
        __walk_node(document, scope, slice.end_idx);
    } else if (base.type == NODE_MAP_LIT) {
        let map -> MapLitNode = node;
        let i -> Int = 0;
        while (i < map.pairs.length()) {
            let pair -> MapPairNode = map.pairs[i];
            __walk_node(document, scope, pair.key);
            __walk_node(document, scope, pair.value);
            i += 1;
        }
    } else if (base.type == NODE_TRY_UNWRAP) {
        let unwrap -> TryUnwrapNode = node;
        __walk_node(document, scope, unwrap.expr);
    } else if (base.type == NODE_CATCH) {
        let catch_node -> CatchNode = node;
        __walk_node(document, scope, catch_node.stmt);
        let catch_scope -> __Scope = __Scope(scope);
        __definition(document, catch_scope, catch_node.err_name, SYMBOL_VARIABLE);
        __walk_node(document, catch_scope, catch_node.body);
    } else if (base.type == NODE_THROW) {
        let throw_node -> ThrowNode = node;
        __walk_node(document, scope, throw_node.value);
    }
}

func __declare_params(
    document -> SemanticDocument,
    scope -> __Scope,
    params -> Vector(Struct)
) -> Void {
    if (params is null) { return; }
    let i -> Int = 0;
    while (i < params.length()) {
        let param -> ParamNode = params[i];
        __walk_type(document, scope, param.type_tok);
        let definition -> SymbolDefinition =
            __definition(document, scope, param.name_tok, SYMBOL_PARAMETER);
        definition.type_name = type_text(param.type_tok);
        i += 1;
    }
}

func __walk_function(
    document -> SemanticDocument,
    parent -> __Scope,
    params -> Vector(Struct),
    return_type -> Struct,
    body -> Struct
) -> Void {
    let scope -> __Scope = __Scope(parent);
    __declare_params(document, scope, params);
    __walk_type(document, scope, return_type);
    __walk_node(document, scope, body);
}

func __declare_top_level(document -> SemanticDocument, scope -> __Scope) -> Void {
    let block -> BlockNode = document.syntax.ast;
    let i -> Int = 0;
    while (i < block.stmts.length()) {
        let node -> Struct = block.stmts[i];
        let base -> BaseNode = node;
        if (base.type == NODE_FUNC_DEF) {
            let function_node -> FunctionDefNode = node;
            let definition -> SymbolDefinition =
                __definition(document, scope, function_node.name_tok, SYMBOL_FUNCTION);
            definition.top_level = true;
            definition.type_name = type_text(function_node.ret_type_tok);
        } else if (base.type == NODE_EXTERN_FUNC) {
            let extern_node -> ExternFuncNode = node;
            let definition -> SymbolDefinition =
                __definition(document, scope, extern_node.name_tok, SYMBOL_FUNCTION);
            definition.top_level = true;
            definition.type_name = type_text(extern_node.ret_type_tok);
        } else if (base.type == NODE_EXTERN_BLOCK) {
            let extern_block -> ExternBlockNode = node;
            let j -> Int = 0;
            while (j < extern_block.funcs.length()) {
                let extern_node -> ExternFuncNode = extern_block.funcs[j];
                let definition -> SymbolDefinition =
                    __definition(document, scope, extern_node.name_tok, SYMBOL_FUNCTION);
                definition.top_level = true;
                definition.type_name = type_text(extern_node.ret_type_tok);
                j += 1;
            }
        } else if (base.type == NODE_VAR_DECL) {
            let declaration -> VarDeclareNode = node;
            let kind -> Int = SYMBOL_VARIABLE;
            if (declaration.is_const) { kind = SYMBOL_CONSTANT; }
            let definition -> SymbolDefinition =
                __definition(document, scope, declaration.name_tok, kind);
            definition.top_level = true;
            definition.type_name = type_text(declaration.type_node);
            if (definition.type_name == "Auto") {
                definition.type_name = __expression_type(scope, declaration.value);
            }
        } else if (base.type == NODE_STRUCT_DEF) {
            let struct_node -> StructDefNode = node;
            let definition -> SymbolDefinition =
                __definition(document, scope, struct_node.name_tok, SYMBOL_STRUCT);
            definition.top_level = true;
            definition.type_name = struct_node.name_tok.value;
        } else if (base.type == NODE_CLASS_DEF) {
            let class_node -> ClassDefNode = node;
            let definition -> SymbolDefinition =
                __definition(document, scope, class_node.name_tok, SYMBOL_CLASS);
            definition.top_level = true;
            definition.type_name = class_node.name_tok.value;
        } else if (base.type == NODE_ENUM_DEF) {
            let enum_node -> EnumDefNode = node;
            let kind -> Int = SYMBOL_ENUM;
            if (enum_node.is_error) { kind = SYMBOL_ERROR; }
            let definition -> SymbolDefinition =
                __definition(document, scope, enum_node.name_tok, kind);
            definition.top_level = true;
            definition.type_name = enum_node.name_tok.value;
        } else if (base.type == NODE_INTERFACE_DEF) {
            let interface_node -> InterfaceDefNode = node;
            let definition -> SymbolDefinition =
                __definition(document, scope, interface_node.name_tok, SYMBOL_INTERFACE);
            definition.top_level = true;
            definition.type_name = interface_node.name_tok.value;
        } else if (base.type == NODE_IMPORT) {
            let import_node -> ImportNode = node;
            if (import_node.alias_tok is !null) {
                let module -> SymbolDefinition =
                    __definition(document, scope, import_node.alias_tok, SYMBOL_MODULE);
                module.top_level = true;
                module.import_path = import_node.path_tok.value;
            }
            if (import_node.symbols is !null) {
                let j -> Int = 0;
                while (j < import_node.symbols.length()) {
                    let imported -> ImportSymbolNode = import_node.symbols[j];
                    let token -> Token = imported.name_tok;
                    if (imported.alias_tok is !null) { token = imported.alias_tok; }
                    let definition -> SymbolDefinition =
                        __definition(document, scope, token, SYMBOL_IMPORT);
                    definition.top_level = true;
                    definition.import_path = import_node.path_tok.value;
                    definition.import_name = imported.name_tok.value;
                    j += 1;
                }
            }
        }
        i += 1;
    }
}

func __walk_top_level(document -> SemanticDocument, scope -> __Scope) -> Void {
    let block -> BlockNode = document.syntax.ast;
    let i -> Int = 0;
    while (i < block.stmts.length()) {
        let node -> Struct = block.stmts[i];
        let base -> BaseNode = node;
        if (base.type == NODE_FUNC_DEF) {
            let function_node -> FunctionDefNode = node;
            __walk_function(
                document,
                scope,
                function_node.params,
                function_node.ret_type_tok,
                function_node.body
            );
        } else if (base.type == NODE_EXTERN_FUNC) {
            let extern_node -> ExternFuncNode = node;
            let function_scope -> __Scope = __Scope(scope);
            __declare_params(document, function_scope, extern_node.params);
            __walk_type(document, function_scope, extern_node.ret_type_tok);
        } else if (base.type == NODE_EXTERN_BLOCK) {
            let extern_block -> ExternBlockNode = node;
            let j -> Int = 0;
            while (j < extern_block.funcs.length()) {
                let extern_node -> ExternFuncNode = extern_block.funcs[j];
                let function_scope -> __Scope = __Scope(scope);
                __declare_params(document, function_scope, extern_node.params);
                __walk_type(document, function_scope, extern_node.ret_type_tok);
                j += 1;
            }
        } else if (base.type == NODE_VAR_DECL) {
            let declaration -> VarDeclareNode = node;
            __walk_type(document, scope, declaration.type_node);
            __walk_node(document, scope, declaration.value);
        } else if (base.type == NODE_STRUCT_DEF) {
            let struct_node -> StructDefNode = node;
            let j -> Int = 0;
            while (j < struct_node.fields.length()) {
                let field -> ParamNode = struct_node.fields[j];
                __walk_type(document, scope, field.type_tok);
                j += 1;
            }
            __walk_node(document, scope, struct_node.body);
        } else if (base.type == NODE_CLASS_DEF) {
            let class_node -> ClassDefNode = node;
            if (class_node.parent_tok is !null) {
                __reference(document, scope, class_node.parent_tok);
            }
            if (class_node.interfaces is !null) {
                let j -> Int = 0;
                while (j < class_node.interfaces.length()) {
                    let interface_token -> Token = class_node.interfaces[j];
                    __reference(document, scope, interface_token);
                    j += 1;
                }
            }

            let class_scope -> __Scope = __Scope(scope);
            let j -> Int = 0;
            while (j < class_node.fields.length()) {
                let field -> VarDeclareNode = class_node.fields[j];
                __walk_type(document, class_scope, field.type_node);
                __walk_node(document, class_scope, field.value);
                let definition -> SymbolDefinition =
                    __definition(document, class_scope, field.name_tok, SYMBOL_FIELD);
                definition.type_name = type_text(field.type_node);
                __register_member(
                    document,
                    class_node.name_tok.value,
                    definition
                );
                if (definition.type_name == "Auto") {
                    definition.type_name = __expression_type(class_scope, field.value);
                }
                j += 1;
            }
            j = 0;
            while (j < class_node.methods.length()) {
                let method_node -> MethodDefNode = class_node.methods[j];
                let method_kind -> Int = SYMBOL_METHOD;
                let method_name -> String = method_node.name_tok.value;
                let method_token -> Token = method_node.name_tok;
                if (method_name == "$init") {
                    method_token = Token(
                        type=WhitelangTokens.TOK_IDENTIFIER,
                        value="init",
                        line=method_node.pos.ln,
                        col=method_node.pos.col
                    );
                } else if (method_name == "$deinit") {
                    method_token = Token(
                        type=WhitelangTokens.TOK_IDENTIFIER,
                        value="deinit",
                        line=method_node.pos.ln,
                        col=method_node.pos.col
                    );
                } else if (method_name == "$type") {
                    method_kind = SYMBOL_CONVERSION;
                    method_token = Token(
                        type=WhitelangTokens.TOK_IDENTIFIER,
                        value="type",
                        line=method_node.pos.ln,
                        col=method_node.pos.col
                    );
                }
                let definition -> SymbolDefinition =
                    __definition(document, class_scope, method_token, method_kind);
                definition.type_name = type_text(method_node.return_type);
                __register_member(
                    document,
                    class_node.name_tok.value,
                    definition
                );
                j += 1;
            }
            j = 0;
            while (j < class_node.methods.length()) {
                let method_node -> MethodDefNode = class_node.methods[j];
                __walk_function(
                    document,
                    class_scope,
                    method_node.params,
                    method_node.return_type,
                    method_node.body
                );
                j += 1;
            }
        } else if (base.type == NODE_ENUM_DEF) {
            let enum_node -> EnumDefNode = node;
            let j -> Int = 0;
            while (j < enum_node.fields.length()) {
                let field -> EnumFieldNode = enum_node.fields[j];
                __walk_node(document, scope, field.value);
                j += 1;
            }
        } else if (base.type == NODE_INTERFACE_DEF) {
            let interface_node -> InterfaceDefNode = node;
            let j -> Int = 0;
            while (j < interface_node.methods.length()) {
                let method_node -> MethodDefNode = interface_node.methods[j];
                let method_scope -> __Scope = __Scope(scope);
                __declare_params(document, method_scope, method_node.params);
                __walk_type(document, method_scope, method_node.return_type);
                j += 1;
            }
        }
        i += 1;
    }
}

func analyze_document(syntax -> FrontendDocument) -> SemanticDocument {
    let document -> SemanticDocument = SemanticDocument(syntax, [], []);
    if (syntax is null || syntax.ast is null) { return document; }

    __index_member_symbols(document);
    let scope -> __Scope = __Scope(null);
    __declare_top_level(document, scope);
    __walk_top_level(document, scope);
    return document;
}

func __contains_utf16(
    range -> WhitelangExceptions.SourceRange,
    line -> Int,
    column -> Int
) -> Bool {
    if (range is null || range.start.line != line || range.end.line != line) {
        return false;
    }
    return column >= range.start.utf16_column &&
           column < range.end.utf16_column;
}

func definition_at(
    document -> SemanticDocument,
    line -> Int,
    utf16_column -> Int
) -> SymbolDefinition {
    if (document is null) { return null; }

    let reference -> SymbolReference =
        reference_at(document, line, utf16_column);
    if (reference is !null) { return reference.definition; }

    let i -> Int = 0;
    while (i < document.definitions.length()) {
        let definition -> SymbolDefinition = document.definitions[i];
        if (__contains_utf16(definition.range, line, utf16_column)) {
            return definition;
        }
        i += 1;
    }
    return null;
}

func reference_at(
    document -> SemanticDocument,
    line -> Int,
    utf16_column -> Int
) -> SymbolReference {
    if (document is null) { return null; }
    let i -> Int = 0;
    while (i < document.references.length()) {
        let reference -> SymbolReference = document.references[i];
        if (__contains_utf16(reference.range, line, utf16_column)) {
            return reference;
        }
        i += 1;
    }
    return null;
}

func type_at(
    document -> SemanticDocument,
    line -> Int,
    utf16_column -> Int
) -> String {
    let definition -> SymbolDefinition =
        definition_at(document, line, utf16_column);
    if (definition is null) { return ""; }
    return definition.type_name;
}
