import Dict from "dict"
import * from "WhitelangFrontend.wl"
import * from "WhitelangSemantic.wl"
import * from "WhitelangAnalysis.wl"
import * from "../../../../src/core/WhitelangNodes.wl"
import "../../../../src/core/WhitelangTokens.wl"

class WorkspaceSource {
    let path -> String;
    let version -> Int;
    let result -> FrontendResult;

    init(path -> String, version -> Int, result -> FrontendResult) {
        self.path = path;
        self.version = version;
        self.result = result;
    }
}

func source_dir(path -> String) -> String {
    let i -> Int = path.length() - 1;
    while (i >= 0) {
        if (path[i] == '/' || path[i] == '\\') {
            return path.slice(0, i);
        }
        i -= 1;
    }
    return ".";
}

func normalize_source_path(path -> String) -> String {
    let absolute -> Bool =
        path.length() > 0 && (path[0] == '/' || path[0] == '\\');
    let parts -> Vector(String) = [];
    let start -> Int = 0;
    let i -> Int = 0;
    while (i <= path.length()) {
        let separator -> Bool = i == path.length();
        if (!separator) {
            separator = path[i] == '/' || path[i] == '\\';
        }
        if (separator) {
            if (i > start) { parts.append(path.slice(start, i)); }
            start = i + 1;
        }
        i += 1;
    }

    let normalized -> Vector(String) = [];
    i = 0;
    while (i < parts.length()) {
        let part -> String = parts[i];
        if (part == ".") {
        } else if (part == "..") {
            if (normalized.length() > 0 &&
                normalized[normalized.length() - 1] != "..") {
                let shortened -> Vector(String) = [];
                let j -> Int = 0;
                while (j + 1 < normalized.length()) {
                    shortened.append(normalized[j]);
                    j += 1;
                }
                normalized = shortened;
            } else if (!absolute) {
                normalized.append(part);
            }
        } else {
            normalized.append(part);
        }
        i += 1;
    }

    let result -> String = "";
    if (absolute) { result = "/"; }
    i = 0;
    while (i < normalized.length()) {
        result += normalized[i];
        if (i + 1 < normalized.length()) { result += "/"; }
        i += 1;
    }
    if (result.length() == 0) { return "."; }
    return result;
}

func __module_name(path -> String) -> String {
    let start -> Int = 0;
    let i -> Int = 0;
    while (i < path.length()) {
        if (path[i] == '/' || path[i] == '\\') { start = i + 1; }
        i += 1;
    }
    let end -> Int = path.length();
    if (path.ends_with(".wl")) { end -= 3; }
    if (end <= start) { return ""; }
    return path.slice(start, end);
}

func __top_level_definition(
    document -> SemanticDocument,
    name -> String
) -> SymbolDefinition {
    if (document is null) { return null; }
    let i -> Int = 0;
    while (i < document.definitions.length()) {
        let definition -> SymbolDefinition = document.definitions[i];
        if (definition.name == name && definition.top_level) {
            return definition;
        }
        i += 1;
    }
    return null;
}

class FrontendWorkspace {
    let documents -> Dict = null;
    let sources -> Vector(Struct) = null;
    let members -> Dict = null;
    let file_members -> Dict = null;

    init() {
        self.documents = Dict(32);
        self.sources = [];
        self.members = Dict(64);
        self.file_members = Dict(64);
    }

    method __member_key(owner_type -> String, name -> String) -> String {
        return owner_type + "." + name;
    }

    method __member_owner(type_name -> String) -> String {
        let result -> String = type_name;
        while (result.starts_with("ptr ")) {
            result = result.slice(4, result.length());
        }
        if (result.ends_with("?")) {
            result = result.slice(0, result.length() - 1);
        }
        return result;
    }

    method __rebuild_members() -> Void {
        // rebuilding keeps replacement and close semantics deterministic
        self.members = Dict(64);
        self.file_members = Dict(64);
        let i -> Int = 0;
        while (i < self.sources.length()) {
            let source -> WorkspaceSource = self.sources[i];
            if (source.result is !null &&
                source.result.valid &&
                source.result.semantics is !null) {
                let definitions -> Vector(Struct) =
                    source.result.semantics.definitions;
                let j -> Int = 0;
                while (j < definitions.length()) {
                    let definition -> SymbolDefinition = definitions[j];
                    if (definition.owner_type.length() > 0) {
                        let key -> String = self.__member_key(
                            definition.owner_type,
                            definition.name
                        );
                        self.file_members.put(
                            definition.range.file + "\n" + key,
                            definition
                        );
                        let existing -> SymbolDefinition =
                            self.members[key];
                        if (existing is null) {
                            self.members.put(key, definition);
                        } else if (existing.range is !null &&
                                   existing.range.file !=
                                   definition.range.file) {
                            let ambiguous -> SymbolDefinition =
                                SymbolDefinition("", 0, null);
                            ambiguous.owner_type = definition.owner_type;
                            self.members.put(key, ambiguous);
                        }
                    }
                    j += 1;
                }
            }
            i += 1;
        }
    }

    method __local_type(
        source -> WorkspaceSource,
        type_name -> String
    ) -> SymbolDefinition {
        let definition -> SymbolDefinition =
            __top_level_definition(source.result.semantics, type_name);
        if (definition is null) { return null; }
        if (definition.kind == SYMBOL_STRUCT ||
            definition.kind == SYMBOL_CLASS ||
            definition.kind == SYMBOL_INTERFACE ||
            definition.kind == SYMBOL_ENUM ||
            definition.kind == SYMBOL_ERROR) {
            return definition;
        }
        return null;
    }

    method __imported_type(
        source -> WorkspaceSource,
        type_name -> String
    ) -> SymbolDefinition {
        let block -> BlockNode = source.result.syntax.ast;
        let i -> Int = 0;
        while (i < block.stmts.length()) {
            let base -> BaseNode = block.stmts[i];
            if (base.type == NODE_IMPORT) {
                let import_node -> ImportNode = block.stmts[i];
                if (import_node.symbols is !null) {
                    let j -> Int = 0;
                    while (j < import_node.symbols.length()) {
                        let imported -> ImportSymbolNode =
                            import_node.symbols[j];
                        let visible_name -> String =
                            imported.name_tok.value;
                        if (imported.alias_tok is !null) {
                            visible_name = imported.alias_tok.value;
                        }
                        if (visible_name == type_name) {
                            return self.__resolve_import(
                                source,
                                import_node.path_tok.value,
                                imported.name_tok.value
                            );
                        }
                        if (imported.name_tok.type ==
                            WhitelangTokens.TOK_MUL) {
                            let resolved -> SymbolDefinition =
                                self.__resolve_import(
                                    source,
                                    import_node.path_tok.value,
                                    type_name
                                );
                            if (resolved is !null) { return resolved; }
                        }
                        j += 1;
                    }
                } else {
                    let resolved -> SymbolDefinition =
                        self.__resolve_import(
                            source,
                            import_node.path_tok.value,
                            type_name
                        );
                    if (resolved is !null) { return resolved; }
                }
            }
            i += 1;
        }
        return null;
    }

    method __resolve_member(
        source -> WorkspaceSource,
        owner_type -> String,
        name -> String
    ) -> SymbolDefinition {
        if (owner_type.length() == 0) { return null; }
        let key -> String = self.__member_key(owner_type, name);
        let candidate -> SymbolDefinition = self.members[key];
        if (candidate is null) { return null; }
        if (candidate.range is !null) { return candidate; }

        // duplicate type names need the import source to select a member table
        let owner -> SymbolDefinition =
            self.__local_type(source, owner_type);
        if (owner is null) {
            owner = self.__imported_type(source, owner_type);
        }
        if (owner is !null && owner.range is !null) {
            let exact -> SymbolDefinition =
                self.file_members[owner.range.file + "\n" + key];
            if (exact is !null) { return exact; }
        }
        return null;
    }

    method __resolve_members() -> Void {
        let i -> Int = 0;
        while (i < self.sources.length()) {
            let source -> WorkspaceSource = self.sources[i];
            if (source.result is !null &&
                source.result.valid &&
                source.result.semantics is !null) {
                let references -> Vector(Struct) =
                    source.result.semantics.references;
                let j -> Int = 0;
                while (j < references.length()) {
                    let reference -> SymbolReference = references[j];
                    if (reference.receiver is !null &&
                        reference.receiver.definition is !null) {
                        let receiver_type -> String =
                            reference.receiver.definition.type_name;
                        if (receiver_type.length() > 0) {
                            reference.owner_type =
                                self.__member_owner(receiver_type);
                        }
                    }
                    if (reference.owner_type.length() > 0) {
                        let local -> Bool =
                            reference.definition is !null &&
                            reference.definition.range is !null &&
                            reference.definition.range.file == source.path;
                        if (!local) {
                            reference.definition = self.resolve_member(
                                source.path,
                                reference.owner_type,
                                reference.name
                            );
                        }
                    }
                    j += 1;
                }
            }
            i += 1;
        }
    }

    method resolve_member(
        path -> String,
        owner_type -> String,
        name -> String
    ) -> SymbolDefinition {
        let source -> WorkspaceSource = self.find(path);
        if (source is null ||
            source.result is null ||
            !source.result.valid) {
            return null;
        }
        return self.__resolve_member(source, owner_type, name);
    }

    method update(path -> String, version -> Int, text -> String) -> FrontendResult {
        let normalized -> String = normalize_source_path(path);
        let result -> FrontendResult = check_source(normalized, text);
        let source -> WorkspaceSource = self.documents[normalized];
        if (source is null) {
            source = WorkspaceSource(normalized, version, result);
            self.documents.put(normalized, source);
            self.sources.append(source);
        } else {
            source.version = version;
            source.result = result;
        }
        self.__rebuild_members();
        self.__resolve_members();
        return result;
    }

    method remove(path -> String) -> Bool {
        let normalized -> String = normalize_source_path(path);
        if (!self.documents.contains_key(normalized)) { return false; }
        self.documents.remove(normalized);
        let remaining -> Vector(Struct) = [];
        let i -> Int = 0;
        while (i < self.sources.length()) {
            let source -> WorkspaceSource = self.sources[i];
            if (source.path != normalized) { remaining.append(source); }
            i += 1;
        }
        self.sources = remaining;
        self.__rebuild_members();
        self.__resolve_members();
        return true;
    }

    method find(path -> String) -> WorkspaceSource {
        return self.documents[normalize_source_path(path)];
    }

    method __import_source(from_path -> String, raw_path -> String) -> WorkspaceSource {
        let candidate -> String = raw_path;
        let absolute -> Bool =
            raw_path.length() > 0 &&
            (raw_path[0] == '/' || raw_path[0] == '\\');
        let drive_path -> Bool =
            raw_path.length() > 1 && raw_path[1] == ':';
        if (!absolute && !drive_path) {
            candidate = source_dir(from_path) + "/" + raw_path;
        }
        candidate = normalize_source_path(candidate);

        let source -> WorkspaceSource = self.documents[candidate];
        if (source is !null) { return source; }
        if (!candidate.ends_with(".wl")) {
            source = self.documents[candidate + ".wl"];
            if (source is !null) { return source; }
            source = self.documents[candidate + "/_pkg.wl"];
        }
        return source;
    }

    method __resolve_import(
        source_document -> WorkspaceSource,
        path -> String,
        name -> String
    ) -> SymbolDefinition {
        if (path is null || path.length() == 0) { return null; }
        let imported -> WorkspaceSource =
            self.__import_source(source_document.path, path);
        if (imported is null ||
            imported.result is null ||
            !imported.result.valid) {
            return null;
        }
        return __top_level_definition(imported.result.semantics, name);
    }

    method __qualified_import(
        source_document -> WorkspaceSource,
        qualifier -> String,
        name -> String
    ) -> SymbolDefinition {
        let block -> BlockNode = source_document.result.syntax.ast;
        let i -> Int = 0;
        while (i < block.stmts.length()) {
            let base -> BaseNode = block.stmts[i];
            if (base.type == NODE_IMPORT) {
                let import_node -> ImportNode = block.stmts[i];
                if (import_node.symbols is null) {
                    let module_name -> String =
                        __module_name(import_node.path_tok.value);
                    if (import_node.alias_tok is !null) {
                        module_name = import_node.alias_tok.value;
                    }
                    if (module_name == qualifier) {
                        return self.__resolve_import(
                            source_document,
                            import_node.path_tok.value,
                            name
                        );
                    }
                }
            }
            i += 1;
        }
        return null;
    }

    method __star_import(
        source_document -> WorkspaceSource,
        name -> String
    ) -> SymbolDefinition {
        let block -> BlockNode = source_document.result.syntax.ast;
        let i -> Int = 0;
        while (i < block.stmts.length()) {
            let base -> BaseNode = block.stmts[i];
            if (base.type == NODE_IMPORT) {
                let import_node -> ImportNode = block.stmts[i];
                if (import_node.symbols is !null) {
                    let j -> Int = 0;
                    while (j < import_node.symbols.length()) {
                        let imported -> ImportSymbolNode = import_node.symbols[j];
                        if (imported.name_tok.type == WhitelangTokens.TOK_MUL) {
                            let definition -> SymbolDefinition =
                                self.__resolve_import(
                                    source_document,
                                    import_node.path_tok.value,
                                    name
                                );
                            if (definition is !null) { return definition; }
                            break;
                        }
                        j += 1;
                    }
                }
            }
            i += 1;
        }
        return null;
    }

    method definition(
        path -> String,
        line -> Int,
        utf16_column -> Int
    ) -> SymbolDefinition {
        let document -> WorkspaceSource = self.find(path);
        if (document is null ||
            document.result is null ||
            !document.result.valid) {
            return null;
        }

        let reference -> SymbolReference =
            reference_at(document.result.semantics, line, utf16_column);
        if (reference is null) {
            return definition_at(
                document.result.semantics,
                line,
                utf16_column
            );
        }

        return self.resolve_reference(path, reference);
    }

    method resolve_reference(
        path -> String,
        reference -> SymbolReference
    ) -> SymbolDefinition {
        let document -> WorkspaceSource = self.find(path);
        if (document is null ||
            document.result is null ||
            !document.result.valid ||
            reference is null) {
            return null;
        }

        if (reference.definition is !null) {
            let definition -> SymbolDefinition = reference.definition;
            if (definition.kind == SYMBOL_IMPORT) {
                let imported -> SymbolDefinition =
                    self.__resolve_import(
                        document,
                        definition.import_path,
                        definition.import_name
                    );
                if (imported is !null) { return imported; }
            }
            return definition;
        }

        if (reference.qualifier.length() > 0) {
            let qualified -> SymbolDefinition =
                self.__qualified_import(
                    document,
                    reference.qualifier,
                    reference.name
                );
            if (qualified is !null) { return qualified; }
        }
        return self.__star_import(document, reference.name);
    }

    method type_name(
        path -> String,
        line -> Int,
        utf16_column -> Int
    ) -> String {
        let definition -> SymbolDefinition =
            self.definition(path, line, utf16_column);
        if (definition is null) { return ""; }
        return definition.type_name;
    }
}
