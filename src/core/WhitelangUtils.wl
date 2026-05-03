// core/WhitelangUtils.wl
import "builtin"
import "file_io"
import "dict"
import "WhitelangTokens.wl"
import "WhitelangNodes.wl"
import "WhitelangExceptions.wl"


extern func wl_getenv(name -> String) -> String from "C";


// Type constants
const TYPE_INT   -> Int = 1;
const TYPE_FLOAT -> Int = 2;
const TYPE_BOOL  -> Int = 3;
const TYPE_VOID  -> Int = 4;
const TYPE_STRING -> Int = 5;
const TYPE_NULL   -> Int = 6;
const TYPE_GENERIC_STRUCT   -> Int = 7;
const TYPE_GENERIC_FUNCTION -> Int = 8;
const TYPE_LONG  -> Int = 9;
const TYPE_BYTE  -> Int = 10;
const TYPE_GENERIC_CLASS -> Int = 11;
const TYPE_GENERIC_METHOD -> Int = 12;
const TYPE_AUTO -> Int = 13;
const TYPE_NULLPTR -> Int = 99;


// Core data structures
struct GCTracker(
    reg  -> String,
    type -> Int
)

struct CompileResult(
    reg  -> String,
    type -> Int,
    origin_type -> Int
)

struct SymbolInfo(
    reg  -> String, 
    type -> Int,
    origin_type -> Int, // for generic type
    is_const -> Bool // const
)

struct FuncInfo(
    name     -> String,
    base_name  -> String,
    ret_type -> Int, 
    arg_types -> Vector(Struct),
    is_varargs -> Bool
)

struct TypeListNode(type -> Int)

struct Scope(
    table  -> Dict, // symbol table of the current level
    parent -> Struct,  // parent scope or null
    gc_vars -> Vector(Struct)
)

struct StringConstant(
    id    -> Int,
    value -> String
)

struct FieldInfo(
    name      -> String,
    type      -> Int,
    llvm_type -> String,
    offset    -> Int   // for getelementptr
)
struct StructInfo(
    name      -> String,
    type_id   -> Int,
    fields    -> Vector(Struct),
    llvm_name -> String,
    init_body -> Struct,
    is_class  -> Bool,
    vtable_name -> String,
    parent_id   -> Int,
    vtable      -> Vector(Struct)
)

struct ArrayInfo(
    base_type -> Int,
    size      -> Int,
    llvm_name -> String
)

struct CaptureScope(
    local_vars -> Dict,
    captured_vars -> Dict,
    captured_list -> Vector(String)
)

struct Compiler(
    output_file -> File,
    reg_count   -> Int, 
    symbol_table -> Scope,
    global_symbol_table -> Dict,
    func_table   -> Dict,
    struct_table -> Dict,
    struct_id_map -> Dict,
    indent      -> String, 
    loop_stack   -> Struct, 
    scope_depth  -> Int,
    has_main     -> Bool,
    current_ret_type -> Int,
    string_list -> Vector(Struct),
    str_count   -> Int,
    type_counter -> Int, // custom type
    ptr_cache -> Dict,       // Key: "ptr_ID", Value: SymbolInfo(reg="", type=ptr_id)
    ptr_base_map -> Dict,    // Key: "ID", Value: SymbolInfo(reg="", type=base_id)
    vector_cache -> Dict,    // Key: "vec_baseID", Value: SymbolInfo(type=new_id)
    vector_base_map -> Dict, // Key: "ID", Value: SymbolInfo(type=baseID)
    array_info_map -> Dict, 
    array_type_cache -> Dict,
    func_ret_map -> Dict,
    declared_externs -> Dict,
    imported_modules -> Dict,
    current_package_prefix -> String,
    loaded_packages -> Dict,
    loaded_files -> Dict,
    current_dir -> String,
    curr_func -> FuncInfo,
    expected_type -> Int,
    type_drop_list -> Vector(Struct),
    global_buffer -> String,
    string_pool -> Dict
)


struct LoopScope(
    label_continue -> String,
    label_break    -> String,
    parent         -> Struct
)


// compiler init & state utils
func new_compiler(out_path -> String) -> Compiler {
    let f -> File = File(out_path, "w");
    // initialize empty scope
    let root_scope -> Scope = Scope(table=Dict(32), parent=null, gc_vars=[]);

    let comp -> Compiler = Compiler(
        output_file = f,
        reg_count = 1,
        symbol_table = root_scope,
        global_symbol_table = Dict(32),
        func_table = Dict(32),
        struct_table = Dict(32),
        struct_id_map = Dict(32),
        indent = "  ",
        loop_stack = null,
        scope_depth = 0,
        has_main = false,
        current_ret_type = TYPE_VOID,
        string_list = [],
        str_count = 0,
        type_counter = 100,
        ptr_cache=Dict(32),
        ptr_base_map=Dict(32),
        vector_cache=Dict(32),
        vector_base_map=Dict(32),
        func_ret_map=Dict(32),
        declared_externs=Dict(32),
        imported_modules=Dict(32),
        current_package_prefix = "",
        loaded_packages = Dict(32),
        loaded_files = Dict(32),
        current_dir = ".",
        curr_func = null,
        expected_type = 0,
        type_drop_list = [],
        global_buffer = "",
        string_pool = Dict(128),
        array_info_map = Dict(32),
        array_type_cache = Dict(32)
    );

    comp.type_drop_list.append(TypeListNode(type=TYPE_GENERIC_FUNCTION));
    return comp;
}

func next_reg(c -> Compiler) -> String {
    let name -> String = "%t" + c.reg_count;
    c.reg_count += 1;
    return name;
}

func next_label(c -> Compiler) -> String {
    let name -> String = "L" + c.reg_count;
    c.reg_count += 1;
    return name;
}

func void_result() -> CompileResult {
    return CompileResult(reg="", type=TYPE_VOID);
}


// symbol & field lookup utils
func find_symbol(c -> Compiler, name -> String) -> SymbolInfo {
    let curr -> Scope = c.symbol_table;
    while (curr is !null) {
        let info -> SymbolInfo = curr.table.get(name);
        if (info is !null) { return info; }
        curr = curr.parent;
    }

    return c.global_symbol_table.get(name);
}

func find_field(s_info -> StructInfo, name -> String) -> FieldInfo {
    if (s_info is null) { return null; }
    let fields -> Vector(Struct) = s_info.fields;
    let len -> Int = 0; if (fields is !null) { len = fields.length(); }
    let i -> Int = 0;
    while (i < len) {
        let curr -> FieldInfo = fields[i];
        if (curr.name == name) { return curr; }
        i += 1;
    }
    return null;
}

func get_field_by_index(s_info -> StructInfo, index -> Int) -> FieldInfo {
    if (s_info is null) { return null; }
    let fields -> Vector(Struct) = s_info.fields;
    let len -> Int = 0; if (fields is !null) { len = fields.length(); }
    if (index >= 0 && index < len) {
        return fields[index];
    }
    return null;
}

func bind_import_symbols(c -> Compiler, node -> ImportNode, prefix -> String) -> Void {
    let symbols -> Vector(Struct) = node.symbols;
    let s_len -> Int = 0; if (symbols is !null) { s_len = symbols.length(); }
    let i -> Int = 0;
    while (i < s_len) {
        let curr_sym -> ImportSymbolNode = symbols[i];
        let orig_name -> String = curr_sym.name_tok.value;
        let target_name -> String = orig_name;
        
        if (curr_sym.alias_tok is !null) {
            let a_tok -> Token = curr_sym.alias_tok;
            target_name = a_tok.value;
        }

        let lookup_name -> String = prefix + orig_name;
        if (target_name != lookup_name) {
            if (c.func_table.get(target_name) is !null ||
                c.struct_table.get(target_name) is !null ||
                c.global_symbol_table.get(target_name) is !null) {
                WhitelangExceptions.throw_import_error(node.pos, "Name '" + target_name + "' is already defined. Use 'as' to alias it.");
            }
        }

        let found -> Bool = false;

        let f_info -> FuncInfo = c.func_table.get(lookup_name);
        if (f_info is !null) {
            c.func_table.put(target_name, f_info);
            found = true;
        }

        let s_info -> StructInfo = c.struct_table.get(lookup_name);
        if (s_info is !null) {
            c.struct_table.put(target_name, s_info);
            found = true;
        }

        let g_info -> SymbolInfo = c.global_symbol_table.get(lookup_name);
        if (g_info is !null) {
            c.global_symbol_table.put(target_name, g_info);
            found = true;
        }

        if (!found) {
            WhitelangExceptions.throw_import_error(node.pos, "Cannot import '" + orig_name + "': symbol not found in module.");
        }

        i += 1;
    }
}


// type system & mapping utils
func get_llvm_type_str(c -> Compiler, type_id -> Int) -> String {
    if (type_id == TYPE_INT)   { return "i32"; }
    if (type_id == TYPE_LONG)   { return "i64"; }
    if (type_id == TYPE_BYTE)  { return "i8"; }
    if (type_id == TYPE_FLOAT) { return "double"; }
    if (type_id == TYPE_BOOL)  { return "i1"; }
    if (type_id == TYPE_VOID)  { return "void"; }
    if (type_id == TYPE_STRING){ return "i8*"; }
    if (type_id == TYPE_GENERIC_STRUCT) { return "i8*"; }
    if (type_id == TYPE_GENERIC_FUNCTION) { return "i8*"; }
    if (type_id == TYPE_GENERIC_CLASS) { return "i8*"; }
    if (type_id == TYPE_GENERIC_METHOD) { return "i8*"; }
    if (type_id == TYPE_AUTO) { return "i8*"; }

    let arr_info -> ArrayInfo = c.array_info_map.get("" + type_id);
    if (arr_info is !null) {
        return arr_info.llvm_name;
    }

    if (type_id >= 100) {
        let f_info -> SymbolInfo = c.func_ret_map.get("" + type_id);
        if (f_info is !null) {
            return "i8*";
        }

        let ptr_info -> SymbolInfo = c.ptr_base_map.get("" + type_id);
        if (ptr_info is !null) {
            if (ptr_info.type == TYPE_VOID) { return "i8*"; }
            return get_llvm_type_str(c, ptr_info.type) + "*";
        }

        let s_info -> StructInfo = c.struct_id_map.get("" + type_id);
        if (s_info is !null) { 
            return s_info.llvm_name + "*"; 
        }

        let v_info -> SymbolInfo = c.vector_base_map.get("" + type_id);
        if (v_info is !null) {
            let elem_ty -> String = get_llvm_type_str(c, v_info.type);
            return "{ i64, i64, " + elem_ty + "* }*";
        }
    }

    return "i8*"; // unknow type
}

func get_type_name(c -> Compiler, type_id -> Int) -> String {
    if (type_id == TYPE_INT)   { return "Int"; }
    if (type_id == TYPE_LONG)  { return "Long"; }
    if (type_id == TYPE_BYTE)  { return "Byte"; }
    if (type_id == TYPE_FLOAT) { return "Float"; }
    if (type_id == TYPE_BOOL)  { return "Bool"; }
    if (type_id == TYPE_VOID)  { return "Void"; }
    if (type_id == TYPE_STRING) { return "String"; }
    if (type_id == TYPE_NULL)   { return "null"; }
    if (type_id == TYPE_NULLPTR){ return "nullptr"; }
    if (type_id == TYPE_GENERIC_STRUCT) { return "Struct"; }
    if (type_id == TYPE_GENERIC_FUNCTION) { return "Function"; }
    if (type_id == TYPE_GENERIC_CLASS) { return "Class"; }
    if (type_id == TYPE_GENERIC_METHOD) { return "Method"; }
    if (type_id == TYPE_AUTO) { return "Auto"; }

    if (type_id >= 100) {
        let f_info -> SymbolInfo = c.func_ret_map.get("" + type_id);
        if (f_info is !null) {
            return "Function(" + get_type_name(c, f_info.type) + ")";
        }

        let s_info -> StructInfo = c.struct_id_map.get("" + type_id);
        if (s_info is !null) {
            return s_info.name;
        }

        let ptr_info -> SymbolInfo = c.ptr_base_map.get("" + type_id);
        if (ptr_info is !null) {
            return "Ptr<" + get_type_name(c, ptr_info.type) + ">";
        }

        let v_info -> SymbolInfo = c.vector_base_map.get("" + type_id);
        if (v_info is !null) {
            return "Vector(" + get_type_name(c, v_info.type) + ")";
        }

        let arr_info -> ArrayInfo = c.array_info_map.get("" + type_id);
        if (arr_info is !null) {
            return get_type_name(c, arr_info.base_type) + "[" + arr_info.size + "]";
        }
    }
    
    return "Unknown";
}

func is_pointer_type(c -> Compiler, type_id -> Int) -> Bool {
    if (type_id == TYPE_NULLPTR) { return true; } 

    if (type_id >= 100) {
        let key -> String = "" + type_id;
        let info -> SymbolInfo = c.ptr_base_map.get(key);
        if (info is !null) { return true; }
    }
    
    return false;
}

func is_void_ptr(c -> Compiler, type_id -> Int) -> Bool {
    if (type_id >= 100) {
        let base_info -> SymbolInfo = c.ptr_base_map.get("" + type_id);
        if (base_info is !null && base_info.type == TYPE_VOID) {
            return true;
        }
    }
    return false;
}

func is_ref_type(c -> Compiler, type_id -> Int) -> Bool {
    if (type_id == TYPE_STRING) { return true; }
    if (type_id == TYPE_GENERIC_STRUCT) { return true; }
    if (type_id == TYPE_GENERIC_FUNCTION) { return true; }
    if (type_id == TYPE_GENERIC_CLASS) { return true; }
    if (type_id == TYPE_GENERIC_METHOD) { return true; }
    if (type_id == TYPE_AUTO) { return true; }

    if (type_id >= 100) {
        if (c.array_info_map.get("" + type_id) is !null) { return false; }
        if (c.struct_id_map.get("" + type_id) is !null) { return true; }
        if (c.vector_base_map.get("" + type_id) is !null) { return true; }
        if (c.func_ret_map.get("" + type_id) is !null) { return true; }
    }
    
    return false;
}

func get_ptr_type_id(c -> Compiler, base_id -> Int) -> Int {
    let key -> String = "ptr_" + base_id;
    let cached -> SymbolInfo = c.ptr_cache.get(key);
    if (cached is !null) { return cached.type; }
    
    let new_id -> Int = c.type_counter;
    c.type_counter += 1;

    c.ptr_cache.put(key, SymbolInfo(reg="", type=new_id, origin_type=0));
    c.ptr_base_map.put("" + new_id, SymbolInfo(reg="", type=base_id));
    
    return new_id;
}

func get_func_type_id(c -> Compiler, ret_type_id -> Int) -> Int {
    let key -> String = "func_" + ret_type_id;
    let cached -> SymbolInfo = c.ptr_cache.get(key);
    if (cached is !null) { return cached.type; }
    let new_id -> Int = c.type_counter;
    c.type_counter += 1;

    c.func_ret_map.put("" + new_id, SymbolInfo(reg="", type=ret_type_id, origin_type=0));
    c.ptr_cache.put(key, SymbolInfo(reg="", type=new_id, origin_type=0));
    return new_id;
}

func get_vector_type_id(c -> Compiler, base_id -> Int) -> Int {
    let key -> String = "vec_" + base_id;
    let cached -> SymbolInfo = c.vector_cache.get(key);
    if (cached is !null) { return cached.type; }
    
    let new_id -> Int = c.type_counter;
    c.type_drop_list.append(TypeListNode(type=new_id));
    c.type_counter += 1;

    c.vector_cache.put(key, SymbolInfo(reg="", type=new_id, origin_type=0));
    c.vector_base_map.put("" + new_id, SymbolInfo(reg="", type=base_id, origin_type=0));
    
    return new_id;
}

func get_expr_type(c -> Compiler, node -> Struct) -> Int {
    if (node is null) { return 0; }
    let base -> BaseNode = node;
    
    if (base.type == NODE_STRING) { return TYPE_STRING; }
    if (base.type == NODE_INT) { return TYPE_INT; }
    if (base.type == NODE_FLOAT) { return TYPE_FLOAT; }
    if (base.type == NODE_BOOL) { return TYPE_BOOL; }
    
    if (base.type == NODE_VAR_ACCESS) {
        let v -> VarAccessNode = node;
        let info -> SymbolInfo = find_symbol(c, v.name_tok.value);
        if (info is !null) { return info.type; }
        return 0;
    }
    
    if (base.type == NODE_FIELD_ACCESS) {
        let f -> FieldAccessNode = node;
        let obj_type -> Int = get_expr_type(c, f.obj);
        if (obj_type >= 100) {
            let s_info -> StructInfo = c.struct_id_map.get("" + obj_type);
            if (s_info is !null) {
                let field -> FieldInfo = find_field(s_info, f.field_name);
                if (field is !null) { return field.type; }
            }
        }
        return 0;
    }
    
    if (base.type == NODE_INDEX_ACCESS) {
        let idx_node -> IndexAccessNode = node;
        let target_type -> Int = get_expr_type(c, idx_node.target);
        if (is_pointer_type(c, target_type) == true) {
            let base_info -> SymbolInfo = c.ptr_base_map.get("" + target_type);
            if (base_info is !null) { return base_info.type; }
        }
        if (target_type >= 100) {
            let v_info -> SymbolInfo = c.vector_base_map.get("" + target_type);
            if (v_info is !null) { return v_info.type; }
        }
        if (target_type == TYPE_STRING) { return TYPE_BYTE; }
        return 0;
    }

    if (base.type == NODE_CALL) {
        let call_node -> CallNode = node;
        let callee -> BaseNode = call_node.callee;
        if (callee.type == NODE_VAR_ACCESS) {
            let v -> VarAccessNode = call_node.callee;
            let f_info -> FuncInfo = c.func_table.get(v.name_tok.value);
            if (f_info is !null) { return f_info.ret_type; }
        }
        return 0;
    }

    return 0;
}

func resolve_type(c -> Compiler, node -> Struct) -> Int {
    if (node is null) { return TYPE_VOID; }
    let base -> BaseNode = node;

    if (base.type == NODE_FUNCTION_TYPE) {
        let f_node -> FunctionTypeNode = node;
        let ret_id -> Int = resolve_type(c, f_node.return_type);
        return get_func_type_id(c, ret_id);
    }
    if (base.type == NODE_METHOD_TYPE) {
        let m_node -> MethodTypeNode = node;
        let ret_id -> Int = resolve_type(c, m_node.return_type);
        return get_func_type_id(c, ret_id);
    }

    // Pointer Type (ptr*N Type)
    if (base.type == NODE_PTR_TYPE) {
        let p_node -> PointerTypeNode = node;
        let inner_base -> BaseNode = p_node.base_type;

        if (inner_base.type == NODE_ARRAY_TYPE) {
            let arr_node -> ArrayTypeNode = p_node.base_type;

            let fake_ptr -> PointerTypeNode = PointerTypeNode(
                type=p_node.type,
                base_type=arr_node.base_type,
                level=p_node.level,
                pos=p_node.pos
            );
            
            let fake_arr -> ArrayTypeNode = ArrayTypeNode(
                type=arr_node.type,
                base_type=fake_ptr,
                size_tok=arr_node.size_tok,
                pos=arr_node.pos
            );
            
            return resolve_type(c, fake_arr);
        }

        let base_id -> Int = resolve_type(c, p_node.base_type);
        
        let current_id -> Int = base_id;
        let i -> Int = 0;
        while (i < p_node.level) {
            current_id = get_ptr_type_id(c, current_id);
            i += 1;
        }
        return current_id;
    }

    // Vector
    if (base.type == NODE_VECTOR_TYPE) {
        let v_node -> VectorTypeNode = node;
        let elem_id -> Int = resolve_type(c, v_node.element_type);
        return get_vector_type_id(c, elem_id);
    }

    if (base.type == NODE_ARRAY_TYPE) {
        let arr_node -> ArrayTypeNode = node;
        let base_id -> Int = resolve_type(c, arr_node.base_type);

        let size_str -> String = arr_node.size_tok.value;
        let size -> Int = 0;
        let s_i -> Int = 0;
        while (s_i < size_str.length()) {
            let ch -> Int = size_str[s_i];
            size = size * 10 + (ch - 48);
            s_i += 1;
        }
        
        let cache_key -> String = "arr_" + base_id + "_" + size;
        let cached -> SymbolInfo = c.array_type_cache.get(cache_key);
        
        if (cached is !null) {
            return cached.type;
        }
        
        let new_id -> Int = c.type_counter;
        c.type_counter += 1;
        
        c.array_type_cache.put(cache_key, SymbolInfo(reg="", type=new_id, origin_type=0, is_const=false));
        
        let llvm_name -> String = "[" + size + " x " + get_llvm_type_str(c, base_id) + "]";
        c.array_info_map.put("" + new_id, ArrayInfo(base_type=base_id, size=size, llvm_name=llvm_name));
        
        return new_id;
    }

    if (base.type == NODE_SLICE_TYPE) {
        let s_node -> SliceTypeNode = node;
        let elem_id -> Int = resolve_type(c, s_node.element_type);
        
        let cache_key -> String = "slice_" + elem_id;
        let cached -> SymbolInfo = c.array_type_cache.get(cache_key);
        if (cached is !null) { return cached.type; }
        
        let new_id -> Int = c.type_counter;
        c.type_counter += 1;
        c.array_type_cache.put(cache_key, SymbolInfo(reg="", type=new_id, origin_type=0, is_const=false));
        
        let elem_ty_str -> String = get_llvm_type_str(c, elem_id);
        let llvm_name -> String = "%slice." + new_id;
        c.array_info_map.put("" + new_id, ArrayInfo(base_type=elem_id, size=-1, llvm_name=llvm_name));

        // { i64 length, T* data }
        c.output_file.write(llvm_name + " = type { i64, " + elem_ty_str + "* }\n\n");

        return new_id;
    }
    
    // Named Type (Int, Float, StructName)
    if (base.type == NODE_VAR_ACCESS) {
        let v -> VarAccessNode = node;
        let name -> String = v.name_tok.value;
        
        if (name == "Int") { return TYPE_INT; }
        if (name == "Long") { return TYPE_LONG; }
        if (name == "Byte") { return TYPE_BYTE; }
        if (name == "Float") { return TYPE_FLOAT; }
        if (name == "Bool") { return TYPE_BOOL; }
        if (name == "String") { return TYPE_STRING; }
        if (name == "Void") { return TYPE_VOID; }
        if (name == "Struct") { return TYPE_GENERIC_STRUCT; }
        if (name == "Function") { return TYPE_GENERIC_FUNCTION; }
        if (name == "Class") { return TYPE_GENERIC_CLASS; }
        if (name == "Method") { return TYPE_GENERIC_METHOD; }
        if (name == "Auto") { return TYPE_AUTO; }
        
        let s_info -> StructInfo = c.struct_table.get(name);
        if (s_info is !null) { return s_info.type_id; }
        
        WhitelangExceptions.throw_type_error(v.pos, "Unknown type: " + name);
    }
    
    return TYPE_VOID;
}

func get_func_sig_str(c -> Compiler, info -> FuncInfo) -> String {
    let ret_str -> String = get_llvm_type_str(c, info.ret_type);
    let args_str -> String = "";
    let arg_types -> Vector(Struct) = info.arg_types;
    let len -> Int = 0; if (arg_types is !null) { len = arg_types.length(); }
    let i -> Int = 0;
    
    while (i < len) {
        let curr -> TypeListNode = arg_types[i];
        if (i > 0) { args_str = args_str + ", "; }
        args_str += get_llvm_type_str(c, curr.type);
        i += 1;
    }

    if (info.is_varargs) {
        if (len > 0) { args_str = args_str + ", ..."; }
        else { args_str = "..."; }
    }
    
    return ret_str + " (" + args_str + ")*";
}


// file & text utils
func string_escape(s -> String) -> String {
    let res -> String = "";
    let i -> Int = 0;
    let len -> Int = s.length();
    
    while (i < len) {
        let b -> Byte = s[i];
        let code -> Int = b;
        
        if (code == 34) { // "
            res = res + "\\22";
        } else if (code == 92) { // \
            res = res + "\\5C";
        } else if (code == 10) { // \n
            res = res + "\\0A";
        } else if (code == 13) { // \r
            res = res + "\\0D";
        } else {
            res += s.slice(i, i + 1);
        }
        i += 1;
    }
    return res;
}

func file_exists(path -> String) -> Bool {
    let f -> File = File(path, "rb");
    if (f.handle is nullptr) {
        return false;
    }
    f.close();
    return true;
}

func get_dir_name(path -> String) -> String {
    let len -> Int = path.length();
    let i -> Int = len - 1;
    while (i >= 0) {
        let c -> Int = path[i];
        if (c == 47 || c == 92) { // '/' or '\'
            return path.slice(0, i);
        }
        i -= 1;
    }
    return ".";
}

func resolve_import_path(c -> Compiler, raw_path -> String, pos -> Position) -> String {
    if (raw_path.ends_with(".wl")) {
        if (c.current_dir == ".") {
            return raw_path;
        }
        return c.current_dir + "/" + raw_path;
    }

    let wl_path -> String = wl_getenv("WL_PATH");
    if (wl_path is null) {
        WhitelangExceptions.throw_environment_error("Missing 'WL_PATH' variable.");
    }

    let pkg_entry -> String = wl_path + "/std/" + raw_path + "/_pkg.wl";
    if (file_exists(pkg_entry)) {
        return pkg_entry;
    }

    let file_entry -> String = wl_path + "/std/" + raw_path + ".wl";
    if (file_exists(file_entry)) {
        return file_entry;
    }

    WhitelangExceptions.throw_import_error(pos, "Cannot find module '" + raw_path + "'. Searched:\n - " + pkg_entry + "\n - " + file_entry);
    return "";
}


// closure analysis utils
func record_capture(scope -> CaptureScope, v_name -> String) -> Void {
    if (scope.local_vars.get(v_name) is null) {
        if (scope.captured_vars.get(v_name) is null) {
            scope.captured_vars.put(v_name, TypeListNode(type=1));
            scope.captured_list.append(v_name);
        }
    }
}

func analyze_captures(node -> Struct, scope -> CaptureScope) -> Void {
    if (node is null) { return; }
    let base -> BaseNode = node;
    let type -> Int = base.type;

    if (type == NODE_BLOCK) {
        let b -> BlockNode = node;
        let stmts -> Vector(Struct) = b.stmts;
        let i -> Int = 0;
        let len -> Int = 0; if (stmts is !null) { len = stmts.length(); }
        while (i < len) {
            analyze_captures(stmts[i], scope);
            i += 1;
        }
    }
    else if (type == NODE_VAR_DECL) {
        let decl -> VarDeclareNode = node;
        scope.local_vars.put(decl.name_tok.value, TypeListNode(type=1));
        if (decl.value is !null) {
            analyze_captures(decl.value, scope);
        }
    }
    else if (type == NODE_VAR_ACCESS) {
        let acc -> VarAccessNode = node;
        record_capture(scope, acc.name_tok.value);
    }
    else if (type == NODE_VAR_ASSIGN) {
        let assign -> VarAssignNode = node;
        record_capture(scope, assign.name_tok.value);
        if (assign.value is !null) {
            analyze_captures(assign.value, scope);
        }
    }
    else if (type == NODE_BINOP) {
        let binop -> BinOpNode = node;
        analyze_captures(binop.left, scope);
        analyze_captures(binop.right, scope);
    }
    else if (type == NODE_UNARYOP) {
        let uop -> UnaryOpNode = node;
        analyze_captures(uop.node, scope);
    }
    else if (type == NODE_POSTFIX) {
        let pop -> PostfixOpNode = node;
        analyze_captures(pop.node, scope);
    }
    else if (type == NODE_IF) {
        let if_n -> IfNode = node;
        analyze_captures(if_n.condition, scope);
        analyze_captures(if_n.body, scope);
        analyze_captures(if_n.else_body, scope);
    }
    else if (type == NODE_WHILE) {
        let w_n -> WhileNode = node;
        analyze_captures(w_n.condition, scope);
        analyze_captures(w_n.body, scope);
    }
    else if (type == NODE_FOR) {
        let f_n -> ForNode = node;
        analyze_captures(f_n.init, scope);
        analyze_captures(f_n.cond, scope);
        analyze_captures(f_n.step, scope);
        analyze_captures(f_n.body, scope);
    }
    else if (type == NODE_CALL) {
        let call -> CallNode = node;
        analyze_captures(call.callee, scope);
        let args -> Vector(Struct) = call.args;
        let i -> Int = 0;
        let len -> Int = 0; if (args is !null) { len = args.length(); }
        while (i < len) {
            let arg -> ArgNode = args[i];
            analyze_captures(arg.val, scope);
            i += 1;
        }
    }
    else if (type == NODE_RETURN) {
        let ret -> ReturnNode = node;
        analyze_captures(ret.value, scope);
    }
    else if (type == NODE_FIELD_ACCESS) {
        let fa -> FieldAccessNode = node;
        analyze_captures(fa.obj, scope);
    }
    else if (type == NODE_FIELD_ASSIGN) {
        let fass -> FieldAssignNode = node;
        analyze_captures(fass.obj, scope);
        analyze_captures(fass.value, scope);
    }
    else if (type == NODE_INDEX_ACCESS) {
        let ia -> IndexAccessNode = node;
        analyze_captures(ia.target, scope);
        analyze_captures(ia.index_node, scope);
    }
    else if (type == NODE_INDEX_ASSIGN) {
        let iass -> IndexAssignNode = node;
        analyze_captures(iass.target, scope);
        analyze_captures(iass.index_node, scope);
        analyze_captures(iass.value, scope);
    }
    else if (type == NODE_VECTOR_LIT) {
        let vec -> VectorLitNode = node;
        let elems -> Vector(Struct) = vec.elements;
        let i -> Int = 0;
        let len -> Int = 0; if (elems is !null) { len = elems.length(); }
        while (i < len) {
            let arg -> ArgNode = elems[i];
            analyze_captures(arg.val, scope);
            i += 1;
        }
    }
    else if (type == NODE_REF) {
        let ref_n -> RefNode = node;
        analyze_captures(ref_n.node, scope);
    }
    else if (type == NODE_DEREF) {
        let deref_n -> DerefNode = node;
        analyze_captures(deref_n.node, scope);
    }
    else if (type == NODE_PTR_ASSIGN) {
        let pass -> PtrAssignNode = node;
        analyze_captures(pass.pointer, scope);
        analyze_captures(pass.value, scope);
    }
    else if (type == NODE_FUNC_DEF) {
        let func_def -> FunctionDefNode = node;
        scope.local_vars.put(func_def.name_tok.value, TypeListNode(type=1));

        let child_scope -> CaptureScope = CaptureScope(local_vars=Dict(32), captured_vars=Dict(32), captured_list=[]);

        let params -> Vector(Struct) = func_def.params;
        let i -> Int = 0;
        let len -> Int = 0; if (params is !null) { len = params.length(); }
        while (i < len) {
            let p_node -> ParamNode = params[i];
            child_scope.local_vars.put(p_node.name_tok.value, TypeListNode(type=1));
            i += 1;
        }

        analyze_captures(func_def.body, child_scope);
        let k_i -> Int = 0;
        let k_len -> Int = child_scope.captured_list.length();
        while (k_i < k_len) {
            let k_str -> String = child_scope.captured_list[k_i];
            record_capture(scope, k_str);
            k_i += 1;
        }
    }
}

// vector util
func check_out_index(c -> Compiler, target_node -> Struct, index_node -> Struct, pos -> Position) -> Void {
    let base_idx -> BaseNode = index_node;
    if (base_idx.type == NODE_INT) {
        let i_node -> IntNode = index_node;
        let val_str -> String = i_node.tok.value;

        let idx_val -> Int = 0;
        let start -> Int = 0;
        let is_neg -> Bool = false;

        if (val_str.length() > 0 && val_str[0] == 45) { // '-'
            start = 1;
            is_neg = true;
        }

        let j -> Int = start;
        while (j < val_str.length()) {
            let code -> Int = val_str[j];
            if (code >= 48 && code <= 57) {
                idx_val *= 10;
                idx_val += (code - 48);
            }
            j += 1;
        }
        
        if is_neg { 
            idx_val = -idx_val; 
        }

        if (idx_val < 0) {
            WhitelangExceptions.throw_index_error(pos, "Negative index " + val_str + " is not supported yet.");
        }

        let base_target -> BaseNode = target_node;
        if (base_target.type == NODE_VECTOR_LIT) {
            let vec_node -> VectorLitNode = target_node;
            let count -> Int = vec_node.count;
            
            if (idx_val >= count) {
                WhitelangExceptions.throw_index_error(pos, "Index " + val_str + " is out of bounds for vector of size " + count + ".");
            }
        }

        if (base_target.type == NODE_STRING) {
            let str_node -> StringNode = target_node;
            let s_len -> Int = str_node.tok.value.length();

            if (idx_val >= s_len) {
                WhitelangExceptions.throw_index_error(pos, "Index " + val_str + " is out of bounds for string of length " + s_len + ".");
            }
        }
    }
}


// oop
func is_subclass(c -> Compiler, child_id -> Int, parent_id -> Int) -> Bool {
    if (child_id == parent_id) { return true; }
    let s_info -> StructInfo = c.struct_id_map.get("" + child_id);
    if (s_info is null) { return false; }
    let curr_parent -> Int = s_info.parent_id;
    while (curr_parent != 0) {
        if (curr_parent == parent_id) { return true; }
        let p_info -> StructInfo = c.struct_id_map.get("" + curr_parent);
        if (p_info is null) { return false; }
        curr_parent = p_info.parent_id;
    }
    return false;
}