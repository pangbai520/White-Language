// core/WhitelangCompiler.wl
import "builtin"
import "file_io"
import "map"
import "WhitelangTokens.wl"
import "WhitelangNodes.wl"
import "WhitelangExceptions.wl"


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

const TYPE_NULLPTR -> Int = 99;


struct GCTracker(
    reg  -> String,
    type -> Int,
    next -> Struct
)

struct CompileResult(
    reg  -> String,
    type -> Int
)

struct SymbolInfo(
    reg  -> String, 
    type -> Int,
    origin_type -> Int // for generic type
)

struct FuncInfo(
    ret_type -> Int, 
    arg_types -> Struct,
    is_varargs -> Bool
)

struct TypeListNode(
    type -> Int,
    next -> Struct
)

struct Scope(
    table  -> HashMap, // symbol table of the current level
    parent -> Struct,  // parent scope or null
    gc_vars -> Struct
)

struct StringConstant(
    id    -> Int,
    value -> String,
    next  -> Struct
)

struct FieldInfo(
    name      -> String,
    type      -> Int,
    llvm_type -> String,
    offset    -> Int,   // for getelementptr
    next      -> Struct 
)
struct StructInfo(
    name      -> String,
    type_id   -> Int,
    fields    -> Struct,
    llvm_name -> String,
    init_body -> Struct
)

struct Compiler(
    output_file -> File,
    reg_count   -> Int, 
    symbol_table -> Scope,
    global_symbol_table -> HashMap,
    func_table   -> HashMap,
    struct_table -> HashMap,
    struct_id_map -> HashMap,
    indent      -> String, 
    loop_stack   -> Struct, 
    scope_depth  -> Int,
    has_main     -> Int,
    current_ret_type -> Int,
    string_list -> Struct,
    str_count   -> Int,
    type_counter -> Int, // custom type
    ptr_cache -> HashMap,       // Key: "ptr_ID", Value: SymbolInfo(reg="", type=ptr_id)
    ptr_base_map -> HashMap,    // Key: "ID", Value: SymbolInfo(reg="", type=base_id)
    vector_cache -> HashMap,    // Key: "vec_baseID", Value: SymbolInfo(type=new_id)
    vector_base_map -> HashMap, // Key: "ID", Value: SymbolInfo(type=baseID)
    func_ret_map -> HashMap,
    declared_externs -> HashMap
)


struct LoopScope(
    label_continue -> String,  
    label_break    -> String,  
    parent         -> Struct
)


func new_compiler(out_path -> String) -> Compiler {
    let f -> File = open(out_path, "w");
    // initialize empty scope
    let root_scope -> Scope = Scope(table=map_new(32), parent=null, gc_vars=null);

    return Compiler(
        output_file = f,
        reg_count = 1,
        symbol_table = root_scope,
        global_symbol_table = map_new(32),
        func_table = map_new(32),
        struct_table = map_new(32),
        struct_id_map = map_new(32),
        indent = "  ",
        loop_stack = null,
        scope_depth = 0,
        has_main = 0,
        current_ret_type = TYPE_VOID,
        string_list = null,
        str_count = 0,
        type_counter = 100,
        ptr_cache=map_new(32),
        ptr_base_map=map_new(32),
        vector_cache=map_new(32),
        vector_base_map=map_new(32),
        func_ret_map=map_new(32),
        declared_externs=map_new(32)
    );
}

func next_reg(c -> Compiler) -> String {
    let name -> String = "%t" + c.reg_count;
    c.reg_count = c.reg_count + 1;
    return name;
}

func promote_to_float(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_FLOAT) { return res; }
    let input_reg -> String = res.reg;

    if (res.type == TYPE_BYTE) {
        let uitofp_reg -> String = next_reg(c);
        write(c.output_file, c.indent + uitofp_reg + " = uitofp i8 " + input_reg + " to double\n");
        return CompileResult(reg=uitofp_reg, type=TYPE_FLOAT);
    }

    if (res.type == TYPE_LONG) {
        let sitofp_reg -> String = next_reg(c);
        write(c.output_file, c.indent + sitofp_reg + " = sitofp i64 " + input_reg + " to double\n");
        return CompileResult(reg=sitofp_reg, type=TYPE_FLOAT);
    }

    if (res.type == TYPE_BOOL) {
        let zext_reg -> String = next_reg(c);
        write(c.output_file, c.indent + zext_reg + " = zext i1 " + input_reg + " to i32\n");
        input_reg = zext_reg;
    }
    
    let n_reg -> String = next_reg(c);
    write(c.output_file, c.indent + n_reg + " = sitofp i32 " + input_reg + " to double\n");
    return CompileResult(reg=n_reg, type=TYPE_FLOAT);
}
func promote_to_long(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_LONG) { return res; }
    let input_reg -> String = res.reg;

    if (res.type == TYPE_BYTE) {
        let zext_reg -> String = next_reg(c);
        write(c.output_file, c.indent + zext_reg + " = zext i8 " + input_reg + " to i64\n");
        return CompileResult(reg=zext_reg, type=TYPE_LONG);
    }

    if (res.type == TYPE_BOOL) {
        let zext_reg -> String = next_reg(c);
        write(c.output_file, c.indent + zext_reg + " = zext i1 " + input_reg + " to i64\n");
        return CompileResult(reg=zext_reg, type=TYPE_LONG);
    }

    if (res.type == TYPE_INT) {
        let n_reg -> String = next_reg(c);
        // sext: sign extension
        write(c.output_file, c.indent + n_reg + " = sext i32 " + input_reg + " to i64\n");
        return CompileResult(reg=n_reg, type=TYPE_LONG);
    }
    
    return res;
}
func promote_to_int(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type != TYPE_BYTE) { return res; }
    
    let zext_reg -> String = next_reg(c);
    write(c.output_file, c.indent + zext_reg + " = zext i8 " + res.reg + " to i32\n");
    return CompileResult(reg=zext_reg, type=TYPE_INT);
}

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

    if (type_id >= 100) {
        let f_info -> SymbolInfo = map_get(c.func_ret_map, "" + type_id);
        if (f_info is !null) {
            return "i8*";
        }

        let ptr_info -> SymbolInfo = map_get(c.ptr_base_map, "" + type_id);
        if (ptr_info is !null) {
            return get_llvm_type_str(c, ptr_info.type) + "*";
        }

        let s_info -> StructInfo = map_get(c.struct_id_map, "" + type_id);
        if (s_info is !null) { 
            return s_info.llvm_name + "*"; 
        }

        let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + type_id);
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

    if (type_id >= 100) {
        let f_info -> SymbolInfo = map_get(c.func_ret_map, "" + type_id);
        if (f_info is !null) {
            return "Function(" + get_type_name(c, f_info.type) + ")";
        }

        let s_info -> StructInfo = map_get(c.struct_id_map, "" + type_id);
        if (s_info is !null) {
            return s_info.name;
        }

        let ptr_info -> SymbolInfo = map_get(c.ptr_base_map, "" + type_id);
        if (ptr_info is !null) {
            return "Ptr<" + get_type_name(c, ptr_info.type) + ">";
        }

        let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + type_id);
        if (v_info is !null) {
            return "Vector(" + get_type_name(c, v_info.type) + ")";
        }
    }
    
    return "Unknown";
}

func next_label(c -> Compiler) -> String {
    let name -> String = "L" + c.reg_count;
    c.reg_count = c.reg_count + 1;
    return name;
}

func void_result() -> CompileResult {
    return CompileResult(reg="", type=TYPE_VOID);
}

func find_symbol(c -> Compiler, name -> String) -> SymbolInfo {
    let curr -> Scope = c.symbol_table;
    while (curr is !null) {
        let info -> SymbolInfo = map_get(curr.table, name);
        if (info is !null) { return info; }
        curr = curr.parent;
    }

    return map_get(c.global_symbol_table, name);
}

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
            res = res + s.slice(i, i + 1);
        }
        i += 1;
    }
    return res;
}

func find_field(s_info -> StructInfo, name -> String) -> FieldInfo {
    if (s_info is null) { return null; }
    let curr -> FieldInfo = s_info.fields;
    while (curr is !null) {
        if (curr.name == name) { return curr; }
        curr = curr.next;
    }
    return null;
}

func get_field_by_index(s_info -> StructInfo, index -> Int) -> FieldInfo {
    let curr -> FieldInfo = s_info.fields;
    let i -> Int = 0;
    while (curr is !null) {
        if (i == index) { return curr; }
        curr = curr.next;
        i += 1;
    }
    return null;
}

func is_pointer_type(c -> Compiler, type_id -> Int) -> Bool {
    if (type_id == TYPE_NULLPTR) { return true; } 

    if (type_id >= 100) {
        let key -> String = "" + type_id;
        let info -> SymbolInfo = map_get(c.ptr_base_map, key);
        if (info is !null) { return true; }
    }
    
    return false;
}
func get_ptr_type_id(c -> Compiler, base_id -> Int) -> Int {
    let key -> String = "ptr_" + base_id;
    let cached -> SymbolInfo = map_get(c.ptr_cache, key);
    if (cached is !null) { return cached.type; }
    
    let new_id -> Int = c.type_counter;
    c.type_counter = c.type_counter + 1;

    map_put(c.ptr_cache, key, SymbolInfo(reg="", type=new_id, origin_type=0));
    map_put(c.ptr_base_map, "" + new_id, SymbolInfo(reg="", type=base_id));
    
    return new_id;
}

func get_func_type_id(c -> Compiler, ret_type_id -> Int) -> Int {
    let key -> String = "func_" + ret_type_id;
    let cached -> SymbolInfo = map_get(c.ptr_cache, key);
    if (cached is !null) { return cached.type; }
    let new_id -> Int = c.type_counter;
    c.type_counter = c.type_counter + 1;

    map_put(c.func_ret_map, "" + new_id, SymbolInfo(reg="", type=ret_type_id, origin_type=0));
    map_put(c.ptr_cache, key, SymbolInfo(reg="", type=new_id, origin_type=0));
    return new_id;
}

func get_vector_type_id(c -> Compiler, base_id -> Int) -> Int {
    let key -> String = "vec_" + base_id;
    let cached -> SymbolInfo = map_get(c.vector_cache, key);
    if (cached is !null) { return cached.type; }
    
    let new_id -> Int = c.type_counter;
    c.type_counter = c.type_counter + 1;

    map_put(c.vector_cache, key, SymbolInfo(reg="", type=new_id, origin_type=0));
    map_put(c.vector_base_map, "" + new_id, SymbolInfo(reg="", type=base_id, origin_type=0));
    
    return new_id;
}

// Recursively resolve types from AST nodes to Int ID
func resolve_type(c -> Compiler, node -> Struct) -> Int {
    if (node is null) { return TYPE_VOID; }
    let base -> BaseNode = node;

    if (base.type == NODE_FUNCTION_TYPE) {
        let f_node -> FunctionTypeNode = node;
        let ret_id -> Int = resolve_type(c, f_node.return_type);
        
        return get_func_type_id(c, ret_id);
    }
    
    // Pointer Type (ptr*N Type)
    if (base.type == NODE_PTR_TYPE) {
        let p_node -> PointerTypeNode = node;
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
        return get_vector_type_id(c, elem_id); // 类似于 get_ptr_type_id
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
        
        let s_info -> StructInfo = map_get(c.struct_table, name);
        if (s_info is !null) { return s_info.type_id; }
        
        throw_type_error(v.pos, "Unknown type: " + name);
    }
    
    return TYPE_VOID;
}

func convert_to_string(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_STRING) { return res; }

    if (res.type == TYPE_INT) {
        let buf -> String = next_reg(c);
        write(c.output_file, c.indent + buf + " = alloca [20 x i8]\n");
        let buf_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + buf_ptr + " = getelementptr [20 x i8], [20 x i8]* " + buf + ", i32 0, i32 0\n");
        
        let fmt -> String = next_reg(c);
        write(c.output_file, c.indent + fmt + " = getelementptr [3 x i8], [3 x i8]* @.fmt_int_simple, i32 0, i32 0\n");
        
        // call sprintf(buf, "%d", val)
        write(c.output_file, c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 20, i8* " + fmt + ", i32 " + res.reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_LONG) {
        let buf -> String = next_reg(c);
        write(c.output_file, c.indent + buf + " = alloca [32 x i8]\n");
        let buf_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + buf_ptr + " = getelementptr [32 x i8], [32 x i8]* " + buf + ", i32 0, i32 0\n");
        
        let fmt -> String = next_reg(c);
        write(c.output_file, c.indent + fmt + " = getelementptr [5 x i8], [5 x i8]* @.fmt_long_simple, i32 0, i32 0\n");
        
        write(c.output_file, c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 32, i8* " + fmt + ", i64 " + res.reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_BYTE) {
        let val_i32 -> String = next_reg(c);
        write(c.output_file, c.indent + val_i32 + " = zext i8 " + res.reg + " to i32\n");
        
        let buf -> String = next_reg(c);
        write(c.output_file, c.indent + buf + " = alloca [20 x i8]\n");
        let buf_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + buf_ptr + " = getelementptr [20 x i8], [20 x i8]* " + buf + ", i32 0, i32 0\n");

        let fmt -> String = next_reg(c);
        write(c.output_file, c.indent + fmt + " = getelementptr [3 x i8], [3 x i8]* @.fmt_int_simple, i32 0, i32 0\n");
        
        write(c.output_file, c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 20, i8* " + fmt + ", i32 " + val_i32 + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_FLOAT) {
        let buf -> String = next_reg(c);
        write(c.output_file, c.indent + buf + " = alloca [32 x i8]\n");
        let buf_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + buf_ptr + " = getelementptr [32 x i8], [32 x i8]* " + buf + ", i32 0, i32 0\n");
        
        let fmt -> String = next_reg(c);
        write(c.output_file, c.indent + fmt + " = getelementptr [3 x i8], [3 x i8]* @.fmt_float_simple, i32 0, i32 0\n");
        
        write(c.output_file, c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 20, i8* " + fmt + ", i32 " + res.reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_BOOL) {
        let ptr_true -> String = next_reg(c);
        write(c.output_file, c.indent + ptr_true + " = getelementptr [5 x i8], [5 x i8]* @.str_true, i32 0, i32 0\n");
        let ptr_false -> String = next_reg(c);
        write(c.output_file, c.indent + ptr_false + " = getelementptr [6 x i8], [6 x i8]* @.str_false, i32 0, i32 0\n");
        
        let res_reg -> String = next_reg(c);
        // select i1 %cond, type %v1, type %v2
        write(c.output_file, c.indent + res_reg + " = select i1 " + res.reg + ", i8* " + ptr_true + ", i8* " + ptr_false + "\n");
        return CompileResult(reg=res_reg, type=TYPE_STRING);
    }

    if (res.type == TYPE_NULL) {
        let null_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + null_ptr + " = getelementptr [5 x i8], [5 x i8]* @.str_null, i32 0, i32 0\n");
        return CompileResult(reg=null_ptr, type=TYPE_STRING);
    }

    let empty_res -> String = next_reg(c);
    write(c.output_file, c.indent + empty_res + " = getelementptr [5 x i8], [5 x i8]* @.str_null, i32 0, i32 0\n");
    return CompileResult(reg=empty_res, type=TYPE_STRING);
}

func get_func_sig_str(c -> Compiler, info -> FuncInfo) -> String {
    let ret_str -> String = get_llvm_type_str(c, info.ret_type);
    let args_str -> String = "";
    let curr -> TypeListNode = info.arg_types;
    let first -> Int = 1;
    while (curr is !null) {
        if (first == 0) { args_str = args_str + ", "; }
        args_str = args_str + get_llvm_type_str(c, curr.type);
        first = 0;
        curr = curr.next;
    }
    return ret_str + " (" + args_str + ")*";
}


// === SCOPE ===
func enter_scope(c -> Compiler) -> Void {
    let new_scope -> Scope = Scope(table=map_new(32), parent=c.symbol_table, gc_vars=null);
    c.symbol_table = new_scope;
    c.scope_depth = c.scope_depth + 1;
}
func exit_scope(c -> Compiler) -> Void {
    let curr_scope -> Scope = c.symbol_table;
    let curr_gc -> GCTracker = curr_scope.gc_vars;
    
    while (curr_gc is !null) {
        let ty_str -> String = get_llvm_type_str(c, curr_gc.type);
        let val_reg -> String = next_reg(c);
        write(c.output_file, c.indent + val_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_gc.reg + "\n");

        emit_release(c, val_reg, curr_gc.type);
        
        curr_gc = curr_gc.next;
    }

    if (c.symbol_table.parent is !null) {
        c.symbol_table = c.symbol_table.parent;
    }
    c.scope_depth -= 1;
}


// ===========
// === ARC ===
// ===========
func is_ref_type(c -> Compiler, type_id -> Int) -> Bool {
    if (type_id == TYPE_STRING) { return true; }
    if (type_id == TYPE_GENERIC_STRUCT) { return true; }
    if (type_id >= 100) {
        let s_info -> StructInfo = map_get(c.struct_id_map, "" + type_id);
        if (s_info is !null) { return true; }
    }
    
    return false;
}

func emit_retain(c -> Compiler, reg -> String, type_id -> Int) -> Void {
    if (is_ref_type(c, type_id) == 0) { return; }
    
    // cast to i8* for the runtime function
    let cast_reg -> String = next_reg(c);
    let src_ty -> String = get_llvm_type_str(c, type_id);
    write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + reg + " to i8*\n");
    write(c.output_file, c.indent + "call void @__wl_retain(i8* " + cast_reg + ")\n");
}

func emit_release(c -> Compiler, reg -> String, type_id -> Int) -> Void {
    if (is_ref_type(c, type_id) == 0) { return; }

    let cast_reg -> String = next_reg(c);
    let src_ty -> String = get_llvm_type_str(c, type_id);
    write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + reg + " to i8*\n");
    write(c.output_file, c.indent + "call void @__wl_release(i8* " + cast_reg + ")\n");
}

func cleanup_all_scopes(c -> Compiler) -> Void {
    let curr -> Scope = c.symbol_table;
    while (curr is !null) { 
        let gc_node -> GCTracker = curr.gc_vars;
        while (gc_node is !null) {
            let ty_str -> String = get_llvm_type_str(c, gc_node.type);
            let val_reg -> String = next_reg(c);
            write(c.output_file, c.indent + val_reg + " = load " + ty_str + ", " + ty_str + "* " + gc_node.reg + "\n");
            emit_release(c, val_reg, gc_node.type);
            gc_node = gc_node.next;
        }
        curr = curr.parent;
    }
}

func emit_vector_bounds_check(c -> Compiler, vec_reg -> String, idx_reg -> String, struct_ty -> String, pos -> Position) -> Void {
    let size_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    write(c.output_file, c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");

    let idx_i64 -> String = next_reg(c);
    write(c.output_file, c.indent + idx_i64 + " = sext i32 " + idx_reg + " to i64\n");

    let cmp_reg -> String = next_reg(c);
    write(c.output_file, c.indent + cmp_reg + " = icmp uge i64 " + idx_i64 + ", " + size_val + "\n");

    let fail_label -> String = "bounds_fail_" + c.type_counter;
    let ok_label -> String = "bounds_ok_" + c.type_counter;
    c.type_counter = c.type_counter + 1;
    
    write(c.output_file, c.indent + "br i1 " + cmp_reg + ", label %" + fail_label + ", label %" + ok_label + "\n");

    write(c.output_file, "\n" + fail_label + ":\n");

    let fmt_ptr -> String = "getelementptr inbounds ([66 x i8], [66 x i8]* @.fmt_err_bounds, i32 0, i32 0)";
    let line_num -> Int = pos.ln + 1;
    let col_num -> Int = pos.col + 1;
    write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + fmt_ptr + ", i32 " + line_num + ", i32 " + col_num + ")\n");

    let full_text -> String = pos.text;
    if (full_text.length() > 0) {
        let current_ln -> Int = 0;
        let scan_idx -> Int = 0;
        let len -> Int = full_text.length();
        let line_start_idx -> Int = 0;
        while (scan_idx < len) {
            if (current_ln == pos.ln) {
                line_start_idx = scan_idx;
                break;
            }
            if (full_text[scan_idx] == 10) { // \n
                current_ln = current_ln + 1;
            }
            scan_idx += 1;
        }

        let line_end_idx -> Int = line_start_idx;
        while (line_end_idx < len) {
            let ch -> Int = full_text[line_end_idx];
            if (ch == 10 || ch == 13) { break; }
            line_end_idx = line_end_idx + 1;
        }
        
        let raw_line -> String = full_text.slice(line_start_idx, line_end_idx);
        let code_content -> String = "    " + raw_line + "\n";
        
        let code_id -> Int = c.str_count;
        c.str_count = c.str_count + 1;
        let code_const -> StringConstant = StringConstant(id=code_id, value=code_content, next=c.string_list);
        c.string_list = code_const;

        let code_len -> Int = 4 + raw_line.length() + 2; 
        
        let code_ptr -> String = "getelementptr inbounds ([" + code_len + " x i8], [" + code_len + " x i8]* @.str." + code_id + ", i32 0, i32 0)";
        write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + code_ptr + ")\n");

        let arrow_str -> String = "    ";
        let k -> Int = 0;
        while (k < pos.col) {
            arrow_str = arrow_str + " ";
            k = k + 1;
        }
        arrow_str = arrow_str + "^\n";
        
        let arrow_id -> Int = c.str_count;
        c.str_count = c.str_count + 1;
        let arrow_const -> StringConstant = StringConstant(id=arrow_id, value=arrow_str, next=c.string_list);
        c.string_list = arrow_const;

        let arrow_len -> Int = 4 + pos.col + 3;
        
        let arrow_ptr -> String = "getelementptr inbounds ([" + arrow_len + " x i8], [" + arrow_len + " x i8]* @.str." + arrow_id + ", i32 0, i32 0)";
        write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + arrow_ptr + ")\n");
    }
    
    write(c.output_file, c.indent + "call void @exit(i32 1)\n");
    write(c.output_file, c.indent + "unreachable\n");

    write(c.output_file, "\n" + ok_label + ":\n");
}

func compile_arc_hooks(c -> Compiler) -> Void {
    // --- __wl_retain(i8* ptr) ---
    // if (ptr is null) return; ptr[-8].rc++;
    write(c.output_file, "define void @__wl_retain(i8* %ptr) {\n");
    write(c.output_file, "entry:\n");
    write(c.output_file, "  %is_null = icmp eq i8* %ptr, null\n");
    write(c.output_file, "  br i1 %is_null, label %done, label %work\n");
    write(c.output_file, "work:\n");
    write(c.output_file, "  %base = getelementptr i8, i8* %ptr, i32 -8\n");
    write(c.output_file, "  %rc_ptr = bitcast i8* %base to i32*\n");
    write(c.output_file, "  %rc = load i32, i32* %rc_ptr\n");
    write(c.output_file, "  %new_rc = add i32 %rc, 1\n");
    write(c.output_file, "  store i32 %new_rc, i32* %rc_ptr\n");
    write(c.output_file, "  br label %done\n");
    write(c.output_file, "done:\n");
    write(c.output_file, "  ret void\n");
    write(c.output_file, "}\n\n");

    // --- __wl_release(i8* ptr) ---
    // if (ptr is null) return; ptr[-8].rc--; if (rc == 0) free(ptr[-8]);
    write(c.output_file, "define void @__wl_release(i8* %ptr) {\n");
    write(c.output_file, "entry:\n");
    write(c.output_file, "  %is_null = icmp eq i8* %ptr, null\n");
    write(c.output_file, "  br i1 %is_null, label %done, label %work\n");
    write(c.output_file, "work:\n");
    write(c.output_file, "  %base = getelementptr i8, i8* %ptr, i32 -8\n");
    write(c.output_file, "  %rc_ptr = bitcast i8* %base to i32*\n");
    write(c.output_file, "  %rc = load i32, i32* %rc_ptr\n");
    write(c.output_file, "  %new_rc = sub i32 %rc, 1\n");
    write(c.output_file, "  store i32 %new_rc, i32* %rc_ptr\n");
    write(c.output_file, "  %is_zero = icmp eq i32 %new_rc, 0\n");
    write(c.output_file, "  br i1 %is_zero, label %free_it, label %done\n");
    write(c.output_file, "free_it:\n");

    write(c.output_file, "  call void @free(i8* %base)\n"); 
    write(c.output_file, "  br label %done\n");
    write(c.output_file, "done:\n");
    write(c.output_file, "  ret void\n");
    write(c.output_file, "}\n\n");
}



func pre_register_structs(c -> Compiler, node -> Struct) -> Void {
    let block -> BlockNode = node;
    let curr -> StmtListNode = block.stmts;
    while (curr is !null) {
        let base -> BaseNode = curr.stmt;
        if (base.type == NODE_STRUCT_DEF) {
            let n -> StructDefNode = curr.stmt;
            let s_name -> String = n.name_tok.value;
            let new_id -> Int = c.type_counter;
            c.type_counter = c.type_counter + 1;
            let info -> StructInfo = StructInfo(
                name = s_name,
                type_id = new_id,
                fields = null, 
                llvm_name = "%struct." + s_name,
                init_body = n.body
            );

            map_put(c.struct_table, s_name, info);
            map_put(c.struct_id_map, "" + new_id, info); 
        }
        curr = curr.next;
    }
}

// === COMPILE ===
func compile_block(c -> Compiler, node -> BlockNode) -> CompileResult {
    let is_root -> Int = 0;
    if (c.scope_depth == 0) {
        is_root = 1;
    }

    if (is_root == 0) {
        enter_scope(c);
    }
    
    let curr -> StmtListNode = node.stmts;
    let last_res -> CompileResult = null;
    let terminated -> Bool = false;
    while (curr is !null) {
        let stmt -> BaseNode = curr.stmt;

        if (stmt.type == NODE_RETURN) { terminated = true; }
        if (stmt.type == NODE_BREAK) { terminated = true; }
        if (stmt.type == NODE_CONTINUE) { terminated = true; }

        last_res = compile_node(c, curr.stmt);
        curr = curr.next;
    }
    
    if (is_root == 0) {
        if (terminated == 1) {
            if (c.symbol_table.parent is !null) {
                c.symbol_table = c.symbol_table.parent;
            }
            c.scope_depth -= 1;
        } else {
            exit_scope(c);
        }
    }
    
    if (last_res is null) { return void_result();}
    return last_res;
}

func compile_var_decl(c -> Compiler, node -> VarDeclareNode) -> CompileResult {
    let target_type_id -> Int = resolve_type(c, node.type_node);
    let llvm_ty_str -> String = get_llvm_type_str(c, target_type_id);
    
    let var_name -> String = node.name_tok.value;

    if (c.scope_depth == 0) {
        let global_name -> String = "@" + var_name;
        let init_val_str -> String = "0";
        if (target_type_id == TYPE_STRING || target_type_id >= 100 || target_type_id == TYPE_GENERIC_STRUCT || target_type_id == TYPE_GENERIC_FUNCTION) { init_val_str = "null"; }
        
        if (node.value is !null) {
            let val_node -> BaseNode = node.value;
            if (val_node.type == NODE_STRING) {
                let s_node -> StringNode = node.value;
                let s_val -> String = s_node.tok.value;
                let s_id -> Int = c.str_count;
                c.str_count = c.str_count + 1;
                let new_str -> StringConstant = StringConstant(id=s_id, value=s_val, next=c.string_list);
                c.string_list = new_str;

                let len -> Int = s_val.length() + 1;
                init_val_str = "getelementptr inbounds ([" + len + " x i8], [" + len + " x i8]* @.str." + s_id + ", i32 0, i32 0)";
            }
            else if (val_node.type == NODE_NULLPTR) {
                if (is_pointer_type(c, target_type_id) == false) {
                    throw_type_error(node.pos, "Global 'nullptr' can only be assigned to pointer types.");
                }
                init_val_str = "null";
            }
            else if (val_node.type == NODE_NULL) {
                if (is_pointer_type(c, target_type_id) == true) {
                throw_type_error(node.pos, "Global 'null' cannot be assigned to explicit pointer types. Use 'nullptr'.");
                }
                if (target_type_id == TYPE_INT || target_type_id == TYPE_FLOAT || target_type_id == TYPE_BOOL) {
                throw_type_error(node.pos, "Primitive types cannot be null.");
                }
                init_val_str = "null";
            }
            else if (val_node.type == NODE_INT) {
                let n -> IntNode = node.value;
                init_val_str = n.tok.value;
                if (target_type_id == TYPE_FLOAT) { throw_type_error(node.pos, "Type mismatch (Int -> Float). "); }
            } else if (val_node.type == NODE_FLOAT) {
                let n -> FloatNode = node.value;
                init_val_str = n.tok.value;
                if (target_type_id != TYPE_FLOAT) { throw_type_error(node.pos, "Type mismatch (Float -> Int). "); }
            } else if (val_node.type == NODE_BOOL) {
                let n -> BooleanNode = node.value;
                if (n.value == 1) { init_val_str = "1"; } else { init_val_str = "0"; }
                if (target_type_id != TYPE_BOOL) { throw_type_error(node.pos, "Type mismatch. "); }
            } else {
                throw_type_error(node.pos, "Global variable initialization must be a constant literal. ");
            }
        }
        
        write(c.output_file, global_name + " = global " + llvm_ty_str + " " + init_val_str + "\n");
        map_put(c.global_symbol_table, var_name, SymbolInfo(reg=global_name, type=target_type_id, origin_type=target_type_id));
        return void_result();
    }

    let ptr_reg -> String = next_reg(c);
    write(c.output_file, c.indent + ptr_reg + " = alloca " + llvm_ty_str + "\n");
    let origin_id -> Int = target_type_id;

    if (node.value is !null) {
        let val_res -> CompileResult = compile_node(c, node.value);
        if (val_res.type == TYPE_NULLPTR) {
            if (is_pointer_type(c, target_type_id) == false) {
                throw_type_error(node.pos, "Keyword 'nullptr' can only be assigned to explicit pointer types (ptr ...). Use 'null' for objects.");
            }
        }
        else if (val_res.type == TYPE_NULL) {
            if (is_pointer_type(c, target_type_id) == true) {
                throw_type_error(node.pos, "Keyword 'null' cannot be assigned to explicit pointer types. Use 'nullptr'.");
            }
            if (target_type_id == TYPE_INT || target_type_id == TYPE_FLOAT || target_type_id == TYPE_BOOL) {
                throw_type_error(node.pos, "Primitive types (Int, Float, Bool) cannot be null.");
            }
        }
        else {
            if (target_type_id == TYPE_BYTE && val_res.type == TYPE_INT) {
                let trunc_reg -> String = next_reg(c);
                write(c.output_file, c.indent + trunc_reg + " = trunc i32 " + val_res.reg + " to i8\n");
                val_res.reg = trunc_reg;
                val_res.type = TYPE_BYTE;
            }
            if (target_type_id == TYPE_LONG && val_res.type == TYPE_INT) {
                val_res = promote_to_long(c, val_res);
            }
            if (target_type_id == TYPE_FLOAT && val_res.type == TYPE_INT) {
                val_res = promote_to_float(c, val_res);
            }

            if (target_type_id == TYPE_GENERIC_STRUCT && val_res.type >= 100) {
                origin_id = val_res.type;
                let cast_reg -> String = next_reg(c);
                let src_ty -> String = get_llvm_type_str(c, val_res.type);
                write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to i8*\n");
                val_res.reg = cast_reg;
                val_res.type = TYPE_GENERIC_STRUCT;
            }
            if (val_res.type == TYPE_GENERIC_STRUCT && target_type_id >= 100) {
                if (map_get(c.struct_id_map, "" + target_type_id) is !null) {
                    let cast_reg -> String = next_reg(c);
                    let dest_ty -> String = get_llvm_type_str(c, target_type_id);
                    write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + val_res.reg + " to " + dest_ty + "\n");
                    val_res.reg = cast_reg;
                    val_res.type = target_type_id;
                }
            }
            if (val_res.type == TYPE_GENERIC_FUNCTION && target_type_id >= 100) {
                if (map_get(c.func_ret_map, "" + target_type_id) is !null) {
                    val_res.type = target_type_id;
                }
            }
            if (target_type_id == TYPE_GENERIC_FUNCTION && val_res.type >= 100) {
                let f_check -> SymbolInfo = map_get(c.func_ret_map, "" + val_res.type);
                if (f_check is !null) {
                    origin_id = val_res.type;
                    val_res.type = TYPE_GENERIC_FUNCTION;
                }
            }

            if (target_type_id != val_res.type) {
                throw_type_error(node.pos, "Cannot assign type '" + get_type_name(c, val_res.type) + "' to variable of type '" + get_type_name(c, target_type_id) + "'. ");
            }
        }

        if (c.scope_depth > 0) {
            emit_retain(c, val_res.reg, target_type_id);
        }

        write(c.output_file, c.indent + "store " + llvm_ty_str + " " + val_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
    }
    else {
        if (target_type_id == TYPE_GENERIC_STRUCT) {
            throw_missing_initializer(node.pos, "Variable of generic type 'Struct' must be initialized explicitly.");
        }

        let s_info -> StructInfo = map_get(c.struct_id_map, "" + target_type_id);
        if (s_info is !null) {
            let fake_call -> CallNode = CallNode(type=16, callee=null, args=null, pos=node.pos);
            let init_res -> CompileResult = compile_struct_init(c, s_info, fake_call);
            
            if (c.scope_depth > 0) {
                emit_retain(c, init_res.reg, target_type_id);
            }

            write(c.output_file, c.indent + "store " + llvm_ty_str + " " + init_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
        } else {
            let zero -> String = "0";
            if (target_type_id == TYPE_STRING || target_type_id >= 100 || target_type_id == TYPE_GENERIC_STRUCT || target_type_id == TYPE_GENERIC_FUNCTION) { zero = "null"; }
            if (target_type_id == TYPE_FLOAT) { zero = "0.0"; }
            write(c.output_file, c.indent + "store " + llvm_ty_str + " " + zero + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
        }
    }

    let curr_scope -> Scope = c.symbol_table;
    map_put(curr_scope.table, var_name, SymbolInfo(reg=ptr_reg, type=target_type_id, origin_type=origin_id));

    if (c.scope_depth > 0) {
        if (is_ref_type(c, target_type_id)) {
            let new_gc_node -> GCTracker = GCTracker(
                reg = ptr_reg,
                type = target_type_id,
                next = curr_scope.gc_vars
            );
            curr_scope.gc_vars = new_gc_node;
        }
    }

    return void_result(); 
}
func compile_var_assign(c -> Compiler, node -> VarAssignNode) -> CompileResult {
    let var_name -> String = node.name_tok.value;
    let info -> SymbolInfo = find_symbol(c, var_name);
    if (info is null) {
        throw_name_error(node.pos, "Undefined variable '" + var_name + "'.");
    }

    let val_res -> CompileResult = compile_node(c, node.value);

    if (val_res.type == TYPE_NULLPTR) {
        if (is_pointer_type(c, info.type) == false) {
            throw_type_error(node.pos, "nullptr can only be assigned to explicit pointer types.");
        }
    } else if (val_res.type == TYPE_NULL) {
        if (is_pointer_type(c, info.type) == true) {
            throw_type_error(node.pos, "null cannot be assigned to explicit pointer types. Use 'nullptr'.");
        }
        if (info.type == TYPE_INT || info.type == TYPE_FLOAT || info.type == TYPE_BOOL) {
            throw_type_error(node.pos, "Primitive types cannot be null.");
        }
    } else {
        if (info.type == TYPE_BYTE && val_res.type == TYPE_INT) {
            let trunc_reg -> String = next_reg(c);
            write(c.output_file, c.indent + trunc_reg + " = trunc i32 " + val_res.reg + " to i8\n");
            val_res.reg = trunc_reg;
            val_res.type = TYPE_BYTE;
        }
        if (info.type == TYPE_LONG && val_res.type == TYPE_INT) {
            val_res = promote_to_long(c, val_res);
        }
        if (info.type == TYPE_FLOAT && val_res.type == TYPE_INT) {
            val_res = promote_to_float(c, val_res);
        }
        if (info.type == TYPE_GENERIC_STRUCT && val_res.type >= 100) {
            let cast_reg -> String = next_reg(c);
            let src_ty -> String = get_llvm_type_str(c, val_res.type);
            write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to i8*\n");
            val_res.reg = cast_reg;
            val_res.type = TYPE_GENERIC_STRUCT;
        }
        if (val_res.type == TYPE_GENERIC_STRUCT && info.type >= 100) {
             if (map_get(c.struct_id_map, "" + info.type) is !null) {
                let cast_reg -> String = next_reg(c);
                let dest_ty -> String = get_llvm_type_str(c, info.type);
                write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + val_res.reg + " to " + dest_ty + "\n");
                val_res.reg = cast_reg;
                val_res.type = info.type;
             }
        }
        if (val_res.type == TYPE_GENERIC_FUNCTION && info.type >= 100) {
             if (map_get(c.func_ret_map, "" + info.type) is !null) {
                val_res.type = info.type;
             }
        }
        if (info.type != val_res.type) {
            throw_type_error(node.pos, "Type mismatch in assignment.");
        }
    }

    if (is_ref_type(c, info.type)) {
        emit_retain(c, val_res.reg, info.type);

        let old_val_reg -> String = next_reg(c);
        let ty_str -> String = get_llvm_type_str(c, info.type);
        write(c.output_file, c.indent + old_val_reg + " = load " + ty_str + ", " + ty_str + "* " + info.reg + "\n");

        emit_release(c, old_val_reg, info.type);
    }
    
    let ty_str -> String = get_llvm_type_str(c, info.type);
    write(c.output_file, c.indent + "store " + ty_str + " " + val_res.reg + ", " + ty_str + "* " + info.reg + "\n");
    return val_res; 
}

func compile_if(c -> Compiler, node -> IfNode) -> CompileResult {
    let cond_res -> CompileResult = compile_node(c, node.condition);
    if (cond_res.type != TYPE_BOOL) {
        throw_type_error(node.pos, "If condition must be a Bool. ");
    }
    
    let label_then -> String = next_label(c);
    let label_else -> String = next_label(c);
    let label_merge -> String = next_label(c);
    
    let target_else -> String = label_else;
    if (node.else_body is null) {
        target_else = label_merge;
    }
    
    write(c.output_file, c.indent + "br i1 " + cond_res.reg + ", label %" + label_then + ", label %" + target_else + "\n");
    
    write(c.output_file, "\n" + label_then + ":\n");
    compile_node(c, node.body);
    write(c.output_file, c.indent + "br label %" + label_merge + "\n");
    
    if (node.else_body is !null) {
        write(c.output_file, "\n" + label_else + ":\n");
        compile_node(c, node.else_body);
        write(c.output_file, c.indent + "br label %" + label_merge + "\n");
    }

    write(c.output_file, "\n" + label_merge + ":\n");
    return void_result();
}

func compile_while(c -> Compiler, node -> WhileNode) -> CompileResult {
    let label_cond -> String = next_label(c);
    let label_body -> String = next_label(c);
    let label_end  -> String = next_label(c);

    let current_scope -> LoopScope = LoopScope(label_continue=label_cond, label_break=label_end, parent=c.loop_stack);
    c.loop_stack = current_scope;

    write(c.output_file, c.indent + "br label %" + label_cond + "\n");
    write(c.output_file, "\n" + label_cond + ":\n");
    
    let cond_res -> CompileResult = compile_node(c, node.condition);
    if (cond_res.type != TYPE_BOOL) {
        throw_type_error(node.pos, "While condition must be a Bool. ");
    }
    write(c.output_file, c.indent + "br i1 " + cond_res.reg + ", label %" + label_body + ", label %" + label_end + "\n");

    write(c.output_file, "\n" + label_body + ":\n");
    compile_node(c, node.body);
    write(c.output_file, c.indent + "br label %" + label_cond + "\n");

    write(c.output_file, "\n" + label_end + ":\n");
    c.loop_stack = current_scope.parent;
    return void_result();
}

func compile_for(c -> Compiler, node -> ForNode) -> CompileResult {
    if (node.init is !null) {
        compile_node(c, node.init);
    }
    let label_cond -> String = next_label(c);
    let label_body -> String = next_label(c);
    let label_step -> String = next_label(c);
    let label_end  -> String = next_label(c);

    let current_scope -> LoopScope = LoopScope(label_continue=label_step, label_break=label_end, parent=c.loop_stack);
    c.loop_stack = current_scope;
    
    write(c.output_file, c.indent + "br label %" + label_cond + "\n");
    write(c.output_file, "\n" + label_cond + ":\n");
    if (node.cond is !null) {
        let cond_res -> CompileResult = compile_node(c, node.cond);
        if (cond_res.type != TYPE_BOOL) {
            throw_type_error(node.pos, "For condition must be a Bool. ");
        }
        write(c.output_file, c.indent + "br i1 " + cond_res.reg + ", label %" + label_body + ", label %" + label_end + "\n");
    } else {
        write(c.output_file, c.indent + "br label %" + label_body + "\n");
    }

    write(c.output_file, "\n" + label_body + ":\n");
    compile_node(c, node.body);

    write(c.output_file, c.indent + "br label %" + label_step + "\n");
    write(c.output_file, "\n" + label_step + ":\n");
    if (node.step is !null) {
        compile_node(c, node.step);
    }
    write(c.output_file, c.indent + "br label %" + label_cond + "\n");
    write(c.output_file, "\n" + label_end + ":\n");
    c.loop_stack = current_scope.parent;
    
    return void_result();
}

func compile_ptr_assign(c -> Compiler, node -> PtrAssignNode) -> CompileResult {
    let d_node -> DerefNode = node.pointer;
    let ptr_res -> CompileResult = compile_node(c, d_node.node); 

    let val_res -> CompileResult = compile_node(c, node.value);

    let i -> Int = 0;
    let curr_reg -> String = ptr_res.reg;
    let curr_type -> Int = ptr_res.type;

    while (i < d_node.level - 1) {
        if (curr_type == TYPE_NULL) { 
            throw_type_error(node.pos, "Cannot dereference 'nullptr'."); 
        }
        let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + curr_type);
        if (base_info is null) { 
            throw_type_error(node.pos, "Cannot dereference non-pointer."); 
        }
        
        let next_type -> Int = base_info.type;
        let ty_str -> String = get_llvm_type_str(c, next_type);
        let next_reg -> String = next_reg(c);
        write(c.output_file, c.indent + next_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_reg + "\n");
        
        curr_reg = next_reg;
        curr_type = next_type;
        i += 1;
    }

    if (curr_type == TYPE_NULL) {
        throw_null_dereference_error(node.pos, "Cannot dereference 'nullptr'. ");
    }

    let final_base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + curr_type);
    if (final_base_info is null) { 
        throw_type_error(node.pos, "Cannot assign to non-pointer."); 
    }
    
    // check type
    if (final_base_info.type != val_res.type) {
        throw_type_error(node.pos, "Type mismatch in pointer assignment.");
    }
    
    let target_type_id -> Int = final_base_info.type;
    if (val_res.type == TYPE_NULLPTR) {
        if (is_pointer_type(c, target_type_id) == false) {
            throw_type_error(node.pos, "nullptr can only be assigned to pointer types.");
        }
    }
    else if (val_res.type == TYPE_NULL) {
        if (is_pointer_type(c, target_type_id) == true) {
            throw_type_error(node.pos, "null cannot be assigned to pointer types. Use 'nullptr'.");
        }
        if (target_type_id == TYPE_INT || target_type_id == TYPE_FLOAT || target_type_id == TYPE_BOOL) {
            throw_type_error(node.pos, "Primitive types (Int, Float, Bool) cannot be null.");
        }
    }

    let llvm_ty -> String = get_llvm_type_str(c, target_type_id);

    if (is_ref_type(c, target_type_id)) {
        // retain new value
        emit_retain(c, val_res.reg, target_type_id);
        
        // load old value
        let old_val_reg -> String = next_reg(c);
        write(c.output_file, c.indent + old_val_reg + " = load " + llvm_ty + ", " + llvm_ty + "* " + curr_reg + "\n");
        
        // release old value
        emit_release(c, old_val_reg, target_type_id);
    }
    
    // store new value
    write(c.output_file, c.indent + "store " + llvm_ty + " " + val_res.reg + ", " + llvm_ty + "* " + curr_reg + "\n");

    return val_res;
}

func compile_func_def(c -> Compiler, node -> FunctionDefNode) -> CompileResult {
    let func_name -> String = node.name_tok.value;
    
    if (func_name == "main") {
        c.has_main = 1;
    }

    let ret_type_id -> Int = resolve_type(c, node.ret_type_tok);
    let llvm_ret_type -> String = get_llvm_type_str(c, ret_type_id);

    c.current_ret_type = ret_type_id;

    // register function signature
    let arg_types_head -> TypeListNode = null;
    let arg_types_curr -> TypeListNode = null;
    let curr_p -> ParamListNode = node.params;
    
    while (curr_p is !null) {
        let p -> ParamNode = curr_p.param;
        let p_id -> Int = resolve_type(c, p.type_tok);
        
        let t_node -> TypeListNode = TypeListNode(type=p_id, next=null);
        if (arg_types_head is null) { arg_types_head = t_node; arg_types_curr = t_node; }
        else { arg_types_curr.next = t_node; arg_types_curr = t_node; }
        
        curr_p = curr_p.next;
    }

    map_put(c.func_table, func_name, FuncInfo(ret_type=ret_type_id, arg_types=arg_types_head, is_varargs=false));

    let params_str -> String = "";
    let curr -> ParamListNode = node.params;
    let arg_idx -> Int = 0;
    
    while (curr is !null) {
        let p -> ParamNode = curr.param;
        let p_type_id -> Int = resolve_type(c, p.type_tok);
        let p_llvm_type -> String = get_llvm_type_str(c, p_type_id);
        
        if (arg_idx > 0) { params_str = params_str + ", "; }
        params_str = params_str + p_llvm_type + " %arg" + arg_idx;
        
        arg_idx += 1;
        curr = curr.next;
    }
    
    write(c.output_file, "define " + llvm_ret_type + " @" + func_name + "(" + params_str + ") {\n");
    write(c.output_file, "entry:\n");

    let old_sym -> Scope = c.symbol_table;
    c.symbol_table = Scope(table=map_new(32), parent=null, gc_vars=null);
    
    c.reg_count = 0; 
    c.scope_depth = 1;
    
    curr = node.params;
    arg_idx = 0;
    while (curr is !null) {
        let p -> ParamNode = curr.param;
        let p_name -> String = p.name_tok.value;
        
        let target_type_id -> Int = resolve_type(c, p.type_tok);
        let llvm_ty -> String = get_llvm_type_str(c, target_type_id);
        
        let addr_reg -> String = next_reg(c); 
        write(c.output_file, c.indent + addr_reg + " = alloca " + llvm_ty + "\n");
        write(c.output_file, c.indent + "store " + llvm_ty + " %arg" + arg_idx + ", " + llvm_ty + "* " + addr_reg + "\n");
        let curr_scope -> Scope = c.symbol_table;
        map_put(curr_scope.table, p_name, SymbolInfo(reg=addr_reg, type=target_type_id, origin_type=target_type_id));
        
        arg_idx += 1;
        curr = curr.next;
    }
    
    compile_node(c, node.body);
    let block -> BlockNode = node.body;
    let stmt_curr -> StmtListNode = block.stmts;
    let last_stmt -> Struct = null;
    
    while (stmt_curr is !null) {
        last_stmt = stmt_curr.stmt;
        stmt_curr = stmt_curr.next;
    }

    let has_term -> Bool = false;
    if (last_stmt is !null) {
        let base -> BaseNode = last_stmt;
        if (base.type == NODE_RETURN) { has_term = true; }
    }

    if (has_term == 0) {
        if (ret_type_id == TYPE_VOID) {
            write(c.output_file, c.indent + "ret void\n");
        } else {
            let zero_val -> String = "0";
            if (ret_type_id == TYPE_FLOAT) { zero_val = "0.0"; }
            else if (ret_type_id == TYPE_STRING || ret_type_id >= 100 || ret_type_id == TYPE_GENERIC_STRUCT || ret_type_id == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
            
            write(c.output_file, c.indent + "ret " + llvm_ret_type + " " + zero_val + "\n");
        }
    }
    
    write(c.output_file, "}\n\n");
    
    // restore scope
    c.symbol_table = old_sym;
    c.scope_depth = 0;
    
    return void_result();
}

func compile_return(c -> Compiler, node -> ReturnNode) -> CompileResult {
    if (node.value is !null) {
        // return void check
        if (c.current_ret_type == TYPE_VOID) {
            throw_type_error(node.pos, "Void function cannot return a value. ");
        }

        let res -> CompileResult = compile_node(c, node.value);
        let ret_val_reg -> String = res.reg;
        let target_ty -> String = get_llvm_type_str(c, c.current_ret_type);

        if (c.current_ret_type == TYPE_GENERIC_STRUCT && res.type >= 100) {
            let cast_reg -> String = next_reg(c);
            let src_ty -> String = get_llvm_type_str(c, res.type);
            write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + res.reg + " to i8*\n");
            ret_val_reg = cast_reg;
            res.type = TYPE_GENERIC_STRUCT;

        }

        if (res.type != c.current_ret_type) {
            // Bool -> Int
            if (res.type == TYPE_BOOL) {
                if (c.current_ret_type == TYPE_INT) {
                    let cast_reg -> String = next_reg(c);
                    write(c.output_file, c.indent + cast_reg + " = zext i1 " + res.reg + " to i32\n");
                    ret_val_reg = cast_reg;
                } else {
                    throw_type_error(node.pos, "Type mismatch in return. Expected " + get_type_name(c, c.current_ret_type) + ", got Bool. ");
                }
            } else {
                throw_type_error(node.pos, "Type mismatch in return. Expected " + get_type_name(c, c.current_ret_type) + ", got " + get_type_name(c, res.type));
            }
        }

        if (is_ref_type(c, c.current_ret_type)) {
            emit_retain(c, ret_val_reg, c.current_ret_type);
        }

        cleanup_all_scopes(c);

        write(c.output_file, c.indent + "ret " + target_ty + " " + ret_val_reg + "\n");
    } else {
        if (c.current_ret_type != TYPE_VOID) {
            throw_type_error(node.pos, "Non-void function must return a value. ");
        }
        cleanup_all_scopes(c);
        write(c.output_file, c.indent + "ret void\n");
    }
    
    return void_result();
}

func compile_struct_def(c -> Compiler, node -> StructDefNode) -> CompileResult {
    let struct_name -> String = node.name_tok.value;
    let info -> StructInfo = map_get(c.struct_table, struct_name);
    if (info is null) {
        throw_type_error(node.pos, "Struct info missing for '" + struct_name + "'.");
    }

    let llvm_body -> String = "";
    let field_head -> FieldInfo = null;
    let field_curr -> FieldInfo = null;
    let curr_field_node -> ParamListNode = node.fields; 
    let idx -> Int = 0;
    
    while (curr_field_node is !null) {
        let p -> ParamNode = curr_field_node.param;
        let f_name -> String = p.name_tok.value;
        
        let f_type_id -> Int = resolve_type(c, p.type_tok);
        let f_llvm_type -> String = get_llvm_type_str(c, f_type_id);

        if (idx > 0) { llvm_body = llvm_body + ", "; }
        llvm_body = llvm_body + f_llvm_type;
        
        let new_field -> FieldInfo = FieldInfo(name=f_name, type=f_type_id, llvm_type=f_llvm_type, offset=idx, next=null);
        if (field_head is null) { field_head = new_field; field_curr = new_field; }
        else { field_curr.next = new_field; field_curr = new_field; }
        
        idx += 1;
        curr_field_node = curr_field_node.next;
    }

    info.fields = field_head;

    // %struct.Test = type { i32, i32 }
    let def_str -> String = info.llvm_name + " = type { " + llvm_body + " }\n\n";
    write(c.output_file, def_str);
    
    return void_result();
}
func compile_struct_init(c -> Compiler, s_info -> StructInfo, n_call -> CallNode) -> CompileResult {
    let size_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + size_ptr + " = getelementptr " + s_info.llvm_name + ", " + s_info.llvm_name + "* null, i64 1\n");
    let size_i64 -> String = next_reg(c);
    write(c.output_file, c.indent + size_i64 + " = ptrtoint " + s_info.llvm_name + "* " + size_ptr + " to i64\n");
    
    // add header size (+8 bytes)
    let total_size -> String = next_reg(c);
    write(c.output_file, c.indent + total_size + " = add i64 " + size_i64 + ", 8\n");

    // malloc
    let raw_mem -> String = next_reg(c);
    write(c.output_file, c.indent + raw_mem + " = call i8* @malloc(i64 " + total_size + ")\n");

    // init RC = 0
    let rc_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + rc_ptr + " = bitcast i8* " + raw_mem + " to i32*\n");
    write(c.output_file, c.indent + "store i32 0, i32* " + rc_ptr + "\n");

    // offset pointer (+8)
    let user_ptr_i8 -> String = next_reg(c);
    write(c.output_file, c.indent + user_ptr_i8 + " = getelementptr inbounds i8, i8* " + raw_mem + ", i32 8\n");

    // cast back to Struct*
    let obj_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + obj_ptr + " = bitcast i8* " + user_ptr_i8 + " to " + s_info.llvm_name + "*\n");

    let f_curr -> FieldInfo = s_info.fields;
    while (f_curr is !null) {
        let f_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_ptr + ", i32 0, i32 " + f_curr.offset + "\n");
        
        let zero_val -> String = "0";
        if (f_curr.type == TYPE_FLOAT) { zero_val = "0.0"; }
        else if (f_curr.type == TYPE_STRING || f_curr.type >= 100 || f_curr.type == TYPE_GENERIC_STRUCT || f_curr.type == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
        
        write(c.output_file, c.indent + "store " + f_curr.llvm_type + " " + zero_val + ", " + f_curr.llvm_type + "* " + f_ptr + "\n");

        f_curr = f_curr.next;
    }

    if (s_info.init_body is !null) {
        enter_scope(c);
        let this_ptr_addr -> String = next_reg(c);
        let struct_ptr_ty -> String = s_info.llvm_name + "*";
        
        write(c.output_file, c.indent + this_ptr_addr + " = alloca " + struct_ptr_ty + "\n");
        write(c.output_file, c.indent + "store " + struct_ptr_ty + " " + obj_ptr + ", " + struct_ptr_ty + "* " + this_ptr_addr + "\n");
        map_put(c.symbol_table.table, "this", SymbolInfo(reg=this_ptr_addr, type=s_info.type_id, origin_type=s_info.type_id));
        compile_node(c, s_info.init_body);
        exit_scope(c);
    }

    let arg_curr -> ArgNode = n_call.args;
    let arg_idx -> Int = 0;
    while (arg_curr is !null) {
        let val_res -> CompileResult = compile_node(c, arg_curr.val);
        let target_f -> FieldInfo = null;
        if (arg_curr.name is !null) { target_f = find_field(s_info, arg_curr.name); } 
        else { target_f = get_field_by_index(s_info, arg_idx); }

        if (target_f is !null) {
            if (val_res.type == TYPE_NULL) {/*...*/}

            if (target_f.type == TYPE_GENERIC_STRUCT && val_res.type >= 100) {
                let cast_reg -> String = next_reg(c);
                let src_ty -> String = get_llvm_type_str(c, val_res.type);

                write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to i8*\n");
                val_res.reg = cast_reg;
                val_res.type = TYPE_GENERIC_STRUCT;

            }
            
            let f_ptr -> String = next_reg(c);
            write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_ptr + ", i32 0, i32 " + target_f.offset + "\n");
            write(c.output_file, c.indent + "store " + target_f.llvm_type + " " + val_res.reg + ", " + target_f.llvm_type + "* " + f_ptr + "\n");
        }
        arg_curr = arg_curr.next;
        arg_idx += 1;
    }
    return CompileResult(reg=obj_ptr, type=s_info.type_id);
}

func compile_field_access(c -> Compiler, node -> FieldAccessNode) -> CompileResult {
    let obj_res -> CompileResult = compile_node(c, node.obj);
    let type_id -> Int = obj_res.type;
    let obj_reg -> String = obj_res.reg;

    if (type_id == TYPE_GENERIC_STRUCT) {
        let base_obj -> BaseNode = node.obj;
        if (base_obj.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = node.obj;
            let info -> SymbolInfo = find_symbol(c, v_node.name_tok.value);
            if (info is !null && info.origin_type >= 100) {
                type_id = info.origin_type;
                
                // i8* -> %struct.Test*
                let s_info_temp -> StructInfo = map_get(c.struct_id_map, "" + type_id);
                if (s_info_temp is !null) {
                    let cast_reg -> String = next_reg(c);
                    write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + obj_reg + " to " + s_info_temp.llvm_name + "*\n");
                    obj_reg = cast_reg;
                }
            }
        }
    }

    let s_info -> StructInfo = map_get(c.struct_id_map, "" + type_id);
    if (s_info is null) {
        throw_type_error(node.pos, "Cannot access field on non-struct type (or generic Struct without origin inference).");
    }
    
    let field -> FieldInfo = find_field(s_info, node.field_name);
    if (field is null) {
        throw_name_error(node.pos, "Field '" + node.field_name + "' not found in struct '" + s_info.name + "'.");
    }
    
    let f_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_reg + ", i32 0, i32 " + field.offset + "\n");
    let val_reg -> String = next_reg(c);
    write(c.output_file, c.indent + val_reg + " = load " + field.llvm_type + ", " + field.llvm_type + "* " + f_ptr + "\n");
    return CompileResult(reg=val_reg, type=field.type);
}

func compile_field_assign(c -> Compiler, node -> FieldAssignNode) -> CompileResult {
    let obj_node -> BaseNode = node.obj;
    let struct_ptr_reg -> String = "";
    let struct_type_id -> Int = 0;

    if (obj_node.type == NODE_DEREF) {
        let d_node -> DerefNode = obj_node;
        let ptr_res -> CompileResult = compile_node(c, d_node.node);
        let loaded_reg -> String = next_reg(c);
        let ptr_ty_str -> String = get_llvm_type_str(c, ptr_res.type);
        write(c.output_file, c.indent + loaded_reg + " = load " + ptr_ty_str + ", " + ptr_ty_str + "* " + ptr_res.reg + "\n");
        
        struct_ptr_reg = loaded_reg;
        let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + ptr_res.type);
        if (base_info is null) {
            throw_type_error(node.pos, "Attempt to access field on non-pointer type (expected dereference).");
        }
        struct_type_id = base_info.type;
    }
    else if (obj_node.type == NODE_VAR_ACCESS) {
        let v_node -> VarAccessNode = obj_node;
        let var_name -> String = v_node.name_tok.value;
        let info -> SymbolInfo = find_symbol(c, var_name);
        
        if (info is null) {
            throw_name_error(node.pos, "Undefined variable '" + var_name + "'.");
        }

        let loaded_reg -> String = next_reg(c);
        let var_ty_str -> String = get_llvm_type_str(c, info.type);
        write(c.output_file, c.indent + loaded_reg + " = load " + var_ty_str + ", " + var_ty_str + "* " + info.reg + "\n");

        struct_ptr_reg = loaded_reg; 
        struct_type_id = info.type;
    }
    else {
        throw_invalid_syntax(node.pos, "Invalid assignment target. Only variables and dereferences are supported for field assignment.");
    }

    let s_info -> StructInfo = map_get(c.struct_id_map, "" + struct_type_id);
    if (s_info is null) {
        throw_type_error(node.pos, "Cannot assign field to non-struct type.");
    }

    let field -> FieldInfo = find_field(s_info, node.field_name);
    if (field is null) {
        throw_name_error(node.pos, "Field '" + node.field_name + "' not found in struct '" + s_info.name + "'.");
    }

    let val_res -> CompileResult = compile_node(c, node.value);

    if (val_res.type == TYPE_NULL) {
        if (is_pointer_type(c, field.type) == false) {
            throw_type_error(node.pos, "nullptr can only be assigned to pointer types.");
        }
    } else {
        if (field.type == TYPE_FLOAT && val_res.type == TYPE_INT) {
            val_res = promote_to_float(c, val_res);
        }

        if (field.type == TYPE_GENERIC_STRUCT && val_res.type >= 100) {
            let cast_reg -> String = next_reg(c);
            let src_ty -> String = get_llvm_type_str(c, val_res.type);

            write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to i8*\n");
            val_res.reg = cast_reg;
            val_res.type = TYPE_GENERIC_STRUCT;

        }

        if (field.type != val_res.type) {
            throw_type_error(node.pos, "Type mismatch in field assignment. Expected " + get_type_name(c, field.type) + ", got " + get_type_name(c, val_res.type));
        }
    }
    
    let f_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + struct_ptr_reg + ", i32 0, i32 " + field.offset + "\n");

    let store_ty -> String = get_llvm_type_str(c, field.type);
    write(c.output_file, c.indent + "store " + store_ty + " " + val_res.reg + ", " + store_ty + "* " + f_ptr + "\n");
    
    return val_res;
}

func compile_extern_func(c -> Compiler, node -> ExternFuncNode) -> CompileResult {
    let func_name -> String = node.name_tok.value;
    let ret_type_id -> Int = resolve_type(c, node.ret_type_tok);
    
    let arg_head -> TypeListNode = null;
    let arg_curr -> TypeListNode = null;
    let curr_p -> ParamListNode = node.params;
    
    let params_str -> String = "";
    let first -> Int = 1;
    
    while (curr_p is !null) {
        let p -> ParamNode = curr_p.param;
        let p_id -> Int = resolve_type(c, p.type_tok);
        
        let t_node -> TypeListNode = TypeListNode(type=p_id, next=null);
        if (arg_head is null) { arg_head = t_node; arg_curr = t_node; }
        else { arg_curr.next = t_node; arg_curr = t_node; }
        
        if (first == 0) { params_str = params_str + ", "; }
        params_str = params_str + get_llvm_type_str(c, p_id);
        first = 0;
        
        curr_p = curr_p.next;
    }

    if node.is_varargs {
        if (first == 0) { params_str = params_str + ", "; }
        params_str = params_str + "...";
    }
    
    map_put(c.func_table, func_name, FuncInfo(ret_type=ret_type_id, arg_types=arg_head, is_varargs=node.is_varargs));
    if (map_get(c.declared_externs, func_name) is null) {
        let ret_llvm -> String = get_llvm_type_str(c, ret_type_id);
        write(c.output_file, "declare " + ret_llvm + " @" + func_name + "(" + params_str + ")\n");

        map_put(c.declared_externs, func_name, StringConstant(id=0, value="", next=null)); 
    }
    return void_result();
}

func compile_extern_block(c -> Compiler, node -> ExternBlockNode) -> CompileResult {
    let curr -> StmtListNode = node.funcs;
    while (curr is !null) {
        compile_extern_func(c, curr.stmt);
        curr = curr.next;
    }
    return void_result();
}

func compile_vector_lit(c -> Compiler, node -> VectorLitNode) -> CompileResult {
    let count -> Int = node.count;
    let elem_type_id -> Int = TYPE_INT; 
    if (node.elements is !null) {
        let first_arg -> ArgNode = node.elements;
        let first_res -> CompileResult = compile_node(c, first_arg.val);
        elem_type_id = first_res.type;
    }
    
    let vec_type_id -> Int = get_vector_type_id(c, elem_type_id);
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type_id);
    let struct_name -> String = "{ i64, i64, " + elem_ty_str + "* }";
    
    // malloc vector struct header
    let struct_size_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + struct_size_ptr + " = getelementptr " + struct_name + ", " + struct_name + "* null, i32 1\n");
    let struct_size -> String = next_reg(c);
    write(c.output_file, c.indent + struct_size + " = ptrtoint " + struct_name + "* " + struct_size_ptr + " to i64\n");
    let raw_struct -> String = next_reg(c);
    write(c.output_file, c.indent + raw_struct + " = call i8* @malloc(i64 " + struct_size + ")\n");
    let vec_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + vec_ptr + " = bitcast i8* " + raw_struct + " to " + struct_name + "*\n");
    
    // malloc data array
    let arr_size_ptr -> String = next_reg(c);
    // size = count * sizeof(T)
    write(c.output_file, c.indent + arr_size_ptr + " = getelementptr " + elem_ty_str + ", " + elem_ty_str + "* null, i64 " + count + "\n");
    let arr_bytes -> String = next_reg(c);
    write(c.output_file, c.indent + arr_bytes + " = ptrtoint " + elem_ty_str + "* " + arr_size_ptr + " to i64\n");
    
    let raw_data -> String = next_reg(c);
    // TODO: Handle count == 0 case carefully
    write(c.output_file, c.indent + raw_data + " = call i8* @malloc(i64 " + arr_bytes + ")\n");
    let data_ptr -> String = next_reg(c);
    write(c.output_file, c.indent + data_ptr + " = bitcast i8* " + raw_data + " to " + elem_ty_str + "*\n");

    // struct { i64 size, i64 capacity, T* data }
    // size
    let size_ptr -> String = next_reg(c); 
    write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 0\n");
    write(c.output_file, c.indent + "store i64 " + count + ", i64* " + size_ptr + "\n");
    
    // capacity
    let cap_ptr -> String = next_reg(c); 
    write(c.output_file, c.indent + cap_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 1\n");
    write(c.output_file, c.indent + "store i64 " + count + ", i64* " + cap_ptr + "\n"); 
    
    // data pointer
    let data_field_ptr -> String = next_reg(c); 
    write(c.output_file, c.indent + data_field_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 2\n");
    write(c.output_file, c.indent + "store " + elem_ty_str + "* " + data_ptr + ", " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    let curr -> ArgNode = node.elements;
    let idx -> Int = 0;
    while (curr is !null) {
        let val_res -> CompileResult = compile_node(c, curr.val);

        if (val_res.type != elem_type_id) {
            if (elem_type_id == TYPE_INT && val_res.type == TYPE_BYTE) {
                val_res = promote_to_int(c, val_res);
            } else {
                throw_type_error(node.pos, "Mixed types in vector literal. Expected " + get_type_name(c, elem_type_id) + ", got " + get_type_name(c, val_res.type) + ".");
            }
        }
        
        let slot_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx + "\n");

        if (is_ref_type(c, elem_type_id)) {
            emit_retain(c, val_res.reg, elem_type_id);
        }
        
        write(c.output_file, c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + slot_ptr + "\n");
        
        idx = idx + 1;
        curr = curr.next;
    }
    
    return CompileResult(reg=vec_ptr, type=vec_type_id);
}

func compile_length_method(c -> Compiler, obj_node -> Struct, call_node -> CallNode) -> CompileResult {
    if (call_node.args is !null) {
        throw_type_error(call_node.pos, "Method 'length' does not accept arguments.");
    }

    let obj_res -> CompileResult = compile_node(c, obj_node);
    let type_id -> Int = obj_res.type;

    // String.length()
    if (type_id == TYPE_STRING) {
        let len_reg -> String = next_reg(c);
        // strlen returns i32 in our declaration
        write(c.output_file, c.indent + len_reg + " = call i32 @strlen(i8* " + obj_res.reg + ")\n");
        return CompileResult(reg=len_reg, type=TYPE_INT);
    }

    // Vector.length()
    let is_vec -> Bool = false;
    if (type_id >= 100) {
        let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + type_id);
        if (v_info is !null) { is_vec = true; }
    }

    if is_vec {
        let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + type_id);
        let elem_ty_str -> String = get_llvm_type_str(c, v_info.type);
        let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";
        
        let size_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + obj_res.reg + ", i32 0, i32 0\n");
        
        let size_val -> String = next_reg(c);
        write(c.output_file, c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");

        let trunc_reg -> String = next_reg(c);
        write(c.output_file, c.indent + trunc_reg + " = trunc i64 " + size_val + " to i32\n");
        
        return CompileResult(reg=trunc_reg, type=TYPE_INT);
    }

    throw_type_error(call_node.pos, "Method 'length' is not defined for type " + get_type_name(c, type_id));
    return void_result();
}

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
            throw_index_error(pos, "Negative index " + val_str + " is not supported yet.");
        }

        let base_target -> BaseNode = target_node;
        if (base_target.type == NODE_VECTOR_LIT) {
            let vec_node -> VectorLitNode = target_node;
            let count -> Int = vec_node.count;
            
            if (idx_val >= count) {
                throw_index_error(pos, "Index " + val_str + " is out of bounds for vector of size " + count + ".");
            }
        }

        if (base_target.type == NODE_STRING) {
            let str_node -> StringNode = target_node;
            let s_len -> Int = str_node.tok.value.length();

            if (idx_val >= s_len) {
                throw_index_error(pos, "Index " + val_str + " is out of bounds for string of length " + s_len + ".");
            }
        }
    }
}
func compile_index_access(c -> Compiler, node -> IndexAccessNode) -> CompileResult {
    check_out_index(c, node.target, node.index_node, node.pos);
    let target_res -> CompileResult = compile_node(c, node.target);
    let index_res -> CompileResult = compile_node(c, node.index_node);
    
    // idx type
    if (index_res.type != TYPE_INT) {
        throw_type_error(node.pos, "Index must be an Integer.");
    }
    
    // Vector access
    let is_vec -> Bool = false;
    if (target_res.type >= 100) {
        if (map_get(c.vector_base_map, "" + target_res.type) is !null) { is_vec = true; }
    }
    
    if is_vec {
        let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + target_res.type);
        let elem_type -> Int = v_info.type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
        
        // { i64, i64, T* }
        let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

        emit_vector_bounds_check(c, target_res.reg, index_res.reg, struct_ty, node.pos);

        let data_field_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + target_res.reg + ", i32 0, i32 2\n");
        
        let data_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");

        let slot_ptr -> String = next_reg(c);
        
        let idx_i64 -> String = next_reg(c);
        write(c.output_file, c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        
        write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx_i64 + "\n");
        
        let val_reg -> String = next_reg(c);
        write(c.output_file, c.indent + val_reg + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");
        
        return CompileResult(reg=val_reg, type=elem_type);
    }
    
    // str access(-> Byte)
    if (target_res.type == TYPE_STRING) {
        let idx_i64 -> String = next_reg(c);
        write(c.output_file, c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        
        let char_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + char_ptr + " = getelementptr inbounds i8, i8* " + target_res.reg + ", i64 " + idx_i64 + "\n");
        
        let char_val -> String = next_reg(c);
        write(c.output_file, c.indent + char_val + " = load i8, i8* " + char_ptr + "\n");
        
        return CompileResult(reg=char_val, type=TYPE_BYTE);
    }
    
    throw_type_error(node.pos, "Type " + get_type_name(c, target_res.type) + " is not indexable.");
    return void_result();
}
func compile_index_assign(c -> Compiler, node -> IndexAssignNode) -> CompileResult {
    check_out_index(c, node.target, node.index_node, node.pos);
    let target_res -> CompileResult = compile_node(c, node.target);
    let index_res -> CompileResult = compile_node(c, node.index_node);
    let val_res -> CompileResult = compile_node(c, node.value);
    
    if (index_res.type != TYPE_INT) {
        throw_type_error(node.pos, "Index must be an Integer.");
    }
    
    let is_vec -> Bool = false;
    if (target_res.type >= 100) {
        if (map_get(c.vector_base_map, "" + target_res.type) is !null) { is_vec = true; }
    }

    if (target_res.type == TYPE_STRING) {
        throw_type_error(node.pos, "Strings are immutable. Cannot assign to index.");
    }

    if is_vec {
        let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + target_res.type);
        let elem_type -> Int = v_info.type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);

        if (val_res.type != elem_type) {
            if (elem_type == TYPE_INT && val_res.type == TYPE_BYTE) {
                val_res = promote_to_int(c, val_res);
            } else {
                throw_type_error(node.pos, "Type mismatch. Vector expects " + get_type_name(c, elem_type) + ", got " + get_type_name(c, val_res.type));
            }
        }

        let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

        emit_vector_bounds_check(c, target_res.reg, index_res.reg, struct_ty, node.pos);

        let data_field_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + target_res.reg + ", i32 0, i32 2\n");
        let data_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");
        
        let idx_i64 -> String = next_reg(c);
        write(c.output_file, c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        let slot_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx_i64 + "\n");

        if (is_ref_type(c, elem_type)) {
            emit_retain(c, val_res.reg, elem_type);
    
            let old_val -> String = next_reg(c);
            write(c.output_file, c.indent + old_val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");

            emit_release(c, old_val, elem_type);
        }

        write(c.output_file, c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + slot_ptr + "\n");
        
        return val_res;
    }
    
    throw_type_error(node.pos, "Type " + get_type_name(c, target_res.type) + " does not support index assignment.");
    return void_result();
}

func compile_binop(c -> Compiler, node -> BinOpNode) -> CompileResult {
    let left -> CompileResult = compile_node(c, node.left);
    let op_type -> Int = node.op_tok.type; 

    if (op_type == TOK_AND || op_type == TOK_OR) {
        if (left.type != TYPE_BOOL) {
            throw_type_error(node.pos, "Logic operators '&&' and '||' require Bool operands. ");
        }
        let label_rhs -> String = next_label(c);
        let label_merge -> String = next_label(c);
        let res_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + res_ptr + " = alloca i1\n");
        write(c.output_file, c.indent + "store i1 " + left.reg + ", i1* " + res_ptr + "\n");
        
        if (op_type == TOK_AND) {
            write(c.output_file, c.indent + "br i1 " + left.reg + ", label %" + label_rhs + ", label %" + label_merge + "\n");
        } else {
            write(c.output_file, c.indent + "br i1 " + left.reg + ", label %" + label_merge + ", label %" + label_rhs + "\n");
        }

        write(c.output_file, "\n" + label_rhs + ":\n");
        let right_res -> CompileResult = compile_node(c, node.right);
        if (right_res.type != TYPE_BOOL) { throw_type_error(node.pos, "Right operand must be Bool."); }
        write(c.output_file, c.indent + "store i1 " + right_res.reg + ", i1* " + res_ptr + "\n");
        write(c.output_file, c.indent + "br label %" + label_merge + "\n");

        write(c.output_file, "\n" + label_merge + ":\n");
        let final_reg -> String = next_reg(c);
        write(c.output_file, c.indent + final_reg + " = load i1, i1* " + res_ptr + "\n");
        return CompileResult(reg=final_reg, type=TYPE_BOOL);
    }

    let right -> CompileResult = compile_node(c, node.right);
    if (op_type == TOK_EE || op_type == TOK_NE) {
        if (left.type == TYPE_NULL || left.type == TYPE_NULLPTR ||
            right.type == TYPE_NULL || right.type == TYPE_NULLPTR) {
            throw_type_error(node.pos, "Invalid operator. Do not use '==' or '!=' with null/nullptr. Use 'is' or 'is !'.");
        }
    }

    // String
    if (left.type == TYPE_STRING || right.type == TYPE_STRING) {
        if (op_type == TOK_PLUS) {
            // convert type
            if (left.type != TYPE_STRING) {
                left = convert_to_string(c, left);
            }
            if (right.type != TYPE_STRING) {
                right = convert_to_string(c, right);
            }

            // get str length
            let len1 -> String = next_reg(c);
            write(c.output_file, c.indent + len1 + " = call i32 @strlen(i8* " + left.reg + ")\n");
            
            let len2 -> String = next_reg(c);
            write(c.output_file, c.indent + len2 + " = call i32 @strlen(i8* " + right.reg + ")\n");

            // upgrade to i64
            let len1_64 -> String = next_reg(c);
            write(c.output_file, c.indent + len1_64 + " = zext i32 " + len1 + " to i64\n");
            let len2_64 -> String = next_reg(c);
            write(c.output_file, c.indent + len2_64 + " = zext i32 " + len2 + " to i64\n");
            
            // total length
            let sum_len -> String = next_reg(c);
            write(c.output_file, c.indent + sum_len + " = add i64 " + len1_64 + ", " + len2_64 + "\n");
            
            // for \0
            let total_size -> String = next_reg(c);
            write(c.output_file, c.indent + total_size + " = add i64 " + sum_len + ", 1\n");
            
            // malloc(total_size)
            let new_str_ptr -> String = next_reg(c);
            write(c.output_file, c.indent + new_str_ptr + " = call i8* @malloc(i64 " + total_size + ")\n");
            
            // strcpy(new_ptr, left) -> null
            let ign1 -> String = next_reg(c);
            write(c.output_file, c.indent + ign1 + " = call i8* @strcpy(i8* " + new_str_ptr + ", i8* " + left.reg + ")\n");
            
            // strcat(new_ptr, right) -> null
            let ign2 -> String = next_reg(c);
            write(c.output_file, c.indent + ign2 + " = call i8* @strcat(i8* " + new_str_ptr + ", i8* " + right.reg + ")\n");
            
            return CompileResult(reg=new_str_ptr, type=TYPE_STRING);
        }

        if (left.type != right.type) {
            throw_type_error(node.pos, "Cannot operate on String with other types.");
        }


        let allowed -> Int = 0;
        if (op_type == TOK_EE) { allowed = 1; }
        if (op_type == TOK_NE) { allowed = 1; }
        
        if (allowed == 0) {
            throw_type_error(node.pos, "Arithmetic operations on Strings are not supported (except +).");
        }

        let cmp_val -> String = next_reg(c);
        write(c.output_file, c.indent + cmp_val + " = call i32 @strcmp(i8* " + left.reg + ", i8* " + right.reg + ")\n");

        let res_reg -> String = next_reg(c);
        let op_code -> String = "icmp eq";

        if (op_type == TOK_NE) { op_code = "icmp ne"; }

        write(c.output_file, c.indent + res_reg + " = " + op_code + " i32 " + cmp_val + ", 0\n");
        
        return CompileResult(reg=res_reg, type=TYPE_BOOL);
    }

    if (op_type == TOK_POW) {
        left = promote_to_float(c, left);
        right = promote_to_float(c, right);
        let res_reg -> String = next_reg(c);
        write(c.output_file, c.indent + res_reg + " = call double @llvm.pow.f64(double " + left.reg + ", double " + right.reg + ")\n");
        return CompileResult(reg=res_reg, type=TYPE_FLOAT);
    }

    let is_cmp -> Int = 0;
    if (op_type == TOK_EE) { is_cmp = 1; }
    if (op_type == TOK_NE) { is_cmp = 1; }
    if (op_type == TOK_GT) { is_cmp = 1; }
    if (op_type == TOK_LT) { is_cmp = 1; }
    if (op_type == TOK_GTE) { is_cmp = 1; }
    if (op_type == TOK_LTE) { is_cmp = 1; }

    if (is_cmp == 1) {
        if (left.type >= 100 || right.type >= 100 || left.type == TYPE_NULL || right.type == TYPE_NULL) {
            throw_type_error(node.pos, "Pointer comparison is not supported yet. Please use loop counters.");
        }
        if (left.type == TYPE_BOOL || right.type == TYPE_BOOL) {
            if (left.type != right.type) { throw_type_error(node.pos, "Cannot compare Bool with other types."); }
            if (op_type != TOK_EE && op_type != TOK_NE) { throw_type_error(node.pos, "Invalid Bool comparison."); }
            let res_reg -> String = next_reg(c);
            let op_code -> String = "icmp eq";
            if (op_type == TOK_NE) { op_code = "icmp ne"; }
            write(c.output_file, c.indent + res_reg + " = " + op_code + " i1 " + left.reg + ", " + right.reg + "\n");
            return CompileResult(reg=res_reg, type=TYPE_BOOL);
        }

        let cmp_mode -> Int = TYPE_BYTE;
        if (left.type == TYPE_FLOAT || right.type == TYPE_FLOAT) {
            cmp_mode = TYPE_FLOAT;
            left = promote_to_float(c, left);
            right = promote_to_float(c, right);
        } else if (left.type == TYPE_LONG || right.type == TYPE_LONG) {
            cmp_mode = TYPE_LONG;
            left = promote_to_long(c, left);
            right = promote_to_long(c, right);
        } else if (left.type == TYPE_INT || right.type == TYPE_INT) {
            cmp_mode = TYPE_INT;
            if (left.type == TYPE_BYTE) { left = promote_to_int(c, left); }
            if (right.type == TYPE_BYTE) { right = promote_to_int(c, right); }
        }

        let res_reg -> String = next_reg(c);
        let op_code -> String = "";
        let type_str -> String = "i8";
        
        if (cmp_mode == TYPE_FLOAT) {
            type_str = "double";
            if (op_type == TOK_EE) { op_code = "fcmp oeq"; }
            else if (op_type == TOK_NE) { op_code = "fcmp one"; }
            else if (op_type == TOK_GT) { op_code = "fcmp ogt"; }
            else if (op_type == TOK_LT) { op_code = "fcmp olt"; }
            else if (op_type == TOK_GTE) { op_code = "fcmp oge"; }
            else if (op_type == TOK_LTE) { op_code = "fcmp ole"; }
        } else {
            // Int, Long, Byte
            let suffix -> String = "u"; // default unsigned for Byte
            
            if (cmp_mode == TYPE_LONG) { 
                type_str = "i64"; 
                suffix = "s"; // signed for Long
            }
            else if (cmp_mode == TYPE_INT) { 
                type_str = "i32"; 
                suffix = "s"; // signed for Int
            }
            
            if (op_type == TOK_EE) { op_code = "icmp eq"; }
            else if (op_type == TOK_NE) { op_code = "icmp ne"; }
            else if (op_type == TOK_GT) { op_code = "icmp " + suffix + "gt"; }
            else if (op_type == TOK_LT) { op_code = "icmp " + suffix + "lt"; }
            else if (op_type == TOK_GTE) { op_code = "icmp " + suffix + "ge"; }
            else if (op_type == TOK_LTE) { op_code = "icmp " + suffix + "le"; }
        }
        
        write(c.output_file, c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n");
        return CompileResult(reg=res_reg, type=TYPE_BOOL);
    }

    if (left.type == TYPE_BOOL || right.type == TYPE_BOOL) {
        throw_type_error(node.pos, "Arithmetic operators cannot be used on Bool. ");
    }

    let target_type -> Int = TYPE_BYTE;
    
    if (left.type == TYPE_FLOAT || right.type == TYPE_FLOAT) {
        target_type = TYPE_FLOAT;
        left = promote_to_float(c, left);
        right = promote_to_float(c, right);
    } else if (left.type == TYPE_LONG || right.type == TYPE_LONG) {
        target_type = TYPE_LONG;
        left = promote_to_long(c, left);
        right = promote_to_long(c, right);
    } else if (left.type == TYPE_INT || right.type == TYPE_INT) {
        target_type = TYPE_INT;
        if (left.type == TYPE_BYTE) { left = promote_to_int(c, left); }
        if (right.type == TYPE_BYTE) { right = promote_to_int(c, right); }
    }

    let res_reg -> String = next_reg(c);
    let op_code -> String = "";
    let type_str -> String = "i8";
    
    if (target_type == TYPE_FLOAT) {
        type_str = "double";
        if (op_type == TOK_PLUS)  { op_code = "fadd"; }
        else if (op_type == TOK_SUB)   { op_code = "fsub"; }
        else if (op_type == TOK_MUL)   { op_code = "fmul"; }
        else if (op_type == TOK_DIV)   { op_code = "fdiv"; }
        else if (op_type == TOK_MOD)   { op_code = "frem"; } 
    } else {
        if (target_type == TYPE_LONG) { type_str = "i64"; }
        else if (target_type == TYPE_INT) { type_str = "i32"; }
        
        if (op_type == TOK_PLUS)  { op_code = "add"; }
        else if (op_type == TOK_SUB)   { op_code = "sub"; }
        else if (op_type == TOK_MUL)   { op_code = "mul"; }

        else if (op_type == TOK_DIV) { 
            if (target_type == TYPE_BYTE) { op_code = "udiv"; } else { op_code = "sdiv"; }
        }
        else if (op_type == TOK_MOD) { 
            if (target_type == TYPE_BYTE) { op_code = "urem"; } else { op_code = "srem"; }
        } 
    }

    write(c.output_file, c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n");
    return CompileResult(reg=res_reg, type=target_type);
}

func compile_node(c -> Compiler, node -> Struct) -> CompileResult {
    let base -> BaseNode = node;

    if (base.type == NODE_BLOCK) {
        return compile_block(c, node);
    }

    if (base.type == NODE_STRING) {
        let n -> StringNode = node;
        let val -> String = n.tok.value;
        let id -> Int = c.str_count;
        c.str_count = c.str_count + 1;

        let new_str -> StringConstant = StringConstant(id=id, value=val, next=c.string_list);
        c.string_list = new_str;
        let len -> Int = val.length() + 1; // +1 for \0

        let total_size -> Int = len + 8;
        let raw_mem -> String = next_reg(c);
        write(c.output_file, c.indent + raw_mem + " = call i8* @malloc(i64 " + total_size + ")\n");

        // init RC = 0 (Header)
        // casting raw_mem to i32*
        let rc_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + rc_ptr + " = bitcast i8* " + raw_mem + " to i32*\n");
        // Store 0. Variable assignment will 'retain' it to 1 later.
        write(c.output_file, c.indent + "store i32 0, i32* " + rc_ptr + "\n");

        // get pointer (Offset +8)
        let user_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + user_ptr + " = getelementptr inbounds i8, i8* " + raw_mem + ", i32 8\n");

        // strcpy(ptr, constant_ptr)
        let const_ptr -> String = next_reg(c);
        write(c.output_file, c.indent + const_ptr + " = getelementptr inbounds [" + len + " x i8], [" + len + " x i8]* @.str." + id + ", i32 0, i32 0\n");
        write(c.output_file, c.indent + "call i8* @strcpy(i8* " + user_ptr + ", i8* " + const_ptr + ")\n");

        return CompileResult(reg=user_ptr, type=TYPE_STRING);
    }

    if (base.type == NODE_VAR_DECL) { return compile_var_decl(c, node); }
    if (base.type == NODE_IF)       { return compile_if(c, node); }
    if (base.type == NODE_WHILE)    { return compile_while(c, node); }
    if (base.type == NODE_FOR)      { return compile_for(c, node); }
    if (base.type == NODE_BINOP)    { return compile_binop(c, node); }
    if (base.type == NODE_FUNC_DEF) { return compile_func_def(c, node); }
    if (base.type == NODE_RETURN)   { return compile_return(c, node); }
    if (base.type == NODE_STRUCT_DEF) { return compile_struct_def(c, node); }
    if (base.type == NODE_FIELD_ACCESS) { return compile_field_access(c, node); }
    if (base.type == NODE_FIELD_ASSIGN) { return compile_field_assign(c, node); }
    if (base.type == NODE_EXTERN_BLOCK) { return compile_extern_block(c, node); }
    if (base.type == NODE_VECTOR_LIT) { return compile_vector_lit(c, node); }
    if (base.type == NODE_INDEX_ACCESS) { return compile_index_access(c, node); }
    if (base.type == NODE_INDEX_ASSIGN) { return compile_index_assign(c, node); }

    // ptr
    if (base.type == NODE_PTR_ASSIGN) {return compile_ptr_assign(c, node);}
    // ref
    if (base.type == NODE_REF) {
        let r_node -> RefNode = node;
        let target -> BaseNode = r_node.node;
        
        if (target.type != NODE_VAR_ACCESS) {
            throw_invalid_syntax(r_node.pos, "Cannot take ref of r-value.");
        }
        
        let v -> VarAccessNode = r_node.node;
        let name -> String = v.name_tok.value;

        let info -> SymbolInfo = find_symbol(c, name);
        
        if (info is null) {
            let f_info -> FuncInfo = map_get(c.func_table, name);
            if (f_info is !null) {
                let specific_type_id -> Int = get_func_type_id(c, f_info.ret_type);
                let sig -> String = get_func_sig_str(c, f_info);
                let func_ptr -> String = "@" + name;
                
                let cast_reg -> String = next_reg(c);
                write(c.output_file, c.indent + cast_reg + " = bitcast " + sig + " " + func_ptr + " to i8*\n");
                
                return CompileResult(reg=cast_reg, type=specific_type_id);
            }
            
            throw_name_error(r_node.pos, "Unknown variable or function '" + name + "'.");
        }

        let ptr_id -> Int = get_ptr_type_id(c, info.type);
        return CompileResult(reg=info.reg, type=ptr_id);
    }
    // deref
    if (base.type == NODE_DEREF) {
        let d_node -> DerefNode = node;
        let res -> CompileResult = compile_node(c, d_node.node);
        
        let i -> Int = 0;
        let curr_reg -> String = res.reg;
        let curr_type -> Int = res.type;
        
        while (i < d_node.level) {
            if (curr_type == TYPE_NULL) {
                throw_null_dereference_error(d_node.pos, "Cannot dereference 'nullptr'. ");
            }
            let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + curr_type);
            if (base_info is null) { throw_type_error(d_node.pos, "Attempt to dereference non-pointer. "); }
            
            let next_type -> Int = base_info.type;
            let ty_str -> String = get_llvm_type_str(c, next_type);
            let next_reg -> String = next_reg(c);
            
            write(c.output_file, c.indent + next_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_reg + "\n");
            
            curr_reg = next_reg;
            curr_type = next_type;
            i += 1;
        }
        return CompileResult(reg=curr_reg, type=curr_type);
    }
    
    if (base.type == NODE_NULLPTR) {
        return CompileResult(reg="null", type=TYPE_NULLPTR);
    }

    if (base.type == NODE_NULL) {
        return CompileResult(reg="null", type=TYPE_NULL);
    }

    if (base.type == NODE_IS || base.type == NODE_IS_NOT) {
        let b_node -> BinOpNode = node;
        let lhs_res -> CompileResult = compile_node(c, b_node.left);
        let rhs_res -> CompileResult = compile_node(c, b_node.right);

        let l_reg -> String = lhs_res.reg;
        let r_reg -> String = rhs_res.reg;
        
        // convert to i8* for address comparison
        if (lhs_res.type != TYPE_STRING && lhs_res.type != TYPE_NULL && lhs_res.type != TYPE_NULLPTR) {
            let cast_l -> String = next_reg(c);
            let ty_l -> String = get_llvm_type_str(c, lhs_res.type);
            write(c.output_file, c.indent + cast_l + " = bitcast " + ty_l + " " + l_reg + " to i8*\n");
            l_reg = cast_l;
        }
        if (rhs_res.type != TYPE_STRING && rhs_res.type != TYPE_NULL && rhs_res.type != TYPE_NULLPTR) {
            let cast_r -> String = next_reg(c);
            let ty_r -> String = get_llvm_type_str(c, rhs_res.type);
            write(c.output_file, c.indent + cast_r + " = bitcast " + ty_r + " " + r_reg + " to i8*\n");
            r_reg = cast_r;
        }

        let cmp_reg -> String = next_reg(c);
        let cond -> String = "eq";
        if (base.type == 33) { cond = "ne"; } // NODE_IS_NOT
        
        write(c.output_file, c.indent + cmp_reg + " = icmp " + cond + " i8* " + l_reg + ", " + r_reg + "\n");
        return CompileResult(reg=cmp_reg, type=TYPE_BOOL);
    }

    if (base.type == NODE_INT) {
        let n -> IntNode = node;
        return CompileResult(reg=n.tok.value, type=TYPE_INT); 
    }
    if (base.type == NODE_FLOAT) {
        let n -> FloatNode = node;
        return CompileResult(reg=n.tok.value, type=TYPE_FLOAT); 
    }
    
    if (base.type == NODE_BOOL) {
        let b -> BooleanNode = node;
        let val_str -> String = "0";
        if (b.value == 1) { val_str = "1"; }
        return CompileResult(reg=val_str, type=TYPE_BOOL);
    }

    if (base.type == NODE_VAR_ACCESS) {
        let v -> VarAccessNode = node;
        let var_name -> String = v.name_tok.value; 
        
        let info -> SymbolInfo = find_symbol(c, var_name);
        if (info is null) {
            let f_info -> FuncInfo = map_get(c.func_table, var_name);
            if (f_info is !null) {
                let specific_type_id -> Int = get_func_type_id(c, f_info.ret_type);
                
                let sig -> String = get_func_sig_str(c, f_info);
                let func_ptr -> String = "@" + var_name;
                
                let cast_reg -> String = next_reg(c);
                write(c.output_file, c.indent + cast_reg + " = bitcast " + sig + " " + func_ptr + " to i8*\n");
                
                return CompileResult(reg=cast_reg, type=specific_type_id);
            }

            throw_name_error(v.pos, "Undefined variable or function '" + var_name + "'. ");
        }
        
        let val_reg -> String = next_reg(c);
        let llvm_ty_str -> String = get_llvm_type_str(c, info.type);
        if (llvm_ty_str == "") {
            throw_type_error(v.pos, "Variable '" + var_name + "' has invalid internal type ID. ");
        }
        
        write(c.output_file, c.indent + val_reg + " = load " + llvm_ty_str + ", " + llvm_ty_str + "* " + info.reg + "\n");
        return CompileResult(reg=val_reg, type=info.type);
    }

    if (base.type == NODE_VAR_ASSIGN) {
        return compile_var_assign(c, node);
    }

    if (base.type == NODE_CALL) {
        let n_call -> CallNode = node;
        let callee -> BaseNode = n_call.callee;

        if (callee.type == NODE_FIELD_ACCESS) {
            let f_acc -> FieldAccessNode = callee;
            if (f_acc.field_name == "length") {
                return compile_length_method(c, f_acc.obj, n_call);
            }
        }

        let func_name -> String = "";
        let is_direct -> Int = 0;

        if (callee.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = n_call.callee;
            func_name = v_node.name_tok.value;
            
            if (func_name == "pseudo_print") {
                is_direct = 1;
            } else {
                let s_check -> StructInfo = map_get(c.struct_table, func_name);
                if (s_check is !null) {
                    is_direct = 1;
                } else {
                    let f_check -> FuncInfo = map_get(c.func_table, func_name);
                    let v_check -> SymbolInfo = find_symbol(c, func_name);
                    
                    if (f_check is !null && v_check is null) {
                        is_direct = 1;
                    }
                }
            }
        } else {
            is_direct = 0;
        }

        if (is_direct == 1) {
            if (func_name == "pseudo_print") {
                let args -> ArgNode = n_call.args;
                if (args is null) { throw_type_error(n_call.pos, "'pseudo_print' requires exactly 1 argument. "); }
                if (args.next is !null) { throw_type_error(n_call.pos, "'pseudo_print' currently supports only 1 argument. "); }

                let arg_res -> CompileResult = compile_node(c, args.val);
                let fmt -> String = "%d\\0A\\00";
                let ty_str -> String = "i32";
                
                if (arg_res.type == TYPE_FLOAT) {
                    fmt = "%f\\0A\\00"; ty_str = "double";
                } else if (arg_res.type == TYPE_BOOL) {
                    fmt = "%d\\0A\\00"; ty_str = "i1";
                    let ext_reg -> String = next_reg(c);
                    write(c.output_file, c.indent + ext_reg + " = zext i1 " + arg_res.reg + " to i32\n");
                    arg_res.reg = ext_reg; arg_res.type = TYPE_INT; ty_str = "i32";
                } else if (arg_res.type == TYPE_STRING) {
                    fmt = "%s\\0A\\00"; ty_str = "i8*";
                } else if (arg_res.type == TYPE_INT) {
                    // pass
                } else if (arg_res.type == TYPE_LONG) {
                    fmt = "%lld\\0A\\00"; ty_str = "i64";
                } else if (arg_res.type == TYPE_BYTE) {
                    fmt = "%d\\0A\\00";
                    ty_str = "i32";
                    let ext -> String = next_reg(c);
                    write(c.output_file, c.indent + ext + " = zext i8 " + arg_res.reg + " to i32\n");
                    arg_res.reg = ext;
                    // use_fmt = "@.fmt_int";
                } else {
                    throw_type_error(n_call.pos, "Cannot print type " + get_llvm_type_str(c, arg_res.type));
                }

                let use_fmt -> String = "@.fmt_int";
                if (ty_str == "double") { use_fmt = "@.fmt_float"; }
                if (ty_str == "i8*") { use_fmt = "@.fmt_str"; }
                if (ty_str == "i64") { use_fmt = "@.fmt_long"; }

                let ptr_reg -> String = next_reg(c);
                write(c.output_file, c.indent + ptr_reg + " = getelementptr [4 x i8], [4 x i8]* " + use_fmt + ", i32 0, i32 0\n");
                write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + ptr_reg + ", " + ty_str + " " + arg_res.reg + ")\n");
                return void_result();
            }

            let s_info -> StructInfo = map_get(c.struct_table, func_name);
            if (s_info is !null) {
                return compile_struct_init(c, s_info, n_call);
            }

            let func_info -> FuncInfo = map_get(c.func_table, func_name);

            let args_str -> String = "";
            let arg_node_curr -> ArgNode = n_call.args;
            let type_node_curr -> TypeListNode = func_info.arg_types;
            let is_first -> Bool = true;

            while (arg_node_curr is !null) {
                if (type_node_curr is null) { 
                    if (func_info.is_varargs == 0) {
                        throw_type_error(n_call.pos, "Too many arguments."); 
                    }
                    let arg_val -> CompileResult = compile_node(c, arg_node_curr.val);

                    // Byte -> Int
                    if (arg_val.type == TYPE_BYTE) {
                        arg_val = promote_to_int(c, arg_val);
                    }
                    // Bool -> Int
                    if (arg_val.type == TYPE_BOOL) {
                        let zext_reg -> String = next_reg(c);
                        write(c.output_file, c.indent + zext_reg + " = zext i1 " + arg_val.reg + " to i32\n");
                        arg_val = CompileResult(reg=zext_reg, type=TYPE_INT);
                    }
                    // Float -> double(Float)
                    
                    if !is_first { args_str = args_str + ", "; }
                    let ty_str -> String = get_llvm_type_str(c, arg_val.type);
                    args_str = args_str + ty_str + " " + arg_val.reg;

                    is_first = false;
                    
                    arg_node_curr = arg_node_curr.next;
                    continue;
                }
                
                let arg_val -> CompileResult = compile_node(c, arg_node_curr.val);
                let expected_type -> Int = type_node_curr.type;
                
                if (expected_type == TYPE_FLOAT && arg_val.type == TYPE_INT) {
                    arg_val = promote_to_float(c, arg_val);
                }

                if (expected_type == TYPE_GENERIC_STRUCT && arg_val.type >= 100) {
                    let cast_reg -> String = next_reg(c);
                    let src_ty -> String = get_llvm_type_str(c, arg_val.type);
                    write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + arg_val.reg + " to i8*\n");
                    arg_val = CompileResult(reg=cast_reg, type=TYPE_GENERIC_STRUCT);
                }

                if (expected_type == TYPE_GENERIC_FUNCTION && arg_val.type >= 100) {
                    
                }

                if (arg_val.type != expected_type) { throw_type_error(n_call.pos, "Argument type mismatch."); }

                let ty_str -> String = get_llvm_type_str(c, arg_val.type);
                if !is_first { args_str = args_str + ", "; }
                args_str = args_str + ty_str + " " + arg_val.reg;
                is_first = false;
                
                arg_node_curr = arg_node_curr.next;
                type_node_curr = type_node_curr.next;
            }
            
            if (type_node_curr is !null) { throw_type_error(n_call.pos, "Too few arguments."); }

            let ret_type_str -> String = get_llvm_type_str(c, func_info.ret_type);
            let call_res_reg -> String = "";
            
            if (func_info.ret_type == TYPE_VOID) {
                write(c.output_file, c.indent + "call void @" + func_name + "(" + args_str + ")\n");
                return void_result();
            } else {
                call_res_reg = next_reg(c);
                write(c.output_file, c.indent + call_res_reg + " = call " + ret_type_str + " @" + func_name + "(" + args_str + ")\n");
                return CompileResult(reg=call_res_reg, type=func_info.ret_type);
            }
        } 
        
        else {
            if (callee.type == NODE_VAR_ACCESS) {
                let v_node -> VarAccessNode = n_call.callee;
                let s_info -> StructInfo = map_get(c.struct_table, v_node.name_tok.value);
                if (s_info is !null) {
                    return compile_struct_init(c, s_info, n_call);
                }
            }

            let callee_res -> CompileResult = compile_node(c, n_call.callee);
            let ptr_type -> Int = callee_res.type;

            let ret_type_id -> Int = 0;
            let is_valid_call -> Int = 0;

            if (ptr_type == TYPE_GENERIC_FUNCTION) {
                if (callee.type == NODE_VAR_ACCESS) {
                    let v_node -> VarAccessNode = callee;
                    let info -> SymbolInfo = find_symbol(c, v_node.name_tok.value);
                    if (info is !null && info.origin_type >= 100) {
                        let f_ret_info -> SymbolInfo = map_get(c.func_ret_map, "" + info.origin_type);
                        if (f_ret_info is !null) {
                            ret_type_id = f_ret_info.type;
                            is_valid_call = 1;
                        }
                    }
                }
                
                if (is_valid_call == 0) {
                    throw_type_error(n_call.pos, "Generic Function must specify return type.");
                }
            } 

            else {
                let f_ret_info -> SymbolInfo = map_get(c.func_ret_map, "" + ptr_type);
                if (f_ret_info is !null) {
                    ret_type_id = f_ret_info.type;
                    is_valid_call = 1;
                }
            }

            if (is_valid_call == 1) {
                let args_sig -> String = "";
                let args_val_str -> String = "";
                let curr_arg -> ArgNode = n_call.args;
                let first -> Int = 1;

                while (curr_arg is !null) {
                    let a_res -> CompileResult = compile_node(c, curr_arg.val);
                    let a_ty -> String = get_llvm_type_str(c, a_res.type);

                    if (first == 0) {
                        args_sig = args_sig + ", ";
                        args_val_str = args_val_str + ", ";
                    }
                    args_sig = args_sig + a_ty;
                    args_val_str = args_val_str + a_ty + " " + a_res.reg;
                    first = 0;
                    curr_arg = curr_arg.next;
                }

                let ret_ty_str -> String = get_llvm_type_str(c, ret_type_id);
                let func_ptr_ty -> String = ret_ty_str + " (" + args_sig + ")*";
                
                let cast_reg -> String = next_reg(c);
                write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + callee_res.reg + " to " + func_ptr_ty + "\n");

                let call_reg -> String = "";
                if (ret_type_id == TYPE_VOID) {
                    write(c.output_file, c.indent + "call void " + cast_reg + "(" + args_val_str + ")\n");
                    return void_result();
                } else {
                    call_reg = next_reg(c);
                    write(c.output_file, c.indent + call_reg + " = call " + ret_ty_str + " " + cast_reg + "(" + args_val_str + ")\n");
                    return CompileResult(reg=call_reg, type=ret_type_id); 
                }
            }

            throw_name_error(n_call.pos, "Call target is not a function or function pointer.");
        }
    }

    if (base.type == NODE_BREAK) {
        let n_break -> BreakNode = node;
        if (c.loop_stack is null) {
            throw_invalid_syntax(n_break.pos, "'break' outside of loop. ");
        }
        let scope -> LoopScope = c.loop_stack;
        write(c.output_file, c.indent + "br label %" + scope.label_break + "\n");

        return void_result();
    }

    if (base.type == NODE_CONTINUE) {
        let n_cont -> ContinueNode = node;
        if (c.loop_stack is null) {
            throw_invalid_syntax(n_cont.pos, "'continue' outside of loop. ");
        }
        let scope -> LoopScope = c.loop_stack;
        write(c.output_file, c.indent + "br label %" + scope.label_continue + "\n");

        return void_result();
    }

    if (base.type == NODE_POSTFIX) {
        let u -> PostfixOpNode = node;
        let op_type -> Int = u.op_tok.type;
        
        let var_node -> BaseNode = u.node;
        if (var_node.type != NODE_VAR_ACCESS) {
            let op_str -> String = "++";
            if (op_type == TOK_DEC) { op_str = "--"; }
            throw_type_error(u.pos, "Operator '" + op_str + "' can only be applied to variables. ");
        }
        
        let v_acc -> VarAccessNode = u.node;
        let var_name -> String = v_acc.name_tok.value;

        let info -> SymbolInfo = find_symbol(c, var_name);
        
        if (info is null) {
            throw_name_error(v_acc.pos, "Undefined variable '" + var_name + "'. ");
        }

        if (info.type == TYPE_BOOL) {
            throw_type_error(u.pos, "Cannot increment/decrement Bool type. ");
        }

        let old_val_reg -> String = next_reg(c);
        let type_str -> String = get_llvm_type_str(c, info.type);
        
        write(c.output_file, c.indent + old_val_reg + " = load " + type_str + ", " + type_str + "* " + info.reg + "\n");

        let new_val_reg -> String = next_reg(c);
        if (info.type == TYPE_INT) {
            if (op_type == TOK_INC) {
                write(c.output_file, c.indent + new_val_reg + " = add i32 " + old_val_reg + ", 1\n");
            } else {
                write(c.output_file, c.indent + new_val_reg + " = sub i32 " + old_val_reg + ", 1\n");
            }
        } else {
            if (op_type == TOK_INC) {
                write(c.output_file, c.indent + new_val_reg + " = fadd double " + old_val_reg + ", 1.0\n");
            } else {
                write(c.output_file, c.indent + new_val_reg + " = fsub double " + old_val_reg + ", 1.0\n");
            }
        }

        write(c.output_file, c.indent + "store " + type_str + " " + new_val_reg + ", " + type_str + "* " + info.reg + "\n");
        return CompileResult(reg=old_val_reg, type=info.type);
    }

    if (base.type == NODE_UNARYOP) {
        let u -> UnaryOpNode = node;
        let op_type -> Int = u.op_tok.type; 
        
        let operand -> CompileResult = compile_node(c, u.node);
        let res_reg -> String = next_reg(c);
        
        if (op_type == TOK_SUB) {
            if (operand.type == TYPE_INT) {
                write(c.output_file, c.indent + res_reg + " = sub i32 0, " + operand.reg + "\n");
                return CompileResult(reg=res_reg, type=TYPE_INT);
            } else if (operand.type == TYPE_FLOAT) {
                write(c.output_file, c.indent + res_reg + " = fneg double " + operand.reg + "\n");
                return CompileResult(reg=res_reg, type=TYPE_FLOAT);
            } else {
                throw_type_error(u.pos, "Cannot negate non-numeric type. ");
            }
        } else if (op_type == TOK_NOT) {
            if (operand.type != TYPE_BOOL) {
                throw_type_error(u.pos, "Operator '!' requires Bool type. ");
            }
            write(c.output_file, c.indent + res_reg + " = xor i1 " + operand.reg + ", 1\n");
            return CompileResult(reg=res_reg, type=TYPE_BOOL);
        } else {
            return operand;
        }
    }

    return null;
}

func compile_start(c -> Compiler) -> Void {
    write(c.output_file, "declare i32 @printf(i8*, ...)\n");
    map_put(c.declared_externs, "printf", StringConstant(id=0, value="", next=null));

    write(c.output_file, "declare i32 @snprintf(i8*, i64, i8*, ...)\n");
    map_put(c.declared_externs, "snprintf", StringConstant(id=0, value="", next=null)); 

    write(c.output_file, "declare double @llvm.pow.f64(double, double)\n\n");

    write(c.output_file, "declare i8* @malloc(i64)\n");
    map_put(c.declared_externs, "malloc", StringConstant(id=0, value="", next=null));

    write(c.output_file, "declare i32 @strlen(i8*)\n");
    map_put(c.declared_externs, "strlen", StringConstant(id=0, value="", next=null)); 

    write(c.output_file, "declare i8* @strcpy(i8*, i8*)\n");
    map_put(c.declared_externs, "strcpy", StringConstant(id=0, value="", next=null)); 

    write(c.output_file, "declare i8* @strcat(i8*, i8*)\n\n");
    map_put(c.declared_externs, "strcat", StringConstant(id=0, value="", next=null)); 

    write(c.output_file, "declare i32 @strcmp(i8*, i8*)\n\n");
    map_put(c.declared_externs, "strcmp", StringConstant(id=0, value="", next=null)); 

    write(c.output_file, "declare void @free(i8*)\n");
    map_put(c.declared_externs, "free", StringConstant(id=0, value="", next=null));

    write(c.output_file, "declare void @exit(i32)\n");
    map_put(c.declared_externs, "exit", StringConstant(id=0, value="", next=null));

    compile_arc_hooks(c);
    
    write(c.output_file, "@.fmt_int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"\n");
    write(c.output_file, "@.fmt_long = private unnamed_addr constant [6 x i8] c\"%lld\\0A\\00\"\n");
    write(c.output_file, "@.fmt_float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\"\n");
    write(c.output_file, "@.fmt_str = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\"\n\n");

    write(c.output_file, "@.fmt_int_simple = private unnamed_addr constant [3 x i8] c\"%d\\00\"\n");
    write(c.output_file, "@.fmt_float_simple = private unnamed_addr constant [3 x i8] c\"%f\\00\"\n");
    write(c.output_file, "@.str_true = private unnamed_addr constant [5 x i8] c\"true\\00\"\n");
    write(c.output_file, "@.str_false = private unnamed_addr constant [6 x i8] c\"false\\00\"\n");
    write(c.output_file, "@.str_null = private unnamed_addr constant [5 x i8] c\"null\\00\"\n\n");

    write(c.output_file, "@.str_idx_err = private unnamed_addr constant [21 x i8] c\"Index out of bounds\\0A\\00\"\n\n");
    write(c.output_file, "@.fmt_err_bounds = private unnamed_addr constant [66 x i8] c\"\\0ARuntimeError\\1B[0m: Index out of bounds\\0A    at Line %d, Column %d\\0A\\00\"\n\n");
}

func compile(c -> Compiler, node -> Struct) -> Void {
    compile_start(c);
    pre_register_structs(c, node);

    let block -> BlockNode = node;
    let curr -> StmtListNode = block.stmts;
    let last_res -> CompileResult = null;
    
    while (curr is !null) {
        last_res = compile_node(c, curr.stmt);
        curr = curr.next;
    }

    compile_end(c, last_res);
}

func compile_end(c -> Compiler, last -> CompileResult) -> Void {
    if (c.has_main == 0) {
        throw_missing_main_function();
    }
    let curr -> StringConstant = c.string_list;

    while (curr is !null) {
        let val -> String = curr.value;
        let escaped_val -> String = string_escape(val);
        
        let id -> Int = curr.id;
        let len -> Int = val.length() + 1; 
        
        let def -> String = "@.str." + id + " = private unnamed_addr constant [" + len + " x i8] c\"" + escaped_val + "\\00\"\n";
        write(c.output_file, def);
        
        curr = curr.next;
    }
    close(c.output_file);
}