// core/WhitelangCompiler.wl
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

struct CompileResult(
    reg  -> String,
    type -> Int
)

struct SymbolInfo(
    reg  -> String, 
    type -> Int
)

struct FuncInfo(
    ret_type -> Int, 
    arg_types -> Struct
)

struct TypeListNode(
    type -> Int,
    next -> Struct
)

struct Scope(
    table  -> HashMap, // symbol table of the current level
    parent -> Struct   // parent scope or null
)

struct StringConstant(
    id    -> Int,
    value -> String,
    next  -> Struct
)

struct Compiler(
    output_file -> File,
    reg_count   -> Int, 
    symbol_table -> Scope,
    global_symbol_table -> HashMap,
    func_table   -> HashMap,
    indent      -> String, 
    loop_stack   -> Struct, 
    scope_depth  -> Int,
    has_main     -> Int,
    current_ret_type -> Int,
    string_list -> Struct,
    str_count   -> Int
)


struct LoopScope(
    label_continue -> String,  
    label_break    -> String,  
    parent         -> Struct
)


func new_compiler(out_path -> String) -> Compiler {
    let f -> File = open(out_path, "w");
    // initialize empty scope
    let root_scope -> Scope = Scope(table=map_new(32), parent=null);

    return Compiler(
        output_file = f,
        reg_count = 1,
        symbol_table = root_scope,
        global_symbol_table = map_new(32),
        func_table = map_new(32),
        indent = "  ",
        loop_stack = null,
        scope_depth = 0,
        has_main = 0,
        current_ret_type = TYPE_VOID,
        string_list = null,
        str_count = 0
    );
}

func next_reg(c -> Compiler) -> String {
    let name -> String = "%t" + c.reg_count;
    c.reg_count = c.reg_count + 1;
    return name;
}

func promote_to_float(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_FLOAT) { return res; }
    
    let n_reg -> String = next_reg(c);
    let ir -> String = c.indent + n_reg + " = sitofp i32 " + res.reg + " to double\n";
    write(c.output_file, ir);
    return CompileResult(reg=n_reg, type=TYPE_FLOAT);
}

func get_llvm_type_str(type_id -> Int) -> String {
    if (type_id == TYPE_INT)   { return "i32"; }
    if (type_id == TYPE_FLOAT) { return "double"; }
    if (type_id == TYPE_BOOL)  { return "i1"; }
    if (type_id == TYPE_VOID)  { return "void"; }
    if (type_id == TYPE_STRING) { return "i8*"; }
    return ""; 
}

func get_type_name(type_id -> Int) -> String {
    if (type_id == TYPE_INT)   { return "Int"; }
    if (type_id == TYPE_FLOAT) { return "Float"; }
    if (type_id == TYPE_BOOL)  { return "Bool"; }
    if (type_id == TYPE_VOID)  { return "Void"; }
    if (type_id == TYPE_STRING) { return "String"; }
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
    while (curr != null) {
        let info -> SymbolInfo = map_get(curr.table, name);
        if (info != null) { return info; }
        curr = curr.parent;
    }

    return map_get(c.global_symbol_table, name);
}


func enter_scope(c -> Compiler) -> Void {
    let new_scope -> Scope = Scope(table=map_new(32), parent=c.symbol_table);
    c.symbol_table = new_scope;
    c.scope_depth = c.scope_depth + 1;
}
func exit_scope(c -> Compiler) -> Void {
    if (c.symbol_table.parent != null) {
        c.symbol_table = c.symbol_table.parent;
    }
    c.scope_depth = c.scope_depth - 1;
}

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
    while (curr != null) {
        last_res = compile_node(c, curr.stmt);
        curr = curr.next;
    }
    
    if (is_root == 0) {
        exit_scope(c);
    }
    
    if (last_res == null) { return void_result();}
    return last_res;
}

func compile_var_decl(c -> Compiler, node -> VarDeclareNode) -> CompileResult {
    let llvm_ty_str -> String = "";
    let target_type_id -> Int = 0;
    let type_name -> String = node.type_tok.value; 

    if (type_name == "Int") {
        llvm_ty_str = "i32";
        target_type_id = TYPE_INT;
    } else if (type_name == "Float") {
        llvm_ty_str = "double";
        target_type_id = TYPE_FLOAT;
    } else if (type_name == "Bool") {
        llvm_ty_str = "i1";
        target_type_id = TYPE_BOOL;
    } else if (type_name == "String") {
        llvm_ty_str = "i8*";
        target_type_id = TYPE_STRING;
    } else if (type_name == "Void") {
        throw_type_error(node.pos, "Cannot declare variable of type 'Void'. ");
    } else {
        throw_type_error(node.pos, "Unknown type '" + type_name + "'. ");
    }
    
    let var_name -> String = node.name_tok.value;

    // global var
    if (c.scope_depth == 0) {
        let global_name -> String = "@" + var_name;
        let init_val_str -> String = "0";
        if (target_type_id == TYPE_STRING) { init_val_str = "null"; }
        
        if (node.value != null) {
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
                 
            } else if (val_node.type == NODE_INT) {
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
        map_put(c.global_symbol_table, var_name, SymbolInfo(reg=global_name, type=target_type_id));
        return void_result();
    }

    // local var
    let ptr_reg -> String = next_reg(c);
    write(c.output_file, c.indent + ptr_reg + " = alloca " + llvm_ty_str + "\n");
    
    if (node.value != null) {
        let val_res -> CompileResult = compile_node(c, node.value);
        
        if (target_type_id == TYPE_FLOAT && val_res.type == TYPE_INT) {
            val_res = promote_to_float(c, val_res);
        }

        if (target_type_id != val_res.type) {
            throw_type_error(node.pos, "Cannot assign type '" + get_type_name(val_res.type) + "' to variable of type '" + type_name + "'. ");
        }
        write(c.output_file, c.indent + "store " + llvm_ty_str + " " + val_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
    } else {
        let zero -> String = "0";
        if (target_type_id == TYPE_STRING) { zero = "null"; }
        if (target_type_id == TYPE_FLOAT) { zero = "0.0"; }
        write(c.output_file, c.indent + "store " + llvm_ty_str + " " + zero + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
    }
    
    // store in local table (current scope)
    let curr_scope -> Scope = c.symbol_table;
    map_put(curr_scope.table, var_name, SymbolInfo(reg=ptr_reg, type=target_type_id));
    return void_result(); 
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
    if (node.else_body == null) {
        target_else = label_merge;
    }
    
    write(c.output_file, c.indent + "br i1 " + cond_res.reg + ", label %" + label_then + ", label %" + target_else + "\n");
    
    write(c.output_file, "\n" + label_then + ":\n");
    compile_node(c, node.body);
    write(c.output_file, c.indent + "br label %" + label_merge + "\n");
    
    if (node.else_body != null) {
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
    if (node.init != null) {
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
    if (node.cond != null) {
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
    if (node.step != null) {
        compile_node(c, node.step);
    }
    write(c.output_file, c.indent + "br label %" + label_cond + "\n");
    write(c.output_file, "\n" + label_end + ":\n");
    c.loop_stack = current_scope.parent;
    
    return void_result();
}

func compile_func_def(c -> Compiler, node -> FunctionDefNode) -> CompileResult {
    let func_name -> String = node.name_tok.value;
    
    if (func_name == "main") {
        c.has_main = 1;
    }

    let ret_type_str -> String = node.ret_type_tok.value;
    let llvm_ret_type -> String = "void";
    let ret_type_id -> Int = TYPE_VOID;
    
    if (ret_type_str == "Int") { llvm_ret_type = "i32"; ret_type_id = TYPE_INT; }
    else if (ret_type_str == "Float") { llvm_ret_type = "double"; ret_type_id = TYPE_FLOAT; }
    else if (ret_type_str == "Bool") { llvm_ret_type = "i1"; ret_type_id = TYPE_BOOL; }
    else if (ret_type_str == "String") { llvm_ret_type = "i8*"; ret_type_id = TYPE_STRING; }
    else if (ret_type_str == "Void") { llvm_ret_type = "void"; ret_type_id = TYPE_VOID; }
    else { throw_type_error(node.pos, "Unknown return type. "); }

    c.current_ret_type = ret_type_id;

    // register function signature
    let arg_types_head -> TypeListNode = null;
    let arg_types_curr -> TypeListNode = null;
    let curr_p -> ParamListNode = node.params;
    
    while (curr_p != null) {
        let p -> ParamNode = curr_p.param;
        let p_type -> String = p.type_tok.value;
        let p_id -> Int = TYPE_INT;
        if (p_type == "Float") { p_id = TYPE_FLOAT; }
        if (p_type == "Bool")  { p_id = TYPE_BOOL; }
        if (p_type == "String") { p_id = TYPE_STRING; }
        
        let t_node -> TypeListNode = TypeListNode(type=p_id, next=null);
        if (arg_types_head == null) { arg_types_head = t_node; arg_types_curr = t_node; }
        else { arg_types_curr.next = t_node; arg_types_curr = t_node; }
        
        curr_p = curr_p.next;
    }

    map_put(c.func_table, func_name, FuncInfo(ret_type=ret_type_id, arg_types=arg_types_head));

    let params_str -> String = "";
    let curr -> ParamListNode = node.params;
    let arg_idx -> Int = 0;
    
    while (curr != null) {
        let p -> ParamNode = curr.param;
        let p_type -> String = p.type_tok.value;
        let p_llvm_type -> String = "i32";
        if (p_type == "Int") { p_llvm_type = "i32"; }
        if (p_type == "Float") { p_llvm_type = "double"; }
        if (p_type == "Bool") { p_llvm_type = "i1"; }
        if (p_type == "String") { p_llvm_type = "i8*"; }
        
        if (arg_idx > 0) { params_str = params_str + ", "; }
        params_str = params_str + p_llvm_type + " %arg" + arg_idx;
        
        arg_idx = arg_idx + 1;
        curr = curr.next;
    }
    
    write(c.output_file, "define " + llvm_ret_type + " @" + func_name + "(" + params_str + ") {\n");
    write(c.output_file, "entry:\n");

    let old_sym -> Scope = c.symbol_table;
    c.symbol_table = Scope(table=map_new(32), parent=null);
    
    c.reg_count = 0; 
    c.scope_depth = 1; 
    
    curr = node.params;
    arg_idx = 0;
    while (curr != null) {
        let p -> ParamNode = curr.param;
        let p_name -> String = p.name_tok.value;
        let p_type -> String = p.type_tok.value;
        
        let target_type_id -> Int = TYPE_INT;
        let llvm_ty -> String = "i32";
        if (p_type == "Float") { target_type_id = TYPE_FLOAT; llvm_ty = "double"; }
        if (p_type == "Bool")  { target_type_id = TYPE_BOOL;  llvm_ty = "i1"; }
        if (p_type == "String") { target_type_id = TYPE_STRING; llvm_ty = "i8*"; }
        
        let addr_reg -> String = next_reg(c); 
        write(c.output_file, c.indent + addr_reg + " = alloca " + llvm_ty + "\n");
        write(c.output_file, c.indent + "store " + llvm_ty + " %arg" + arg_idx + ", " + llvm_ty + "* " + addr_reg + "\n");
        let curr_scope -> Scope = c.symbol_table;
        map_put(curr_scope.table, p_name, SymbolInfo(reg=addr_reg, type=target_type_id));
        
        arg_idx = arg_idx + 1;
        curr = curr.next;
    }
    
    compile_node(c, node.body);
    
    if (ret_type_id == TYPE_VOID) {
        write(c.output_file, c.indent + "ret void\n");
    } else {
        // If execution falls through without return, return 0/0.0/false
        if (ret_type_id == TYPE_INT) { write(c.output_file, c.indent + "ret i32 0\n"); }
        if (ret_type_id == TYPE_FLOAT) { write(c.output_file, c.indent + "ret double 0.0\n"); }
        if (ret_type_id == TYPE_BOOL) { write(c.output_file, c.indent + "ret i1 0\n"); }
        if (ret_type_id == TYPE_STRING) { write(c.output_file, c.indent + "ret i8* null\n"); }
    }
    
    write(c.output_file, "}\n\n");
    
    // restore scope
    c.symbol_table = old_sym;
    c.scope_depth = 0;
    
    return void_result();
}

func compile_return(c -> Compiler, node -> ReturnNode) -> CompileResult {
    if (node.value != null) {
        // return void check
        if (c.current_ret_type == TYPE_VOID) {
            throw_type_error(node.pos, "Void function cannot return a value. ");
        }

        let res -> CompileResult = compile_node(c, node.value);
        let ret_val_reg -> String = res.reg;
        let target_ty -> String = get_llvm_type_str(c.current_ret_type);

        if (res.type != c.current_ret_type) {
            // Bool -> Int
            if (res.type == TYPE_BOOL) {
                if (c.current_ret_type == TYPE_INT) {
                    let cast_reg -> String = next_reg(c);
                    write(c.output_file, c.indent + cast_reg + " = zext i1 " + res.reg + " to i32\n");
                    ret_val_reg = cast_reg;
                } else {
                    throw_type_error(node.pos, "Type mismatch in return. Expected " + get_type_name(c.current_ret_type) + ", got Bool. ");
                }
            } else {
                throw_type_error(node.pos, "Type mismatch in return. Expected " + get_type_name(c.current_ret_type) + ", got " + get_type_name(res.type));
            }
        }

        write(c.output_file, c.indent + "ret " + target_ty + " " + ret_val_reg + "\n");
    } else {
        if (c.current_ret_type != TYPE_VOID) {
            throw_type_error(node.pos, "Non-void function must return a value. ");
        }
        write(c.output_file, c.indent + "ret void\n");
    }
    
    let dead -> String = next_label(c);
    write(c.output_file, "\n" + dead + ": ; Unreachable\n");
    
    return void_result();
}

func compile_binop(c -> Compiler, node -> BinOpNode) -> CompileResult {
    let left -> CompileResult = compile_node(c, node.left);
    let right -> CompileResult = compile_node(c, node.right);
    let op_type -> Int = node.op_tok.type; 

    if (op_type == TOK_POW) {
        left = promote_to_float(c, left);
        right = promote_to_float(c, right);
        let res_reg -> String = next_reg(c);
        write(c.output_file, c.indent + res_reg + " = call double @llvm.pow.f64(double " + left.reg + ", double " + right.reg + ")\n");
        return CompileResult(reg=res_reg, type=TYPE_FLOAT);
    }

    if (op_type == TOK_AND || op_type == TOK_OR) {
        if (left.type != TYPE_BOOL || right.type != TYPE_BOOL) {
            throw_type_error(node.pos, "Logic operators '&&' and '||' require Bool operands. ");
        }
        let res_reg -> String = next_reg(c);
        let op_code -> String = "and";
        if (op_type == TOK_OR) { op_code = "or"; }
        
        write(c.output_file, c.indent + res_reg + " = " + op_code + " i1 " + left.reg + ", " + right.reg + "\n");
        return CompileResult(reg=res_reg, type=TYPE_BOOL);
    }

    let is_cmp -> Int = 0;
    if (op_type == TOK_EE) { is_cmp = 1; }
    if (op_type == TOK_NE) { is_cmp = 1; }
    if (op_type == TOK_GT) { is_cmp = 1; }
    if (op_type == TOK_LT) { is_cmp = 1; }
    if (op_type == TOK_GTE) { is_cmp = 1; }
    if (op_type == TOK_LTE) { is_cmp = 1; }

    if (is_cmp == 1) {
        if (left.type == TYPE_BOOL || right.type == TYPE_BOOL) {
            if (left.type != right.type) {
                throw_type_error(node.pos, "Cannot compare Bool with other types. ");
            }
            if (op_type != TOK_EE && op_type != TOK_NE) {
                throw_type_error(node.pos, "Operator '" + get_token_name(op_type) + "' is not defined for type Bool. ");
            }
            let res_reg -> String = next_reg(c);
            let op_code -> String = "icmp eq";
            if (op_type == TOK_NE) { op_code = "icmp ne"; }
            write(c.output_file, c.indent + res_reg + " = " + op_code + " i1 " + left.reg + ", " + right.reg + "\n");
            return CompileResult(reg=res_reg, type=TYPE_BOOL);
        }

        let cmp_mode -> Int = TYPE_INT;
        if (left.type == TYPE_FLOAT || right.type == TYPE_FLOAT) {
            cmp_mode = TYPE_FLOAT;
            left = promote_to_float(c, left);
            right = promote_to_float(c, right);
        }

        let res_reg -> String = next_reg(c);
        let op_code -> String = "";
        let type_str -> String = "i32";
        
        if (cmp_mode == TYPE_FLOAT) {
            type_str = "double";
            if (op_type == TOK_EE) { op_code = "fcmp oeq"; }
            if (op_type == TOK_NE) { op_code = "fcmp one"; }
            if (op_type == TOK_GT) { op_code = "fcmp ogt"; }
            if (op_type == TOK_LT) { op_code = "fcmp olt"; }
            if (op_type == TOK_GTE) { op_code = "fcmp oge"; }
            if (op_type == TOK_LTE) { op_code = "fcmp ole"; }
        } else {
            if (cmp_mode == TYPE_BOOL) { type_str = "i1"; }
            if (op_type == TOK_EE) { op_code = "icmp eq"; }
            if (op_type == TOK_NE) { op_code = "icmp ne"; }
            if (op_type == TOK_GT) { op_code = "icmp sgt"; }
            if (op_type == TOK_LT) { op_code = "icmp slt"; }
            if (op_type == TOK_GTE) { op_code = "icmp sge"; }
            if (op_type == TOK_LTE) { op_code = "icmp sle"; }
            if (cmp_mode == TYPE_BOOL) {
                if (op_type != TOK_EE && op_type != TOK_NE) {
                    throw_type_error(node.pos, "Operator '" + get_token_name(op_type) + "' is not defined for type Bool. ");
                }
                if (op_type == TOK_GT) { op_code = "icmp ugt"; }
                if (op_type == TOK_LT) { op_code = "icmp ult"; }
                if (op_type == TOK_GTE) { op_code = "icmp uge"; }
                if (op_type == TOK_LTE) { op_code = "icmp ule"; }
            }
        }
        
        write(c.output_file, c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n");
        return CompileResult(reg=res_reg, type=TYPE_BOOL);
    }

    if (left.type == TYPE_BOOL || right.type == TYPE_BOOL) {
        throw_type_error(node.pos, "Arithmetic operators cannot be used on Bool. ");
    }

    let target_type -> Int = TYPE_INT;
    if (left.type == TYPE_FLOAT || right.type == TYPE_FLOAT) {
        target_type = TYPE_FLOAT;
        left = promote_to_float(c, left);
        right = promote_to_float(c, right);
    }

    let res_reg -> String = next_reg(c);
    let op_code -> String = "";
    let type_str -> String = "i32";
    if (target_type == TYPE_FLOAT) { type_str = "double"; }
    
    if (target_type == TYPE_INT) {
        if (op_type == TOK_PLUS)  { op_code = "add"; }
        if (op_type == TOK_SUB)   { op_code = "sub"; }
        if (op_type == TOK_MUL)   { op_code = "mul"; }
        if (op_type == TOK_DIV)   { op_code = "sdiv"; }
        if (op_type == TOK_MOD)   { op_code = "srem"; } 
    } else {
        if (op_type == TOK_PLUS)  { op_code = "fadd"; }
        if (op_type == TOK_SUB)   { op_code = "fsub"; }
        if (op_type == TOK_MUL)   { op_code = "fmul"; }
        if (op_type == TOK_DIV)   { op_code = "fdiv"; }
        if (op_type == TOK_MOD)   { op_code = "frem"; } 
    }

    let ir -> String = c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n";
    write(c.output_file, ir);
    
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

        let reg -> String = next_reg(c);
        let len -> Int = val.length() + 1;
        write(c.output_file, c.indent + reg + " = getelementptr inbounds [" + len + " x i8], [" + len + " x i8]* @.str." + id + ", i32 0, i32 0\n");
        
        return CompileResult(reg=reg, type=TYPE_STRING);
    }

    if (base.type == NODE_VAR_DECL) { return compile_var_decl(c, node); }
    if (base.type == NODE_IF)       { return compile_if(c, node); }
    if (base.type == NODE_WHILE)    { return compile_while(c, node); }
    if (base.type == NODE_FOR)      { return compile_for(c, node); }
    if (base.type == NODE_BINOP)    { return compile_binop(c, node); }
    if (base.type == NODE_FUNC_DEF) { return compile_func_def(c, node); }
    if (base.type == NODE_RETURN)   { return compile_return(c, node); }

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
        
        if (info == null) {
            throw_name_error(v.pos, "Undefined variable '" + var_name + "'. ");
        }
        
        let val_reg -> String = next_reg(c);
        let llvm_ty_str -> String = get_llvm_type_str(info.type);
        if (llvm_ty_str == "") {
            throw_type_error(v.pos, "Variable '" + var_name + "' has invalid internal type ID. ");
        }
        
        write(c.output_file, c.indent + val_reg + " = load " + llvm_ty_str + ", " + llvm_ty_str + "* " + info.reg + "\n");
        return CompileResult(reg=val_reg, type=info.type);
    }

    if (base.type == NODE_VAR_ASSIGN) {
        let n_assign -> VarAssignNode = node;
        let var_name -> String = n_assign.name_tok.value;

        let info -> SymbolInfo = find_symbol(c, var_name);

        if (info == null) {
            throw_name_error(n_assign.pos, "Cannot assign to undefined variable '" + var_name + "'. ");
        }

        let val_res -> CompileResult = compile_node(c, n_assign.value);
        if (info.type == TYPE_FLOAT && val_res.type == TYPE_INT) {
            val_res = promote_to_float(c, val_res);
        }

        if (info.type != val_res.type) {
            throw_type_error(n_assign.pos, "Type mismatch in assignment. Expected " + get_llvm_type_str(info.type) + ", got " + get_llvm_type_str(val_res.type));
        }
        
        let ty_str -> String = get_llvm_type_str(info.type);
        write(c.output_file, c.indent + "store " + ty_str + " " + val_res.reg + ", " + ty_str + "* " + info.reg + "\n");
        
        return val_res; 
    }

    if (base.type == NODE_CALL) {
        let n_call -> CallNode = node;
        let callee -> BaseNode = n_call.callee;
        
        if (callee.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = n_call.callee;
            let func_name -> String = v_node.name_tok.value;
            
            if (func_name == "pseudo_print") {
                let args -> ArgNode = n_call.args;
                if (args == null) {
                    throw_type_error(n_call.pos, "'pseudo_print' requires exactly 1 argument. ");
                }
                if (args.next != null) {
                    throw_type_error(n_call.pos, "'pseudo_print' currently supports only 1 argument. ");
                }

                let arg_res -> CompileResult = compile_node(c, args.val);
                let fmt -> String = "%d\\0A\\00";
                let ty_str -> String = "i32";
                
                if (arg_res.type == TYPE_FLOAT) {
                    fmt = "%f\\0A\\00";
                    ty_str = "double";
                } else if (arg_res.type == TYPE_BOOL) {
                    fmt = "%d\\0A\\00";
                    ty_str = "i1";
                    let ext_reg -> String = next_reg(c);
                    write(c.output_file, c.indent + ext_reg + " = zext i1 " + arg_res.reg + " to i32\n");
                    arg_res.reg = ext_reg;
                    arg_res.type = TYPE_INT;
                    ty_str = "i32";
                } else if (arg_res.type == TYPE_STRING) {
                    fmt = "%s\\0A\\00";
                    ty_str = "i8*";
                } else if (arg_res.type == TYPE_INT) {
                    // pass
                } else {
                    throw_type_error(n_call.pos, "Cannot print type " + get_llvm_type_str(arg_res.type));
                }

                let use_fmt -> String = "@.fmt_int";
                if (ty_str == "double") { use_fmt = "@.fmt_float"; }
                if (ty_str == "i8*") { use_fmt = "@.fmt_str"; }

                let ptr_reg -> String = next_reg(c);
                write(c.output_file, c.indent + ptr_reg + " = getelementptr [4 x i8], [4 x i8]* " + use_fmt + ", i32 0, i32 0\n");
                write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + ptr_reg + ", " + ty_str + " " + arg_res.reg + ")\n");
                return void_result();
            }

            // General Function Call
            let func_info -> FuncInfo = map_get(c.func_table, func_name);
            if (func_info == null) {
                throw_name_error(n_call.pos, "Undefined function '" + func_name + "'. ");
            }

            let args_str -> String = "";
            let arg_node_curr -> ArgNode = n_call.args;
            let type_node_curr -> TypeListNode = func_info.arg_types;
            let first -> Int = 1;

            while (arg_node_curr != null) {
                if (type_node_curr == null) {
                    throw_type_error(n_call.pos, "Too many arguments.");
                }
                
                let arg_val -> CompileResult = compile_node(c, arg_node_curr.val);
                let expected_type -> Int = type_node_curr.type;
                
                // Implicit Cast: Int -> Float
                if (expected_type == TYPE_FLOAT && arg_val.type == TYPE_INT) {
                    arg_val = promote_to_float(c, arg_val);
                }
                
                if (arg_val.type != expected_type) {
                    throw_type_error(n_call.pos, "Argument type mismatch.");
                }

                let ty_str -> String = get_llvm_type_str(arg_val.type);
                if (first == 0) { args_str = args_str + ", "; }
                args_str = args_str + ty_str + " " + arg_val.reg;
                first = 0;
                
                arg_node_curr = arg_node_curr.next;
                type_node_curr = type_node_curr.next;
            }
            
            if (type_node_curr != null) {
                throw_type_error(n_call.pos, "Too few arguments.");
            }

            let ret_type_str -> String = get_llvm_type_str(func_info.ret_type);
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
        throw_name_error(n_call.pos, "Call target must be a function name. ");
    }

    if (base.type == NODE_BREAK) {
        let n_break -> BreakNode = node;
        if (c.loop_stack == null) {
            throw_invalid_syntax(n_break.pos, "'break' outside of loop. ");
        }
        let scope -> LoopScope = c.loop_stack;
        write(c.output_file, c.indent + "br label %" + scope.label_break + "\n");

        return void_result();
    }

    if (base.type == NODE_CONTINUE) {
        let n_cont -> ContinueNode = node;
        if (c.loop_stack == null) {
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
        
        if (info == null) {
            throw_name_error(v_acc.pos, "Undefined variable '" + var_name + "'. ");
        }

        if (info.type == TYPE_BOOL) {
            throw_type_error(u.pos, "Cannot increment/decrement Bool type. ");
        }

        let old_val_reg -> String = next_reg(c);
        let type_str -> String = get_llvm_type_str(info.type);
        
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
    write(c.output_file, "declare double @llvm.pow.f64(double, double)\n\n");
    
    write(c.output_file, "@.fmt_int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"\n");
    write(c.output_file, "@.fmt_float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\"\n");
    write(c.output_file, "@.fmt_str = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\"\n\n");
}

func compile_end(c -> Compiler, last -> CompileResult) -> Void {
    if (c.has_main == 0) {
        throw_missing_main_function();
    }
    let curr -> StringConstant = c.string_list;

    while (curr != null) {
        let val -> String = curr.value;
        let id -> Int = curr.id;
        let len -> Int = val.length() + 1;
        let def -> String = "@.str." + id + " = private unnamed_addr constant [" + len + " x i8] c\"" + val + "\\00\"\n";
        write(c.output_file, def);
        
        curr = curr.next;
    }
    close(c.output_file);
}