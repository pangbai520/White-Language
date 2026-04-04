// core/WhitelangCompiler.wl
import "builtin"
import "file_io"
import "map"
import "WhitelangTokens.wl"
import "WhitelangNodes.wl"
import "WhitelangExceptions.wl"
import "WhitelangUtils.wl"

import "WhitelangLexer.wl"
import "WhitelangParser.wl"


func promote_to_float(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_FLOAT) { return res; }
    let input_reg -> String = res.reg;

    if (res.type == TYPE_BYTE) {
        let uitofp_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + uitofp_reg + " = uitofp i8 " + input_reg + " to double\n");
        return CompileResult(reg=uitofp_reg, type=TYPE_FLOAT);
    }

    if (res.type == TYPE_LONG) {
        let sitofp_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + sitofp_reg + " = sitofp i64 " + input_reg + " to double\n");
        return CompileResult(reg=sitofp_reg, type=TYPE_FLOAT);
    }

    if (res.type == TYPE_BOOL) {
        let zext_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + zext_reg + " = zext i1 " + input_reg + " to i32\n");
        input_reg = zext_reg;
    }
    
    let n_reg -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + n_reg + " = sitofp i32 " + input_reg + " to double\n");
    return CompileResult(reg=n_reg, type=TYPE_FLOAT);
}
func promote_to_long(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_LONG) { return res; }
    let input_reg -> String = res.reg;

    if (res.type == TYPE_BYTE) {
        let zext_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + zext_reg + " = zext i8 " + input_reg + " to i64\n");
        return CompileResult(reg=zext_reg, type=TYPE_LONG);
    }

    if (res.type == TYPE_BOOL) {
        let zext_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + zext_reg + " = zext i1 " + input_reg + " to i64\n");
        return CompileResult(reg=zext_reg, type=TYPE_LONG);
    }

    if (res.type == TYPE_INT) {
        let n_reg -> String = next_reg(c);
        // sext: sign extension
        file_io.write(c.output_file, c.indent + n_reg + " = sext i32 " + input_reg + " to i64\n");
        return CompileResult(reg=n_reg, type=TYPE_LONG);
    }
    
    return res;
}
func promote_to_int(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type != TYPE_BYTE) { return res; }
    
    let zext_reg -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + zext_reg + " = zext i8 " + res.reg + " to i32\n");
    return CompileResult(reg=zext_reg, type=TYPE_INT);
}
func promote_to_byte(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_BYTE) { return res; }
    if (res.type == TYPE_INT) {
        let trunc_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + trunc_reg + " = trunc i32 " + res.reg + " to i8\n");
        return CompileResult(reg=trunc_reg, type=TYPE_BYTE, origin_type=res.origin_type);
    }
    return res;
}

func emit_implicit_cast(c -> Compiler, val_res -> CompileResult, expected_type -> Int, pos -> Position) -> CompileResult {
    if (val_res.type == expected_type) { return val_res; }
    let origin -> Int = val_res.origin_type;

    if (val_res.type == TYPE_NULLPTR) {
        if (is_pointer_type(c, expected_type)) {
            return CompileResult(reg="null", type=expected_type, origin_type=expected_type);
        }
        WhitelangExceptions.throw_type_error(pos, "nullptr can only be assigned to explicit pointer types.");
    }
    if (val_res.type == TYPE_NULL) {
        if (is_pointer_type(c, expected_type)) {
            WhitelangExceptions.throw_type_error(pos, "Keyword 'null' cannot be assigned to explicit pointer types. Use 'nullptr'.");
        }
        if (expected_type == TYPE_INT || expected_type == TYPE_FLOAT || expected_type == TYPE_BOOL || expected_type == TYPE_BYTE || expected_type == TYPE_LONG) {
            WhitelangExceptions.throw_type_error(pos, "Primitive types cannot be null.");
        }
        return CompileResult(reg="null", type=expected_type, origin_type=expected_type);
    }

    if (expected_type == TYPE_BYTE && val_res.type == TYPE_INT) { return promote_to_byte(c, val_res); }
    if (expected_type == TYPE_INT && val_res.type == TYPE_BYTE) { return promote_to_int(c, val_res); }
    if (expected_type == TYPE_LONG && val_res.type == TYPE_INT) { return promote_to_long(c, val_res); }
    if (expected_type == TYPE_FLOAT && val_res.type == TYPE_INT) { return promote_to_float(c, val_res); }

    if (expected_type == TYPE_INT && val_res.type == TYPE_BOOL) {
        let zext_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + zext_reg + " = zext i1 " + val_res.reg + " to i32\n");
        return CompileResult(reg=zext_reg, type=TYPE_INT, origin_type=origin);
    }

    if (expected_type == TYPE_GENERIC_STRUCT && val_res.type >= 100) {
        let cast_reg -> String = next_reg(c);
        let src_ty -> String = get_llvm_type_str(c, val_res.type);
        file_io.write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to i8*\n");
        return CompileResult(reg=cast_reg, type=TYPE_GENERIC_STRUCT, origin_type=val_res.type);
    }
    if (val_res.type == TYPE_GENERIC_STRUCT && expected_type >= 100) {
        if (map_get(c.struct_id_map, "" + expected_type) is !null || map_get(c.vector_base_map, "" + expected_type) is !null) {
            let cast_reg -> String = next_reg(c);
            let dest_ty -> String = get_llvm_type_str(c, expected_type);
            file_io.write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + val_res.reg + " to " + dest_ty + "\n");
            return CompileResult(reg=cast_reg, type=expected_type, origin_type=origin);
        }
    }

    if (is_pointer_type(c, expected_type) && is_pointer_type(c, val_res.type)) {
        if (is_void_ptr(c, expected_type) || is_void_ptr(c, val_res.type)) {
            let cast_reg -> String = next_reg(c);
            let dest_ty -> String = get_llvm_type_str(c, expected_type);
            let src_ty -> String = get_llvm_type_str(c, val_res.type);
            if (dest_ty != src_ty) {
                file_io.write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to " + dest_ty + "\n");
                return CompileResult(reg=cast_reg, type=expected_type, origin_type=origin);
            }
            return CompileResult(reg=val_res.reg, type=expected_type, origin_type=origin);
        }
    }

    if (expected_type == TYPE_GENERIC_FUNCTION && val_res.type >= 100) {
        let f_check -> SymbolInfo = map_get(c.func_ret_map, "" + val_res.type);
        if (f_check is !null) {
            return CompileResult(reg=val_res.reg, type=TYPE_GENERIC_FUNCTION, origin_type=val_res.type);
        }
    }
    if (val_res.type == TYPE_GENERIC_FUNCTION && expected_type >= 100) {
        if (map_get(c.func_ret_map, "" + expected_type) is !null) {
            return CompileResult(reg=val_res.reg, type=expected_type, origin_type=origin);
        }
    }

    WhitelangExceptions.throw_type_error(pos, "Type mismatch. Expected " + get_type_name(c, expected_type) + ", got " + get_type_name(c, val_res.type));
    return val_res;
}

func convert_to_string(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_STRING) { return res; }

    let buf_ptr -> String = emit_alloc_obj(c, "32", "" + TYPE_STRING, "i8*");

    if (res.type == TYPE_INT || res.type == TYPE_BYTE) {
        let val_reg -> String = res.reg;
        if (res.type == TYPE_BYTE) {
            val_reg = next_reg(c);
            file_io.write(c.output_file, c.indent + val_reg + " = zext i8 " + res.reg + " to i32\n");
        }
        let fmt -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + fmt + " = getelementptr [3 x i8], [3 x i8]* @.fmt_int_simple, i32 0, i32 0\n");
        
        // call snprintf(buf_ptr, 32, "%d", val)
        file_io.write(c.output_file, c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 32, i8* " + fmt + ", i32 " + val_reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_LONG) {
        let fmt -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + fmt + " = getelementptr [5 x i8], [5 x i8]* @.fmt_long_simple, i32 0, i32 0\n");
        
        file_io.write(c.output_file, c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 32, i8* " + fmt + ", i64 " + res.reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_FLOAT) {
        let fmt -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + fmt + " = getelementptr [3 x i8], [3 x i8]* @.fmt_float_simple, i32 0, i32 0\n");
        
        file_io.write(c.output_file, c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 32, i8* " + fmt + ", double " + res.reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_BOOL) {
        let ptr_true -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + ptr_true + " = getelementptr [5 x i8], [5 x i8]* @.str_true, i32 0, i32 0\n");
        let ptr_false -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + ptr_false + " = getelementptr [6 x i8], [6 x i8]* @.str_false, i32 0, i32 0\n");
        
        let src_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + src_reg + " = select i1 " + res.reg + ", i8* " + ptr_true + ", i8* " + ptr_false + "\n");
        
        file_io.write(c.output_file, c.indent + "call i8* @strcpy(i8* " + buf_ptr + ", i8* " + src_reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    let empty_src -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + empty_src + " = getelementptr [5 x i8], [5 x i8]* @.str_null, i32 0, i32 0\n");
    file_io.write(c.output_file, c.indent + "call i8* @strcpy(i8* " + buf_ptr + ", i8* " + empty_src + ")\n");
    return CompileResult(reg=buf_ptr, type=TYPE_STRING);
}

func pre_register_structs(c -> Compiler, node -> Struct) -> Void {
    let block -> BlockNode = node;
    let stmts -> Vector(Struct) = block.stmts;
    let len -> Int = 0;
    if (stmts is !null) { len = stmts.length(); }
    let i -> Int = 0;
    
    while (i < len) {
        let base -> BaseNode = stmts[i];
        if (base.type == NODE_STRUCT_DEF) {
            let n -> StructDefNode = stmts[i];
            let s_name -> String = n.name_tok.value;
            let new_id -> Int = c.type_counter;
            c.type_counter += 1;
            let info -> StructInfo = StructInfo(
                name = s_name,
                type_id = new_id,
                fields = null, 
                llvm_name = "%struct." + s_name,
                init_body = n.body,
                is_class = false,
                vtable_name = ""
            );
            map_put(c.struct_table, s_name, info);
            map_put(c.struct_id_map, "" + new_id, info);
            
        } else if (base.type == NODE_CLASS_DEF) {
            let c_node -> ClassDefNode = stmts[i];
            let c_name -> String = c_node.name_tok.value;
            let new_id -> Int = c.type_counter;
            c.type_counter += 1;
            let info -> StructInfo = StructInfo(
                name = c_name,
                type_id = new_id,
                fields = null, 
                llvm_name = "%class." + c_name,
                init_body = c_node,
                is_class = true,
                vtable_name = "@vtable." + c_name
            );
            map_put(c.struct_table, c_name, info);
            map_put(c.struct_id_map, "" + new_id, info);
        }
        i += 1;
    }
}
func pre_register_funcs(c -> Compiler, node -> Struct) -> Void {
    let block -> BlockNode = node;
    let stmts -> Vector(Struct) = block.stmts;
    let len -> Int = 0;
    if (stmts is !null) { len = stmts.length(); }
    let i -> Int = 0;
    
    while (i < len) {
        let base -> BaseNode = stmts[i];
        if (base.type == NODE_FUNC_DEF) {
            let f_node -> FunctionDefNode = stmts[i];
            let raw_name -> String = f_node.name_tok.value;
            let func_name -> String = raw_name;
            if (raw_name != "main") {
                func_name = c.current_package_prefix + raw_name;
            }
            
            if (map_get(c.func_table, func_name) is !null) {
                WhitelangExceptions.throw_type_error(f_node.pos, "Function '" + func_name + "' is already defined.");
            }
            
            let ret_type_id -> Int = resolve_type(c, f_node.ret_type_tok);
            let arg_types -> Vector(Struct) = [];
            
            let params -> Vector(Struct) = f_node.params;
            let p_len -> Int = 0; if (params is !null) { p_len = params.length(); }
            let p_idx -> Int = 0;
            
            while (p_idx < p_len) {
                let p -> ParamNode = params[p_idx];
                let p_id -> Int = resolve_type(c, p.type_tok);
                arg_types.append(TypeListNode(type=p_id));
                p_idx += 1;
            }
            
            let f_info -> FuncInfo = FuncInfo(name=func_name, ret_type=ret_type_id, arg_types=arg_types, is_varargs=false);
            map_put(c.func_table, func_name, f_info);

        } else if (base.type == NODE_CLASS_DEF) {
            let c_node -> ClassDefNode = stmts[i];
            let c_name -> String = c_node.name_tok.value;
            let m_vec -> Vector(Struct) = c_node.methods;
            let m_len -> Int = 0; if (m_vec is !null) { m_len = m_vec.length(); }

            let c_info -> StructInfo = map_get(c.struct_table, c_name);
            let class_type_id -> Int = c_info.type_id;

            let m_idx -> Int = 0;
            while (m_idx < m_len) {
                let m_node -> MethodDefNode = m_vec[m_idx];
                let m_raw_name -> String = m_node.name_tok.value;
                let m_name -> String = c.current_package_prefix + c_name + "_" + m_raw_name; 
                
                if (map_get(c.func_table, m_name) is !null) {
                    WhitelangExceptions.throw_type_error(m_node.pos, "Method '" + m_name + "' is already defined.");
                }

                let ret_id -> Int = resolve_type(c, m_node.return_type);
                let arg_types -> Vector(Struct) = [];

                arg_types.append(TypeListNode(type=class_type_id));
                
                let p_vec -> Vector(Struct) = m_node.params;
                let p_len -> Int = 0; if (p_vec is !null) { p_len = p_vec.length(); }
                let p_idx -> Int = 0;
                while (p_idx < p_len) {
                    let p -> ParamNode = p_vec[p_idx];
                    let p_type -> Int = resolve_type(c, p.type_tok);
                    arg_types.append(TypeListNode(type=p_type));
                    p_idx += 1;
                }
                
                let f_info -> FuncInfo = FuncInfo(name=m_name, ret_type=ret_id, arg_types=arg_types, is_varargs=false);
                map_put(c.func_table, m_name, f_info);
                m_idx += 1;
            }
        }
        i += 1;
    }
}

// === SCOPE ===
func enter_scope(c -> Compiler) -> Void {
    let new_scope -> Scope = Scope(table=map_new(32), parent=c.symbol_table, gc_vars=[]);
    c.symbol_table = new_scope;
    c.scope_depth += 1;
}
func exit_scope(c -> Compiler) -> Void {
    let curr_scope -> Scope = c.symbol_table;

    let gc_vec -> Vector(Struct) = curr_scope.gc_vars;
    let gc_len -> Int = 0; if (gc_vec is !null) { gc_len = gc_vec.length(); }
    let gc_idx -> Int = 0;
    while (gc_idx < gc_len) {
        let curr_gc -> GCTracker = gc_vec[gc_idx];
        let ty_str -> String = get_llvm_type_str(c, curr_gc.type);
        let val_reg -> String = next_reg(c);
        write(c.output_file, c.indent + val_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_gc.reg + "\n");
        emit_release(c, val_reg, curr_gc.type);
        gc_idx += 1;
    }

    if (c.symbol_table.parent is !null) {
        c.symbol_table = c.symbol_table.parent;
    }
    c.scope_depth -= 1;
}
func cleanup_all_scopes(c -> Compiler) -> Void {
    let curr -> Scope = c.symbol_table;
    while (curr is !null) { 
        let gc_vec -> Vector(Struct) = curr.gc_vars;
        let gc_len -> Int = 0; if (gc_vec is !null) { gc_len = gc_vec.length(); }
        let gc_idx -> Int = 0;
        while (gc_idx < gc_len) {
            let gc_node -> GCTracker = gc_vec[gc_idx];
            let ty_str -> String = get_llvm_type_str(c, gc_node.type);
            let val_reg -> String = next_reg(c);
            write(c.output_file, c.indent + val_reg + " = load " + ty_str + ", " + ty_str + "* " + gc_node.reg + "\n");
            emit_release(c, val_reg, gc_node.type);
            gc_idx += 1;
        }
        curr = curr.parent;
    }
}


func emit_retain(c -> Compiler, reg -> String, type_id -> Int) -> Void {
    if (!is_ref_type(c, type_id)) { return; }
    
    // cast to i8* for the runtime function
    let cast_reg -> String = next_reg(c);
    let src_ty -> String = get_llvm_type_str(c, type_id);
    if (src_ty == "i8*") {
        file_io.write(c.output_file, c.indent + "call void @__wl_retain(i8* " + reg + ")\n");
        return;
    }
    file_io.write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + reg + " to i8*\n");
    file_io.write(c.output_file, c.indent + "call void @__wl_retain(i8* " + cast_reg + ")\n");
}

func emit_release(c -> Compiler, reg -> String, type_id -> Int) -> Void {
    if (!is_ref_type(c, type_id)) { return; }

    let cast_reg -> String = next_reg(c);
    let src_ty -> String = get_llvm_type_str(c, type_id);
    if (src_ty == "i8*") {
        file_io.write(c.output_file, c.indent + "call void @__wl_release(i8* " + reg + ")\n");
        return;
    }
    file_io.write(c.output_file, c.indent + cast_reg + " = bitcast " + src_ty + " " + reg + " to i8*\n");
    file_io.write(c.output_file, c.indent + "call void @__wl_release(i8* " + cast_reg + ")\n");
}

func emit_alloc_obj(c -> Compiler, payload_size_reg -> String, type_id_str -> String, dest_llvm_type -> String) -> String {
    let total_size -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + total_size + " = add i64 " + payload_size_reg + ", 8\n");
    
    let raw_mem -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + raw_mem + " = call i8* @malloc(i64 " + total_size + ")\n");
    
    let rc_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + rc_ptr + " = bitcast i8* " + raw_mem + " to i32*\n");
    file_io.write(c.output_file, c.indent + "store i32 0, i32* " + rc_ptr + "\n");
    
    let type_ptr_i8 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + type_ptr_i8 + " = getelementptr inbounds i8, i8* " + raw_mem + ", i32 4\n");
    let type_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + type_ptr + " = bitcast i8* " + type_ptr_i8 + " to i32*\n");
    file_io.write(c.output_file, c.indent + "store i32 " + type_id_str + ", i32* " + type_ptr + "\n");
    
    let payload_i8 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + payload_i8 + " = getelementptr inbounds i8, i8* " + raw_mem + ", i32 8\n");
    
    if (dest_llvm_type == "i8*") {
        return payload_i8; 
    }
    
    let final_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + final_ptr + " = bitcast i8* " + payload_i8 + " to " + dest_llvm_type + "\n");
    return final_ptr;
}

func hoist_allocas(c -> Compiler, node -> Struct) -> Void {
    if (node is null) { return;
    }
    let base -> BaseNode = node;
    if (base.type == NODE_BLOCK) {
        let block -> BlockNode = node;
        let stmts -> Vector(Struct) = block.stmts;
        let len -> Int = 0;
        if (stmts is !null) { len = stmts.length(); }
        let i -> Int = 0;
        while (i < len) {
            hoist_allocas(c, stmts[i]);
            i += 1;
        }
    } else if (base.type == NODE_IF) {
        let if_n -> IfNode = node;
        hoist_allocas(c, if_n.body);
        hoist_allocas(c, if_n.else_body);
    } else if (base.type == NODE_WHILE) {
        let w_n -> WhileNode = node;
        hoist_allocas(c, w_n.body);
    } else if (base.type == NODE_FOR) {
        let f_n -> ForNode = node;
        hoist_allocas(c, f_n.init);
        hoist_allocas(c, f_n.body);
    } else if (base.type == NODE_VAR_DECL) {
        let v_node -> VarDeclareNode = node;
        if (c.scope_depth > 0) {
            let target_type_id -> Int = resolve_type(c, v_node.type_node);
            let llvm_ty_str -> String = get_llvm_type_str(c, target_type_id);
            let ptr_reg -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + ptr_reg + " = alloca " + llvm_ty_str + "\n");
            v_node.alloc_reg = ptr_reg;
        }
    }
}

func emit_runtime_error(c -> Compiler, pos -> Position, msg -> String) -> Void {
    let header_fmt -> String = "RuntimeError: " + msg + "\n    at Line %d, Column %d\n\n";

    let header_id -> Int = c.str_count;
    c.str_count += 1;
    c.string_list.append(StringConstant(id=header_id, value=header_fmt));

    let header_len -> Int = header_fmt.length() + 1;

    let header_ptr -> String = "getelementptr inbounds ([" + header_len + " x i8], [" + header_len + " x i8]* @.str." + header_id + ", i32 0, i32 0)";
    
    let ln -> Int = pos.ln + 1;
    let col -> Int = pos.col + 1;
    file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + header_ptr + ", i32 " + ln + ", i32 " + col + ")\n");

    let full_text -> String = pos.text;
    if (full_text.length() > 0) {
        let current_ln -> Int = 0;
        let scan_idx -> Int = 0;
        let len -> Int = full_text.length();
        let line_start_idx -> Int = 0;

        while (scan_idx < len) {
            if (current_ln == pos.ln) { line_start_idx = scan_idx; break; }
            if (full_text[scan_idx] == 10) { current_ln += 1; }
            scan_idx += 1;
        }

        let line_end_idx -> Int = line_start_idx;
        while (line_end_idx < len) {
            let ch -> Int = full_text[line_end_idx];
            if (ch == 10 || ch == 13) { break; }
            line_end_idx += 1;
        }

        let raw_line -> String = full_text.slice(line_start_idx, line_end_idx);
        let code_content -> String = "    " + raw_line + "\n";
    
        let code_id -> Int = c.str_count;
        c.str_count += 1;
        c.string_list.append(StringConstant(id=code_id, value=code_content));
    
        let code_len -> Int = code_content.length() + 1;
        
        let code_ptr -> String = "getelementptr inbounds ([" + code_len + " x i8], [" + code_len + " x i8]* @.str." + code_id + ", i32 0, i32 0)";
        file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + code_ptr + ")\n");

        let err_len -> Int = 1;
        let line_len -> Int = raw_line.length();
        if (pos.col < line_len) {
            let ch -> Int = raw_line[pos.col];
            if ((ch >= 65 && ch <= 90) || (ch >= 97 && ch <= 122) || ch == 95) {
                let cur -> Int = pos.col + 1;
                while (cur < line_len) {
                    let c2 -> Int = raw_line[cur];
                    if ((c2 >= 65 && c2 <= 90) || (c2 >= 97 && c2 <= 122) || c2 == 95 || (c2 >= 48 && c2 <= 57)) {
                        cur += 1;
                    } else {
                        break;
                    }
                }
                err_len = cur - pos.col;
            }
        }

        let arrow_str -> String = "    ";
        let k -> Int = 0;
        while (k < pos.col) {
            arrow_str += " ";
            k += 1;
        }
        let j -> Int = 0;
        while (j < err_len) {
            arrow_str += "^";
            j += 1;
        }
        arrow_str += "\n";
    
        let arrow_id -> Int = c.str_count;
        c.str_count += 1;
        c.string_list.append(StringConstant(id=arrow_id, value=arrow_str));
    
        let arrow_len -> Int = arrow_str.length() + 1;
        
        let arrow_ptr -> String = "getelementptr inbounds ([" + arrow_len + " x i8], [" + arrow_len + " x i8]* @.str." + arrow_id + ", i32 0, i32 0)";
        file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + arrow_ptr + ")\n");
    }

    file_io.write(c.output_file, c.indent + "call void @exit(i32 1)\n");
    file_io.write(c.output_file, c.indent + "unreachable\n");
}


func emit_vector_bounds_check(c -> Compiler, vec_reg -> String, idx_reg -> String, struct_ty -> String, pos -> Position) -> Void {
    let size_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");

    let idx_i64 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + idx_i64 + " = sext i32 " + idx_reg + " to i64\n");

    let cmp_reg -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + cmp_reg + " = icmp uge i64 " + idx_i64 + ", " + size_val + "\n");

    let fail_label -> String = "bounds_fail_" + c.type_counter;
    let ok_label -> String = "bounds_ok_" + c.type_counter;
    c.type_counter += 1;
    
    file_io.write(c.output_file, c.indent + "br i1 " + cmp_reg + ", label %" + fail_label + ", label %" + ok_label + "\n");

    file_io.write(c.output_file, "\n" + fail_label + ":\n");
    emit_runtime_error(c, pos, "Index out of bounds");

    file_io.write(c.output_file, "\n" + ok_label + ":\n");
}

// === COMPILE ===
func compile_arc_hooks(c -> Compiler) -> Void {
    // --- __wl_retain(i8* ptr) ---
    // if (ptr is null) return; ptr[-8].rc++;
    file_io.write(c.output_file, "define void @__wl_retain(i8* %ptr) {\n");
    file_io.write(c.output_file, "entry:\n");
    file_io.write(c.output_file, "  %is_null = icmp eq i8* %ptr, null\n");
    file_io.write(c.output_file, "  br i1 %is_null, label %done, label %work\n");
    file_io.write(c.output_file, "work:\n");
    file_io.write(c.output_file, "  %base = getelementptr i8, i8* %ptr, i32 -8\n");
    file_io.write(c.output_file, "  %rc_ptr = bitcast i8* %base to i32*\n");
    file_io.write(c.output_file, "  %rc = load i32, i32* %rc_ptr\n");
    file_io.write(c.output_file, "  %new_rc = add i32 %rc, 1\n");
    file_io.write(c.output_file, "  store i32 %new_rc, i32* %rc_ptr\n");
    file_io.write(c.output_file, "  br label %done\n");
    file_io.write(c.output_file, "done:\n");
    file_io.write(c.output_file, "  ret void\n");
    file_io.write(c.output_file, "}\n\n");

    // --- __wl_release(i8* ptr) ---
    // if (ptr is null) return; ptr[-8].rc--; if (rc == 0) free(ptr[-8]);
    file_io.write(c.output_file, "define void @__wl_release(i8* %ptr) {\n");
    file_io.write(c.output_file, "entry:\n");
    file_io.write(c.output_file, "  %is_null = icmp eq i8* %ptr, null\n");
    file_io.write(c.output_file, "  br i1 %is_null, label %done, label %work\n");
    file_io.write(c.output_file, "work:\n");
    file_io.write(c.output_file, "  %base = getelementptr i8, i8* %ptr, i32 -8\n");
    file_io.write(c.output_file, "  %rc_ptr = bitcast i8* %base to i32*\n");
    file_io.write(c.output_file, "  %rc = load i32, i32* %rc_ptr\n");
    file_io.write(c.output_file, "  %new_rc = sub i32 %rc, 1\n");
    file_io.write(c.output_file, "  store i32 %new_rc, i32* %rc_ptr\n");
    file_io.write(c.output_file, "  %is_zero = icmp eq i32 %new_rc, 0\n");
    file_io.write(c.output_file, "  br i1 %is_zero, label %free_check, label %done\n");
    
    // get type_id from header: base[4]
    file_io.write(c.output_file, "free_check:\n");
    file_io.write(c.output_file, "  %type_ptr = getelementptr i8, i8* %base, i32 4\n");
    file_io.write(c.output_file, "  %type_ptr_i32 = bitcast i8* %type_ptr to i32*\n");
    file_io.write(c.output_file, "  %type_id = load i32, i32* %type_ptr_i32\n");
    
    // switch routing for deep drop
    file_io.write(c.output_file, "  switch i32 %type_id, label %free_default [\n");

    let drop_list -> Vector(Struct) = c.type_drop_list;
    let d_len -> Int = 0; if (drop_list is !null) { d_len = drop_list.length(); }
    let d_idx -> Int = 0;
    while (d_idx < d_len) {
        let curr_drop -> TypeListNode = drop_list[d_idx];
        file_io.write(c.output_file, "    i32 " + curr_drop.type + ", label %drop_" + curr_drop.type + "\n");
        d_idx += 1;
    }

    let class_idx -> Int = 100;
    while (class_idx < c.type_counter) {
        let cls_info -> StructInfo = map_get(c.struct_id_map, "" + class_idx);
        if (cls_info is !null && cls_info.is_class) {
            let is_in_drop -> Bool = false;
            let check_idx -> Int = 0;
            while (check_idx < d_len) {
                let c_drop -> TypeListNode = drop_list[check_idx];
                if (c_drop.type == class_idx) { is_in_drop = true; }
                check_idx += 1;
            }
            if (!is_in_drop) {
                file_io.write(c.output_file, "    i32 " + class_idx + ", label %drop_" + class_idx + "\n");
            }
        }
        class_idx += 1;
    }

    file_io.write(c.output_file, "  ]\n");

    // default: free(ptr[-8])
    file_io.write(c.output_file, "\nfree_default:\n");
    file_io.write(c.output_file, "  call void @free(i8* %base)\n"); 
    file_io.write(c.output_file, "  br label %done\n");

    // generate drop blocks for complex types
    d_idx = 0;
    while (d_idx < d_len) {
        let curr_drop -> TypeListNode = drop_list[d_idx];
        let t_id -> Int = curr_drop.type;
        file_io.write(c.output_file, "\ndrop_" + t_id + ":\n");
        
        let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + t_id);
        
        if (t_id == TYPE_GENERIC_FUNCTION) {
            file_io.write(c.output_file, "  %env_ptr_addr_" + t_id + " = getelementptr inbounds i8, i8* %ptr, i32 8\n");
            file_io.write(c.output_file, "  %env_ptr_cast_" + t_id + " = bitcast i8* %env_ptr_addr_" + t_id + " to i8**\n");
            file_io.write(c.output_file, "  %env_val_" + t_id + " = load i8*, i8** %env_ptr_cast_" + t_id + "\n");
            emit_release(c, "%env_val_" + t_id, TYPE_GENERIC_STRUCT);
            file_io.write(c.output_file, "  br label %free_default\n");
        } else if (v_info is !null) {
            // Vector: free(vector.data)
            let elem_ty_str -> String = get_llvm_type_str(c, v_info.type);
            let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";
            file_io.write(c.output_file, "  %vec_cast_" + t_id + " = bitcast i8* %ptr to " + struct_ty + "*\n");
            
            if (is_ref_type(c, v_info.type)) {
                file_io.write(c.output_file, "  %size_ptr_" + t_id + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* %vec_cast_" + t_id + ", i32 0, i32 0\n");
                file_io.write(c.output_file, "  %size_" + t_id + " = load i64, i64* %size_ptr_" + t_id + "\n");
                file_io.write(c.output_file, "  %data_ptr_ptr_" + t_id + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* %vec_cast_" + t_id + ", i32 0, i32 2\n");
                file_io.write(c.output_file, "  %data_ptr_" + t_id + " = load " + elem_ty_str + "*, " + elem_ty_str + "** %data_ptr_ptr_" + t_id + "\n");

                let loop_cond -> String = "vec_drop_cond_" + t_id;
                let loop_body -> String = "vec_drop_body_" + t_id;
                let loop_end -> String = "vec_drop_end_" + t_id;

                file_io.write(c.output_file, "  %idx_ptr_" + t_id + " = alloca i64\n");
                file_io.write(c.output_file, "  store i64 0, i64* %idx_ptr_" + t_id + "\n");
                file_io.write(c.output_file, "  br label %" + loop_cond + "\n");

                file_io.write(c.output_file, "\n" + loop_cond + ":\n");
                file_io.write(c.output_file, "  %curr_idx_" + t_id + " = load i64, i64* %idx_ptr_" + t_id + "\n");
                file_io.write(c.output_file, "  %cmp_" + t_id + " = icmp slt i64 %curr_idx_" + t_id + ", %size_" + t_id + "\n");
                file_io.write(c.output_file, "  br i1 %cmp_" + t_id + ", label %" + loop_body + ", label %" + loop_end + "\n");

                file_io.write(c.output_file, "\n" + loop_body + ":\n");
                file_io.write(c.output_file, "  %slot_" + t_id + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* %data_ptr_" + t_id + ", i64 %curr_idx_" + t_id + "\n");
                file_io.write(c.output_file, "  %elem_" + t_id + " = load " + elem_ty_str + ", " + elem_ty_str + "* %slot_" + t_id + "\n");
                
                emit_release(c, "%elem_" + t_id, v_info.type);

                file_io.write(c.output_file, "  %next_idx_" + t_id + " = add i64 %curr_idx_" + t_id + ", 1\n");
                file_io.write(c.output_file, "  store i64 %next_idx_" + t_id + ", i64* %idx_ptr_" + t_id + "\n");
                file_io.write(c.output_file, "  br label %" + loop_cond + "\n");

                file_io.write(c.output_file, "\n" + loop_end + ":\n");
                file_io.write(c.output_file, "  %data_i8_" + t_id + " = bitcast " + elem_ty_str + "* %data_ptr_" + t_id + " to i8*\n");
                file_io.write(c.output_file, "  call void @free(i8* %data_i8_" + t_id + ")\n");
            } else {
                file_io.write(c.output_file, "  %data_ptr_ptr_" + t_id + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* %vec_cast_" + t_id + ", i32 0, i32 2\n");
                file_io.write(c.output_file, "  %data_ptr_" + t_id + " = load " + elem_ty_str + "*, " + elem_ty_str + "** %data_ptr_ptr_" + t_id + "\n");
                file_io.write(c.output_file, "  %data_i8_" + t_id + " = bitcast " + elem_ty_str + "* %data_ptr_" + t_id + " to i8*\n");
                file_io.write(c.output_file, "  call void @free(i8* %data_i8_" + t_id + ")\n");
            }
            file_io.write(c.output_file, "  br label %free_default\n");
        } else {
            let s_info -> StructInfo = map_get(c.struct_id_map, "" + t_id);
            if (s_info is !null) {
                // Struct: release each ref field
                file_io.write(c.output_file, "  %struct_cast_" + t_id + " = bitcast i8* %ptr to " + s_info.llvm_name + "*\n");

                let fields_vec -> Vector(Struct) = s_info.fields;
                let f_len -> Int = 0; if (fields_vec is !null) { f_len = fields_vec.length(); }
                let f_idx -> Int = 0;
                
                while (f_idx < f_len) {
                    let f_curr -> FieldInfo = fields_vec[f_idx];
                    if (is_ref_type(c, f_curr.type) && f_curr.name != "_vptr") {
                        file_io.write(c.output_file, "  %f_ptr_" + t_id + "_" + f_curr.offset + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* %struct_cast_" + t_id + ", i32 0, i32 " + f_curr.offset + "\n");
                        file_io.write(c.output_file, "  %f_val_" + t_id + "_" + f_curr.offset + " = load " + f_curr.llvm_type + ", " + f_curr.llvm_type + "* %f_ptr_" + t_id + "_" + f_curr.offset + "\n");
                        emit_release(c, "%f_val_" + t_id + "_" + f_curr.offset, f_curr.type);
                    }
                    f_idx += 1;
                }
                file_io.write(c.output_file, "  br label %free_default\n");
            } else {
                file_io.write(c.output_file, "  br label %free_default\n");
            }
        }
        d_idx += 1;
    }

    class_idx = 100;
    while (class_idx < c.type_counter) {
        let cls_info -> StructInfo = map_get(c.struct_id_map, "" + class_idx);
        if (cls_info is !null && cls_info.is_class) {
            let is_in_drop -> Bool = false;
            let check_idx -> Int = 0;
            while (check_idx < d_len) {
                let c_drop -> TypeListNode = drop_list[check_idx];
                if (c_drop.type == class_idx) { is_in_drop = true; }
                check_idx += 1;
            }
            if (!is_in_drop) {
                file_io.write(c.output_file, "\ndrop_" + class_idx + ":\n");
                file_io.write(c.output_file, "  %struct_cast_" + class_idx + " = bitcast i8* %ptr to " + cls_info.llvm_name + "*\n");

                let fields_vec -> Vector(Struct) = cls_info.fields;
                let f_len -> Int = 0; if (fields_vec is !null) { f_len = fields_vec.length(); }
                let f_idx -> Int = 0;
                
                while (f_idx < f_len) {
                    let f_curr -> FieldInfo = fields_vec[f_idx];
                    if (is_ref_type(c, f_curr.type) && f_curr.name != "_vptr") {
                        file_io.write(c.output_file, "  %f_ptr_" + class_idx + "_" + f_curr.offset + " = getelementptr inbounds " + cls_info.llvm_name + ", " + cls_info.llvm_name + "* %struct_cast_" + class_idx + ", i32 0, i32 " + f_curr.offset + "\n");
                        file_io.write(c.output_file, "  %f_val_" + class_idx + "_" + f_curr.offset + " = load " + f_curr.llvm_type + ", " + f_curr.llvm_type + "* %f_ptr_" + class_idx + "_" + f_curr.offset + "\n");
                        emit_release(c, "%f_val_" + class_idx + "_" + f_curr.offset, f_curr.type);
                    }
                    f_idx += 1;
                }
                file_io.write(c.output_file, "  br label %free_default\n");
            }
        }
        class_idx += 1;
    }

    file_io.write(c.output_file, "done:\n");
    file_io.write(c.output_file, "  ret void\n");
    file_io.write(c.output_file, "}\n\n");
}

func compile_ast(c -> Compiler, node -> Struct) -> Void {
    let block -> BlockNode = node;
    let stmts -> Vector(Struct) = block.stmts;
    let len -> Int = 0;
    if (stmts is !null) { len = stmts.length(); }

    let i -> Int = 0;
    while (i < len) {
        let base -> BaseNode = stmts[i];
        if (base.type == NODE_IMPORT) {
            compile_import(c, stmts[i]);
        }
        i += 1;
    }

    pre_register_structs(c, node);
    pre_register_funcs(c, node);

    i = 0;
    while (i < len) {
        let base -> BaseNode = stmts[i];
        if (base.type != NODE_IMPORT) {
            compile_node(c, stmts[i]);
        }
        i += 1;
    }
}

func compile_import(c -> Compiler, node -> ImportNode) -> Void {
    let raw_path -> String = node.path_tok.value;
    let final_path -> String = resolve_import_path(c, raw_path, node.pos);

    let import_prefix -> String = "";
    let is_pkg -> Bool = false;
    if (!raw_path.ends_with(".wl")) {
        if (final_path.ends_with("/_pkg.wl") || final_path.ends_with("\\_pkg.wl") || final_path.ends_with("\\_pgk.wl")) {
            import_prefix = raw_path + ".";
            is_pkg = true;
        }
    }

    if (map_get(c.imported_modules, final_path) is !null) { 
        if (node.symbols is !null) {
            bind_import_symbols(c, node, import_prefix);
        }
        return; 
    }

    let marker -> StringConstant = StringConstant(id=0, value="imported");

    if is_pkg {
        let pkg_mod_name -> String = raw_path;
        if (node.alias_tok is !null) {
            pkg_mod_name = node.alias_tok.value;
        }
        let pkg_val -> StringConstant = StringConstant(id=0, value=raw_path);
        map_put(c.loaded_packages, pkg_mod_name, pkg_val);
    } else {
        let file_mod_name -> String = "";
        if (node.alias_tok is !null) {
            file_mod_name = node.alias_tok.value;
        } else {
            let len -> Int = raw_path.length();
            let end_idx -> Int = len;
            if (raw_path.ends_with(".wl")) {
                end_idx = len - 3;
            }
            let start_idx -> Int = 0;
            let i -> Int = len - 1;
            while (i >= 0) {
                let ch -> Int = raw_path[i];
                if (ch == 47 || ch == 92) {
                    start_idx = i + 1;
                    break;
                }
                i -= 1;
            }
            file_mod_name = raw_path.slice(start_idx, end_idx);
        }
        map_put(c.loaded_files, file_mod_name, marker);
    }

    if (map_get(c.imported_modules, final_path) is !null) { 
        if (node.symbols is !null) {
            bind_import_symbols(c, node, import_prefix);
        }
        return; 
    }

    map_put(c.imported_modules, final_path, marker);
    
    let old_prefix -> String = c.current_package_prefix;
    let is_package_entry -> Bool = false;

    let old_dir -> String = c.current_dir;
    c.current_dir = get_dir_name(final_path);

    if is_pkg {
        c.current_package_prefix = raw_path + ".";
        is_package_entry = true;
    }

    let f -> File = file_io.open(final_path, "rb");
    if (f is null) { WhitelangExceptions.throw_import_error(node.pos, "Failed to open: " + final_path); }
    let source -> String = file_io.read_all(f);
    file_io.close(f);

    let lexer -> Lexer = WhitelangLexer.new_lexer(final_path, source);
    let parser -> Parser = WhitelangParser.Parser(lexer=lexer, current_tok=get_next_token(lexer));
    let mod_ast -> Struct = WhitelangParser.parse(parser);

    compile_ast(c, mod_ast);

    c.current_package_prefix = old_prefix;
    c.current_dir = old_dir;

    if (node.symbols is !null) {
        bind_import_symbols(c, node, import_prefix);
    }
}

func compile_block(c -> Compiler, node -> BlockNode) -> CompileResult {
    let is_root -> Bool = false;
    if (c.scope_depth == 0) {
        is_root = true;
    }

    if (!is_root) {
        enter_scope(c);
    }
    
    let stmts -> Vector(Struct) = node.stmts;
    let len -> Int = 0;
    if (stmts is !null) { len = stmts.length(); }
    let i -> Int = 0;
    
    let last_res -> CompileResult = null;
    let terminated -> Bool = false;
    while (i < len) {
        let stmt -> BaseNode = stmts[i];
        if (stmt.type == NODE_RETURN) { terminated = true; }
        if (stmt.type == NODE_BREAK) { terminated = true;
        }
        if (stmt.type == NODE_CONTINUE) { terminated = true;
        }

        last_res = compile_node(c, stmts[i]);
        if (stmt.type == NODE_RETURN || stmt.type == NODE_BREAK || stmt.type == NODE_CONTINUE) { 
            terminated = true;
            break;
        }
        i += 1;
    }
    
    if (!is_root) {
        if terminated {
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
                c.str_count += 1;
                c.string_list.append(StringConstant(id=s_id, value=s_val));

                let len -> Int = s_val.length() + 1;
                init_val_str = "getelementptr inbounds ([" + len + " x i8], [" + len + " x i8]* @.str." + s_id + ", i32 0, i32 0)";
            }
            else if (val_node.type == NODE_NULLPTR) {
                if (is_pointer_type(c, target_type_id) == false) {
                    WhitelangExceptions.throw_type_error(node.pos, "Global 'nullptr' can only be assigned to pointer types.");
                }
                init_val_str = "null";
            }
            else if (val_node.type == NODE_NULL) {
                if (is_pointer_type(c, target_type_id) == true) {
                WhitelangExceptions.throw_type_error(node.pos, "Global 'null' cannot be assigned to explicit pointer types. Use 'nullptr'.");
                }
                if (target_type_id == TYPE_INT || target_type_id == TYPE_FLOAT || target_type_id == TYPE_BOOL) {
                WhitelangExceptions.throw_type_error(node.pos, "Primitive types cannot be null.");
                }
                init_val_str = "null";
            }
            else if (val_node.type == NODE_INT) {
                let n -> IntNode = node.value;
                init_val_str = n.tok.value;
                if (target_type_id == TYPE_FLOAT) { WhitelangExceptions.throw_type_error(node.pos, "Type mismatch (Int -> Float). "); }
            } else if (val_node.type == NODE_FLOAT) {
                let n -> FloatNode = node.value;
                init_val_str = n.tok.value;
                if (target_type_id != TYPE_FLOAT) { WhitelangExceptions.throw_type_error(node.pos, "Type mismatch (Float -> Int). "); }
            } else if (val_node.type == NODE_BOOL) {
                let n -> BooleanNode = node.value;
                if (n.value == 1) { init_val_str = "1"; } else { init_val_str = "0"; }
                if (target_type_id != TYPE_BOOL) { WhitelangExceptions.throw_type_error(node.pos, "Type mismatch. "); }
            } else {
                WhitelangExceptions.throw_type_error(node.pos, "Global variable initialization must be a constant literal. ");
            }
        }
        
        file_io.write(c.output_file, global_name + " = global " + llvm_ty_str + " " + init_val_str + "\n");
        map_put(c.global_symbol_table, var_name, SymbolInfo(reg=global_name, type=target_type_id, origin_type=target_type_id, is_const=node.is_const));
        return void_result();
    }

    let ptr_reg -> String = node.alloc_reg;
    let origin_id -> Int = target_type_id;

    if (node.value is !null) {
        c.expected_type = target_type_id;
        let val_res -> CompileResult = compile_node(c, node.value);
        c.expected_type = 0;

        val_res = emit_implicit_cast(c, val_res, target_type_id, node.pos);
        if (target_type_id == TYPE_GENERIC_STRUCT && val_res.origin_type >= 100) {
            origin_id = val_res.origin_type;
        } else if (target_type_id == TYPE_GENERIC_FUNCTION && val_res.origin_type >= 100) {
            origin_id = val_res.origin_type;
        }

        if (c.scope_depth > 0) {
            emit_retain(c, val_res.reg, target_type_id);
        }

        file_io.write(c.output_file, c.indent + "store " + llvm_ty_str + " " + val_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
    }
    else {
        if (target_type_id == TYPE_GENERIC_STRUCT) {
            WhitelangExceptions.throw_missing_initializer(node.pos, "Variable of generic type 'Struct' must be initialized explicitly.");
        }

        let s_info -> StructInfo = map_get(c.struct_id_map, "" + target_type_id);
        if (s_info is !null) {
            let fake_call -> CallNode = CallNode(type=16, callee=null, args=null, pos=node.pos);
            let init_res -> CompileResult = compile_struct_init(c, s_info, fake_call);
            
            if (c.scope_depth > 0) {
                emit_retain(c, init_res.reg, target_type_id);
            }

            file_io.write(c.output_file, c.indent + "store " + llvm_ty_str + " " + init_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
        } else {
            let zero -> String = "0";
            if (target_type_id == TYPE_STRING || target_type_id >= 100 || target_type_id == TYPE_GENERIC_STRUCT || target_type_id == TYPE_GENERIC_FUNCTION) { zero = "null"; }
            if (target_type_id == TYPE_FLOAT) { zero = "0.0"; }
            file_io.write(c.output_file, c.indent + "store " + llvm_ty_str + " " + zero + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
        }
    }

    let curr_scope -> Scope = c.symbol_table;
    map_put(curr_scope.table, var_name, SymbolInfo(reg=ptr_reg, type=target_type_id, origin_type=origin_id, is_const=node.is_const));

    if (c.scope_depth > 0) {
        if (is_ref_type(c, target_type_id)) {
            curr_scope.gc_vars.append(GCTracker(reg = ptr_reg, type = target_type_id));
        }
    }

    return void_result(); 
}
func compile_var_assign(c -> Compiler, node -> VarAssignNode) -> CompileResult {
    let var_name -> String = node.name_tok.value;
    let info -> SymbolInfo = find_symbol(c, var_name);
    if (info is null) {
        WhitelangExceptions.throw_name_error(node.pos, "Undefined variable '" + var_name + "'.");
    }

    if (info.is_const) {
        WhitelangExceptions.throw_type_error(node.pos, "Cannot assign to constant variable '" + var_name + "'.");
    }

    c.expected_type = info.type;
    let val_res -> CompileResult = compile_node(c, node.value);
    c.expected_type = 0;

    val_res = emit_implicit_cast(c, val_res, info.type, node.pos);

    if (is_ref_type(c, info.type)) {
        emit_retain(c, val_res.reg, info.type);

        let old_val_reg -> String = next_reg(c);
        let ty_str -> String = get_llvm_type_str(c, info.type);
        file_io.write(c.output_file, c.indent + old_val_reg + " = load " + ty_str + ", " + ty_str + "* " + info.reg + "\n");

        emit_release(c, old_val_reg, info.type);
    }
    
    let ty_str -> String = get_llvm_type_str(c, info.type);
    file_io.write(c.output_file, c.indent + "store " + ty_str + " " + val_res.reg + ", " + ty_str + "* " + info.reg + "\n");
    return val_res; 
}

func compile_if(c -> Compiler, node -> IfNode) -> CompileResult {
    let cond_res -> CompileResult = compile_node(c, node.condition);
    if (cond_res.type != TYPE_BOOL) {
        WhitelangExceptions.throw_type_error(node.pos, "If condition must be a Bool. ");
    }
    
    let label_then -> String = next_label(c);
    let label_else -> String = next_label(c);
    let label_merge -> String = next_label(c);
    
    let target_else -> String = label_else;
    if (node.else_body is null) {
        target_else = label_merge;
    }
    
    file_io.write(c.output_file, c.indent + "br i1 " + cond_res.reg + ", label %" + label_then + ", label %" + target_else + "\n");
    
    file_io.write(c.output_file, "\n" + label_then + ":\n");
    compile_node(c, node.body);
    file_io.write(c.output_file, c.indent + "br label %" + label_merge + "\n");
    
    if (node.else_body is !null) {
        file_io.write(c.output_file, "\n" + label_else + ":\n");
        compile_node(c, node.else_body);
        file_io.write(c.output_file, c.indent + "br label %" + label_merge + "\n");
    }

    file_io.write(c.output_file, "\n" + label_merge + ":\n");
    return void_result();
}

func compile_while(c -> Compiler, node -> WhileNode) -> CompileResult {
    let label_cond -> String = next_label(c);
    let label_body -> String = next_label(c);
    let label_end  -> String = next_label(c);

    let current_scope -> LoopScope = LoopScope(label_continue=label_cond, label_break=label_end, parent=c.loop_stack);
    c.loop_stack = current_scope;

    file_io.write(c.output_file, c.indent + "br label %" + label_cond + "\n");
    file_io.write(c.output_file, "\n" + label_cond + ":\n");
    
    let cond_res -> CompileResult = compile_node(c, node.condition);
    if (cond_res.type != TYPE_BOOL) {
        WhitelangExceptions.throw_type_error(node.pos, "While condition must be a Bool. ");
    }
    file_io.write(c.output_file, c.indent + "br i1 " + cond_res.reg + ", label %" + label_body + ", label %" + label_end + "\n");

    file_io.write(c.output_file, "\n" + label_body + ":\n");
    compile_node(c, node.body);
    file_io.write(c.output_file, c.indent + "br label %" + label_cond + "\n");

    file_io.write(c.output_file, "\n" + label_end + ":\n");
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
    
    file_io.write(c.output_file, c.indent + "br label %" + label_cond + "\n");
    file_io.write(c.output_file, "\n" + label_cond + ":\n");
    if (node.cond is !null) {
        let cond_res -> CompileResult = compile_node(c, node.cond);
        if (cond_res.type != TYPE_BOOL) {
            WhitelangExceptions.throw_type_error(node.pos, "For condition must be a Bool. ");
        }
        file_io.write(c.output_file, c.indent + "br i1 " + cond_res.reg + ", label %" + label_body + ", label %" + label_end + "\n");
    } else {
        file_io.write(c.output_file, c.indent + "br label %" + label_body + "\n");
    }

    file_io.write(c.output_file, "\n" + label_body + ":\n");
    compile_node(c, node.body);

    file_io.write(c.output_file, c.indent + "br label %" + label_step + "\n");
    file_io.write(c.output_file, "\n" + label_step + ":\n");
    if (node.step is !null) {
        compile_node(c, node.step);
    }
    file_io.write(c.output_file, c.indent + "br label %" + label_cond + "\n");
    file_io.write(c.output_file, "\n" + label_end + ":\n");
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
            WhitelangExceptions.throw_type_error(node.pos, "Cannot dereference 'nullptr'."); 
        }
        let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + curr_type);
        if (base_info is null) { 
            WhitelangExceptions.throw_type_error(node.pos, "Cannot dereference non-pointer."); 
        }
        
        let next_type -> Int = base_info.type;
        if (next_type == TYPE_VOID) {
            WhitelangExceptions.throw_type_error(d_node.pos, "Cannot dereference 'ptr Void'. Cast it to a specific pointer type first.");
        }
        let ty_str -> String = get_llvm_type_str(c, next_type);
        let next_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + next_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_reg + "\n");
        
        curr_reg = next_reg;
        curr_type = next_type;
        i += 1;
    }

    if (curr_type == TYPE_NULL) {
        WhitelangExceptions.throw_null_dereference_error(node.pos, "Cannot dereference 'nullptr'. ");
    }

    let final_base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + curr_type);
    if (final_base_info is null) { 
        WhitelangExceptions.throw_type_error(node.pos, "Cannot assign to non-pointer."); 
    }
    
    let target_type_id -> Int = final_base_info.type;
    val_res = emit_implicit_cast(c, val_res, target_type_id, node.pos);
    
    let llvm_ty -> String = get_llvm_type_str(c, target_type_id);

    if (is_ref_type(c, target_type_id)) {
        // retain new value
        emit_retain(c, val_res.reg, target_type_id);
        
        // load old value
        let old_val_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + old_val_reg + " = load " + llvm_ty + ", " + llvm_ty + "* " + curr_reg + "\n");
        
        // release old value
        emit_release(c, old_val_reg, target_type_id);
    }
    
    // store new value
    file_io.write(c.output_file, c.indent + "store " + llvm_ty + " " + val_res.reg + ", " + llvm_ty + "* " + curr_reg + "\n");

    return val_res;
}

func compile_func_def(c -> Compiler, node -> FunctionDefNode) -> CompileResult {
    let raw_name -> String = node.name_tok.value;

    let func_name -> String = raw_name;
    if (raw_name != "main") {
        func_name = c.current_package_prefix + raw_name;
    }

    if (raw_name == "main") {
        c.has_main = true;
    }

    let f_info -> FuncInfo = map_get(c.func_table, func_name);
    let ret_type_id -> Int = f_info.ret_type;
    let llvm_ret_type -> String = get_llvm_type_str(c, ret_type_id);

    c.current_ret_type = ret_type_id;

    let params_str -> String = "";
    let params -> Vector(Struct) = node.params;
    let p_len -> Int = 0;
    if (params is !null) { p_len = params.length(); }
    let arg_idx -> Int = 0;
    
    while (arg_idx < p_len) {
        let p -> ParamNode = params[arg_idx];
        let p_type_id -> Int = resolve_type(c, p.type_tok);
        let p_llvm_type -> String = get_llvm_type_str(c, p_type_id);
        if (arg_idx > 0) { params_str = params_str + ", ";
        }
        params_str += p_llvm_type + " %arg" + arg_idx;
        
        arg_idx += 1;
    }

    file_io.write(c.output_file, "define " + llvm_ret_type + " @" + func_name + "(" + params_str + ") {\n");
    file_io.write(c.output_file, "entry:\n");

    let old_sym -> Scope = c.symbol_table;
    c.symbol_table = Scope(table=map_new(32), parent=null, gc_vars=[]);
    
    c.reg_count = 0; 
    c.scope_depth = 1;
    c.curr_func = f_info;
    arg_idx = 0;
    while (arg_idx < p_len) {
        let p -> ParamNode = params[arg_idx];
        let p_name -> String = p.name_tok.value;
        
        let target_type_id -> Int = resolve_type(c, p.type_tok);
        let llvm_ty -> String = get_llvm_type_str(c, target_type_id);
        let addr_reg -> String = next_reg(c); 
        file_io.write(c.output_file, c.indent + addr_reg + " = alloca " + llvm_ty + "\n");
        file_io.write(c.output_file, c.indent + "store " + llvm_ty + " %arg" + arg_idx + ", " + llvm_ty + "* " + addr_reg + "\n");
        let curr_scope -> Scope = c.symbol_table;
        map_put(curr_scope.table, p_name, SymbolInfo(reg=addr_reg, type=target_type_id, origin_type=target_type_id));
        
        arg_idx += 1;
    }

    hoist_allocas(c, node.body);

    compile_node(c, node.body);

    let block -> BlockNode = node.body;
    let stmts -> Vector(Struct) = block.stmts;
    let last_stmt -> Struct = null;
    
    if (stmts is !null) {
        let len -> Int = stmts.length();
        if (len > 0) {
            last_stmt = stmts[len - 1];
        }
    }

    let has_term -> Bool = false;
    if (last_stmt is !null) {
        let base -> BaseNode = last_stmt;
        if (base.type == NODE_RETURN) { has_term = true; }
    }

    if (!has_term) {
        if (ret_type_id == TYPE_VOID) {
            file_io.write(c.output_file, c.indent + "ret void\n");
        } else {
            let zero_val -> String = "0";
            if (ret_type_id == TYPE_FLOAT) { zero_val = "0.0"; }
            else if (ret_type_id == TYPE_STRING || ret_type_id >= 100 || ret_type_id == TYPE_GENERIC_STRUCT || ret_type_id == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
            
            file_io.write(c.output_file, c.indent + "ret " + llvm_ret_type + " " + zero_val + "\n");
        }
    }
    
    file_io.write(c.output_file, "}\n\n");

    // restore scope
    c.symbol_table = old_sym;
    c.scope_depth = 0;
    
    c.curr_func = null;
    
    return void_result();
}

func compile_method_def(c -> Compiler, class_name -> String, node -> MethodDefNode) -> CompileResult {
    let raw_name -> String = node.name_tok.value;
    let m_name -> String = c.current_package_prefix + class_name + "_" + raw_name;
    
    let f_info -> FuncInfo = map_get(c.func_table, m_name);
    let ret_type_id -> Int = f_info.ret_type;
    let llvm_ret_type -> String = get_llvm_type_str(c, ret_type_id);

    c.current_ret_type = ret_type_id;

    let c_info -> StructInfo = map_get(c.struct_table, class_name);
    let class_type_id -> Int = c_info.type_id;
    let class_ptr_llvm -> String = get_llvm_type_str(c, class_type_id); 

    let params_str -> String = class_ptr_llvm + " %arg0";
    
    let params -> Vector(Struct) = node.params;
    let p_len -> Int = 0; if (params is !null) { p_len = params.length(); }
    let arg_idx -> Int = 0;
    
    while (arg_idx < p_len) {
        let p -> ParamNode = params[arg_idx];
        let p_type_id -> Int = resolve_type(c, p.type_tok);
        let p_llvm_type -> String = get_llvm_type_str(c, p_type_id);
        let arg_num -> Int = arg_idx + 1;
        params_str = params_str + ", " + p_llvm_type + " %arg" + arg_num;
        arg_idx += 1;
    }

    file_io.write(c.output_file, "define " + llvm_ret_type + " @" + m_name + "(" + params_str + ") {\n");
    file_io.write(c.output_file, "entry:\n");

    let old_sym -> Scope = c.symbol_table;
    c.symbol_table = Scope(table=map_new(32), parent=null, gc_vars=[]);
    
    c.reg_count = 0; 
    c.scope_depth = 1;
    c.curr_func = f_info;
    
    let self_addr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + self_addr + " = alloca " + class_ptr_llvm + "\n");
    file_io.write(c.output_file, c.indent + "store " + class_ptr_llvm + " %arg0, " + class_ptr_llvm + "* " + self_addr + "\n");
    let curr_scope -> Scope = c.symbol_table;
    map_put(curr_scope.table, "self", SymbolInfo(reg=self_addr, type=class_type_id, origin_type=class_type_id, is_const=false));

    arg_idx = 0;
    while (arg_idx < p_len) {
        let p -> ParamNode = params[arg_idx];
        let p_name -> String = p.name_tok.value;
        let target_type_id -> Int = resolve_type(c, p.type_tok);
        let llvm_ty -> String = get_llvm_type_str(c, target_type_id);
        let addr_reg -> String = next_reg(c); 
        file_io.write(c.output_file, c.indent + addr_reg + " = alloca " + llvm_ty + "\n");
        let arg_num -> Int = arg_idx + 1;
        file_io.write(c.output_file, c.indent + "store " + llvm_ty + " %arg" + arg_num + ", " + llvm_ty + "* " + addr_reg + "\n");
        map_put(curr_scope.table, p_name, SymbolInfo(reg=addr_reg, type=target_type_id, origin_type=target_type_id, is_const=false));
        arg_idx += 1;
    }

    hoist_allocas(c, node.body);
    compile_node(c, node.body);

    let block -> BlockNode = node.body;
    let stmts -> Vector(Struct) = block.stmts;
    let last_stmt -> Struct = null;
    if (stmts is !null) {
        let len -> Int = stmts.length();
        if (len > 0) { last_stmt = stmts[len - 1]; }
    }

    let has_term -> Bool = false;
    if (last_stmt is !null) {
        let base -> BaseNode = last_stmt;
        if (base.type == NODE_RETURN) { has_term = true; }
    }

    if (!has_term) {
        if (ret_type_id == TYPE_VOID) {
            file_io.write(c.output_file, c.indent + "ret void\n");
        } else {
            let zero_val -> String = "0";
            if (ret_type_id == TYPE_FLOAT) { zero_val = "0.0"; }
            else if (ret_type_id == TYPE_STRING || ret_type_id >= 100 || ret_type_id == TYPE_GENERIC_STRUCT || ret_type_id == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
            file_io.write(c.output_file, c.indent + "ret " + llvm_ret_type + " " + zero_val + "\n");
        }
    }
    
    file_io.write(c.output_file, "}\n\n");
    c.symbol_table = old_sym;
    c.scope_depth = 0;
    c.curr_func = null;
    
    return void_result();
}

func compile_class_method_call(c -> Compiler, s_info -> StructInfo, obj_res -> CompileResult, method_name -> String, n_call -> CallNode) -> CompileResult {
    let c_node -> ClassDefNode = s_info.init_body;
    let methods -> Vector(Struct) = c_node.methods;
    let m_len -> Int = 0; if (methods is !null) { m_len = methods.length(); }
    
    let m_idx -> Int = 0;
    let found -> Bool = false;
    while (m_idx < m_len) {
        let m -> MethodDefNode = methods[m_idx];
        if (m.name_tok.value == method_name) {
            found = true;
            break;
        }
        m_idx += 1;
    }
    
    if (!found) {
        WhitelangExceptions.throw_name_error(n_call.pos, "Method '" + method_name + "' not found in class '" + s_info.name + "'.");
    }
    
    let full_m_name -> String = c.current_package_prefix + s_info.name + "_" + method_name;
    let f_info -> FuncInfo = map_get(c.func_table, full_m_name);
    let sig -> String = get_func_sig_str(c, f_info); 
    
    let class_llvm_ty -> String = s_info.llvm_name;
    let obj_ptr -> String = obj_res.reg;
    
    let vptr_addr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + vptr_addr + " = getelementptr inbounds " + class_llvm_ty + ", " + class_llvm_ty + "* " + obj_ptr + ", i32 0, i32 0\n");
    let vtable_i8ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + vtable_i8ptr + " = load i8*, i8** " + vptr_addr + "\n");
    
    let vtable_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + vtable_ptr + " = bitcast i8* " + vtable_i8ptr + " to %vtable_type." + s_info.name + "*\n");
    
    let method_i8ptr_addr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + method_i8ptr_addr + " = getelementptr inbounds %vtable_type." + s_info.name + ", %vtable_type." + s_info.name + "* " + vtable_ptr + ", i32 0, i32 " + m_idx + "\n");
    let method_i8ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + method_i8ptr + " = load i8*, i8** " + method_i8ptr_addr + "\n");
    
    let func_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + func_ptr + " = bitcast i8* " + method_i8ptr + " to " + sig + "*\n");
    
    let args_str -> String = class_llvm_ty + "* " + obj_ptr; 
    let args -> Vector(Struct) = n_call.args;
    let a_len -> Int = 0; if (args is !null) { a_len = args.length(); }
    let arg_idx -> Int = 0;
    let expected_types -> Vector(Struct) = f_info.arg_types;
    
    while (arg_idx < a_len) {
        let arg_node_curr -> ArgNode = args[arg_idx];
        let expected_type_node -> TypeListNode = expected_types[arg_idx + 1];
        let expected_type -> Int = expected_type_node.type;
        
        c.expected_type = expected_type;
        let arg_val -> CompileResult = compile_node(c, arg_node_curr.val);
        c.expected_type = 0;
        arg_val = emit_implicit_cast(c, arg_val, expected_type, n_call.pos);
        
        let ty_str -> String = get_llvm_type_str(c, arg_val.type);
        args_str = args_str + ", " + ty_str + " " + arg_val.reg;
        arg_idx += 1;
    }
    
    let llvm_ret_type -> String = get_llvm_type_str(c, f_info.ret_type);
    if (f_info.ret_type == TYPE_VOID) {
        file_io.write(c.output_file, c.indent + "call " + llvm_ret_type + " " + func_ptr + "(" + args_str + ")\n");
        return CompileResult(reg="", type=TYPE_VOID, origin_type=0);
    } else {
        let call_res -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + call_res + " = call " + llvm_ret_type + " " + func_ptr + "(" + args_str + ")\n");
        return CompileResult(reg=call_res, type=f_info.ret_type, origin_type=0);
    }
}

func compile_local_closure(c -> Compiler, func_def -> FunctionDefNode) -> CompileResult {
    let scope -> CaptureScope = CaptureScope(local_vars=map_new(32), captured_vars=map_new(32), captured_list=[]);
    let params -> Vector(Struct) = func_def.params;
    let p_len -> Int = 0; if (params is !null) { p_len = params.length(); }
    let p_i -> Int = 0;
    while (p_i < p_len) {
        let p_node -> ParamNode = params[p_i];
        map_put(scope.local_vars, p_node.name_tok.value, TypeListNode(type=1));
        p_i += 1;
    }
    
    analyze_captures(func_def.body, scope);
    
    let captures -> Vector(String) = [];
    let capture_types -> Vector(Struct) = [];
    
    let c_i -> Int = 0;
    let cap_len -> Int = scope.captured_list.length();
    while (c_i < cap_len) {
        let v_name -> String = scope.captured_list[c_i];
        let is_global -> Bool = false;
        if (map_get(c.global_symbol_table, v_name) is !null) { is_global = true; }
        if (map_get(c.func_table, v_name) is !null) { is_global = true; }
        if (map_get(c.struct_table, v_name) is !null) { is_global = true; }
        if (map_get(c.loaded_packages, v_name) is !null) { is_global = true; }
        if (map_get(c.loaded_files, v_name) is !null) { is_global = true; }

        if (!is_global) {
            let info -> SymbolInfo = find_symbol(c, v_name);
            if (info is null) {
                WhitelangExceptions.throw_name_error(func_def.pos, "Cannot capture undefined variable '" + v_name + "'.");
            }
            captures.append(v_name); 
            capture_types.append(TypeListNode(type=info.type));
        }
        c_i += 1;
    }

    let env_id -> Int = c.type_counter;
    c.type_counter += 1;

    let t_len -> Int = captures.length();
    let env_struct_name -> String = "env." + env_id;
    let env_body -> String = "";
    let env_fields -> Vector(Struct) = [];
    let t_i -> Int = 0;
    while (t_i < t_len) {
        let t_node -> TypeListNode = capture_types[t_i];
        let f_llvm -> String = get_llvm_type_str(c, t_node.type);
        if (t_i > 0) { env_body += ", "; }
        env_body += f_llvm;
        env_fields.append(FieldInfo(name=captures[t_i], type=t_node.type, llvm_type=f_llvm, offset=t_i));
        t_i += 1;
    }
    let llvm_env_name -> String = "{ " + env_body + " }";

    let env_info -> StructInfo = StructInfo(name=env_struct_name, type_id=env_id, fields=env_fields, llvm_name=llvm_env_name, init_body=null);
    map_put(c.struct_id_map, "" + env_id, env_info);
    c.type_drop_list.append(TypeListNode(type=env_id));

    let size_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr " + llvm_env_name + ", " + llvm_env_name + "* null, i32 1\n");
    let size_i64 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_i64 + " = ptrtoint " + llvm_env_name + "* " + size_ptr + " to i64\n");
    let env_payload -> String = emit_alloc_obj(c, size_i64, "" + env_id, llvm_env_name + "*");

    let env_payload_i8 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + env_payload_i8 + " = bitcast " + llvm_env_name + "* " + env_payload + " to i8*\n");

    t_i = 0;
    while (t_i < t_len) {
        let v_name -> String = captures[t_i];
        let t_node -> TypeListNode = capture_types[t_i];
        let v_type -> Int = t_node.type;
        let llvm_ty -> String = get_llvm_type_str(c, v_type);
        let info -> SymbolInfo = find_symbol(c, v_name);
        
        let val_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + val_reg + " = load " + llvm_ty + ", " + llvm_ty + "* " + info.reg + "\n");
        if (is_ref_type(c, v_type)) { emit_retain(c, val_reg, v_type); }
        
        let slot_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + llvm_env_name + ", " + llvm_env_name + "* " + env_payload + ", i32 0, i32 " + t_i + "\n");
        file_io.write(c.output_file, c.indent + "store " + llvm_ty + " " + val_reg + ", " + llvm_ty + "* " + slot_ptr + "\n");
        t_i += 1;
    }

    let old_file -> File = c.output_file;
    let tmp_name -> String = ".lambda_" + env_id + ".ll";
    c.output_file = file_io.open(tmp_name, "w");
    
    let lambda_name -> String = "lambda." + func_def.name_tok.value + "." + env_id;
    let ret_type_id -> Int = resolve_type(c, func_def.ret_type_tok);
    let ret_ty_str -> String = get_llvm_type_str(c, ret_type_id);
    let specific_type_id -> Int = get_func_type_id(c, ret_type_id);
    
    let sig_def -> String = "i8* %raw_env";
    let sig_ty -> String = "i8*";
    p_i = 0;
    while (p_i < p_len) {
        let p_node -> ParamNode = params[p_i];
        let p_ty -> String = get_llvm_type_str(c, resolve_type(c, p_node.type_tok));
        sig_def = sig_def + ", " + p_ty + " %arg" + p_i;
        sig_ty = sig_ty + ", " + p_ty;
        p_i += 1;
    }
    
    file_io.write(c.output_file, "define " + ret_ty_str + " @" + lambda_name + "(" + sig_def + ") {\nentry:\n");
    let old_sym -> Scope = c.symbol_table;
    let old_depth -> Int = c.scope_depth;
    let old_reg -> Int = c.reg_count;
    let old_ret -> Int = c.current_ret_type;
    
    c.symbol_table = Scope(table=map_new(32), parent=null, gc_vars=[]);
    c.scope_depth = 1;
    c.reg_count = 1;
    c.current_ret_type = ret_type_id;

    let lambda_env_ptr -> String = "%lambda_env_ptr";
    file_io.write(c.output_file, "  " + lambda_env_ptr + " = bitcast i8* %raw_env to " + llvm_env_name + "*\n");

    t_i = 0;
    while (t_i < t_len) {
        let v_name -> String = captures[t_i];
        let t_node -> TypeListNode = capture_types[t_i];
        let v_type -> Int = t_node.type;
        let llvm_ty -> String = get_llvm_type_str(c, v_type);

        let slot_ptr -> String = "%env.slot." + t_i;
        file_io.write(c.output_file, "  " + slot_ptr + " = getelementptr inbounds " + llvm_env_name + ", " + llvm_env_name + "* " + lambda_env_ptr + ", i32 0, i32 " + t_i + "\n");

        map_put(c.symbol_table.table, v_name, SymbolInfo(reg=slot_ptr, type=v_type, origin_type=v_type, is_const=false));
        t_i += 1;
    }
    
    p_i = 0;
    while (p_i < p_len) {
        let p_node -> ParamNode = params[p_i];
        let p_ty_id -> Int = resolve_type(c, p_node.type_tok);
        let p_ty -> String = get_llvm_type_str(c, p_ty_id);
        let addr_reg -> String = next_reg(c);
        file_io.write(c.output_file, "  " + addr_reg + " = alloca " + p_ty + "\n");
        file_io.write(c.output_file, "  store " + p_ty + " %arg" + p_i + ", " + p_ty + "* " + addr_reg + "\n");
        map_put(c.symbol_table.table, p_node.name_tok.value, SymbolInfo(reg=addr_reg, type=p_ty_id, origin_type=p_ty_id, is_const=false));
        p_i += 1;
    }
    
    hoist_allocas(c, func_def.body);
    compile_node(c, func_def.body);
    
    if (ret_type_id == TYPE_VOID) {
        file_io.write(c.output_file, "  ret void\n");
    } else {
        let zero_val -> String = "0";
        if (ret_type_id == TYPE_FLOAT) { zero_val = "0.0"; }
        else if (ret_type_id == TYPE_STRING || ret_type_id >= 100 || ret_type_id == TYPE_GENERIC_STRUCT || ret_type_id == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
        file_io.write(c.output_file, "  ret " + ret_ty_str + " " + zero_val + "\n");
    }
    file_io.write(c.output_file, "}\n\n");
    
    c.symbol_table = old_sym;
    c.scope_depth = old_depth;
    c.reg_count = old_reg;
    c.current_ret_type = old_ret;
    
    file_io.close(c.output_file);
    c.output_file = old_file;
    let tmp_read -> File = file_io.open(tmp_name, "rb");
    let lambda_ir -> String = file_io.read_all(tmp_read);
    file_io.close(tmp_read);
    file_io.remove_file(tmp_name);
    c.global_buffer = c.global_buffer + lambda_ir;

    let clo_payload -> String = emit_alloc_obj(c, "16", "8", "i8*");

    let clo_func_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + clo_func_ptr + " = bitcast i8* " + clo_payload + " to i8**\n");
    
    let lambda_casted -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + lambda_casted + " = bitcast " + ret_ty_str + " (" + sig_ty + ")* @" + lambda_name + " to i8*\n");
    file_io.write(c.output_file, c.indent + "store i8* " + lambda_casted + ", i8** " + clo_func_ptr + "\n");

    let clo_env_ptr_i8 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + clo_env_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_payload + ", i32 8\n");
    let clo_env_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + clo_env_ptr + " = bitcast i8* " + clo_env_ptr_i8 + " to i8**\n");
    file_io.write(c.output_file, c.indent + "store i8* " + env_payload_i8 + ", i8** " + clo_env_ptr + "\n");

    let local_alloc -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + local_alloc + " = alloca i8*\n");
    file_io.write(c.output_file, c.indent + "store i8* " + clo_payload + ", i8** " + local_alloc + "\n");

    emit_retain(c, clo_payload, specific_type_id);
    map_put(c.symbol_table.table, func_def.name_tok.value, SymbolInfo(reg=local_alloc, type=specific_type_id, origin_type=ret_type_id, is_const=true));
    c.symbol_table.gc_vars.append(GCTracker(reg=local_alloc, type=specific_type_id));

    return CompileResult(reg=clo_payload, type=specific_type_id, origin_type=ret_type_id);
}

func compile_return(c -> Compiler, node -> ReturnNode) -> CompileResult {
    if (node.value is !null) {
        // return void check
        if (c.current_ret_type == TYPE_VOID) {
            WhitelangExceptions.throw_type_error(node.pos, "Void function cannot return a value. ");
        }

        c.expected_type = c.current_ret_type;
        let res -> CompileResult = compile_node(c, node.value);
        c.expected_type = 0;

        let ret_val_reg -> String = res.reg;
        let target_ty -> String = get_llvm_type_str(c, c.current_ret_type);

        if (res.type == TYPE_NULLPTR) {
            if (is_pointer_type(c, c.current_ret_type) == false) {
                WhitelangExceptions.throw_type_error(node.pos, "nullptr can only be returned for explicit pointer types.");
            }
            res.type = c.current_ret_type;
        } else if (res.type == TYPE_NULL) {
            if (is_pointer_type(c, c.current_ret_type) == true) {
                WhitelangExceptions.throw_type_error(node.pos, "null cannot be returned for explicit pointer types. Use 'nullptr'.");
            }
            if (c.current_ret_type == TYPE_INT || c.current_ret_type == TYPE_FLOAT || c.current_ret_type == TYPE_BOOL || c.current_ret_type == TYPE_BYTE) {
                WhitelangExceptions.throw_type_error(node.pos, "Primitive types cannot be null.");
            }
            res.type = c.current_ret_type;
        }

        res = emit_implicit_cast(c, res, c.current_ret_type, node.pos);
        let ret_val_reg -> String = res.reg;

        if (is_ref_type(c, c.current_ret_type)) {
            emit_retain(c, ret_val_reg, c.current_ret_type);
        }

        cleanup_all_scopes(c);

        file_io.write(c.output_file, c.indent + "ret " + target_ty + " " + ret_val_reg + "\n");
    } else {
        if (c.current_ret_type != TYPE_VOID) {
            WhitelangExceptions.throw_type_error(node.pos, "Non-void function must return a value. ");
        }
        cleanup_all_scopes(c);
        file_io.write(c.output_file, c.indent + "ret void\n");
    }
    
    return void_result();
}

func compile_struct_def(c -> Compiler, node -> StructDefNode) -> CompileResult {
    let struct_name -> String = node.name_tok.value;

    let full_name -> String = "struct." + struct_name;
    if (map_get(c.struct_table, full_name) is !null) {
        WhitelangExceptions.throw_import_error(node.pos, "Struct '" + struct_name + "' is already defined in another module.");
    }

    let info -> StructInfo = map_get(c.struct_table, struct_name);
    if (info is null) {
        WhitelangExceptions.throw_type_error(node.pos, "Struct info missing for '" + struct_name + "'.");
    }

    let llvm_body -> String = "";
    let fields_vec -> Vector(Struct) = [];
    
    let fields -> Vector(Struct) = node.fields;
    let f_len -> Int = 0; if (fields is !null) { f_len = fields.length(); }
    let idx -> Int = 0;
    
    while (idx < f_len) {
        let p -> ParamNode = fields[idx];
        let f_name -> String = p.name_tok.value;
        
        let f_type_id -> Int = resolve_type(c, p.type_tok);
        let f_llvm_type -> String = get_llvm_type_str(c, f_type_id);
        if (idx > 0) { llvm_body = llvm_body + ", "; }
        llvm_body += f_llvm_type;
        
        fields_vec.append(FieldInfo(name=f_name, type=f_type_id, llvm_type=f_llvm_type, offset=idx));
        idx += 1;
    }

    info.fields = fields_vec;

    // %struct.Test = type { i32, i32 }
    let def_str -> String = info.llvm_name + " = type { " + llvm_body + " }\n\n";
    file_io.write(c.output_file, def_str);
    
    return void_result();
}

func compile_struct_init(c -> Compiler, s_info -> StructInfo, n_call -> CallNode) -> CompileResult {
    let size_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr " + s_info.llvm_name + ", " + s_info.llvm_name + "* null, i64 1\n");
    let size_i64 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_i64 + " = ptrtoint " + s_info.llvm_name + "* " + size_ptr + " to i64\n");
    let obj_ptr -> String = emit_alloc_obj(c, size_i64, "" + s_info.type_id, s_info.llvm_name + "*");

    let fields_vec -> Vector(Struct) = s_info.fields;
    let f_len -> Int = 0;
    if (fields_vec is !null) { f_len = fields_vec.length(); }
    let f_idx -> Int = 0;
    
    while (f_idx < f_len) {
        let f_curr -> FieldInfo = fields_vec[f_idx];
        let f_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_ptr + ", i32 0, i32 " + f_curr.offset + "\n");
        let zero_val -> String = "0";
        if (f_curr.type == TYPE_FLOAT) { zero_val = "0.0"; }
        else if (f_curr.type == TYPE_STRING || f_curr.type >= 100 || f_curr.type == TYPE_GENERIC_STRUCT || f_curr.type == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
        
        file_io.write(c.output_file, c.indent + "store " + f_curr.llvm_type + " " + zero_val + ", " + f_curr.llvm_type + "* " + f_ptr + "\n");
        
        f_idx += 1;
    }

    if (s_info.init_body is !null) {
        enter_scope(c);
        let this_ptr_addr -> String = next_reg(c);
        let struct_ptr_ty -> String = s_info.llvm_name + "*";
        
        file_io.write(c.output_file, c.indent + this_ptr_addr + " = alloca " + struct_ptr_ty + "\n");
        file_io.write(c.output_file, c.indent + "store " + struct_ptr_ty + " " + obj_ptr + ", " + struct_ptr_ty + "* " + this_ptr_addr + "\n");
        map_put(c.symbol_table.table, "this", SymbolInfo(reg=this_ptr_addr, type=s_info.type_id, origin_type=s_info.type_id));
        compile_node(c, s_info.init_body);
        exit_scope(c);
    }

    let args -> Vector(Struct) = n_call.args;
    let a_len -> Int = 0;
    if (args is !null) { a_len = args.length(); }
    let arg_idx -> Int = 0;
    
    while (arg_idx < a_len) {
        let arg_curr -> ArgNode = args[arg_idx];

        let target_f -> FieldInfo = null;
        if (arg_curr.name is !null) { target_f = find_field(s_info, arg_curr.name); } 
        else { target_f = get_field_by_index(s_info, arg_idx); }

        if (target_f is !null) { c.expected_type = target_f.type; }
        let val_res -> CompileResult = compile_node(c, arg_curr.val);
        c.expected_type = 0;

        if (target_f is !null) {
            val_res = emit_implicit_cast(c, val_res, target_f.type, n_call.pos);

            let f_ptr -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_ptr + ", i32 0, i32 " + target_f.offset + "\n");
            if (is_ref_type(c, target_f.type)) {
                emit_retain(c, val_res.reg, target_f.type);
            }
            file_io.write(c.output_file, c.indent + "store " + target_f.llvm_type + " " + val_res.reg + ", " + target_f.llvm_type + "* " + f_ptr + "\n");
        }
        arg_idx += 1;
    }
    return CompileResult(reg=obj_ptr, type=s_info.type_id);
}

func compile_class_init(c -> Compiler, s_info -> StructInfo, n_call -> CallNode) -> CompileResult {
    let size_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr " + s_info.llvm_name + ", " + s_info.llvm_name + "* null, i64 1\n");
    let size_i64 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_i64 + " = ptrtoint " + s_info.llvm_name + "* " + size_ptr + " to i64\n");
    let obj_ptr -> String = emit_alloc_obj(c, size_i64, "" + s_info.type_id, s_info.llvm_name + "*");

    let fields_vec -> Vector(Struct) = s_info.fields;
    let f_len -> Int = 0;
    if (fields_vec is !null) { f_len = fields_vec.length(); }
    let f_idx -> Int = 0;

    while (f_idx < f_len) {
        let f_curr -> FieldInfo = fields_vec[f_idx];
        let f_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_ptr + ", i32 0, i32 " + f_curr.offset + "\n");
        
        if (f_curr.name == "_vptr") {
            let vtable_cast -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + vtable_cast + " = bitcast %vtable_type." + s_info.name + "* " + s_info.vtable_name + " to i8*\n");
            file_io.write(c.output_file, c.indent + "store i8* " + vtable_cast + ", i8** " + f_ptr + "\n");
        } else {
            let zero_val -> String = "0";
            if (f_curr.type == TYPE_FLOAT) { zero_val = "0.0"; }
            else if (f_curr.type == TYPE_STRING || f_curr.type >= 100 || f_curr.type == TYPE_GENERIC_STRUCT || f_curr.type == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
            
            file_io.write(c.output_file, c.indent + "store " + f_curr.llvm_type + " " + zero_val + ", " + f_curr.llvm_type + "* " + f_ptr + "\n");
        }
        f_idx += 1;
    }

    let init_name -> String = c.current_package_prefix + s_info.name + "_$init";
    let init_func -> FuncInfo = map_get(c.func_table, init_name);
    
    if (init_func is !null) {
        let args_str -> String = s_info.llvm_name + "* " + obj_ptr;
        let args -> Vector(Struct) = n_call.args;
        let a_len -> Int = 0; if (args is !null) { a_len = args.length(); }
        let arg_idx -> Int = 0;
        let arg_types -> Vector(Struct) = init_func.arg_types;
        
        let expected_arg_count -> Int = 0;
        if (arg_types is !null) { expected_arg_count = arg_types.length() - 1; }
        
        if (a_len != expected_arg_count) {
            WhitelangExceptions.throw_type_error(n_call.pos, "Class init expects " + expected_arg_count + " arguments, got " + a_len + ".");
        }

        while (arg_idx < a_len) {
            let arg_node_curr -> ArgNode = args[arg_idx];
            let type_node_curr -> TypeListNode = arg_types[arg_idx + 1]; // +1 skip self
            let expected_type -> Int = type_node_curr.type;

            c.expected_type = expected_type;
            let arg_val -> CompileResult = compile_node(c, arg_node_curr.val);
            c.expected_type = 0;
            arg_val = emit_implicit_cast(c, arg_val, expected_type, n_call.pos);

            let ty_str -> String = get_llvm_type_str(c, arg_val.type);
            args_str = args_str + ", " + ty_str + " " + arg_val.reg;
            arg_idx += 1;
        }

        file_io.write(c.output_file, c.indent + "call void @" + init_name + "(" + args_str + ")\n");
    } else {
        let args -> Vector(Struct) = n_call.args;
        let a_len -> Int = 0; if (args is !null) { a_len = args.length(); }
        if (a_len > 0) {
            WhitelangExceptions.throw_type_error(n_call.pos, "Class '" + s_info.name + "' has no init method, but arguments were provided.");
        }
    }

    return CompileResult(reg=obj_ptr, type=s_info.type_id);
}

func compile_class_def(c -> Compiler, node -> ClassDefNode) -> CompileResult {
    let class_name -> String = node.name_tok.value;
    let full_name -> String = "class." + class_name;
    if (map_get(c.struct_table, full_name) is !null) {
        WhitelangExceptions.throw_import_error(node.pos, "Class '" + class_name + "' is already defined.");
    }

    let info -> StructInfo = map_get(c.struct_table, class_name);
    if (info is null) {
        WhitelangExceptions.throw_type_error(node.pos, "Class info missing for '" + class_name + "'.");
    }

    let llvm_body -> String = "i8*"; 
    let fields_vec -> Vector(Struct) = [];
    fields_vec.append(FieldInfo(name="_vptr", type=TYPE_STRING, llvm_type="i8*", offset=0));

    let fields -> Vector(Struct) = node.fields;
    let f_len -> Int = 0; if (fields is !null) { f_len = fields.length(); }
    let idx -> Int = 0;
    
    while (idx < f_len) {
        let p -> VarDeclareNode = fields[idx];
        let f_name -> String = p.name_tok.value;
        let f_type_id -> Int = resolve_type(c, p.type_node);
        let f_llvm_type -> String = get_llvm_type_str(c, f_type_id);
        
        llvm_body = llvm_body + ", " + f_llvm_type;
        fields_vec.append(FieldInfo(name=f_name, type=f_type_id, llvm_type=f_llvm_type, offset=idx + 1));
        idx += 1;
    }

    info.fields = fields_vec;

    let def_str -> String = info.llvm_name + " = type { " + llvm_body + " }\n";
    file_io.write(c.output_file, def_str);

    let methods -> Vector(Struct) = node.methods;
    let m_len -> Int = 0; if (methods is !null) { m_len = methods.length(); }
    file_io.write(c.output_file, "%vtable_type." + class_name + " = type [ " + m_len + " x i8* ]\n");

    let vt_str -> String = info.vtable_name + " = global %vtable_type." + class_name;
    if (m_len == 0) {
        vt_str += " zeroinitializer\n\n";
    } else {
        vt_str += " [ ";
        let m_idx -> Int = 0;
        while (m_idx < m_len) {
            let m_node -> MethodDefNode = methods[m_idx];
            let m_name -> String = c.current_package_prefix + class_name + "_" + m_node.name_tok.value;
            let f_info -> FuncInfo = map_get(c.func_table, m_name);
            let sig -> String = get_func_sig_str(c, f_info);
            
            if (m_idx > 0) { vt_str += ", "; }
            vt_str += "i8* bitcast (" + sig + " @" + m_name + " to i8*)";
            m_idx += 1;
        }
        vt_str += " ]\n\n";
    }
    file_io.write(c.output_file, vt_str);

    let m_idx2 -> Int = 0;
    while (m_idx2 < m_len) {
        let m_node -> MethodDefNode = methods[m_idx2];
        compile_method_def(c, class_name, m_node);
        m_idx2 += 1;
    }
    
    return void_result();
}

func compile_field_access(c -> Compiler, node -> FieldAccessNode) -> CompileResult {
    let obj_res -> CompileResult = compile_node(c, node.obj);

    if (is_pointer_type(c, obj_res.type)) {
        let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + obj_res.type);
        if (base_info is !null) {
            let s_check -> StructInfo = map_get(c.struct_id_map, "" + base_info.type);
            if (s_check is !null) {
                let ptr_is_null -> String = next_reg(c);
                let ptr_ty_str -> String = get_llvm_type_str(c, obj_res.type);
                file_io.write(c.output_file, c.indent + ptr_is_null + " = icmp eq " + ptr_ty_str + " " + obj_res.reg + ", null\n");
                
                let label_ok -> String = "ptr_ok_" + c.reg_count;
                let label_fail -> String = "ptr_fail_" + c.reg_count;
                c.reg_count += 1;
                
                file_io.write(c.output_file, c.indent + "br i1 " + ptr_is_null + ", label %" + label_fail + ", label %" + label_ok + "\n");
                file_io.write(c.output_file, "\n" + label_fail + ":\n");
                emit_runtime_error(c, node.pos, "Null pointer dereference");
                
                file_io.write(c.output_file, "\n" + label_ok + ":\n");

                let loaded_reg -> String = next_reg(c);
                let base_ty_str -> String = get_llvm_type_str(c, base_info.type);
                file_io.write(c.output_file, c.indent + loaded_reg + " = load " + base_ty_str + ", " + base_ty_str + "* " + obj_res.reg + "\n");

                obj_res.reg = loaded_reg;
                obj_res.type = base_info.type;
            }
        }
    }

    let type_id -> Int = obj_res.type;
    let obj_reg -> String = obj_res.reg;


    let is_null -> String = next_reg(c);
    let obj_llvm_ty -> String = get_llvm_type_str(c, obj_res.type);
    file_io.write(c.output_file, c.indent + is_null + " = icmp eq " + obj_llvm_ty + " " + obj_reg + ", null\n");
    let label_ok -> String = "access_ok_" + c.reg_count;
    let label_fail -> String = "access_fail_" + c.reg_count;
    c.reg_count += 1;
    file_io.write(c.output_file, c.indent + "br i1 " + is_null + ", label %" + label_fail + ", label %" + label_ok + "\n");
    
    file_io.write(c.output_file, "\n" + label_fail + ":\n");
    emit_runtime_error(c, node.pos, "Null pointer dereference");
    
    file_io.write(c.output_file, "\n" + label_ok + ":\n");


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
                    file_io.write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + obj_reg + " to " + s_info_temp.llvm_name + "*\n");
                    obj_reg = cast_reg;
                }
            }
        }
    }

    let s_info -> StructInfo = map_get(c.struct_id_map, "" + type_id);
    if (s_info is null) {
        WhitelangExceptions.throw_type_error(node.pos, "Cannot access field on non-struct type (or generic Struct without origin inference).");
    }
    
    let field -> FieldInfo = find_field(s_info, node.field_name);
    if (field is null) {
        WhitelangExceptions.throw_name_error(node.pos, "Field '" + node.field_name + "' not found in struct '" + s_info.name + "'.");
    }
    
    let f_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_reg + ", i32 0, i32 " + field.offset + "\n");
    let val_reg -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + val_reg + " = load " + field.llvm_type + ", " + field.llvm_type + "* " + f_ptr + "\n");
    return CompileResult(reg=val_reg, type=field.type);
}

func compile_field_assign(c -> Compiler, node -> FieldAssignNode) -> CompileResult {
    let obj_base -> BaseNode = node.obj;
    if (obj_base.type == NODE_CALL || obj_base.type == NODE_VECTOR_LIT || obj_base.type == NODE_STRING) {
        WhitelangExceptions.throw_invalid_syntax(node.pos, "Cannot assign to a field of a temporary right-value object. Assign it to a variable first.");
    }
    
    let obj_res -> CompileResult = compile_node(c, node.obj);
    let struct_type_id -> Int = obj_res.type;
    let struct_ptr_reg -> String = obj_res.reg;

    if (struct_type_id == TYPE_GENERIC_STRUCT && obj_res.origin_type >= 100) {
        struct_type_id = obj_res.origin_type;
        let s_info_temp -> StructInfo = map_get(c.struct_id_map, "" + struct_type_id);
        if (s_info_temp is !null) {
            let cast_reg -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + struct_ptr_reg + " to " + s_info_temp.llvm_name + "*\n");
            struct_ptr_reg = cast_reg;
        }
    }

    let s_info -> StructInfo = map_get(c.struct_id_map, "" + struct_type_id);
    if (s_info is null) {
        WhitelangExceptions.throw_type_error(node.pos, "Cannot assign field to non-struct type.");
    }

    let field -> FieldInfo = find_field(s_info, node.field_name);
    if (field is null) {
        WhitelangExceptions.throw_name_error(node.pos, "Field '" + node.field_name + "' not found in struct '" + s_info.name + "'.");
    }

    let val_res -> CompileResult = compile_node(c, node.value);

    val_res = emit_implicit_cast(c, val_res, field.type, node.pos);
    
    let f_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + struct_ptr_reg + ", i32 0, i32 " + field.offset + "\n");

    if (is_ref_type(c, field.type)) {
        emit_retain(c, val_res.reg, field.type);

        let old_val_reg -> String = next_reg(c);
        let field_ty_str -> String = get_llvm_type_str(c, field.type);
        file_io.write(c.output_file, c.indent + old_val_reg + " = load " + field_ty_str + ", " + field_ty_str + "* " + f_ptr + "\n");

        emit_release(c, old_val_reg, field.type);
    }

    let store_ty -> String = get_llvm_type_str(c, field.type);
    file_io.write(c.output_file, c.indent + "store " + store_ty + " " + val_res.reg + ", " + store_ty + "* " + f_ptr + "\n");
    return val_res;
}

func compile_extern_func(c -> Compiler, node -> ExternFuncNode) -> CompileResult {
    let func_name -> String = node.name_tok.value;
    let ret_type_id -> Int = resolve_type(c, node.ret_type_tok);
    
    let arg_types -> Vector(Struct) = [];
    
    let params -> Vector(Struct) = node.params;
    let p_len -> Int = 0; if (params is !null) { p_len = params.length(); }
    let p_idx -> Int = 0;
    
    let params_str -> String = "";
    let first -> Bool = true;
    
    while (p_idx < p_len) {
        let p -> ParamNode = params[p_idx];
        let p_id -> Int = resolve_type(c, p.type_tok);
        
        arg_types.append(TypeListNode(type=p_id));
        
        if (!first) { params_str = params_str + ", "; }
        params_str += get_llvm_type_str(c, p_id);
        first = false;
        
        p_idx += 1;
    }

    if (node.is_varargs) {
        if (!first) { params_str = params_str + ", "; }
        params_str = params_str + "...";
    }

    map_put(c.func_table, func_name, FuncInfo(name=func_name, ret_type=ret_type_id, arg_types=arg_types, is_varargs=node.is_varargs));
    if (map_get(c.declared_externs, func_name) is null) {
        let ret_llvm -> String = get_llvm_type_str(c, ret_type_id);
        file_io.write(c.output_file, "declare " + ret_llvm + " @" + func_name + "(" + params_str + ")\n");

        map_put(c.declared_externs, func_name, StringConstant(id=0, value="")); 
    }
    return void_result();
}

func compile_extern_block(c -> Compiler, node -> ExternBlockNode) -> CompileResult {
    let funcs -> Vector(Struct) = node.funcs;
    let len -> Int = 0; if (funcs is !null) { len = funcs.length(); }
    let i -> Int = 0;
    while (i < len) {
        let f_node -> ExternFuncNode = funcs[i];
        compile_extern_func(c, f_node);
        i += 1;
    }
    return void_result();
}

func compile_vector_append(c -> Compiler, vec_node -> Struct, call_node -> CallNode) -> CompileResult {
    let args -> Vector(Struct) = call_node.args;
    let a_len -> Int = 0;
    if (args is !null) { a_len = args.length(); }
    if (a_len != 1) { WhitelangExceptions.throw_type_error(call_node.pos, "append expects exactly 1 argument."); }
    
    let arg_node -> ArgNode = args[0];
    let vec_res -> CompileResult = compile_node(c, vec_node);
    let arg_res -> CompileResult = compile_node(c, arg_node.val);

    let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + vec_res.type);
    if (v_info is null) { WhitelangExceptions.throw_type_error(call_node.pos, "append is only for Vectors."); }
    
    let elem_type -> Int = v_info.type;
    arg_res = emit_implicit_cast(c, arg_res, elem_type, call_node.pos);
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
    let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

    let size_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");
    
    let cap_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + cap_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 1\n");
    let cap_val -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + cap_val + " = load i64, i64* " + cap_ptr + "\n");

    let cmp_reg -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + cmp_reg + " = icmp uge i64 " + size_val + ", " + cap_val + "\n");
    
    let grow_label -> String = "vec_grow_" + c.type_counter;
    let push_label -> String = "vec_push_" + c.type_counter;
    c.type_counter += 1;
    
    file_io.write(c.output_file, c.indent + "br i1 " + cmp_reg + ", label %" + grow_label + ", label %" + push_label + "\n");

    file_io.write(c.output_file, "\n" + grow_label + ":\n");
    
    // new_cap = (cap == 0) ? 4 : cap * 2
    let is_zero_cap -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + is_zero_cap + " = icmp eq i64 " + cap_val + ", 0\n");
    let dbl_cap -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + dbl_cap + " = mul i64 " + cap_val + ", 2\n");
    let new_cap -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + new_cap + " = select i1 " + is_zero_cap + ", i64 4, i64 " + dbl_cap + "\n");
    
    file_io.write(c.output_file, c.indent + "store i64 " + new_cap + ", i64* " + cap_ptr + "\n");
    
    // realloc
    let data_field_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 2\n");
    let old_data -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + old_data + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    let old_data_i8 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + old_data_i8 + " = bitcast " + elem_ty_str + "* " + old_data + " to i8*\n");

    let elem_size -> Int = 8; 
    if (elem_type == TYPE_INT) { elem_size = 4; } 
    if (elem_type == TYPE_BYTE || elem_type == TYPE_BOOL) { elem_size = 1; }
    
    let new_bytes -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + new_bytes + " = mul i64 " + new_cap + ", " + elem_size + "\n");
    
    let new_data_i8 -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + new_data_i8 + " = call i8* @realloc(i8* " + old_data_i8 + ", i64 " + new_bytes + ")\n");
    
    let new_data_typed -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + new_data_typed + " = bitcast i8* " + new_data_i8 + " to " + elem_ty_str + "*\n");
    file_io.write(c.output_file, c.indent + "store " + elem_ty_str + "* " + new_data_typed + ", " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    file_io.write(c.output_file, c.indent + "br label %" + push_label + "\n");
    file_io.write(c.output_file, "\n" + push_label + ":\n");
    
    let final_data_field_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + final_data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 2\n");
    let final_data -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + final_data + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + final_data_field_ptr + "\n");
    
    let slot_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + final_data + ", i64 " + size_val + "\n");
    
    if (is_ref_type(c, elem_type)) { emit_retain(c, arg_res.reg, elem_type); }
    file_io.write(c.output_file, c.indent + "store " + elem_ty_str + " " + arg_res.reg + ", " + elem_ty_str + "* " + slot_ptr + "\n");
    
    let new_size -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + new_size + " = add i64 " + size_val + ", 1\n");
    file_io.write(c.output_file, c.indent + "store i64 " + new_size + ", i64* " + size_ptr + "\n");
    
    return void_result();
}
func compile_vector_drop(c -> Compiler, vec_node -> Struct, call_node -> CallNode) -> CompileResult {
    let args -> Vector(Struct) = call_node.args;
    let a_len -> Int = 0;
    if (args is !null) { a_len = args.length(); }
    if (a_len > 0) { WhitelangExceptions.throw_type_error(call_node.pos, "drop expects 0 arguments."); }
    
    let vec_res -> CompileResult = compile_node(c, vec_node);
    let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + vec_res.type);
    let elem_type -> Int = v_info.type;
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
    let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

    let size_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");

    let cmp_reg -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + cmp_reg + " = icmp ugt i64 " + size_val + ", 0\n");
    
    let pop_label -> String = "vec_pop_" + c.type_counter;
    let empty_label -> String = "vec_empty_" + c.type_counter;
    let end_label -> String = "vec_pop_end_" + c.type_counter;
    c.type_counter += 1;
    
    file_io.write(c.output_file, c.indent + "br i1 " + cmp_reg + ", label %" + pop_label + ", label %" + empty_label + "\n");

    file_io.write(c.output_file, "\n" + empty_label + ":\n");

    emit_runtime_error(c, call_node.pos, "drop from empty vector");

    file_io.write(c.output_file, "\n" + pop_label + ":\n");
    
    // size--
    let new_size -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + new_size + " = sub i64 " + size_val + ", 1\n");
    file_io.write(c.output_file, c.indent + "store i64 " + new_size + ", i64* " + size_ptr + "\n");

    let data_field_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 2\n");
    let data_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    let slot_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + new_size + "\n");
    
    let ret_val -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + ret_val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");
    
    file_io.write(c.output_file, c.indent + "br label %" + end_label + "\n");
    
    file_io.write(c.output_file, "\n" + end_label + ":\n");
    return CompileResult(reg=ret_val, type=elem_type);
}
func compile_vector_lit(c -> Compiler, node -> VectorLitNode) -> CompileResult {
    let count -> Int = node.count;
    let elem_type_id -> Int = TYPE_INT; 
    let elements -> Vector(Struct) = node.elements;
    let e_len -> Int = 0;
    if (elements is !null) { e_len = elements.length(); }
    
    if (e_len > 0) {
        let first_arg -> ArgNode = elements[0];
        let first_res -> CompileResult = compile_node(c, first_arg.val);
        elem_type_id = first_res.type;
    } else {
        if (c.expected_type >= 100) {
            let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + c.expected_type);
            if (v_info is !null) {
                elem_type_id = v_info.type;
            }
        }
    }
    
    let vec_type_id -> Int = get_vector_type_id(c, elem_type_id);
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type_id);
    let struct_name -> String = "{ i64, i64, " + elem_ty_str + "* }";

    let struct_size_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + struct_size_ptr + " = getelementptr " + struct_name + ", " + struct_name + "* null, i32 1\n");
    let struct_size -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + struct_size + " = ptrtoint " + struct_name + "* " + struct_size_ptr + " to i64\n");
    let vec_ptr -> String = emit_alloc_obj(c, struct_size, "" + vec_type_id, struct_name + "*");
    
    // malloc data array
    let arr_size_ptr -> String = next_reg(c);
    // size = count * sizeof(T)
    file_io.write(c.output_file, c.indent + arr_size_ptr + " = getelementptr " + elem_ty_str + ", " + elem_ty_str + "* null, i64 " + count + "\n");
    let arr_bytes -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + arr_bytes + " = ptrtoint " + elem_ty_str + "* " + arr_size_ptr + " to i64\n");
    
    let raw_data -> String = next_reg(c);
    // TODO: Handle count == 0 case carefully
    file_io.write(c.output_file, c.indent + raw_data + " = call i8* @malloc(i64 " + arr_bytes + ")\n");
    let data_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + data_ptr + " = bitcast i8* " + raw_data + " to " + elem_ty_str + "*\n");

    // struct { i64 size, i64 capacity, T* data }
    // size
    let size_ptr -> String = next_reg(c); 
    file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 0\n");
    file_io.write(c.output_file, c.indent + "store i64 " + count + ", i64* " + size_ptr + "\n");
    
    // capacity
    let cap_ptr -> String = next_reg(c); 
    file_io.write(c.output_file, c.indent + cap_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 1\n");
    file_io.write(c.output_file, c.indent + "store i64 " + count + ", i64* " + cap_ptr + "\n"); 
    
    // data pointer
    let data_field_ptr -> String = next_reg(c); 
    file_io.write(c.output_file, c.indent + data_field_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 2\n");
    file_io.write(c.output_file, c.indent + "store " + elem_ty_str + "* " + data_ptr + ", " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    let idx -> Int = 0;
    while (idx < e_len) {
        let curr -> ArgNode = elements[idx];
        let val_res -> CompileResult = compile_node(c, curr.val);

        if (val_res.type != elem_type_id) {
            if (elem_type_id == TYPE_INT && val_res.type == TYPE_BYTE) {
                val_res = promote_to_int(c, val_res);
            } else {
                WhitelangExceptions.throw_type_error(node.pos, "Mixed types in vector literal. Expected " + get_type_name(c, elem_type_id) + ", got " + get_type_name(c, val_res.type) + ".");
            }
        }
        
        let slot_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx + "\n");

        if (is_ref_type(c, elem_type_id)) {
            emit_retain(c, val_res.reg, elem_type_id);
        }
        
        file_io.write(c.output_file, c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + slot_ptr + "\n");
        
        idx += 1;
    }
    
    return CompileResult(reg=vec_ptr, type=vec_type_id);
}

func compile_length_method(c -> Compiler, obj_node -> Struct, call_node -> CallNode) -> CompileResult {
    let args -> Vector(Struct) = call_node.args;
    let a_len -> Int = 0;
    if (args is !null) { a_len = args.length(); }
    if (a_len > 0) {
        WhitelangExceptions.throw_type_error(call_node.pos, "Method 'length' does not accept arguments.");
    }

    let obj_res -> CompileResult = compile_node(c, obj_node);
    let type_id -> Int = obj_res.type;

    // String.length()
    if (type_id == TYPE_STRING) {
        let len_reg -> String = next_reg(c);
        // strlen returns i32 in our declaration
        file_io.write(c.output_file, c.indent + len_reg + " = call i32 @strlen(i8* " + obj_res.reg + ")\n");
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
        file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + obj_res.reg + ", i32 0, i32 0\n");
        
        let size_val -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");

        let trunc_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + trunc_reg + " = trunc i64 " + size_val + " to i32\n");
        
        return CompileResult(reg=trunc_reg, type=TYPE_INT);
    }

    WhitelangExceptions.throw_type_error(call_node.pos, "Method 'length' is not defined for type " + get_type_name(c, type_id));
    return void_result();
}

func compile_index_access(c -> Compiler, node -> IndexAccessNode) -> CompileResult {
    check_out_index(c, node.target, node.index_node, node.pos);
    let target_res -> CompileResult = compile_node(c, node.target);
    let index_res -> CompileResult = compile_node(c, node.index_node);
    
    // idx type
    if (index_res.type != TYPE_INT) {
        WhitelangExceptions.throw_type_error(node.pos, "Index must be an Integer.");
    }

    if (is_pointer_type(c, target_res.type)) {
        let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + target_res.type);
        if (base_info is !null) {
            let elem_type -> Int = base_info.type;
            
            if (elem_type == TYPE_VOID) {
                WhitelangExceptions.throw_type_error(node.pos, "Cannot index 'ptr Void'. Cast it to a specific pointer type first.");
            }
            
            let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
            
            let addr_reg -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + addr_reg + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + target_res.reg + ", i32 " + index_res.reg + "\n");
            
            let load_reg -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + load_reg + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + addr_reg + "\n");
            
            return CompileResult(reg=load_reg, type=elem_type, origin_type=elem_type);
        }
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
        file_io.write(c.output_file, c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + target_res.reg + ", i32 0, i32 2\n");
        
        let data_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");

        let slot_ptr -> String = next_reg(c);
        
        let idx_i64 -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        
        file_io.write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx_i64 + "\n");
        
        let val_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + val_reg + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");
        
        return CompileResult(reg=val_reg, type=elem_type);
    }
    
    // str access(-> Byte)
    if (target_res.type == TYPE_STRING) {
        let idx_i64 -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        
        let char_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + char_ptr + " = getelementptr inbounds i8, i8* " + target_res.reg + ", i64 " + idx_i64 + "\n");
        
        let char_val -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + char_val + " = load i8, i8* " + char_ptr + "\n");
        
        return CompileResult(reg=char_val, type=TYPE_BYTE);
    }
    
    WhitelangExceptions.throw_type_error(node.pos, "Type " + get_type_name(c, target_res.type) + " is not indexable.");
    return void_result();
}

func compile_index_assign(c -> Compiler, node -> IndexAssignNode) -> CompileResult {
    check_out_index(c, node.target, node.index_node, node.pos);
    let target_res -> CompileResult = compile_node(c, node.target);
    let index_res -> CompileResult = compile_node(c, node.index_node);
    let val_res -> CompileResult = compile_node(c, node.value);
    
    if (index_res.type != TYPE_INT) {
        WhitelangExceptions.throw_type_error(node.pos, "Index must be an Integer.");
    }

    if (is_pointer_type(c, target_res.type)) {
        let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + target_res.type);
        if (base_info is !null) {
            let elem_type -> Int = base_info.type;
            
            if (elem_type == TYPE_VOID) {
                WhitelangExceptions.throw_type_error(node.pos, "Cannot index 'ptr Void'. Cast it to a specific pointer type first.");
            }
            
            val_res = emit_implicit_cast(c, val_res, elem_type, node.pos);
            
            let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
            let addr_reg -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + addr_reg + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + target_res.reg + ", i32 " + index_res.reg + "\n");
            
            if (is_ref_type(c, elem_type)) {
                emit_retain(c, val_res.reg, elem_type);
                let old_val -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + old_val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + addr_reg + "\n");
                emit_release(c, old_val, elem_type);
            }
            
            file_io.write(c.output_file, c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + addr_reg + "\n");
            return val_res;
        }
    }
    
    let is_vec -> Bool = false;
    if (target_res.type >= 100) {
        if (map_get(c.vector_base_map, "" + target_res.type) is !null) { is_vec = true; }
    }

    if (target_res.type == TYPE_STRING) {
        let is_magic_func -> Bool = false;
        if (c.curr_func is !null) {
            if (c.curr_func.name == "builtin.string_slice") {
                is_magic_func = true;
            }
        }

        if is_magic_func {
            let idx_i64 -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");

            let ptr_reg -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + ptr_reg + " = getelementptr inbounds i8, i8* " + target_res.reg + ", i64 " + idx_i64 + "\n");
            val_res = emit_implicit_cast(c, val_res, TYPE_BYTE, node.pos);

            file_io.write(c.output_file, c.indent + "store i8 " + val_res.reg + ", i8* " + ptr_reg + "\n");
            return val_res;
        }

        WhitelangExceptions.throw_type_error(node.pos, "Strings are immutable. Cannot assign to index.");
    }

    if is_vec {
        let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + target_res.type);
        let elem_type -> Int = v_info.type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
        val_res = emit_implicit_cast(c, val_res, elem_type, node.pos);
        let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

        emit_vector_bounds_check(c, target_res.reg, index_res.reg, struct_ty, node.pos);

        let data_field_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + target_res.reg + ", i32 0, i32 2\n");
        let data_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");
        
        let idx_i64 -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        let slot_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx_i64 + "\n");

        if (is_ref_type(c, elem_type)) {
            emit_retain(c, val_res.reg, elem_type);
    
            let old_val -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + old_val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");

            emit_release(c, old_val, elem_type);
        }

        file_io.write(c.output_file, c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + slot_ptr + "\n");
        
        return val_res;
    }
    
    WhitelangExceptions.throw_type_error(node.pos, "Type " + get_type_name(c, target_res.type) + " does not support index assignment.");
    return void_result();
}

func compile_binop(c -> Compiler, node -> BinOpNode) -> CompileResult {
    let left -> CompileResult = compile_node(c, node.left);
    let op_type -> Int = node.op_tok.type; 

    if (op_type == TOK_AND || op_type == TOK_OR) {
        if (left.type != TYPE_BOOL) {
            WhitelangExceptions.throw_type_error(node.pos, "Logic operators '&&' and '||' require Bool operands. ");
        }
        let label_rhs -> String = next_label(c);
        let label_merge -> String = next_label(c);
        let res_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + res_ptr + " = alloca i1\n");
        file_io.write(c.output_file, c.indent + "store i1 " + left.reg + ", i1* " + res_ptr + "\n");
        
        if (op_type == TOK_AND) {
            file_io.write(c.output_file, c.indent + "br i1 " + left.reg + ", label %" + label_rhs + ", label %" + label_merge + "\n");
        } else {
            file_io.write(c.output_file, c.indent + "br i1 " + left.reg + ", label %" + label_merge + ", label %" + label_rhs + "\n");
        }

        file_io.write(c.output_file, "\n" + label_rhs + ":\n");
        let right_res -> CompileResult = compile_node(c, node.right);
        if (right_res.type != TYPE_BOOL) { WhitelangExceptions.throw_type_error(node.pos, "Right operand must be Bool."); }
        file_io.write(c.output_file, c.indent + "store i1 " + right_res.reg + ", i1* " + res_ptr + "\n");
        file_io.write(c.output_file, c.indent + "br label %" + label_merge + "\n");

        file_io.write(c.output_file, "\n" + label_merge + ":\n");
        let final_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + final_reg + " = load i1, i1* " + res_ptr + "\n");
        return CompileResult(reg=final_reg, type=TYPE_BOOL);
    }

    let right -> CompileResult = compile_node(c, node.right);
    if (op_type == TOK_EE || op_type == TOK_NE) {
        if (left.type == TYPE_NULL || left.type == TYPE_NULLPTR ||
            right.type == TYPE_NULL || right.type == TYPE_NULLPTR) {
            WhitelangExceptions.throw_type_error(node.pos, "Invalid operator. Do not use '==' or '!=' with null/nullptr. Use 'is' or 'is !'.");
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
            file_io.write(c.output_file, c.indent + len1 + " = call i32 @strlen(i8* " + left.reg + ")\n");
            
            let len2 -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + len2 + " = call i32 @strlen(i8* " + right.reg + ")\n");

            // upgrade to i64
            let len1_64 -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + len1_64 + " = zext i32 " + len1 + " to i64\n");
            let len2_64 -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + len2_64 + " = zext i32 " + len2 + " to i64\n");
            
            // total length
            let sum_len -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + sum_len + " = add i64 " + len1_64 + ", " + len2_64 + "\n");
            
            // for \0
            let total_size -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + total_size + " = add i64 " + sum_len + ", 1\n");

            let new_str_ptr -> String = emit_alloc_obj(c, total_size, "" + TYPE_STRING, "i8*");
            
            // strcpy(new_ptr, left) -> null
            let ign1 -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + ign1 + " = call i8* @strcpy(i8* " + new_str_ptr + ", i8* " + left.reg + ")\n");
            
            // strcat(new_ptr, right) -> null
            let ign2 -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + ign2 + " = call i8* @strcat(i8* " + new_str_ptr + ", i8* " + right.reg + ")\n");
            
            return CompileResult(reg=new_str_ptr, type=TYPE_STRING);
        }

        if (left.type != right.type) {
            WhitelangExceptions.throw_type_error(node.pos, "Cannot operate on String with other types.");
        }


        let allowed -> Bool = false;
        if (op_type == TOK_EE) { allowed = true; }
        if (op_type == TOK_NE) { allowed = true; }
        
        if (!allowed) {
            WhitelangExceptions.throw_type_error(node.pos, "Arithmetic operations on Strings are not supported (except +).");
        }

        let cmp_val -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + cmp_val + " = call i32 @strcmp(i8* " + left.reg + ", i8* " + right.reg + ")\n");

        let res_reg -> String = next_reg(c);
        let op_code -> String = "icmp eq";

        if (op_type == TOK_NE) { op_code = "icmp ne"; }

        file_io.write(c.output_file, c.indent + res_reg + " = " + op_code + " i32 " + cmp_val + ", 0\n");
        
        return CompileResult(reg=res_reg, type=TYPE_BOOL);
    }

    if (op_type == TOK_POW) {
        left = promote_to_float(c, left);
        right = promote_to_float(c, right);
        let res_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + res_reg + " = call double @llvm.pow.f64(double " + left.reg + ", double " + right.reg + ")\n");
        return CompileResult(reg=res_reg, type=TYPE_FLOAT);
    }

    let is_cmp -> Bool = false;
    if (op_type == TOK_EE) { is_cmp = true; }
    if (op_type == TOK_NE) { is_cmp = true; }
    if (op_type == TOK_GT) { is_cmp = true; }
    if (op_type == TOK_LT) { is_cmp = true; }
    if (op_type == TOK_GTE) { is_cmp = true; }
    if (op_type == TOK_LTE) { is_cmp = true; }

    if is_cmp {
        if (left.type >= 100 || right.type >= 100 || left.type == TYPE_NULL || right.type == TYPE_NULL) {
            WhitelangExceptions.throw_type_error(node.pos, "Pointer comparison is not supported yet. Please use loop counters.");
        }
        if (left.type == TYPE_BOOL || right.type == TYPE_BOOL) {
            if (left.type != right.type) { WhitelangExceptions.throw_type_error(node.pos, "Cannot compare Bool with other types."); }
            if (op_type != TOK_EE && op_type != TOK_NE) { WhitelangExceptions.throw_type_error(node.pos, "Invalid Bool comparison."); }
            let res_reg -> String = next_reg(c);
            let op_code -> String = "icmp eq";
            if (op_type == TOK_NE) { op_code = "icmp ne"; }
            file_io.write(c.output_file, c.indent + res_reg + " = " + op_code + " i1 " + left.reg + ", " + right.reg + "\n");
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
        
        file_io.write(c.output_file, c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n");
        return CompileResult(reg=res_reg, type=TYPE_BOOL);
    }

    if (left.type == TYPE_BOOL || right.type == TYPE_BOOL) {
        WhitelangExceptions.throw_type_error(node.pos, "Arithmetic operators cannot be used on Bool. ");
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

    if (op_type == TOK_DIV || op_type == TOK_MOD) {
            if (right.reg == "0" || right.reg == "0.0") {
                WhitelangExceptions.throw_zero_division_error(node.pos, "Cannot divide by zero. ");
            }

            let is_zero_reg -> String = next_reg(c);
            if (target_type == TYPE_FLOAT) {
                file_io.write(c.output_file, c.indent + is_zero_reg + " = fcmp oeq double " + right.reg + ", 0.0\n");
            } else {
                file_io.write(c.output_file, c.indent + is_zero_reg + " = icmp eq " + type_str + " " + right.reg + ", 0\n");
            }
            
            let err_label -> String = "div_zero_" + c.type_counter;
            let ok_label -> String = "div_ok_" + c.type_counter;
            c.type_counter += 1;
            
            file_io.write(c.output_file, c.indent + "br i1 " + is_zero_reg + ", label %" + err_label + ", label %" + ok_label + "\n");
            
            file_io.write(c.output_file, "\n" + err_label + ":\n");
            emit_runtime_error(c, node.pos, "Division by zero");
            
            file_io.write(c.output_file, "\n" + ok_label + ":\n");
        }

    file_io.write(c.output_file, c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n");
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
        c.str_count += 1;

        c.string_list.append(StringConstant(id=id, value=val));
        let len -> Int = val.length() + 1; // +1 for \0

        let user_ptr -> String = emit_alloc_obj(c, "" + len, "" + TYPE_STRING, "i8*");

        // strcpy(ptr, constant_ptr)
        let const_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + const_ptr + " = getelementptr inbounds [" + len + " x i8], [" + len + " x i8]* @.str." + id + ", i32 0, i32 0\n");
        file_io.write(c.output_file, c.indent + "call i8* @strcpy(i8* " + user_ptr + ", i8* " + const_ptr + ")\n");

        return CompileResult(reg=user_ptr, type=TYPE_STRING);
    }

    if (base.type == NODE_VAR_DECL) { return compile_var_decl(c, node); }
    if (base.type == NODE_IF)       { return compile_if(c, node); }
    if (base.type == NODE_WHILE)    { return compile_while(c, node); }
    if (base.type == NODE_FOR)      { return compile_for(c, node); }
    if (base.type == NODE_BINOP)    { return compile_binop(c, node); }
    if (base.type == NODE_RETURN)   { return compile_return(c, node); }
    if (base.type == NODE_STRUCT_DEF) { return compile_struct_def(c, node); }
    if (base.type == NODE_CLASS_DEF)  { return compile_class_def(c, node); }
    if (base.type == NODE_FIELD_ACCESS) { return compile_field_access(c, node); }
    if (base.type == NODE_FIELD_ASSIGN) { return compile_field_assign(c, node); }
    if (base.type == NODE_EXTERN_BLOCK) { return compile_extern_block(c, node); }
    if (base.type == NODE_VECTOR_LIT) { return compile_vector_lit(c, node); }
    if (base.type == NODE_INDEX_ACCESS) { return compile_index_access(c, node); }
    if (base.type == NODE_INDEX_ASSIGN) { return compile_index_assign(c, node); }

    // function and closure
    if (base.type == NODE_FUNC_DEF) {
            let func_def -> FunctionDefNode = node;
            
            if (c.scope_depth == 0) {
                compile_func_def(c, func_def);
                return null;
            } else {
                return compile_local_closure(c, func_def);
            }
        }

    // ptr
    if (base.type == NODE_PTR_ASSIGN) {return compile_ptr_assign(c, node);}
    // ref
    if (base.type == NODE_REF) {
        let r_node -> RefNode = node;
        let target -> BaseNode = r_node.node;
        
        if (target.type != NODE_VAR_ACCESS) {
            WhitelangExceptions.throw_invalid_syntax(r_node.pos, "Cannot take ref of r-value.");
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
                file_io.write(c.output_file, c.indent + cast_reg + " = bitcast " + sig + " " + func_ptr + " to i8*\n");

                let clo_payload -> String = emit_alloc_obj(c, "16", "8", "i8*");
                
                let clo_func_ptr -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_func_ptr + " = bitcast i8* " + clo_payload + " to i8**\n");
                file_io.write(c.output_file, c.indent + "store i8* " + cast_reg + ", i8** " + clo_func_ptr + "\n");
                
                let clo_env_ptr_i8 -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_env_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_payload + ", i32 8\n");
                let clo_env_ptr -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_env_ptr + " = bitcast i8* " + clo_env_ptr_i8 + " to i8**\n");
                file_io.write(c.output_file, c.indent + "store i8* null, i8** " + clo_env_ptr + "\n");

                return CompileResult(reg=clo_payload, type=specific_type_id);
            }
            
            WhitelangExceptions.throw_name_error(r_node.pos, "Unknown variable or function '" + name + "'.");
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
                WhitelangExceptions.throw_null_dereference_error(d_node.pos, "Cannot dereference 'nullptr'. ");
            }
            let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + curr_type);
            if (base_info is null) { WhitelangExceptions.throw_type_error(d_node.pos, "Attempt to dereference non-pointer. "); }
            
            let next_type -> Int = base_info.type;
            if (next_type == TYPE_VOID) {
                WhitelangExceptions.throw_type_error(d_node.pos, "Cannot dereference 'ptr Void'. Cast it to a specific pointer type first.");
            }
            let ty_str -> String = get_llvm_type_str(c, next_type);
            let next_reg -> String = next_reg(c);
            
            file_io.write(c.output_file, c.indent + next_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_reg + "\n");
            
            curr_reg = next_reg;
            curr_type = next_type;
            i += 1;
        }
        return CompileResult(reg=curr_reg, type=curr_type);
    }

    if (base.type == NODE_IMPORT) { 
        compile_import(c, node);
        return void_result();
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

        if (lhs_res.type == TYPE_NULL && is_pointer_type(c, rhs_res.type)) {
            WhitelangExceptions.throw_type_error(b_node.pos, "Cannot use 'null' with explicit pointer types. Use 'nullptr'.");
        }
        if (rhs_res.type == TYPE_NULL && is_pointer_type(c, lhs_res.type)) {
            WhitelangExceptions.throw_type_error(b_node.pos, "Cannot use 'null' with explicit pointer types. Use 'nullptr'.");
        }
        if (lhs_res.type == TYPE_NULLPTR && !is_pointer_type(c, rhs_res.type) && rhs_res.type != TYPE_NULLPTR) {
            WhitelangExceptions.throw_type_error(b_node.pos, "Cannot use 'nullptr' with non-pointer types.");
        }
        if (rhs_res.type == TYPE_NULLPTR && !is_pointer_type(c, lhs_res.type) && lhs_res.type != TYPE_NULLPTR) {
            WhitelangExceptions.throw_type_error(b_node.pos, "Cannot use 'nullptr' with non-pointer types.");
        }
        if (lhs_res.type == TYPE_INT || lhs_res.type == TYPE_FLOAT || lhs_res.type == TYPE_BOOL || lhs_res.type == TYPE_BYTE) {
            WhitelangExceptions.throw_type_error(b_node.pos, "Operator 'is' cannot be used with primitive types.");
        }
        
        // convert to i8* for address comparison
        if (lhs_res.type != TYPE_STRING && lhs_res.type != TYPE_NULL && lhs_res.type != TYPE_NULLPTR) {
            let cast_l -> String = next_reg(c);
            let ty_l -> String = get_llvm_type_str(c, lhs_res.type);
            file_io.write(c.output_file, c.indent + cast_l + " = bitcast " + ty_l + " " + l_reg + " to i8*\n");
            l_reg = cast_l;
        }
        if (rhs_res.type != TYPE_STRING && rhs_res.type != TYPE_NULL && rhs_res.type != TYPE_NULLPTR) {
            let cast_r -> String = next_reg(c);
            let ty_r -> String = get_llvm_type_str(c, rhs_res.type);
            file_io.write(c.output_file, c.indent + cast_r + " = bitcast " + ty_r + " " + r_reg + " to i8*\n");
            r_reg = cast_r;
        }

        let cmp_reg -> String = next_reg(c);
        let cond -> String = "eq";
        if (base.type == 33) { cond = "ne"; } // NODE_IS_NOT
        
        file_io.write(c.output_file, c.indent + cmp_reg + " = icmp " + cond + " i8* " + l_reg + ", " + r_reg + "\n");
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
                file_io.write(c.output_file, c.indent + cast_reg + " = bitcast " + sig + " " + func_ptr + " to i8*\n");
                let clo_reg -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_reg + " = call i8* @malloc(i64 24)\n");
                let clo_rc_ptr -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_rc_ptr + " = bitcast i8* " + clo_reg + " to i32*\n");
                file_io.write(c.output_file, c.indent + "store i32 0, i32* " + clo_rc_ptr + "\n");
                let clo_type_ptr_i8 -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_type_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_reg + ", i32 4\n");
                let clo_type_ptr -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_type_ptr + " = bitcast i8* " + clo_type_ptr_i8 + " to i32*\n");
                file_io.write(c.output_file, c.indent + "store i32 8, i32* " + clo_type_ptr + "\n");
                let clo_func_ptr_i8 -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_func_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_reg + ", i32 8\n");
                let clo_func_ptr -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_func_ptr + " = bitcast i8* " + clo_func_ptr_i8 + " to i8**\n");
                file_io.write(c.output_file, c.indent + "store i8* " + cast_reg + ", i8** " + clo_func_ptr + "\n");
                let clo_env_ptr_i8 -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_env_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_reg + ", i32 16\n");
                let clo_env_ptr -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_env_ptr + " = bitcast i8* " + clo_env_ptr_i8 + " to i8**\n");
                file_io.write(c.output_file, c.indent + "store i8* null, i8** " + clo_env_ptr + "\n");
                
                let clo_payload -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + clo_payload + " = getelementptr inbounds i8, i8* " + clo_reg + ", i32 8\n");

                return CompileResult(reg=clo_payload, type=specific_type_id);
            }

            WhitelangExceptions.throw_name_error(v.pos, "Undefined variable or function '" + var_name + "'. ");
        }
        
        let val_reg -> String = next_reg(c);
        let llvm_ty_str -> String = get_llvm_type_str(c, info.type);
        if (llvm_ty_str == "") {
            WhitelangExceptions.throw_type_error(v.pos, "Variable '" + var_name + "' has invalid internal type ID. ");
        }
        
        file_io.write(c.output_file, c.indent + val_reg + " = load " + llvm_ty_str + ", " + llvm_ty_str + "* " + info.reg + "\n");
        return CompileResult(reg=val_reg, type=info.type, origin_type=info.origin_type);
    }

    if (base.type == NODE_VAR_ASSIGN) {
        return compile_var_assign(c, node);
    }

    if (base.type == NODE_CALL) {
        let n_call -> CallNode = node;
        let callee -> BaseNode = n_call.callee;

        let func_name -> String = "";
        let is_direct -> Bool = false;
        let is_package_call -> Bool = false;

        if (callee.type == NODE_FIELD_ACCESS) {
            let f_acc -> FieldAccessNode = n_call.callee;
            if (f_acc.field_name == "length") {
                return compile_length_method(c, f_acc.obj, n_call);
            }
            if (f_acc.field_name == "append") {
                return compile_vector_append(c, f_acc.obj, n_call);
            }
            if (f_acc.field_name == "drop") {
                return compile_vector_drop(c, f_acc.obj, n_call);
            }

            let try_string_method -> Bool = false;
            let guessed_type -> Int = get_expr_type(c, f_acc.obj);
            if (guessed_type == TYPE_STRING) {
                try_string_method = true;
            }

            if try_string_method {
                let res -> CompileResult = compile_string_method_call(c, f_acc.obj, f_acc.field_name, n_call);
                if (res is !null) { return res; }
            }

            let obj_base -> BaseNode = f_acc.obj;
            if (obj_base.type == NODE_VAR_ACCESS) {
                let v_node -> VarAccessNode = f_acc.obj;
                let pkg_name -> String = v_node.name_tok.value;
                if (find_symbol(c, pkg_name) is null) {
                    let pkg_marker -> StringConstant = map_get(c.loaded_packages, pkg_name);
                    if (pkg_marker is !null) {
                        func_name = pkg_marker.value + "." + f_acc.field_name;
                        is_package_call = true;
                    } else if (map_get(c.loaded_files, pkg_name) is !null) {
                        func_name = f_acc.field_name; 
                        is_package_call = true;
                    }
                }
            }

            if (!is_package_call && !try_string_method) {
                let obj_res -> CompileResult = compile_node(c, f_acc.obj);
                let struct_type_id -> Int = obj_res.type;
                if (struct_type_id == TYPE_GENERIC_STRUCT && obj_res.origin_type >= 100) {
                    struct_type_id = obj_res.origin_type;
                }
                let s_info -> StructInfo = map_get(c.struct_id_map, "" + struct_type_id);
                if (s_info is !null && s_info.is_class) {
                    return compile_class_method_call(c, s_info, obj_res, f_acc.field_name, n_call);
                }
            }
            // ======================================
        }

        if (callee.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = n_call.callee;
            func_name = v_node.name_tok.value;
        }

        if (func_name != "") {
            if (is_package_call) {
                is_direct = true;
            } else if (func_name == "print") {
                is_direct = true;
            } else {
                let s_check -> StructInfo = map_get(c.struct_table, func_name);
                if (s_check is !null) {
                    is_direct = true;
                } else {
                    let f_check -> FuncInfo = map_get(c.func_table, func_name);
                    let v_check -> SymbolInfo = find_symbol(c, func_name);
                    
                    if (f_check is !null && v_check is null) {
                        is_direct = true;
                    }
                }
            }
        }

        if is_direct {
            if (func_name == "builtin.print" || func_name == "print") {
                let args -> Vector(Struct) = n_call.args;
                let a_len -> Int = 0;
                if (args is !null) { a_len = args.length(); }

                if (a_len == 0) {
                    let fmt_ptr -> String = next_reg(c);
                    file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_newline, i32 0, i32 0))\n");
                    return void_result();
                }
                
                let a_idx -> Int = 0;
                while (a_idx < a_len) {
                    let curr_arg -> ArgNode = args[a_idx];
                    let arg_res -> CompileResult = compile_node(c, curr_arg.val);
                    compile_print(c, arg_res.reg, arg_res.type, n_call.pos, arg_res.origin_type);
                    a_idx += 1;
                }

                file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_newline, i32 0, i32 0))\n");
                return void_result();
            }

            let s_info -> StructInfo = map_get(c.struct_table, func_name);
            if (s_info is !null) {
                if (s_info.is_class) {
                    return compile_class_init(c, s_info, n_call);
                } else {
                    return compile_struct_init(c, s_info, n_call);
                }
            }

            let func_info -> FuncInfo = map_get(c.func_table, func_name);

            if (func_info is null) {
                WhitelangExceptions.throw_name_error(n_call.pos, "Function '" + func_name + "' is not defined.");
            }

            let args_str -> String = "";
            let args -> Vector(Struct) = n_call.args;
            let a_len -> Int = 0;
            if (args is !null) { a_len = args.length(); }
            let arg_idx -> Int = 0;

            let arg_types -> Vector(Struct) = func_info.arg_types;
            let type_len -> Int = 0; 
            if (arg_types is !null) { type_len = arg_types.length(); }
            
            let is_first -> Bool = true;
            
            while (arg_idx < a_len) {
                let arg_node_curr -> ArgNode = args[arg_idx];

                if (arg_idx >= type_len) { 
                    if (!func_info.is_varargs) {
                        WhitelangExceptions.throw_type_error(n_call.pos, "Too many arguments.");
                    }
                    let arg_val -> CompileResult = compile_node(c, arg_node_curr.val);

                    if (arg_val.type == TYPE_BYTE) {
                        arg_val = promote_to_int(c, arg_val);
                    }
                    if (arg_val.type == TYPE_BOOL) {
                        let zext_reg -> String = next_reg(c);
                        file_io.write(c.output_file, c.indent + zext_reg + " = zext i1 " + arg_val.reg + " to i32\n");
                        arg_val = CompileResult(reg=zext_reg, type=TYPE_INT);
                    }
                    if (!is_first) { args_str = args_str + ", "; }
                    let ty_str -> String = get_llvm_type_str(c, arg_val.type);
                    args_str += ty_str + " " + arg_val.reg;

                    is_first = false;
                    arg_idx += 1;
                    continue;
                }

                let type_node_curr -> TypeListNode = arg_types[arg_idx];
                let expected_type -> Int = type_node_curr.type;

                c.expected_type = expected_type;
                let arg_val -> CompileResult = compile_node(c, arg_node_curr.val);
                c.expected_type = 0;
                
                arg_val = emit_implicit_cast(c, arg_val, expected_type, n_call.pos);

                let ty_str -> String = get_llvm_type_str(c, arg_val.type);
                if (is_first == false) { args_str = args_str + ", "; }
                args_str += ty_str + " " + arg_val.reg;
                is_first = false;
                
                arg_idx += 1;
            }
            
            if (arg_idx < type_len) { WhitelangExceptions.throw_type_error(n_call.pos, "Too few arguments."); }

            let ret_type_str -> String = get_llvm_type_str(c, func_info.ret_type);
            let call_res_reg -> String = "";
            
            let call_prefix -> String = ret_type_str + " ";
            if (func_info.is_varargs) {
                let sig_args -> String = "";
                let p_idx -> Int = 0;
                let first_p -> Bool = true;
                while (p_idx < type_len) {
                    let p_curr -> TypeListNode = arg_types[p_idx];
                    if (!first_p) { sig_args = sig_args + ", "; }
                    sig_args = sig_args + get_llvm_type_str(c, p_curr.type);
                    first_p = false;
                    p_idx += 1;
                }
                if (!first_p) { sig_args = sig_args + ", ..."; }
                else { sig_args = "..."; }
                call_prefix = ret_type_str + " (" + sig_args + ") ";
            }
            
            if (func_info.ret_type == TYPE_VOID) {
                if (func_info.is_varargs) {
                    file_io.write(c.output_file, c.indent + "call " + call_prefix + "@" + func_info.name + "(" + args_str + ")\n");
                } else {
                    file_io.write(c.output_file, c.indent + "call void @" + func_info.name + "(" + args_str + ")\n");
                }
                return void_result();
            } else {
                call_res_reg = next_reg(c);
                file_io.write(c.output_file, c.indent + call_res_reg + " = call " + call_prefix + "@" + func_info.name + "(" + args_str + ")\n");
                return CompileResult(reg=call_res_reg, type=func_info.ret_type);
            }
        }

        else {
            if (callee.type == NODE_VAR_ACCESS) {
                let v_node -> VarAccessNode = n_call.callee;
                let s_info -> StructInfo = map_get(c.struct_table, v_node.name_tok.value);
                if (s_info is !null) {
                    if (s_info.is_class) {
                        return compile_class_init(c, s_info, n_call);
                    } else {
                        return compile_struct_init(c, s_info, n_call);
                    }
                }
            }

            let callee_res -> CompileResult = compile_node(c, n_call.callee);
            let ptr_type -> Int = callee_res.type;

            let ret_type_id -> Int = 0;
            let is_valid_call -> Bool = false;

            if (ptr_type == TYPE_GENERIC_FUNCTION) {
                if (callee.type == NODE_VAR_ACCESS) {
                    let v_node -> VarAccessNode = n_call.callee;
                    let info -> SymbolInfo = find_symbol(c, v_node.name_tok.value);
                    if (info is !null && info.origin_type >= 100) {
                        let f_ret_info -> SymbolInfo = map_get(c.func_ret_map, "" + info.origin_type);
                        if (f_ret_info is !null) {
                            ret_type_id = f_ret_info.type;
                            is_valid_call = true;
                        }
                    }
                } else {
                    ret_type_id = callee_res.origin_type;
                    if (ret_type_id != 0) { is_valid_call = true; }
                }
                
                if (!is_valid_call) {
                    WhitelangExceptions.throw_type_error(n_call.pos, "Generic Function must specify return type.");
                }
            } 
            else {
                let f_ret_info -> SymbolInfo = map_get(c.func_ret_map, "" + ptr_type);
                if (f_ret_info is !null) {
                    ret_type_id = f_ret_info.type;
                    is_valid_call = true;
                }
            }

            if is_valid_call {
                let is_closure -> Bool = false;
                let actual_env_reg -> String = "";
                let raw_func_ptr -> String = callee_res.reg;
                if (ptr_type == TYPE_GENERIC_FUNCTION || map_get(c.func_ret_map, "" + ptr_type) is !null) {
                    is_closure = true;
                    let env_ptr_i8_addr -> String = next_reg(c);
                    file_io.write(c.output_file, c.indent + env_ptr_i8_addr + " = getelementptr inbounds i8, i8* " + callee_res.reg + ", i32 8\n");
                    let env_ptr_addr -> String = next_reg(c);
                    file_io.write(c.output_file, c.indent + env_ptr_addr + " = bitcast i8* " + env_ptr_i8_addr + " to i8**\n");
                    actual_env_reg = next_reg(c);
                    file_io.write(c.output_file, c.indent + actual_env_reg + " = load i8*, i8** " + env_ptr_addr + "\n");
                    let f_ptr_i8_addr -> String = next_reg(c);
                    file_io.write(c.output_file, c.indent + f_ptr_i8_addr + " = getelementptr inbounds i8, i8* " + callee_res.reg + ", i32 0\n");
                    let f_ptr_addr -> String = next_reg(c);
                    file_io.write(c.output_file, c.indent + f_ptr_addr + " = bitcast i8* " + f_ptr_i8_addr + " to i8**\n");
                    raw_func_ptr = next_reg(c);
                    file_io.write(c.output_file, c.indent + raw_func_ptr + " = load i8*, i8** " + f_ptr_addr + "\n");
                }

                let args -> Vector(Struct) = n_call.args;
                let a_len -> Int = 0; if (args is !null) { a_len = args.length(); }
                let a_idx -> Int = 0;

                let sig_g -> String = "";
                let sig_c -> String = "i8*";
                let args_g_str -> String = "";
                let args_c_str -> String = "i8* " + actual_env_reg;
                let first -> Bool = true;
                
                while (a_idx < a_len) {
                    let curr_arg -> ArgNode = args[a_idx];
                    let a_res -> CompileResult = compile_node(c, curr_arg.val);
                    let a_ty -> String = get_llvm_type_str(c, a_res.type);

                    if (!first) {
                        sig_g = sig_g + ", ";
                        args_g_str = args_g_str + ", ";
                        sig_c = sig_c + ", ";
                        args_c_str = args_c_str + ", ";
                    } else {
                        sig_c = sig_c + ", ";
                        args_c_str = args_c_str + ", ";
                    }
                    
                    sig_g += a_ty;
                    args_g_str += a_ty + " " + a_res.reg;
                    sig_c += a_ty;
                    args_c_str += a_ty + " " + a_res.reg;
                    first = false;
                    a_idx += 1;
                }

                let ret_ty_str -> String = get_llvm_type_str(c, ret_type_id);

                if is_closure {
                    let is_env_null -> String = next_reg(c);
                    file_io.write(c.output_file, c.indent + is_env_null + " = icmp eq i8* " + actual_env_reg + ", null\n");
                    
                    let l_global -> String = "call_g_" + c.type_counter;
                    let l_closure -> String = "call_c_" + c.type_counter;
                    let l_merge -> String = "call_m_" + c.type_counter;
                    c.type_counter += 1;
                    
                    file_io.write(c.output_file, c.indent + "br i1 " + is_env_null + ", label %" + l_global + ", label %" + l_closure + "\n");

                    file_io.write(c.output_file, "\n" + l_global + ":\n");
                    let cast_g -> String = next_reg(c);
                    file_io.write(c.output_file, "  " + cast_g + " = bitcast i8* " + raw_func_ptr + " to " + ret_ty_str + " (" + sig_g + ")*\n");
                    let res_g -> String = "";
                    if (ret_type_id == TYPE_VOID) {
                        file_io.write(c.output_file, "  call void " + cast_g + "(" + args_g_str + ")\n");
                    } else {
                        res_g = next_reg(c);
                        file_io.write(c.output_file, "  " + res_g + " = call " + ret_ty_str + " " + cast_g + "(" + args_g_str + ")\n");
                    }
                    file_io.write(c.output_file, "  br label %" + l_merge + "\n");

                    file_io.write(c.output_file, "\n" + l_closure + ":\n");
                    let cast_c -> String = next_reg(c);
                    file_io.write(c.output_file, "  " + cast_c + " = bitcast i8* " + raw_func_ptr + " to " + ret_ty_str + " (" + sig_c + ")*\n");
                    let res_c -> String = "";
                    if (ret_type_id == TYPE_VOID) {
                        file_io.write(c.output_file, "  call void " + cast_c + "(" + args_c_str + ")\n");
                    } else {
                        res_c = next_reg(c);
                        file_io.write(c.output_file, "  " + res_c + " = call " + ret_ty_str + " " + cast_c + "(" + args_c_str + ")\n");
                    }
                    file_io.write(c.output_file, "  br label %" + l_merge + "\n");

                    file_io.write(c.output_file, "\n" + l_merge + ":\n");
                    if (ret_type_id == TYPE_VOID) {
                        return void_result();
                    } else {
                        let final_res -> String = next_reg(c);
                        file_io.write(c.output_file, "  " + final_res + " = phi " + ret_ty_str + " [ " + res_g + ", %" + l_global + " ], [ " + res_c + ", %" + l_closure + " ]\n");
                        return CompileResult(reg=final_res, type=ret_type_id, origin_type=0);
                    }
                }
            }

            WhitelangExceptions.throw_name_error(n_call.pos, "Call target is not a function or function pointer.");
        }
    }

    if (base.type == NODE_BREAK) {
        let n_break -> BreakNode = node;
        if (c.loop_stack is null) {
            WhitelangExceptions.throw_invalid_syntax(n_break.pos, "'break' outside of loop. ");
        }
        let scope -> LoopScope = c.loop_stack;
        file_io.write(c.output_file, c.indent + "br label %" + scope.label_break + "\n");

        return void_result();
    }

    if (base.type == NODE_CONTINUE) {
        let n_cont -> ContinueNode = node;
        if (c.loop_stack is null) {
            WhitelangExceptions.throw_invalid_syntax(n_cont.pos, "'continue' outside of loop. ");
        }
        let scope -> LoopScope = c.loop_stack;
        file_io.write(c.output_file, c.indent + "br label %" + scope.label_continue + "\n");

        return void_result();
    }

    if (base.type == NODE_POSTFIX) {
        let u -> PostfixOpNode = node;
        let op_type -> Int = u.op_tok.type;

        let var_node -> BaseNode = u.node;

        let target_reg -> String = "";
        let target_type -> Int = 0;
        let type_str -> String = "";

        if (var_node.type == NODE_VAR_ACCESS) {
            let v_acc -> VarAccessNode = u.node;
            let var_name -> String = v_acc.name_tok.value;

            let info -> SymbolInfo = find_symbol(c, var_name);
            if (info is null) { WhitelangExceptions.throw_name_error(v_acc.pos, "Undefined variable '" + var_name + "'. "); }
            if (info.is_const) { WhitelangExceptions.throw_type_error(u.pos, "Cannot modify constant variable '" + var_name + "'."); }

            target_reg = info.reg;
            target_type = info.type;
            type_str = get_llvm_type_str(c, info.type);
            
        }
        else if (var_node.type == NODE_FIELD_ACCESS) {
            let f_acc -> FieldAccessNode = u.node;
            let obj_res -> CompileResult = compile_node(c, f_acc.obj);
            
            let type_id -> Int = obj_res.type;
            let obj_reg -> String = obj_res.reg;
            
            if (type_id == TYPE_GENERIC_STRUCT) {
                let base_obj -> BaseNode = f_acc.obj;
                if (base_obj.type == NODE_VAR_ACCESS) {
                    let v_node -> VarAccessNode = f_acc.obj;
                    let info -> SymbolInfo = find_symbol(c, v_node.name_tok.value);
                    if (info is !null && info.origin_type >= 100) {
                        type_id = info.origin_type;
                        let s_info_temp -> StructInfo = map_get(c.struct_id_map, "" + type_id);
                        if (s_info_temp is !null) {
                            let cast_reg -> String = next_reg(c);
                            file_io.write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + obj_reg + " to " + s_info_temp.llvm_name + "*\n");
                            obj_reg = cast_reg;
                        }
                    }
                }
            }
            
            let s_info -> StructInfo = map_get(c.struct_id_map, "" + type_id);
            if (s_info is null) { WhitelangExceptions.throw_type_error(u.pos, "Cannot access field on non-struct type."); }
            
            let field -> FieldInfo = find_field(s_info, f_acc.field_name);
            if (field is null) { WhitelangExceptions.throw_name_error(u.pos, "Field '" + f_acc.field_name + "' not found."); }
            
            target_type = field.type;
            type_str = field.llvm_type;
            target_reg = next_reg(c);

            file_io.write(c.output_file, c.indent + target_reg + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_reg + ", i32 0, i32 " + field.offset + "\n");
            
        } else {
            let op_str -> String = "++";
            if (op_type == TOK_DEC) { op_str = "--"; }
            WhitelangExceptions.throw_type_error(u.pos, "Operator '" + op_str + "' can only be applied to variables or struct fields.");
        }
        
        if (target_type == TYPE_BOOL) {
            WhitelangExceptions.throw_type_error(u.pos, "Cannot increment/decrement Bool type. ");
        }

        let old_val_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + old_val_reg + " = load " + type_str + ", " + type_str + "* " + target_reg + "\n");
        
        let new_val_reg -> String = next_reg(c);
        if (target_type == TYPE_INT) {
            let op_code -> String = "add i32";
            if (op_type == TOK_DEC) { op_code = "sub i32"; }
            file_io.write(c.output_file, c.indent + new_val_reg + " = " + op_code + " " + old_val_reg + ", 1\n");
        } 
        else if (target_type == TYPE_LONG) {
            let op_code -> String = "add i64";
            if (op_type == TOK_DEC) { op_code = "sub i64"; }
            file_io.write(c.output_file, c.indent + new_val_reg + " = " + op_code + " " + old_val_reg + ", 1\n");
        }
        else if (target_type == TYPE_FLOAT) {
            let op_code -> String = "fadd double";
            if (op_type == TOK_DEC) { op_code = "fsub double"; }
            file_io.write(c.output_file, c.indent + new_val_reg + " = " + op_code + " " + old_val_reg + ", 1.0\n");
        }
        else {
            WhitelangExceptions.throw_type_error(u.pos, "Cannot increment/decrement type " + get_type_name(c, target_type));
        }

        file_io.write(c.output_file, c.indent + "store " + type_str + " " + new_val_reg + ", " + type_str + "* " + target_reg + "\n");
        return CompileResult(reg=old_val_reg, type=target_type);
    }

    if (base.type == NODE_UNARYOP) {
        let u -> UnaryOpNode = node;
        let op_type -> Int = u.op_tok.type; 
        
        let operand -> CompileResult = compile_node(c, u.node);
        let res_reg -> String = next_reg(c);
        
        if (op_type == TOK_SUB) {
            if (operand.type == TYPE_INT) {
                file_io.write(c.output_file, c.indent + res_reg + " = sub i32 0, " + operand.reg + "\n");
                return CompileResult(reg=res_reg, type=TYPE_INT);
            } else if (operand.type == TYPE_FLOAT) {
                file_io.write(c.output_file, c.indent + res_reg + " = fneg double " + operand.reg + "\n");
                return CompileResult(reg=res_reg, type=TYPE_FLOAT);
            } else {
                WhitelangExceptions.throw_type_error(u.pos, "Cannot negate non-numeric type. ");
            }
        } else if (op_type == TOK_NOT) {
            if (operand.type != TYPE_BOOL) {
                WhitelangExceptions.throw_type_error(u.pos, "Operator '!' requires Bool type. ");
            }
            file_io.write(c.output_file, c.indent + res_reg + " = xor i1 " + operand.reg + ", 1\n");
            return CompileResult(reg=res_reg, type=TYPE_BOOL);
        } else {
            return operand;
        }
    }

    return null;
}

// BUILTIN HELPER
func compile_print(c -> Compiler, reg -> String, type_id -> Int, pos -> Position, origin_id -> Int) -> Void {
    if (type_id == TYPE_INT) {
        file_io.write(c.output_file, c.indent + "call void @builtin.print_i(i32 " + reg + ")\n");
    } else if (type_id == TYPE_LONG) {
        file_io.write(c.output_file, c.indent + "call void @builtin.print_l(i64 " + reg + ")\n");
    } else if (type_id == TYPE_FLOAT) {
        file_io.write(c.output_file, c.indent + "call void @builtin.print_f(double " + reg + ")\n");
    } else if (type_id == TYPE_STRING) {
        file_io.write(c.output_file, c.indent + "call void @builtin.print_s(i8* " + reg + ")\n");
    } else if (type_id == TYPE_BOOL) {
        file_io.write(c.output_file, c.indent + "call void @builtin.print_b(i1 " + reg + ")\n");
    } else if (type_id == TYPE_BYTE) {
        file_io.write(c.output_file, c.indent + "call void @builtin.print_c(i8 " + reg + ")\n");
    }
    else if (is_pointer_type(c, type_id)) {
        let base_info -> SymbolInfo = map_get(c.ptr_base_map, "" + type_id);

        if (base_info is !null && base_info.type == TYPE_BYTE) {
            file_io.write(c.output_file, c.indent + "call void @builtin.print_s(i8* " + reg + ")\n");
        } else {
            file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.fmt_hex_ptr, i32 0, i32 0), i8* " + reg + ")\n");
        }
    }
    else if (type_id == TYPE_GENERIC_STRUCT) {
        if (origin_id >= 100) {
            let s_info_real -> StructInfo = map_get(c.struct_id_map, "" + origin_id);
            if (s_info_real is !null) {
                let cast_reg -> String = next_reg(c);
                file_io.write(c.output_file, c.indent + cast_reg + " = bitcast i8* " + reg + " to " + s_info_real.llvm_name + "*\n");
                compile_print_struct_internal(c, cast_reg, s_info_real, pos);
            } else {
                file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.fmt_hex_ptr, i32 0, i32 0), i8* " + reg + ")\n");
            }
        }
        else {
            file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.fmt_hex_ptr, i32 0, i32 0), i8* " + reg + ")\n");
        }
    }
    else if (type_id >= 100) {
        let s_info -> StructInfo = map_get(c.struct_id_map, "" + type_id);
        if (s_info is !null) {
            compile_print_struct_internal(c, reg, s_info, pos);
        } else {
            let v_info -> SymbolInfo = map_get(c.vector_base_map, "" + type_id);
            if (v_info is !null) {
                compile_print_vector_internal(c, reg, v_info, pos);
            }
        }
    } 
    else if (type_id == TYPE_NULL || type_id == TYPE_NULLPTR) {
        file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str_null, i32 0, i32 0))\n");
    }
}

func compile_print_struct_internal(c -> Compiler, obj_reg -> String, s_info -> StructInfo, pos -> Position) -> Void {
    let header -> String = s_info.name + "(";
    let header_id -> Int = c.str_count;
    c.str_count += 1;
    c.string_list.append(StringConstant(id=header_id, value=header));
    file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([" + (header.length() + 1) + " x i8], [" + (header.length() + 1) + " x i8]* @.str." + header_id + ", i32 0, i32 0))\n");

    let fields_vec -> Vector(Struct) = s_info.fields;
    let f_len -> Int = 0;
    if (fields_vec is !null) { f_len = fields_vec.length(); }
    let f_idx -> Int = 0;
    
    while (f_idx < f_len) {
        let f_curr -> FieldInfo = fields_vec[f_idx];
        let f_name_eq -> String = f_curr.name + "=";
        let fn_id -> Int = c.str_count;
        c.str_count += 1;
        c.string_list.append(StringConstant(id=fn_id, value=f_name_eq));
        file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([" + (f_name_eq.length() + 1) + " x i8], [" + (f_name_eq.length() + 1) + " x i8]* @.str." + fn_id + ", i32 0, i32 0))\n");

        let f_ptr -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_reg + ", i32 0, i32 " + f_curr.offset + "\n");
        let f_val_reg -> String = next_reg(c);
        file_io.write(c.output_file, c.indent + f_val_reg + " = load " + f_curr.llvm_type + ", " + f_curr.llvm_type + "* " + f_ptr + "\n");
        compile_print(c, f_val_reg, f_curr.type, pos, f_curr.type); 

        f_idx += 1;
        if (f_idx < f_len) {
            file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str_comma_space, i32 0, i32 0))\n");
        }
    }
    file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_close_paren, i32 0, i32 0))\n");
}

func compile_print_vector_internal(c -> Compiler, vec_reg -> String, v_info -> SymbolInfo, pos -> Position) -> Void {
    let elem_type -> Int = v_info.type;
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
    let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

    let size_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");
    
    let data_ptr_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + data_ptr_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_reg + ", i32 0, i32 2\n");
    let data_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_ptr_ptr + "\n");

    file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_open_bracket, i32 0, i32 0))\n");

    let label_cond -> String = next_label(c);
    let label_body -> String = next_label(c);
    let label_sep  -> String = next_label(c);
    let label_end  -> String = next_label(c);

    let idx_ptr -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + idx_ptr + " = alloca i64\n");
    file_io.write(c.output_file, c.indent + "store i64 0, i64* " + idx_ptr + "\n");
    file_io.write(c.output_file, c.indent + "br label %" + label_cond + "\n");

    file_io.write(c.output_file, "\n" + label_cond + ":\n");
    let curr_idx -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + curr_idx + " = load i64, i64* " + idx_ptr + "\n");
    let cmp -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + cmp + " = icmp slt i64 " + curr_idx + ", " + size_val + "\n");
    file_io.write(c.output_file, c.indent + "br i1 " + cmp + ", label %" + label_body + ", label %" + label_end + "\n");

    file_io.write(c.output_file, "\n" + label_body + ":\n");
    let slot -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + slot + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + curr_idx + "\n");
    let val -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot + "\n");
    
    compile_print(c, val, elem_type, pos, elem_type);

    let next_idx -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + next_idx + " = add i64 " + curr_idx + ", 1\n");
    file_io.write(c.output_file, c.indent + "store i64 " + next_idx + ", i64* " + idx_ptr + "\n");
    
    let is_not_last -> String = next_reg(c);
    file_io.write(c.output_file, c.indent + is_not_last + " = icmp slt i64 " + next_idx + ", " + size_val + "\n");
    file_io.write(c.output_file, c.indent + "br i1 " + is_not_last + ", label %" + label_sep + ", label %" + label_cond + "\n");

    file_io.write(c.output_file, "\n" + label_sep + ":\n");
    file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str_comma_space, i32 0, i32 0))\n");
    file_io.write(c.output_file, c.indent + "br label %" + label_cond + "\n");

    file_io.write(c.output_file, "\n" + label_end + ":\n");
    file_io.write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_close_bracket, i32 0, i32 0))\n");
}

func compile_string_method_call(c -> Compiler, obj_node -> Struct, method_name -> String, call_node -> CallNode) -> CompileResult {
    let target_func -> String = "string_" + method_name;
    let real_func_name -> String = "";
    
    if (map_get(c.func_table, target_func) is !null) {
        real_func_name = target_func;
    } else if (map_get(c.func_table, "builtin." + target_func) is !null) {
        real_func_name = "builtin." + target_func;
    }
    
    if (real_func_name == "") {
        return null;
    }

    let obj_res -> CompileResult = compile_node(c, obj_node);
    let args_str -> String = "i8* " + obj_res.reg;
    let args -> Vector(Struct) = call_node.args;
    let a_len -> Int = 0;
    if (args is !null) { a_len = args.length(); }
    let a_idx -> Int = 0;
    
    while (a_idx < a_len) {
        args_str = args_str + ", ";
        let curr_arg -> ArgNode = args[a_idx];
        let arg_res -> CompileResult = compile_node(c, curr_arg.val);
        if (arg_res.type == TYPE_BYTE) { arg_res = promote_to_int(c, arg_res); }
        if (arg_res.type == TYPE_BOOL) { 
            let zext_reg -> String = next_reg(c);
            file_io.write(c.output_file, c.indent + zext_reg + " = zext i1 " + arg_res.reg + " to i32\n");
            arg_res = CompileResult(reg=zext_reg, type=TYPE_INT);
        }
        
        args_str = args_str + get_llvm_type_str(c, arg_res.type) + " " + arg_res.reg;
        a_idx += 1;
    }

    let f_info -> FuncInfo = map_get(c.func_table, real_func_name);
    let ret_ty_str -> String = get_llvm_type_str(c, f_info.ret_type);
    let call_reg -> String = "";
    if (f_info.ret_type == TYPE_VOID) {
        file_io.write(c.output_file, c.indent + "call void @" + real_func_name + "(" + args_str + ")\n");
        return void_result();
    } else {
        call_reg = next_reg(c);
        file_io.write(c.output_file, c.indent + call_reg + " = call " + ret_ty_str + " @" + real_func_name + "(" + args_str + ")\n");
        return CompileResult(reg=call_reg, type=f_info.ret_type);
    }
}
// --------------

func compile_start(c -> Compiler) -> Void {
    file_io.write(c.output_file, "declare i32 @printf(i8*, ...)\n");
    map_put(c.declared_externs, "printf", StringConstant(id=0, value=""));

    file_io.write(c.output_file, "declare i32 @snprintf(i8*, i64, i8*, ...)\n");
    map_put(c.declared_externs, "snprintf", StringConstant(id=0, value="")); 

    file_io.write(c.output_file, "declare double @llvm.pow.f64(double, double)\n\n");

    file_io.write(c.output_file, "declare i8* @malloc(i64)\n");
    map_put(c.declared_externs, "malloc", StringConstant(id=0, value=""));

    file_io.write(c.output_file, "declare i32 @strlen(i8*)\n");
    map_put(c.declared_externs, "strlen", StringConstant(id=0, value="")); 

    file_io.write(c.output_file, "declare i8* @strcpy(i8*, i8*)\n");
    map_put(c.declared_externs, "strcpy", StringConstant(id=0, value="")); 

    file_io.write(c.output_file, "declare i8* @strcat(i8*, i8*)\n\n");
    map_put(c.declared_externs, "strcat", StringConstant(id=0, value="")); 

    file_io.write(c.output_file, "declare i32 @strcmp(i8*, i8*)\n\n");
    map_put(c.declared_externs, "strcmp", StringConstant(id=0, value="")); 

    file_io.write(c.output_file, "declare void @free(i8*)\n");
    map_put(c.declared_externs, "free", StringConstant(id=0, value=""));

    file_io.write(c.output_file, "declare void @exit(i32)\n");
    map_put(c.declared_externs, "exit", StringConstant(id=0, value=""));

    file_io.write(c.output_file, "declare i8* @realloc(i8*, i64)\n");
    map_put(c.declared_externs, "realloc", StringConstant(id=0, value=""));


    file_io.write(c.output_file, "@.fmt_int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"\n");
    file_io.write(c.output_file, "@.fmt_long = private unnamed_addr constant [6 x i8] c\"%lld\\0A\\00\"\n");
    file_io.write(c.output_file, "@.fmt_float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\"\n");
    file_io.write(c.output_file, "@.fmt_str = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\"\n\n");
    file_io.write(c.output_file, "@.fmt_hex_ptr = private unnamed_addr constant [3 x i8] c\"%p\\00\"\n");

    file_io.write(c.output_file, "@.fmt_int_simple = private unnamed_addr constant [3 x i8] c\"%d\\00\"\n");
    file_io.write(c.output_file, "@.fmt_float_simple = private unnamed_addr constant [3 x i8] c\"%f\\00\"\n");
    file_io.write(c.output_file, "@.str_true = private unnamed_addr constant [5 x i8] c\"true\\00\"\n");
    file_io.write(c.output_file, "@.str_false = private unnamed_addr constant [6 x i8] c\"false\\00\"\n");
    file_io.write(c.output_file, "@.str_null = private unnamed_addr constant [5 x i8] c\"null\\00\"\n\n");
    file_io.write(c.output_file, "@.str_newline = private unnamed_addr constant [2 x i8] c\"\\0A\\00\"\n");

    file_io.write(c.output_file, "@.str_idx_err = private unnamed_addr constant [21 x i8] c\"Index out of bounds\\0A\\00\"\n\n");
    file_io.write(c.output_file, "@.fmt_err_bounds = private unnamed_addr constant [66 x i8] c\"\\0ARuntimeError\\1B[0m: Index out of bounds\\0A    at Line %d, Column %d\\0A\\00\"\n\n");

    // builtin.print
    file_io.write(c.output_file, "@.str_open_bracket = private unnamed_addr constant [2 x i8] c\"[\\00\"\n");
    file_io.write(c.output_file, "@.str_close_bracket = private unnamed_addr constant [2 x i8] c\"]\\00\"\n");
    file_io.write(c.output_file, "@.str_comma_space = private unnamed_addr constant [3 x i8] c\", \\00\"\n");
    file_io.write(c.output_file, "@.str_open_paren = private unnamed_addr constant [2 x i8] c\"(\\00\"\n");
    file_io.write(c.output_file, "@.str_close_paren = private unnamed_addr constant [2 x i8] c\")\\00\"\n");
    file_io.write(c.output_file, "@.str_equal = private unnamed_addr constant [2 x i8] c\"=\\00\"\n");
}

func compile(c -> Compiler, node -> Struct) -> Void {
    compile_start(c);
    compile_ast(c, node);
    compile_end(c);
}

func compile_end(c -> Compiler) -> Void {
    if (!c.has_main) {
        WhitelangExceptions.throw_missing_main_function();
    }
    compile_arc_hooks(c);

    let str_vec -> Vector(Struct) = c.string_list;
    let s_len -> Int = 0; if (str_vec is !null) { s_len = str_vec.length(); }
    let s_idx -> Int = 0;
    while (s_idx < s_len) {
        let curr -> StringConstant = str_vec[s_idx];
        let val -> String = curr.value;
        let escaped_val -> String = string_escape(val);
        let id -> Int = curr.id;
        let len -> Int = val.length() + 1;
        let def -> String = "@.str." + id + " = private unnamed_addr constant [" + len + " x i8] c\"" + escaped_val + "\\00\"\n";
        write(c.output_file, def);
        s_idx += 1;
    }

    file_io.write(c.output_file, "; ====== Lambda Lifted Closures and Envs =====\n");
    file_io.write(c.output_file, c.global_buffer);
    file_io.write(c.output_file, "\n");
    file_io.close(c.output_file);
}