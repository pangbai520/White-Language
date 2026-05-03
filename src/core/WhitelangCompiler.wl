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
        c.output_file.write(c.indent + uitofp_reg + " = uitofp i8 " + input_reg + " to double\n");
        return CompileResult(reg=uitofp_reg, type=TYPE_FLOAT);
    }

    if (res.type == TYPE_LONG) {
        let sitofp_reg -> String = next_reg(c);
        c.output_file.write(c.indent + sitofp_reg + " = sitofp i64 " + input_reg + " to double\n");
        return CompileResult(reg=sitofp_reg, type=TYPE_FLOAT);
    }

    if (res.type == TYPE_BOOL) {
        let zext_reg -> String = next_reg(c);
        c.output_file.write(c.indent + zext_reg + " = zext i1 " + input_reg + " to i32\n");
        input_reg = zext_reg;
    }
    
    let n_reg -> String = next_reg(c);
    c.output_file.write(c.indent + n_reg + " = sitofp i32 " + input_reg + " to double\n");
    return CompileResult(reg=n_reg, type=TYPE_FLOAT);
}
func promote_to_long(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_LONG) { return res; }
    let input_reg -> String = res.reg;

    if (res.type == TYPE_BYTE) {
        let zext_reg -> String = next_reg(c);
        c.output_file.write(c.indent + zext_reg + " = zext i8 " + input_reg + " to i64\n");
        return CompileResult(reg=zext_reg, type=TYPE_LONG);
    }

    if (res.type == TYPE_BOOL) {
        let zext_reg -> String = next_reg(c);
        c.output_file.write(c.indent + zext_reg + " = zext i1 " + input_reg + " to i64\n");
        return CompileResult(reg=zext_reg, type=TYPE_LONG);
    }

    if (res.type == TYPE_INT) {
        let n_reg -> String = next_reg(c);
        // sext: sign extension
        c.output_file.write(c.indent + n_reg + " = sext i32 " + input_reg + " to i64\n");
        return CompileResult(reg=n_reg, type=TYPE_LONG);
    }
    
    return res;
}
func promote_to_int(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type != TYPE_BYTE) { return res; }
    
    let zext_reg -> String = next_reg(c);
    c.output_file.write(c.indent + zext_reg + " = zext i8 " + res.reg + " to i32\n");
    return CompileResult(reg=zext_reg, type=TYPE_INT);
}
func promote_to_byte(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_BYTE) { return res; }
    if (res.type == TYPE_INT) {
        let trunc_reg -> String = next_reg(c);
        c.output_file.write(c.indent + trunc_reg + " = trunc i32 " + res.reg + " to i8\n");
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
        c.output_file.write(c.indent + zext_reg + " = zext i1 " + val_res.reg + " to i32\n");
        return CompileResult(reg=zext_reg, type=TYPE_INT, origin_type=origin);
    }

    if (expected_type == TYPE_GENERIC_STRUCT || expected_type == TYPE_GENERIC_CLASS) {
        if (val_res.type >= 100) {
            let cast_reg -> String = next_reg(c);
            let src_ty -> String = get_llvm_type_str(c, val_res.type);
            c.output_file.write(c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to i8*\n");
            return CompileResult(reg=cast_reg, type=expected_type, origin_type=val_res.type);
        }
    }
    if ((val_res.type == TYPE_GENERIC_STRUCT || val_res.type == TYPE_GENERIC_CLASS) && expected_type >= 100) {
        if (c.struct_id_map.get("" + expected_type) is !null || c.vector_base_map.get("" + expected_type) is !null) {
            let cast_reg -> String = next_reg(c);
            let dest_ty -> String = get_llvm_type_str(c, expected_type);
            c.output_file.write(c.indent + cast_reg + " = bitcast i8* " + val_res.reg + " to " + dest_ty + "\n");
            return CompileResult(reg=cast_reg, type=expected_type, origin_type=origin);
        }
    }

    if (is_pointer_type(c, expected_type) && is_pointer_type(c, val_res.type)) {
        if (is_void_ptr(c, expected_type) || is_void_ptr(c, val_res.type)) {
            let cast_reg -> String = next_reg(c);
            let dest_ty -> String = get_llvm_type_str(c, expected_type);
            let src_ty -> String = get_llvm_type_str(c, val_res.type);
            if (dest_ty != src_ty) {
                c.output_file.write(c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to " + dest_ty + "\n");
                return CompileResult(reg=cast_reg, type=expected_type, origin_type=origin);
            }
            return CompileResult(reg=val_res.reg, type=expected_type, origin_type=origin);
        }
    }

    if (is_void_ptr(c, expected_type)) {
        if (val_res.type == TYPE_STRING) {
            return CompileResult(reg=val_res.reg, type=expected_type, origin_type=val_res.type);
        }
        if (!is_pointer_type(c, val_res.type) && val_res.type != TYPE_NULL && val_res.type != TYPE_NULLPTR) {
            let prim_ty -> String = get_llvm_type_str(c, val_res.type);
            let temp_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + temp_ptr + " = alloca " + prim_ty + "\n");
            c.output_file.write(c.indent + "store " + prim_ty + " " + val_res.reg + ", " + prim_ty + "* " + temp_ptr + "\n");
            let void_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + void_ptr + " = bitcast " + prim_ty + "* " + temp_ptr + " to i8*\n");
            return CompileResult(reg=void_ptr, type=expected_type, origin_type=val_res.type);
        }
    }

    if (is_void_ptr(c, val_res.type) && expected_type != TYPE_NULL && expected_type != TYPE_NULLPTR && !is_void_ptr(c, expected_type)) {
        if (expected_type == TYPE_STRING) {
            return CompileResult(reg=val_res.reg, type=expected_type, origin_type=origin);
        }
        if (!is_pointer_type(c, expected_type)) {
            let dest_ty -> String = get_llvm_type_str(c, expected_type);
            let typed_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + typed_ptr + " = bitcast i8* " + val_res.reg + " to " + dest_ty + "*\n");
            let loaded_val -> String = next_reg(c);
            c.output_file.write(c.indent + loaded_val + " = load " + dest_ty + ", " + dest_ty + "* " + typed_ptr + "\n");
            return CompileResult(reg=loaded_val, type=expected_type, origin_type=0);
        }
    }

    if (expected_type == TYPE_GENERIC_FUNCTION || expected_type == TYPE_GENERIC_METHOD) {
        if (val_res.type >= 100) {
            let f_check -> SymbolInfo = c.func_ret_map.get("" + val_res.type);
            if (f_check is !null) {
                return CompileResult(reg=val_res.reg, type=expected_type, origin_type=val_res.type);
            }
        }
    }
    if ((val_res.type == TYPE_GENERIC_FUNCTION || val_res.type == TYPE_GENERIC_METHOD) && expected_type >= 100) {
        if (c.func_ret_map.get("" + expected_type) is !null) {
            return CompileResult(reg=val_res.reg, type=expected_type, origin_type=origin);
        }
    }

    if (val_res.type >= 100 && expected_type >= 100) {
        if (is_subclass(c, val_res.type, expected_type)) {
            let cast_reg -> String = next_reg(c);
            let dest_ty -> String = get_llvm_type_str(c, expected_type);
            let src_ty -> String = get_llvm_type_str(c, val_res.type);
            c.output_file.write(c.indent + cast_reg + " = bitcast " + src_ty + " " + val_res.reg + " to " + dest_ty + "\n");
            return CompileResult(reg=cast_reg, type=expected_type, origin_type=val_res.origin_type);
        }
    }

    let expected_arr -> ArrayInfo = c.array_info_map.get("" + expected_type);
    if (expected_arr is !null && expected_arr.size == -1) {
        let elem_type -> Int = expected_arr.base_type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);

        let val_arr -> ArrayInfo = c.array_info_map.get("" + val_res.type);
        if (val_arr is !null && val_arr.size > 0 && val_arr.base_type == elem_type) {
            let data_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + data_ptr + " = getelementptr inbounds " + val_arr.llvm_name + ", " + val_arr.llvm_name + "* " + val_res.reg + ", i32 0, i32 0\n");
            
            let v1 -> String = next_reg(c);
            c.output_file.write(c.indent + v1 + " = insertvalue " + expected_arr.llvm_name + " undef, i64 " + val_arr.size + ", 0\n");
            let v2 -> String = next_reg(c);
            c.output_file.write(c.indent + v2 + " = insertvalue " + expected_arr.llvm_name + " " + v1 + ", " + elem_ty_str + "* " + data_ptr + ", 1\n");
            return CompileResult(reg=v2, type=expected_type, origin_type=0);
        }

        let val_vec -> SymbolInfo = c.vector_base_map.get("" + val_res.type);
        if (val_vec is !null && val_vec.type == elem_type) {
            let vec_struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";
            let size_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + size_ptr + " = getelementptr inbounds " + vec_struct_ty + ", " + vec_struct_ty + "* " + val_res.reg + ", i32 0, i32 0\n");
            let size_val -> String = next_reg(c);
            c.output_file.write(c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");
            
            let data_ptr_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + data_ptr_ptr + " = getelementptr inbounds " + vec_struct_ty + ", " + vec_struct_ty + "* " + val_res.reg + ", i32 0, i32 2\n");
            let data_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_ptr_ptr + "\n");
            
            let v1 -> String = next_reg(c);
            c.output_file.write(c.indent + v1 + " = insertvalue " + expected_arr.llvm_name + " undef, i64 " + size_val + ", 0\n");
            let v2 -> String = next_reg(c);
            c.output_file.write(c.indent + v2 + " = insertvalue " + expected_arr.llvm_name + " " + v1 + ", " + elem_ty_str + "* " + data_ptr + ", 1\n");
            return CompileResult(reg=v2, type=expected_type, origin_type=0);
        }
    }

    WhitelangExceptions.throw_type_error(pos, "Type mismatch. Expected " + get_type_name(c, expected_type) + ", got " + get_type_name(c, val_res.type));
    return val_res;
}

func register_string_constant(c -> Compiler, val -> String) -> Int {
    let exist -> StringConstant = c.string_pool.get(val);
    if (exist is !null) {
        return exist.id;
    }
    let s_id -> Int = c.str_count;
    c.str_count += 1;
    let sc -> StringConstant = StringConstant(id=s_id, value=val);
    c.string_list.append(sc);
    c.string_pool.put(val, sc);
    return s_id;
}

func get_string_ptr(s_id -> Int, s_val -> String) -> String {
    let len -> Int = s_val.length() + 1;
    return "getelementptr inbounds ({ i32, i32, [" + len + " x i8] }, { i32, i32, [" + len + " x i8] }* @.str." + s_id + ", i32 0, i32 2, i32 0)";
}

func convert_to_string(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_STRING) { return res; }

    let buf_ptr -> String = emit_alloc_obj(c, "32", "" + TYPE_STRING, "i8*");

    if (res.type == TYPE_INT || res.type == TYPE_BYTE) {
        let val_reg -> String = res.reg;
        if (res.type == TYPE_BYTE) {
            val_reg = next_reg(c);
            c.output_file.write(c.indent + val_reg + " = zext i8 " + res.reg + " to i32\n");
        }
        let fmt -> String = next_reg(c);
        c.output_file.write(c.indent + fmt + " = getelementptr [3 x i8], [3 x i8]* @.fmt_int_simple, i32 0, i32 0\n");
        
        // call snprintf(buf_ptr, 32, "%d", val)
        c.output_file.write(c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 32, i8* " + fmt + ", i32 " + val_reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_LONG) {
        let fmt -> String = next_reg(c);
        c.output_file.write(c.indent + fmt + " = getelementptr [5 x i8], [5 x i8]* @.fmt_long_simple, i32 0, i32 0\n");
        
        c.output_file.write(c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 32, i8* " + fmt + ", i64 " + res.reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_FLOAT) {
        let fmt -> String = next_reg(c);
        c.output_file.write(c.indent + fmt + " = getelementptr [3 x i8], [3 x i8]* @.fmt_float_simple, i32 0, i32 0\n");
        
        c.output_file.write(c.indent + "call i32 (i8*, i64, i8*, ...) @snprintf(i8* " + buf_ptr + ", i64 32, i8* " + fmt + ", double " + res.reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    if (res.type == TYPE_BOOL) {
        let ptr_true -> String = next_reg(c);
        c.output_file.write(c.indent + ptr_true + " = getelementptr [5 x i8], [5 x i8]* @.str_true, i32 0, i32 0\n");
        let ptr_false -> String = next_reg(c);
        c.output_file.write(c.indent + ptr_false + " = getelementptr [6 x i8], [6 x i8]* @.str_false, i32 0, i32 0\n");
        
        let src_reg -> String = next_reg(c);
        c.output_file.write(c.indent + src_reg + " = select i1 " + res.reg + ", i8* " + ptr_true + ", i8* " + ptr_false + "\n");
        
        c.output_file.write(c.indent + "call i8* @strcpy(i8* " + buf_ptr + ", i8* " + src_reg + ")\n");
        return CompileResult(reg=buf_ptr, type=TYPE_STRING);
    }

    let empty_src -> String = next_reg(c);
    c.output_file.write(c.indent + empty_src + " = getelementptr [5 x i8], [5 x i8]* @.str_null, i32 0, i32 0\n");
    c.output_file.write(c.indent + "call i8* @strcpy(i8* " + buf_ptr + ", i8* " + empty_src + ")\n");
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
                name=s_name, 
                type_id=new_id, 
                fields=null, 
                llvm_name="%struct." + s_name, 
                init_body=n.body, is_class=false, 
                vtable_name="", 
                parent_id=0, 
                vtable=null
            );
            c.struct_table.put(s_name, info);
            c.struct_id_map.put("" + new_id, info);
            
        } else if (base.type == NODE_CLASS_DEF) {
            let c_node -> ClassDefNode = stmts[i];
            let c_name -> String = c_node.name_tok.value;
            let new_id -> Int = c.type_counter;
            c.type_counter += 1;
            let info -> StructInfo = StructInfo(
                name=c_name, 
                type_id=new_id, 
                fields=null, 
                llvm_name="%class." + c_name, 
                init_body=c_node, 
                is_class=true, 
                vtable_name="@vtable." + c_name, 
                parent_id=0, 
                vtable=null
            );
            c.struct_table.put(c_name, info);
            c.struct_id_map.put("" + new_id, info);
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
            
            if (c.func_table.get(func_name) is !null) {
                WhitelangExceptions.throw_import_error(f_node.pos, "Function '" + func_name + "' is already defined.");
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
            
            let f_info -> FuncInfo = FuncInfo(name=func_name, base_name=raw_name, ret_type=ret_type_id, arg_types=arg_types, is_varargs=false);
            c.func_table.put(func_name, f_info);

        } else if (base.type == NODE_CLASS_DEF) {
            let c_node -> ClassDefNode = stmts[i];
            let c_name -> String = c_node.name_tok.value;
            let m_vec -> Vector(Struct) = c_node.methods;
            let m_len -> Int = 0; if (m_vec is !null) { m_len = m_vec.length(); }

            let c_info -> StructInfo = c.struct_table.get(c_name);
            let class_type_id -> Int = c_info.type_id;

            let m_idx -> Int = 0;
            while (m_idx < m_len) {
                let m_node -> MethodDefNode = m_vec[m_idx];
                let m_raw_name -> String = m_node.name_tok.value;
                let m_name -> String = c.current_package_prefix + c_name + "_" + m_raw_name; 
                
                if (c.func_table.get(m_name) is !null) {
                    WhitelangExceptions.throw_import_error(m_node.pos, "Method '" + m_name + "' is already defined.");
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
                
                let f_info -> FuncInfo = FuncInfo(name=m_name, base_name=m_raw_name, ret_type=ret_id, arg_types=arg_types, is_varargs=false);
                c.func_table.put(m_name, f_info);
                m_idx += 1;
            }
        }
        i += 1;
    }
}

// === SCOPE ===
func enter_scope(c -> Compiler) -> Void {
    let new_scope -> Scope = Scope(table=HashMap(32), parent=c.symbol_table, gc_vars=[]);
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
        c.output_file.write(c.indent + val_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_gc.reg + "\n");
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
            c.output_file.write(c.indent + val_reg + " = load " + ty_str + ", " + ty_str + "* " + gc_node.reg + "\n");
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
        c.output_file.write(c.indent + "call void @__wl_retain(i8* " + reg + ")\n");
        return;
    }
    c.output_file.write(c.indent + cast_reg + " = bitcast " + src_ty + " " + reg + " to i8*\n");
    c.output_file.write(c.indent + "call void @__wl_retain(i8* " + cast_reg + ")\n");
}

func emit_release(c -> Compiler, reg -> String, type_id -> Int) -> Void {
    if (!is_ref_type(c, type_id)) { return; }

    let cast_reg -> String = next_reg(c);
    let src_ty -> String = get_llvm_type_str(c, type_id);
    if (src_ty == "i8*") {
        c.output_file.write(c.indent + "call void @__wl_release(i8* " + reg + ")\n");
        return;
    }
    c.output_file.write(c.indent + cast_reg + " = bitcast " + src_ty + " " + reg + " to i8*\n");
    c.output_file.write(c.indent + "call void @__wl_release(i8* " + cast_reg + ")\n");
}

func emit_alloc_obj(c -> Compiler, payload_size_reg -> String, type_id_str -> String, dest_llvm_type -> String) -> String {
    let total_size -> String = next_reg(c);
    c.output_file.write(c.indent + total_size + " = add i64 " + payload_size_reg + ", 8\n");
    
    let raw_mem -> String = next_reg(c);
    c.output_file.write(c.indent + raw_mem + " = call i8* @malloc(i64 " + total_size + ")\n");
    
    let rc_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + rc_ptr + " = bitcast i8* " + raw_mem + " to i32*\n");
    c.output_file.write(c.indent + "store i32 0, i32* " + rc_ptr + "\n");
    
    let type_ptr_i8 -> String = next_reg(c);
    c.output_file.write(c.indent + type_ptr_i8 + " = getelementptr inbounds i8, i8* " + raw_mem + ", i32 4\n");
    let type_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + type_ptr + " = bitcast i8* " + type_ptr_i8 + " to i32*\n");
    c.output_file.write(c.indent + "store i32 " + type_id_str + ", i32* " + type_ptr + "\n");
    
    let payload_i8 -> String = next_reg(c);
    c.output_file.write(c.indent + payload_i8 + " = getelementptr inbounds i8, i8* " + raw_mem + ", i32 8\n");
    
    if (dest_llvm_type == "i8*") {
        return payload_i8; 
    }
    
    let final_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + final_ptr + " = bitcast i8* " + payload_i8 + " to " + dest_llvm_type + "\n");
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

            if (target_type_id == TYPE_AUTO) {
                if (v_node.value is null) {
                    WhitelangExceptions.throw_invalid_syntax(v_node.pos, "Cannot infer 'Auto' type without an initializer.");
                }
                target_type_id = get_expr_type(c, v_node.value);
                if (target_type_id == 0 || target_type_id == TYPE_AUTO) {
                    WhitelangExceptions.throw_type_error(v_node.pos, "Failed to statically infer type for 'Auto'. Please specify type explicitly.");
                }
            }

            let llvm_ty_str -> String = get_llvm_type_str(c, target_type_id);
            let ptr_reg -> String = next_reg(c);
            c.output_file.write(c.indent + ptr_reg + " = alloca " + llvm_ty_str + "\n");
            v_node.alloc_reg = ptr_reg;
        }
    }
}

func emit_runtime_error(c -> Compiler, pos -> Position, msg -> String) -> Void {
    let header_fmt -> String = "RuntimeError: " + msg + "\n    at Line %d, Column %d\n\n";
    let header_id -> Int = register_string_constant(c, header_fmt);
    let header_ptr -> String = get_string_ptr(header_id, header_fmt);
    
    let ln -> Int = pos.ln + 1;
    let col -> Int = pos.col + 1;
    c.output_file.write(c.indent + "call i32 (i8*, ...) @printf(i8* " + header_ptr + ", i32 " + ln + ", i32 " + col + ")\n");

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
    
        let code_id -> Int = register_string_constant(c, code_content);
        let code_ptr -> String = get_string_ptr(code_id, code_content);
        c.output_file.write(c.indent + "call i32 (i8*, ...) @printf(i8* " + code_ptr + ")\n");

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
        let arrow_id -> Int = register_string_constant(c, arrow_str);
        let arrow_ptr -> String = get_string_ptr(arrow_id, arrow_str);
        c.output_file.write(c.indent + "call i32 (i8*, ...) @printf(i8* " + arrow_ptr + ")\n");
    }

    c.output_file.write(c.indent + "call void @exit(i32 1)\n");
    c.output_file.write(c.indent + "unreachable\n");
}


func emit_vector_bounds_check(c -> Compiler, vec_reg -> String, idx_reg -> String, struct_ty -> String, pos -> Position) -> Void {
    let size_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    c.output_file.write(c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");

    let idx_i64 -> String = next_reg(c);
    c.output_file.write(c.indent + idx_i64 + " = sext i32 " + idx_reg + " to i64\n");

    let cmp_reg -> String = next_reg(c);
    c.output_file.write(c.indent + cmp_reg + " = icmp uge i64 " + idx_i64 + ", " + size_val + "\n");

    let fail_label -> String = "bounds_fail_" + c.type_counter;
    let ok_label -> String = "bounds_ok_" + c.type_counter;
    c.type_counter += 1;
    
    c.output_file.write(c.indent + "br i1 " + cmp_reg + ", label %" + fail_label + ", label %" + ok_label + "\n");

    c.output_file.write("\n" + fail_label + ":\n");
    emit_runtime_error(c, pos, "Index out of bounds");

    c.output_file.write("\n" + ok_label + ":\n");
}

func emit_array_bounds_check(c -> Compiler, idx_reg -> String, len_val -> String, pos -> Position) -> Void {
    let cmp1 -> String = next_reg(c);
    c.output_file.write(c.indent + cmp1 + " = icmp slt i32 " + idx_reg + ", 0\n");
    let cmp2 -> String = next_reg(c);
    c.output_file.write(c.indent + cmp2 + " = icmp sge i32 " + idx_reg + ", " + len_val + "\n");
    
    let or1 -> String = next_reg(c);
    c.output_file.write(c.indent + or1 + " = or i1 " + cmp1 + ", " + cmp2 + "\n");
    
    let fail_lbl -> String = "arr_fail_" + c.type_counter;
    let ok_lbl -> String = "arr_ok_" + c.type_counter;
    c.type_counter += 1;

    c.output_file.write(c.indent + "br i1 " + or1 + ", label %" + fail_lbl + ", label %" + ok_lbl + "\n");
    c.output_file.write("\n" + fail_lbl + ":\n");
    emit_runtime_error(c, pos, "Index out of bounds.");
    c.output_file.write("\n" + ok_lbl + ":\n");
}

func emit_slice_bounds_check(c -> Compiler, start_reg -> String, end_reg -> String, len_val -> String, pos -> Position) -> Void {
    let cmp1 -> String = next_reg(c);
    c.output_file.write(c.indent + cmp1 + " = icmp slt i32 " + start_reg + ", 0\n");
    let cmp2 -> String = next_reg(c);
    c.output_file.write(c.indent + cmp2 + " = icmp sgt i32 " + start_reg + ", " + end_reg + "\n");
    let cmp3 -> String = next_reg(c);
    c.output_file.write(c.indent + cmp3 + " = icmp sgt i32 " + end_reg + ", " + len_val + "\n");
    
    let or1 -> String = next_reg(c);
    c.output_file.write(c.indent + or1 + " = or i1 " + cmp1 + ", " + cmp2 + "\n");
    let or2 -> String = next_reg(c);
    c.output_file.write(c.indent + or2 + " = or i1 " + or1 + ", " + cmp3 + "\n");
    
    let fail_lbl -> String = "slice_fail_" + c.type_counter;
    let ok_lbl -> String = "slice_ok_" + c.type_counter;
    c.type_counter += 1;
    
    c.output_file.write(c.indent + "br i1 " + or2 + ", label %" + fail_lbl + ", label %" + ok_lbl + "\n");
    c.output_file.write("\n" + fail_lbl + ":\n");
    emit_runtime_error(c, pos, "Slice boundaries out of range.");
    c.output_file.write("\n" + ok_lbl + ":\n");
}

// === COMPILE ===
func compile_arc_hooks(c -> Compiler) -> Void {
    // --- __wl_retain(i8* ptr) ---
    c.output_file.write("define void @__wl_retain(i8* %ptr) {\n");
    c.output_file.write("entry:\n");
    c.output_file.write("  %is_null = icmp eq i8* %ptr, null\n");
    c.output_file.write("  br i1 %is_null, label %done, label %work\n");
    c.output_file.write("work:\n");
    c.output_file.write("  %base = getelementptr i8, i8* %ptr, i32 -8\n");
    c.output_file.write("  %rc_ptr = bitcast i8* %base to i32*\n");
    c.output_file.write("  %rc = load i32, i32* %rc_ptr\n");

    c.output_file.write("  %is_static = icmp eq i32 %rc, -1\n");
    c.output_file.write("  br i1 %is_static, label %done, label %do_retain\n");
    
    c.output_file.write("do_retain:\n");
    c.output_file.write("  %new_rc = add i32 %rc, 1\n");
    c.output_file.write("  store i32 %new_rc, i32* %rc_ptr\n");
    c.output_file.write("  br label %done\n");
    c.output_file.write("done:\n");
    c.output_file.write("  ret void\n");
    c.output_file.write("}\n\n");

    // --- __wl_release(i8* ptr) ---
    c.output_file.write("define void @__wl_release(i8* %ptr) {\n");
    c.output_file.write("entry:\n");
    c.output_file.write("  %is_null = icmp eq i8* %ptr, null\n");
    c.output_file.write("  br i1 %is_null, label %done, label %work\n");
    c.output_file.write("work:\n");
    c.output_file.write("  %base = getelementptr i8, i8* %ptr, i32 -8\n");
    c.output_file.write("  %rc_ptr = bitcast i8* %base to i32*\n");
    c.output_file.write("  %rc = load i32, i32* %rc_ptr\n");

    c.output_file.write("  %is_static = icmp eq i32 %rc, -1\n");
    c.output_file.write("  br i1 %is_static, label %done, label %do_release\n");
    
    c.output_file.write("do_release:\n");
    c.output_file.write("  %new_rc = sub i32 %rc, 1\n");
    c.output_file.write("  store i32 %new_rc, i32* %rc_ptr\n");
    c.output_file.write("  %is_zero = icmp eq i32 %new_rc, 0\n");
    c.output_file.write("  br i1 %is_zero, label %free_check, label %done\n");
    
    // get type_id from header: base[4]
    c.output_file.write("free_check:\n");
    c.output_file.write("  %type_ptr = getelementptr i8, i8* %base, i32 4\n");
    c.output_file.write("  %type_ptr_i32 = bitcast i8* %type_ptr to i32*\n");
    c.output_file.write("  %type_id = load i32, i32* %type_ptr_i32\n");
    
    // switch routing for deep drop
    c.output_file.write("  switch i32 %type_id, label %free_default [\n");

    let drop_list -> Vector(Struct) = c.type_drop_list;
    let d_len -> Int = 0; if (drop_list is !null) { d_len = drop_list.length(); }
    let d_idx -> Int = 0;
    while (d_idx < d_len) {
        let curr_drop -> TypeListNode = drop_list[d_idx];
        c.output_file.write("    i32 " + curr_drop.type + ", label %drop_" + curr_drop.type + "\n");
        d_idx += 1;
    }

    let class_idx -> Int = 100;
    while (class_idx < c.type_counter) {
        let cls_info -> StructInfo = c.struct_id_map.get("" + class_idx);
        if (cls_info is !null && cls_info.is_class) {
            let is_in_drop -> Bool = false;
            let check_idx -> Int = 0;
            while (check_idx < d_len) {
                let c_drop -> TypeListNode = drop_list[check_idx];
                if (c_drop.type == class_idx) { is_in_drop = true; }
                check_idx += 1;
            }
            if (!is_in_drop) {
                c.output_file.write("    i32 " + class_idx + ", label %drop_" + class_idx + "\n");
            }
        }
        class_idx += 1;
    }

    c.output_file.write("  ]\n");

    // default: free(ptr[-8])
    c.output_file.write("\nfree_default:\n");
    c.output_file.write("  call void @free(i8* %base)\n"); 
    c.output_file.write("  br label %done\n");

    // generate drop blocks for complex types
    d_idx = 0;
    while (d_idx < d_len) {
        let curr_drop -> TypeListNode = drop_list[d_idx];
        let t_id -> Int = curr_drop.type;
        c.output_file.write("\ndrop_" + t_id + ":\n");
        
        let v_info -> SymbolInfo = c.vector_base_map.get("" + t_id);
        if (t_id == TYPE_GENERIC_FUNCTION || t_id == TYPE_GENERIC_METHOD || c.func_ret_map.get("" + t_id) is !null) {
            c.output_file.write("  %env_ptr_addr_" + t_id + " = getelementptr inbounds i8, i8* %ptr, i32 8\n");
            c.output_file.write("  %env_ptr_cast_" + t_id + " = bitcast i8* %env_ptr_addr_" + t_id + " to i8**\n");
            c.output_file.write("  %env_val_" + t_id + " = load i8*, i8** %env_ptr_cast_" + t_id + "\n");
            emit_release(c, "%env_val_" + t_id, TYPE_GENERIC_STRUCT);
            c.output_file.write("  br label %free_default\n");
        } else if (v_info is !null) {
            // Vector: free(vector.data)
            let elem_ty_str -> String = get_llvm_type_str(c, v_info.type);
            let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";
            c.output_file.write("  %vec_cast_" + t_id + " = bitcast i8* %ptr to " + struct_ty + "*\n");
            
            if (is_ref_type(c, v_info.type)) {
                c.output_file.write("  %size_ptr_" + t_id + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* %vec_cast_" + t_id + ", i32 0, i32 0\n");
                c.output_file.write("  %size_" + t_id + " = load i64, i64* %size_ptr_" + t_id + "\n");
                c.output_file.write("  %data_ptr_ptr_" + t_id + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* %vec_cast_" + t_id + ", i32 0, i32 2\n");
                c.output_file.write("  %data_ptr_" + t_id + " = load " + elem_ty_str + "*, " + elem_ty_str + "** %data_ptr_ptr_" + t_id + "\n");

                let loop_cond -> String = "vec_drop_cond_" + t_id;
                let loop_body -> String = "vec_drop_body_" + t_id;
                let loop_end -> String = "vec_drop_end_" + t_id;

                c.output_file.write("  %idx_ptr_" + t_id + " = alloca i64\n");
                c.output_file.write("  store i64 0, i64* %idx_ptr_" + t_id + "\n");
                c.output_file.write("  br label %" + loop_cond + "\n");

                c.output_file.write("\n" + loop_cond + ":\n");
                c.output_file.write("  %curr_idx_" + t_id + " = load i64, i64* %idx_ptr_" + t_id + "\n");
                c.output_file.write("  %cmp_" + t_id + " = icmp slt i64 %curr_idx_" + t_id + ", %size_" + t_id + "\n");
                c.output_file.write("  br i1 %cmp_" + t_id + ", label %" + loop_body + ", label %" + loop_end + "\n");

                c.output_file.write("\n" + loop_body + ":\n");
                c.output_file.write("  %slot_" + t_id + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* %data_ptr_" + t_id + ", i64 %curr_idx_" + t_id + "\n");
                c.output_file.write("  %elem_" + t_id + " = load " + elem_ty_str + ", " + elem_ty_str + "* %slot_" + t_id + "\n");
                
                emit_release(c, "%elem_" + t_id, v_info.type);

                c.output_file.write("  %next_idx_" + t_id + " = add i64 %curr_idx_" + t_id + ", 1\n");
                c.output_file.write("  store i64 %next_idx_" + t_id + ", i64* %idx_ptr_" + t_id + "\n");
                c.output_file.write("  br label %" + loop_cond + "\n");

                c.output_file.write("\n" + loop_end + ":\n");
                c.output_file.write("  %data_i8_" + t_id + " = bitcast " + elem_ty_str + "* %data_ptr_" + t_id + " to i8*\n");
                c.output_file.write("  call void @free(i8* %data_i8_" + t_id + ")\n");
            } else {
                c.output_file.write("  %data_ptr_ptr_" + t_id + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* %vec_cast_" + t_id + ", i32 0, i32 2\n");
                c.output_file.write("  %data_ptr_" + t_id + " = load " + elem_ty_str + "*, " + elem_ty_str + "** %data_ptr_ptr_" + t_id + "\n");
                c.output_file.write("  %data_i8_" + t_id + " = bitcast " + elem_ty_str + "* %data_ptr_" + t_id + " to i8*\n");
                c.output_file.write("  call void @free(i8* %data_i8_" + t_id + ")\n");
            }
            c.output_file.write("  br label %free_default\n");
        } else {
            let s_info -> StructInfo = c.struct_id_map.get("" + t_id);
            if (s_info is !null) {
                // Struct: release each ref field
                c.output_file.write("  %struct_cast_" + t_id + " = bitcast i8* %ptr to " + s_info.llvm_name + "*\n");

                let fields_vec -> Vector(Struct) = s_info.fields;
                let f_len -> Int = 0; if (fields_vec is !null) { f_len = fields_vec.length(); }
                let f_idx -> Int = 0;
                
                while (f_idx < f_len) {
                    let f_curr -> FieldInfo = fields_vec[f_idx];
                    if (is_ref_type(c, f_curr.type) && f_curr.name != "_vptr") {
                        c.output_file.write("  %f_ptr_" + t_id + "_" + f_curr.offset + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* %struct_cast_" + t_id + ", i32 0, i32 " + f_curr.offset + "\n");
                        c.output_file.write("  %f_val_" + t_id + "_" + f_curr.offset + " = load " + f_curr.llvm_type + ", " + f_curr.llvm_type + "* %f_ptr_" + t_id + "_" + f_curr.offset + "\n");
                        emit_release(c, "%f_val_" + t_id + "_" + f_curr.offset, f_curr.type);
                    }
                    f_idx += 1;
                }
                c.output_file.write("  br label %free_default\n");
            } else {
                c.output_file.write("  br label %free_default\n");
            }
        }
        d_idx += 1;
    }

    class_idx = 100;
    while (class_idx < c.type_counter) {
        let cls_info -> StructInfo = c.struct_id_map.get("" + class_idx);
        if (cls_info is !null && cls_info.is_class) {
            let is_in_drop -> Bool = false;
            let check_idx -> Int = 0;
            while (check_idx < d_len) {
                let c_drop -> TypeListNode = drop_list[check_idx];
                if (c_drop.type == class_idx) { is_in_drop = true; }
                check_idx += 1;
            }
            if (!is_in_drop) {
                c.output_file.write("\ndrop_" + class_idx + ":\n");

                let vtable -> Vector(Struct) = cls_info.vtable;
                let v_len -> Int = 0; if (vtable is !null) { v_len = vtable.length(); }
                let v_idx -> Int = 0;
                let deinit_func -> FuncInfo = null;
                while (v_idx < v_len) {
                    let f_info -> FuncInfo = vtable[v_idx];
                    if (f_info.base_name == "$deinit") {
                        deinit_func = f_info;
                        break;
                    }
                    v_idx += 1;
                }
                
                if (deinit_func is !null) {
                    let expected_self_node -> TypeListNode = deinit_func.arg_types[0];
                    let expected_ty_str -> String = get_llvm_type_str(c, expected_self_node.type);
                    let ret_ty_str -> String = get_llvm_type_str(c, deinit_func.ret_type);
                    c.output_file.write("  %deinit_cast_" + class_idx + " = bitcast i8* %ptr to " + expected_ty_str + "\n");
                    c.output_file.write("  call " + ret_ty_str + " @" + deinit_func.name + "(" + expected_ty_str + " %deinit_cast_" + class_idx + ")\n");
                }
                c.output_file.write("  %struct_cast_" + class_idx + " = bitcast i8* %ptr to " + cls_info.llvm_name + "*\n");

                let fields_vec -> Vector(Struct) = cls_info.fields;
                let f_len -> Int = 0; if (fields_vec is !null) { f_len = fields_vec.length(); }
                let f_idx -> Int = 0;
                
                while (f_idx < f_len) {
                    let f_curr -> FieldInfo = fields_vec[f_idx];
                    if (is_ref_type(c, f_curr.type) && f_curr.name != "_vptr") {
                        c.output_file.write("  %f_ptr_" + class_idx + "_" + f_curr.offset + " = getelementptr inbounds " + cls_info.llvm_name + ", " + cls_info.llvm_name + "* %struct_cast_" + class_idx + ", i32 0, i32 " + f_curr.offset + "\n");
                        c.output_file.write("  %f_val_" + class_idx + "_" + f_curr.offset + " = load " + f_curr.llvm_type + ", " + f_curr.llvm_type + "* %f_ptr_" + class_idx + "_" + f_curr.offset + "\n");
                        emit_release(c, "%f_val_" + class_idx + "_" + f_curr.offset, f_curr.type);
                    }
                    f_idx += 1;
                }
                c.output_file.write("  br label %free_default\n");
            }
        }
        class_idx += 1;
    }

    c.output_file.write("done:\n");
    c.output_file.write("  ret void\n");
    c.output_file.write("}\n\n");
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

    let is_pkg -> Bool = false;
    if (!raw_path.ends_with(".wl")) {
        if (final_path.ends_with("/_pkg.wl") || final_path.ends_with("\\_pkg.wl") || final_path.ends_with("\\_pgk.wl")) {
            is_pkg = true;
        }
    }

    let import_prefix -> String = "";
    if is_pkg {
        let pkg_mod_name -> String = raw_path;
        if (node.alias_tok is !null) {
            pkg_mod_name = node.alias_tok.value;
        }
        import_prefix = pkg_mod_name + ".";
    } else {
        if (node.alias_tok is !null) {
            import_prefix = node.alias_tok.value + ".";
        } else {
            if (c.current_package_prefix != "") {
                import_prefix = c.current_package_prefix;
            } else {
                import_prefix = ""; 
            }
        }
    }

    if (c.imported_modules.get(final_path) is !null) { 
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
        c.loaded_packages.put(pkg_mod_name, pkg_val);
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
        let file_val -> StringConstant = StringConstant(id=0, value=import_prefix);
        c.loaded_files.put(file_mod_name, file_val);
    }

    c.imported_modules.put(final_path, marker);
    
    let old_prefix -> String = c.current_package_prefix;
    let old_dir -> String = c.current_dir;

    c.current_dir = get_dir_name(final_path);
    c.current_package_prefix = import_prefix;

    let f -> File = File(final_path, "rb");
    if (f is null) { WhitelangExceptions.throw_import_error(node.pos, "Failed to open: " + final_path); }
    let source -> String = f.read_all();
    f.close();

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

    if (target_type_id == TYPE_AUTO) {
        if (node.value is null) {
            WhitelangExceptions.throw_type_error(node.pos, "Cannot infer 'Auto' type without an initializer.");
        }
        target_type_id = get_expr_type(c, node.value);
        if (target_type_id == 0 || target_type_id == TYPE_AUTO) {
             WhitelangExceptions.throw_type_error(node.pos, "Failed to statically infer type for 'Auto'.");
        }
        if (target_type_id == TYPE_NULL || target_type_id == TYPE_NULLPTR || target_type_id == TYPE_VOID) {
             WhitelangExceptions.throw_type_error(node.pos, "Cannot infer 'Auto' as null or Void.");
        }
    }

    let llvm_ty_str -> String = get_llvm_type_str(c, target_type_id);
    let var_name -> String = node.name_tok.value;

    if (c.scope_depth == 0) {
        let global_name -> String = "@" + var_name;
        let init_val_str -> String = "0";
        if (target_type_id == TYPE_STRING || target_type_id >= 100 || target_type_id == TYPE_GENERIC_STRUCT || target_type_id == TYPE_GENERIC_CLASS || target_type_id == TYPE_GENERIC_FUNCTION || target_type_id == TYPE_GENERIC_METHOD) { init_val_str = "null";}
        
        if (node.value is !null) {
            let val_node -> BaseNode = node.value;
            if (val_node.type == NODE_STRING) {
                let s_node -> StringNode = node.value;
                let s_val -> String = s_node.tok.value;
                let s_id -> Int = register_string_constant(c, s_val);
                init_val_str = get_string_ptr(s_id, s_val);
            }
            else if (val_node.type == NODE_NULLPTR) {
                if (is_pointer_type(c, target_type_id) == false) {
                    WhitelangExceptions.throw_invalid_syntax(node.pos, "Global 'nullptr' can only be assigned to pointer types.");
                }
                init_val_str = "null";
            }
            else if (val_node.type == NODE_NULL) {
                if (is_pointer_type(c, target_type_id) == true) {
                WhitelangExceptions.throw_invalid_syntax(node.pos, "Global 'null' cannot be assigned to explicit pointer types. Use 'nullptr'.");
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
                WhitelangExceptions.throw_invalid_syntax(node.pos, "Global variable initialisation must be a constant literal. ");
            }
        }
        
        c.output_file.write(global_name + " = global " + llvm_ty_str + " " + init_val_str + "\n");
        c.global_symbol_table.put(var_name, SymbolInfo(reg=global_name, type=target_type_id, origin_type=target_type_id, is_const=node.is_const));
        return void_result();
    }

    let ptr_reg -> String = node.alloc_reg;
    let origin_id -> Int = target_type_id;

    if (node.value is null) {
        let s_info -> StructInfo = c.struct_id_map.get("" + target_type_id);
        let is_valid_struct -> Bool = false;
        
        if (s_info is !null) {
            if (c.array_info_map.get("" + target_type_id) is null && 
                c.vector_base_map.get("" + target_type_id) is null && 
                c.func_ret_map.get("" + target_type_id) is null) {
                is_valid_struct = true;
            }
        }

        if (is_valid_struct) {
            let fake_args -> Vector(Struct) = [];
            let fake_call -> CallNode = CallNode(type=16, callee=null, args=fake_args, pos=node.pos);
            let val_res -> CompileResult = null;
            
            if (s_info.is_class) {
                val_res = compile_class_init(c, s_info, fake_call);
            } else {
                val_res = compile_struct_init(c, s_info, fake_call);
            }
            
            if (c.scope_depth > 0) {
                emit_retain(c, val_res.reg, target_type_id);
            }
            c.output_file.write(c.indent + "store " + llvm_ty_str + " " + val_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
        } else {
            WhitelangExceptions.throw_missing_initializer(node.pos, "Local variable '" + var_name + "' must be initialised immediately upon declaration.");
        }
    } else {
        let is_array_init -> Bool = false;
        let val_base -> BaseNode = node.value;
        let target_arr -> ArrayInfo = c.array_info_map.get("" + target_type_id);
        if (target_arr is !null && val_base.type == NODE_VECTOR_LIT) {
            if (target_arr.size == -1) {
                WhitelangExceptions.throw_type_error(node.pos, "Cannot initialise Array(Type) slice directly from a literal.");
            }

            is_array_init = true;
            let lit_node -> VectorLitNode = node.value;
            
            if (lit_node.count > target_arr.size) {
                WhitelangExceptions.throw_type_error(node.pos, "Array literal too large: expected " + target_arr.size + " elements.");
            }

            compile_array_literal(c, lit_node, target_type_id, ptr_reg);
        }

        if (!is_array_init) {
            c.expected_type = target_type_id;
            let val_res -> CompileResult = compile_node(c, node.value);
            c.expected_type = 0;

            val_res = emit_implicit_cast(c, val_res, target_type_id, node.pos);
            if (target_type_id == TYPE_GENERIC_STRUCT || target_type_id == TYPE_GENERIC_CLASS) {
                if (val_res.origin_type >= 100) { origin_id = val_res.origin_type; }
            } else if (target_type_id == TYPE_GENERIC_FUNCTION || target_type_id == TYPE_GENERIC_METHOD) {
                if (val_res.origin_type >= 100) { origin_id = val_res.origin_type; }
            }

            if (c.scope_depth > 0) {
                emit_retain(c, val_res.reg, target_type_id);
            }

            c.output_file.write(c.indent + "store " + llvm_ty_str + " " + val_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
        }
    }

    let curr_scope -> Scope = c.symbol_table;
    curr_scope.table.put(var_name, SymbolInfo(reg=ptr_reg, type=target_type_id, origin_type=origin_id, is_const=node.is_const));

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
        c.output_file.write(c.indent + old_val_reg + " = load " + ty_str + ", " + ty_str + "* " + info.reg + "\n");

        emit_release(c, old_val_reg, info.type);
    }
    
    let ty_str -> String = get_llvm_type_str(c, info.type);
    c.output_file.write(c.indent + "store " + ty_str + " " + val_res.reg + ", " + ty_str + "* " + info.reg + "\n");
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
    
    c.output_file.write(c.indent + "br i1 " + cond_res.reg + ", label %" + label_then + ", label %" + target_else + "\n");
    
    c.output_file.write("\n" + label_then + ":\n");
    compile_node(c, node.body);
    c.output_file.write(c.indent + "br label %" + label_merge + "\n");
    
    if (node.else_body is !null) {
        c.output_file.write("\n" + label_else + ":\n");
        compile_node(c, node.else_body);
        c.output_file.write(c.indent + "br label %" + label_merge + "\n");
    }

    c.output_file.write("\n" + label_merge + ":\n");
    return void_result();
}

func compile_while(c -> Compiler, node -> WhileNode) -> CompileResult {
    let label_cond -> String = next_label(c);
    let label_body -> String = next_label(c);
    let label_end  -> String = next_label(c);

    let current_scope -> LoopScope = LoopScope(label_continue=label_cond, label_break=label_end, parent=c.loop_stack);
    c.loop_stack = current_scope;

    c.output_file.write(c.indent + "br label %" + label_cond + "\n");
    c.output_file.write("\n" + label_cond + ":\n");
    
    let cond_res -> CompileResult = compile_node(c, node.condition);
    if (cond_res.type != TYPE_BOOL) {
        WhitelangExceptions.throw_type_error(node.pos, "While condition must be a Bool. ");
    }
    c.output_file.write(c.indent + "br i1 " + cond_res.reg + ", label %" + label_body + ", label %" + label_end + "\n");

    c.output_file.write("\n" + label_body + ":\n");
    compile_node(c, node.body);
    c.output_file.write(c.indent + "br label %" + label_cond + "\n");

    c.output_file.write("\n" + label_end + ":\n");
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
    
    c.output_file.write(c.indent + "br label %" + label_cond + "\n");
    c.output_file.write("\n" + label_cond + ":\n");
    if (node.cond is !null) {
        let cond_res -> CompileResult = compile_node(c, node.cond);
        if (cond_res.type != TYPE_BOOL) {
            WhitelangExceptions.throw_type_error(node.pos, "For condition must be a Bool. ");
        }
        c.output_file.write(c.indent + "br i1 " + cond_res.reg + ", label %" + label_body + ", label %" + label_end + "\n");
    } else {
        c.output_file.write(c.indent + "br label %" + label_body + "\n");
    }

    c.output_file.write("\n" + label_body + ":\n");
    compile_node(c, node.body);

    c.output_file.write(c.indent + "br label %" + label_step + "\n");
    c.output_file.write("\n" + label_step + ":\n");
    if (node.step is !null) {
        compile_node(c, node.step);
    }
    c.output_file.write(c.indent + "br label %" + label_cond + "\n");
    c.output_file.write("\n" + label_end + ":\n");
    c.loop_stack = current_scope.parent;
    
    return void_result();
}

func compile_ptr_assign(c -> Compiler, node -> PtrAssignNode) -> CompileResult {
    let d_node -> DerefNode = node.pointer;
    let ptr_res -> CompileResult = compile_node(c, d_node.node);

    let i -> Int = 0;
    let curr_reg -> String = ptr_res.reg;
    let curr_type -> Int = ptr_res.type;

    while (i < d_node.level - 1) {
        if (curr_type == TYPE_NULL) { 
            WhitelangExceptions.throw_type_error(node.pos, "Cannot dereference 'nullptr'."); 
        }
        let base_info -> SymbolInfo = c.ptr_base_map.get("" + curr_type);
        if (base_info is null) { 
            WhitelangExceptions.throw_type_error(node.pos, "Cannot dereference non-pointer."); 
        }
        
        let next_type -> Int = base_info.type;
        if (next_type == TYPE_VOID) {
            WhitelangExceptions.throw_type_error(d_node.pos, "Cannot dereference 'ptr Void'. Cast it to a specific pointer type first.");
        }
        let ty_str -> String = get_llvm_type_str(c, next_type);
        let next_reg -> String = next_reg(c);
        c.output_file.write(c.indent + next_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_reg + "\n");
        
        curr_reg = next_reg;
        curr_type = next_type;
        i += 1;
    }

    if (curr_type == TYPE_NULL) {
        WhitelangExceptions.throw_null_dereference_error(node.pos, "Cannot dereference 'nullptr'. ");
    }

    let final_base_info -> SymbolInfo = c.ptr_base_map.get("" + curr_type);
    if (final_base_info is null) { 
        WhitelangExceptions.throw_type_error(node.pos, "Cannot assign to non-pointer."); 
    }
    
    let target_type_id -> Int = final_base_info.type;

    c.expected_type = target_type_id;
    let val_res -> CompileResult = compile_node(c, node.value);
    c.expected_type = 0;
    
    val_res = emit_implicit_cast(c, val_res, target_type_id, node.pos);
    
    let llvm_ty -> String = get_llvm_type_str(c, target_type_id);

    if (is_ref_type(c, target_type_id)) {
        emit_retain(c, val_res.reg, target_type_id);
        let old_val_reg -> String = next_reg(c);
        c.output_file.write(c.indent + old_val_reg + " = load " + llvm_ty + ", " + llvm_ty + "* " + curr_reg + "\n");
        emit_release(c, old_val_reg, target_type_id);
    }
    
    c.output_file.write(c.indent + "store " + llvm_ty + " " + val_res.reg + ", " + llvm_ty + "* " + curr_reg + "\n");

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

    let f_info -> FuncInfo = c.func_table.get(func_name);
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

    c.output_file.write("define " + llvm_ret_type + " @" + func_name + "(" + params_str + ") {\n");
    c.output_file.write("entry:\n");

    let old_sym -> Scope = c.symbol_table;
    c.symbol_table = Scope(table=HashMap(32), parent=null, gc_vars=[]);
    
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
        c.output_file.write(c.indent + addr_reg + " = alloca " + llvm_ty + "\n");
        c.output_file.write(c.indent + "store " + llvm_ty + " %arg" + arg_idx + ", " + llvm_ty + "* " + addr_reg + "\n");
        let curr_scope -> Scope = c.symbol_table;
        curr_scope.table.put(p_name, SymbolInfo(reg=addr_reg, type=target_type_id, origin_type=target_type_id));
        
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
            c.output_file.write(c.indent + "ret void\n");
        } else {
            let zero_val -> String = "0";
            if (ret_type_id == TYPE_FLOAT) { zero_val = "0.0"; }
            else if (ret_type_id == TYPE_STRING || ret_type_id >= 100 || ret_type_id == TYPE_GENERIC_STRUCT || ret_type_id == TYPE_GENERIC_CLASS || ret_type_id == TYPE_GENERIC_FUNCTION || ret_type_id == TYPE_GENERIC_METHOD) { zero_val = "null"; }
            
            c.output_file.write(c.indent + "ret " + llvm_ret_type + " " + zero_val + "\n");
        }
    }
    
    c.output_file.write("}\n\n");

    // restore scope
    c.symbol_table = old_sym;
    c.scope_depth = 0;
    
    c.curr_func = null;
    
    return void_result();
}

func compile_method_def(c -> Compiler, class_name -> String, node -> MethodDefNode) -> CompileResult {
    let raw_name -> String = node.name_tok.value;
    let m_name -> String = c.current_package_prefix + class_name + "_" + raw_name;
    
    let f_info -> FuncInfo = c.func_table.get(m_name);
    let ret_type_id -> Int = f_info.ret_type;
    let llvm_ret_type -> String = get_llvm_type_str(c, ret_type_id);

    c.current_ret_type = ret_type_id;

    let c_info -> StructInfo = c.struct_table.get(class_name);
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

    c.output_file.write("define " + llvm_ret_type + " @" + m_name + "(" + params_str + ") {\n");
    c.output_file.write("entry:\n");

    let old_sym -> Scope = c.symbol_table;
    c.symbol_table = Scope(table=HashMap(32), parent=null, gc_vars=[]);
    
    c.reg_count = 0; 
    c.scope_depth = 1;
    c.curr_func = f_info;
    
    let self_addr -> String = next_reg(c);
    c.output_file.write(c.indent + self_addr + " = alloca " + class_ptr_llvm + "\n");
    c.output_file.write(c.indent + "store " + class_ptr_llvm + " %arg0, " + class_ptr_llvm + "* " + self_addr + "\n");
    let curr_scope -> Scope = c.symbol_table;
    curr_scope.table.put("self", SymbolInfo(reg=self_addr, type=class_type_id, origin_type=class_type_id, is_const=false));

    arg_idx = 0;
    while (arg_idx < p_len) {
        let p -> ParamNode = params[arg_idx];
        let p_name -> String = p.name_tok.value;
        let target_type_id -> Int = resolve_type(c, p.type_tok);
        let llvm_ty -> String = get_llvm_type_str(c, target_type_id);
        let addr_reg -> String = next_reg(c); 
        c.output_file.write(c.indent + addr_reg + " = alloca " + llvm_ty + "\n");
        let arg_num -> Int = arg_idx + 1;
        c.output_file.write(c.indent + "store " + llvm_ty + " %arg" + arg_num + ", " + llvm_ty + "* " + addr_reg + "\n");
        curr_scope.table.put(p_name, SymbolInfo(reg=addr_reg, type=target_type_id, origin_type=target_type_id, is_const=false));
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
            c.output_file.write(c.indent + "ret void\n");
        } else {
            let zero_val -> String = "0";
            if (ret_type_id == TYPE_FLOAT) { zero_val = "0.0"; }
            else if (ret_type_id == TYPE_STRING || ret_type_id >= 100 || ret_type_id == TYPE_GENERIC_STRUCT || ret_type_id == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
            c.output_file.write(c.indent + "ret " + llvm_ret_type + " " + zero_val + "\n");
        }
    }
    
    c.output_file.write("}\n\n");
    c.symbol_table = old_sym;
    c.scope_depth = 0;
    c.curr_func = null;
    
    return void_result();
}

func compile_class_method_call(c -> Compiler, s_info -> StructInfo, obj_res -> CompileResult, method_name -> String, n_call -> CallNode) -> CompileResult {
    let vtable_vec -> Vector(Struct) = s_info.vtable;
    let v_len -> Int = 0; if (vtable_vec is !null) { v_len = vtable_vec.length(); }
    
    let m_idx -> Int = 0;
    let found -> Bool = false;
    let f_info -> FuncInfo = null;
    
    while (m_idx < v_len) {
        let m -> FuncInfo = vtable_vec[m_idx];
        if (m.base_name == method_name) {
            f_info = m;
            found = true;
            break;
        }
        m_idx += 1;
    }
    
    if (!found) { WhitelangExceptions.throw_name_error(n_call.pos, "Method '" + method_name + "' not found in class '" + s_info.name + "'."); }
    
    let sig -> String = get_func_sig_str(c, f_info); 
    let class_llvm_ty -> String = s_info.llvm_name;
    let obj_ptr -> String = obj_res.reg;
    
    let vptr_addr -> String = next_reg(c);
    c.output_file.write(c.indent + vptr_addr + " = getelementptr inbounds " + class_llvm_ty + ", " + class_llvm_ty + "* " + obj_ptr + ", i32 0, i32 0\n");
    
    let vtable_i8ptr -> String = next_reg(c);
    c.output_file.write(c.indent + vtable_i8ptr + " = load i8*, i8** " + vptr_addr + "\n");
    
    let vtable_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + vtable_ptr + " = bitcast i8* " + vtable_i8ptr + " to %vtable_type." + s_info.name + "*\n");
    
    let method_i8ptr_addr -> String = next_reg(c);
    c.output_file.write(c.indent + method_i8ptr_addr + " = getelementptr inbounds %vtable_type." + s_info.name + ", %vtable_type." + s_info.name + "* " + vtable_ptr + ", i32 0, i32 " + m_idx + "\n");
    
    let method_i8ptr -> String = next_reg(c);
    c.output_file.write(c.indent + method_i8ptr + " = load i8*, i8** " + method_i8ptr_addr + "\n");
    
    let func_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + func_ptr + " = bitcast i8* " + method_i8ptr + " to " + sig + "\n");

    let self_type_node -> TypeListNode = f_info.arg_types[0];
    let self_expected_type -> Int = self_type_node.type;
    
    c.expected_type = self_expected_type;
    let casted_obj -> CompileResult = emit_implicit_cast(c, obj_res, self_expected_type, n_call.pos);
    c.expected_type = 0;

    let args_str -> String = get_llvm_type_str(c, self_expected_type) + " " + casted_obj.reg; 
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
        c.output_file.write(c.indent + "call " + llvm_ret_type + " " + func_ptr + "(" + args_str + ")\n");
        return CompileResult(reg="", type=TYPE_VOID, origin_type=0);
    } else {
        let call_res -> String = next_reg(c);
        c.output_file.write(c.indent + call_res + " = call " + llvm_ret_type + " " + func_ptr + "(" + args_str + ")\n");
        return CompileResult(reg=call_res, type=f_info.ret_type, origin_type=0);
    }
}

func compile_local_closure(c -> Compiler, func_def -> FunctionDefNode) -> CompileResult {
    let scope -> CaptureScope = CaptureScope(local_vars=HashMap(32), captured_vars=HashMap(32), captured_list=[]);
    let params -> Vector(Struct) = func_def.params;
    let p_len -> Int = 0; if (params is !null) { p_len = params.length(); }
    let p_i -> Int = 0;
    while (p_i < p_len) {
        let p_node -> ParamNode = params[p_i];
        scope.local_vars.put(p_node.name_tok.value, TypeListNode(type=1));
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
        if (c.global_symbol_table.get(v_name) is !null) { is_global = true; }
        if (c.func_table.get(v_name) is !null) { is_global = true; }
        if (c.struct_table.get(v_name) is !null) { is_global = true; }
        if (c.loaded_packages.get(v_name) is !null) { is_global = true; }
        if (c.loaded_files.get(v_name) is !null) { is_global = true; }

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

    let env_info -> StructInfo = StructInfo(name=env_struct_name, type_id=env_id, fields=env_fields, llvm_name=llvm_env_name, init_body=null, is_class=false, vtable_name="", parent_id=0, vtable=null);
    c.struct_id_map.put("" + env_id, env_info);
    c.type_drop_list.append(TypeListNode(type=env_id));

    let size_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + size_ptr + " = getelementptr " + llvm_env_name + ", " + llvm_env_name + "* null, i32 1\n");
    let size_i64 -> String = next_reg(c);
    c.output_file.write(c.indent + size_i64 + " = ptrtoint " + llvm_env_name + "* " + size_ptr + " to i64\n");
    let env_payload -> String = emit_alloc_obj(c, size_i64, "" + env_id, llvm_env_name + "*");

    let env_payload_i8 -> String = next_reg(c);
    c.output_file.write(c.indent + env_payload_i8 + " = bitcast " + llvm_env_name + "* " + env_payload + " to i8*\n");

    t_i = 0;
    while (t_i < t_len) {
        let v_name -> String = captures[t_i];
        let t_node -> TypeListNode = capture_types[t_i];
        let v_type -> Int = t_node.type;
        let llvm_ty -> String = get_llvm_type_str(c, v_type);
        let info -> SymbolInfo = find_symbol(c, v_name);
        
        let val_reg -> String = next_reg(c);
        c.output_file.write(c.indent + val_reg + " = load " + llvm_ty + ", " + llvm_ty + "* " + info.reg + "\n");
        if (is_ref_type(c, v_type)) { emit_retain(c, val_reg, v_type); }
        
        let slot_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + slot_ptr + " = getelementptr inbounds " + llvm_env_name + ", " + llvm_env_name + "* " + env_payload + ", i32 0, i32 " + t_i + "\n");
        c.output_file.write(c.indent + "store " + llvm_ty + " " + val_reg + ", " + llvm_ty + "* " + slot_ptr + "\n");
        t_i += 1;
    }

    let old_file -> File = c.output_file;
    let tmp_name -> String = ".lambda_" + env_id + ".ll";
    c.output_file = File(tmp_name, "w");
    
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
    
    c.output_file.write("define " + ret_ty_str + " @" + lambda_name + "(" + sig_def + ") {\nentry:\n");
    let old_sym -> Scope = c.symbol_table;
    let old_depth -> Int = c.scope_depth;
    let old_reg -> Int = c.reg_count;
    let old_ret -> Int = c.current_ret_type;
    
    c.symbol_table = Scope(table=HashMap(32), parent=null, gc_vars=[]);
    c.scope_depth = 1;
    c.reg_count = 1;
    c.current_ret_type = ret_type_id;

    let lambda_env_ptr -> String = "%lambda_env_ptr";
    c.output_file.write("  " + lambda_env_ptr + " = bitcast i8* %raw_env to " + llvm_env_name + "*\n");

    t_i = 0;
    while (t_i < t_len) {
        let v_name -> String = captures[t_i];
        let t_node -> TypeListNode = capture_types[t_i];
        let v_type -> Int = t_node.type;
        let llvm_ty -> String = get_llvm_type_str(c, v_type);

        let slot_ptr -> String = "%env.slot." + t_i;
        c.output_file.write("  " + slot_ptr + " = getelementptr inbounds " + llvm_env_name + ", " + llvm_env_name + "* " + lambda_env_ptr + ", i32 0, i32 " + t_i + "\n");

        c.symbol_table.table.put(v_name, SymbolInfo(reg=slot_ptr, type=v_type, origin_type=v_type, is_const=false));
        t_i += 1;
    }
    
    p_i = 0;
    while (p_i < p_len) {
        let p_node -> ParamNode = params[p_i];
        let p_ty_id -> Int = resolve_type(c, p_node.type_tok);
        let p_ty -> String = get_llvm_type_str(c, p_ty_id);
        let addr_reg -> String = next_reg(c);
        c.output_file.write("  " + addr_reg + " = alloca " + p_ty + "\n");
        c.output_file.write("  store " + p_ty + " %arg" + p_i + ", " + p_ty + "* " + addr_reg + "\n");
        c.symbol_table.table.put(p_node.name_tok.value, SymbolInfo(reg=addr_reg, type=p_ty_id, origin_type=p_ty_id, is_const=false));
        p_i += 1;
    }
    
    hoist_allocas(c, func_def.body);
    compile_node(c, func_def.body);
    
    if (ret_type_id == TYPE_VOID) {
        c.output_file.write("  ret void\n");
    } else {
        let zero_val -> String = "0";
        if (ret_type_id == TYPE_FLOAT) { zero_val = "0.0"; }
        else if (ret_type_id == TYPE_STRING || ret_type_id >= 100 || ret_type_id == TYPE_GENERIC_STRUCT || ret_type_id == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
        c.output_file.write("  ret " + ret_ty_str + " " + zero_val + "\n");
    }
    c.output_file.write("}\n\n");
    
    c.symbol_table = old_sym;
    c.scope_depth = old_depth;
    c.reg_count = old_reg;
    c.current_ret_type = old_ret;
    
    c.output_file.close();
    c.output_file = old_file;
    let tmp_read -> File = File(tmp_name, "rb");
    let lambda_ir -> String = tmp_read.read_all();
    tmp_read.close();
    file_io.remove_file(tmp_name);
    c.global_buffer = c.global_buffer + lambda_ir;

    let clo_payload -> String = emit_alloc_obj(c, "16", "8", "i8*");

    let clo_func_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + clo_func_ptr + " = bitcast i8* " + clo_payload + " to i8**\n");
    
    let lambda_casted -> String = next_reg(c);
    c.output_file.write(c.indent + lambda_casted + " = bitcast " + ret_ty_str + " (" + sig_ty + ")* @" + lambda_name + " to i8*\n");
    c.output_file.write(c.indent + "store i8* " + lambda_casted + ", i8** " + clo_func_ptr + "\n");

    let clo_env_ptr_i8 -> String = next_reg(c);
    c.output_file.write(c.indent + clo_env_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_payload + ", i32 8\n");
    let clo_env_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + clo_env_ptr + " = bitcast i8* " + clo_env_ptr_i8 + " to i8**\n");
    c.output_file.write(c.indent + "store i8* " + env_payload_i8 + ", i8** " + clo_env_ptr + "\n");

    emit_retain(c, clo_payload, specific_type_id);
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

        c.output_file.write(c.indent + "ret " + target_ty + " " + ret_val_reg + "\n");
    } else {
        if (c.current_ret_type != TYPE_VOID) {
            WhitelangExceptions.throw_type_error(node.pos, "Non-void function must return a value. ");
        }
        cleanup_all_scopes(c);
        c.output_file.write(c.indent + "ret void\n");
    }
    
    return void_result();
}

func compile_struct_def(c -> Compiler, node -> StructDefNode) -> CompileResult {
    let struct_name -> String = node.name_tok.value;

    let full_name -> String = "struct." + struct_name;
    if (c.struct_table.get(full_name) is !null) {
        WhitelangExceptions.throw_import_error(node.pos, "Struct '" + struct_name + "' is already defined in another module.");
    }

    let info -> StructInfo = c.struct_table.get(struct_name);
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
    c.output_file.write(def_str);
    
    return void_result();
}

func compile_struct_init(c -> Compiler, s_info -> StructInfo, n_call -> CallNode) -> CompileResult {
    let size_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + size_ptr + " = getelementptr " + s_info.llvm_name + ", " + s_info.llvm_name + "* null, i64 1\n");
    let size_i64 -> String = next_reg(c);
    c.output_file.write(c.indent + size_i64 + " = ptrtoint " + s_info.llvm_name + "* " + size_ptr + " to i64\n");
    let obj_ptr -> String = emit_alloc_obj(c, size_i64, "" + s_info.type_id, s_info.llvm_name + "*");

    let fields_vec -> Vector(Struct) = s_info.fields;
    let f_len -> Int = 0;
    if (fields_vec is !null) { f_len = fields_vec.length(); }
    let f_idx -> Int = 0;
    
    while (f_idx < f_len) {
        let f_curr -> FieldInfo = fields_vec[f_idx];
        let f_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_ptr + ", i32 0, i32 " + f_curr.offset + "\n");
        let zero_val -> String = "0";
        if (f_curr.type == TYPE_FLOAT) { zero_val = "0.0"; }
        else if (f_curr.type == TYPE_STRING || f_curr.type >= 100 || f_curr.type == TYPE_GENERIC_STRUCT || f_curr.type == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
        
        c.output_file.write(c.indent + "store " + f_curr.llvm_type + " " + zero_val + ", " + f_curr.llvm_type + "* " + f_ptr + "\n");
        
        f_idx += 1;
    }

    if (s_info.init_body is !null) {
        enter_scope(c);
        let this_ptr_addr -> String = next_reg(c);
        let struct_ptr_ty -> String = s_info.llvm_name + "*";
        
        c.output_file.write(c.indent + this_ptr_addr + " = alloca " + struct_ptr_ty + "\n");
        c.output_file.write(c.indent + "store " + struct_ptr_ty + " " + obj_ptr + ", " + struct_ptr_ty + "* " + this_ptr_addr + "\n");
        c.symbol_table.table.put("this", SymbolInfo(reg=this_ptr_addr, type=s_info.type_id, origin_type=s_info.type_id));
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
            c.output_file.write(c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_ptr + ", i32 0, i32 " + target_f.offset + "\n");
            if (is_ref_type(c, target_f.type)) {
                emit_retain(c, val_res.reg, target_f.type);
            }
            c.output_file.write(c.indent + "store " + target_f.llvm_type + " " + val_res.reg + ", " + target_f.llvm_type + "* " + f_ptr + "\n");
        }
        arg_idx += 1;
    }
    return CompileResult(reg=obj_ptr, type=s_info.type_id);
}

func compile_class_init(c -> Compiler, s_info -> StructInfo, n_call -> CallNode) -> CompileResult {
    let size_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + size_ptr + " = getelementptr " + s_info.llvm_name + ", " + s_info.llvm_name + "* null, i64 1\n");
    let size_i64 -> String = next_reg(c);
    c.output_file.write(c.indent + size_i64 + " = ptrtoint " + s_info.llvm_name + "* " + size_ptr + " to i64\n");
    let obj_ptr -> String = emit_alloc_obj(c, size_i64, "" + s_info.type_id, s_info.llvm_name + "*");

    let fields_vec -> Vector(Struct) = s_info.fields;
    let f_len -> Int = 0;
    if (fields_vec is !null) { f_len = fields_vec.length(); }
    let f_idx -> Int = 0;

    while (f_idx < f_len) {
        let f_curr -> FieldInfo = fields_vec[f_idx];
        let f_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_ptr + ", i32 0, i32 " + f_curr.offset + "\n");
        
        if (f_curr.name == "_vptr") {
            let vtable_cast -> String = next_reg(c);
            c.output_file.write(c.indent + vtable_cast + " = bitcast %vtable_type." + s_info.name + "* " + s_info.vtable_name + " to i8*\n");
            c.output_file.write(c.indent + "store i8* " + vtable_cast + ", i8** " + f_ptr + "\n");
        } else {
            let zero_val -> String = "0";
            if (f_curr.type == TYPE_FLOAT) { zero_val = "0.0"; }
            else if (f_curr.type == TYPE_STRING || f_curr.type >= 100 || f_curr.type == TYPE_GENERIC_STRUCT || f_curr.type == TYPE_GENERIC_FUNCTION) { zero_val = "null"; }
            
            c.output_file.write(c.indent + "store " + f_curr.llvm_type + " " + zero_val + ", " + f_curr.llvm_type + "* " + f_ptr + "\n");
        }
        f_idx += 1;
    }

    let init_name -> String = c.current_package_prefix + s_info.name + "_$init";
    let init_func -> FuncInfo = c.func_table.get(init_name);
    
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

        c.output_file.write(c.indent + "call void @" + init_name + "(" + args_str + ")\n");
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
    if (c.struct_table.get(full_name) is !null) {
        WhitelangExceptions.throw_import_error(node.pos, "Class '" + class_name + "' is already defined.");
    }

    let info -> StructInfo = c.struct_table.get(class_name);

    let parent_info -> StructInfo = null;
    if (node.parent_tok is !null) {
        let p_name -> String = node.parent_tok.value;
        parent_info = c.struct_table.get(p_name);
        if (parent_info is null || !parent_info.is_class) {
            WhitelangExceptions.throw_name_error(node.pos, "Parent class '" + p_name + "' is not defined or is not a class.");
        }
        info.parent_id = parent_info.type_id;
    }

    let llvm_body -> String = "";
    let fields_vec -> Vector(Struct) = [];
    let vtable_vec -> Vector(Struct) = [];
    let current_offset -> Int = 0;

    if (parent_info is !null) {
        let p_fields -> Vector(Struct) = parent_info.fields;
        let pf_len -> Int = p_fields.length();
        let pf_i -> Int = 0;
        while (pf_i < pf_len) {
            let pf -> FieldInfo = p_fields[pf_i];
            fields_vec.append(FieldInfo(name=pf.name, type=pf.type, llvm_type=pf.llvm_type, offset=pf.offset));
            if (pf_i > 0) { llvm_body += ", "; }
            llvm_body += pf.llvm_type;
            current_offset += 1;
            pf_i += 1;
        }

        let p_vt -> Vector(Struct) = parent_info.vtable;
        let pvt_len -> Int = p_vt.length();
        let pvt_i -> Int = 0;
        while (pvt_i < pvt_len) {
            vtable_vec.append(p_vt[pvt_i]);
            pvt_i += 1;
        }
    } else {
        fields_vec.append(FieldInfo(name="_vptr", type=TYPE_VOID, llvm_type="i8*", offset=0));
        llvm_body = "i8*";
        current_offset = 1;
    }

    let my_fields -> Vector(Struct) = node.fields;
    let mf_len -> Int = 0; if (my_fields is !null) { mf_len = my_fields.length(); }
    let mf_idx -> Int = 0;
    while (mf_idx < mf_len) {
        let p -> VarDeclareNode = my_fields[mf_idx];
        let f_name -> String = p.name_tok.value;
        let f_type_id -> Int = resolve_type(c, p.type_node);
        let f_llvm_type -> String = get_llvm_type_str(c, f_type_id);
        
        if (current_offset > 0) { llvm_body += ", "; }
        llvm_body += f_llvm_type;
        fields_vec.append(FieldInfo(name=f_name, type=f_type_id, llvm_type=f_llvm_type, offset=current_offset));
        current_offset += 1;
        mf_idx += 1;
    }
    info.fields = fields_vec;

    let def_str -> String = info.llvm_name + " = type { " + llvm_body + " }\n";
    c.output_file.write(def_str);

    let my_methods -> Vector(Struct) = node.methods;
    let mm_len -> Int = 0; if (my_methods is !null) { mm_len = my_methods.length(); }
    let mm_idx -> Int = 0;
    while (mm_idx < mm_len) {
        let m_node -> MethodDefNode = my_methods[mm_idx];
        let raw_m_name -> String = m_node.name_tok.value;
        
        if (raw_m_name != "$init") {
            let m_name -> String = c.current_package_prefix + class_name + "_" + raw_m_name;
            let f_info -> FuncInfo = c.func_table.get(m_name);
            
            let vt_len -> Int = vtable_vec.length();
            let vt_i -> Int = 0;
            let is_override -> Bool = false;
            while (vt_i < vt_len) {
                let p_func -> FuncInfo = vtable_vec[vt_i];
                if (p_func.base_name == raw_m_name) {
                    vtable_vec[vt_i] = f_info;
                    is_override = true;
                    break;
                }
                vt_i += 1;
            }
            if (!is_override) {
                vtable_vec.append(f_info);
            }
        }
        mm_idx += 1;
    }
    info.vtable = vtable_vec;

    let vt_final_len -> Int = vtable_vec.length();
    c.output_file.write("%vtable_type." + class_name + " = type [ " + vt_final_len + " x i8* ]\n");
    let vt_str -> String = info.vtable_name + " = global %vtable_type." + class_name;
    if (vt_final_len == 0) {
        vt_str += " zeroinitializer\n\n";
    } else {
        vt_str += " [ ";
        let vt_i -> Int = 0;
        while (vt_i < vt_final_len) {
            let f_info -> FuncInfo = vtable_vec[vt_i];
            let sig -> String = get_func_sig_str(c, f_info);
            if (vt_i > 0) { vt_str += ", "; }
            vt_str += "i8* bitcast (" + sig + " @" + f_info.name + " to i8*)";
            vt_i += 1;
        }
        vt_str += " ]\n\n";
    }
    c.output_file.write(vt_str);

    mm_idx = 0;
    while (mm_idx < mm_len) {
        let m_node -> MethodDefNode = my_methods[mm_idx];
        compile_method_def(c, class_name, m_node);
        mm_idx += 1;
    }
    
    return void_result();
}

func compile_field_access(c -> Compiler, node -> FieldAccessNode) -> CompileResult {
    let obj_res -> CompileResult = compile_node(c, node.obj);

    if (is_pointer_type(c, obj_res.type)) {
        let base_info -> SymbolInfo = c.ptr_base_map.get("" + obj_res.type);
        if (base_info is !null) {
            let s_check -> StructInfo = c.struct_id_map.get("" + base_info.type);
            if (s_check is !null) {
                let ptr_is_null -> String = next_reg(c);
                let ptr_ty_str -> String = get_llvm_type_str(c, obj_res.type);
                c.output_file.write(c.indent + ptr_is_null + " = icmp eq " + ptr_ty_str + " " + obj_res.reg + ", null\n");
                
                let label_ok -> String = "ptr_ok_" + c.reg_count;
                let label_fail -> String = "ptr_fail_" + c.reg_count;
                c.reg_count += 1;
                
                c.output_file.write(c.indent + "br i1 " + ptr_is_null + ", label %" + label_fail + ", label %" + label_ok + "\n");
                c.output_file.write("\n" + label_fail + ":\n");
                emit_runtime_error(c, node.pos, "Null pointer dereference");
                
                c.output_file.write("\n" + label_ok + ":\n");

                let loaded_reg -> String = next_reg(c);
                let base_ty_str -> String = get_llvm_type_str(c, base_info.type);
                c.output_file.write(c.indent + loaded_reg + " = load " + base_ty_str + ", " + base_ty_str + "* " + obj_res.reg + "\n");

                obj_res.reg = loaded_reg;
                obj_res.type = base_info.type;
            }
        }
    }

    let type_id -> Int = obj_res.type;
    let obj_reg -> String = obj_res.reg;


    let is_null -> String = next_reg(c);
    let obj_llvm_ty -> String = get_llvm_type_str(c, obj_res.type);
    c.output_file.write(c.indent + is_null + " = icmp eq " + obj_llvm_ty + " " + obj_reg + ", null\n");
    let label_ok -> String = "access_ok_" + c.reg_count;
    let label_fail -> String = "access_fail_" + c.reg_count;
    c.reg_count += 1;
    c.output_file.write(c.indent + "br i1 " + is_null + ", label %" + label_fail + ", label %" + label_ok + "\n");
    
    c.output_file.write("\n" + label_fail + ":\n");
    emit_runtime_error(c, node.pos, "Null pointer dereference");
    
    c.output_file.write("\n" + label_ok + ":\n");


    if (type_id == TYPE_GENERIC_STRUCT || type_id == TYPE_GENERIC_CLASS) {
        let base_obj -> BaseNode = node.obj;
        if (base_obj.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = node.obj;
            let info -> SymbolInfo = find_symbol(c, v_node.name_tok.value);
            if (info is !null && info.origin_type >= 100) {
                type_id = info.origin_type;
                
                // i8* -> %struct.Test*
                let s_info_temp -> StructInfo = c.struct_id_map.get("" + type_id);
                if (s_info_temp is !null) {
                    let cast_reg -> String = next_reg(c);
                    c.output_file.write(c.indent + cast_reg + " = bitcast i8* " + obj_reg + " to " + s_info_temp.llvm_name + "*\n");
                    obj_reg = cast_reg;
                }
            }
        }
    }

    let s_info -> StructInfo = c.struct_id_map.get("" + type_id);
    if (s_info is null) {
        WhitelangExceptions.throw_type_error(node.pos, "Cannot access field on non-struct type (or generic Struct/Class without origin inference).");
    }
    
    let field -> FieldInfo = find_field(s_info, node.field_name);
    
    if (field is !null) {
        let f_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_reg + ", i32 0, i32 " + field.offset + "\n");

        let arr_check -> ArrayInfo = c.array_info_map.get("" + field.type);
        if (arr_check is !null) {
            if (arr_check.size != -1) {
                return CompileResult(reg=f_ptr, type=field.type);
            }
        }

        let val_reg -> String = next_reg(c);
        c.output_file.write(c.indent + val_reg + " = load " + field.llvm_type + ", " + field.llvm_type + "* " + f_ptr + "\n");
        return CompileResult(reg=val_reg, type=field.type);
    }

    if (s_info.is_class) {
        let vtable_vec -> Vector(Struct) = s_info.vtable;
        let v_len -> Int = 0; if (vtable_vec is !null) { v_len = vtable_vec.length(); }
        let m_idx -> Int = 0;
        let target_func -> FuncInfo = null;
        
        while (m_idx < v_len) {
            let m -> FuncInfo = vtable_vec[m_idx];
            if (m.base_name == node.field_name) {
                target_func = m;
                break;
            }
            m_idx += 1;
        }
        
        if (target_func is !null) {
            let class_llvm_ty -> String = s_info.llvm_name;
            let vptr_addr -> String = next_reg(c);
            c.output_file.write(c.indent + vptr_addr + " = getelementptr inbounds " + class_llvm_ty + ", " + class_llvm_ty + "* " + obj_reg + ", i32 0, i32 0\n");
            let vtable_i8ptr -> String = next_reg(c);
            c.output_file.write(c.indent + vtable_i8ptr + " = load i8*, i8** " + vptr_addr + "\n");
            let vtable_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + vtable_ptr + " = bitcast i8* " + vtable_i8ptr + " to %vtable_type." + s_info.name + "*\n");
            let method_i8ptr_addr -> String = next_reg(c);
            c.output_file.write(c.indent + method_i8ptr_addr + " = getelementptr inbounds %vtable_type." + s_info.name + ", %vtable_type." + s_info.name + "* " + vtable_ptr + ", i32 0, i32 " + m_idx + "\n");
            let method_i8ptr -> String = next_reg(c);
            c.output_file.write(c.indent + method_i8ptr + " = load i8*, i8** " + method_i8ptr_addr + "\n");

            let specific_type_id -> Int = get_func_type_id(c, target_func.ret_type);
            // 8 is TYPE_GENERIC_FUNCTION
            let clo_payload -> String = emit_alloc_obj(c, "16", "8", "i8*");
            
            let clo_func_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + clo_func_ptr + " = bitcast i8* " + clo_payload + " to i8**\n");
            c.output_file.write(c.indent + "store i8* " + method_i8ptr + ", i8** " + clo_func_ptr + "\n");
            
            let clo_env_ptr_i8 -> String = next_reg(c);
            c.output_file.write(c.indent + clo_env_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_payload + ", i32 8\n");
            let clo_env_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + clo_env_ptr + " = bitcast i8* " + clo_env_ptr_i8 + " to i8**\n");
            let obj_i8_ptr -> String = next_reg(c);
            c.output_file.write(c.indent + obj_i8_ptr + " = bitcast " + class_llvm_ty + "* " + obj_reg + " to i8*\n");
            c.output_file.write(c.indent + "store i8* " + obj_i8_ptr + ", i8** " + clo_env_ptr + "\n");
            
            emit_retain(c, obj_reg, type_id);
            
            return CompileResult(reg=clo_payload, type=specific_type_id, origin_type=target_func.ret_type);
        }
    }

    WhitelangExceptions.throw_name_error(node.pos, "Field '" + node.field_name + "' not found in struct '" + s_info.name + "'.");
    return CompileResult(reg="null", type=4, origin_type=0); // 4 is TYPE_VOID
}

func compile_field_assign(c -> Compiler, node -> FieldAssignNode) -> CompileResult {
    let obj_base -> BaseNode = node.obj;
    if (obj_base.type == NODE_CALL || obj_base.type == NODE_VECTOR_LIT || obj_base.type == NODE_STRING) {
        WhitelangExceptions.throw_invalid_syntax(node.pos, "Cannot assign to a field of a temporary right-value object. Assign it to a variable first.");
    }
    
    let obj_res -> CompileResult = compile_node(c, node.obj);
    let struct_type_id -> Int = obj_res.type;
    let struct_ptr_reg -> String = obj_res.reg;

    if ((struct_type_id == TYPE_GENERIC_STRUCT || struct_type_id == TYPE_GENERIC_CLASS) && obj_res.origin_type >= 100) {
        struct_type_id = obj_res.origin_type;
        let s_info_temp -> StructInfo = c.struct_id_map.get("" + struct_type_id);
        if (s_info_temp is !null) {
            let cast_reg -> String = next_reg(c);
            c.output_file.write(c.indent + cast_reg + " = bitcast i8* " + struct_ptr_reg + " to " + s_info_temp.llvm_name + "*\n");
            struct_ptr_reg = cast_reg;
        }
    }

    let s_info -> StructInfo = c.struct_id_map.get("" + struct_type_id);
    if (s_info is null) {
        WhitelangExceptions.throw_type_error(node.pos, "Cannot assign field to non-struct type.");
    }

    let field -> FieldInfo = find_field(s_info, node.field_name);
    if (field is null) {
        WhitelangExceptions.throw_name_error(node.pos, "Field '" + node.field_name + "' not found in struct '" + s_info.name + "'.");
    }

    c.expected_type = field.type;
    let val_res -> CompileResult = compile_node(c, node.value);
    c.expected_type = 0;

    val_res = emit_implicit_cast(c, val_res, field.type, node.pos);
    
    let f_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + struct_ptr_reg + ", i32 0, i32 " + field.offset + "\n");

    if (is_ref_type(c, field.type)) {
        emit_retain(c, val_res.reg, field.type);

        let old_val_reg -> String = next_reg(c);
        let field_ty_str -> String = get_llvm_type_str(c, field.type);
        c.output_file.write(c.indent + old_val_reg + " = load " + field_ty_str + ", " + field_ty_str + "* " + f_ptr + "\n");

        emit_release(c, old_val_reg, field.type);
    }

    let store_ty -> String = get_llvm_type_str(c, field.type);
    c.output_file.write(c.indent + "store " + store_ty + " " + val_res.reg + ", " + store_ty + "* " + f_ptr + "\n");
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

    c.func_table.put(func_name, FuncInfo(name=func_name, base_name=func_name, ret_type=ret_type_id, arg_types=arg_types, is_varargs=node.is_varargs));
    if (c.declared_externs.get(func_name) is null) {
        let ret_llvm -> String = get_llvm_type_str(c, ret_type_id);
        c.output_file.write("declare " + ret_llvm + " @" + func_name + "(" + params_str + ")\n");

        c.declared_externs.put(func_name, StringConstant(id=0, value="")); 
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

func compile_array_literal(c -> Compiler, lit_node -> VectorLitNode, target_arr_id -> Int, ptr_reg -> String) -> Void {
    let target_arr -> ArrayInfo = c.array_info_map.get("" + target_arr_id);
    if (target_arr is null) { return; }
    
    if (lit_node.count > target_arr.size) {
        WhitelangExceptions.throw_type_error(lit_node.pos, "Array literal too large: expected " + target_arr.size + " elements, got " + lit_node.count);
    }
    
    let lit_i -> Int = 0;
    let elem_ty_str -> String = get_llvm_type_str(c, target_arr.base_type);
    
    while (lit_i < lit_node.count) {
        let elem_node -> ArgNode = lit_node.elements[lit_i];
        let elem_base -> BaseNode = elem_node.val;
        
        let elem_ptr_reg -> String = next_reg(c);
        c.output_file.write(c.indent + elem_ptr_reg + " = getelementptr inbounds " + target_arr.llvm_name + ", " + target_arr.llvm_name + "* " + ptr_reg + ", i32 0, i32 " + lit_i + "\n");
        
        let is_nested -> Bool = false;
        if (elem_base.type == NODE_VECTOR_LIT) {
            let inner_arr_info -> ArrayInfo = c.array_info_map.get("" + target_arr.base_type);
            if (inner_arr_info is !null) {
                is_nested = true;
                let inner_lit -> VectorLitNode = elem_node.val;
                compile_array_literal(c, inner_lit, target_arr.base_type, elem_ptr_reg);
            }
        }
        
        if (!is_nested) {
            c.expected_type = target_arr.base_type;
            let elem_res -> CompileResult = compile_node(c, elem_node.val);
            c.expected_type = 0;
            
            let casted_res -> CompileResult = emit_implicit_cast(c, elem_res, target_arr.base_type, lit_node.pos);
            c.output_file.write(c.indent + "store " + elem_ty_str + " " + casted_res.reg + ", " + elem_ty_str + "* " + elem_ptr_reg + "\n");
            
            if (c.scope_depth > 0 && is_ref_type(c, target_arr.base_type)) {
                emit_retain(c, casted_res.reg, target_arr.base_type);
            }
        }
        
        lit_i += 1;
    }
}

func compile_vector_append(c -> Compiler, vec_node -> Struct, call_node -> CallNode) -> CompileResult {
    let args -> Vector(Struct) = call_node.args;
    let a_len -> Int = 0;
    if (args is !null) { a_len = args.length(); }
    if (a_len != 1) { WhitelangExceptions.throw_type_error(call_node.pos, "append expects exactly 1 argument."); }
    
    let arg_node -> ArgNode = args[0];
    let vec_res -> CompileResult = compile_node(c, vec_node);

    let v_info -> SymbolInfo = c.vector_base_map.get("" + vec_res.type);
    if (v_info is null) { WhitelangExceptions.throw_type_error(call_node.pos, "append is only for Vectors."); }
    
    let elem_type -> Int = v_info.type;

    c.expected_type = elem_type;
    let arg_res -> CompileResult = compile_node(c, arg_node.val);
    c.expected_type = 0;

    arg_res = emit_implicit_cast(c, arg_res, elem_type, call_node.pos);
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
    let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

    let size_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    c.output_file.write(c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");
    
    let cap_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + cap_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 1\n");
    let cap_val -> String = next_reg(c);
    c.output_file.write(c.indent + cap_val + " = load i64, i64* " + cap_ptr + "\n");

    let cmp_reg -> String = next_reg(c);
    c.output_file.write(c.indent + cmp_reg + " = icmp uge i64 " + size_val + ", " + cap_val + "\n");
    
    let grow_label -> String = "vec_grow_" + c.type_counter;
    let push_label -> String = "vec_push_" + c.type_counter;
    c.type_counter += 1;
    
    c.output_file.write(c.indent + "br i1 " + cmp_reg + ", label %" + grow_label + ", label %" + push_label + "\n");

    c.output_file.write("\n" + grow_label + ":\n");
    
    // new_cap = (cap == 0) ? 4 : cap * 2
    let is_zero_cap -> String = next_reg(c);
    c.output_file.write(c.indent + is_zero_cap + " = icmp eq i64 " + cap_val + ", 0\n");
    let dbl_cap -> String = next_reg(c);
    c.output_file.write(c.indent + dbl_cap + " = mul i64 " + cap_val + ", 2\n");
    let new_cap -> String = next_reg(c);
    c.output_file.write(c.indent + new_cap + " = select i1 " + is_zero_cap + ", i64 4, i64 " + dbl_cap + "\n");
    
    c.output_file.write(c.indent + "store i64 " + new_cap + ", i64* " + cap_ptr + "\n");
    
    // realloc
    let data_field_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 2\n");
    let old_data -> String = next_reg(c);
    c.output_file.write(c.indent + old_data + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    let old_data_i8 -> String = next_reg(c);
    c.output_file.write(c.indent + old_data_i8 + " = bitcast " + elem_ty_str + "* " + old_data + " to i8*\n");

    let elem_size -> Int = 8; 
    if (elem_type == TYPE_INT) { elem_size = 4; } 
    if (elem_type == TYPE_BYTE || elem_type == TYPE_BOOL) { elem_size = 1; }
    
    let new_bytes -> String = next_reg(c);
    c.output_file.write(c.indent + new_bytes + " = mul i64 " + new_cap + ", " + elem_size + "\n");
    
    let new_data_i8 -> String = next_reg(c);
    c.output_file.write(c.indent + new_data_i8 + " = call i8* @realloc(i8* " + old_data_i8 + ", i64 " + new_bytes + ")\n");
    
    let new_data_typed -> String = next_reg(c);
    c.output_file.write(c.indent + new_data_typed + " = bitcast i8* " + new_data_i8 + " to " + elem_ty_str + "*\n");
    c.output_file.write(c.indent + "store " + elem_ty_str + "* " + new_data_typed + ", " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    c.output_file.write(c.indent + "br label %" + push_label + "\n");
    c.output_file.write("\n" + push_label + ":\n");
    
    let final_data_field_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + final_data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 2\n");
    let final_data -> String = next_reg(c);
    c.output_file.write(c.indent + final_data + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + final_data_field_ptr + "\n");
    
    let slot_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + final_data + ", i64 " + size_val + "\n");
    
    if (is_ref_type(c, elem_type)) { emit_retain(c, arg_res.reg, elem_type); }
    c.output_file.write(c.indent + "store " + elem_ty_str + " " + arg_res.reg + ", " + elem_ty_str + "* " + slot_ptr + "\n");
    
    let new_size -> String = next_reg(c);
    c.output_file.write(c.indent + new_size + " = add i64 " + size_val + ", 1\n");
    c.output_file.write(c.indent + "store i64 " + new_size + ", i64* " + size_ptr + "\n");
    
    return void_result();
}
func compile_vector_drop(c -> Compiler, vec_node -> Struct, call_node -> CallNode) -> CompileResult {
    let args -> Vector(Struct) = call_node.args;
    let a_len -> Int = 0;
    if (args is !null) { a_len = args.length(); }
    if (a_len > 0) { WhitelangExceptions.throw_type_error(call_node.pos, "drop expects 0 arguments."); }
    
    let vec_res -> CompileResult = compile_node(c, vec_node);
    let v_info -> SymbolInfo = c.vector_base_map.get("" + vec_res.type);
    let elem_type -> Int = v_info.type;
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
    let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

    let size_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    c.output_file.write(c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");

    let cmp_reg -> String = next_reg(c);
    c.output_file.write(c.indent + cmp_reg + " = icmp ugt i64 " + size_val + ", 0\n");
    
    let pop_label -> String = "vec_pop_" + c.type_counter;
    let empty_label -> String = "vec_empty_" + c.type_counter;
    let end_label -> String = "vec_pop_end_" + c.type_counter;
    c.type_counter += 1;
    
    c.output_file.write(c.indent + "br i1 " + cmp_reg + ", label %" + pop_label + ", label %" + empty_label + "\n");

    c.output_file.write("\n" + empty_label + ":\n");

    emit_runtime_error(c, call_node.pos, "drop from empty vector");

    c.output_file.write("\n" + pop_label + ":\n");
    
    // size--
    let new_size -> String = next_reg(c);
    c.output_file.write(c.indent + new_size + " = sub i64 " + size_val + ", 1\n");
    c.output_file.write(c.indent + "store i64 " + new_size + ", i64* " + size_ptr + "\n");

    let data_field_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_res.reg + ", i32 0, i32 2\n");
    let data_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    let slot_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + new_size + "\n");
    
    let ret_val -> String = next_reg(c);
    c.output_file.write(c.indent + ret_val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");
    
    if (is_ref_type(c, elem_type)) {
        emit_release(c, ret_val, elem_type);
    }
    c.output_file.write(c.indent + "br label %" + end_label + "\n");
    
    c.output_file.write("\n" + end_label + ":\n");
    return CompileResult(reg=ret_val, type=elem_type);
}
func compile_vector_lit(c -> Compiler, node -> VectorLitNode) -> CompileResult {
    let count -> Int = node.count;
    let elem_type_id -> Int = TYPE_INT; 
    let elements -> Vector(Struct) = node.elements;
    let e_len -> Int = 0;
    if (elements is !null) { e_len = elements.length(); }
    
    let has_expected -> Bool = false;
    if (c.expected_type > 0) {
        let v_info -> SymbolInfo = c.vector_base_map.get("" + c.expected_type);
        if (v_info is !null) {
            elem_type_id = v_info.type;
            has_expected = true;
        } else {
            elem_type_id = c.expected_type;
            has_expected = true;
        }
    }
    
    if (!has_expected && e_len > 0) {
        let first_arg -> ArgNode = elements[0];
        let old_exp -> Int = c.expected_type;
        c.expected_type = 0;
        let first_res -> CompileResult = compile_node(c, first_arg.val);
        c.expected_type = old_exp;
        elem_type_id = first_res.type;
    }
    
    let vec_type_id -> Int = get_vector_type_id(c, elem_type_id);
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type_id);
    let struct_name -> String = "{ i64, i64, " + elem_ty_str + "* }";

    let struct_size_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + struct_size_ptr + " = getelementptr " + struct_name + ", " + struct_name + "* null, i32 1\n");
    let struct_size -> String = next_reg(c);
    c.output_file.write(c.indent + struct_size + " = ptrtoint " + struct_name + "* " + struct_size_ptr + " to i64\n");
    let vec_ptr -> String = emit_alloc_obj(c, struct_size, "" + vec_type_id, struct_name + "*");
    
    // malloc data array
    let arr_size_ptr -> String = next_reg(c);
    // size = count * sizeof(T)
    c.output_file.write(c.indent + arr_size_ptr + " = getelementptr " + elem_ty_str + ", " + elem_ty_str + "* null, i64 " + count + "\n");
    let arr_bytes -> String = next_reg(c);
    c.output_file.write(c.indent + arr_bytes + " = ptrtoint " + elem_ty_str + "* " + arr_size_ptr + " to i64\n");
    
    let raw_data -> String = next_reg(c);
    // TODO: Handle count == 0 case carefully
    c.output_file.write(c.indent + raw_data + " = call i8* @malloc(i64 " + arr_bytes + ")\n");
    let data_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + data_ptr + " = bitcast i8* " + raw_data + " to " + elem_ty_str + "*\n");

    // struct { i64 size, i64 capacity, T* data }
    // size
    let size_ptr -> String = next_reg(c); 
    c.output_file.write(c.indent + size_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 0\n");
    c.output_file.write(c.indent + "store i64 " + count + ", i64* " + size_ptr + "\n");
    
    // capacity
    let cap_ptr -> String = next_reg(c); 
    c.output_file.write(c.indent + cap_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 1\n");
    c.output_file.write(c.indent + "store i64 " + count + ", i64* " + cap_ptr + "\n"); 
    
    // data pointer
    let data_field_ptr -> String = next_reg(c); 
    c.output_file.write(c.indent + data_field_ptr + " = getelementptr inbounds " + struct_name + ", " + struct_name + "* " + vec_ptr + ", i32 0, i32 2\n");
    c.output_file.write(c.indent + "store " + elem_ty_str + "* " + data_ptr + ", " + elem_ty_str + "** " + data_field_ptr + "\n");
    
    let idx -> Int = 0;
    while (idx < e_len) {
        let curr -> ArgNode = elements[idx];

        let old_exp -> Int = c.expected_type;
        c.expected_type = elem_type_id;
        let val_res -> CompileResult = compile_node(c, curr.val);
        c.expected_type = old_exp;

        val_res = emit_implicit_cast(c, val_res, elem_type_id, node.pos);
        
        let slot_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx + "\n");

        if (is_ref_type(c, elem_type_id)) {
            emit_retain(c, val_res.reg, elem_type_id);
        }
        
        c.output_file.write(c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + slot_ptr + "\n");

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
        c.output_file.write(c.indent + len_reg + " = call i32 @strlen(i8* " + obj_res.reg + ")\n");
        return CompileResult(reg=len_reg, type=TYPE_INT);
    }

    // Vector.length()
    let is_vec -> Bool = false;
    if (type_id >= 100) {
        let v_info -> SymbolInfo = c.vector_base_map.get("" + type_id);
        if (v_info is !null) { is_vec = true; }
    }

    let arr_info -> ArrayInfo = c.array_info_map.get("" + type_id);
        if (arr_info is !null) {
            if (arr_info.size == -1) { // Slice
                let size_i64 -> String = next_reg(c);
                c.output_file.write(c.indent + size_i64 + " = extractvalue " + arr_info.llvm_name + " " + obj_res.reg + ", 0\n");
                let trunc_reg -> String = next_reg(c);
                c.output_file.write(c.indent + trunc_reg + " = trunc i64 " + size_i64 + " to i32\n");
                return CompileResult(reg=trunc_reg, type=TYPE_INT);
            } else {
                return CompileResult(reg="" + arr_info.size, type=TYPE_INT);
            }
        }

    if is_vec {
        let v_info -> SymbolInfo = c.vector_base_map.get("" + type_id);
        let elem_ty_str -> String = get_llvm_type_str(c, v_info.type);
        let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";
        
        let size_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + obj_res.reg + ", i32 0, i32 0\n");
        
        let size_val -> String = next_reg(c);
        c.output_file.write(c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");

        let trunc_reg -> String = next_reg(c);
        c.output_file.write(c.indent + trunc_reg + " = trunc i64 " + size_val + " to i32\n");
        
        return CompileResult(reg=trunc_reg, type=TYPE_INT);
    }

    WhitelangExceptions.throw_type_error(call_node.pos, "Method 'length' is not defined for type " + get_type_name(c, type_id));
    return void_result();
}

func compile_index_access(c -> Compiler, node -> IndexAccessNode) -> CompileResult {
    check_out_index(c, node.target, node.index_node, node.pos);
    let target_res -> CompileResult = compile_node(c, node.target);

    let s_info -> StructInfo = c.struct_id_map.get("" + target_res.type);
    if (s_info is !null && s_info.is_class) {
        let has_get -> Bool = false;
        let v_len -> Int = 0; if (s_info.vtable is !null) { v_len = s_info.vtable.length(); }
        let m_idx -> Int = 0;
        while (m_idx < v_len) {
            let m -> FuncInfo = s_info.vtable[m_idx];
            if (m.base_name == "get") { has_get = true; break; }
            m_idx += 1;
        }

        if has_get {
            let fake_args -> Vector(Struct) = [];
            fake_args.append(ArgNode(val=node.index_node, name=null));
            let fake_call -> CallNode = CallNode(type=16, callee=null, args=fake_args, pos=node.pos);
            return compile_class_method_call(c, s_info, target_res, "get", fake_call);
        }
    }

    let index_res -> CompileResult = compile_node(c, node.index_node);
    
    // idx type
    if (index_res.type != TYPE_INT) {
        WhitelangExceptions.throw_type_error(node.pos, "Index must be an Integer.");
    }

    if (is_pointer_type(c, target_res.type)) {
        let base_info -> SymbolInfo = c.ptr_base_map.get("" + target_res.type);
        if (base_info is !null) {
            let elem_type -> Int = base_info.type;
            
            if (elem_type == TYPE_VOID) {
                WhitelangExceptions.throw_type_error(node.pos, "Cannot index 'ptr Void'. Cast it to a specific pointer type first.");
            }
            
            let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
            
            let addr_reg -> String = next_reg(c);
            c.output_file.write(c.indent + addr_reg + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + target_res.reg + ", i32 " + index_res.reg + "\n");
            
            let load_reg -> String = next_reg(c);
            c.output_file.write(c.indent + load_reg + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + addr_reg + "\n");
            
            return CompileResult(reg=load_reg, type=elem_type, origin_type=elem_type);
        }
    }

    // Array / Slice access
    let arr_info -> ArrayInfo = c.array_info_map.get("" + target_res.type);
    if (arr_info is !null) {
        let elem_type -> Int = arr_info.base_type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
        
        let idx_i32 -> String = index_res.reg;

        let curr_len -> String = "";
        let data_ptr -> String = "";
        if (arr_info.size == -1) {
            let len_i64 -> String = next_reg(c);
            c.output_file.write(c.indent + len_i64 + " = extractvalue " + arr_info.llvm_name + " " + target_res.reg + ", 0\n");
            curr_len = next_reg(c);
            c.output_file.write(c.indent + curr_len + " = trunc i64 " + len_i64 + " to i32\n");
            
            data_ptr = next_reg(c);
            c.output_file.write(c.indent + data_ptr + " = extractvalue " + arr_info.llvm_name + " " + target_res.reg + ", 1\n");
        } else {
            curr_len = "" + arr_info.size;
            data_ptr = next_reg(c);
            c.output_file.write(c.indent + data_ptr + " = getelementptr inbounds " + arr_info.llvm_name + ", " + arr_info.llvm_name + "* " + target_res.reg + ", i32 0, i32 0\n");
        }

        emit_array_bounds_check(c, idx_i32, curr_len, node.pos);

        let ptr_reg -> String = next_reg(c);
        c.output_file.write(c.indent + ptr_reg + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i32 " + idx_i32 + "\n");

        if (c.array_info_map.get("" + elem_type) is !null) {
            return CompileResult(reg=ptr_reg, type=elem_type, origin_type=elem_type);
        }
        
        let val_reg -> String = next_reg(c);
        c.output_file.write(c.indent + val_reg + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + ptr_reg + "\n");
        return CompileResult(reg=val_reg, type=elem_type, origin_type=elem_type);
    }
    
    // Vector access
    let is_vec -> Bool = false;
    if (target_res.type >= 100) {
        if (c.vector_base_map.get("" + target_res.type) is !null) { is_vec = true; }
    }
    
    if is_vec {
        let v_info -> SymbolInfo = c.vector_base_map.get("" + target_res.type);
        let elem_type -> Int = v_info.type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
        
        // { i64, i64, T* }
        let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

        emit_vector_bounds_check(c, target_res.reg, index_res.reg, struct_ty, node.pos);

        let data_field_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + target_res.reg + ", i32 0, i32 2\n");
        
        let data_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");

        let slot_ptr -> String = next_reg(c);
        
        let idx_i64 -> String = next_reg(c);
        c.output_file.write(c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        
        c.output_file.write(c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx_i64 + "\n");
        
        let val_reg -> String = next_reg(c);
        c.output_file.write(c.indent + val_reg + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");
        
        return CompileResult(reg=val_reg, type=elem_type);
    }
    
    // str access(-> Byte)
    if (target_res.type == TYPE_STRING) {
        let idx_i64 -> String = next_reg(c);
        c.output_file.write(c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        
        let char_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + char_ptr + " = getelementptr inbounds i8, i8* " + target_res.reg + ", i64 " + idx_i64 + "\n");
        
        let char_val -> String = next_reg(c);
        c.output_file.write(c.indent + char_val + " = load i8, i8* " + char_ptr + "\n");
        
        return CompileResult(reg=char_val, type=TYPE_BYTE);
    }
    
    WhitelangExceptions.throw_type_error(node.pos, "Type " + get_type_name(c, target_res.type) + " is not indexable.");
    return void_result();
}

func compile_index_assign(c -> Compiler, node -> IndexAssignNode) -> CompileResult {
    check_out_index(c, node.target, node.index_node, node.pos);
    let target_res -> CompileResult = compile_node(c, node.target);

    let s_info -> StructInfo = c.struct_id_map.get("" + target_res.type);
    if (s_info is !null && s_info.is_class) {
        let has_put -> Bool = false;
        let v_len -> Int = 0; if (s_info.vtable is !null) { v_len = s_info.vtable.length(); }
        let m_idx -> Int = 0;
        while (m_idx < v_len) {
            let m -> FuncInfo = s_info.vtable[m_idx];
            if (m.base_name == "put") { has_put = true; break; }
            m_idx += 1;
        }

        if has_put {
            let fake_args -> Vector(Struct) = [];
            fake_args.append(ArgNode(val=node.index_node, name=null));
            fake_args.append(ArgNode(val=node.value, name=null));
            let fake_call -> CallNode = CallNode(type=16, callee=null, args=fake_args, pos=node.pos);
            compile_class_method_call(c, s_info, target_res, "put", fake_call);
            return void_result();
        }
    }

    let index_res -> CompileResult = compile_node(c, node.index_node);
    
    if (index_res.type != TYPE_INT) {
        WhitelangExceptions.throw_type_error(node.pos, "Index must be an Integer.");
    }

    if (is_pointer_type(c, target_res.type)) {
        let base_info -> SymbolInfo = c.ptr_base_map.get("" + target_res.type);
        if (base_info is !null) {
            let elem_type -> Int = base_info.type;
            
            if (elem_type == TYPE_VOID) {
                WhitelangExceptions.throw_type_error(node.pos, "Cannot index 'ptr Void'. Cast it to a specific pointer type first.");
            }

            c.expected_type = elem_type;
            let val_res -> CompileResult = compile_node(c, node.value);
            c.expected_type = 0;
            
            val_res = emit_implicit_cast(c, val_res, elem_type, node.pos);
            
            let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
            let addr_reg -> String = next_reg(c);
            c.output_file.write(c.indent + addr_reg + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + target_res.reg + ", i32 " + index_res.reg + "\n");
            
            if (is_ref_type(c, elem_type)) {
                emit_retain(c, val_res.reg, elem_type);
                let old_val -> String = next_reg(c);
                c.output_file.write(c.indent + old_val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + addr_reg + "\n");
                emit_release(c, old_val, elem_type);
            }
            
            c.output_file.write(c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + addr_reg + "\n");
            return val_res;
        }
    }

    // Array / Slice assign
    let arr_info -> ArrayInfo = c.array_info_map.get("" + target_res.type);
    if (arr_info is !null) {
        let elem_type -> Int = arr_info.base_type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);

        c.expected_type = elem_type;
        let val_res -> CompileResult = compile_node(c, node.value);
        c.expected_type = 0;
        
        val_res = emit_implicit_cast(c, val_res, elem_type, node.pos);

        let curr_len -> String = "";
        let data_ptr -> String = "";
        if (arr_info.size == -1) {
            let len_i64 -> String = next_reg(c);
            c.output_file.write(c.indent + len_i64 + " = extractvalue " + arr_info.llvm_name + " " + target_res.reg + ", 0\n");
            curr_len = next_reg(c);
            c.output_file.write(c.indent + curr_len + " = trunc i64 " + len_i64 + " to i32\n");
            
            data_ptr = next_reg(c);
            c.output_file.write(c.indent + data_ptr + " = extractvalue " + arr_info.llvm_name + " " + target_res.reg + ", 1\n");
        } else {
            curr_len = "" + arr_info.size;
            data_ptr = next_reg(c);
            c.output_file.write(c.indent + data_ptr + " = getelementptr inbounds " + arr_info.llvm_name + ", " + arr_info.llvm_name + "* " + target_res.reg + ", i32 0, i32 0\n");
        }

        emit_array_bounds_check(c, index_res.reg, curr_len, node.pos);

        let ptr_reg -> String = next_reg(c);
        c.output_file.write(c.indent + ptr_reg + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i32 " + index_res.reg + "\n");
        
        if (is_ref_type(c, elem_type)) {
            emit_retain(c, val_res.reg, elem_type);
            let old_val -> String = next_reg(c);
            c.output_file.write(c.indent + old_val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + ptr_reg + "\n");
            emit_release(c, old_val, elem_type);
        }
        
        c.output_file.write(c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + ptr_reg + "\n");
        return val_res;
    }
    
    let is_vec -> Bool = false;
    if (target_res.type >= 100) {
        if (c.vector_base_map.get("" + target_res.type) is !null) { is_vec = true; }
    }

    if (target_res.type == TYPE_STRING) {
        let is_magic_func -> Bool = false;
        if (c.curr_func is !null) {
            if (c.curr_func.name == "builtin.string_slice") {
                is_magic_func = true;
            }
        }

        if is_magic_func {
            let val_res -> CompileResult = compile_node(c, node.value);
            let idx_i64 -> String = next_reg(c);
            c.output_file.write(c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");

            let ptr_reg -> String = next_reg(c);
            c.output_file.write(c.indent + ptr_reg + " = getelementptr inbounds i8, i8* " + target_res.reg + ", i64 " + idx_i64 + "\n");
            val_res = emit_implicit_cast(c, val_res, TYPE_BYTE, node.pos);

            c.output_file.write(c.indent + "store i8 " + val_res.reg + ", i8* " + ptr_reg + "\n");
            return val_res;
        }

        WhitelangExceptions.throw_type_error(node.pos, "Strings are immutable. Cannot assign to index.");
    }

    if is_vec {
        let v_info -> SymbolInfo = c.vector_base_map.get("" + target_res.type);
        let elem_type -> Int = v_info.type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);

        c.expected_type = elem_type;
        let val_res -> CompileResult = compile_node(c, node.value);
        c.expected_type = 0;
        
        val_res = emit_implicit_cast(c, val_res, elem_type, node.pos);
        let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

        emit_vector_bounds_check(c, target_res.reg, index_res.reg, struct_ty, node.pos);

        let data_field_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + target_res.reg + ", i32 0, i32 2\n");
        let data_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");
        
        let idx_i64 -> String = next_reg(c);
        c.output_file.write(c.indent + idx_i64 + " = sext i32 " + index_res.reg + " to i64\n");
        let slot_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + idx_i64 + "\n");

        if (is_ref_type(c, elem_type)) {
            emit_retain(c, val_res.reg, elem_type);
            let old_val -> String = next_reg(c);
            c.output_file.write(c.indent + old_val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");
            emit_release(c, old_val, elem_type);
        }

        c.output_file.write(c.indent + "store " + elem_ty_str + " " + val_res.reg + ", " + elem_ty_str + "* " + slot_ptr + "\n");
        
        return val_res;
    }
    
    WhitelangExceptions.throw_type_error(node.pos, "Type " + get_type_name(c, target_res.type) + " does not support index assignment.");
    return void_result();
}

func compile_slice_access(c -> Compiler, node -> SliceAccessNode) -> CompileResult {
    let target_res -> CompileResult = compile_node(c, node.target);
    let start_res -> CompileResult = compile_node(c, node.start_idx);
    let end_res -> CompileResult = compile_node(c, node.end_idx);
    
    if (target_res.type == TYPE_STRING) {
        let call_reg -> String = next_reg(c);
        c.output_file.write(c.indent + call_reg + " = call i8* @builtin.string_slice(i8* " + target_res.reg + ", i32 " + start_res.reg + ", i32 " + end_res.reg + ")\n");
        return CompileResult(reg=call_reg, type=TYPE_STRING, origin_type=0);
    }

    let elem_type -> Int = 0;
    let base_ptr -> String = "";
    let curr_len -> String = "";
    
    let arr_info -> ArrayInfo = c.array_info_map.get("" + target_res.type);
    let vec_info -> SymbolInfo = c.vector_base_map.get("" + target_res.type);
    
    if (arr_info is !null) {
        elem_type = arr_info.base_type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
        if (arr_info.size == -1) {
            let len_i64 -> String = next_reg(c);
            c.output_file.write(c.indent + len_i64 + " = extractvalue " + arr_info.llvm_name + " " + target_res.reg + ", 0\n");
            curr_len = next_reg(c);
            c.output_file.write(c.indent + curr_len + " = trunc i64 " + len_i64 + " to i32\n");
            
            base_ptr = next_reg(c);
            c.output_file.write(c.indent + base_ptr + " = extractvalue " + arr_info.llvm_name + " " + target_res.reg + ", 1\n");
        } else {
            curr_len = "" + arr_info.size;
            base_ptr = next_reg(c);
            c.output_file.write(c.indent + base_ptr + " = getelementptr inbounds " + arr_info.llvm_name + ", " + arr_info.llvm_name + "* " + target_res.reg + ", i32 0, i32 0\n");
        }
    } else if (vec_info is !null) {
        elem_type = vec_info.type;
        let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
        let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";
        
        let size_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + target_res.reg + ", i32 0, i32 0\n");
        let len_i64 -> String = next_reg(c);
        c.output_file.write(c.indent + len_i64 + " = load i64, i64* " + size_ptr + "\n");
        curr_len = next_reg(c);
        c.output_file.write(c.indent + curr_len + " = trunc i64 " + len_i64 + " to i32\n");
        
        let data_field_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + data_field_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + target_res.reg + ", i32 0, i32 2\n");
        base_ptr = next_reg(c);
        c.output_file.write(c.indent + base_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_field_ptr + "\n");
    } else {
        WhitelangExceptions.throw_type_error(node.pos, "Cannot slice type '" + get_type_name(c, target_res.type) + "'. Only Array, Vector, and String can be sliced.");
    }

    emit_slice_bounds_check(c, start_res.reg, end_res.reg, curr_len, node.pos);

    let new_len -> String = next_reg(c);
    c.output_file.write(c.indent + new_len + " = sub i32 " + end_res.reg + ", " + start_res.reg + "\n");
    let new_len_i64 -> String = next_reg(c);
    c.output_file.write(c.indent + new_len_i64 + " = sext i32 " + new_len + " to i64\n");

    let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
    let new_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + new_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + base_ptr + ", i32 " + start_res.reg + "\n");

    let cache_key -> String = "slice_" + elem_type;
    let slice_type_id -> Int = 0;
    let cached -> SymbolInfo = c.array_type_cache.get(cache_key);
    
    if (cached is !null) { 
        slice_type_id = cached.type; 
    } else {
        slice_type_id = c.type_counter;
        c.type_counter += 1;
        c.array_type_cache.put(cache_key, SymbolInfo(reg="", type=slice_type_id, origin_type=0, is_const=false));
        let llvm_name -> String = "%slice." + slice_type_id;
        c.array_info_map.put("" + slice_type_id, ArrayInfo(base_type=elem_type, size=-1, llvm_name=llvm_name));
        c.output_file.write(llvm_name + " = type { i64, " + elem_ty_str + "* }\n\n");
    }
    
    let slice_llvm_ty -> String = get_llvm_type_str(c, slice_type_id);
    let v1 -> String = next_reg(c);
    c.output_file.write(c.indent + v1 + " = insertvalue " + slice_llvm_ty + " undef, i64 " + new_len_i64 + ", 0\n");
    let v2 -> String = next_reg(c);
    c.output_file.write(c.indent + v2 + " = insertvalue " + slice_llvm_ty + " " + v1 + ", " + elem_ty_str + "* " + new_ptr + ", 1\n");

    return CompileResult(reg=v2, type=slice_type_id, origin_type=0);
}

func compile_binop(c -> Compiler, node -> BinOpNode) -> CompileResult {
    let left -> CompileResult = compile_node(c, node.left);
    let op_type -> Int = node.op_tok.type; 

    if (op_type == TOK_AND || op_type == TOK_OR) {
        if (left.type != TYPE_BOOL) {
            WhitelangExceptions.throw_type_error(node.pos, "Logic operators '&&' and '||' require Bool operands. ");
        }
        let label_rhs -> String = "logic_rhs_" + c.type_counter;
        let label_merge -> String = "logic_merge_" + c.type_counter;
        let label_left -> String = "logic_left_" + c.type_counter;
        c.type_counter += 1;

        c.output_file.write(c.indent + "br label %" + label_left + "\n");
        c.output_file.write("\n" + label_left + ":\n");

        if (op_type == TOK_AND) {
            c.output_file.write(c.indent + "br i1 " + left.reg + ", label %" + label_rhs + ", label %" + label_merge + "\n");
        } else {
            c.output_file.write(c.indent + "br i1 " + left.reg + ", label %" + label_merge + ", label %" + label_rhs + "\n");
        }

        c.output_file.write("\n" + label_rhs + ":\n");
        let right_res -> CompileResult = compile_node(c, node.right);
        if (right_res.type != TYPE_BOOL) { WhitelangExceptions.throw_type_error(node.pos, "Right operand must be Bool."); }
        
        let label_rhs_end -> String = "logic_rhs_end_" + c.type_counter;
        c.type_counter += 1;
        c.output_file.write(c.indent + "br label %" + label_rhs_end + "\n");
        c.output_file.write("\n" + label_rhs_end + ":\n");
        c.output_file.write(c.indent + "br label %" + label_merge + "\n");

        c.output_file.write("\n" + label_merge + ":\n");
        let final_reg -> String = next_reg(c);
        c.output_file.write(c.indent + final_reg + " = phi i1 [ " + left.reg + ", %" + label_left + " ], [ " + right_res.reg + ", %" + label_rhs_end + " ]\n");
        
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
            c.output_file.write(c.indent + len1 + " = call i32 @strlen(i8* " + left.reg + ")\n");
            
            let len2 -> String = next_reg(c);
            c.output_file.write(c.indent + len2 + " = call i32 @strlen(i8* " + right.reg + ")\n");

            // upgrade to i64
            let len1_64 -> String = next_reg(c);
            c.output_file.write(c.indent + len1_64 + " = zext i32 " + len1 + " to i64\n");
            let len2_64 -> String = next_reg(c);
            c.output_file.write(c.indent + len2_64 + " = zext i32 " + len2 + " to i64\n");
            
            // total length
            let sum_len -> String = next_reg(c);
            c.output_file.write(c.indent + sum_len + " = add i64 " + len1_64 + ", " + len2_64 + "\n");
            
            // for \0
            let total_size -> String = next_reg(c);
            c.output_file.write(c.indent + total_size + " = add i64 " + sum_len + ", 1\n");

            let new_str_ptr -> String = emit_alloc_obj(c, total_size, "" + TYPE_STRING, "i8*");
            
            // strcpy(new_ptr, left) -> null
            let ign1 -> String = next_reg(c);
            c.output_file.write(c.indent + ign1 + " = call i8* @strcpy(i8* " + new_str_ptr + ", i8* " + left.reg + ")\n");
            
            // strcat(new_ptr, right) -> null
            let ign2 -> String = next_reg(c);
            c.output_file.write(c.indent + ign2 + " = call i8* @strcat(i8* " + new_str_ptr + ", i8* " + right.reg + ")\n");
            
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
        c.output_file.write(c.indent + cmp_val + " = call i32 @strcmp(i8* " + left.reg + ", i8* " + right.reg + ")\n");

        let res_reg -> String = next_reg(c);
        let op_code -> String = "icmp eq";

        if (op_type == TOK_NE) { op_code = "icmp ne"; }

        c.output_file.write(c.indent + res_reg + " = " + op_code + " i32 " + cmp_val + ", 0\n");
        
        return CompileResult(reg=res_reg, type=TYPE_BOOL);
    }

    if (op_type == TOK_POW) {
        left = promote_to_float(c, left);
        right = promote_to_float(c, right);
        let res_reg -> String = next_reg(c);
        c.output_file.write(c.indent + res_reg + " = call double @llvm.pow.f64(double " + left.reg + ", double " + right.reg + ")\n");
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
            c.output_file.write(c.indent + res_reg + " = " + op_code + " i1 " + left.reg + ", " + right.reg + "\n");
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
        
        c.output_file.write(c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n");
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
        else if (op_type == TOK_BIT_AND) { op_code = "and"; }
        else if (op_type == TOK_BIT_OR) { op_code = "or"; }
        else if (op_type == TOK_BIT_XOR) { op_code = "xor"; }
        else if (op_type == TOK_LSHIFT) { op_code = "shl"; }
        else if (op_type == TOK_RSHIFT) { op_code = "ashr"; }
    }

    if (op_type == TOK_DIV || op_type == TOK_MOD) {
            if (right.reg == "0" || right.reg == "0.0") {
                WhitelangExceptions.throw_zero_division_error(node.pos, "Cannot divide by zero. ");
            }

            let is_zero_reg -> String = next_reg(c);
            if (target_type == TYPE_FLOAT) {
                c.output_file.write(c.indent + is_zero_reg + " = fcmp oeq double " + right.reg + ", 0.0\n");
            } else {
                c.output_file.write(c.indent + is_zero_reg + " = icmp eq " + type_str + " " + right.reg + ", 0\n");
            }
            
            let err_label -> String = "div_zero_" + c.type_counter;
            let ok_label -> String = "div_ok_" + c.type_counter;
            c.type_counter += 1;
            
            c.output_file.write(c.indent + "br i1 " + is_zero_reg + ", label %" + err_label + ", label %" + ok_label + "\n");
            
            c.output_file.write("\n" + err_label + ":\n");
            emit_runtime_error(c, node.pos, "Division by zero");
            
            c.output_file.write("\n" + ok_label + ":\n");
        }

    c.output_file.write(c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n");
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
        let id -> Int = register_string_constant(c, val);
        let len -> Int = val.length() + 1;
        let res_reg -> String = next_reg(c);

        c.output_file.write(c.indent + res_reg + " = getelementptr inbounds { i32, i32, [" + len + " x i8] }, { i32, i32, [" + len + " x i8] }* @.str." + id + ", i32 0, i32 2, i32 0\n");
        
        return CompileResult(reg=res_reg, type=TYPE_STRING, origin_type=0);
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
    if (base.type == NODE_SLICE_ACCESS) { return compile_slice_access(c, node); }

    // function and closure
    if (base.type == NODE_FUNC_DEF) {
            let func_def -> FunctionDefNode = node;
            if (c.scope_depth == 0) {
                compile_func_def(c, func_def);
                return null;
            } else {
                let clo_res -> CompileResult = compile_local_closure(c, func_def);
                let f_name -> String = func_def.name_tok.value;

                if (f_name != "") {
                    let llvm_ty_str -> String = get_llvm_type_str(c, clo_res.type);
                    let ptr_reg -> String = next_reg(c);
                    
                    c.output_file.write(c.indent + ptr_reg + " = alloca " + llvm_ty_str + "\n");
                    c.output_file.write(c.indent + "store " + llvm_ty_str + " " + clo_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
                    
                    let curr_scope -> Scope = c.symbol_table;
                    curr_scope.table.put(f_name, SymbolInfo(reg=ptr_reg, type=clo_res.type, origin_type=clo_res.origin_type, is_const=true));
                    curr_scope.gc_vars.append(GCTracker(reg=ptr_reg, type=clo_res.type));
                }

                return clo_res;
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
            let f_info -> FuncInfo = c.func_table.get(name);
            if (f_info is !null) {
                let specific_type_id -> Int = get_func_type_id(c, f_info.ret_type);
                let sig -> String = get_func_sig_str(c, f_info);
                let func_ptr -> String = "@" + name;
                
                let cast_reg -> String = next_reg(c);
                c.output_file.write(c.indent + cast_reg + " = bitcast " + sig + " " + func_ptr + " to i8*\n");

                let clo_payload -> String = emit_alloc_obj(c, "16", "8", "i8*");
                
                let clo_func_ptr -> String = next_reg(c);
                c.output_file.write(c.indent + clo_func_ptr + " = bitcast i8* " + clo_payload + " to i8**\n");
                c.output_file.write(c.indent + "store i8* " + cast_reg + ", i8** " + clo_func_ptr + "\n");
                
                let clo_env_ptr_i8 -> String = next_reg(c);
                c.output_file.write(c.indent + clo_env_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_payload + ", i32 8\n");
                let clo_env_ptr -> String = next_reg(c);
                c.output_file.write(c.indent + clo_env_ptr + " = bitcast i8* " + clo_env_ptr_i8 + " to i8**\n");
                c.output_file.write(c.indent + "store i8* null, i8** " + clo_env_ptr + "\n");

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
            let base_info -> SymbolInfo = c.ptr_base_map.get("" + curr_type);
            if (base_info is null) { WhitelangExceptions.throw_type_error(d_node.pos, "Attempt to dereference non-pointer. "); }
            
            let next_type -> Int = base_info.type;
            if (next_type == TYPE_VOID) {
                WhitelangExceptions.throw_type_error(d_node.pos, "Cannot dereference 'ptr Void'. Cast it to a specific pointer type first.");
            }
            let ty_str -> String = get_llvm_type_str(c, next_type);
            let next_reg -> String = next_reg(c);
            
            c.output_file.write(c.indent + next_reg + " = load " + ty_str + ", " + ty_str + "* " + curr_reg + "\n");
            
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
            c.output_file.write(c.indent + cast_l + " = bitcast " + ty_l + " " + l_reg + " to i8*\n");
            l_reg = cast_l;
        }
        if (rhs_res.type != TYPE_STRING && rhs_res.type != TYPE_NULL && rhs_res.type != TYPE_NULLPTR) {
            let cast_r -> String = next_reg(c);
            let ty_r -> String = get_llvm_type_str(c, rhs_res.type);
            c.output_file.write(c.indent + cast_r + " = bitcast " + ty_r + " " + r_reg + " to i8*\n");
            r_reg = cast_r;
        }

        let cmp_reg -> String = next_reg(c);
        let cond -> String = "eq";
        if (base.type == NODE_IS_NOT) { cond = "ne"; }
        
        c.output_file.write(c.indent + cmp_reg + " = icmp " + cond + " i8* " + l_reg + ", " + r_reg + "\n");
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
            let f_info -> FuncInfo = c.func_table.get(var_name);
            if (f_info is !null) {
                let specific_type_id -> Int = get_func_type_id(c, f_info.ret_type);
                let sig -> String = get_func_sig_str(c, f_info);
                let func_ptr -> String = "@" + var_name;
                
                let cast_reg -> String = next_reg(c);
                c.output_file.write(c.indent + cast_reg + " = bitcast " + sig + " " + func_ptr + " to i8*\n");
                let clo_reg -> String = next_reg(c);
                c.output_file.write(c.indent + clo_reg + " = call i8* @malloc(i64 24)\n");
                let clo_rc_ptr -> String = next_reg(c);
                c.output_file.write(c.indent + clo_rc_ptr + " = bitcast i8* " + clo_reg + " to i32*\n");
                c.output_file.write(c.indent + "store i32 0, i32* " + clo_rc_ptr + "\n");
                let clo_type_ptr_i8 -> String = next_reg(c);
                c.output_file.write(c.indent + clo_type_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_reg + ", i32 4\n");
                let clo_type_ptr -> String = next_reg(c);
                c.output_file.write(c.indent + clo_type_ptr + " = bitcast i8* " + clo_type_ptr_i8 + " to i32*\n");
                c.output_file.write(c.indent + "store i32 8, i32* " + clo_type_ptr + "\n");
                let clo_func_ptr_i8 -> String = next_reg(c);
                c.output_file.write(c.indent + clo_func_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_reg + ", i32 8\n");
                let clo_func_ptr -> String = next_reg(c);
                c.output_file.write(c.indent + clo_func_ptr + " = bitcast i8* " + clo_func_ptr_i8 + " to i8**\n");
                c.output_file.write(c.indent + "store i8* " + cast_reg + ", i8** " + clo_func_ptr + "\n");
                let clo_env_ptr_i8 -> String = next_reg(c);
                c.output_file.write(c.indent + clo_env_ptr_i8 + " = getelementptr inbounds i8, i8* " + clo_reg + ", i32 16\n");
                let clo_env_ptr -> String = next_reg(c);
                c.output_file.write(c.indent + clo_env_ptr + " = bitcast i8* " + clo_env_ptr_i8 + " to i8**\n");
                c.output_file.write(c.indent + "store i8* null, i8** " + clo_env_ptr + "\n");
                
                let clo_payload -> String = next_reg(c);
                c.output_file.write(c.indent + clo_payload + " = getelementptr inbounds i8, i8* " + clo_reg + ", i32 8\n");

                return CompileResult(reg=clo_payload, type=specific_type_id);
            }

            WhitelangExceptions.throw_name_error(v.pos, "Undefined variable or function '" + var_name + "'. ");
        }
        
        let llvm_ty_str -> String = get_llvm_type_str(c, info.type);
        if (llvm_ty_str == "") {
            WhitelangExceptions.throw_type_error(v.pos, "Variable '" + var_name + "' has invalid internal type ID. ");
        }

        let arr_check -> ArrayInfo = c.array_info_map.get("" + info.type);
        if (arr_check is !null) {
            if (arr_check.size != -1) {
                return CompileResult(reg=info.reg, type=info.type, origin_type=info.origin_type);
            }
        }
        
        let val_reg -> String = next_reg(c);
        c.output_file.write(c.indent + val_reg + " = load " + llvm_ty_str + ", " + llvm_ty_str + "* " + info.reg + "\n");
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

            let obj_base_pre -> BaseNode = f_acc.obj;
            if (obj_base_pre.type == NODE_SUPER) {
                let self_info -> SymbolInfo = find_symbol(c, "self");
                if (self_info is null) { WhitelangExceptions.throw_invalid_syntax(n_call.pos, "Cannot use 'super' outside of a method."); }

                let curr_class -> StructInfo = c.struct_id_map.get("" + self_info.type);
                if (curr_class is null || !curr_class.is_class || curr_class.parent_id == 0) {
                    WhitelangExceptions.throw_type_error(n_call.pos, "Cannot use 'super', class has no parent.");
                }

                let p_info -> StructInfo = c.struct_id_map.get("" + curr_class.parent_id);
                let target_m_name -> String = f_acc.field_name;
                if (target_m_name == "init") { target_m_name = "$init"; }
                if (target_m_name == "deinit") { target_m_name = "$deinit"; }

                let full_m_name -> String = c.current_package_prefix + p_info.name + "_" + target_m_name;
                let f_info -> FuncInfo = c.func_table.get(full_m_name);
                if (f_info is null) {
                    WhitelangExceptions.throw_name_error(n_call.pos, "Method '" + target_m_name + "' not found in parent class '" + p_info.name + "'.");
                }

                let self_ty_str -> String = get_llvm_type_str(c, self_info.type);
                let self_val_reg -> String = next_reg(c);
                c.output_file.write(c.indent + self_val_reg + " = load " + self_ty_str + ", " + self_ty_str + "* " + self_info.reg + "\n");
                let self_res -> CompileResult = CompileResult(reg=self_val_reg, type=self_info.type, origin_type=0);

                c.expected_type = curr_class.parent_id;
                let casted_self -> CompileResult = emit_implicit_cast(c, self_res, curr_class.parent_id, n_call.pos);
                c.expected_type = 0;

                let sig -> String = get_func_sig_str(c, f_info);
                let args_str -> String = get_llvm_type_str(c, curr_class.parent_id) + " " + casted_self.reg;

                let args -> Vector(Struct) = n_call.args;
                let a_len -> Int = 0; if (args is !null) { a_len = args.length(); }
                let arg_idx -> Int = 0;
                let expected_types -> Vector(Struct) = f_info.arg_types;
                
                let expected_arg_count -> Int = 0;
                if (expected_types is !null) { expected_arg_count = expected_types.length() - 1; }
                
                if (a_len != expected_arg_count) {
                    WhitelangExceptions.throw_type_error(n_call.pos, "super." + target_m_name + " expects " + expected_arg_count + " arguments, got " + a_len + ".");
                }

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
                    c.output_file.write(c.indent + "call " + llvm_ret_type + " @" + full_m_name + "(" + args_str + ")\n");
                    return CompileResult(reg="", type=TYPE_VOID, origin_type=0);
                } else {
                    let call_res -> String = next_reg(c);
                    c.output_file.write(c.indent + call_res + " = call " + llvm_ret_type + " @" + full_m_name + "(" + args_str + ")\n");
                    return CompileResult(reg=call_res, type=f_info.ret_type, origin_type=0);
                }
            }

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
                    let pkg_marker -> StringConstant = c.loaded_packages.get(pkg_name);
                    if (pkg_marker is !null) {
                        func_name = pkg_marker.value + "." + f_acc.field_name;
                        is_package_call = true;
                    } else if (c.loaded_files.get(pkg_name) is !null) {
                        let file_marker -> StringConstant = c.loaded_files.get(pkg_name);
                        func_name = file_marker.value + f_acc.field_name;
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
                let s_info -> StructInfo = c.struct_id_map.get("" + struct_type_id);
                if (s_info is !null && s_info.is_class) {
                    return compile_class_method_call(c, s_info, obj_res, f_acc.field_name, n_call);
                }
            }
        }

        if (callee.type == NODE_VAR_ACCESS) {
            let v_node -> VarAccessNode = n_call.callee;
            func_name = v_node.name_tok.value;
        }

        if (func_name != "") {
            if (is_package_call) {
                is_direct = true;
            } else {
                let s_check -> StructInfo = c.struct_table.get(func_name);
                if (s_check is !null) {
                    is_direct = true;
                } else {
                    let f_check -> FuncInfo = c.func_table.get(func_name);
                    let v_check -> SymbolInfo = find_symbol(c, func_name);
                    
                    if (f_check is !null && v_check is null) {
                        is_direct = true;
                    }
                }
            }
        }

        if is_direct {
            let target_func_name -> String = func_name;
            let check_built -> FuncInfo = c.func_table.get(func_name);
            if (check_built is !null) { target_func_name = check_built.name; }

            if (target_func_name == "builtin.print") {
                let args -> Vector(Struct) = n_call.args;
                let a_len -> Int = 0;
                if (args is !null) { a_len = args.length(); }

                if (a_len == 0) {
                    let fmt_ptr -> String = next_reg(c);
                    c.output_file.write(c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_newline, i32 0, i32 0))\n");
                    return void_result();
                }
                
                let a_idx -> Int = 0;
                while (a_idx < a_len) {
                    let curr_arg -> ArgNode = args[a_idx];
                    let arg_res -> CompileResult = compile_node(c, curr_arg.val);
                    compile_print(c, arg_res.reg, arg_res.type, n_call.pos, arg_res.origin_type);
                    a_idx += 1;
                }

                c.output_file.write(c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_newline, i32 0, i32 0))\n");
                return void_result();
            }

            let s_info -> StructInfo = c.struct_table.get(func_name);
            if (s_info is !null) {
                if (s_info.is_class) {
                    return compile_class_init(c, s_info, n_call);
                } else {
                    return compile_struct_init(c, s_info, n_call);
                }
            }

            let func_info -> FuncInfo = c.func_table.get(func_name);

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
                        c.output_file.write(c.indent + zext_reg + " = zext i1 " + arg_val.reg + " to i32\n");
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
                    c.output_file.write(c.indent + "call " + call_prefix + "@" + func_info.name + "(" + args_str + ")\n");
                } else {
                    c.output_file.write(c.indent + "call void @" + func_info.name + "(" + args_str + ")\n");
                }
                return void_result();
            } else {
                call_res_reg = next_reg(c);
                c.output_file.write(c.indent + call_res_reg + " = call " + call_prefix + "@" + func_info.name + "(" + args_str + ")\n");
                return CompileResult(reg=call_res_reg, type=func_info.ret_type);
            }
        }

        else {
            if (callee.type == NODE_VAR_ACCESS) {
                let v_node -> VarAccessNode = n_call.callee;
                let s_info -> StructInfo = c.struct_table.get(v_node.name_tok.value);
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

            if (ptr_type == TYPE_GENERIC_FUNCTION || ptr_type == TYPE_GENERIC_METHOD) {
                if (callee.type == NODE_VAR_ACCESS) {
                    let v_node -> VarAccessNode = n_call.callee;
                    let info -> SymbolInfo = find_symbol(c, v_node.name_tok.value);
                    if (info is !null && info.origin_type >= 100) {
                        let f_ret_info -> SymbolInfo = c.func_ret_map.get("" + info.origin_type);
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
                let f_ret_info -> SymbolInfo = c.func_ret_map.get("" + ptr_type);
                if (f_ret_info is !null) {
                    ret_type_id = f_ret_info.type;
                    is_valid_call = true;
                }
            }

            if is_valid_call {
                let is_closure -> Bool = false;
                let actual_env_reg -> String = "";
                let raw_func_ptr -> String = callee_res.reg;
                if (ptr_type == TYPE_GENERIC_FUNCTION || c.func_ret_map.get("" + ptr_type) is !null) {
                    is_closure = true;
                    let env_ptr_i8_addr -> String = next_reg(c);
                    c.output_file.write(c.indent + env_ptr_i8_addr + " = getelementptr inbounds i8, i8* " + callee_res.reg + ", i32 8\n");
                    let env_ptr_addr -> String = next_reg(c);
                    c.output_file.write(c.indent + env_ptr_addr + " = bitcast i8* " + env_ptr_i8_addr + " to i8**\n");
                    actual_env_reg = next_reg(c);
                    c.output_file.write(c.indent + actual_env_reg + " = load i8*, i8** " + env_ptr_addr + "\n");
                    let f_ptr_i8_addr -> String = next_reg(c);
                    c.output_file.write(c.indent + f_ptr_i8_addr + " = getelementptr inbounds i8, i8* " + callee_res.reg + ", i32 0\n");
                    let f_ptr_addr -> String = next_reg(c);
                    c.output_file.write(c.indent + f_ptr_addr + " = bitcast i8* " + f_ptr_i8_addr + " to i8**\n");
                    raw_func_ptr = next_reg(c);
                    c.output_file.write(c.indent + raw_func_ptr + " = load i8*, i8** " + f_ptr_addr + "\n");
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
                    c.output_file.write(c.indent + is_env_null + " = icmp eq i8* " + actual_env_reg + ", null\n");
                    
                    let l_global -> String = "call_g_" + c.type_counter;
                    let l_closure -> String = "call_c_" + c.type_counter;
                    let l_merge -> String = "call_m_" + c.type_counter;
                    c.type_counter += 1;
                    
                    c.output_file.write(c.indent + "br i1 " + is_env_null + ", label %" + l_global + ", label %" + l_closure + "\n");

                    c.output_file.write("\n" + l_global + ":\n");
                    let cast_g -> String = next_reg(c);
                    c.output_file.write("  " + cast_g + " = bitcast i8* " + raw_func_ptr + " to " + ret_ty_str + " (" + sig_g + ")*\n");
                    let res_g -> String = "";
                    if (ret_type_id == TYPE_VOID) {
                        c.output_file.write("  call void " + cast_g + "(" + args_g_str + ")\n");
                    } else {
                        res_g = next_reg(c);
                        c.output_file.write("  " + res_g + " = call " + ret_ty_str + " " + cast_g + "(" + args_g_str + ")\n");
                    }
                    c.output_file.write("  br label %" + l_merge + "\n");

                    c.output_file.write("\n" + l_closure + ":\n");
                    let cast_c -> String = next_reg(c);
                    c.output_file.write("  " + cast_c + " = bitcast i8* " + raw_func_ptr + " to " + ret_ty_str + " (" + sig_c + ")*\n");
                    let res_c -> String = "";
                    if (ret_type_id == TYPE_VOID) {
                        c.output_file.write("  call void " + cast_c + "(" + args_c_str + ")\n");
                    } else {
                        res_c = next_reg(c);
                        c.output_file.write("  " + res_c + " = call " + ret_ty_str + " " + cast_c + "(" + args_c_str + ")\n");
                    }
                    c.output_file.write("  br label %" + l_merge + "\n");

                    c.output_file.write("\n" + l_merge + ":\n");
                    if (ret_type_id == TYPE_VOID) {
                        return void_result();
                    } else {
                        let final_res -> String = next_reg(c);
                        c.output_file.write("  " + final_res + " = phi " + ret_ty_str + " [ " + res_g + ", %" + l_global + " ], [ " + res_c + ", %" + l_closure + " ]\n");
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
        c.output_file.write(c.indent + "br label %" + scope.label_break + "\n");

        return void_result();
    }

    if (base.type == NODE_CONTINUE) {
        let n_cont -> ContinueNode = node;
        if (c.loop_stack is null) {
            WhitelangExceptions.throw_invalid_syntax(n_cont.pos, "'continue' outside of loop. ");
        }
        let scope -> LoopScope = c.loop_stack;
        c.output_file.write(c.indent + "br label %" + scope.label_continue + "\n");

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
                        let s_info_temp -> StructInfo = c.struct_id_map.get("" + type_id);
                        if (s_info_temp is !null) {
                            let cast_reg -> String = next_reg(c);
                            c.output_file.write(c.indent + cast_reg + " = bitcast i8* " + obj_reg + " to " + s_info_temp.llvm_name + "*\n");
                            obj_reg = cast_reg;
                        }
                    }
                }
            }
            
            let s_info -> StructInfo = c.struct_id_map.get("" + type_id);
            if (s_info is null) { WhitelangExceptions.throw_type_error(u.pos, "Cannot access field on non-struct type."); }
            
            let field -> FieldInfo = find_field(s_info, f_acc.field_name);
            if (field is null) { WhitelangExceptions.throw_name_error(u.pos, "Field '" + f_acc.field_name + "' not found."); }
            
            target_type = field.type;
            type_str = field.llvm_type;
            target_reg = next_reg(c);

            c.output_file.write(c.indent + target_reg + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_reg + ", i32 0, i32 " + field.offset + "\n");
            
        } else {
            let op_str -> String = "++";
            if (op_type == TOK_DEC) { op_str = "--"; }
            WhitelangExceptions.throw_type_error(u.pos, "Operator '" + op_str + "' can only be applied to variables or struct fields.");
        }
        
        if (target_type == TYPE_BOOL) {
            WhitelangExceptions.throw_type_error(u.pos, "Cannot increment/decrement Bool type. ");
        }

        let old_val_reg -> String = next_reg(c);
        c.output_file.write(c.indent + old_val_reg + " = load " + type_str + ", " + type_str + "* " + target_reg + "\n");
        
        let new_val_reg -> String = next_reg(c);
        if (target_type == TYPE_INT) {
            let op_code -> String = "add i32";
            if (op_type == TOK_DEC) { op_code = "sub i32"; }
            c.output_file.write(c.indent + new_val_reg + " = " + op_code + " " + old_val_reg + ", 1\n");
        } 
        else if (target_type == TYPE_LONG) {
            let op_code -> String = "add i64";
            if (op_type == TOK_DEC) { op_code = "sub i64"; }
            c.output_file.write(c.indent + new_val_reg + " = " + op_code + " " + old_val_reg + ", 1\n");
        }
        else if (target_type == TYPE_FLOAT) {
            let op_code -> String = "fadd double";
            if (op_type == TOK_DEC) { op_code = "fsub double"; }
            c.output_file.write(c.indent + new_val_reg + " = " + op_code + " " + old_val_reg + ", 1.0\n");
        }
        else {
            WhitelangExceptions.throw_type_error(u.pos, "Cannot increment/decrement type " + get_type_name(c, target_type));
        }

        c.output_file.write(c.indent + "store " + type_str + " " + new_val_reg + ", " + type_str + "* " + target_reg + "\n");
        return CompileResult(reg=old_val_reg, type=target_type);
    }

    if (base.type == NODE_UNARYOP) {
        let u -> UnaryOpNode = node;
        let op_type -> Int = u.op_tok.type; 
        
        let operand -> CompileResult = compile_node(c, u.node);
        let res_reg -> String = next_reg(c);
        
        if (op_type == TOK_SUB) {
            if (operand.type == TYPE_INT) {
                c.output_file.write(c.indent + res_reg + " = sub i32 0, " + operand.reg + "\n");
                return CompileResult(reg=res_reg, type=TYPE_INT);
            } else if (operand.type == TYPE_FLOAT) {
                c.output_file.write(c.indent + res_reg + " = fneg double " + operand.reg + "\n");
                return CompileResult(reg=res_reg, type=TYPE_FLOAT);
            } else {
                WhitelangExceptions.throw_type_error(u.pos, "Cannot negate non-numeric type. ");
            }
        } else if (op_type == TOK_NOT) {
            if (operand.type != TYPE_BOOL) {
                WhitelangExceptions.throw_type_error(u.pos, "Operator '!' requires Bool type. ");
            }
            c.output_file.write(c.indent + res_reg + " = xor i1 " + operand.reg + ", 1\n");
            return CompileResult(reg=res_reg, type=TYPE_BOOL);
        } 
        else if (op_type == TOK_BIT_NOT) {
            if (operand.type == TYPE_INT) {
                c.output_file.write(c.indent + res_reg + " = xor i32 " + operand.reg + ", -1\n");
                return CompileResult(reg=res_reg, type=TYPE_INT);
            } else if (operand.type == TYPE_LONG) {
                c.output_file.write(c.indent + res_reg + " = xor i64 " + operand.reg + ", -1\n");
                return CompileResult(reg=res_reg, type=TYPE_LONG);
            } else if (operand.type == TYPE_BYTE) {
                c.output_file.write(c.indent + res_reg + " = xor i8 " + operand.reg + ", -1\n");
                return CompileResult(reg=res_reg, type=TYPE_BYTE);
            } else {
                WhitelangExceptions.throw_type_error(u.pos, "Operator '~' requires an integer type (Byte, Int, Long).");
            }
        }
        else {
            return operand;
        }
    }

    return null;
}

// BUILTIN HELPER
func compile_print(c -> Compiler, reg -> String, type_id -> Int, pos -> Position, origin_id -> Int) -> Void {
    if (type_id == TYPE_STRING) {
        c.output_file.write(c.indent + "call void @wl_write_utf8(i8* " + reg + ")\n");
        return;
    }

    if (type_id == TYPE_INT || type_id == TYPE_LONG || type_id == TYPE_FLOAT || type_id == TYPE_BOOL || type_id == TYPE_BYTE) {
        let temp_res -> CompileResult = CompileResult(reg=reg, type=type_id, origin_type=origin_id);
        let str_res -> CompileResult = convert_to_string(c, temp_res);
        c.output_file.write(c.indent + "call void @wl_write_utf8(i8* " + str_res.reg + ")\n");
        return;
    }

    if (type_id == TYPE_NULL || type_id == TYPE_NULLPTR) {
        c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str_null, i32 0, i32 0))\n");
        return;
    }

    if (is_pointer_type(c, type_id)) {
        let base_info -> SymbolInfo = c.ptr_base_map.get("" + type_id);
        if (base_info is !null && base_info.type == TYPE_BYTE) {
            c.output_file.write(c.indent + "call void @wl_write_utf8(i8* " + reg + ")\n");
        } else {
            c.output_file.write(c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.fmt_hex_ptr, i32 0, i32 0), i8* " + reg + ")\n");
        }
        return;
    }
    
    if (type_id == TYPE_GENERIC_STRUCT || type_id == TYPE_GENERIC_CLASS) {
        if (origin_id >= 100) {
            let s_info_real -> StructInfo = c.struct_id_map.get("" + origin_id);
            if (s_info_real is !null) {
                let cast_reg -> String = next_reg(c);
                c.output_file.write(c.indent + cast_reg + " = bitcast i8* " + reg + " to " + s_info_real.llvm_name + "*\n");
                compile_print_struct_internal(c, cast_reg, s_info_real, pos);
                return;
            }
        }
        c.output_file.write(c.indent + "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.fmt_hex_ptr, i32 0, i32 0), i8* " + reg + ")\n");
        return;
    }
    
    if (type_id >= 100) {
        let s_info -> StructInfo = c.struct_id_map.get("" + type_id);
        if (s_info is !null) {
            compile_print_struct_internal(c, reg, s_info, pos);
            return;
        } 
        
        let v_info -> SymbolInfo = c.vector_base_map.get("" + type_id);
        if (v_info is !null) {
            compile_print_vector_internal(c, reg, v_info, pos);
            return;
        }

        let arr_info -> ArrayInfo = c.array_info_map.get("" + type_id);
        if (arr_info is !null) {
            compile_print_array_internal(c, reg, arr_info, pos);
            return;
        }
    } 
}

func compile_print_struct_internal(c -> Compiler, obj_reg -> String, s_info -> StructInfo, pos -> Position) -> Void {
    let header -> String = s_info.name + "(";
    let header_id -> Int = register_string_constant(c, header);
    let header_ptr -> String = get_string_ptr(header_id, header);
    
    c.output_file.write(c.indent + "call void @wl_write_utf8(i8* " + header_ptr + ")\n");

    let fields_vec -> Vector(Struct) = s_info.fields;
    let f_len -> Int = 0;
    if (fields_vec is !null) { f_len = fields_vec.length(); }
    let f_idx -> Int = 0;
    
    while (f_idx < f_len) {
        let f_curr -> FieldInfo = fields_vec[f_idx];
        let f_name_eq -> String = f_curr.name + "=";
        let fn_id -> Int = register_string_constant(c, f_name_eq);
        let fn_ptr -> String = get_string_ptr(fn_id, f_name_eq);
    
        c.output_file.write(c.indent + "call void @wl_write_utf8(i8* " + fn_ptr + ")\n");
        
        let f_ptr -> String = next_reg(c);
        c.output_file.write(c.indent + f_ptr + " = getelementptr inbounds " + s_info.llvm_name + ", " + s_info.llvm_name + "* " + obj_reg + ", i32 0, i32 " + f_curr.offset + "\n");
        let f_val_reg -> String = next_reg(c);
        c.output_file.write(c.indent + f_val_reg + " = load " + f_curr.llvm_type + ", " + f_curr.llvm_type + "* " + f_ptr + "\n");
        compile_print(c, f_val_reg, f_curr.type, pos, f_curr.type); 

        f_idx += 1;
        if (f_idx < f_len) {
            c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str_comma_space, i32 0, i32 0))\n");
        }
    }
    c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_close_paren, i32 0, i32 0))\n");
}

func compile_print_vector_internal(c -> Compiler, vec_reg -> String, v_info -> SymbolInfo, pos -> Position) -> Void {
    let elem_type -> Int = v_info.type;
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type);
    let struct_ty -> String = "{ i64, i64, " + elem_ty_str + "* }";

    let size_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + size_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_reg + ", i32 0, i32 0\n");
    let size_val -> String = next_reg(c);
    c.output_file.write(c.indent + size_val + " = load i64, i64* " + size_ptr + "\n");
    
    let data_ptr_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + data_ptr_ptr + " = getelementptr inbounds " + struct_ty + ", " + struct_ty + "* " + vec_reg + ", i32 0, i32 2\n");
    let data_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + data_ptr + " = load " + elem_ty_str + "*, " + elem_ty_str + "** " + data_ptr_ptr + "\n");

    c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_open_bracket, i32 0, i32 0))\n");

    let label_cond -> String = next_label(c);
    let label_body -> String = next_label(c);
    let label_sep  -> String = next_label(c);
    let label_end  -> String = next_label(c);

    let idx_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + idx_ptr + " = alloca i64\n");
    c.output_file.write(c.indent + "store i64 0, i64* " + idx_ptr + "\n");
    c.output_file.write(c.indent + "br label %" + label_cond + "\n");

    c.output_file.write("\n" + label_cond + ":\n");
    let curr_idx -> String = next_reg(c);
    c.output_file.write(c.indent + curr_idx + " = load i64, i64* " + idx_ptr + "\n");
    let cmp -> String = next_reg(c);
    c.output_file.write(c.indent + cmp + " = icmp slt i64 " + curr_idx + ", " + size_val + "\n");
    c.output_file.write(c.indent + "br i1 " + cmp + ", label %" + label_body + ", label %" + label_end + "\n");

    c.output_file.write("\n" + label_body + ":\n");
    let slot -> String = next_reg(c);
    c.output_file.write(c.indent + slot + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i64 " + curr_idx + "\n");
    let val -> String = next_reg(c);
    c.output_file.write(c.indent + val + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot + "\n");
    
    compile_print(c, val, elem_type, pos, elem_type);

    let next_idx -> String = next_reg(c);
    c.output_file.write(c.indent + next_idx + " = add i64 " + curr_idx + ", 1\n");
    c.output_file.write(c.indent + "store i64 " + next_idx + ", i64* " + idx_ptr + "\n");
    
    let is_not_last -> String = next_reg(c);
    c.output_file.write(c.indent + is_not_last + " = icmp slt i64 " + next_idx + ", " + size_val + "\n");
    c.output_file.write(c.indent + "br i1 " + is_not_last + ", label %" + label_sep + ", label %" + label_cond + "\n");

    c.output_file.write("\n" + label_sep + ":\n");
    c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str_comma_space, i32 0, i32 0))\n");
    c.output_file.write(c.indent + "br label %" + label_cond + "\n");

    c.output_file.write("\n" + label_end + ":\n");
    c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_close_bracket, i32 0, i32 0))\n");
}

func compile_print_array_internal(c -> Compiler, arr_reg -> String, arr_info -> ArrayInfo, pos -> Position) -> Void {
    let elem_type -> Int = arr_info.base_type;
    let elem_ty_str -> String = get_llvm_type_str(c, elem_type);

    c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_open_bracket, i32 0, i32 0))\n");

    let size_val -> String = next_reg(c);
    if (arr_info.size == -1) {
        let size_i64 -> String = next_reg(c);
        c.output_file.write(c.indent + size_i64 + " = extractvalue " + arr_info.llvm_name + " " + arr_reg + ", 0\n");
        c.output_file.write(c.indent + size_val + " = trunc i64 " + size_i64 + " to i32\n");
    } else {
        c.output_file.write(c.indent + size_val + " = add i32 0, " + arr_info.size + "\n");
    }

    let data_ptr -> String = "";
    if (arr_info.size == -1) {
        data_ptr = next_reg(c);
        c.output_file.write(c.indent + data_ptr + " = extractvalue " + arr_info.llvm_name + " " + arr_reg + ", 1\n");
    } else {
        data_ptr = next_reg(c);
        c.output_file.write(c.indent + data_ptr + " = getelementptr inbounds " + arr_info.llvm_name + ", " + arr_info.llvm_name + "* " + arr_reg + ", i32 0, i32 0\n");
    }

    let label_cond -> String = next_label(c);
    let label_body -> String = next_label(c);
    let label_sep  -> String = next_label(c);
    let label_end  -> String = next_label(c);

    let idx_ptr -> String = next_reg(c);
    c.output_file.write(c.indent + idx_ptr + " = alloca i32\n");
    c.output_file.write(c.indent + "store i32 0, i32* " + idx_ptr + "\n");
    c.output_file.write(c.indent + "br label %" + label_cond + "\n");

    c.output_file.write("\n" + label_cond + ":\n");
    let curr_idx -> String = next_reg(c);
    c.output_file.write(c.indent + curr_idx + " = load i32, i32* " + idx_ptr + "\n");
    let cmp -> String = next_reg(c);

    c.output_file.write(c.indent + cmp + " = icmp slt i32 " + curr_idx + ", " + size_val + "\n");
    c.output_file.write(c.indent + "br i1 " + cmp + ", label %" + label_body + ", label %" + label_end + "\n");

    c.output_file.write("\n" + label_body + ":\n");
    let slot_ptr -> String = next_reg(c);

    c.output_file.write(c.indent + slot_ptr + " = getelementptr inbounds " + elem_ty_str + ", " + elem_ty_str + "* " + data_ptr + ", i32 " + curr_idx + "\n");
    
    let val_reg -> String = "";
    if (c.array_info_map.get("" + elem_type) is !null) {
        val_reg = slot_ptr;
    } else {
        val_reg = next_reg(c);
        c.output_file.write(c.indent + val_reg + " = load " + elem_ty_str + ", " + elem_ty_str + "* " + slot_ptr + "\n");
    }
    
    compile_print(c, val_reg, elem_type, pos, elem_type);

    let next_idx -> String = next_reg(c);
    c.output_file.write(c.indent + next_idx + " = add i32 " + curr_idx + ", 1\n");
    c.output_file.write(c.indent + "store i32 " + next_idx + ", i32* " + idx_ptr + "\n");
    
    let is_not_last -> String = next_reg(c);
    c.output_file.write(c.indent + is_not_last + " = icmp slt i32 " + next_idx + ", " + size_val + "\n");
    c.output_file.write(c.indent + "br i1 " + is_not_last + ", label %" + label_sep + ", label %" + label_cond + "\n");

    c.output_file.write("\n" + label_sep + ":\n");
    c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str_comma_space, i32 0, i32 0))\n");
    c.output_file.write(c.indent + "br label %" + label_cond + "\n");

    c.output_file.write("\n" + label_end + ":\n");
    c.output_file.write(c.indent + "call void @wl_write_utf8(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str_close_bracket, i32 0, i32 0))\n");
}

func compile_string_method_call(c -> Compiler, obj_node -> Struct, method_name -> String, call_node -> CallNode) -> CompileResult {
    let target_func -> String = "string_" + method_name;
    let real_func_name -> String = "";
    
    if (c.func_table.get(target_func) is !null) {
        real_func_name = target_func;
    } else if (c.func_table.get("builtin." + target_func) is !null) {
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
            c.output_file.write(c.indent + zext_reg + " = zext i1 " + arg_res.reg + " to i32\n");
            arg_res = CompileResult(reg=zext_reg, type=TYPE_INT);
        }
        
        args_str = args_str + get_llvm_type_str(c, arg_res.type) + " " + arg_res.reg;
        a_idx += 1;
    }

    let f_info -> FuncInfo = c.func_table.get(real_func_name);
    let ret_ty_str -> String = get_llvm_type_str(c, f_info.ret_type);
    let call_reg -> String = "";
    if (f_info.ret_type == TYPE_VOID) {
        c.output_file.write(c.indent + "call void @" + real_func_name + "(" + args_str + ")\n");
        return void_result();
    } else {
        call_reg = next_reg(c);
        c.output_file.write(c.indent + call_reg + " = call " + ret_ty_str + " @" + real_func_name + "(" + args_str + ")\n");
        return CompileResult(reg=call_reg, type=f_info.ret_type);
    }
}
// --------------

func compile_start(c -> Compiler) -> Void {
    c.output_file.write("declare i32 @printf(i8*, ...)\n");
    c.declared_externs.put("printf", StringConstant(id=0, value=""));

    c.output_file.write("declare i32 @snprintf(i8*, i64, i8*, ...)\n");
    c.declared_externs.put("snprintf", StringConstant(id=0, value="")); 

    c.output_file.write("declare double @llvm.pow.f64(double, double)\n\n");

    c.output_file.write("declare i8* @malloc(i64)\n");
    c.declared_externs.put("malloc", StringConstant(id=0, value=""));

    c.output_file.write("declare i32 @strlen(i8*)\n");
    c.declared_externs.put("strlen", StringConstant(id=0, value="")); 

    c.output_file.write("declare i8* @strcpy(i8*, i8*)\n");
    c.declared_externs.put("strcpy", StringConstant(id=0, value="")); 

    c.output_file.write("declare i8* @strcat(i8*, i8*)\n\n");
    c.declared_externs.put("strcat", StringConstant(id=0, value="")); 

    c.output_file.write("declare i32 @strcmp(i8*, i8*)\n\n");
    c.declared_externs.put("strcmp", StringConstant(id=0, value="")); 

    c.output_file.write("declare void @free(i8*)\n");
    c.declared_externs.put("free", StringConstant(id=0, value=""));

    c.output_file.write("declare void @exit(i32)\n");
    c.declared_externs.put("exit", StringConstant(id=0, value=""));

    c.output_file.write("declare i8* @realloc(i8*, i64)\n");
    c.declared_externs.put("realloc", StringConstant(id=0, value=""));

    c.output_file.write("declare void @wl_write_utf8(i8*)\n");
    c.declared_externs.put("wl_write_utf8", StringConstant(id=0, value=""));


    c.output_file.write("@.fmt_int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"\n");
    c.output_file.write("@.fmt_long = private unnamed_addr constant [6 x i8] c\"%lld\\0A\\00\"\n");
    c.output_file.write("@.fmt_float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\"\n");
    c.output_file.write("@.fmt_str = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\"\n\n");
    c.output_file.write("@.fmt_hex_ptr = private unnamed_addr constant [3 x i8] c\"%p\\00\"\n");

    c.output_file.write("@.fmt_int_simple = private unnamed_addr constant [3 x i8] c\"%d\\00\"\n");
    c.output_file.write("@.fmt_float_simple = private unnamed_addr constant [3 x i8] c\"%f\\00\"\n");
    c.output_file.write("@.str_true = private unnamed_addr constant [5 x i8] c\"true\\00\"\n");
    c.output_file.write("@.str_false = private unnamed_addr constant [6 x i8] c\"false\\00\"\n");
    c.output_file.write("@.str_null = private unnamed_addr constant [5 x i8] c\"null\\00\"\n\n");
    c.output_file.write("@.str_newline = private unnamed_addr constant [2 x i8] c\"\\0A\\00\"\n");

    c.output_file.write("@.str_idx_err = private unnamed_addr constant [21 x i8] c\"Index out of bounds\\0A\\00\"\n\n");
    c.output_file.write("@.fmt_err_bounds = private unnamed_addr constant [66 x i8] c\"\\0ARuntimeError\\1B[0m: Index out of bounds\\0A    at Line %d, Column %d\\0A\\00\"\n\n");

    // builtin.print
    c.output_file.write("@.str_open_bracket = private unnamed_addr constant [2 x i8] c\"[\\00\"\n");
    c.output_file.write("@.str_close_bracket = private unnamed_addr constant [2 x i8] c\"]\\00\"\n");
    c.output_file.write("@.str_comma_space = private unnamed_addr constant [3 x i8] c\", \\00\"\n");
    c.output_file.write("@.str_open_paren = private unnamed_addr constant [2 x i8] c\"(\\00\"\n");
    c.output_file.write("@.str_close_paren = private unnamed_addr constant [2 x i8] c\")\\00\"\n");
    c.output_file.write("@.str_equal = private unnamed_addr constant [2 x i8] c\"=\\00\"\n");
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
        let def -> String = "@.str." + id + " = private unnamed_addr constant { i32, i32, [" + len + " x i8] } { i32 -1, i32 5, [" + len + " x i8] c\"" + escaped_val + "\\00\" }\n";
        c.output_file.write(def);
        s_idx += 1;
    }

    c.output_file.write("; ====== Lambda Lifted Closures and Envs =====\n");
    c.output_file.write(c.global_buffer);
    c.output_file.write("\n");
    c.output_file.close();
}