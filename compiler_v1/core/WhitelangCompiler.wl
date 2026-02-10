// core/WhitelangCompiler.wl
import "file_io"
import "map"
import "WhitelangTokens.wl"
import "WhitelangNodes.wl"
import "WhitelangExceptions.wl"


// Type constants for internal tracking
const TYPE_INT   -> Int = 1;
const TYPE_FLOAT -> Int = 2;
const TYPE_BOOL  -> Int = 3;
const TYPE_VOID  -> Int = 4;

// Result wrapper to track both register name and its LLVM type
struct CompileResult(
    reg  -> String,
    type -> Int
)

struct SymbolInfo(
    reg  -> String, // Pointer register (e.g.: %t1)
    type -> Int     // Variable type
)

struct Compiler(
    output_file -> File,
    reg_count   -> Int,        // Virtual register counter like: (t1, t2...)
    symbol_table -> HashMap,   // Variable name -> allocated pointer name (%a)
    indent      -> String,     // IR indentation used for generation
    loop_stack   -> Struct     // Loop Context
)


struct LoopScope(
    label_continue -> String,  // continue
    label_break      -> String,  // break 
    parent         -> Struct   // loop scope
)


func new_compiler(out_path -> String) -> Compiler {
    let f -> File = open(out_path, "w");
    // The initialization registers start from 1
    return Compiler(
        output_file = f,
        reg_count = 1,
        symbol_table = map_new(32),
        indent = "  ",
        loop_stack = null
    );
}

// Create a new register name
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
    return ""; // error case
}

func next_label(c -> Compiler) -> String {
    let name -> String = "L" + c.reg_count;
    c.reg_count = c.reg_count + 1;
    return name;
}

func void_result() -> CompileResult {
    return CompileResult(reg="", type=TYPE_VOID);
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
    } else {
        throw_type_error(node.pos, "Unknown or unsupported type '" + type_name + "' in compiler backend. ");
    }
    
    // allocate stack memory
    let ptr_reg -> String = next_reg(c);
    write(c.output_file, c.indent + ptr_reg + " = alloca " + llvm_ty_str + "\n");
    
    // r-value
    if (node.value != null) {
        let val_res -> CompileResult = compile_node(c, node.value);
        
        // type checking
        if (target_type_id != val_res.type) {
            let type_name_got -> String = "Unknown";
            if (val_res.type == TYPE_INT)   { type_name_got = "Int"; }
            if (val_res.type == TYPE_FLOAT) { type_name_got = "Float"; }
            if (val_res.type == TYPE_BOOL)  { type_name_got = "Bool"; }
            
            throw_type_error(node.pos, "Cannot assign type '" + type_name_got + "' to variable of type '" + type_name + "'. ");
        }

        // store
        write(c.output_file, c.indent + "store " + llvm_ty_str + " " + val_res.reg + ", " + llvm_ty_str + "* " + ptr_reg + "\n");
    }
    
    // register to symbol table
    let var_name -> String = node.name_tok.value; 
    let info -> SymbolInfo = SymbolInfo(reg=ptr_reg, type=target_type_id);
    map_put(c.symbol_table, var_name, info);
    
    return void_result(); 
}

func compile_if(c -> Compiler, node -> IfNode) -> CompileResult {
    let cond_res -> CompileResult = compile_node(c, node.condition);
    if (cond_res.type != TYPE_BOOL) {
        throw_type_error(node.pos, "If condition must be a Bool. ");
    }
    
    // labels
    let label_then -> String = next_label(c);
    let label_else -> String = next_label(c);
    let label_merge -> String = next_label(c);
    
    // condition branch
    let target_else -> String = label_else;
    if (node.else_body == null) {
        target_else = label_merge;
    }
    
    write(c.output_file, c.indent + "br i1 " + cond_res.reg + ", label %" + label_then + ", label %" + target_else + "\n");
    
    // then block
    write(c.output_file, "\n" + label_then + ":\n");
    compile_node(c, node.body);
    write(c.output_file, c.indent + "br label %" + label_merge + "\n");
    
    // else
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

    // (&&, ||) -> Bool
    if (op_type == TOK_AND || op_type == TOK_OR) {
        if (left.type != TYPE_BOOL || right.type != TYPE_BOOL) {
            throw_type_error(node.pos, "Logic operators '&&' and '||' require Bool operands. ");
        }
        let res_reg -> String = next_reg(c);
        let op_code -> String = "and";
        if (op_type == TOK_OR) { op_code = "or"; }
        
        // llvm bitwise operators work on i1 as logic operators
        write(c.output_file, c.indent + res_reg + " = " + op_code + " i1 " + left.reg + ", " + right.reg + "\n");
        return CompileResult(reg=res_reg, type=TYPE_BOOL);
    }

    // (==, !=, >, <, >=, <=) -> Bool
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
            // Ordered float comparison (returns false on NaN)
            if (op_type == TOK_EE) { op_code = "fcmp oeq"; }
            if (op_type == TOK_NE) { op_code = "fcmp one"; }
            if (op_type == TOK_GT) { op_code = "fcmp ogt"; }
            if (op_type == TOK_LT) { op_code = "fcmp olt"; }
            if (op_type == TOK_GTE) { op_code = "fcmp oge"; }
            if (op_type == TOK_LTE) { op_code = "fcmp ole"; }
        } else {
            // Int or Bool
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

// --- Main Dispatch Function ---

func compile_node(c -> Compiler, node -> Struct) -> CompileResult {
    let base -> BaseNode = node;

    if (base.type == NODE_BLOCK) {
        let b -> BlockNode = node;
        let curr -> StmtListNode = b.stmts;
        let last_res -> CompileResult = null;
        while (curr != null) {
            last_res = compile_node(c, curr.stmt);
            curr = curr.next;
        }
        if (last_res == null) { return void_result();}
        return last_res;
    }

    if (base.type == NODE_VAR_DECL) { return compile_var_decl(c, node); }
    if (base.type == NODE_IF)       { return compile_if(c, node); }
    if (base.type == NODE_WHILE)    { return compile_while(c, node); }
    if (base.type == NODE_FOR)      { return compile_for(c, node); }
    if (base.type == NODE_BINOP)    { return compile_binop(c, node); }

    // Literals
    if (base.type == NODE_INT) {
        let n -> IntNode = node;
        return CompileResult(reg=n.tok.value, type=TYPE_INT); 
    }
    if (base.type == NODE_FLOAT) {
        let n -> FloatNode = node;
        return CompileResult(reg=n.tok.value, type=TYPE_FLOAT); 
    }
    
    // BooleanNode
    if (base.type == NODE_BOOL) {
        let b -> BooleanNode = node;
        // LLVM i1: true -> 1, false -> 0
        let val_str -> String = "0";
        if (b.value == 1) { val_str = "1"; }
        return CompileResult(reg=val_str, type=TYPE_BOOL);
    }

    if (base.type == NODE_VAR_ACCESS) {
        let v -> VarAccessNode = node;
        let var_name -> String = v.name_tok.value; // Access from Token
        
        // LUT
        let info -> SymbolInfo = map_get(c.symbol_table, var_name);
        if (info == null) {
             throw_name_error(v.pos, "Undefined variable '" + var_name + "'. ");
        }
        
        // load value
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

        let info -> SymbolInfo = map_get(c.symbol_table, var_name);
        if (info == null) {
            throw_name_error(n_assign.pos, "Cannot assign to undefined variable '" + var_name + "'. ");
        }

        let val_res -> CompileResult = compile_node(c, n_assign.value);
        
        // strict type check
        if (info.type != val_res.type) {
            throw_type_error(n_assign.pos, "Type mismatch in assignment. Expected " + get_llvm_type_str(info.type) + ", got " + get_llvm_type_str(val_res.type));
        }
        
        // store
        let ty_str -> String = get_llvm_type_str(info.type);
        write(c.output_file, c.indent + "store " + ty_str + " " + val_res.reg + ", " + ty_str + "* " + info.reg + "\n");
        
        return void_result(); // return void
    }

    if (base.type == NODE_CALL) {
        let n_call -> CallNode = node;
        let callee -> BaseNode = n_call.callee;
        
        // ! pseudo-function
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
                    // i1 -> i32 for printf
                    let ext_reg -> String = next_reg(c);
                    write(c.output_file, c.indent + ext_reg + " = zext i1 " + arg_res.reg + " to i32\n");
                    arg_res.reg = ext_reg;
                    arg_res.type = TYPE_INT;
                    ty_str = "i32";
                } else if (arg_res.type == TYPE_INT) {
                    // pass
                } else {
                    throw_type_error(n_call.pos, "Cannot print type " + get_llvm_type_str(arg_res.type));
                }

                let use_fmt -> String = "@.fmt_int";
                if (ty_str == "double") { use_fmt = "@.fmt_float"; }

                let ptr_reg -> String = next_reg(c);
                write(c.output_file, c.indent + ptr_reg + " = getelementptr [4 x i8], [4 x i8]* " + use_fmt + ", i32 0, i32 0\n");
                write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* " + ptr_reg + ", " + ty_str + " " + arg_res.reg + ")\n");

                return void_result();
            }
        }
        throw_name_error(n_call.pos, "Function definitions/calls are not fully implemented yet. Only 'pseudo_print' is available. ");
    }

    // BreakNode
    if (base.type == NODE_BREAK) {
        let n_break -> BreakNode = node;
        if (c.loop_stack == null) {
            throw_invalid_syntax(n_break.pos, "'break' outside of loop. ");
        }
        let scope -> LoopScope = c.loop_stack;
        write(c.output_file, c.indent + "br label %" + scope.label_break + "\n"); // 使用 label_break
        
        let dead_label -> String = next_label(c);
        write(c.output_file, "\n" + dead_label + ": ; Unreachable\n");
        return void_result();
    }

    // ContinueNode
    if (base.type == NODE_CONTINUE) {
        let n_cont -> ContinueNode = node;
        if (c.loop_stack == null) {
            throw_invalid_syntax(n_cont.pos, "'continue' outside of loop. ");
        }
        let scope -> LoopScope = c.loop_stack;
        write(c.output_file, c.indent + "br label %" + scope.label_continue + "\n"); // 使用 label_continue
        
        let dead_label -> String = next_label(c);
        write(c.output_file, "\n" + dead_label + ": ; Unreachable\n");
        return void_result();
    }

    // PostfixOpNode (a++, a--)
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
        let info -> SymbolInfo = map_get(c.symbol_table, var_name);
        
        if (info == null) {
            throw_name_error(v_acc.pos, "Undefined variable '" + var_name + "'. ");
        }

        // Fix allowing ++/-- on bool values in the Python compiler
        if (info.type == TYPE_BOOL) {
            throw_type_error(u.pos, "Cannot increment/decrement Bool type. "); // Boolean ++/-- is no longer allowed.
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

    // Unary op
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
            // xor i1 %val, 1  (flips the bit)
            if (operand.type != TYPE_BOOL) {
                throw_type_error(u.pos, "Operator '!' requires Bool type. ");
            }
            write(c.output_file, c.indent + res_reg + " = xor i1 " + operand.reg + ", 1\n");
            return CompileResult(reg=res_reg, type=TYPE_BOOL);
        } else {
            return operand; // +5 -> 5
        }
    }

    return null;
}

// compiler header
func compile_start(c -> Compiler) -> Void {
    // int printf(i8* format, ...)
    write(c.output_file, "declare i32 @printf(i8*, ...)\n");
    // double pow(double, double)
    write(c.output_file, "declare double @llvm.pow.f64(double, double)\n\n");
    
    // self.printf_fmt_int = self._global_string("fmt_int", "%d\n\0")
    write(c.output_file, "@.fmt_int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"\n");
    // self.printf_fmt_float = self._global_string("fmt_float", "%f\n\0")
    write(c.output_file, "@.fmt_float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\"\n\n");

    write(c.output_file, "define i32 @main() {\n");
    write(c.output_file, "entry:\n");
}

// Output
func compile_end(c -> Compiler, last -> CompileResult) -> Void {
    if (last == null || last.type == TYPE_VOID) {
        write(c.output_file, c.indent + "ret i32 0\n");
        write(c.output_file, "}\n\n");
        close(c.output_file);
        return;
    }

    let fmt -> String = "%d\\0A\\00";
    let type_str -> String = "i32";

    if (last.type == TYPE_FLOAT) {
        fmt = "%f\\0A\\00";
        type_str = "double";
    } else if (last.type == TYPE_BOOL) {
        fmt = "%d\\0A\\00";
        type_str = "i1";
        let ext_reg -> String = next_reg(c);
        write(c.output_file, c.indent + ext_reg + " = zext i1 " + last.reg + " to i32\n");
        last.reg = ext_reg;
        last.type = TYPE_INT;
        type_str = "i32";
    }

    write(c.output_file, c.indent + "%fmt_ptr = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0\n");
    write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* %fmt_ptr, " + type_str + " " + last.reg + ")\n");
    write(c.output_file, c.indent + "ret i32 0\n");
    write(c.output_file, "}\n\n");

    let global_def -> String = "@.str = private unnamed_addr constant [4 x i8] c\"" + fmt + "\"";
    write(c.output_file, global_def + "\n");
    
    close(c.output_file);
}