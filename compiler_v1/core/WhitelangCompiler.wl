// core/WhitelangCompiler.wl

import "file_io"
import "map"
import "WhitelangTokens.wl"
import "WhitelangNodes.wl"

// Type constants for internal tracking
const TYPE_INT   -> Int = 1;
const TYPE_FLOAT -> Int = 2;

// Result wrapper to track both register name and its LLVM type
struct CompileResult(
    reg  -> String,
    type -> Int
)

struct Compiler(
    output_file -> File,
    reg_count   -> Int,        // Virtual register counter like: (t1, t2...)
    symbol_table -> HashMap,   // Variable name -> allocated pointer name (%a)
    indent      -> String      // IR indentation used for generation
)

func new_compiler(out_path -> String) -> Compiler {
    let f -> File = open(out_path, "w");
    // The initialization registers start from 1
    return Compiler(
        output_file = f,
        reg_count = 1,
        symbol_table = map_new(32),
        indent = "  "
    );
}

// Create a new register name
func next_reg(c -> Compiler) -> String {
    let name -> String = "%t" + c.reg_count;
    c.reg_count = c.reg_count + 1;
    return name;
}

// Helper: Promote i32 to double using LLVM 'sitofp' instruction
func promote_to_float(c -> Compiler, res -> CompileResult) -> CompileResult {
    if (res.type == TYPE_FLOAT) { return res; }
    
    let n_reg -> String = next_reg(c);
    let ir -> String = c.indent + n_reg + " = sitofp i32 " + res.reg + " to double\n";
    write(c.output_file, ir);
    return CompileResult(reg=n_reg, type=TYPE_FLOAT);
}

// Access the node and return the CompileResult (reg + type)
func compile_node(c -> Compiler, node -> Struct) -> CompileResult {
    let base -> BaseNode = node;

    // Int
    if (base.type == NODE_INT) {
        let n -> IntNode = node;
        return CompileResult(reg=n.value, type=TYPE_INT);
    }

    // Float
    if (base.type == NODE_FLOAT) {
        let n -> FloatNode = node;
        // Note: LLVM requires decimal point in float constants
        return CompileResult(reg=n.value, type=TYPE_FLOAT);
    }

    // Unary op (Negative numbers)
    if (base.type == NODE_UNARYOP) {
        let u -> UnaryOpNode = node;
        let operand -> CompileResult = compile_node(c, u.node);
        let res_reg -> String = next_reg(c);

        if (u.op == TOK_SUB) {
            if (operand.type == TYPE_INT) {
                write(c.output_file, c.indent + res_reg + " = sub i32 0, " + operand.reg + "\n");
            } else {
                write(c.output_file, c.indent + res_reg + " = fneg double " + operand.reg + "\n");
            }
        } else {
            return operand; // Positive sign '+' does nothing
        }
        return CompileResult(reg=res_reg, type=operand.type);
    }

    // Bin op
    if (base.type == NODE_BINOP) {
        let b -> BinOpNode = node;
        let left -> CompileResult = compile_node(c, b.left);
        let right -> CompileResult = compile_node(c, b.right);
        
        // Implicit Type Promotion Logic
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
            if (b.op == TOK_PLUS)  { op_code = "add"; }
            if (b.op == TOK_SUB)   { op_code = "sub"; }
            if (b.op == TOK_MUL)   { op_code = "mul"; }
            if (b.op == TOK_DIV)   { op_code = "sdiv"; }
        } else {
            if (b.op == TOK_PLUS)  { op_code = "fadd"; }
            if (b.op == TOK_SUB)   { op_code = "fsub"; }
            if (b.op == TOK_MUL)   { op_code = "fmul"; }
            if (b.op == TOK_DIV)   { op_code = "fdiv"; }
        }

        let ir -> String = c.indent + res_reg + " = " + op_code + " " + type_str + " " + left.reg + ", " + right.reg + "\n";
        write(c.output_file, ir);
        
        return CompileResult(reg=res_reg, type=target_type);
    }

    return null;
}

// compiler header
func compile_start(c -> Compiler) -> Void {
    write(c.output_file, "declare i32 @printf(i8*, ...)\n\n");
    // Global format strings will be emitted at the end or on demand
    write(c.output_file, "define i32 @main() {\n");
    write(c.output_file, "entry:\n");
}

// Output
func compile_end(c -> Compiler, last -> CompileResult) -> Void {
    let fmt -> String = "%d\\0A\\00";
    let type_str -> String = "i32";

    if (last.type == TYPE_FLOAT) {
        fmt = "%f\\0A\\00";
        type_str = "double";
    }

    write(c.output_file, c.indent + "%fmt_ptr = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0\n");
    write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* %fmt_ptr, " + type_str + " " + last.reg + ")\n");
    write(c.output_file, c.indent + "ret i32 0\n");
    write(c.output_file, "}\n\n");

    let global_def -> String = "@.str = private unnamed_addr constant [4 x i8] c\"" + fmt + "\"";
    write(c.output_file, global_def + "\n");
    
    close(c.output_file);
}