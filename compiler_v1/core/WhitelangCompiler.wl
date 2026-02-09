// core/WhitelangCompiler.wl

import "file_io"
import "map"
import "WhitelangTokens.wl"
import "WhitelangNodes.wl"

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

// Access the node and return the register/constant name where the result is located
func compile_node(c -> Compiler, node -> Struct) -> String {
    let base -> BaseNode = node;

    // Int
    if (base.type == NODE_INTEGER) {
        let n -> IntegerNode = node;
        return n.value;
    }

    // Bin op
    if (base.type == NODE_BINOP) {
        let b -> BinOpNode = node;
        let left_val -> String = compile_node(c, b.left);
        let right_val -> String = compile_node(c, b.right);
        
        let res_reg -> String = next_reg(c);
        let op_code -> String = "";
        
        if (b.op == TOK_PLUS)  { op_code = "add"; }
        if (b.op == TOK_SUB)   { op_code = "sub"; }
        if (b.op == TOK_MUL)   { op_code = "mul"; }
        if (b.op == TOK_DIV)   { op_code = "sdiv"; }

        // %t1 = add i32 10, 20
        let ir -> String = c.indent + res_reg + " = " + op_code + " i32 " + left_val + ", " + right_val + "\n";
        write(c.output_file, ir);
        
        return res_reg;
    }

    return "";
}


// compiler header
func compile_start(c -> Compiler) -> Void {
    write(c.output_file, "declare i32 @printf(i8*, ...)\n");
    write(c.output_file, "@fstr = internal constant [4 x i8] c\"%d\\0A\\00\"\n\n");
    write(c.output_file, "define i32 @main() {\n");
    write(c.output_file, "entry:\n");
}

// Output
func compile_end(c -> Compiler, last_reg -> String) -> Void {
    write(c.output_file, c.indent + "%fmt_ptr = getelementptr [4 x i8], [4 x i8]* @fstr, i32 0, i32 0\n");
    write(c.output_file, c.indent + "call i32 (i8*, ...) @printf(i8* %fmt_ptr, i32 " + last_reg + ")\n");
    write(c.output_file, c.indent + "ret i32 0\n");
    write(c.output_file, "}\n");
    close(c.output_file);
}