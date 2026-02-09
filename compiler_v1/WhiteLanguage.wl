// compiler_v1/WhiteLanguage.wl
import "builtin"
import "file_io"

// Core components
import "core/WhitelangTokens.wl"
import "core/WhitelangLexer.wl"
import "core/WhitelangNodes.wl"
import "core/WhitelangParser.wl"
import "core/WhitelangCompiler.wl"


func get_output_name(input_name -> String) -> String {
    let len -> Int = input_name.length();
    if (len < 4) { return "output.ll"; }
    let prefix -> String = input_name.slice(0, len - 3);
    return prefix + ".ll";
}


func print_ast(node -> Struct, indent -> String) -> Void {
    if (node == null) {
        return;
    }

    let base -> BaseNode = node;
    
    if (base.type == NODE_INTEGER) {
        let n -> IntegerNode = node;
        builtin.print(indent + "IntegerNode(" + n.value + ")");
    }

    if (base.type == NODE_BINOP) {
        let b -> BinOpNode = node;
        let op_name -> String = get_token_name(b.op);
        builtin.print(indent + "BinOpNode(" + op_name + ")");
        print_ast(b.left,  indent + "  ");
        print_ast(b.right, indent + "  ");
    }
}


func main() -> Int {
    let input_filename -> String = "./tests/calculator.wl"; // Input source code file
    let f_in -> File = file_io.open(input_filename, "r");
    if (f_in == null) {
        builtin.print("Error: Could not open input file " + input_filename);
        return 1;
    }
    let source -> String = file_io.read_all(f_in);
    file_io.close(f_in); // Release file

    builtin.print("Compiling: " + input_filename + "...");

    // Lex & Parse
    let lexer -> Lexer = new_lexer(source);
    let parser -> Parser = Parser(lexer=lexer, current_tok=get_next_token(lexer));
    let ast -> Struct = parse(parser);
    if (ast == null) {
        builtin.print("Parsing failed.");
        return 1;
    }
    
    // compile
    let output_filename -> String = get_output_name(input_filename);

    let compiler -> Compiler = new_compiler(output_filename);
    compile_start(compiler); // Header required for compiling LLVM
    
    let result_reg -> String = compile_node(compiler, ast);
    
    compile_end(compiler, result_reg);

    builtin.print("Success! Generated: " + output_filename);
    return 0;
}