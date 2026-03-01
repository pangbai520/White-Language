// src/wlc.wl
import "builtin"
import "file_io"

// Core components
import "core/WhitelangTokens.wl"
import "core/WhitelangLexer.wl"
import "core/WhitelangNodes.wl"
import "core/WhitelangParser.wl"
import "core/WhitelangCompiler.wl"


extern func get_arg(ptr argv -> String, idx -> Int) -> String from "C";
extern func system_call(cmd -> String) -> Int from "C";
extern func is_windows() -> Int from "C";
extern func remove_file(path -> String) -> Int from "C";
extern func wl_getenv(name -> String) -> String from "C";

const EMIT_EXE  -> Int = 0;
const EMIT_OBJ  -> Int = 1;
const EMIT_ASM  -> Int = 2;
const EMIT_LLVM -> Int = 3;
const EMIT_BC   -> Int = 4;

struct CompilerConfig(
    source_file -> String,
    output_file -> String,
    extern_file -> String,
    emit_mode   -> Int,
    opt_level   -> String,
    verbose     -> Bool,
    dump_ast    -> Bool,
    dump_ir     -> Bool,
    keep_temps  -> Bool
)

func print_usage() -> Void {
    builtin.print("WhiteLang Compiler Driver (v1.0 Industrial)");
    builtin.print("Usage: wlc <source.wl> [options]");
    builtin.print("");
    builtin.print("Options:");
    builtin.print("  -o <file>             Specify output filename");
    builtin.print("  --extern <file>       Link an extra external C file (optional)");
    builtin.print("  -O<level>             Optimization level (0, 1, 2, 3). Default: 2");
    builtin.print("  --emit <type>         Output format: exe, obj, asm, llvm, bc");
    builtin.print("  -v, --verbose         Enable verbose logging");
    builtin.print("  --dump-ast            Dump Abstract Syntax Tree to stdout");
    builtin.print("  --dump-ir             Dump LLVM IR to stdout");
    builtin.print("  -h, --help            Show this help message");
}

func log_stage(cfg -> CompilerConfig, name -> String) -> Void {
    if (cfg.verbose) {
        builtin.print("");
        builtin.print("[Stage: " + name + "] ------------------------------");
    }
}

func get_base_name(path -> String) -> String {
    let len -> Int = path.length();
    if (path.ends_with(".wl")) {
        return path.slice(0, len - 3);
    }
    return path;
}

func main(argc -> Int, ptr argv -> String) -> Int {
    if (argc < 2) {
        print_usage();
        return 1;
    }

    let cfg -> CompilerConfig = CompilerConfig(
        source_file = "",
        output_file = "",
        extern_file = null,
        emit_mode   = EMIT_EXE,
        opt_level   = "-O2",
        verbose     = false,
        dump_ast    = false,
        dump_ir     = false,
        keep_temps  = false
    );

    let i -> Int = 1;
    while (i < argc) {
        let arg -> String = get_arg(argv, i);

        if (arg == "-h" || arg == "--help") {
            print_usage();
            return 0;
        }
        else if (arg == "-v" || arg == "--verbose") {
            cfg.verbose = true;
        }
        else if (arg == "--dump-ast") {
            cfg.dump_ast = true;
        }
        else if (arg == "--dump-ir") {
            cfg.dump_ir = true;
        }
        else if (arg == "-o") {
            i++;
            if (i >= argc) { builtin.print("Error: -o requires an argument"); return 1; }
            cfg.output_file = get_arg(argv, i);
        }
        else if (arg == "--extern") {
            i++;
            if (i >= argc) { builtin.print("Error: --extern requires an argument"); return 1; }
            cfg.extern_file = get_arg(argv, i);
        }
        else if (arg == "-O0") { cfg.opt_level = "-O0"; }
        else if (arg == "-O1") { cfg.opt_level = "-O1"; }
        else if (arg == "-O2") { cfg.opt_level = "-O2"; }
        else if (arg == "-O3") { cfg.opt_level = "-O3"; }
        else if (arg == "--emit") {
            i++;
            if (i >= argc) { builtin.print("Error: --emit requires (exe|obj|asm|llvm|bc)"); return 1; }
            let val -> String = get_arg(argv, i);
            if (val == "exe") { cfg.emit_mode = EMIT_EXE; }
            else if (val == "obj") { cfg.emit_mode = EMIT_OBJ; }
            else if (val == "asm") { cfg.emit_mode = EMIT_ASM; }
            else if (val == "llvm") { cfg.emit_mode = EMIT_LLVM; }
            else if (val == "bc") { cfg.emit_mode = EMIT_BC; }
            else { builtin.print("Error: Unknown emit format: " + val); return 1; }
        }
        else {
            if (cfg.source_file.length() == 0) {
                cfg.source_file = arg;
            } else {
                builtin.print("Warning: Ignoring extra argument: " + arg);
            }
        }
        i++;
    }

    if (cfg.source_file.length() == 0) {
        builtin.print("Error: No input file.");
        return 1;
    }

    let base_name -> String = get_base_name(cfg.source_file);
    let ll_file -> String = base_name + ".ll";

    if (cfg.output_file.length() == 0) {
        if (cfg.emit_mode == EMIT_LLVM) { cfg.output_file = base_name + ".ll"; }
        else if (cfg.emit_mode == EMIT_BC) { cfg.output_file = base_name + ".bc"; }
        else if (cfg.emit_mode == EMIT_ASM) { cfg.output_file = base_name + ".s"; }
        else if (cfg.emit_mode == EMIT_OBJ) { 
            if (is_windows() == 1) { cfg.output_file = base_name + ".obj"; }
            else { cfg.output_file = base_name + ".o"; }
        }
        else { // EXE
            if (is_windows() == 1) { cfg.output_file = base_name + ".exe"; }
            else { cfg.output_file = base_name; }
        }
    }

    log_stage(cfg, "Frontend & Middle-end");
    let f_in -> File = open(cfg.source_file, "rb");
    if (f_in is null) {
        builtin.print("Error: Could not open " + cfg.source_file);
        return 1;
    }
    let source -> String = read_all(f_in);
    close(f_in);

    let lexer -> Lexer = new_lexer(cfg.source_file, source);
    let parser -> Parser = Parser(lexer=lexer, current_tok=get_next_token(lexer));
    let ast -> Struct = parse(parser);

    if (cfg.dump_ast) { builtin.print("[Debug] AST Dumped"); }

    let compiler -> Compiler = new_compiler(ll_file);
    compiler.current_dir = get_dir_name(cfg.source_file);
    compile(compiler, ast);

    if (cfg.dump_ir) {
        let f_ir -> File = open(ll_file, "rb");
        let ir_content -> String = read_all(f_ir);
        close(f_ir);
        builtin.print(ir_content);
    }

    if (cfg.emit_mode == EMIT_LLVM) {
        if (cfg.output_file != ll_file) { builtin.print("Generated: " + ll_file); }
        return 0;
    }

    log_stage(cfg, "Backend/Linker");
    let wl_path -> String = wl_getenv("WL_PATH");
    let clang_cmd -> String = "clang";

    if (wl_path is !null) {
        if (is_windows() == 1) {
            clang_cmd = wl_path + "/tools/llvm/bin/clang.exe";
        } else {
            clang_cmd = wl_path + "/tools/llvm/bin/clang";
        }
    }
    
    let cmd -> String = "\"" + clang_cmd + "\" \"" + ll_file + "\"";

    if (cfg.emit_mode == EMIT_ASM) {
        cmd = cmd + " -S -o \"" + cfg.output_file + "\" " + cfg.opt_level + " -Wno-override-module";
    }
    else if (cfg.emit_mode == EMIT_OBJ) {
        cmd = cmd + " -c -o \"" + cfg.output_file + "\" " + cfg.opt_level + " -Wno-override-module";
    }
    else if (cfg.emit_mode == EMIT_BC) {
        cmd = cmd + " -emit-llvm -c -o \"" + cfg.output_file + "\" " + cfg.opt_level + " -Wno-override-module";
    }
    else {
        let wl_path -> String = wl_getenv("WL_PATH");
        if (wl_path is !null) {
            cmd = cmd + " \"" + wl_path + "/runtime/wl_runtime.c\"";
        } else {
            builtin.print("Warning: WL_PATH environment variable is not set. Auto-linking of runtime skipped.");
        }

        if (cfg.extern_file is !null) {
            cmd = cmd + " \"" + cfg.extern_file + "\"";
        }

        cmd = cmd + " -o \"" + cfg.output_file + "\" " + cfg.opt_level + " -Wno-override-module";

        if (is_windows() == 0) {
            cmd = cmd + " -lm -lc";
        }
    }

    if (cfg.verbose) { builtin.print("Command: " + cmd); }
    if (is_windows() == 1) { cmd = "\"" + cmd + "\""; }
    
    let ret -> Int = system_call(cmd);

    if (!cfg.keep_temps) {
        if (cfg.verbose) { builtin.print("Cleaning up: " + ll_file); }
        remove_file(ll_file);
    }

    if (ret != 0) {
        builtin.print("Build Failed (Clang exit code: " + ret + ")");
        return ret;
    }

    builtin.print("Build success: " + cfg.output_file);
    return 0;
}