// src/wlc.wl
import "builtin"
import "sys"
import "process"
import "file"

// Core components
import "core/WhitelangTokens.wl"
import "core/WhitelangLexer.wl"
import "core/WhitelangNodes.wl"
import "core/WhitelangParser.wl"
import "core/WhitelangExceptions.wl"
import "core/WhitelangCompiler.wl"
import "core/WhitelangUtils.wl"


extern func get_arg(ptr argv -> String, idx -> Int) -> String from "C";

struct CompilerConfig(
    source_file     -> String,
    output_file     -> String,
    extra_ldflags   -> String,
    library_paths   -> Vector(String),
    extra_files     -> String,
    is_compile_only -> Bool,  // -c
    is_asm_only     -> Bool,  // -S
    is_emit_llvm    -> Bool,  // --emit-llvm
    debug_info      -> Bool,  // -g
    opt_level       -> String,
    verbose         -> Bool,
    dump_ast        -> Bool,
    dump_ir         -> Bool,
    keep_temps      -> Bool,
    is_shared       -> Bool
)

func print_usage() -> Void {
    builtin.print("White Language Compiler (v0.2.12)");
    builtin.print("Usage: wlc <source.wl> [extra_files...] [options]");
    builtin.print("");
    builtin.print("Arguments:");
    builtin.print("  <source.wl>            Primary WhiteLang source file");
    builtin.print("  [extra_files...]       Additional .wl, .c, or .obj files to compile/link");
    builtin.print("");
    builtin.print("Options:");
    builtin.print("  -o <file>              Write output to <file>");
    builtin.print("  -c                     Compile and assemble, but do not link");
    builtin.print("  -S                     Compile only; do not assemble or link");
    builtin.print("  --emit-llvm            Use the LLVM representation for assembler and object files");
    builtin.print("  -O<level>              Optimization level (0, 1, 2, 3). Default: 2");
    builtin.print("  -g                     Generate source-level debug information");
    builtin.print("  -L <dir>               Add <dir> to the linker library search path");
    builtin.print("  --library-path <dir>   Add <dir> to the linker library search path");
    builtin.print("  --ldflags <flags>      Pass extra flags to the linker (e.g., \"-lm -lpthread\")");
    builtin.print("  -v, --verbose          Enable verbose logging");
    builtin.print("  --dump-ast             Dump Abstract Syntax Tree to stdout");
    builtin.print("  --dump-ir              Dump LLVM IR to stdout");
    builtin.print("  --keep-temps           Do not delete intermediate LLVM IR files");
    builtin.print("  --shared               Build a shared library (dll, so, dylib)");
    builtin.print("  -h, --help             Display this information");
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
        source_file     = "",
        output_file     = "",
        extra_ldflags   = "",
        library_paths   = [],
        extra_files     = "",
        is_compile_only = false,
        is_asm_only     = false,
        is_emit_llvm    = false,
        debug_info      = false,
        opt_level       = "-O2",
        verbose         = false,
        dump_ast        = false,
        dump_ir         = false,
        keep_temps      = false,
        is_shared       = false
    );

    let i -> Int = 1;
    while (i < argc) {
        let arg -> String = get_arg(argv, i);

        if (arg == "-h" || arg == "--help") {
            print_usage();
            return 0;
        }
        else if (arg == "-v" || arg == "--verbose") { cfg.verbose = true; }
        else if (arg == "--dump-ast") { cfg.dump_ast = true; }
        else if (arg == "--dump-ir") { cfg.dump_ir = true; }
        else if (arg == "--keep-temps") { cfg.keep_temps = true; }
        else if (arg == "-c") { cfg.is_compile_only = true; }
        else if (arg == "-S") { cfg.is_asm_only = true; }
        else if (arg == "--shared") { cfg.is_shared = true; }
        else if (arg == "--emit-llvm") { cfg.is_emit_llvm = true; }
        else if (arg == "-g") { cfg.debug_info = true; }
        else if (arg == "-O0") { cfg.opt_level = "-O0"; }
        else if (arg == "-O1") { cfg.opt_level = "-O1"; }
        else if (arg == "-O2") { cfg.opt_level = "-O2"; }
        else if (arg == "-O3") { cfg.opt_level = "-O3"; }
        else if (arg == "-o") {
            i++;
            if (i >= argc) { builtin.print("Error: -o requires an argument"); return 1; }
            cfg.output_file = get_arg(argv, i);
        }
        else if (arg == "-L" || arg == "--library-path") {
            i++;
            if (i >= argc) { builtin.print("Error: " + arg + " requires an argument"); return 1; }
            let library_path -> String = get_arg(argv, i);
            if (library_path.length() == 0) { builtin.print("Error: Library search path cannot be empty"); return 1; }
            cfg.library_paths.append(library_path);
        }
        else if (arg.starts_with("-L") && arg.length() > 2) {
            cfg.library_paths.append(arg.slice(2, arg.length()));
        }
        else if (arg == "--ldflags") {
            i++;
            if (i >= argc) { builtin.print("Error: --ldflags requires an argument"); return 1; }
            cfg.extra_ldflags = cfg.extra_ldflags + " " + get_arg(argv, i);
        }
        else {
            if (cfg.source_file.length() == 0) {
                cfg.source_file = arg;
            } else {
                let linked_file -> String = "\"" + arg + "\"";
                if (cfg.extra_files.length() == 0) {
                    cfg.extra_files = linked_file;
                } else {
                    cfg.extra_files = cfg.extra_files + " " + linked_file;
                }
            }
        }
        i++;
    }

    if (cfg.source_file.length() == 0) {
        builtin.print("Error: No input file.");
        return 1;
    }

    let base_name -> String = get_base_name(cfg.source_file);
    let ll_file -> String = "";
    
    if (cfg.keep_temps || cfg.is_emit_llvm) {
        ll_file = base_name + ".ll";
    } else {
        let temp_dir -> String = "";
        
        if (sys.OS == "WINDOWS") {
            temp_dir = sys.env.get_env("TMP");
            if (temp_dir is null) { temp_dir = sys.env.get_env("TEMP"); }
            if (temp_dir is null) { temp_dir = "."; }
            
            if (!temp_dir.ends_with("\\") && !temp_dir.ends_with("/")) {
                temp_dir += "\\";
            }
        } else {
            temp_dir = "/tmp/";
        }

        let file_only -> String = base_name;
        let len -> Int = base_name.length();
        let idx -> Int = len - 1;
        while (idx >= 0) {
            let ch -> Char = base_name[idx];
            if (ch == '/' || ch == '\\') {
                file_only = base_name.slice(idx + 1, len);
                break;
            }
            idx -= 1;
        }

        ll_file = temp_dir + "wlc_tmp_" + file_only + ".ll";
    }

    if (!cfg.keep_temps && !cfg.is_emit_llvm) {
        WhitelangExceptions.CLEAN_TMP_LL = ll_file;
    }

    if (cfg.output_file.length() == 0) {
        if (cfg.is_asm_only) {
            if (cfg.is_emit_llvm) { cfg.output_file = base_name + ".ll"; }
            else { cfg.output_file = base_name + ".s"; }
        }
        else if (cfg.is_compile_only) {
            if (cfg.is_emit_llvm) { cfg.output_file = base_name + ".bc"; }
            else { 
                if (sys.OS == "WINDOWS") { cfg.output_file = base_name + ".obj"; }
                else { cfg.output_file = base_name + ".o"; }
            }
        }
        else if (cfg.is_shared) {
            if (sys.OS == "WINDOWS") { 
                cfg.output_file = base_name + ".dll"; 
            } else if (sys.OS == "MACOS") { 
                cfg.output_file = "lib" + base_name + ".dylib";
            } else { 
                cfg.output_file = "lib" + base_name + ".so"; 
            }
        }
        else { // EXE
            if (sys.OS == "WINDOWS") { cfg.output_file = base_name + ".exe"; }
            else { cfg.output_file = base_name; }
        }
    }

    log_stage(cfg, "Frontend & Middle-end");
    let f_in -> file.File = file.open(cfg.source_file);
    if (f_in is null || !f_in.is_open()) {
        builtin.print("Error: Could not open " + cfg.source_file);
        return 1;
    }
    let source -> String = f_in.read_all();
    f_in.close();

    let lexer -> WhitelangLexer.Lexer = WhitelangLexer.new_lexer(cfg.source_file, source);
    let parser -> WhitelangParser.Parser = WhitelangParser.Parser(lexer=lexer, current_tok=WhitelangLexer.get_next_token(lexer));
    let ast -> Struct = WhitelangParser.parse(parser);

    WhitelangExceptions.check_errors_and_abort();

    if (cfg.dump_ast) { builtin.print("[Debug] AST Dumped"); }

    let compiler -> WhitelangUtils.Compiler = WhitelangUtils.new_compiler(ll_file, cfg.is_shared);
    compiler.current_dir = WhitelangUtils.get_dir_name(cfg.source_file);
    WhitelangExceptions.ACTIVE_FILE = compiler.output_file;
    WhitelangCompiler.compile(compiler, ast);

    WhitelangExceptions.check_errors_and_abort();

    if (cfg.dump_ir) {
        let f_ir -> file.File = file.open(ll_file);
        let ir_content -> String = f_ir.read_all();
        f_ir.close();
        builtin.print(ir_content);
    }

    if (cfg.is_asm_only && cfg.is_emit_llvm) {
        if (cfg.output_file != ll_file) { builtin.print("Generated: " + ll_file); }
        return 0;
    }

    log_stage(cfg, "Backend/Linker");
    let clang_cmd -> String = "clang";
    if (sys.OS == "WINDOWS") {
        clang_cmd = "clang.exe";
    }

    let has_clang -> Bool = false;

    let wl_path -> String = sys.env.get_env("WL_PATH");
    if (wl_path is !null) {
        let portable_clang -> String = "";
        if (sys.OS == "WINDOWS") {
            portable_clang = wl_path + "/tools/llvm/bin/clang.exe";
        } else {
            portable_clang = wl_path + "/tools/llvm/bin/clang";
        }

        let probe -> file.File = file.open(portable_clang);
        if (probe is !null && probe.is_open()) {
            probe.close();
            clang_cmd = "\"" + portable_clang + "\"";
            has_clang = true;
            if (cfg.verbose) { builtin.print("Using portable LLVM: " + portable_clang); }
        } else {
            if (cfg.verbose) { builtin.print("Portable LLVM not found, falling back to system " + clang_cmd + "."); }
        }
    }

    if (!has_clang) {
        let check_ret -> Int = 1;
        if (sys.OS == "WINDOWS") {
            check_ret = process.shell("where " + clang_cmd + " >nul 2>nul");
        } else {
            check_ret = process.shell("which " + clang_cmd + " > /dev/null 2>&1");
        }

        if (check_ret == 0) {
            has_clang = true;
        }
    }

    if (!has_clang) {
        builtin.print("Error: Could not find C compiler ('clang').");
        builtin.print("Please ensure your WhiteLanguage installation is complete, or install 'clang' and add it to your system PATH.");
        return 1;
    }
    
    let cmd -> String = clang_cmd;
    if (cfg.debug_info) { cmd += " -g"; }
    
    cmd += " -Wno-override-module " + cfg.opt_level + " \"" + ll_file + "\"";

    if (cfg.extra_files.length() > 0) {
        cmd += " " + cfg.extra_files;
    }

    if (cfg.is_asm_only) {
        cmd += " -S";
        if (cfg.is_emit_llvm) { cmd += " -emit-llvm"; }
        cmd += " -o \"" + cfg.output_file + "\"";
    }
    else if (cfg.is_compile_only) {
        cmd += " -c";
        if (cfg.is_emit_llvm) { cmd += " -emit-llvm"; }
        cmd += " -o \"" + cfg.output_file + "\"";
    }
    else {
        if (cfg.is_shared) {
            if (sys.OS == "MACOS") {
                cmd += " -dynamiclib";
            } else {
                cmd += " -shared";
            }
            if (sys.OS != "WINDOWS") { cmd += " -fPIC"; }
        }

        let wl_path -> String = sys.env.get_env("WL_PATH");
        if (wl_path is !null) {
            if (sys.OS == "WINDOWS") {
                cmd += " \"" + wl_path + "/runtime/wl_runtime.obj\"";
                if (cfg.is_shared) {
                    cmd += " -nostdlib -Xlinker /entry:DllMainCRTStartup -lkernel32 -lshell32";
                } else {
                    cmd += " -nostdlib -Xlinker /entry:mainCRTStartup -Xlinker /subsystem:console -lkernel32 -lshell32";
                }
            } else {
                cmd += " \"" + wl_path + "/runtime/wl_runtime.o\"";
            }
        } else {
            builtin.print("Warning: WL_PATH environment variable is not set. Auto-linking of runtime skipped.");
        }

        let lib_idx -> Int = 0;
        let path_idx -> Int = 0;
        while (path_idx < cfg.library_paths.length()) {
            cmd += " -L \"" + cfg.library_paths[path_idx] + "\"";
            path_idx += 1;
        }

        while (lib_idx < compiler.extra_libs.length()) {
            cmd += " -l" + compiler.extra_libs[lib_idx];
            lib_idx += 1;
        }

        if (cfg.extra_ldflags.length() > 0) {
            cmd += " " + cfg.extra_ldflags;
        }

        cmd += " -o \"" + cfg.output_file + "\"";

        if (sys.OS != "WINDOWS") {
            cmd += " -lm -lc";
        }
    }

    if (cfg.verbose) { builtin.print("Command: " + cmd); }
    let ret -> Int = process.shell(cmd);

    if (!cfg.keep_temps && cfg.output_file != ll_file) {
        if (cfg.verbose) { builtin.print("Cleaning up: " + ll_file); }
        file.remove(ll_file);
        WhitelangExceptions.CLEAN_TMP_LL = "";
    }

    if (ret != 0) {
        builtin.print("Build Failed (Clang exit code: " + ret + ")");
        return ret;
    }

    builtin.print("Build success: " + cfg.output_file);
    return 0;
}
