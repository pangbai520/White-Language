// tools/wlls/wlls.wl
import "builtin"
import "internal/server/_pkg.wl" as server

extern func get_arg(ptr argv -> String, idx -> Int) -> String from "C";

func print_usage() -> Void {
    builtin.print("White Language Language Server");
    builtin.print("Usage: wlls --stdio");
}

func main(argc -> Int, ptr argv -> String) -> Int {
    if (argc < 2) {
        print_usage();
        return 1;
    }

    let option -> String = get_arg(argv, 1);
    if (option == "-h" || option == "--help") {
        print_usage();
        return 0;
    }
    if (option != "--stdio" || argc != 2) {
        print_usage();
        return 1;
    }

    let status -> Int = server.run()?;
    catch(err) {
        return 1;
    }
    return status;
}
