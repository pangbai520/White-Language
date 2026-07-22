// compiler process hooks

import "sys"
import "internal/platform/windows"
import "internal/platform/posix"

@CompilerLink("process_exit")
func process_exit(status -> Int) -> Void {
    if (sys.OS == "WINDOWS") {
        windows.ExitProcess(status);
        return;
    }
    posix.wl_posix_exit(status);
}
