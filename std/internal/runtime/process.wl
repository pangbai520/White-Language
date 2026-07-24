// compiler process hooks

import "sys"
import "internal/platform/windows"
import "internal/platform/posix"
import "string.wl" as runtime_string

func write_err(message -> String) -> Void {
    if (message is null || message.length() == 0) { return; }
    if (sys.OS == "WINDOWS") {
        let handle -> AnyPtr = windows.GetStdHandle(windows.STD_ERROR_HANDLE);
        if (!windows.is_invalid_handle(handle)) {
            let written -> Int = 0;
            windows.WriteFile(handle, runtime_string.data(message), message.length(), ref written, nullptr);
        }
        return;
    }
    posix.write(2, runtime_string.data(message), Long(message.length()));
}

func panic(message -> String) -> Void {
    write_err("fatal: " + message + "\n");
    process_exit(1);
}

func panic_capacity_overflow(owner -> String) -> Void {
    panic(owner + " capacity overflow");
}

func panic_out_of_memory(owner -> String) -> Void {
    panic(owner + " allocation failed: out of memory");
}

@CompilerLink("process_exit")
func process_exit(status -> Int) -> Void {
    if (sys.OS == "WINDOWS") {
        windows.ExitProcess(status);
        return;
    }
    posix.wl_posix_exit(status);
}
