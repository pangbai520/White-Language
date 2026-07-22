// process control

import "sys"
import "internal/platform/windows"
import "internal/platform/posix"
import "internal/runtime"

func shell(command -> String) -> Int {
    if (sys.OS == "WINDOWS") {
        if (command is null || command.length() == 0) { return -1; }

        // use cmd.exe so redirection and built-ins retain shell behavior
        let shell_command -> String = "cmd.exe /d /s /c \"" + command + "\"";
        let wide_command -> AnyPtr = windows.utf8_to_utf16(shell_command);
        if (wide_command is nullptr) { return -1; }

        // these are the x64 layouts of STARTUPINFOW and PROCESS_INFORMATION
        let startup_info -> AnyPtr = windows.HeapAlloc(windows.GetProcessHeap(), windows.HEAP_ZERO_MEMORY, 104L);
        let process_info -> AnyPtr = windows.HeapAlloc(windows.GetProcessHeap(), windows.HEAP_ZERO_MEMORY, 24L);
        if (startup_info is nullptr || process_info is nullptr) {
            if (startup_info is !nullptr) { windows.HeapFree(windows.GetProcessHeap(), 0, startup_info); }
            if (process_info is !nullptr) { windows.HeapFree(windows.GetProcessHeap(), 0, process_info); }
            windows.free_utf16(wide_command);
            return -1;
        }

        let ptr startup_words -> Int = startup_info;
        startup_words[0] = 104;

        let created -> Int = windows.CreateProcessW(nullptr, wide_command, nullptr, nullptr, 0, 0, nullptr, nullptr, startup_info, process_info);
        windows.free_utf16(wide_command);
        if (created == 0) {
            windows.HeapFree(windows.GetProcessHeap(), 0, startup_info);
            windows.HeapFree(windows.GetProcessHeap(), 0, process_info);
            return -1;
        }

        let ptr handles -> AnyPtr = process_info;
        let process_handle -> AnyPtr = handles[0];
        let thread_handle -> AnyPtr = handles[1];
        let exit_code -> Int = -1;

        if (windows.WaitForSingleObject(process_handle, windows.INFINITE) != windows.WAIT_FAILED) {
            if (windows.GetExitCodeProcess(process_handle, ref exit_code) == 0) { exit_code = -1; }
        }

        windows.CloseHandle(thread_handle);
        windows.CloseHandle(process_handle);
        windows.HeapFree(windows.GetProcessHeap(), 0, startup_info);
        windows.HeapFree(windows.GetProcessHeap(), 0, process_info);
        return exit_code;
    }
    return posix.system_call(command);
}

func exit(status -> Int) -> Void {
    runtime.process_exit(status);
}
