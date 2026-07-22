// system environment access

import * from "target.wl"
import "internal/platform/windows"
import "internal/platform/posix"

func get_env(name -> String) -> String {
    if (OS == "WINDOWS") {
        let wide_name -> AnyPtr = windows.utf8_to_utf16(name);
        if (wide_name is nullptr) { return null; }

        let required -> Int = windows.GetEnvironmentVariableW(wide_name, nullptr, 0);
        if (required <= 0) {
            windows.free_utf16(wide_name);
            return null;
        }

        let wide_value -> AnyPtr = windows.HeapAlloc(windows.GetProcessHeap(), windows.HEAP_ZERO_MEMORY, Long(required) * 2L);
        if (wide_value is nullptr) {
            windows.free_utf16(wide_name);
            return null;
        }

        let length -> Int = windows.GetEnvironmentVariableW(wide_name, wide_value, required);
        windows.free_utf16(wide_name);
        if (length <= 0 || length >= required) {
            windows.free_utf16(wide_value);
            return null;
        }

        let result -> String = windows.utf16_to_utf8(wide_value, length);
        windows.free_utf16(wide_value);
        return result;
    }
    return posix.wl_getenv(name);
}
