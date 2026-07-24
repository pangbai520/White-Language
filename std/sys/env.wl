// system environment access

import * from "target.wl"
import "internal/platform/windows"
import "internal/platform/posix"
import "internal/platform/errors" as platform_errors
import Error from "builtin/errors"

func get(name -> String) -> String? {
    if (name is null || name.length() == 0) { throw Error.InvalidArgument; }
    if (OS == "WINDOWS") {
        let wide_name -> AnyPtr = windows.utf8_to_utf16(name);
        if (wide_name is nullptr) { throw platform_errors.last(); }

        let required -> Int = windows.GetEnvironmentVariableW(wide_name, nullptr, 0);
        if (required <= 0) {
            let code -> Int = windows.GetLastError();
            windows.free_utf16(wide_name);
            if (code == windows.ERROR_ENVVAR_NOT_FOUND) { throw Error.NotFound; }
            throw platform_errors.from_windows(code);
        }

        let wide_value -> AnyPtr = windows.HeapAlloc(windows.GetProcessHeap(), windows.HEAP_ZERO_MEMORY, Long(required) * 2L);
        if (wide_value is nullptr) {
            windows.free_utf16(wide_name);
            throw Error.OutOfMemory;
        }

        let length -> Int = windows.GetEnvironmentVariableW(wide_name, wide_value, required);
        windows.free_utf16(wide_name);
        if (length <= 0 || length >= required) {
            let err -> Error = platform_errors.last();
            windows.free_utf16(wide_value);
            throw err;
        }

        let result -> String = windows.utf16_to_utf8(wide_value, length);
        windows.free_utf16(wide_value);
        if (result is null) { throw Error.OutOfMemory; }
        return result;
    }

    let result -> String = posix.wl_getenv(name);
    if (result is null) { throw Error.NotFound; }
    return result;
}

func get_env(name -> String) -> String {
    let result -> String = get(name)?;
    catch(err) {
        return null;
    }
    return result;
}
