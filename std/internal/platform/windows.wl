// raw windows bindings

import * from "../../sys/target.wl"

extern func GetProcessHeap() -> AnyPtr from "C";
extern func HeapAlloc(hHeap -> AnyPtr, dwFlags -> Int, dwBytes -> Long) -> AnyPtr from "C";
extern func HeapReAlloc(hHeap -> AnyPtr, dwFlags -> Int, lpMem -> AnyPtr, dwBytes -> Long) -> AnyPtr from "C";
extern func HeapFree(hHeap -> AnyPtr, dwFlags -> Int, lpMem -> AnyPtr) -> Int from "C";
extern func GetLastError() -> Int from "C";

extern func GetEnvironmentVariableW(lpName -> AnyPtr, lpBuffer -> AnyPtr, nSize -> Int) -> Int from "C";

extern func CreateProcessW(lpApplicationName -> AnyPtr, lpCommandLine -> AnyPtr, lpProcessAttributes -> AnyPtr, lpThreadAttributes -> AnyPtr, bInheritHandles -> Int, dwCreationFlags -> Int, lpEnvironment -> AnyPtr, lpCurrentDirectory -> AnyPtr, lpStartupInfo -> AnyPtr, lpProcessInformation -> AnyPtr) -> Int from "C";
extern func WaitForSingleObject(hHandle -> AnyPtr, dwMilliseconds -> Int) -> Int from "C";
extern func GetExitCodeProcess(hProcess -> AnyPtr, lpExitCode -> AnyPtr) -> Int from "C";
extern func CloseHandle(hObject -> AnyPtr) -> Int from "C";

extern func CreateFileW(lpFileName -> AnyPtr, dwDesiredAccess -> Int, dwShareMode -> Int, lpSecurityAttributes -> AnyPtr, dwCreationDisposition -> Int, dwFlagsAndAttributes -> Int, hTemplateFile -> AnyPtr) -> AnyPtr from "C";
extern func GetFileSizeEx(hFile -> AnyPtr, lpFileSize -> AnyPtr) -> Int from "C";
extern func ReadFile(hFile -> AnyPtr, lpBuffer -> AnyPtr, nNumberOfBytesToRead -> Int, lpNumberOfBytesRead -> AnyPtr, lpOverlapped -> AnyPtr) -> Int from "C";
extern func WriteFile(hFile -> AnyPtr, lpBuffer -> AnyPtr, nNumberOfBytesToWrite -> Int, lpNumberOfBytesWritten -> AnyPtr, lpOverlapped -> AnyPtr) -> Int from "C";
extern func SetFilePointerEx(hFile -> AnyPtr, liDistanceToMove -> Long, lpNewFilePointer -> AnyPtr, dwMoveMethod -> Int) -> Int from "C";
extern func DeleteFileW(lpFileName -> AnyPtr) -> Int from "C";
extern func ExitProcess(uExitCode -> Int) -> Void from "C";

extern func GetStdHandle(nStdHandle -> Int) -> AnyPtr from "C";
extern func SetConsoleOutputCP(wCodePageID -> Int) -> Int from "C";
extern func GetConsoleMode(hConsoleHandle -> AnyPtr, lpMode -> AnyPtr) -> Int from "C";
extern func WriteConsoleW(hConsoleOutput -> AnyPtr, lpBuffer -> AnyPtr, nNumberOfCharsToWrite -> Int, lpNumberOfCharsWritten -> AnyPtr, lpReserved -> AnyPtr) -> Int from "C";
extern func MultiByteToWideChar(CodePage -> Int, dwFlags -> Int, lpMultiByteStr -> AnyPtr, cbMultiByte -> Int, lpWideCharStr -> AnyPtr, cchWideChar -> Int) -> Int from "C";
extern func WideCharToMultiByte(CodePage -> Int, dwFlags -> Int, lpWideCharStr -> AnyPtr, cchWideChar -> Int, lpMultiByteStr -> AnyPtr, cbMultiByte -> Int, lpDefaultChar -> AnyPtr, lpUsedDefaultChar -> AnyPtr) -> Int from "C";

extern func wl_string_data(value -> String) -> AnyPtr from "C";
extern func wl_alloc_string(size -> Long) -> String from "C";

const STD_OUTPUT_HANDLE -> Int = -11;
const STD_ERROR_HANDLE -> Int = -12;
const CP_UTF8 -> Int = 65001;
const HEAP_ZERO_MEMORY -> Int = 8;
const ERROR_ENVVAR_NOT_FOUND -> Int = 203;

const INFINITE -> Int = -1;
const WAIT_FAILED -> Int = -1;

const GENERIC_READ -> Int = -2147483647 - 1;
const GENERIC_WRITE -> Int = 1073741824;
const FILE_SHARE_READ -> Int = 1;
const FILE_SHARE_WRITE -> Int = 2;
const FILE_SHARE_DELETE -> Int = 4;
const CREATE_ALWAYS -> Int = 2;
const OPEN_EXISTING -> Int = 3;
const OPEN_ALWAYS -> Int = 4;
const FILE_ATTRIBUTE_NORMAL -> Int = 128;
const FILE_BEGIN -> Int = 0;
const FILE_CURRENT -> Int = 1;
const FILE_END -> Int = 2;

func is_invalid_handle(handle -> AnyPtr) -> Bool {
    return handle is nullptr || Long(handle) == -1L;
}

func utf8_to_utf16(value -> String) -> AnyPtr {
    if (OS != "WINDOWS") { return nullptr; }
    if (value is null) { return nullptr; }

    let length -> Int = value.length();
    let wide_length -> Int = 0;
    if (length > 0) {
        wide_length = MultiByteToWideChar(CP_UTF8, 0, wl_string_data(value), length, nullptr, 0);
        if (wide_length <= 0) { return nullptr; }
    }

    let buffer -> AnyPtr = HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, (Long(wide_length) + 1L) * 2L);
    if (buffer is nullptr) { return nullptr; }
    if (wide_length > 0 && MultiByteToWideChar(CP_UTF8, 0, wl_string_data(value), length, buffer, wide_length) <= 0) {
        HeapFree(GetProcessHeap(), 0, buffer);
        return nullptr;
    }
    return buffer;
}

func utf16_to_utf8(value -> AnyPtr, length -> Int) -> String {
    if (OS != "WINDOWS") { return null; }
    if (value is nullptr || length < 0) { return null; }
    if (length == 0) { return wl_alloc_string(0L); }

    let utf8_length -> Int = WideCharToMultiByte(CP_UTF8, 0, value, length, nullptr, 0, nullptr, nullptr);
    if (utf8_length <= 0) { return null; }

    let result -> String = wl_alloc_string(Long(utf8_length));
    if (result is null) { return null; }
    if (WideCharToMultiByte(CP_UTF8, 0, value, length, wl_string_data(result), utf8_length, nullptr, nullptr) <= 0) {
        return null;
    }
    return result;
}

func free_utf16(value -> AnyPtr) -> Void {
    if (OS != "WINDOWS") { return; }
    if (value is !nullptr) { HeapFree(GetProcessHeap(), 0, value); }
}
