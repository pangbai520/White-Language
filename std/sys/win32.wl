// std/sys/win32.wl
// Raw Windows OS Bindings

extern func GetProcessHeap() -> AnyPtr from "C";
extern func HeapAlloc(hHeap -> AnyPtr, dwFlags -> Int, dwBytes -> Long) -> AnyPtr from "C";
extern func HeapFree(hHeap -> AnyPtr, dwFlags -> Int, lpMem -> AnyPtr) -> Int from "C";

extern func GetStdHandle(nStdHandle -> Int) -> AnyPtr from "C";
extern func WriteFile(hFile -> AnyPtr, lpBuffer -> AnyPtr, nNumberOfBytesToWrite -> Int, lpNumberOfBytesWritten -> AnyPtr, lpOverlapped -> AnyPtr) -> Int from "C";
extern func SetConsoleOutputCP(wCodePageID -> Int) -> Int from "C";
extern func GetConsoleMode(hConsoleHandle -> AnyPtr, lpMode -> AnyPtr) -> Int from "C";
extern func WriteConsoleW(hConsoleOutput -> AnyPtr, lpBuffer -> AnyPtr, nNumberOfCharsToWrite -> Int, lpNumberOfCharsWritten -> AnyPtr, lpReserved -> AnyPtr) -> Int from "C";
extern func MultiByteToWideChar(CodePage -> Int, dwFlags -> Int, lpMultiByteStr -> String, cbMultiByte -> Int, lpWideCharStr -> AnyPtr, cchWideChar -> Int) -> Int from "C";

const STD_OUTPUT_HANDLE -> Int = -11;
const STD_ERROR_HANDLE -> Int = -12;
const CP_UTF8 -> Int = 65001;
