// runtime/wl_runtime.c
#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <shellapi.h>

// abi hooks required by msvc- and mingw-targeted objects
int _fltused = 0x9875;
void __main(void) {}

__attribute__((used, noinline)) void* memcpy(void* dest, const void* src, size_t count) {
    volatile unsigned char* output = (volatile unsigned char*)dest;
    const volatile unsigned char* input = (const volatile unsigned char*)src;
    for (size_t i = 0; i < count; ++i) output[i] = input[i];
    return dest;
}

__attribute__((used, noinline)) void* memmove(void* dest, const void* src, size_t count) {
    volatile unsigned char* output = (volatile unsigned char*)dest;
    const volatile unsigned char* input = (const volatile unsigned char*)src;
    const uintptr_t output_addr = (uintptr_t)dest;
    const uintptr_t input_addr = (uintptr_t)src;

    if (output_addr == input_addr || count == 0) return dest;
    if (output_addr < input_addr || output_addr - input_addr >= count) {
        for (size_t i = 0; i < count; ++i) output[i] = input[i];
        return dest;
    }
    for (size_t i = count; i > 0; --i) output[i - 1] = input[i - 1];
    return dest;
}

__attribute__((used, noinline)) void* memset(void* dest, int value, size_t count) {
    volatile unsigned char* output = (volatile unsigned char*)dest;
    const unsigned char byte = (unsigned char)value;
    for (size_t i = 0; i < count; ++i) output[i] = byte;
    return dest;
}

#if defined(__x86_64__) || defined(_M_X64)
// probe each guard page before a large stack allocation
__attribute__((naked, weak)) void ___chkstk_ms(void) {
    __asm__ volatile(
        "pushq %rcx\n\t"
        "pushq %rax\n\t"
        "cmpq $0x1000, %rax\n\t"
        "leaq 24(%rsp), %rcx\n\t"
        "jb 2f\n\t"
        "1:\n\t"
        "subq $0x1000, %rcx\n\t"
        "testb $0, (%rcx)\n\t"
        "subq $0x1000, %rax\n\t"
        "cmpq $0x1000, %rax\n\t"
        "ja 1b\n\t"
        "2:\n\t"
        "subq %rax, %rcx\n\t"
        "testb $0, (%rcx)\n\t"
        "popq %rax\n\t"
        "popq %rcx\n\t"
        "retq\n\t"
    );
}

__attribute__((naked, weak)) void __chkstk(void) {
    __asm__ volatile("jmp ___chkstk_ms");
}
#endif

__attribute__((weak)) BOOL WINAPI DllMainCRTStartup(HINSTANCE instance, DWORD reason, LPVOID reserved) {
    (void)instance;
    (void)reason;
    (void)reserved;
    return TRUE;
}

// x64 ignores argc and argv when the emitted main does not declare them
extern int main(int argc, char** argv) __attribute__((weak));

__declspec(noreturn) __attribute__((weak)) void mainCRTStartup(void) {
    int argc = 0;
    wchar_t** wide_argv = CommandLineToArgvW(GetCommandLineW(), &argc);
    if (wide_argv == NULL || argc < 0) {
        ExitProcess(127);
    }

    HANDLE heap = GetProcessHeap();
    char** argv = (char**)HeapAlloc(heap, HEAP_ZERO_MEMORY, ((SIZE_T)argc + 1u) * sizeof(char*));
    if (argv == NULL) {
        LocalFree(wide_argv);
        ExitProcess(127);
    }

    for (int i = 0; i < argc; ++i) {
        int bytes = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide_argv[i], -1, NULL, 0, NULL, NULL);
        if (bytes <= 0) {
            for (int j = 0; j < i; ++j) HeapFree(heap, 0, argv[j]);
            HeapFree(heap, 0, argv);
            LocalFree(wide_argv);
            ExitProcess(127);
        }

        argv[i] = (char*)HeapAlloc(heap, 0, (SIZE_T)bytes);
        if (argv[i] == NULL || WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide_argv[i], -1, argv[i], bytes, NULL, NULL) <= 0) {
            if (argv[i] != NULL) HeapFree(heap, 0, argv[i]);
            for (int j = 0; j < i; ++j) HeapFree(heap, 0, argv[j]);
            HeapFree(heap, 0, argv);
            LocalFree(wide_argv);
            ExitProcess(127);
        }
    }

    LocalFree(wide_argv);
    if (main == NULL) {
        HeapFree(heap, 0, argv);
        ExitProcess(127);
    }
    int status = main(argc, argv);

    for (int i = 0; i < argc; ++i) HeapFree(heap, 0, argv[i]);
    HeapFree(heap, 0, argv);
    ExitProcess((UINT)status);
}
#endif
