// runtime/wl_runtime.c
#include <limits.h>
#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <shellapi.h>

// abi hooks required by msvc- and mingw-targeted objects
int _fltused = 0x9875;
void __main(void) {}

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
        int bytes = WideCharToMultiByte(CP_UTF8, 0, wide_argv[i], -1, NULL, 0, NULL, NULL);
        if (bytes <= 0) {
            for (int j = 0; j < i; ++j) HeapFree(heap, 0, argv[j]);
            HeapFree(heap, 0, argv);
            LocalFree(wide_argv);
            ExitProcess(127);
        }

        argv[i] = (char*)HeapAlloc(heap, 0, (SIZE_T)bytes);
        if (argv[i] == NULL || WideCharToMultiByte(CP_UTF8, 0, wide_argv[i], -1, argv[i], bytes, NULL, NULL) <= 0) {
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
#else
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif

// keep this layout in sync with %struct.$String
typedef struct {
    char* buf;
    int len;
    int cap;
} wl_string;

enum {
    WL_STRING_TYPE_ID = 5,
    WL_OBJECT_HEADER_SIZE = 8
};

// c strings are scanned only at native boundaries such as argv and getenv
static size_t wl_cstr_len(const char* text) {
    if (text == NULL) return 0;
    const volatile unsigned char* cursor = (const volatile unsigned char*)text;
    size_t len = 0;
    while (cursor[len] != 0) ++len;
    return len;
}

static wl_string* wl_alloc_string_storage(size_t capacity) {
    if (capacity > INT_MAX || capacity > SIZE_MAX - WL_OBJECT_HEADER_SIZE - sizeof(wl_string) - 1) {
        return NULL;
    }

    const size_t total_size = WL_OBJECT_HEADER_SIZE + sizeof(wl_string) + capacity + 1;
    unsigned char* mem;
#ifdef _WIN32
    mem = (unsigned char*)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, total_size);
#else
    mem = (unsigned char*)malloc(total_size);
#endif
    if (mem == NULL) {
        return NULL;
    }

    int* rc = (int*)mem;
    int* type_id = (int*)(mem + sizeof(int));
    wl_string* str = (wl_string*)(mem + WL_OBJECT_HEADER_SIZE);

    *rc = 0;
    *type_id = WL_STRING_TYPE_ID;
    str->buf = (char*)(mem + WL_OBJECT_HEADER_SIZE + sizeof(wl_string));
    str->len = (int)capacity;
    str->cap = (int)capacity;
#ifndef _WIN32
    memset(str->buf, 0, capacity + 1);
#endif
    return str;
}

int is_windows() {
    #ifdef _WIN32
        return 1;
    #else
        return 0;
    #endif
}

int is_macos() {
    #ifdef __APPLE__
        return 1;
    #else
        return 0;
    #endif
}

wl_string* to_wl_str(const char* c_str) {
    if (c_str == NULL) return NULL;
    const size_t len = wl_cstr_len(c_str);
    wl_string* wl_str = wl_alloc_string_storage(len);
    if (wl_str == NULL) return NULL;
    volatile char* dest = (volatile char*)wl_str->buf;
    const volatile char* source = (const volatile char*)c_str;
    for (size_t i = 0; i <= len; ++i) dest[i] = source[i];
    return wl_str;
}

wl_string* get_arg(char** argv, int idx) {
    if (argv == NULL) return NULL;
    return to_wl_str(argv[idx]);
}

#ifndef _WIN32
int system_call(wl_string* cmd) {
    if (cmd == NULL || cmd->buf == NULL) return -1;
    return system(cmd->buf);
}

wl_string* wl_getenv(wl_string* name) {
    if (name == NULL || name->buf == NULL) return NULL;
    char* val = getenv(name->buf);
    if (val == NULL) return NULL;
    return to_wl_str(val);
}

void wl_posix_exit(int status) {
    exit(status);
}
#endif

void __wl_str_set(wl_string* s, int idx, int val) {
    if (s && s->buf && idx >= 0 && idx < s->cap) {
        s->buf[idx] = (char)val;
    }
}

char __wl_str_get(wl_string* s, int idx) {
    if (s && s->buf && idx >= 0 && idx < s->len) {
        return s->buf[idx];
    }
    return 0;
}

void* wl_string_data(wl_string* s) {
    if (s == NULL) return NULL;
    return s->buf;
}

void wl_string_set_length(wl_string* s, int length) {
    if (s == NULL || s->buf == NULL || length < 0 || length > s->cap) return;
    s->len = length;
    s->buf[length] = '\0';
}

wl_string* wl_alloc_string(long long size) {
// allocate an empty string with inline storage
    if (size < 0) return NULL;
    return wl_alloc_string_storage((size_t)size);
}

wl_string* wl_string_concat(wl_string* left, wl_string* right) {
    if (left == NULL || right == NULL || left->buf == NULL || right->buf == NULL) return NULL;
    if (left->len < 0 || right->len < 0 || left->len > INT_MAX - right->len) return NULL;

    const int length = left->len + right->len;
    wl_string* result = wl_alloc_string_storage((size_t)length);
    if (result == NULL) return NULL;

    int i = 0;
    while (i < left->len) {
        result->buf[i] = left->buf[i];
        ++i;
    }
    int j = 0;
    while (j < right->len) {
        result->buf[left->len + j] = right->buf[j];
        ++j;
    }
    return result;
}

int wl_string_compare(wl_string* left, wl_string* right) {
    if (left == right) return 0;
    if (left == NULL || left->buf == NULL) return -1;
    if (right == NULL || right->buf == NULL) return 1;

    const int common_length = left->len < right->len ? left->len : right->len;
    int i = 0;
    while (i < common_length) {
        const unsigned char lhs = (unsigned char)left->buf[i];
        const unsigned char rhs = (unsigned char)right->buf[i];
        if (lhs < rhs) return -1;
        if (lhs > rhs) return 1;
        ++i;
    }
    if (left->len < right->len) return -1;
    if (left->len > right->len) return 1;
    return 0;
}

#ifndef _WIN32
void* wl_fopen(wl_string* filename, wl_string* mode) {
    if (filename == NULL || filename->buf == NULL || mode == NULL || mode->buf == NULL) {
        return NULL;
    }
    return fopen(filename->buf, mode->buf);
}

long long wl_fread(wl_string* dest, long long size, long long count, void* stream) {
    if (dest == NULL || dest->buf == NULL || stream == NULL || size < 0 || count < 0) {
        return 0;
    }
    if (size == 0 || count == 0) {
        dest->len = 0;
        if (dest->cap >= 0) dest->buf[0] = '\0';
        return 0;
    }

    const size_t item_size = (size_t)size;
    size_t item_count = (size_t)count;
    const size_t max_count = (size_t)dest->cap / item_size;
    if (item_count > max_count) item_count = max_count;

    const size_t items_read = fread(dest->buf, item_size, item_count, (FILE*)stream);
    const size_t bytes_read = items_read * item_size;
    dest->len = (int)bytes_read;
    dest->buf[bytes_read] = '\0';
    return (long long)items_read;
}

long long wl_fwrite(wl_string* src, long long size, long long count, void* stream) {
    if (src == NULL || src->buf == NULL || stream == NULL || size < 0 || count < 0) {
        return 0;
    }
    if (size == 0 || count == 0) return 0;

    const size_t item_size = (size_t)size;
    size_t item_count = (size_t)count;
    const size_t max_count = (size_t)src->len / item_size;
    if (item_count > max_count) item_count = max_count;
    return (long long)fwrite(src->buf, item_size, item_count, (FILE*)stream);
}

int wl_remove(wl_string* filename) {
    if (filename == NULL || filename->buf == NULL) return -1;
    return remove(filename->buf);
}
#endif

int wl_format_i128(char* buf, unsigned long long low, long long high) {
    __int128 val = (__int128)(((unsigned __int128)(unsigned long long)high << 64) | low);
    if (val == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return 1;
    }
    int is_neg = 0;
    unsigned __int128 uval;
    if (val < 0) {
        is_neg = 1;
        // negate after conversion so INT128_MIN stays defined
        uval = (unsigned __int128)0 - (unsigned __int128)val;
    } else {
        uval = (unsigned __int128)val;
    }
    
    char temp[64];
    int pos = 0;
    while (uval > 0) {
        temp[pos++] = (char)((uval % 10) + '0');
        uval /= 10;
    }
    if (is_neg) {
        temp[pos++] = '-';
    }
    int i;
    for (i = 0; i < pos; i++) {
        buf[i] = temp[pos - 1 - i];
    }
    buf[pos] = '\0';
    return pos;
}

int wl_format_u128(char* buf, unsigned long long low, unsigned long long high) {
    unsigned __int128 val = ((unsigned __int128)high << 64) | low;
    if (val == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return 1;
    }
    char temp[64];
    int pos = 0;
    while (val > 0) {
        temp[pos++] = (char)((val % 10) + '0');
        val /= 10;
    }
    int i;
    for (i = 0; i < pos; i++) {
        buf[i] = temp[pos - 1 - i];
    }
    buf[pos] = '\0';
    return pos;
}
