// runtime/wl_runtime.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#endif

int is_windows() {
    #ifdef _WIN32
        return 1;
    #else
        return 0;
    #endif
}

char* to_wl_str(const char* c_str) {
    if (c_str == NULL) return NULL;
    int len = strlen(c_str);
    
    // alloc mem for rc(4) + type_id(4) + str data + null term
    void* mem = malloc(8 + len + 1);
    
    // init refcount
    int* rc = (int*)mem;
    *rc = 0;
    
    // type 5 is string
    int* type_id = (int*)((char*)mem + 4);
    *type_id = 5; 
    
    char* wl_str = (char*)mem + 8;
    strcpy(wl_str, c_str);
    
    return wl_str;
}

char* get_arg(char** argv, int idx) {
    return to_wl_str(argv[idx]);
}

int system_call(char* cmd) {
    return system(cmd);
}

char* wl_getenv(const char* name) {
    return to_wl_str(getenv(name));
}

void __wl_str_set(char* s, int idx, int val) {
    if (s) {
        s[idx] = (char)val;
    }
}

char __wl_str_get(char* s, int idx) {
    if (s) {
        return s[idx];
    }
    return 0;
}

// safely allocate an empty string of a specified size
char* wl_alloc_string(long long size) {
    void* mem = malloc(8 + size + 1);

    int* rc = (int*)mem;
    *rc = 0;

    int* type_id = (int*)((char*)mem + 4);
    *type_id = 5;

    char* str = (char*)mem + 8;
    memset(str, 0, size + 1);
    
    return str;
}


// native io handling
void wl_write_utf8(const char* str) {
    if (!str) return;

#ifdef _WIN32
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut == INVALID_HANDLE_VALUE || !hOut) {
        printf("%s", str); // fallback
        return;
    }

    DWORD dwMode = 0;
    // check if it's an actual console or piped/redirected
    if (!GetConsoleMode(hOut, &dwMode)) {
        printf("%s", str);
        return;
    }

    // convert to utf16 for windows console api
    int wlen = MultiByteToWideChar(CP_UTF8, 0, str, -1, NULL, 0);
    if (wlen == 0) return;

    wchar_t* wstr = (wchar_t*)malloc(wlen * sizeof(wchar_t));
    if (!wstr) return;

    MultiByteToWideChar(CP_UTF8, 0, str, -1, wstr, wlen);

    DWORD written = 0;
    // wlen - 1 to drop the null terminator
    WriteConsoleW(hOut, wstr, wlen - 1, &written, NULL);

    free(wstr);
#else
    printf("%s", str);
#endif
}

void wl_print_utf8(const char* str) {
    if (!str) str = "null";
    wl_write_utf8(str);

#ifdef _WIN32
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD dwMode = 0;
    if (hOut != INVALID_HANDLE_VALUE && hOut && GetConsoleMode(hOut, &dwMode)) {
        DWORD written = 0;
        WriteConsoleW(hOut, L"\n", 1, &written, NULL);
    } else {
        printf("\n");
    }
#else
    printf("\n");
#endif
}
