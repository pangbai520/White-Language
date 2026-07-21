
// This file intentionally implements the legacy WhiteLang String ABI where a
// String value is a pointer to NUL-terminated bytes preceded by the 8-byte ARC
// header. It lets the last CString-based compiler build the first compiler that
// emits the structured String ABI. Do not link it into structured-String builds.

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#endif

enum {
    WL_STRING_TYPE_ID = 5,
    WL_OBJECT_HEADER_SIZE = 8
};

static char* legacy_alloc_string(size_t capacity) {
    if (capacity > INT_MAX || capacity > SIZE_MAX - WL_OBJECT_HEADER_SIZE - 1) return NULL;
    unsigned char* mem = (unsigned char*)malloc(WL_OBJECT_HEADER_SIZE + capacity + 1);
    if (mem == NULL) return NULL;
    *(int*)mem = 0;
    *(int*)(mem + sizeof(int)) = WL_STRING_TYPE_ID;
    char* str = (char*)(mem + WL_OBJECT_HEADER_SIZE);
    memset(str, 0, capacity + 1);
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

char* to_wl_str(const char* c_str) {
    if (c_str == NULL) return NULL;
    const size_t len = strlen(c_str);
    char* result = legacy_alloc_string(len);
    if (result != NULL) memcpy(result, c_str, len + 1);
    return result;
}

char* get_arg(char** argv, int idx) {
    if (argv == NULL || idx < 0) return NULL;
    return to_wl_str(argv[idx]);
}

int system_call(char* cmd) {
    if (cmd == NULL) return -1;
    return system(cmd);
}

char* wl_getenv(char* name) {
    if (name == NULL) return NULL;
    return to_wl_str(getenv(name));
}

void __wl_str_set(char* s, int idx, int val) {
    if (s != NULL && idx >= 0) s[idx] = (char)val;
}

char __wl_str_get(char* s, int idx) {
    if (s == NULL || idx < 0) return 0;
    return s[idx];
}

char* wl_alloc_string(long long size) {
    if (size < 0) return NULL;
    return legacy_alloc_string((size_t)size);
}

void* wl_string_data(char* s) {
    return s;
}

int wl_string_at(char* s, int idx) {
    if (s == NULL || idx < 0 || (size_t)idx >= strlen(s)) return 0;
    return (unsigned char)s[idx];
}

char* wl_string_slice(char* s, int start, int end) {
    if (s == NULL) return NULL;
    const int source_len = (int)strlen(s);
    if (start < 0) start = 0;
    if (end > source_len) end = source_len;
    if (start > end) start = end;
    const size_t len = (size_t)(end - start);
    char* result = legacy_alloc_string(len);
    if (result != NULL && len > 0) memcpy(result, s + start, len);
    return result;
}

int wl_string_ends_with(char* s, char* suffix) {
    if (s == NULL || suffix == NULL) return 0;
    const size_t s_len = strlen(s);
    const size_t suffix_len = strlen(suffix);
    if (suffix_len > s_len) return 0;
    return memcmp(s + s_len - suffix_len, suffix, suffix_len) == 0;
}

int wl_string_starts_with(char* s, char* prefix) {
    if (s == NULL || prefix == NULL) return 0;
    const size_t s_len = strlen(s);
    const size_t prefix_len = strlen(prefix);
    if (prefix_len > s_len) return 0;
    return memcmp(s, prefix, prefix_len) == 0;
}

char* wl_string_concat(char* left, char* right) {
    if (left == NULL || right == NULL) return NULL;
    const size_t left_len = strlen(left);
    const size_t right_len = strlen(right);
    if (left_len > SIZE_MAX - right_len) return NULL;
    char* result = legacy_alloc_string(left_len + right_len);
    if (result == NULL) return NULL;
    memcpy(result, left, left_len);
    memcpy(result + left_len, right, right_len + 1);
    return result;
}

int wl_string_compare(char* left, char* right) {
    if (left == right) return 0;
    if (left == NULL) return -1;
    if (right == NULL) return 1;
    return strcmp(left, right);
}

void* wl_fopen(char* filename, char* mode) {
    if (filename == NULL || mode == NULL) return NULL;
    return fopen(filename, mode);
}

long long wl_fread(char* dest, long long size, long long count, void* stream) {
    if (dest == NULL || stream == NULL || size < 0 || count < 0) return 0;
    return (long long)fread(dest, (size_t)size, (size_t)count, (FILE*)stream);
}

long long wl_fwrite(char* src, long long size, long long count, void* stream) {
    if (src == NULL || stream == NULL || size < 0 || count < 0) return 0;
    return (long long)fwrite(src, (size_t)size, (size_t)count, (FILE*)stream);
}

int wl_remove(char* filename) {
    if (filename == NULL) return -1;
    return remove(filename);
}

void wl_format_i128(char* buf, unsigned long long low, long long high) {
    __int128 val = (__int128)(((unsigned __int128)(unsigned long long)high << 64) | low);
    if (val == 0) {
        strcpy(buf, "0");
return;
    }
int is_neg = 0;
    unsigned __int128 uval;
    if (val < 0) {
is_neg = 1;
        uval = (unsigned __int128)(-val);
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
}

void wl_format_u128(char* buf, unsigned long long low, unsigned long long high) {
    unsigned __int128 val = ((unsigned __int128)high << 64) | low;
    if (val == 0) {
        strcpy(buf, "0");
return;
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
}
