// runtime/wl_runtime.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>

#ifdef _WIN32
#include <windows.h>
#endif

// Define WhiteLang String struct layout to ensure C compatibility
typedef struct {
    char* buf;
    int len;
    int cap;
} wl_string;

enum {
    WL_STRING_TYPE_ID = 5,
    WL_OBJECT_HEADER_SIZE = 8
};

static wl_string* wl_alloc_string_storage(size_t capacity) {
    if (capacity > INT_MAX || capacity > SIZE_MAX - WL_OBJECT_HEADER_SIZE - sizeof(wl_string) - 1) {
        return NULL;
    }

    const size_t total_size = WL_OBJECT_HEADER_SIZE + sizeof(wl_string) + capacity + 1;
    unsigned char* mem = (unsigned char*)malloc(total_size);
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
    memset(str->buf, 0, capacity + 1);
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
    const size_t len = strlen(c_str);
    wl_string* wl_str = wl_alloc_string_storage(len);
    if (wl_str == NULL) return NULL;
    memcpy(wl_str->buf, c_str, len + 1);
    return wl_str;
}

wl_string* get_arg(char** argv, int idx) {
    if (argv == NULL) return NULL;
    return to_wl_str(argv[idx]);
}

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

int wl_string_at(wl_string* s, int idx) {
    if (s == NULL || s->buf == NULL || idx < 0 || idx >= s->len) return 0;
    return (unsigned char)s->buf[idx];
}

wl_string* wl_string_slice(wl_string* s, int start, int end) {
    if (s == NULL || s->buf == NULL) return NULL;
    if (start < 0) start = 0;
    if (end > s->len) end = s->len;
    if (start > end) start = end;

    const size_t len = (size_t)(end - start);
    wl_string* result = wl_alloc_string_storage(len);
    if (result == NULL) return NULL;
    if (len > 0) memcpy(result->buf, s->buf + start, len);
    result->buf[len] = '\0';
    return result;
}

int wl_string_ends_with(wl_string* s, wl_string* suffix) {
    if (s == NULL || suffix == NULL || s->buf == NULL || suffix->buf == NULL) return 0;
    if (suffix->len > s->len) return 0;
    const int offset = s->len - suffix->len;
    for (int i = 0; i < suffix->len; ++i) {
        if (s->buf[offset + i] != suffix->buf[i]) return 0;
    }
    return 1;
}

int wl_string_starts_with(wl_string* s, wl_string* prefix) {
    if (s == NULL || prefix == NULL || s->buf == NULL || prefix->buf == NULL) return 0;
    if (prefix->len > s->len) return 0;
    for (int i = 0; i < prefix->len; ++i) {
        if (s->buf[i] != prefix->buf[i]) return 0;
    }
    return 1;
}

wl_string* wl_string_concat(wl_string* left, wl_string* right) {
    if (left == NULL || right == NULL || left->buf == NULL || right->buf == NULL) return NULL;
    const size_t left_len = (size_t)left->len;
    const size_t right_len = (size_t)right->len;
    if (left_len > SIZE_MAX - right_len) return NULL;

    wl_string* result = wl_alloc_string_storage(left_len + right_len);
    if (result == NULL) return NULL;
    if (left_len > 0) memcpy(result->buf, left->buf, left_len);
    if (right_len > 0) memcpy(result->buf + left_len, right->buf, right_len);
    result->buf[left_len + right_len] = '\0';
    return result;
}

int wl_string_compare(wl_string* left, wl_string* right) {
    if (left == right) return 0;
    if (left == NULL) return -1;
    if (right == NULL) return 1;

    const int common_len = left->len < right->len ? left->len : right->len;
    for (int i = 0; i < common_len; ++i) {
        const unsigned char lhs = (unsigned char)left->buf[i];
        const unsigned char rhs = (unsigned char)right->buf[i];
        if (lhs < rhs) return -1;
        if (lhs > rhs) return 1;
    }
    if (left->len < right->len) return -1;
    if (left->len > right->len) return 1;
    return 0;
}

// safely allocate an empty string of a specified size
wl_string* wl_alloc_string(long long size) {
    if (size < 0) return NULL;
    return wl_alloc_string_storage((size_t)size);
}

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
