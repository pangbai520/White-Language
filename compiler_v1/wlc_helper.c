#include <stdio.h>
#include <stdlib.h>
#include <string.h>


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
    
    void* mem = malloc(8 + len + 1);
    
    int* rc = (int*)mem;
    *rc = 0;
    
    char* wl_str = (char*)mem + 8;
    strcpy(wl_str, c_str);
    
    return wl_str;
}

int remove_file(char* path) {
    return remove(path);
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

// for string.wl
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

    char* str = (char*)mem + 8;
    memset(str, 0, size + 1);
    
    return str;
}