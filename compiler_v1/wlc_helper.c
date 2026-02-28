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

int remove_file(char* path) {
    return remove(path);
}

char* get_arg(char** argv, int idx) {
    return argv[idx];
}

int system_call(char* cmd) {
    return system(cmd);
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