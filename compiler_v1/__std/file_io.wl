// std/io.wl
//
// Standard Input/Output Library.
// This file provides high-level abstractions over C stdio file operations.

// ----------------------------------------------------------
// C Runtime Bindings
// ----------------------------------------------------------
extern "C" {
    // 'String' to represent generic C pointers (FILE*, void*).
    func fopen(filename -> String, mode -> String) -> String;
    func fclose(stream -> String) -> Int;
    func fread(p -> String, size -> Long, count -> Long, stream -> String) -> Long;
    func fwrite(p -> String, size -> Long, count -> Long, stream -> String) -> Long;
    func fseek(stream -> String, offset -> Long, origin -> Int) -> Int;
    func ftell(stream -> String) -> Long;
    func rewind(stream -> String) -> Void;
    func calloc(num -> Long, size -> Long) -> String;
    func malloc(size -> Long) -> String;
    func free(p -> String) -> Void;

    // White Lang C Helper
    func __wl_str_set(s -> String, idx -> Int, val -> Int) -> Void;
}

// ==========================================================
// Constants
// ==========================================================
const SEEK_SET -> Int = 0;
const SEEK_CUR -> Int = 1;
const SEEK_END -> Int = 2;

// ==========================================================
// Imports
// ==========================================================
import "builtin"

// ==========================================================
// Type Definitions
// ==========================================================
struct File(handle -> String, path -> String)


// ==========================================================
// File Operations
// ==========================================================
func open(path -> String, mode -> String) -> File {
    let raw_handle -> String = fopen(path, mode);

    if (raw_handle is null) {
        return null;
    }

    return File(handle=raw_handle, path=path);
}

func close(self -> File) -> Void {
    if (self.handle is !null) {
        fclose(self.handle);
        self.handle = null; 
    }
}


func read_all(self -> File) -> String {
    if (self.handle is null) {
        let empty -> String = calloc(1, 1);
        __wl_str_set(empty, 0, 0);
        return empty;
    }

    let handle -> String = self.handle;
    fseek(handle, 0, SEEK_END);
    let size -> Long = ftell(handle);
    rewind(handle);
    let buffer -> String = calloc(size + 1, 1);
    fread(buffer, 1, size, handle);

    return buffer;
}


func write(self -> File, content -> String) -> Void {
    if (self.handle is null) { return; }
    let len -> Int = content.length(); 

    fwrite(content, 1, len, self.handle);
}