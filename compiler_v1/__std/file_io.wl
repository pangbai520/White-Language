// std/io.wl
//
// Standard Input/Output Library.
// This file provides high-level abstractions over C stdio file operations.

// ----------------------------------------------------------
// C Runtime Bindings
// ----------------------------------------------------------
extern "C" {
    // 'String' to represent generic C pointers (FILE*, void*).
    func fopen(filename -> String, mode -> String) -> ptr Void;
    func fclose(stream -> ptr Void) -> Int;
    func fread(p -> String, size -> Long, count -> Long, stream -> ptr Void) -> Long;
    func fwrite(p -> String, size -> Long, count -> Long, stream -> ptr Void) -> Long;
    func fseek(stream -> ptr Void, offset -> Long, origin -> Int) -> Int;
    func ftell(stream -> ptr Void) -> Long;
    func rewind(stream -> ptr Void) -> Void;

    // White Lang C Helper
    func __wl_str_set(s -> String, idx -> Int, val -> Int) -> Void;
    func wl_alloc_string(size -> Long) -> String;
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
struct File(handle -> ptr Void, path -> String)


// ==========================================================
// File Operations
// ==========================================================
func open(path -> String, mode -> String) -> File {
    let ptr raw_handle -> Void = fopen(path, mode);

    if (raw_handle is null) {
        return null;
    }

    return File(handle=raw_handle, path=path);
}

func close(self -> File) -> Void {
    if (self.handle is !null) {
        fclose(self.handle);
        self.handle = nullptr; 
    }
}


func read_all(self -> File) -> String {
    if (self.handle is null) {
        return wl_alloc_string(0);
    }

    let ptr handle -> Void = self.handle;
    fseek(handle, 0, SEEK_END);
    let size -> Long = ftell(handle);
    rewind(handle);
    let buffer -> String = wl_alloc_string(size);
    fread(buffer, 1, size, handle);

    return buffer;
}


func write(self -> File, content -> String) -> Void {
    if (self.handle is null) { return; }
    let len -> Long = content.length(); 

    fwrite(content, 1, len, self.handle);
}