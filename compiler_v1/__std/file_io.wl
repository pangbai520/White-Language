// std/io.wl
//
// Standard Input/Output Library.
// This module provides high-level abstractions over C stdio file operations.

// ----------------------------------------------------------
// C Runtime Bindings (libc stdio)
// ----------------------------------------------------------
// 'String' (i8*) to represent generic C pointers (FILE*, void*).
extern "C" {
    func fopen(filename -> String, mode -> String) -> String;
    func fclose(stream -> String) -> Int;
    func fread(p -> String, size -> Long, count -> Long, stream -> String) -> Long;
    func fwrite(p -> String, size -> Long, count -> Long, stream -> String) -> Long;
    func fseek(stream -> String, offset -> Long, origin -> Int) -> Int;
    func ftell(stream -> String) -> Long;
    func rewind(stream -> String) -> Void;
    
    func malloc(size -> Long) -> String;
    func free(p -> String) -> Void;
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

// Represents an open file handle.
// Syntax corrected: struct Name(field -> Type, ...)
struct File(handle -> String, path -> String)


// ==========================================================
// File Operations
// ==========================================================

// ----------------------------------------------------------
// 1. File Opening
// ----------------------------------------------------------
// Opens a file at the specified path with the given mode.
// Returns a pointer to a File struct (ptr File).
func open(path -> String, mode -> String) -> File {
    let raw_handle -> String = fopen(path, mode);

    if (raw_handle == null) {
        return null;
    }
    
    // Return a new File struct
    // The compiler handles the allocation and initialization automatically
    return File(handle=raw_handle, path=path);
}


// ----------------------------------------------------------
// 2. File Closing
// ----------------------------------------------------------
func close(self -> File) -> Void {
    // Check against 'null' because handle is a String (i8*)
    if (self.handle != null) {
        fclose(self.handle);
        self.handle = null; 
    }
}


// ----------------------------------------------------------
// 3. Read All Content
// ----------------------------------------------------------
// Reads the entire content of a file into a String.
func read_all(self -> File) -> String {
    if (self.handle == null) {
        let empty -> String = malloc(1);
        empty[0] = 0;
        return empty;
    }

    let handle -> String = self.handle;

    // Determine file size
    fseek(handle, 0, SEEK_END);
    let size -> Long = ftell(handle);
    rewind(handle);

    // Allocate buffer: size + 1 byte for '\0'
    // Compiler automatically promotes Int (1) to Long
    let buffer -> String = malloc(size + 1);

    // Read content
    fread(buffer, 1, size, handle);

    // Null-terminate the string
    // Compiler handles implicit casts for indexing and assignment
    buffer[size] = 0;

    return buffer;
}


// ----------------------------------------------------------
// 4. Write String to File
// ----------------------------------------------------------
func write(self -> File, content -> String) -> Void {
    if (self.handle == null) { return; }
    
    // Uses the internal String.length property
    let len -> Int = content.length; 
    
    // fwrite expects Long count, compiler auto-promotes Int len
    fwrite(content, 1, len, self.handle);
}