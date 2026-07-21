// std/file_io.wl
//
// Standard Input/Output Library.
// This file provides high-level abstractions over C stdio file operations.

extern "C" {
    func wl_fopen(filename -> String, mode -> String) -> AnyPtr;
    func fclose(stream -> AnyPtr) -> Int;
    func wl_fread(p -> String, size -> Long, count -> Long, stream -> AnyPtr) -> Long;
    func wl_fwrite(p -> String, size -> Long, count -> Long, stream -> AnyPtr) -> Long;
    func fseek(stream -> AnyPtr, offset -> Long, origin -> Int) -> Int;
    func ftell(stream -> AnyPtr) -> Long;
    func rewind(stream -> AnyPtr) -> Void;
    func wl_remove(filename -> String) -> Int;

    func wl_alloc_string(size -> Long) -> String;
}


const SEEK_SET -> Int = 0;
const SEEK_CUR -> Int = 1;
const SEEK_END -> Int = 2;


class File {
    let handle -> AnyPtr = nullptr;
    let path -> String = "";

    init(p -> String, mode -> String) {
        self.path = p;
        let raw_handle -> AnyPtr = wl_fopen(p, mode);
        if (raw_handle is !nullptr) {
            self.handle = raw_handle;
        }
    }

    method read_all() -> String {
        if (self.handle is nullptr) { return wl_alloc_string(0); }

        fseek(self.handle, 0, SEEK_END);
        let size -> Long = ftell(self.handle);
        rewind(self.handle);
        
        let buffer -> String = wl_alloc_string(size);
        wl_fread(buffer, 1, size, self.handle);
        return buffer;
    }

    method write(content -> String) -> Void {
        if (self.handle is nullptr) { return; }
        let len -> Long = content.length(); 
        wl_fwrite(content, 1, len, self.handle);
    }

    method close() -> Void {
        if (self.handle is !nullptr) {
            fclose(self.handle);
            self.handle = nullptr; 
        }
    }

    deinit() {
        self.close();
    }
}

func remove_file(path -> String) -> Bool {
    let res -> Int = wl_remove(path);
    return res == 0;
}
