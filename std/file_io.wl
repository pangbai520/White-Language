// std/file_io.wl
//
// Standard Input/Output Library.
// This file provides high-level abstractions over C stdio file operations.

extern "C" {
    func fopen(filename -> String, mode -> String) -> ptr Void;
    func fclose(ptr stream -> Void) -> Int;
    func fread(p -> String, size -> Long, count -> Long, ptr stream -> Void) -> Long;
    func fwrite(p -> String, size -> Long, count -> Long, ptr stream -> Void) -> Long;
    func fseek(ptr stream -> Void, offset -> Long, origin -> Int) -> Int;
    func ftell(ptr stream -> Void) -> Long;
    func rewind(ptr stream -> Void) -> Void;
    func remove(filename -> String) -> Int;

    func wl_alloc_string(size -> Long) -> String;
}


const SEEK_SET -> Int = 0;
const SEEK_CUR -> Int = 1;
const SEEK_END -> Int = 2;


class File {
    let ptr handle -> Void = nullptr;
    let path -> String = "";

    init(p -> String, mode -> String) {
        self.path = p;
        let ptr raw_handle -> Void = fopen(p, mode);
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
        fread(buffer, 1, size, self.handle);
        return buffer;
    }

    method write(content -> String) -> Void {
        if (self.handle is nullptr) { return; }
        let len -> Long = content.length(); 
        fwrite(content, 1, len, self.handle);
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
    let res -> Int = remove(path);
    return res == 0;
}
