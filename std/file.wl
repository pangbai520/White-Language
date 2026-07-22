// file access

import "sys"
import "internal/platform/windows"
import "internal/platform/posix"

extern "C" {
    func wl_alloc_string(size -> Long) -> String;
    func wl_string_data(value -> String) -> AnyPtr;
    func wl_string_set_length(value -> String, length -> Int) -> Void;
}

const SEEK_SET -> Int = 0;
const SEEK_CUR -> Int = 1;
const SEEK_END -> Int = 2;

class File {
    let handle -> AnyPtr = nullptr;
    let path -> String = "";

    init(file_path -> String, mode -> String) {
        self.path = file_path;
        if (sys.OS == "WINDOWS") {
            let access -> Int = windows.GENERIC_READ;
            let disposition -> Int = windows.OPEN_EXISTING;
            let append_mode -> Bool = false;

            if (mode.starts_with("w")) {
                access = windows.GENERIC_WRITE;
                disposition = windows.CREATE_ALWAYS;
            } else if (mode.starts_with("a")) {
                access = windows.GENERIC_WRITE;
                disposition = windows.OPEN_ALWAYS;
                append_mode = true;
            } else if (!mode.starts_with("r")) {
                return;
            }

            if (mode.ends_with("+")) { access = windows.GENERIC_READ | windows.GENERIC_WRITE; }

            let wide_path -> AnyPtr = windows.utf8_to_utf16(file_path);
            if (wide_path is nullptr) { return; }
            let raw_handle -> AnyPtr = windows.CreateFileW(
                wide_path,
                access,
                windows.FILE_SHARE_READ | windows.FILE_SHARE_WRITE | windows.FILE_SHARE_DELETE,
                nullptr,
                disposition,
                windows.FILE_ATTRIBUTE_NORMAL,
                nullptr
            );
            windows.free_utf16(wide_path);
            if (!windows.is_invalid_handle(raw_handle)) {
                self.handle = raw_handle;
                if (append_mode) { windows.SetFilePointerEx(self.handle, 0L, nullptr, windows.FILE_END); }
            }
            return;
        }

        let raw_handle -> AnyPtr = posix.wl_fopen(file_path, mode);
        if (raw_handle is !nullptr) { self.handle = raw_handle; }
    }

    method read_all() -> String {
        if (self.handle is nullptr) { return wl_alloc_string(0L); }

        if (sys.OS == "WINDOWS") {
            let size -> Long = 0L;
            if (windows.GetFileSizeEx(self.handle, ref size) == 0 || size < 0L || size > 2147483647L) {
                return wl_alloc_string(0L);
            }

            let buffer -> String = wl_alloc_string(size);
            if (buffer is null) { return null; }
            let bytes_read -> Int = 0;
            if (size > 0L && windows.ReadFile(self.handle, wl_string_data(buffer), Int(size), ref bytes_read, nullptr) == 0) {
                wl_string_set_length(buffer, 0);
                return buffer;
            }
            wl_string_set_length(buffer, bytes_read);
            return buffer;
        }

        posix.fseek(self.handle, 0L, SEEK_END);
        let size -> Long = posix.ftell(self.handle);
        posix.rewind(self.handle);

        let buffer -> String = wl_alloc_string(size);
        posix.wl_fread(buffer, 1L, size, self.handle);
        return buffer;
    }

    method write(content -> String) -> Void {
        if (self.handle is nullptr) { return; }
        let length -> Long = content.length();
        if (sys.OS == "WINDOWS") {
            let bytes_written -> Int = 0;
            if (length > 0L) { windows.WriteFile(self.handle, wl_string_data(content), Int(length), ref bytes_written, nullptr); }
            return;
        }
        posix.wl_fwrite(content, 1L, length, self.handle);
    }

    method is_open() -> Bool {
        return self.handle is !nullptr;
    }

    method close() -> Void {
        if (self.handle is !nullptr) {
            if (sys.OS == "WINDOWS") { windows.CloseHandle(self.handle); }
            else { posix.fclose(self.handle); }
            self.handle = nullptr;
        }
    }

    deinit() {
        self.close();
    }
}

func open(path -> String) -> File {
    return File(path, "rb");
}

func create(path -> String) -> File {
    return File(path, "wb");
}

func append(path -> String) -> File {
    return File(path, "ab");
}

func remove(path -> String) -> Bool {
    if (sys.OS == "WINDOWS") {
        let wide_path -> AnyPtr = windows.utf8_to_utf16(path);
        if (wide_path is nullptr) { return false; }
        let removed -> Bool = windows.DeleteFileW(wide_path) != 0;
        windows.free_utf16(wide_path);
        return removed;
    }
    return posix.wl_remove(path) == 0;
}
