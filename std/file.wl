// file access

import "sys"
import "internal/platform/windows"
import "internal/platform/posix"
import "internal/platform/errors" as platform_errors
import "internal/runtime/string" as runtime_string
import Error from "builtin/errors"

const SEEK_SET -> Int = 0;
const SEEK_CUR -> Int = 1;
const SEEK_END -> Int = 2;

class File {
    let handle -> AnyPtr = nullptr;
    let path -> String = "";
    let last_error -> Error = Error.None;
    let write_buffer -> String = null;
    let write_buffer_len -> Int = 0;

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
                self.last_error = Error.InvalidArgument;
                return;
            }

            if (mode.ends_with("+")) { access = windows.GENERIC_READ | windows.GENERIC_WRITE; }

            let wide_path -> AnyPtr = windows.utf8_to_utf16(file_path);
            if (wide_path is nullptr) {
                self.last_error = platform_errors.last();
                return;
            }
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
            } else {
                self.last_error = platform_errors.last();
            }
            return;
        }

        let raw_handle -> AnyPtr = posix.wl_fopen(file_path, mode);
        if (raw_handle is !nullptr) {
            self.handle = raw_handle;
        } else {
            self.last_error = platform_errors.last();
        }
    }

    method error() -> Error {
        return self.last_error;
    }

    method __flush_write_buffer() -> Bool {
        if (self.write_buffer_len == 0) { return true; }
        if (self.handle is nullptr) {
            self.last_error = Error.IllegalState;
            return false;
        }
        if (sys.OS != "WINDOWS") {
            self.write_buffer_len = 0;
            return true;
        }

        let bytes_written -> Int = 0;
        if (windows.WriteFile(self.handle, runtime_string.data(self.write_buffer), self.write_buffer_len, ref bytes_written, nullptr) == 0) {
            self.last_error = platform_errors.last();
            return false;
        }
        if (bytes_written != self.write_buffer_len) {
            self.last_error = Error.DiskFull;
            return false;
        }

        self.write_buffer_len = 0;
        return true;
    }

    method read_all() -> String? {
        if (self.handle is nullptr) {
            if (self.last_error == Error.None) { throw Error.IllegalState; }
            throw self.last_error;
        }

        if (sys.OS == "WINDOWS") {
            let size -> Long = 0L;
            if (windows.GetFileSizeEx(self.handle, ref size) == 0 || size < 0L || size > 2147483647L) {
                if (size > 2147483647L) { throw Error.Overflow; }
                self.last_error = platform_errors.last();
                throw self.last_error;
            }

            let buffer -> String = runtime_string.alloc(size);
            if (buffer is null) {
                self.last_error = Error.OutOfMemory;
                throw self.last_error;
            }
            let bytes_read -> Int = 0;
            if (size > 0L && windows.ReadFile(self.handle, runtime_string.data(buffer), Int(size), ref bytes_read, nullptr) == 0) {
                self.last_error = platform_errors.last();
                throw self.last_error;
            }
            runtime_string.set_length(buffer, bytes_read);
            self.last_error = Error.None;
            return buffer;
        }

        if (posix.fseek(self.handle, 0L, SEEK_END) != 0) {
            self.last_error = platform_errors.last();
            throw self.last_error;
        }
        let size -> Long = posix.ftell(self.handle);
        if (size < 0L || size > 2147483647L) {
            if (size > 2147483647L) { throw Error.Overflow; }
            self.last_error = platform_errors.last();
            throw self.last_error;
        }
        posix.rewind(self.handle);

        let buffer -> String = runtime_string.alloc(size);
        if (buffer is null) {
            self.last_error = Error.OutOfMemory;
            throw self.last_error;
        }
        let read_count -> Long = posix.wl_fread(buffer, 1L, size, self.handle);
        if (read_count != size) {
            self.last_error = platform_errors.last();
            throw self.last_error;
        }
        self.last_error = Error.None;
        return buffer;
    }

    method write_all(content -> String) -> Int? {
        if (self.handle is nullptr) {
            if (self.last_error == Error.None) { throw Error.IllegalState; }
            throw self.last_error;
        }
        if (!self.__flush_write_buffer()) { throw self.last_error; }
        let length -> Long = content.length();
        if (sys.OS == "WINDOWS") {
            let bytes_written -> Int = 0;
            if (length > 0L && windows.WriteFile(self.handle, runtime_string.data(content), Int(length), ref bytes_written, nullptr) == 0) {
                self.last_error = platform_errors.last();
                throw self.last_error;
            }
            if (Long(bytes_written) != length) {
                self.last_error = Error.DiskFull;
                throw self.last_error;
            }
            self.last_error = Error.None;
            return bytes_written;
        }
        let written -> Long = posix.wl_fwrite(content, 1L, length, self.handle);
        if (written != length) {
            self.last_error = platform_errors.last();
            throw self.last_error;
        }
        self.last_error = Error.None;
        return Int(written);
    }

    method write(content -> String) -> Void {
        if (self.handle is nullptr) { return; }
        let length -> Long = content.length();
        if (sys.OS == "WINDOWS") {
            if (length <= 0L) { return; }
            if (self.write_buffer is null) {
                self.write_buffer = runtime_string.alloc(65536L);
                if (self.write_buffer is null) {
                    self.last_error = Error.OutOfMemory;
                    return;
                }
                runtime_string.set_length(self.write_buffer, 0);
            }

            let ptr source -> Byte = runtime_string.data(content);
            let ptr target -> Byte = runtime_string.data(self.write_buffer);
            let source_idx -> Int = 0;
            let content_len -> Int = Int(length);
            while (source_idx < content_len) {
                if (self.write_buffer_len == 65536 && !self.__flush_write_buffer()) { return; }
                let available -> Int = 65536 - self.write_buffer_len;
                let count -> Int = content_len - source_idx;
                if (count > available) { count = available; }

                let i -> Int = 0;
                while (i < count) {
                    target[self.write_buffer_len + i] = source[source_idx + i];
                    i += 1;
                }
                self.write_buffer_len += count;
                source_idx += count;
            }
            return;
        }
        if (posix.wl_fwrite(content, 1L, length, self.handle) != length) {
            self.last_error = platform_errors.last();
        }
    }

    method is_open() -> Bool {
        return self.handle is !nullptr;
    }

    method close_checked() -> Void? {
        let pending_error -> Error = self.last_error;
        if (self.handle is !nullptr) {
            if (!self.__flush_write_buffer() && pending_error == Error.None) {
                pending_error = self.last_error;
            }
            let closed -> Bool = false;
            if (sys.OS == "WINDOWS") { closed = windows.CloseHandle(self.handle) != 0; }
            else { closed = posix.fclose(self.handle) == 0; }
            self.handle = nullptr;
            if (!closed && pending_error == Error.None) {
                self.last_error = platform_errors.last();
                pending_error = self.last_error;
            }
        }
        self.write_buffer = null;
        self.write_buffer_len = 0;
        if (pending_error != Error.None) {
            self.last_error = pending_error;
            throw pending_error;
        }
        self.last_error = Error.None;
        return;
    }

    method close() -> Void {
        self.close_checked()?;
        catch(err) {
            return;
        }
    }

    deinit() {
        self.close();
    }
}

func open(path -> String) -> File? {
    let result -> File = File(path, "rb");
    if (!result.is_open()) { throw result.error(); }
    return result;
}

func create(path -> String) -> File? {
    let result -> File = File(path, "wb");
    if (!result.is_open()) { throw result.error(); }
    return result;
}

func append(path -> String) -> File? {
    let result -> File = File(path, "ab");
    if (!result.is_open()) { throw result.error(); }
    return result;
}

func exists(path -> String) -> Bool {
    if (sys.OS == "WINDOWS") {
        let wide_path -> AnyPtr = windows.utf8_to_utf16(path);
        if (wide_path is nullptr) { return false; }
        let attributes -> Int = windows.GetFileAttributesW(wide_path);
        windows.free_utf16(wide_path);
        return attributes != windows.INVALID_FILE_ATTRIBUTES;
    }

    let handle -> AnyPtr = posix.wl_fopen(path, "rb");
    if (handle is nullptr) { return false; }
    posix.fclose(handle);
    return true;
}

func remove(path -> String) -> Void? {
    if (sys.OS == "WINDOWS") {
        let wide_path -> AnyPtr = windows.utf8_to_utf16(path);
        if (wide_path is nullptr) { throw platform_errors.last(); }
        let removed -> Bool = windows.DeleteFileW(wide_path) != 0;
        windows.free_utf16(wide_path);
        if (!removed) { throw platform_errors.last(); }
        return;
    }
    if (posix.wl_remove(path) != 0) { throw platform_errors.last(); }
    return;
}
