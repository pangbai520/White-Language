// compiler memory hooks

import "sys"
import "internal/platform/windows"
import "internal/platform/posix"

extern func wl_pointer_size() -> Long from "C";

func pointer_size() -> Long {
    return wl_pointer_size();
}

@CompilerLink("memory_alloc")
func mem_alloc(size -> Long) -> AnyPtr {
    if (size <= 0L) { size = 1L; }
    if (sys.OS == "WINDOWS") {
        return windows.HeapAlloc(windows.GetProcessHeap(), 0, size);
    }
    return posix.malloc(size);
}

@CompilerLink("memory_alloc_zeroed")
func mem_alloc_zeroed(size -> Long) -> AnyPtr {
    if (size <= 0L) { size = 1L; }
    if (sys.OS == "WINDOWS") {
        return windows.HeapAlloc(windows.GetProcessHeap(), windows.HEAP_ZERO_MEMORY, size);
    }
    return posix.calloc(1L, size);
}

@CompilerLink("memory_resize")
func mem_resize(block -> AnyPtr, size -> Long) -> AnyPtr {
    if (block is nullptr) { return mem_alloc(size); }
    if (size <= 0L) { size = 1L; }
    if (sys.OS == "WINDOWS") {
        return windows.HeapReAlloc(windows.GetProcessHeap(), 0, block, size);
    }
    return posix.realloc(block, size);
}

@CompilerLink("memory_free")
func mem_dealloc(block -> AnyPtr) -> Void {
    if (block is nullptr) { return; }
    if (sys.OS == "WINDOWS") {
        windows.HeapFree(windows.GetProcessHeap(), 0, block);
        return;
    }
    posix.free(block);
}

@CompilerLink("memory_copy")
func mem_copy(dest -> AnyPtr, src -> AnyPtr, count -> Long) -> AnyPtr {
    if (dest is nullptr || src is nullptr || count <= 0L) { return dest; }
    let ptr d -> Byte = dest;
    let ptr s -> Byte = src;
    let i -> Long = 0L;
    while (i < count) {
        let idx -> Int = Int(i);
        d[idx] = s[idx];
        i += 1L;
    }
    return dest;
}

@CompilerLink("memory_set")
func mem_set(dest -> AnyPtr, value -> Byte, count -> Long) -> AnyPtr {
    if (dest is nullptr || count <= 0L) { return dest; }
    let ptr d -> Byte = dest;
    let i -> Long = 0L;
    while (i < count) {
        d[Int(i)] = value;
        i += 1L;
    }
    return dest;
}
