// native posix and libc bindings

extern func malloc(size -> Long) -> AnyPtr from "C";
extern func calloc(count -> Long, size -> Long) -> AnyPtr from "C";
extern func realloc(block -> AnyPtr, size -> Long) -> AnyPtr from "C";
extern func free(block -> AnyPtr) -> Void from "C";

extern func write(fd -> Int, buf -> AnyPtr, count -> Long) -> Long from "C";
extern func wl_getenv(name -> String) -> String from "C";
extern func system_call(command -> String) -> Int from "C";
extern func wl_posix_exit(status -> Int) -> Void from "C";
extern func wl_last_errno() -> Int from "C";
extern func getpid() -> Int from "C";
extern func fork() -> Int from "C";
extern func execvp(file -> AnyPtr, argv -> AnyPtr) -> Int from "C";
extern func waitpid(pid -> Int, status -> AnyPtr, options -> Int) -> Int from "C";
extern func _exit(status -> Int) -> Void from "C";

extern func wl_fopen(filename -> String, mode -> String) -> AnyPtr from "C";
extern func fclose(stream -> AnyPtr) -> Int from "C";
extern func wl_fread(p -> String, size -> Long, count -> Long, stream -> AnyPtr) -> Long from "C";
extern func wl_fwrite(p -> String, size -> Long, count -> Long, stream -> AnyPtr) -> Long from "C";
extern func fseek(stream -> AnyPtr, offset -> Long, origin -> Int) -> Int from "C";
extern func ftell(stream -> AnyPtr) -> Long from "C";
extern func rewind(stream -> AnyPtr) -> Void from "C";
extern func wl_remove(filename -> String) -> Int from "C";
