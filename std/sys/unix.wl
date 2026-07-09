// std/sys/unix.wl
// Raw Unix OS Bindings

extern func write(fd -> Int, buf -> String, count -> Long) -> Long from "c";
