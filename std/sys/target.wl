// std/sys/target.wl
// compile-time target operating system
// possible values: WINDOWS, LINUX, MACOS
@CompilerIntrinsic("target_os")
const OS -> String = "";
