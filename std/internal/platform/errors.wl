// platform error conversion

import * from "../../sys/target.wl"
import Error from "../../builtin/errors.wl"
import "windows.wl"
import "posix.wl"

func from_windows(code -> Int) -> Error {
    if (code == 2 || code == 3) { return Error.FileNotFound; }
    if (code == 5 || code == 32) { return Error.PermissionDenied; }
    if (code == 80 || code == 183) { return Error.AlreadyExists; }
    if (code == 8 || code == 14) { return Error.OutOfMemory; }
    if (code == 39 || code == 112) { return Error.DiskFull; }
    if (code == 109) { return Error.BrokenPipe; }
    if (code == 87) { return Error.InvalidArgument; }
    return Error.Unknown;
}

func from_posix(code -> Int) -> Error {
    if (code == 2) { return Error.FileNotFound; }
    if (code == 1 || code == 13) { return Error.PermissionDenied; }
    if (code == 17) { return Error.AlreadyExists; }
    if (code == 12) { return Error.OutOfMemory; }
    if (code == 28) { return Error.DiskFull; }
    if (code == 32) { return Error.BrokenPipe; }
    if (code == 4) { return Error.Interrupted; }
    if (code == 22) { return Error.InvalidArgument; }
    return Error.Unknown;
}

func last() -> Error {
    if (OS == "WINDOWS") {
        return from_windows(windows.GetLastError());
    }
    return from_posix(posix.wl_last_errno());
}
