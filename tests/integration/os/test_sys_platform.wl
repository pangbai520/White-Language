// Test: SYS_PLATFORM_DETECTION
// File: tests/integration/os/test_sys_platform.wl
// Focus: Standard library system module integration, platform constant resolution, and conditional branching.

import "builtin"
import "sys"

func platform_value() -> Int {
    if (sys.OS == "WINDOWS") {
        return 7;
    } else if (sys.OS == "LINUX") {
        return 9;
    } else if (sys.OS == "MACOS") {
        return 11;
    }
    return 0;
}

func main() -> Int {
    let detected -> String = sys.OS;
    if (detected.length() == 0) {
        builtin.print("FAIL: sys.OS is empty");
        return 1;
    }
    if (sys.OS == "WINDOWS") {
        if (platform_value() != 7) {
            builtin.print("FAIL: compile-time platform return");
            return 1;
        }
    } else if (sys.OS == "LINUX") {
        if (platform_value() != 9) {
            builtin.print("FAIL: compile-time platform return");
            return 1;
        }
    } else if (sys.OS == "MACOS") {
        if (platform_value() != 11) {
            builtin.print("FAIL: compile-time platform return");
            return 1;
        }
    }

    if (sys.OS == "WINDOWS") {
        builtin.print("PASS: sys.OS WINDOWS");
    } else if (sys.OS == "LINUX") {
        builtin.print("PASS: sys.OS LINUX");
    } else if (sys.OS == "MACOS") {
        builtin.print("PASS: sys.OS MACOS");
    } else {
        builtin.print("FAIL: unknown sys.OS");
        return 1;
    }
    return 0;
}
