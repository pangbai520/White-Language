# Test: DLL_PYTHON_INTEROP
# File: tests/ffi/test_dll_host.py
# Focus: Python-to-WhiteLang FFI via ctypes, symbol resolution, and ABI validation.
# Compile: python test_dll_host.py

import ctypes
import os
import sys
import platform

def main():
    # detect platform-specific shared library extension
    lib_ext = ".dll" if platform.system() == "Windows" else ".so"
    # ensure the library path is absolute and relative to this script
    lib_path = os.path.abspath(os.path.join(os.path.dirname(__file__), f"test_lib_export{lib_ext}"))

    if not os.path.exists(lib_path):
        print(f"FAIL: Exported library not found at {lib_path}")
        sys.exit(1)

    try:
        # load the WhiteLang shared object (cdecl calling convention)
        wl_lib = ctypes.CDLL(lib_path)

        # map prototypes to ensure correct register/stack alignment
        # Note: ensuring 'c_double' matches the WhiteLang Float precision (f64)
        wl_lib.add.argtypes = [ctypes.c_int, ctypes.c_int]
        wl_lib.add.restype = ctypes.c_int

        wl_lib.factorial.argtypes = [ctypes.c_int]
        wl_lib.factorial.restype = ctypes.c_int

        wl_lib.multiply_float.argtypes = [ctypes.c_double, ctypes.c_double]
        wl_lib.multiply_float.restype = ctypes.c_double

        # trigger procedure calls across the binary boundary
        res_add = wl_lib.add(5, 7)
        res_fact = wl_lib.factorial(4)
        res_float = wl_lib.multiply_float(1.5, 4.0)

        # verify result integrity
        if res_add == 12 and res_fact == 24 and res_float == 6.0:
            print("PASS: Python FFI boundary and symbol resolution stable")
            sys.exit(0)
        else:
            # log failure with actual captured values
            print(f"FAIL: ABI mismatch. Add:{res_add}, Fact:{res_fact}, Float:{res_float}")
            sys.exit(1)

    except Exception as e:
        print(f"FAIL: Runtime FFI error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()