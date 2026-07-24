// Test: STANDARD_LIBRARY_ERRORS
// File: tests/language/errors/test_std_errors.wl
// Focus: File and environment failures report structured errors

import "builtin"
import "file"
import "sys"
import Error from "builtin/errors"

func missing_file_reports_error() -> Bool {
    let input -> file.File = file.open("__whitelang_missing_file_7ce66f31__")?;
    catch(err) {
        return err == Error.FileNotFound;
    }
    input.close();
    return false;
}

func missing_env_reports_error() -> Bool {
    let value -> String = sys.env.get("__WHITELANG_MISSING_ENV_7CE66F31__")?;
    catch(err) {
        return err == Error.NotFound;
    }
    return value is null;
}

func main() -> Int {
    if (!missing_file_reports_error()) {
        builtin.print("FAIL: missing file error");
        return 1;
    }
    if (!missing_env_reports_error()) {
        builtin.print("FAIL: missing environment error");
        return 1;
    }
    builtin.print("PASS: standard library errors");
    return 0;
}
