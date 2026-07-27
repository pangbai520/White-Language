// Test: ERROR_PRINT
// File: tests/language/errors/test_error_print.wl
// Focus: Printing concrete and type-erased error values.

import "builtin"

error ParseError {
    InvalidToken
}

func fail() -> Void? {
    throw ParseError.InvalidToken;
}

func main() -> Int {
    builtin.print(ParseError.InvalidToken);
    fail()?;
    catch(err) {
        builtin.print(err);
        builtin.print("PASS: Error printing");
        return 0;
    }
    builtin.print("FAIL: Error printing");
    return 1;
}
