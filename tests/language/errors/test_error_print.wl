// Test: ERROR_PRINT
// File: tests/language/errors/test_error_print.wl
// Focus: Printing concrete and type-erased error values.


error ParseError {
    InvalidToken
}

func fail() -> Void? {
    throw ParseError.InvalidToken;
}

func main() -> Int {
    print(ParseError.InvalidToken);
    fail()?;
    catch(err) {
        print(err);
        print("PASS: Error printing");
        return 0;
    }
    print("FAIL: Error printing");
    return 1;
}
