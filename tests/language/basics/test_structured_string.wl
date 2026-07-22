// Test: STRUCTURED_STRING_RUNTIME_ABI
// File: tests/integration/os/test_structured_string.wl
// Focus: Structured String ABI stability across runtime calls, ARC cleanup,
// stdlib methods, native file I/O, and type conversions.

import "builtin"
import "file"

func main() -> Int {
    let prefix -> String = "White";
    let language -> String = prefix + "Lang";
    let description -> String = language + "-" + 42 + "-" + true + '-';
    let bounded -> String = language[0:5];
    let whole -> String = ref language[:];

    let methods_ok -> Bool = language.length() == 9 &&
                            language.at(0) == 'W' &&
                            language[8] == 'g' &&
                            language.slice(5, 9) == "Lang" &&
                            bounded == "White" &&
                            whole == "WhiteLang" &&
                            language.starts_with("White") &&
                            language.ends_with("Lang");
    let conversions_ok -> Bool = description == "WhiteLang-42-true-";

    let path -> String = "tests_structured_string_runtime.tmp";
    let writer -> file.File = file.create(path);
    if (writer is null || !writer.is_open()) {
        builtin.print("FAIL: could not create structured String runtime fixture");
        return 1;
    }
    writer.write(description);
    writer.close();

    let appender -> file.File = file.append(path);
    if (appender is null || !appender.is_open()) {
        file.remove(path);
        builtin.print("FAIL: could not append structured String runtime fixture");
        return 1;
    }
    appender.write("!");
    appender.close();

    let reader -> file.File = file.open(path);
    if (reader is null || !reader.is_open()) {
        file.remove(path);
        builtin.print("FAIL: could not reopen structured String runtime fixture");
        return 1;
    }
    let roundtrip -> String = reader.read_all();
    reader.close();
    file.remove(path);

    let file_ok -> Bool = roundtrip.length() == description.length() + 1 && roundtrip == description + "!";
    if (methods_ok && conversions_ok && file_ok) {
        builtin.print("PASS: Structured String runtime ABI");
        return 0;
    }

    builtin.print("FAIL: Structured String runtime ABI");
    return 1;
}
