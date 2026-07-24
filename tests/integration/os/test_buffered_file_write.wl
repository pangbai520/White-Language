// Test: BUFFERED_FILE_WRITE
// File: tests/integration/os/test_buffered_file_write.wl
// Focus: Small writes cross the native buffer boundary and flush in order on close

import "builtin"
import "file"

func main() -> Int {
    let path -> String = "wl_buffered_write_test.tmp";
    let output -> file.File = file.create(path)?;
    catch(err) {
        builtin.print("FAIL: Could not create buffered output");
        return 1;
    }

    let i -> Int = 0;
    while (i < 7000) {
        output.write("0123456789");
        if (output.error() != Error.None) {
            builtin.print("FAIL: Buffered write failed");
            return 1;
        }
        i += 1;
    }
    output.close_checked()?;
    catch(err) {
        builtin.print("FAIL: Could not flush buffered output");
        return 1;
    }

    let input -> file.File = file.open(path)?;
    catch(err) {
        builtin.print("FAIL: Could not reopen buffered output");
        return 1;
    }
    let content -> String = input.read_all()?;
    catch(err) {
        builtin.print("FAIL: Could not read buffered output");
        return 1;
    }
    input.close();
    file.remove(path)?;
    catch(err) {
        builtin.print("FAIL: Could not remove buffered output");
        return 1;
    }

    if (content.length() != 70000 || content[0] != '0' || content[69999] != '9') {
        builtin.print("FAIL: Buffered output was truncated or reordered");
        return 1;
    }

    builtin.print("PASS: Buffered file writes");
    return 0;
}
